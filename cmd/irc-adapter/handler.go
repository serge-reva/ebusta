package main

import (
	"bufio"
	"bytes"
	"encoding/binary"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"mime"
	"net"
	"net/http"
	"net/url"
	"path"
	"strconv"
	"strings"
	"sync"
	"time"

	"ebusta/internal/edge"
	"ebusta/internal/errutil"
	"ebusta/internal/logger"
	"ebusta/internal/presenter"
)

type SearchSession struct {
	Query     string
	Results   *presenter.PresenterResult
	PageSize  int
	Timestamp time.Time
	Channel   string
}

type IRCHandler struct {
	gatewayURL  string
	httpClient  *http.Client
	dccClient   *http.Client
	sessions    map[string]*SearchSession // nick -> session
	engine      *edge.Engine
	formatter   *presenter.IRCFormatter
	pageSize    int
	verbose     bool
	dccEnabled  bool
	dccPublicIP string
	dccPortMin  int
	dccPortMax  int
	dccTimeout  time.Duration
	mu          sync.RWMutex
}

type SearchRequest struct {
	Query string `json:"query"`
	Page  int    `json:"page"`
	Limit int    `json:"limit"`
}

type SearchResponse struct {
	TraceID   string              `json:"trace_id"`
	Total     int                 `json:"total"`
	Books     []presenter.BookDTO `json:"books"`
	Page      int                 `json:"page"`
	Pages     int                 `json:"pages"`
	ExecMode  string              `json:"exec_mode"`
	MatchMode string              `json:"match_mode"`
}

func NewIRCHandler(gatewayURL string, pageSize int, verbose bool) *IRCHandler {
	p := edge.DefaultPolicy("irc")
	p.Actions["command"] = edge.ActionPolicy{PerMinute: 30, Burst: 30}
	return NewIRCHandlerWithPolicy(gatewayURL, pageSize, verbose, p, edge.NewLabelCounterHook())
}

func NewIRCHandlerWithPolicy(gatewayURL string, pageSize int, verbose bool, policy edge.Policy, hook edge.Hook) *IRCHandler {
	return &IRCHandler{
		gatewayURL: gatewayURL,
		httpClient: &http.Client{
			Timeout: 10 * time.Second,
		},
		dccClient: &http.Client{},
		sessions:   make(map[string]*SearchSession),
		engine:     edge.NewEngine(policy, edge.NewMultiHook(hook, &edge.OTelHook{})),
		formatter:  presenter.NewIRCFormatter(pageSize),
		pageSize:   pageSize,
		verbose:    verbose,
		dccPortMin: 40000,
		dccPortMax: 40100,
		dccTimeout: 60 * time.Second,
	}
}

func (h *IRCHandler) SetDCCConfig(enabled bool, publicIP string, portMin, portMax, timeoutSec int) {
	h.dccEnabled = enabled
	h.dccPublicIP = strings.TrimSpace(publicIP)
	if portMin > 0 {
		h.dccPortMin = portMin
	}
	if portMax > 0 {
		h.dccPortMax = portMax
	}
	if h.dccPortMax < h.dccPortMin {
		h.dccPortMax = h.dccPortMin
	}
	if timeoutSec > 0 {
		h.dccTimeout = time.Duration(timeoutSec) * time.Second
	}
}

func (h *IRCHandler) HandleChannelMessage(client *IRCClient, channel, message string) {
	h.handleMessage(client, channel, client.nick, message)
}

func (h *IRCHandler) HandleChannelMessageFrom(client *IRCClient, channel, sender, message string) {
	h.handleMessage(client, channel, sender, message)
}

func (h *IRCHandler) HandlePrivateMessage(client *IRCClient, message string) {
	h.handleMessage(client, client.nick, client.nick, message)
}

func (h *IRCHandler) HandlePrivateMessageFrom(client *IRCClient, sender, message string) {
	h.handleMessage(client, sender, sender, message)
}

func (h *IRCHandler) handleMessage(client *IRCClient, target, sender, message string) {
	message = strings.TrimSpace(message)
	if err := h.engine.ValidateLine(nil, "command", message); err != nil {
		client.SendMessage(target, "❌ Command too long")
		return
	}

	if !strings.HasPrefix(message, "!") && !strings.HasPrefix(message, "/") {
		return
	}

	parts := strings.Fields(message)
	if len(parts) == 0 {
		return
	}

	cmd := strings.ToLower(parts[0])

	switch cmd {
	case "!help", "/help":
		h.cmdHelp(client, target)
	case "!search", "/search":
		h.cmdSearch(client, target, sender, parts)
	case "!page", "/page":
		h.cmdPage(client, target, sender, parts)
	case "!next", "/next":
		h.cmdNext(client, target, sender)
	case "!prev", "/prev":
		h.cmdPrev(client, target, sender)
	case "!lines", "/lines", "!pagesize", "/pagesize":
		h.cmdLines(client, target, sender, parts)
	case "!get", "/get", "!info", "/info":
		h.cmdInfo(client, target, sender, parts)
	case "!download", "/download":
		h.cmdDownload(client, target, sender, parts)
	case "!stats", "/stats":
		h.cmdStats(client, target)
	default:
		client.SendMessage(target, fmt.Sprintf("Unknown command: %s. Try !help", cmd))
	}
}

func (h *IRCHandler) cmdHelp(client *IRCClient, target string) {
	help := []string{
		"🤖 Ebusta Book Search Bot - Commands:",
		"!help                    - Show this help",
		"!search <query>          - Search books (e.g., !search author:king)",
		"!page <n>                - Go to page N of last search",
		"!next / !prev            - Navigate pages",
		"!lines <n>               - Set books per page and reset to page 1",
		"!info <number>           - Show book information",
		"!download <number>       - Start DCC file transfer",
		"!stats                   - Show bot statistics",
		"",
		"📝 Examples:",
		"  !search author:king",
		"  !page 2",
		"  !next",
		"  !lines 10",
		"  !search id:bd6525...",
		"  !info 1",
		"  !download 1",
	}

	for _, line := range help {
		client.SendMessage(target, line)
	}
}

func (h *IRCHandler) cmdSearch(client *IRCClient, target, sender string, parts []string) {
	if len(parts) >= 3 && strings.EqualFold(parts[1], "page") {
		page, err := strconv.Atoi(parts[2])
		if err != nil || page <= 0 {
			client.SendMessage(target, "Usage: !page <n>")
			return
		}
		h.goToPage(client, target, sender, page)
		return
	}

	query, page, err := parseSearchCommand(parts)
	if err != nil {
		client.SendMessage(target, "Usage: !search <query> [page <n>]")
		return
	}
	limit := h.getUserPageSize(sender)
	h.performSearch(client, target, sender, query, page, limit)
}

func (h *IRCHandler) cmdPage(client *IRCClient, target, sender string, parts []string) {
	if len(parts) < 2 {
		client.SendMessage(target, "Usage: !page <n>")
		return
	}
	page, err := strconv.Atoi(parts[1])
	if err != nil || page <= 0 {
		client.SendMessage(target, "Usage: !page <n>")
		return
	}
	h.goToPage(client, target, sender, page)
}

func (h *IRCHandler) cmdNext(client *IRCClient, target, sender string) {
	h.mu.RLock()
	session, exists := h.sessions[sender]
	h.mu.RUnlock()
	if !exists || session.Results == nil || session.Results.Pagination == nil {
		client.SendMessage(target, "❌ No recent search. Use !search first")
		return
	}
	if !session.Results.Pagination.HasNext {
		client.SendMessage(target, "ℹ️ Already on last page")
		return
	}
	h.goToPage(client, target, sender, session.Results.Pagination.CurrentPage+1)
}

func (h *IRCHandler) cmdPrev(client *IRCClient, target, sender string) {
	h.mu.RLock()
	session, exists := h.sessions[sender]
	h.mu.RUnlock()
	if !exists || session.Results == nil || session.Results.Pagination == nil {
		client.SendMessage(target, "❌ No recent search. Use !search first")
		return
	}
	if !session.Results.Pagination.HasPrev {
		client.SendMessage(target, "ℹ️ Already on first page")
		return
	}
	h.goToPage(client, target, sender, session.Results.Pagination.CurrentPage-1)
}

func (h *IRCHandler) cmdLines(client *IRCClient, target, sender string, parts []string) {
	if len(parts) < 2 {
		client.SendMessage(target, "Usage: !lines <n>")
		return
	}
	n, err := strconv.Atoi(parts[1])
	if err != nil || n <= 0 || n > 50 {
		client.SendMessage(target, "❌ Invalid lines value. Allowed range: 1-50")
		return
	}

	h.mu.RLock()
	session, exists := h.sessions[sender]
	h.mu.RUnlock()
	if !exists || strings.TrimSpace(session.Query) == "" {
		client.SendMessage(target, "❌ No recent search. Use !search first")
		return
	}

	client.SendMessage(target, fmt.Sprintf("ℹ️ Page size set to %d, resetting to page 1", n))
	h.performSearch(client, target, sender, session.Query, 1, n)
}

func (h *IRCHandler) goToPage(client *IRCClient, target, sender string, page int) {
	h.mu.RLock()
	session, exists := h.sessions[sender]
	h.mu.RUnlock()
	if !exists || strings.TrimSpace(session.Query) == "" {
		client.SendMessage(target, "❌ No recent search. Use !search first")
		return
	}
	limit := session.PageSize
	if limit <= 0 {
		limit = h.pageSize
	}
	h.performSearch(client, target, sender, session.Query, page, limit)
}

func (h *IRCHandler) getUserPageSize(sender string) int {
	h.mu.RLock()
	defer h.mu.RUnlock()
	if session, ok := h.sessions[sender]; ok && session.PageSize > 0 {
		return session.PageSize
	}
	return h.pageSize
}

func (h *IRCHandler) performSearch(client *IRCClient, target, sender, query string, page, limit int) {
	if !h.checkRateLimit(sender) {
		client.SendMessage(target, "❌ Too many requests. Try again later.")
		return
	}

	traceID := errutil.GenerateTraceID("irc")
	client.SendMessage(target, fmt.Sprintf("🔍 Searching for: %s (page %d)", query, page))

	searchResp, appErr, err := h.requestSearch(query, page, limit, traceID)
	if err != nil {
		logger.GetGlobal().WithField("trace_id", traceID).Error(traceID, "search gateway unavailable", err)
		client.SendMessage(target, fmt.Sprintf("❌ Search service unavailable (trace: %s)", traceID))
		return
	}
	if appErr != nil {
		logger.GetGlobal().
			WithField("trace_id", appErr.TraceID).
			WithField("code", appErr.Code).
			WithField("http", appErr.HTTPCode).
			Warn(appErr.TraceID, "gateway search rejected")
		client.SendMessage(target, fmt.Sprintf("❌ %s (trace: %s)", appErr.Message, appErr.TraceID))
		return
	}

	if searchResp.Total == 0 {
		client.SendMessage(target, "📚 No books found")
		return
	}

	presResult := &presenter.PresenterResult{
		SearchResult: &presenter.SearchResult{
			TraceId: searchResp.TraceID,
			Total:   searchResp.Total,
			Books:   searchResp.Books,
		},
		Pagination: presenter.NewPagination(searchResp.Total, searchResp.Page, limit),
	}
	presResult.Pagination.TotalPages = searchResp.Pages

	h.mu.Lock()
	h.sessions[sender] = &SearchSession{
		Query:     query,
		Results:   presResult,
		PageSize:  limit,
		Timestamp: time.Now(),
		Channel:   target,
	}
	h.mu.Unlock()

	lines, _ := h.formatter.FormatSearchResult(presResult, searchResp.Page)
	if searchResp.ExecMode != "" || searchResp.MatchMode != "" {
		mode := searchResp.ExecMode
		if mode == "" {
			mode = "UNKNOWN"
		}
		match := searchResp.MatchMode
		if match == "" {
			match = "unknown"
		}
		client.SendMessage(target, fmt.Sprintf("🧭 Mode: %s • Match: %s", mode, match))
	}
	for _, line := range lines {
		client.SendMessage(target, line)
	}
}

func (h *IRCHandler) cmdInfo(client *IRCClient, target, sender string, parts []string) {
	if len(parts) < 2 {
		client.SendMessage(target, "Usage: !info <number>")
		return
	}

	num, err := strconv.Atoi(parts[1])
	if err != nil || num <= 0 {
		client.SendMessage(target, "❌ Invalid number")
		return
	}

	book, targetPage, totalPages, appErr, err := h.resolveBookByGlobalIndex(sender, num)
	if err != nil {
		client.SendMessage(target, fmt.Sprintf("❌ %s", err.Error()))
		return
	}
	if appErr != nil {
		client.SendMessage(target, fmt.Sprintf("❌ %s (trace: %s)", appErr.Message, appErr.TraceID))
		return
	}

	client.SendMessage(target, fmt.Sprintf("ℹ️ Book #%d • page %d/%d", num, targetPage, totalPages))
	client.SendMessage(target, fmt.Sprintf("📖 %s", book.Title))
	client.SendMessage(target, fmt.Sprintf("👤 %s", book.FullAuthors))
	downloadURL := h.toAbsoluteDownloadURL(book.DownloadURL)
	if downloadURL != "" {
		client.SendMessage(target, fmt.Sprintf("📥 Download: %s", downloadURL))
	}
	client.SendMessage(target, fmt.Sprintf("📦 DCC: !download %d", num))
}

func (h *IRCHandler) resolveBookByGlobalIndex(sender string, num int) (presenter.BookDTO, int, int, *errutil.AppError, error) {
	h.mu.RLock()
	session, exists := h.sessions[sender]
	h.mu.RUnlock()

	if !exists || session == nil || session.Results == nil {
		return presenter.BookDTO{}, 0, 0, nil, errors.New("No recent search. Use !search first")
	}
	total := session.Results.Total
	if num < 1 || num > total {
		return presenter.BookDTO{}, 0, 0, nil, fmt.Errorf("Invalid number. Available: 1-%d", total)
	}

	pageSize := session.PageSize
	if pageSize <= 0 {
		pageSize = h.pageSize
	}
	targetPage := ((num - 1) / pageSize) + 1
	targetOffset := (num - 1) % pageSize

	currentPage := 0
	totalPages := 0
	if session.Results.Pagination != nil {
		currentPage = session.Results.Pagination.CurrentPage
		totalPages = session.Results.Pagination.TotalPages
	}
	if targetPage == currentPage && targetOffset < len(session.Results.Books) {
		return session.Results.Books[targetOffset], targetPage, totalPages, nil, nil
	}

	traceID := errutil.GenerateTraceID("irc")
	searchResp, appErr, reqErr := h.requestSearch(session.Query, targetPage, pageSize, traceID)
	if reqErr != nil {
		return presenter.BookDTO{}, targetPage, totalPages, nil, fmt.Errorf("Search service unavailable (trace: %s)", traceID)
	}
	if appErr != nil {
		return presenter.BookDTO{}, targetPage, totalPages, appErr, nil
	}
	if targetOffset >= len(searchResp.Books) {
		return presenter.BookDTO{}, targetPage, totalPages, nil, errors.New("Book index out of range for selected page")
	}
	return searchResp.Books[targetOffset], targetPage, searchResp.Pages, nil, nil
}

func (h *IRCHandler) requestSearch(query string, page, limit int, traceID string) (*SearchResponse, *errutil.AppError, error) {
	req := SearchRequest{Query: query, Page: page, Limit: limit}
	jsonData, jerr := json.Marshal(req)
	if jerr != nil {
		return nil, nil, jerr
	}
	httpReq, reqErr := http.NewRequest(http.MethodPost, h.gatewayURL+"/search", bytes.NewBuffer(jsonData))
	if reqErr != nil {
		return nil, nil, reqErr
	}
	httpReq.Header.Set("Content-Type", "application/json")
	httpReq.Header.Set("X-Trace-Id", traceID)

	resp, err := h.httpClient.Do(httpReq)
	if err != nil {
		return nil, nil, err
	}
	defer resp.Body.Close()

	body, appErr := errutil.ReadBodyAndError(resp, traceID)
	if appErr != nil {
		return nil, appErr, nil
	}
	var searchResp SearchResponse
	if err := json.Unmarshal(body, &searchResp); err != nil {
		return nil, nil, fmt.Errorf("invalid gateway response: %w", err)
	}
	return &searchResp, nil, nil
}

func (h *IRCHandler) cmdDownload(client *IRCClient, target, sender string, parts []string) {
	if len(parts) < 2 {
		client.SendMessage(target, "Usage: !download <number>")
		return
	}
	if !h.dccEnabled {
		client.SendMessage(target, "❌ DCC download is disabled in config")
		return
	}
	num, err := strconv.Atoi(parts[1])
	if err != nil || num <= 0 {
		client.SendMessage(target, "❌ Invalid number")
		return
	}

	book, _, _, appErr, resolveErr := h.resolveBookByGlobalIndex(sender, num)
	if resolveErr != nil {
		client.SendMessage(target, fmt.Sprintf("❌ %s", resolveErr.Error()))
		return
	}
	if appErr != nil {
		client.SendMessage(target, fmt.Sprintf("❌ %s (trace: %s)", appErr.Message, appErr.TraceID))
		return
	}
	downloadURL := h.toAbsoluteDownloadURL(book.DownloadURL)
	if strings.TrimSpace(downloadURL) == "" {
		client.SendMessage(target, "❌ No download URL for selected book")
		return
	}

	traceID := errutil.GenerateTraceID("irc-dcc")
	filename, size, metaErr := h.fetchDownloadMeta(downloadURL, traceID)
	if metaErr != nil {
		client.SendMessage(target, fmt.Sprintf("❌ DCC metadata failed: %v (trace: %s)", metaErr, traceID))
		return
	}
	if filename == "" {
		filename = fmt.Sprintf("book-%d.fb2", num)
	}

	listener, port, listenErr := h.openDCCListener()
	if listenErr != nil {
		client.SendMessage(target, fmt.Sprintf("❌ DCC listen failed: %v", listenErr))
		return
	}

	ip, ipErr := h.resolveDCCIP(client)
	if ipErr != nil {
		listener.Close()
		client.SendMessage(target, fmt.Sprintf("❌ DCC IP resolution failed: %v", ipErr))
		return
	}

	dccMsg, dccErr := buildDCCSendMessage(filename, ip, port, size)
	if dccErr != nil {
		listener.Close()
		client.SendMessage(target, fmt.Sprintf("❌ DCC setup failed: %v", dccErr))
		return
	}

	client.Send("PRIVMSG", sender, dccMsg)
	client.SendMessage(target, fmt.Sprintf("📦 DCC offer sent to %s for book #%d (%s, %d bytes)", sender, num, filename, size))

	go h.serveDCCTransfer(client, target, sender, listener, downloadURL, traceID)
}

func (h *IRCHandler) fetchDownloadMeta(downloadURL, traceID string) (string, int64, error) {
	req, err := http.NewRequest(http.MethodHead, downloadURL, nil)
	if err != nil {
		return "", 0, err
	}
	req.Header.Set("X-Trace-Id", traceID)
	resp, err := h.httpClient.Do(req)
	if err != nil {
		return "", 0, err
	}
	defer resp.Body.Close()

	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		// HEAD often has empty body; retry with GET to get downstream JSON error details.
		detail, derr := h.fetchDownloadErrorDetail(downloadURL, traceID)
		if derr != nil {
			return "", 0, derr
		}
		return "", 0, errors.New(detail)
	}

	if _, appErr := errutil.ReadBodyAndError(resp, traceID); appErr != nil {
		return "", 0, fmt.Errorf("%s", appErr.Message)
	}

	size, _ := strconv.ParseInt(resp.Header.Get("Content-Length"), 10, 64)
	filename := filenameFromDisposition(resp.Header.Get("Content-Disposition"))
	if filename == "" {
		if u, perr := url.Parse(downloadURL); perr == nil {
			filename = path.Base(u.Path)
		}
	}
	return sanitizeDCCFilename(filename), size, nil
}

func (h *IRCHandler) fetchDownloadErrorDetail(downloadURL, traceID string) (string, error) {
	req, err := http.NewRequest(http.MethodGet, downloadURL, nil)
	if err != nil {
		return "", err
	}
	req.Header.Set("X-Trace-Id", traceID)
	resp, err := h.httpClient.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()

	if _, appErr := errutil.ReadBodyAndError(resp, traceID); appErr != nil {
		if strings.TrimSpace(appErr.Message) != "" {
			return appErr.Message, nil
		}
		return appErr.Code, nil
	}
	return "download metadata unavailable", nil
}

func (h *IRCHandler) openDCCListener() (net.Listener, int, error) {
	var lastErr error
	for p := h.dccPortMin; p <= h.dccPortMax; p++ {
		ln, err := net.Listen("tcp", fmt.Sprintf(":%d", p))
		if err == nil {
			return ln, p, nil
		}
		lastErr = err
	}
	if lastErr == nil {
		lastErr = errors.New("no available DCC port")
	}
	return nil, 0, lastErr
}

func (h *IRCHandler) resolveDCCIP(client *IRCClient) (net.IP, error) {
	if h.dccPublicIP != "" {
		ip := net.ParseIP(h.dccPublicIP)
		if ip == nil || ip.To4() == nil {
			return nil, fmt.Errorf("dcc_public_ip must be a valid IPv4 address")
		}
		return ip.To4(), nil
	}
	if tcpAddr, ok := client.conn.LocalAddr().(*net.TCPAddr); ok {
		if tcpAddr.IP != nil && tcpAddr.IP.To4() != nil {
			return tcpAddr.IP.To4(), nil
		}
	}
	return nil, errors.New("unable to determine IPv4 address; set irc_adapter.dcc_public_ip")
}

func buildDCCSendMessage(filename string, ip net.IP, port int, size int64) (string, error) {
	ip4 := ip.To4()
	if ip4 == nil {
		return "", errors.New("DCC requires IPv4")
	}
	ipNum := binary.BigEndian.Uint32(ip4)
	name := sanitizeDCCFilename(filename)
	if name == "" {
		name = "book.fb2"
	}
	return fmt.Sprintf("\x01DCC SEND %s %d %d %d\x01", name, ipNum, port, size), nil
}

func filenameFromDisposition(cd string) string {
	if strings.TrimSpace(cd) == "" {
		return ""
	}
	_, params, err := mime.ParseMediaType(cd)
	if err != nil {
		return ""
	}
	return params["filename"]
}

func sanitizeDCCFilename(name string) string {
	n := strings.TrimSpace(name)
	if n == "" {
		return ""
	}
	n = strings.ReplaceAll(n, " ", "_")
	n = strings.ReplaceAll(n, "/", "_")
	n = strings.ReplaceAll(n, "\\", "_")
	return n
}

func (h *IRCHandler) serveDCCTransfer(client *IRCClient, target, sender string, listener net.Listener, downloadURL, traceID string) {
	defer listener.Close()
	if tcpLn, ok := listener.(*net.TCPListener); ok {
		_ = tcpLn.SetDeadline(time.Now().Add(h.dccTimeout))
	}
	conn, err := listener.Accept()
	if err != nil {
		client.SendMessage(target, fmt.Sprintf("❌ DCC timeout or accept error: %v", err))
		return
	}
	defer conn.Close()
	ackCh := make(chan uint32, 1)
	ackErrCh := make(chan error, 1)
	go func() {
		// DCC receiver sends 32-bit network-order ACK counters.
		buf := make([]byte, 4)
		for {
			if _, rerr := io.ReadFull(conn, buf); rerr != nil {
				ackErrCh <- rerr
				close(ackCh)
				return
			}
			ack := binary.BigEndian.Uint32(buf)
			select {
			case ackCh <- ack:
			default:
				select {
				case <-ackCh:
				default:
				}
				ackCh <- ack
			}
		}
	}()

	req, err := http.NewRequest(http.MethodGet, downloadURL, nil)
	if err != nil {
		client.SendMessage(target, fmt.Sprintf("❌ DCC internal request error: %v", err))
		return
	}
	req.Header.Set("X-Trace-Id", traceID)
	resp, err := h.dccClient.Do(req)
	if err != nil {
		logger.GetGlobal().WithField("trace_id", traceID).WithField("url", downloadURL).
			Error("irc", "dcc downstream request failed", err)
		client.SendMessage(target, fmt.Sprintf("❌ DCC download failed: %v (trace: %s)", err, traceID))
		return
	}
	defer resp.Body.Close()
	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		_, appErr := errutil.ReadBodyAndError(resp, traceID)
		if appErr != nil {
			client.SendMessage(target, fmt.Sprintf("❌ DCC download failed: %s (trace: %s)", appErr.Message, appErr.TraceID))
		} else {
			client.SendMessage(target, fmt.Sprintf("❌ DCC download failed: status %d", resp.StatusCode))
		}
		return
	}

	written, err := io.Copy(conn, bufio.NewReader(resp.Body))
	if err != nil {
		logger.GetGlobal().WithField("trace_id", traceID).
			Error("irc", "dcc transfer interrupted", err)
		client.SendMessage(target, fmt.Sprintf("❌ DCC transfer interrupted: %v", err))
		return
	}
	if resp.ContentLength > 0 && written != resp.ContentLength {
		msg := fmt.Sprintf("short transfer: wrote %d of %d bytes", written, resp.ContentLength)
		logger.GetGlobal().WithField("trace_id", traceID).
			Error("irc", "dcc short transfer", errors.New(msg))
		client.SendMessage(target, fmt.Sprintf("❌ DCC transfer interrupted: %s", msg))
		return
	}
	sentBytes := uint32(written)
	if tcpConn, ok := conn.(*net.TCPConn); ok {
		_ = tcpConn.CloseWrite()
		_ = tcpConn.SetReadDeadline(time.Now().Add(5 * time.Second))
	}
	acked := uint32(0)
	for acked < sentBytes {
		select {
		case ack, ok := <-ackCh:
			if !ok {
				if acked >= sentBytes {
					break
				}
				select {
				case aerr := <-ackErrCh:
					msg := fmt.Sprintf("ack channel closed early after %d/%d bytes: %v", acked, sentBytes, aerr)
					logger.GetGlobal().WithField("trace_id", traceID).
						Error("irc", "dcc ack incomplete", errors.New(msg))
					client.SendMessage(target, fmt.Sprintf("❌ DCC transfer interrupted: %s", msg))
					return
				default:
					msg := fmt.Sprintf("ack channel closed early after %d/%d bytes", acked, sentBytes)
					logger.GetGlobal().WithField("trace_id", traceID).
						Error("irc", "dcc ack incomplete", errors.New(msg))
					client.SendMessage(target, fmt.Sprintf("❌ DCC transfer interrupted: %s", msg))
					return
				}
			}
			acked = ack
		case aerr := <-ackErrCh:
			if acked >= sentBytes {
				break
			}
			msg := fmt.Sprintf("ack read failed after %d/%d bytes: %v", acked, sentBytes, aerr)
			logger.GetGlobal().WithField("trace_id", traceID).
				Error("irc", "dcc ack read failed", errors.New(msg))
			client.SendMessage(target, fmt.Sprintf("❌ DCC transfer interrupted: %s", msg))
			return
		case <-time.After(5 * time.Second):
			msg := fmt.Sprintf("ack timeout after %d/%d bytes", acked, sentBytes)
			logger.GetGlobal().WithField("trace_id", traceID).
				Error("irc", "dcc ack timeout", errors.New(msg))
			client.SendMessage(target, fmt.Sprintf("❌ DCC transfer interrupted: %s", msg))
			return
		}
	}
	client.SendMessage(sender, "✅ DCC transfer complete")
}

func (h *IRCHandler) toAbsoluteDownloadURL(downloadURL string) string {
	d := strings.TrimSpace(downloadURL)
	if d == "" {
		return ""
	}
	if strings.HasPrefix(d, "http://") || strings.HasPrefix(d, "https://") {
		return d
	}
	base, err := url.Parse(strings.TrimRight(h.gatewayURL, "/"))
	if err != nil {
		return d
	}
	ref, err := url.Parse(d)
	if err != nil {
		return d
	}
	return base.ResolveReference(ref).String()
}

func (h *IRCHandler) cmdStats(client *IRCClient, target string) {
	h.mu.RLock()
	sessions := len(h.sessions)
	h.mu.RUnlock()

	client.SendMessage(target, "📊 Ebusta Bot Statistics:")
	client.SendMessage(target, fmt.Sprintf("  • Active sessions: %d", sessions))
	client.SendMessage(target, fmt.Sprintf("  • Gateway: %s", h.gatewayURL))
}

func (h *IRCHandler) checkRateLimit(key string) bool {
	return h.engine.Allow(nil, "command", key)
}

func parseSearchCommand(parts []string) (query string, page int, err error) {
	if len(parts) < 2 {
		return "", 1, fmt.Errorf("empty search query")
	}

	page = 1
	query = strings.Join(parts[1:], " ")

	if len(parts) >= 4 && strings.EqualFold(parts[len(parts)-2], "page") {
		p, convErr := strconv.Atoi(parts[len(parts)-1])
		if convErr != nil || p <= 0 {
			return "", 1, fmt.Errorf("invalid page")
		}
		page = p
		query = strings.Join(parts[1:len(parts)-2], " ")
	}

	query = strings.TrimSpace(query)
	if query == "" {
		return "", 1, fmt.Errorf("empty search query")
	}
	return query, page, nil
}
