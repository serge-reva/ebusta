package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"net/http"
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
	gatewayURL string
	httpClient *http.Client
	sessions   map[string]*SearchSession // nick -> session
	engine     *edge.Engine
	formatter  *presenter.IRCFormatter
	pageSize   int
	verbose    bool
	mu         sync.RWMutex
}

type SearchRequest struct {
	Query string `json:"query"`
	Page  int    `json:"page"`
	Limit int    `json:"limit"`
}

type SearchResponse struct {
	TraceID string              `json:"trace_id"`
	Total   int                 `json:"total"`
	Books   []presenter.BookDTO `json:"books"`
	Page    int                 `json:"page"`
	Pages   int                 `json:"pages"`
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
		sessions:  make(map[string]*SearchSession),
		engine:    edge.NewEngine(policy, edge.NewMultiHook(hook, &edge.OTelHook{})),
		formatter: presenter.NewIRCFormatter(pageSize),
		pageSize:  pageSize,
		verbose:   verbose,
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
		h.cmdGet(client, target, sender, parts)
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
		"!stats                   - Show bot statistics",
		"",
		"📝 Examples:",
		"  !search author:king",
		"  !page 2",
		"  !next",
		"  !lines 10",
		"  !search id:bd6525...",
		"  !info 1",
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

	req := SearchRequest{
		Query: query,
		Page:  page,
		Limit: limit,
	}

	jsonData, jerr := json.Marshal(req)
	if jerr != nil {
		logger.GetGlobal().WithField("trace_id", traceID).Error(traceID, "failed to marshal search request", jerr)
		client.SendMessage(target, fmt.Sprintf("❌ Internal error (trace: %s)", traceID))
		return
	}

	httpReq, reqErr := http.NewRequest(http.MethodPost, h.gatewayURL+"/search", bytes.NewBuffer(jsonData))
	if reqErr != nil {
		logger.GetGlobal().WithField("trace_id", traceID).Error(traceID, "failed to create http request", reqErr)
		client.SendMessage(target, fmt.Sprintf("❌ Internal error (trace: %s)", traceID))
		return
	}
	httpReq.Header.Set("Content-Type", "application/json")
	httpReq.Header.Set("X-Trace-Id", traceID)

	resp, err := h.httpClient.Do(httpReq)
	if err != nil {
		logger.GetGlobal().WithField("trace_id", traceID).Error(traceID, "search gateway unavailable", err)
		client.SendMessage(target, fmt.Sprintf("❌ Search service unavailable (trace: %s)", traceID))
		return
	}
	defer resp.Body.Close()

	body, appErr := errutil.ReadBodyAndError(resp, traceID)
	if appErr != nil {
		logger.GetGlobal().
			WithField("trace_id", appErr.TraceID).
			WithField("code", appErr.Code).
			WithField("http", appErr.HTTPCode).
			Warn(appErr.TraceID, "gateway search rejected")
		client.SendMessage(target, fmt.Sprintf("❌ %s (trace: %s)", appErr.Message, appErr.TraceID))
		return
	}

	var searchResp SearchResponse
	if err := json.Unmarshal(body, &searchResp); err != nil {
		logger.GetGlobal().WithField("trace_id", traceID).WithField("body", string(body)).Error(traceID, "invalid gateway response", err)
		client.SendMessage(target, fmt.Sprintf("❌ Invalid response (trace: %s)", traceID))
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
		Pagination: presenter.NewPagination(searchResp.Total, searchResp.Page, req.Limit),
	}
	presResult.Pagination.TotalPages = searchResp.Pages

	h.mu.Lock()
	h.sessions[sender] = &SearchSession{
		Query:     query,
		Results:   presResult,
		PageSize:  req.Limit,
		Timestamp: time.Now(),
		Channel:   target,
	}
	h.mu.Unlock()

	lines, _ := h.formatter.FormatSearchResult(presResult, searchResp.Page)
	for _, line := range lines {
		client.SendMessage(target, line)
	}
}

func (h *IRCHandler) cmdGet(client *IRCClient, target, sender string, parts []string) {
	if len(parts) < 2 {
		client.SendMessage(target, "Usage: !info <number>")
		return
	}

	num, err := strconv.Atoi(parts[1])
	if err != nil {
		client.SendMessage(target, "❌ Invalid number")
		return
	}

	h.mu.RLock()
	session, exists := h.sessions[sender]
	h.mu.RUnlock()

	if !exists {
		client.SendMessage(target, "❌ No recent search. Use !search first")
		return
	}

	if num < 1 || num > len(session.Results.Books) {
		client.SendMessage(target, fmt.Sprintf("❌ Invalid number. Available: 1-%d", len(session.Results.Books)))
		return
	}

	book := session.Results.Books[num-1]

	client.SendMessage(target, fmt.Sprintf("📖 %s", book.Title))
	client.SendMessage(target, fmt.Sprintf("👤 %s", book.FullAuthors))
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
