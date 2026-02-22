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

	"ebusta/internal/errutil"
	"ebusta/internal/logger"
	"ebusta/internal/presenter"
)

type SearchSession struct {
	Query     string
	Results   *presenter.PresenterResult
	Timestamp time.Time
	Channel   string
}

type IRCHandler struct {
	gatewayURL  string
	httpClient  *http.Client
	sessions    map[string]*SearchSession // nick -> session
	rateLimiter map[string]*rateLimit
	formatter   *presenter.IRCFormatter
	pageSize    int
	verbose     bool
	mu          sync.RWMutex
}

type rateLimit struct {
	count     int
	resetTime time.Time
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
	return &IRCHandler{
		gatewayURL: gatewayURL,
		httpClient: &http.Client{
			Timeout: 10 * time.Second,
		},
		sessions:    make(map[string]*SearchSession),
		rateLimiter: make(map[string]*rateLimit),
		formatter:   presenter.NewIRCFormatter(pageSize),
		pageSize:    pageSize,
		verbose:     verbose,
	}
}

func (h *IRCHandler) HandleChannelMessage(client *IRCClient, channel, message string) {
	h.handleMessage(client, channel, message)
}

func (h *IRCHandler) HandlePrivateMessage(client *IRCClient, message string) {
	h.handleMessage(client, client.nick, message)
}

func (h *IRCHandler) handleMessage(client *IRCClient, target, message string) {
	message = strings.TrimSpace(message)

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
		h.cmdSearch(client, target, parts)
	case "!get", "/get", "!info", "/info":
		h.cmdGet(client, target, parts)
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
		"!search page <n>         - Go to page N of last search",
		"!info <number>           - Show book information",
		"!stats                   - Show bot statistics",
		"",
		"📝 Examples:",
		"  !search author:king",
		"  !search title:hobbit",
		"  !search id:bd6525...",
		"  !search author:king page 2",
		"  !info 1",
	}

	for _, line := range help {
		client.SendMessage(target, line)
	}
}

func (h *IRCHandler) cmdSearch(client *IRCClient, target string, parts []string) {
	query, page, err := parseSearchCommand(parts)
	if err != nil {
		client.SendMessage(target, "Usage: !search <query> [page <n>]")
		return
	}

	if !h.checkRateLimit(client.nick) {
		client.SendMessage(target, "❌ Too many requests. Try again later.")
		return
	}

	traceID := errutil.GenerateTraceID("irc")
	client.SendMessage(target, fmt.Sprintf("🔍 Searching for: %s (page %d)", query, page))

	req := SearchRequest{
		Query: query,
		Page:  page,
		Limit: h.pageSize,
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
		Pagination: &presenter.Pagination{
			CurrentPage: searchResp.Page,
			TotalPages:  searchResp.Pages,
			PageSize:    req.Limit,
			TotalItems:  searchResp.Total,
		},
	}

	h.mu.Lock()
	h.sessions[client.nick] = &SearchSession{
		Query:     query,
		Results:   presResult,
		Timestamp: time.Now(),
		Channel:   target,
	}
	h.mu.Unlock()

	lines, _ := h.formatter.FormatSearchResult(presResult, page)
	for _, line := range lines {
		client.SendMessage(target, line)
	}
}

func (h *IRCHandler) cmdGet(client *IRCClient, target string, parts []string) {
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
	session, exists := h.sessions[client.nick]
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
	h.mu.Lock()
	defer h.mu.Unlock()

	now := time.Now()
	limit, exists := h.rateLimiter[key]

	if !exists || now.After(limit.resetTime) {
		h.rateLimiter[key] = &rateLimit{
			count:     1,
			resetTime: now.Add(time.Minute),
		}
		return true
	}

	if limit.count >= 30 {
		return false
	}

	limit.count++
	return true
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
