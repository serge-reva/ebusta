package usecase

import (
	"context"
	"strconv"
	"strings"
	"time"

	"ebusta/internal/botcommand"
	"ebusta/internal/edge"
	"ebusta/internal/errutil"
	"ebusta/internal/gatewayclient"
	corepresenter "ebusta/internal/presenter"
	tgpresenter "ebusta/internal/telegrambot/presenter"
	"ebusta/internal/telegrambot/session"
)

type SearchService interface {
	Search(ctx context.Context, query string, page, limit int, traceID string) (*gatewayclient.SearchResponse, error)
}

type Formatter interface {
	FormatSearchResult(result *corepresenter.PresenterResult, page int) (string, *tgpresenter.InlineKeyboardMarkup, error)
	FormatHelp() string
	FormatError(message, traceID string) string
}

type Result struct {
	Text     string
	Keyboard *tgpresenter.InlineKeyboardMarkup
	TraceID  string
}

type Handler struct {
	searcher  SearchService
	store     session.Store
	formatter Formatter
	engine    *edge.Engine
	pageSize  int
}

func NewHandler(searcher SearchService, store session.Store, formatter Formatter, engine *edge.Engine, pageSize int) *Handler {
	if engine == nil {
		p := edge.DefaultPolicy("telegram")
		p.Actions["command"] = edge.ActionPolicy{PerMinute: 30, Burst: 30}
		engine = edge.NewEngine(p, nil)
	}
	if pageSize <= 0 {
		pageSize = 5
	}
	return &Handler{
		searcher:  searcher,
		store:     store,
		formatter: formatter,
		engine:    engine,
		pageSize:  pageSize,
	}
}

func (h *Handler) HandleSearch(ctx context.Context, userID, input, traceID string) (*Result, error) {
	if traceID == "" {
		traceID = errutil.GenerateTraceID("tg")
	}
	if err := h.engine.ValidateLine(ctx, "command", input); err != nil {
		return &Result{Text: h.formatter.FormatError(err.Error(), traceID), TraceID: traceID}, nil
	}
	if !h.engine.Allow(ctx, "command", userID) {
		return &Result{Text: h.formatter.FormatError("rate limit exceeded", traceID), TraceID: traceID}, nil
	}

	cmd := botcommand.Parse(input)
	search, ok := cmd.(botcommand.SearchCommand)
	if !ok {
		return &Result{Text: h.formatter.FormatError("Usage: /search <query> [page <n>]", traceID), TraceID: traceID}, nil
	}
	return h.searchAndStore(ctx, userID, search.Query, search.Page, traceID)
}

func (h *Handler) HandlePage(ctx context.Context, userID string, page int, traceID string) (*Result, error) {
	if traceID == "" {
		traceID = errutil.GenerateTraceID("tg")
	}
	s, ok := h.store.Get(ctx, userID)
	if !ok || strings.TrimSpace(s.Query) == "" {
		return &Result{Text: h.formatter.FormatError("No recent search. Use /search first.", traceID), TraceID: traceID}, nil
	}
	if page <= 0 {
		page = 1
	}
	return h.searchAndStore(ctx, userID, s.Query, page, traceID)
}

func (h *Handler) HandleHelp(traceID string) *Result {
	if traceID == "" {
		traceID = errutil.GenerateTraceID("tg")
	}
	return &Result{Text: h.formatter.FormatHelp(), TraceID: traceID}
}

func (h *Handler) HandleCallback(ctx context.Context, userID, callbackData, traceID string) (*Result, error) {
	if traceID == "" {
		traceID = errutil.GenerateTraceID("tg")
	}
	switch callbackData {
	case "page:next":
		s, ok := h.store.Get(ctx, userID)
		if !ok {
			return &Result{Text: h.formatter.FormatError("No recent search. Use /search first.", traceID), TraceID: traceID}, nil
		}
		return h.HandlePage(ctx, userID, s.CurrentPage+1, traceID)
	case "page:prev":
		s, ok := h.store.Get(ctx, userID)
		if !ok {
			return &Result{Text: h.formatter.FormatError("No recent search. Use /search first.", traceID), TraceID: traceID}, nil
		}
		return h.HandlePage(ctx, userID, s.CurrentPage-1, traceID)
	default:
		if strings.HasPrefix(callbackData, "page:") {
			page, err := strconv.Atoi(strings.TrimPrefix(callbackData, "page:"))
			if err == nil {
				return h.HandlePage(ctx, userID, page, traceID)
			}
		}
		return &Result{Text: h.formatter.FormatError("Unsupported action.", traceID), TraceID: traceID}, nil
	}
}

func (h *Handler) searchAndStore(ctx context.Context, userID, query string, page int, traceID string) (*Result, error) {
	resp, err := h.searcher.Search(ctx, query, page, h.pageSize, traceID)
	if err != nil {
		return &Result{Text: h.formatter.FormatError(mapError(err), traceID), TraceID: traceID}, nil
	}

	pres := &corepresenter.PresenterResult{
		SearchResult: &corepresenter.SearchResult{
			TraceId: resp.TraceID,
			Total:   resp.Total,
			Books:   toPresenterBooks(resp.Books),
		},
		Pagination: corepresenter.NewPagination(resp.Total, resp.Page, h.pageSize),
	}
	pres.Pagination.TotalPages = resp.Pages
	text, keyboard, err := h.formatter.FormatSearchResult(pres, page)
	if err != nil {
		return nil, err
	}
	_ = h.store.Put(ctx, &session.Session{
		UserID:      userID,
		Query:       query,
		PageSize:    h.pageSize,
		CurrentPage: resp.Page,
		LastResult:  pres,
		UpdatedAt:   time.Now(),
	})

	return &Result{
		Text:     text,
		Keyboard: keyboard,
		TraceID:  resp.TraceID,
	}, nil
}

func toPresenterBooks(books []gatewayclient.SearchBook) []corepresenter.BookDTO {
	result := make([]corepresenter.BookDTO, 0, len(books))
	for _, book := range books {
		result = append(result, corepresenter.BookDTO{
			ID:          book.ID,
			Title:       book.Title,
			Authors:     book.Authors,
			Container:   book.Container,
			Filename:    book.Filename,
			FullAuthors: book.FullAuthors,
			DownloadURL: book.DownloadURL,
		})
	}
	return result
}

func mapError(err error) string {
	appErr, ok := err.(*errutil.AppError)
	if !ok {
		return "Internal error."
	}
	switch appErr.Code {
	case errutil.CodeInvalidArgument:
		return "Invalid search query."
	case errutil.CodeUnavailable, errutil.CodeTimeout, errutil.CodeBadGateway:
		return "Search service is temporarily unavailable."
	case errutil.CodeNotFound:
		return "No books found."
	default:
		if appErr.Message != "" {
			return appErr.Message
		}
		return "Internal error."
	}
}
