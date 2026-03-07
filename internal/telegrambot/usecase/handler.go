package usecase

import (
	"bytes"
	"context"
	"strconv"
	"strings"
	"time"

	"ebusta/internal/botcommand"
	"ebusta/internal/edge"
	"ebusta/internal/errutil"
	"ebusta/internal/gatewayclient"
	"ebusta/internal/logger"
	corepresenter "ebusta/internal/presenter"
	tgpresenter "ebusta/internal/telegrambot/presenter"
	"ebusta/internal/telegrambot/session"
)

type SearchService interface {
	Search(ctx context.Context, query string, page, limit int, traceID string) (*gatewayclient.SearchResponse, error)
	GetMeta(ctx context.Context, token, traceID string) (*gatewayclient.FileMeta, error)
	DownloadBook(ctx context.Context, token, traceID string) ([]byte, *gatewayclient.FileMeta, error)
}

type Formatter interface {
	FormatSearchResult(result *corepresenter.PresenterResult, page int) (string, *tgpresenter.InlineKeyboardMarkup, error)
	FormatBookDetails(book *corepresenter.BookDTO) (string, *tgpresenter.InlineKeyboardMarkup)
	FormatHelp() string
	FormatError(message, traceID string) string
}

type Document struct {
	Filename string
	Data     *bytes.Reader
	Caption  string
}

type Result struct {
	Text      string
	Keyboard  *tgpresenter.InlineKeyboardMarkup
	Document  *Document
	ForceSend bool
	TraceID   string
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
	case "back":
		return h.HandleBack(ctx, userID, traceID)
	case "page:current":
		return nil, nil
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
		if strings.HasPrefix(callbackData, "download:") {
			return h.HandleDownload(ctx, userID, strings.TrimPrefix(callbackData, "download:"), traceID)
		}
		if strings.HasPrefix(callbackData, "page:") {
			page, err := strconv.Atoi(strings.TrimPrefix(callbackData, "page:"))
			if err == nil {
				return h.HandlePage(ctx, userID, page, traceID)
			}
		}
		return &Result{Text: h.formatter.FormatError("Unsupported action.", traceID), TraceID: traceID}, nil
	}
}

func (h *Handler) HandleSelectBook(ctx context.Context, userID string, globalIndex int, traceID string) (*Result, error) {
	if traceID == "" {
		traceID = errutil.GenerateTraceID("tg")
	}
	s, ok := h.store.Get(ctx, userID)
	if !ok || s.LastResult == nil || s.LastResult.SearchResult == nil {
		return &Result{Text: h.formatter.FormatError("No recent search. Use /search first.", traceID), TraceID: traceID}, nil
	}
	book := bookByGlobalIndex(s, globalIndex)
	if book == nil {
		return &Result{Text: h.formatter.FormatError("Book selection is out of range.", traceID), TraceID: traceID}, nil
	}
	text, keyboard := h.formatter.FormatBookDetails(book)
	return &Result{Text: text, Keyboard: keyboard, TraceID: traceID, ForceSend: true}, nil
}

func (h *Handler) HandleBack(ctx context.Context, userID, traceID string) (*Result, error) {
	if traceID == "" {
		traceID = errutil.GenerateTraceID("tg")
	}
	s, ok := h.store.Get(ctx, userID)
	if !ok || s.LastResult == nil {
		return &Result{Text: h.formatter.FormatError("No recent search. Use /search first.", traceID), TraceID: traceID}, nil
	}
	text, keyboard, err := h.formatter.FormatSearchResult(s.LastResult, s.CurrentPage)
	if err != nil {
		return nil, err
	}
	return &Result{Text: text, Keyboard: keyboard, TraceID: traceID}, nil
}

func (h *Handler) HandleDownload(ctx context.Context, userID, sha1, traceID string) (*Result, error) {
	if traceID == "" {
		traceID = errutil.GenerateTraceID("tg")
	}
	s, ok := h.store.Get(ctx, userID)
	if !ok || s.LastResult == nil || s.LastResult.SearchResult == nil {
		return &Result{Text: h.formatter.FormatError("No recent search. Use /search first.", traceID), TraceID: traceID}, nil
	}
	book := bookBySHA1(s, sha1)
	if book == nil {
		return &Result{Text: h.formatter.FormatError("Book is unavailable.", traceID), TraceID: traceID}, nil
	}
	token := strings.TrimPrefix(book.DownloadURL, "/download/")
	if token == "" || token == book.DownloadURL {
		return &Result{Text: h.formatter.FormatError("Download is unavailable.", traceID), TraceID: traceID}, nil
	}
	meta, err := h.searcher.GetMeta(ctx, token, traceID)
	if err != nil {
		code, message, details := mapDownloadMetaError(err)
		logger.GetGlobal().WithFields(map[string]interface{}{
			"sha1":         sha1,
			"token":        token,
			"error_code":   code,
			"error_msg":    message,
			"error_detail": details,
		}).ErrorCtx(ctx, "telegram-bot download metadata lookup failed", err)
		return &Result{Text: h.formatter.FormatError(message, traceID), TraceID: traceID, ForceSend: true}, nil
	}
	if meta.Size >= 20*1024*1024 {
		return &Result{
			Text:      h.formatter.FormatError("File is too large to send through Telegram. Please use the web interface or contact the administrator.", traceID),
			TraceID:   traceID,
			ForceSend: true,
		}, nil
	}
	body, downloadMeta, err := h.searcher.DownloadBook(ctx, token, traceID)
	if err != nil {
		return &Result{Text: h.formatter.FormatError("Failed to download file.", traceID), TraceID: traceID, ForceSend: true}, nil
	}
	filename := meta.Filename
	if filename == "" && downloadMeta != nil {
		filename = downloadMeta.Filename
	}
	if filename == "" {
		filename = "book.fb2"
	}
	return &Result{
		Document: &Document{
			Filename: filename,
			Data:     bytes.NewReader(body),
			Caption:  book.Title,
		},
		TraceID:   traceID,
		ForceSend: true,
	}, nil
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

func bookByGlobalIndex(s *session.Session, globalIndex int) *corepresenter.BookDTO {
	if s == nil || s.LastResult == nil || s.LastResult.SearchResult == nil {
		return nil
	}
	offset := (s.CurrentPage - 1) * s.PageSize
	localIndex := globalIndex - offset - 1
	if localIndex < 0 || localIndex >= len(s.LastResult.Books) {
		return nil
	}
	return &s.LastResult.Books[localIndex]
}

func bookBySHA1(s *session.Session, sha1 string) *corepresenter.BookDTO {
	if s == nil || s.LastResult == nil || s.LastResult.SearchResult == nil {
		return nil
	}
	for i := range s.LastResult.Books {
		if s.LastResult.Books[i].ID == sha1 {
			return &s.LastResult.Books[i]
		}
	}
	return nil
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

func mapDownloadMetaError(err error) (code string, userMessage string, details string) {
	appErr, ok := err.(*errutil.AppError)
	if !ok {
		return errutil.CodeInternal, "Не удалось скачать книгу. Код ошибки: INTERNAL_ERROR. Попробуйте позже.", err.Error()
	}

	switch appErr.Code {
	case errutil.CodeNotFound:
		return appErr.Code, "Книга не найдена в хранилище. Возможно, она была удалена.", appErr.Message
	case errutil.CodeUnavailable, errutil.CodeTimeout:
		return appErr.Code, "Сервис временно недоступен. Пожалуйста, попробуйте позже.", appErr.Message
	case errutil.CodeInternal:
		return appErr.Code, "Внутренняя ошибка. Пожалуйста, сообщите администратору, указав trace_id.", appErr.Message
	default:
		code := appErr.Code
		if strings.TrimSpace(code) == "" {
			code = errutil.CodeInternal
		}
		return code, "Не удалось скачать книгу. Код ошибки: " + code + ". Попробуйте позже.", appErr.Message
	}
}
