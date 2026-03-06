package main

import (
	"fmt"
	"io"
	"mime"
	"net/http"
	"strconv"
	"strings"
	"time"

	"ebusta/internal/errutil"
	"ebusta/internal/gatewayclient"
	"ebusta/internal/logger"
)

type Handler struct {
	gatewayURL    string
	httpClient    *http.Client
	gatewayClient *gatewayclient.Client
	pageSize      int
}

type WebSearchResult struct {
	TraceID string
	Total   int
	Books   []WebBook
	Page    int
	Pages   int
}

type WebBook struct {
	Title       string
	FullAuthors string
	DownloadURL string
}

func (h *Handler) handleIndex(w http.ResponseWriter, r *http.Request) {
	if r.URL.Path != "/" {
		http.NotFound(w, r)
		return
	}
	renderIndex(w)
}

func (h *Handler) handleSearch(w http.ResponseWriter, r *http.Request) {
	query := strings.TrimSpace(r.URL.Query().Get("q"))
	pageStr := r.URL.Query().Get("page")

	traceID := errutil.TraceIDFromRequest(r)
	if traceID == "" {
		traceID = logger.GenerateTraceID("wf")
	}

	logger.GetGlobal().WithField("query", query).
		WithField("page", pageStr).
		WithField("trace_id", traceID).
		InfoCtx(r.Context(), "[web-frontend] search request")

	if query == "" {
		logger.GetGlobal().WithField("trace_id", traceID).WarnCtx(r.Context(), "[web-frontend] empty query")
		renderError(w, "Пустой запрос", traceID)
		return
	}

	page, _ := strconv.Atoi(pageStr)
	if page < 1 {
		page = 1
	}

	resp, err := h.gatewayClient.Search(r.Context(), query, page, h.pageSize, traceID)
	if err != nil {
		logger.GetGlobal().ErrorCtx(r.Context(), "[web-frontend] gateway search failed", err)
		if appErr, ok := err.(*errutil.AppError); ok {
			renderError(w, appErr.Message, appErr.TraceID)
			return
		}
		renderError(w, "Сервис поиска недоступен", traceID)
		return
	}

	result := &WebSearchResult{
		TraceID: resp.TraceID,
		Total:   resp.Total,
		Page:    resp.Page,
		Pages:   resp.Pages,
		Books:   make([]WebBook, 0, len(resp.Books)),
	}
	for _, book := range resp.Books {
		result.Books = append(result.Books, WebBook{
			Title:       book.Title,
			FullAuthors: book.FullAuthors,
			DownloadURL: book.DownloadURL,
		})
	}

	logger.GetGlobal().WithField("total", result.Total).
		WithField("returned", len(result.Books)).
		WithField("page", result.Page).
		WithField("total_pages", result.Pages).
		WithField("trace_id", traceID).
		InfoCtx(r.Context(), "[web-frontend] search completed")

	renderResults(w, result, query)
}

func (h *Handler) handleDownload(w http.ResponseWriter, r *http.Request) {
	traceID := errutil.TraceIDFromRequest(r)
	if traceID == "" {
		traceID = logger.GenerateTraceID("wf")
	}

	token := strings.TrimPrefix(r.URL.Path, "/download/")
	token = strings.TrimSpace(token)

	if token == "" {
		logger.GetGlobal().WithField("trace_id", traceID).
			WarnCtx(r.Context(), "[web-frontend] download missing token")
		errutil.WriteJSONError(w, errutil.New(
			errutil.CodeInvalidArgument,
			"missing token",
		).WithTrace(traceID))
		return
	}

	logger.GetGlobal().WithField("token", token).
		WithField("trace_id", traceID).
		InfoCtx(r.Context(), "[web-frontend] download request")

	downloadURL := fmt.Sprintf("%s/download/%s", h.gatewayURL, token)
	dlReq, err := http.NewRequestWithContext(r.Context(), http.MethodGet, downloadURL, nil)
	if err != nil {
		logger.GetGlobal().WithField("gateway", h.gatewayURL).
			ErrorCtx(r.Context(), "[web-frontend] gateway download request build failed", err)
		errutil.WriteJSONError(w, errutil.New(
			errutil.CodeInternal,
			"ошибка подготовки запроса скачивания",
		).WithTrace(traceID).WithDetails(err.Error()))
		return
	}
	dlReq.Header.Set("X-Trace-Id", traceID)

	dlResp, err := h.httpClient.Do(dlReq)
	if err != nil {
		logger.GetGlobal().WithField("gateway", h.gatewayURL).
			ErrorCtx(r.Context(), "[web-frontend] gateway unavailable", err)
		errutil.WriteJSONError(w, errutil.New(
			errutil.CodeUnavailable,
			"сервис скачивания недоступен",
		).WithTrace(traceID).WithDetails(err.Error()))
		return
	}
	defer dlResp.Body.Close()

	if dlResp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(dlResp.Body)
		appErr := errutil.FromHTTPResponse(dlResp, body, traceID)
		logger.GetGlobal().WithField("status", dlResp.StatusCode).
			WithField("trace_id", traceID).
			WarnCtx(r.Context(), "[web-frontend] gateway download error")
		errutil.WriteJSONError(w, appErr)
		return
	}

	filename := extractFilename(dlResp.Header.Get("Content-Disposition"))

	w.Header().Set("Content-Type", "application/octet-stream")
	w.Header().Set("Content-Disposition", fmt.Sprintf(`attachment; filename="%s"`, filename))
	w.Header().Set("X-Trace-Id", traceID)
	if contentLength := dlResp.Header.Get("Content-Length"); contentLength != "" {
		w.Header().Set("Content-Length", contentLength)
	}

	_, _ = io.Copy(w, dlResp.Body)

	logger.GetGlobal().WithField("token", token).
		WithField("filename", filename).
		WithField("trace_id", traceID).
		InfoCtx(r.Context(), "[web-frontend] download completed")
}

func extractFilename(contentDisposition string) string {
	if contentDisposition == "" {
		return "unknown.bin"
	}

	_, params, err := mime.ParseMediaType(contentDisposition)
	if err != nil {
		return "unknown.bin"
	}

	filename := params["filename"]
	if filename == "" {
		return "unknown.bin"
	}

	return filename
}

func (h *Handler) handleHealth(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	_, _ = w.Write([]byte(`{"status":"ok"}`))
}

func newGatewayClient() *http.Client {
	return &http.Client{
		Timeout: 30 * time.Second,
	}
}
