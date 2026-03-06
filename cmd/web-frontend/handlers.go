package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"mime"
	"net/http"
	"strconv"
	"strings"
	"time"

	"ebusta/internal/errutil"
	"ebusta/internal/logger"
)

type Handler struct {
	gatewayURL string
	httpClient *http.Client
	pageSize   int
}

type gatewaySearchRequest struct {
	Query string `json:"query"`
	Page  int    `json:"page"`
	Limit int    `json:"limit"`
}

type gatewaySearchResponse struct {
	TraceID string              `json:"trace_id"`
	Total   int                 `json:"total"`
	Books   []gatewaySearchBook `json:"books"`
	Page    int                 `json:"page"`
	Pages   int                 `json:"pages"`
}

type gatewaySearchBook struct {
	Title       string `json:"title"`
	FullAuthors string `json:"full_authors"`
	DownloadURL string `json:"download_url"`
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

	reqPayload := gatewaySearchRequest{
		Query: query,
		Page:  page,
		Limit: h.pageSize,
	}
	body, err := json.Marshal(reqPayload)
	if err != nil {
		logger.GetGlobal().ErrorCtx(r.Context(), "[web-frontend] search payload marshal failed", err)
		renderError(w, "Ошибка поиска", traceID)
		return
	}

	searchReq, err := http.NewRequestWithContext(r.Context(), http.MethodPost, h.gatewayURL+"/search", bytes.NewReader(body))
	if err != nil {
		logger.GetGlobal().ErrorCtx(r.Context(), "[web-frontend] search request build failed", err)
		renderError(w, "Ошибка поиска", traceID)
		return
	}
	searchReq.Header.Set("Content-Type", "application/json")
	searchReq.Header.Set("X-Trace-Id", traceID)

	resp, err := h.httpClient.Do(searchReq)
	if err != nil {
		logger.GetGlobal().ErrorCtx(r.Context(), "[web-frontend] gateway search unavailable", err)
		renderError(w, "Сервис поиска недоступен", traceID)
		return
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		respBody, _ := io.ReadAll(resp.Body)
		appErr := errutil.FromHTTPResponse(resp, respBody, traceID)
		logger.GetGlobal().WithField("status", resp.StatusCode).WarnCtx(r.Context(), "[web-frontend] gateway search error")
		renderError(w, appErr.Message, appErr.TraceID)
		return
	}

	var gwResp gatewaySearchResponse
	if err := json.NewDecoder(resp.Body).Decode(&gwResp); err != nil {
		logger.GetGlobal().ErrorCtx(r.Context(), "[web-frontend] decode gateway search failed", err)
		renderError(w, "Ошибка поиска", traceID)
		return
	}

	result := &WebSearchResult{
		TraceID: gwResp.TraceID,
		Total:   gwResp.Total,
		Page:    gwResp.Page,
		Pages:   gwResp.Pages,
		Books:   make([]WebBook, 0, len(gwResp.Books)),
	}
	for _, book := range gwResp.Books {
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
