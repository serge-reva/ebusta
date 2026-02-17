package main

import (
    "fmt"
    "io"
    "mime"
    "net/http"
    "net/url"
    "strconv"
    "strings"

    "ebusta/internal/errutil"
    "ebusta/internal/logger"
    "ebusta/internal/search"
)

type Handler struct {
    searchSvc      *search.Service
    downloaderAddr string
    pageSize       int
}

func (h *Handler) handleIndex(w http.ResponseWriter, r *http.Request) {
    if r.URL.Path != "/" {
        http.NotFound(w, r)
        return
    }
    renderIndex(w)
}

func (h *Handler) handleSearch(w http.ResponseWriter, r *http.Request) {
    query := r.URL.Query().Get("q")
    pageStr := r.URL.Query().Get("page")

    traceID := errutil.TraceIDFromRequest(r)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("wf")
    }

    logger.GetGlobal().WithField("query", query).
        WithField("page", pageStr).
        WithField("trace_id", traceID).
        InfoCtx(r.Context(), "[web-frontend] search request")

    if err := search.ValidateQuery(query); err != nil {
        logger.GetGlobal().WithField("error", err.Error()).
            WithField("query", query).
            WarnCtx(r.Context(), "[web-frontend] invalid query")
        renderError(w, err.Error(), traceID)
        return
    }

    page, _ := strconv.Atoi(pageStr)
    if page < 1 {
        page = 1
    }

    offset := (page - 1) * h.pageSize

    result, err := h.searchSvc.Search(r.Context(), query, h.pageSize, offset, traceID)
    if err != nil {
        logger.GetGlobal().WithField("query", query).
            ErrorCtx(r.Context(), "[web-frontend] search error", err)
        renderError(w, "Ошибка поиска", traceID)
        return
    }

    totalPages := (result.Total + h.pageSize - 1) / h.pageSize
    if totalPages == 0 {
        totalPages = 1
    }
    page = search.ValidatePage(page, totalPages)

    logger.GetGlobal().WithField("total", result.Total).
        WithField("returned", len(result.Books)).
        WithField("page", page).
        WithField("trace_id", traceID).
        InfoCtx(r.Context(), "[web-frontend] search completed")

    renderResults(w, result, query, page, totalPages)
}

func (h *Handler) handleDownload(w http.ResponseWriter, r *http.Request) {
    traceID := errutil.TraceIDFromRequest(r)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("wf")
    }

    sha1 := strings.TrimPrefix(r.URL.Path, "/download/")
    sha1 = strings.TrimSpace(sha1)

    if sha1 == "" {
        logger.GetGlobal().WithField("trace_id", traceID).
            WarnCtx(r.Context(), "[web-frontend] download missing id")
        errutil.WriteJSONError(w, errutil.New(
            errutil.CodeInvalidArgument,
            "missing id",
        ).WithTrace(traceID))
        return
    }

    if !search.ValidateSHA1(sha1) {
        logger.GetGlobal().WithField("sha1", sha1).
            WithField("trace_id", traceID).
            WarnCtx(r.Context(), "[web-frontend] invalid sha1")
        errutil.WriteJSONError(w, errutil.New(
            errutil.CodeInvalidArgument,
            "invalid sha1 (expected 40 hex)",
        ).WithTrace(traceID))
        return
    }

    logger.GetGlobal().WithField("sha1", sha1).
        WithField("trace_id", traceID).
        InfoCtx(r.Context(), "[web-frontend] download request")

    downloadURL := fmt.Sprintf("http://%s/books/%s", h.downloaderAddr, sha1)
    dlResp, err := http.Get(downloadURL)
    if err != nil {
        logger.GetGlobal().WithField("downloader", h.downloaderAddr).
            ErrorCtx(r.Context(), "[web-frontend] downloader unavailable", err)
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
            WarnCtx(r.Context(), "[web-frontend] downloader error")
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

    io.Copy(w, dlResp.Body)

    logger.GetGlobal().WithField("sha1", sha1).
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
    w.Write([]byte(`{"status":"ok"}`))
}

func urlEscape(s string) string {
    return url.QueryEscape(s)
}
