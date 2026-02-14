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
    "ebusta/internal/search"
)

type Handler struct {
    searchSvc      *search.Service
    downloaderAddr string
    pageSize       int
}

// handleIndex — главная страница с формой поиска
func (h *Handler) handleIndex(w http.ResponseWriter, r *http.Request) {
    if r.URL.Path != "/" {
        http.NotFound(w, r)
        return
    }
    renderIndex(w)
}

// handleSearch — обработка поиска
func (h *Handler) handleSearch(w http.ResponseWriter, r *http.Request) {
    query := r.URL.Query().Get("q")
    pageStr := r.URL.Query().Get("page")

    // Получаем или генерируем TraceID
    traceID := errutil.TraceIDFromRequest(r)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("wf")
    }

    // Валидация запроса
    if err := search.ValidateQuery(query); err != nil {
        renderError(w, err.Error(), traceID)
        return
    }

    // Парсинг страницы
    page, _ := strconv.Atoi(pageStr)
    if page < 1 {
        page = 1
    }

    // Вычисление offset для пагинации
    offset := (page - 1) * h.pageSize

    // Выполнение поиска с offset
    result, err := h.searchSvc.Search(r.Context(), query, h.pageSize, offset, traceID)
    if err != nil {
        renderError(w, "Ошибка поиска", traceID)
        return
    }

    // Валидация страницы
    totalPages := (result.Total + h.pageSize - 1) / h.pageSize
    if totalPages == 0 {
        totalPages = 1
    }
    page = search.ValidatePage(page, totalPages)

    renderResults(w, result, query, page, totalPages)
}

// handleDownload — скачивание файла (один HTTP-запрос к Downloader)
// Согласно ТЗ v1.4: имя файла извлекается из Content-Disposition, формат {Filename}
// Согласно ТЗ errutil: возвращает JSON-ошибки с TraceID
func (h *Handler) handleDownload(w http.ResponseWriter, r *http.Request) {
    // Получаем или генерируем TraceID
    traceID := errutil.TraceIDFromRequest(r)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("wf")
    }

    // Извлечение SHA1 из URL: /download/{sha1}
    sha1 := strings.TrimPrefix(r.URL.Path, "/download/")
    sha1 = strings.TrimSpace(sha1)

    if sha1 == "" {
        errutil.WriteJSONError(w, errutil.New(
            errutil.CodeInvalidArgument,
            "missing id",
        ).WithTrace(traceID))
        return
    }

    // Валидация SHA1
    if !search.ValidateSHA1(sha1) {
        errutil.WriteJSONError(w, errutil.New(
            errutil.CodeInvalidArgument,
            "invalid sha1 (expected 40 hex)",
        ).WithTrace(traceID))
        return
    }

    // Один запрос к Downloader
    downloadURL := fmt.Sprintf("http://%s/books/%s", h.downloaderAddr, sha1)
    dlResp, err := http.Get(downloadURL)
    if err != nil {
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
        errutil.WriteJSONError(w, appErr)
        return
    }

    // Извлечение имени файла из Content-Disposition
    // Формат: attachment; filename="king_arrow.fb2"
    filename := extractFilename(dlResp.Header.Get("Content-Disposition"))

    // Установка заголовков ответа
    w.Header().Set("Content-Type", "application/octet-stream")
    w.Header().Set("Content-Disposition", fmt.Sprintf(`attachment; filename="%s"`, filename))
    w.Header().Set("X-Trace-Id", traceID)
    if contentLength := dlResp.Header.Get("Content-Length"); contentLength != "" {
        w.Header().Set("Content-Length", contentLength)
    }

    // Потоковая передача
    io.Copy(w, dlResp.Body)
}

// extractFilename извлекает имя файла из Content-Disposition заголовка
// Ожидаемый формат: attachment; filename="king_arrow.fb2"
// Возвращает только имя файла (без контейнера), например: "king_arrow.fb2"
func extractFilename(contentDisposition string) string {
    if contentDisposition == "" {
        return "unknown.bin"
    }

    // Используем mime.ParseMediaType для корректного парсинга
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

// handleHealth — health check endpoint для мониторинга
func (h *Handler) handleHealth(w http.ResponseWriter, r *http.Request) {
    w.Header().Set("Content-Type", "application/json")
    w.WriteHeader(http.StatusOK)
    w.Write([]byte(`{"status":"ok"}`))
}

func urlEscape(s string) string {
    return url.QueryEscape(s)
}
