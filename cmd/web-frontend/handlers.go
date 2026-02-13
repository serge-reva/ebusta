package main

import (
        "encoding/json"
        "fmt"
        "io"
        "net/http"
        "net/url"
        "strconv"
        "strings"
        "time"

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

        // Валидация запроса
        if err := search.ValidateQuery(query); err != nil {
                renderError(w, err.Error(), "")
                return
        }

        // Парсинг страницы
        page, _ := strconv.Atoi(pageStr)
        if page < 1 {
                page = 1
        }

        // Вычисление offset
        offset := (page - 1) * h.pageSize

        // Генерация TraceID
        traceID := fmt.Sprintf("wf-%d", time.Now().UnixNano())

        // Выполнение поиска
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

// handleDownload — скачивание файла через proxy к Downloader
func (h *Handler) handleDownload(w http.ResponseWriter, r *http.Request) {
        // Извлечение SHA1 из URL: /download/{sha1}
        sha1 := strings.TrimPrefix(r.URL.Path, "/download/")
        sha1 = strings.TrimSpace(sha1)

        if sha1 == "" {
                http.Error(w, "missing id", http.StatusBadRequest)
                return
        }

        // Валидация SHA1
        if !search.ValidateSHA1(sha1) {
                http.Error(w, "invalid sha1 (expected 40 hex)", http.StatusBadRequest)
                return
        }

        // Сначала получаем метаданные для формирования имени файла
        metaURL := fmt.Sprintf("http://%s/books/%s?meta=1", h.downloaderAddr, sha1)
        metaResp, err := http.Get(metaURL)
        if err != nil {
                http.Error(w, "downloader unavailable", http.StatusServiceUnavailable)
                return
        }
        defer metaResp.Body.Close()

        if metaResp.StatusCode != http.StatusOK {
                body, _ := io.ReadAll(metaResp.Body)
                http.Error(w, string(body), metaResp.StatusCode)
                return
        }

        // Парсим метаданные
        var meta struct {
                Sha1      string `json:"sha1"`
                Container string `json:"container"`
                Filename  string `json:"filename"`
                Size      int64  `json:"size"`
                Title     string `json:"title"`
        }
        if err := json.NewDecoder(metaResp.Body).Decode(&meta); err != nil {
                http.Error(w, "failed to parse metadata", http.StatusInternalServerError)
                return
        }

        // Формируем имя файла: {Container}_{Filename}
        downloadFilename := fmt.Sprintf("%s_%s", meta.Container, meta.Filename)

        // Прокси запрос на скачивание
        downloadURL := fmt.Sprintf("http://%s/books/%s", h.downloaderAddr, sha1)
        dlResp, err := http.Get(downloadURL)
        if err != nil {
                http.Error(w, "downloader unavailable", http.StatusServiceUnavailable)
                return
        }
        defer dlResp.Body.Close()

        if dlResp.StatusCode != http.StatusOK {
                body, _ := io.ReadAll(dlResp.Body)
                http.Error(w, string(body), dlResp.StatusCode)
                return
        }

        // Установка заголовков
        w.Header().Set("Content-Type", "application/octet-stream")
        w.Header().Set("Content-Disposition", fmt.Sprintf(`attachment; filename="%s"`, downloadFilename))
        if meta.Size > 0 {
                w.Header().Set("Content-Length", strconv.FormatInt(meta.Size, 10))
        }

        // Потоковая передача
        io.Copy(w, dlResp.Body)
}

func urlEscape(s string) string {
        return url.QueryEscape(s)
}
