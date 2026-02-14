package main

import (
    "encoding/json"
    "fmt"
    "log"
    "net/http"
    "strconv"
    "time"

    "ebusta/internal/config"
    "ebusta/internal/search"
)

func main() {
    cfg := config.Get()
    svc := search.NewService()

    http.HandleFunc("/search", func(w http.ResponseWriter, r *http.Request) {
        query := r.URL.Query().Get("q")
        limitStr := r.URL.Query().Get("limit")
        offsetStr := r.URL.Query().Get("offset")

        // Парсинг limit (по умолчанию 20)
        limit := 20
        if l, err := strconv.Atoi(limitStr); err == nil && l > 0 {
            limit = l
        }

        // Парсинг offset (по умолчанию 0)
        offset := 0
        if o, err := strconv.Atoi(offsetStr); err == nil && o >= 0 {
            offset = o
        }

        tid := r.Header.Get("X-Trace-Id")
        if tid == "" {
            tid = fmt.Sprintf("web-%d", time.Now().UnixNano())
        }

        log.Printf("[%s] Incoming search: q=%s limit=%d offset=%d", tid, query, limit, offset)

        resp, err := svc.Search(r.Context(), query, limit, offset, tid)
        if err != nil {
            log.Printf("[%s] Error: %v", tid, err)
            http.Error(w, err.Error(), http.StatusInternalServerError)
            return
        }

        w.Header().Set("X-Trace-Id", resp.TraceId)
        w.Header().Set("Content-Type", "application/json")
        json.NewEncoder(w).Encode(resp)
    })

    addr := fmt.Sprintf(":%d", cfg.WebAdapter.Port)
    log.Printf("🌐 Web-Adapter with TraceID on %s", addr)
    log.Fatal(http.ListenAndServe(addr, nil))
}
