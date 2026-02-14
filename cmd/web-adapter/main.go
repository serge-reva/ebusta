package main

import (
    "encoding/json"
    "log"
    "net/http"
    "strconv"

    "ebusta/internal/config"
    "ebusta/internal/errutil"
    "ebusta/internal/search"
)

func main() {
    cfg := config.Get()
    svc := search.NewService()

    http.HandleFunc("/search", func(w http.ResponseWriter, r *http.Request) {
        query := r.URL.Query().Get("q")
        limitStr := r.URL.Query().Get("limit")
        offsetStr := r.URL.Query().Get("offset")

        // Получаем или генерируем TraceID
        traceID := errutil.TraceIDFromRequest(r)
        if traceID == "" {
            traceID = errutil.GenerateTraceID("wa")
        }

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

        log.Printf("[%s] Incoming search: q=%s limit=%d offset=%d", traceID, query, limit, offset)

        resp, err := svc.Search(r.Context(), query, limit, offset, traceID)
        if err != nil {
            log.Printf("[%s] Error: %v", traceID, err)
            errutil.WriteJSONError(w, errutil.New(
                errutil.CodeInternal,
                err.Error(),
            ).WithTrace(traceID))
            return
        }

        w.Header().Set("X-Trace-Id", resp.TraceId)
        w.Header().Set("Content-Type", "application/json")
        json.NewEncoder(w).Encode(resp)
    })

    addr := cfg.WebAdapter.Address()
    log.Printf("🌐 Web-Adapter with errutil on %s", addr)
    log.Fatal(http.ListenAndServe(addr, nil))
}
