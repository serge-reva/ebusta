package main

import (
	"context"
	"encoding/json"
	"net/http"
	"strconv"

	"ebusta/internal/config"
	"ebusta/internal/errutil"
	"ebusta/internal/logger"
	"ebusta/internal/search"
	"github.com/prometheus/client_golang/prometheus/promhttp"
)

func main() {
	// Debug-only adapter: used for manual/regression checks, not as production entrypoint.
	cfg := config.Get()
	logger.InitFromConfig(cfg.Logger, "web-adapter")

	svc := search.NewService()

	mux := http.NewServeMux()
	mux.Handle("/metrics", promhttp.Handler())
	mux.HandleFunc("/search", func(w http.ResponseWriter, r *http.Request) {
		query := r.URL.Query().Get("q")
		limitStr := r.URL.Query().Get("limit")
		offsetStr := r.URL.Query().Get("offset")

		traceID := errutil.TraceIDFromRequest(r)
		if traceID == "" {
			traceID = errutil.GenerateTraceID("wa")
		}

		limit := 20
		if l, err := strconv.Atoi(limitStr); err == nil && l > 0 {
			limit = l
		}

		offset := 0
		if o, err := strconv.Atoi(offsetStr); err == nil && o >= 0 {
			offset = o
		}

		logger.GetGlobal().WithField("query", query).
			WithField("limit", limit).
			WithField("offset", offset).
			WithField("trace_id", traceID).
			InfoCtx(r.Context(), "[web-adapter] incoming search")

		resp, err := svc.Search(r.Context(), query, limit, offset, traceID)
		if err != nil {
			logger.GetGlobal().WithField("query", query).
				WithField("trace_id", traceID).
				ErrorCtx(r.Context(), "[web-adapter] search error", err)
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

	addr := cfg.WebAdapter.ListenAddress()
	logger.GetGlobal().WithField("addr", addr).InfoCtx(context.Background(), "[web-adapter] listening")
	if err := http.ListenAndServe(addr, mux); err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "failed to serve", err)
	}
}
