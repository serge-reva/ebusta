package metrics

import (
	"context"
	"fmt"
	"net/http"
	"time"

	"ebusta/internal/logger"
	"github.com/prometheus/client_golang/prometheus/promhttp"
)

func Start(component string, port int) *http.Server {
	if port <= 0 {
		return nil
	}

	mux := http.NewServeMux()
	mux.Handle("/metrics", promhttp.Handler())
	mux.HandleFunc("/health", func(w http.ResponseWriter, _ *http.Request) {
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write([]byte("ok"))
	})

	addr := fmt.Sprintf(":%d", port)
	srv := &http.Server{
		Addr:              addr,
		Handler:           mux,
		ReadHeaderTimeout: 3 * time.Second,
	}

	go func() {
		logger.GetGlobal().WithField("component", component).WithField("addr", addr).InfoCtx(context.Background(), "metrics server started")
		if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			logger.GetGlobal().WithField("component", component).ErrorCtx(context.Background(), "metrics server failed", err)
		}
	}()

	return srv
}

func Shutdown(ctx context.Context, srv *http.Server) {
	if srv == nil {
		return
	}
	_ = srv.Shutdown(ctx)
}
