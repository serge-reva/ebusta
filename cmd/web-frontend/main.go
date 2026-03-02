package main

import (
	"context"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"time"

	"ebusta/internal/config"
	"ebusta/internal/logger"
	"github.com/prometheus/client_golang/prometheus/promhttp"
)

func main() {
	cfg := config.Get()
	logger.InitFromConfig(cfg.Logger, "web-frontend")
	if err := cfg.WebFrontend.Validate(); err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "web-frontend config validation failed", err)
	}
	if err := cfg.Metrics.Validate(); err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "metrics config validation failed", err)
	}

	wfCfg := cfg.WebFrontend

	h := &Handler{
		gatewayURL: strings.TrimRight(wfCfg.GatewayURL, "/"),
		httpClient: newGatewayClient(),
		pageSize:   wfCfg.PageSize,
	}

	mux := http.NewServeMux()
	mux.HandleFunc("/", h.handleIndex)
	mux.HandleFunc("/search", h.handleSearch)
	mux.HandleFunc("/download/", h.handleDownload)
	mux.HandleFunc("/health", h.handleHealth)
	mux.HandleFunc("/healthz", h.handleHealth)
	mux.Handle("/metrics", promhttp.Handler())

	addr := wfCfg.ListenAddr()
	server := &http.Server{
		Addr:    addr,
		Handler: mux,
	}

	stop := make(chan os.Signal, 1)
	signal.Notify(stop, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		logger.GetGlobal().WithField("addr", addr).InfoCtx(context.Background(), "[web-frontend] listening")
		if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			logger.GetGlobal().FatalCtx(context.Background(), "server error", err)
		}
	}()

	sig := <-stop
	logger.GetGlobal().WithField("signal", sig).InfoCtx(context.Background(), "[web-frontend] shutting down")

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	if err := server.Shutdown(ctx); err != nil {
		logger.GetGlobal().ErrorCtx(context.Background(), "[web-frontend] shutdown error", err)
	}

	logger.GetGlobal().InfoCtx(context.Background(), "[web-frontend] stopped")
}
