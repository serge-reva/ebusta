package main

import (
	"context"
	"flag"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"ebusta/internal/config"
	"ebusta/internal/edge"
	"ebusta/internal/logger"
	"github.com/prometheus/client_golang/prometheus/promhttp"
)

func main() {
	var configPath string
	var verbose bool

	flag.StringVar(&configPath, "config", "ebusta.yaml", "path to config file")
	flag.BoolVar(&verbose, "verbose", false, "enable verbose output")
	flag.Parse()

	os.Setenv("EBUSTA_CONFIG", configPath)
	cfg := config.Get()
	if err := cfg.Metrics.Validate(); err != nil {
		log.Fatalf("json-gateway metrics config validation failed: %v", err)
	}
	tgCfg := loadTelegramConfig(cfg, verbose)

	logCfg := cfg.Logger
	if tgCfg.Debug {
		logCfg.Level = "DEBUG"
	}
	logger.InitFromConfig(logCfg, "json-gateway")
	if err := tgCfg.Validate(); err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "json-gateway config validation failed", err)
	}

	labelHook := edge.NewLabelCounterHook()
	policy := edge.PolicyFromConfig(cfg, "telegram")
	handler := NewTGHandler(tgCfg.GatewayURL, tgCfg.PageSize, edge.NewEngine(policy, edge.NewMultiHook(labelHook, &edge.OTelHook{})))

	mux := http.NewServeMux()
	mux.HandleFunc("/health", handler.handleHealth)
	mux.HandleFunc("/update", handler.handleUpdate)
	mux.Handle("/metrics", promhttp.Handler())

	srv := &http.Server{
		Addr:         tgCfg.ListenAddr(),
		Handler:      mux,
		ReadTimeout:  5 * time.Second,
		WriteTimeout: 10 * time.Second,
		IdleTimeout:  120 * time.Second,
	}

	stop := make(chan os.Signal, 1)
	signal.Notify(stop, syscall.SIGINT, syscall.SIGTERM, syscall.SIGHUP)

	go func() {
		logger.GetGlobal().WithField("addr", tgCfg.ListenAddr()).InfoCtx(context.Background(), "json gateway started")
		if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			logger.FatalCtx(context.Background(), "json gateway failed", err)
		}
	}()

	sig := <-stop
	if sig == syscall.SIGHUP {
		if cfg.Edge.ReloadMode == "sighup" {
			logger.InfoCtx(context.Background(), "SIGHUP received: safe restart is required for reload")
		} else {
			logger.InfoCtx(context.Background(), "SIGHUP received: reload_mode is restart-based")
		}
		return
	}

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	if err := srv.Shutdown(ctx); err != nil {
		logger.ErrorCtx(context.Background(), "json gateway shutdown error", err)
	}
	logger.InfoCtx(context.Background(), "json gateway stopped")
}

func loadTelegramConfig(cfg *config.Config, verboseFlag bool) *config.TelegramAdapterConfig {
	tg := cfg.TelegramAdapter

	if tg.ListenHost == "" {
		tg.ListenHost = tg.Host
	}
	if tg.ListenHost == "" {
		tg.ListenHost = "0.0.0.0"
	}
	if tg.Port == 0 {
		tg.Port = 8087
	}
	if tg.GatewayURL == "" {
		tg.GatewayURL = "http://localhost:8443"
	}
	if tg.PageSize <= 0 {
		tg.PageSize = 5
	}
	if verboseFlag {
		tg.Debug = true
	}

	logger.GetGlobal().WithField("addr", tg.ListenAddr()).WithField("debug", tg.Debug).InfoCtx(context.Background(), "json gateway configuration loaded")
	return &tg
}
