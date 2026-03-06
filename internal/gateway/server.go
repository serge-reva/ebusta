package gateway

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"strings"
	"time"

	"ebusta/internal/config"
	"ebusta/internal/gateway/clients"
	"ebusta/internal/gateway/download"
	"ebusta/internal/gateway/mapper"
	"ebusta/internal/gateway/middleware"
	"ebusta/internal/gateway/validation"
	"ebusta/internal/logger"
	"github.com/prometheus/client_golang/prometheus/promhttp"
)

type Server struct {
	config      *config.GatewayRuntimeConfig
	mapper      *mapper.Mapper
	validator   *validation.Validator
	sizeLimiter *validation.SizeLimiter
	sanitizer   *validation.Sanitizer

	orchestrator *clients.OrchestratorClient
	downloader   *clients.DownloaderClient
	downloadMode string

	downloadBackend download.DownloadBackend

	rateLimiter *middleware.RateLimiter
	cors        *middleware.CORS
	security    *middleware.SecurityHeaders
	csrf        *middleware.CSRFProtection
	contentType *middleware.ContentTypeValidation
	recover     *middleware.Recover

	httpServer *http.Server
}

func NewServer(cfg *config.GatewayRuntimeConfig) (*Server, error) {
	mapper := mapper.NewMapper(&cfg.Mapper)
	validator := validation.NewValidator()
	sizeLimiter := validation.NewSizeLimiter(&cfg.Validation)
	sanitizer := validation.NewSanitizer()

	orchestrator, err := clients.NewOrchestratorClient(cfg)
	if err != nil {
		return nil, fmt.Errorf("failed to create orchestrator client: %w", err)
	}

	downloader := clients.NewDownloaderClient(cfg.Services.Downloader)
	downloadMode := strings.ToLower(strings.TrimSpace(cfg.DownloadMode))
	if downloadMode == "" {
		downloadMode = "direct"
	}

	var downloadBackend download.DownloadBackend
	switch downloadMode {
	case "direct":
		downloadBackend = download.NewDirectBackend(mapper, downloader)
	default:
		return nil, fmt.Errorf("unsupported gateway download_mode: %s", downloadMode)
	}

	rateLimiter := middleware.NewRateLimiter(cfg)
	cors := middleware.NewCORS(&cfg.CORS)
	security := &middleware.SecurityHeaders{}
	csrf := &middleware.CSRFProtection{}
	contentType := &middleware.ContentTypeValidation{}
	recover := &middleware.Recover{}

	s := &Server{
		config:          cfg,
		mapper:          mapper,
		validator:       validator,
		sizeLimiter:     sizeLimiter,
		sanitizer:       sanitizer,
		orchestrator:    orchestrator,
		downloader:      downloader,
		downloadMode:    downloadMode,
		downloadBackend: downloadBackend,
		rateLimiter:     rateLimiter,
		cors:            cors,
		security:        security,
		csrf:            csrf,
		contentType:     contentType,
		recover:         recover,
	}

	return s, nil
}

func (s *Server) setupRoutes() http.Handler {
	mux := http.NewServeMux()

	mux.HandleFunc("/health", s.handleHealth)
	mux.HandleFunc("/search", s.handleSearch)
	mux.HandleFunc("/download/", s.handleDownload)
	mux.HandleFunc("/download/token/", s.handleDownloadToken)
	mux.HandleFunc("/debug/mapper", s.handleDebug)
	mux.Handle("/metrics", promhttp.Handler())

	handler := s.recover.Middleware(mux)
	handler = s.security.Middleware(handler)
	handler = s.cors.Middleware(handler)
	handler = s.rateLimiter.Middleware(handler)
	handler = s.contentType.Middleware(handler)
	handler = s.sizeLimiter.LimitBody(handler)

	return handler
}

func (s *Server) Run() error {
	addr := fmt.Sprintf("%s:%d", s.config.ListenHost, s.config.Port)
	handler := s.setupRoutes()

	s.httpServer = &http.Server{
		Addr:         addr,
		Handler:      handler,
		ReadTimeout:  5 * time.Second,
		WriteTimeout: 10 * time.Second,
		IdleTimeout:  120 * time.Second,
	}

	logger.GetGlobal().WithField("addr", addr).WithField("tls", s.config.TLSCert != "").InfoCtx(context.Background(), "gateway starting")

	if s.config.TLSCert != "" && s.config.TLSKey != "" {
		return s.httpServer.ListenAndServeTLS(s.config.TLSCert, s.config.TLSKey)
	}
	return s.httpServer.ListenAndServe()
}

func (s *Server) Shutdown(ctx context.Context) error {
	s.mapper.Stop()
	s.orchestrator.Close()
	return s.httpServer.Shutdown(ctx)
}

func (s *Server) handleHealth(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(map[string]string{
		"status": "ok",
		"time":   time.Now().Format(time.RFC3339),
	})
}
