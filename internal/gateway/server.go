package gateway

import (
    "context"
    "encoding/json"
    "fmt"
    "net/http"
    "time"

    "ebusta/internal/errutil"
    "ebusta/internal/gateway/clients"
    "ebusta/internal/gateway/mapper"
    "ebusta/internal/gateway/middleware"
    "ebusta/internal/gateway/validation"
    "ebusta/internal/logger"
)

type Server struct {
    config          *Config
    mapper          *mapper.Mapper
    validator       *validation.Validator
    sizeLimiter     *validation.SizeLimiter
    sanitizer       *validation.Sanitizer
    
    orchestrator    *clients.OrchestratorClient
    downloader      *clients.DownloaderClient
    
    rateLimiter     *middleware.RateLimiter
    cors            *middleware.CORS
    security        *middleware.SecurityHeaders
    csrf            *middleware.CSRFProtection
    contentType     *middleware.ContentTypeValidation
    recover         *middleware.Recover
    
    httpServer      *http.Server
}

type SearchResponse struct {
    TraceID string         `json:"trace_id"`
    Total   int            `json:"total"`
    Books   []BookResponse `json:"books"`
    Page    int            `json:"page"`
    Pages   int            `json:"pages"`
}

type BookResponse struct {
    Title       string   `json:"title"`
    Authors     []string `json:"authors"`
    FullAuthors string   `json:"full_authors"`
    DownloadURL string   `json:"download_url"` // /download/{token}
}

type DownloadResponse struct {
    Token     string `json:"token"`
    ExpiresIn int64  `json:"expires_in"` // seconds
    Size      int64  `json:"size"`
    Filename  string `json:"filename"`
}

func NewServer(cfg *Config) (*Server, error) {
    // Инициализация компонентов
    mapper := mapper.NewMapper(&cfg.Mapper)
    validator := validation.NewValidator()
    sizeLimiter := validation.NewSizeLimiter(&cfg.Validation)
    sanitizer := validation.NewSanitizer()
    
    // Клиенты
    orchestrator, err := clients.NewOrchestratorClient(cfg)
    if err != nil {
        return nil, fmt.Errorf("failed to create orchestrator client: %w", err)
    }
    
    downloader := clients.NewDownloaderClient(cfg)
    
    // Middleware
    rateLimiter := middleware.NewRateLimiter(&cfg.RateLimit)
    cors := middleware.NewCORS(&cfg.CORS)
    security := &middleware.SecurityHeaders{}
    csrf := &middleware.CSRFProtection{}
    contentType := &middleware.ContentTypeValidation{}
    recover := &middleware.Recover{}
    
    s := &Server{
        config:       cfg,
        mapper:       mapper,
        validator:    validator,
        sizeLimiter:  sizeLimiter,
        sanitizer:    sanitizer,
        orchestrator: orchestrator,
        downloader:   downloader,
        rateLimiter:  rateLimiter,
        cors:         cors,
        security:     security,
        csrf:         csrf,
        contentType:  contentType,
        recover:      recover,
    }
    
    return s, nil
}

func (s *Server) setupRoutes() http.Handler {
    mux := http.NewServeMux()
    
    // Публичные endpoints
    mux.HandleFunc("/health", s.handleHealth)
    mux.HandleFunc("/search", s.handleSearch)
    mux.HandleFunc("/download/", s.handleDownload)
    
    // Цепочка middleware
    handler := s.recover.Middleware(mux)
    handler = s.security.Middleware(handler)
    handler = s.cors.Middleware(handler)
    handler = s.rateLimiter.Middleware(handler)
    handler = s.contentType.Middleware(handler)
    
    return handler
}

func (s *Server) Run() error {
    addr := fmt.Sprintf(":%d", s.config.Port)
    handler := s.setupRoutes()
    
    s.httpServer = &http.Server{
        Addr:         addr,
        Handler:      handler,
        ReadTimeout:  5 * time.Second,
        WriteTimeout: 10 * time.Second,
        IdleTimeout:  120 * time.Second,
    }
    
    logger.InfoCtx(context.Background(), "gateway starting",
        "port", s.config.Port,
        "tls", s.config.TLSCert != "")
    
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
