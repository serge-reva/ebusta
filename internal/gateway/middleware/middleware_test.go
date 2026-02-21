package middleware

import (
    "net/http"
    "net/http/httptest"
    "testing"
    
    "ebusta/internal/gateway/config"
)

func TestRateLimiter(t *testing.T) {
    cfg := &config.RateLimitConfig{
        IP:      60,
        Resolve: 30,
    }
    
    limiter := NewRateLimiter(cfg)
    
    handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        w.WriteHeader(http.StatusOK)
    })
    
    wrapped := limiter.Middleware(handler)
    
    req := httptest.NewRequest("GET", "/health", nil)
    req.RemoteAddr = "192.168.1.1:12345"
    
    w := httptest.NewRecorder()
    wrapped.ServeHTTP(w, req)
    
    if w.Code != http.StatusOK {
        t.Errorf("expected status OK, got %v", w.Code)
    }
}

func TestCORS(t *testing.T) {
    cfg := &config.CORSConfig{
        AllowedOrigins: []string{"https://example.com"},
        AllowedMethods: []string{"GET", "POST"},
        AllowedHeaders: []string{"Content-Type"},
    }
    
    cors := NewCORS(cfg)
    
    handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        w.WriteHeader(http.StatusOK)
    })
    
    wrapped := cors.Middleware(handler)
    
    req := httptest.NewRequest("OPTIONS", "/search", nil)
    req.Header.Set("Origin", "https://example.com")
    
    w := httptest.NewRecorder()
    wrapped.ServeHTTP(w, req)
    
    if w.Code != http.StatusNoContent {
        t.Errorf("expected status NoContent, got %v", w.Code)
    }
}
