package middleware

import (
	"net/http"
	"net/http/httptest"
	"testing"

	"ebusta/internal/config"
)

func TestRateLimiter(t *testing.T) {
	cfg := &config.GatewayRateLimitConfig{
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

func TestRateLimiterExceeds(t *testing.T) {
	cfg := &config.GatewayRateLimitConfig{
		IP:      1,
		Resolve: 1,
	}

	limiter := NewRateLimiter(cfg)

	handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
	})

	wrapped := limiter.Middleware(handler)

	req1 := httptest.NewRequest("GET", "/health", nil)
	req1.RemoteAddr = "192.168.1.55:12345"
	w1 := httptest.NewRecorder()
	wrapped.ServeHTTP(w1, req1)
	if w1.Code != http.StatusOK {
		t.Fatalf("first request should pass, got %d", w1.Code)
	}

	req2 := httptest.NewRequest("GET", "/health", nil)
	req2.RemoteAddr = "192.168.1.55:12345"
	w2 := httptest.NewRecorder()
	wrapped.ServeHTTP(w2, req2)
	if w2.Code != http.StatusTooManyRequests {
		t.Fatalf("second request should be throttled, got %d", w2.Code)
	}
}

func TestCORS(t *testing.T) {
	cfg := &config.GatewayCORSConfig{
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
