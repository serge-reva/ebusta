package validation

import (
	"net/http"

	"ebusta/internal/config"
	"ebusta/internal/edge"
)

type SizeLimiter struct {
	maxBodyBytes int64
	maxJSONDepth int
}

func NewSizeLimiter(cfg *config.GatewayValidationConfig) *SizeLimiter {
	return &SizeLimiter{
		maxBodyBytes: cfg.MaxBodyBytes,
		maxJSONDepth: cfg.MaxJSONDepth,
	}
}

func (l *SizeLimiter) LimitBody(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		r.Body = http.MaxBytesReader(w, r.Body, l.maxBodyBytes)
		next.ServeHTTP(w, r)
	})
}

func (l *SizeLimiter) ValidateJSON(body []byte) error {
	return edge.ValidateJSONPayload(body, l.maxBodyBytes, l.maxJSONDepth)
}
