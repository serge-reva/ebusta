package middleware

import (
	"net"
	"net/http"
	"strings"

	"ebusta/internal/config"
	"ebusta/internal/edge"
	"ebusta/internal/logger"
)

type RateLimiter struct {
	throttler *edge.Throttler
}

func NewRateLimiter(cfg *config.GatewayRateLimitConfig) *RateLimiter {
	t := edge.NewThrottler()
	t.Register("ip", edge.ThrottleRule{
		PerMinute: cfg.IP,
		Burst:     cfg.IP,
	})
	t.Register("resolve", edge.ThrottleRule{
		PerMinute: cfg.Resolve,
		Burst:     cfg.Resolve,
	})
	return &RateLimiter{
		throttler: t,
	}
}

func (rl *RateLimiter) Middleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		ip := realIP(r)

		if !rl.throttler.Allow("ip", ip) {
			logger.WarnCtx(r.Context(), "rate limit exceeded")
			http.Error(w, "rate limit exceeded", http.StatusTooManyRequests)
			return
		}

		if strings.Contains(r.URL.Path, "/download/") {
			if !rl.throttler.Allow("resolve", ip) {
				logger.WarnCtx(r.Context(), "resolve rate limit exceeded")
				http.Error(w, "too many download attempts", http.StatusTooManyRequests)
				return
			}
		}

		next.ServeHTTP(w, r)
	})
}

func realIP(r *http.Request) string {
	if fwd := r.Header.Get("X-Forwarded-For"); fwd != "" {
		parts := strings.Split(fwd, ",")
		return strings.TrimSpace(parts[0])
	}

	if real := r.Header.Get("X-Real-IP"); real != "" {
		return real
	}

	ip, _, err := net.SplitHostPort(r.RemoteAddr)
	if err != nil {
		return r.RemoteAddr
	}
	return ip
}
