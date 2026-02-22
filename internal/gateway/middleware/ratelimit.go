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
	engine  *edge.Engine
	metrics *edge.LabelCounterHook
}

func NewRateLimiter(cfg *config.GatewayRuntimeConfig) *RateLimiter {
	p := edge.DefaultPolicy("gateway")
	if cfg.EdgePolicy.MaxLineLength > 0 {
		p.MaxLineLength = cfg.EdgePolicy.MaxLineLength
	}
	if cfg.EdgePolicy.MaxBodyBytes > 0 {
		p.MaxBodyBytes = cfg.EdgePolicy.MaxBodyBytes
	}
	if cfg.EdgePolicy.MaxJSONDepth > 0 {
		p.MaxJSONDepth = cfg.EdgePolicy.MaxJSONDepth
	}
	p.Actions = map[string]edge.ActionPolicy{
		"ip": {
			PerMinute: cfg.RateLimit.IP,
			Burst:     cfg.RateLimit.IP,
		},
		"resolve": {
			PerMinute: cfg.RateLimit.Resolve,
			Burst:     cfg.RateLimit.Resolve,
		},
	}
	if ac, ok := cfg.EdgePolicy.Actions["ip"]; ok {
		p.Actions["ip"] = edge.ActionPolicy{PerMinute: ac.PerMinute, Burst: ac.Burst}
	}
	if ac, ok := cfg.EdgePolicy.Actions["resolve"]; ok {
		p.Actions["resolve"] = edge.ActionPolicy{PerMinute: ac.PerMinute, Burst: ac.Burst}
	}

	labelHook := edge.NewLabelCounterHook()
	engine := edge.NewEngine(p, edge.NewMultiHook(labelHook, &edge.OTelHook{}))
	return &RateLimiter{
		engine:  engine,
		metrics: labelHook,
	}
}

func (rl *RateLimiter) Middleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		ip := realIP(r)

		if !rl.engine.Allow(r.Context(), "ip", ip) {
			logger.WarnCtx(r.Context(), "rate limit exceeded")
			http.Error(w, "rate limit exceeded", http.StatusTooManyRequests)
			return
		}

		if strings.Contains(r.URL.Path, "/download/") {
			if !rl.engine.Allow(r.Context(), "resolve", ip) {
				logger.WarnCtx(r.Context(), "resolve rate limit exceeded")
				http.Error(w, "too many download attempts", http.StatusTooManyRequests)
				return
			}
		}

		next.ServeHTTP(w, r)
	})
}

func (rl *RateLimiter) MetricsSnapshot() map[string]uint64 {
	if rl.metrics == nil {
		return map[string]uint64{}
	}
	return rl.metrics.Snapshot()
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
