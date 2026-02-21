package middleware

import (
    "net"
    "net/http"
    "strings"
    "sync"
    "time"

    "ebusta/internal/gateway/config"
    "ebusta/internal/logger"
    "golang.org/x/time/rate"
)

type RateLimiter struct {
    config         *config.RateLimitConfig
    ipLimiter      *IPRateLimiter
    resolveLimiter *IPRateLimiter
}

type IPRateLimiter struct {
    limiters map[string]*rate.Limiter
    mu       sync.RWMutex
    rate     rate.Limit
    burst    int
}

func NewIPRateLimiter(r rate.Limit, b int) *IPRateLimiter {
    return &IPRateLimiter{
        limiters: make(map[string]*rate.Limiter),
        rate:     r,
        burst:    b,
    }
}

func (i *IPRateLimiter) GetLimiter(ip string) *rate.Limiter {
    i.mu.Lock()
    defer i.mu.Unlock()
    
    limiter, exists := i.limiters[ip]
    if !exists {
        limiter = rate.NewLimiter(i.rate, i.burst)
        i.limiters[ip] = limiter
        
        time.AfterFunc(time.Hour, func() {
            i.mu.Lock()
            delete(i.limiters, ip)
            i.mu.Unlock()
        })
    }
    
    return limiter
}

func NewRateLimiter(cfg *config.RateLimitConfig) *RateLimiter {
    return &RateLimiter{
        config: cfg,
        ipLimiter: NewIPRateLimiter(
            rate.Limit(cfg.IP)/60,
            cfg.IP,
        ),
        resolveLimiter: NewIPRateLimiter(
            rate.Limit(cfg.Resolve)/60,
            cfg.Resolve,
        ),
    }
}

func (rl *RateLimiter) Middleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        ip := realIP(r)
        
        if !rl.ipLimiter.GetLimiter(ip).Allow() {
            logger.WarnCtx(r.Context(), "rate limit exceeded", "ip", ip)
            http.Error(w, "rate limit exceeded", http.StatusTooManyRequests)
            return
        }
        
        if strings.Contains(r.URL.Path, "/download/") {
            if !rl.resolveLimiter.GetLimiter(ip).Allow() {
                logger.WarnCtx(r.Context(), "resolve rate limit exceeded", "ip", ip)
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
