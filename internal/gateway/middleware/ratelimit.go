package middleware

import (
    "net"
    "net/http"
    "strings"
    "sync"
    "time"

    "ebusta/internal/gateway"
    "ebusta/internal/logger"
    "golang.org/x/time/rate"
)

type RateLimiter struct {
    config      *gateway.RateLimitConfig
    ipLimiter   *IPRateLimiter
    resolveLimiter *IPRateLimiter
    mu          sync.RWMutex
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
        
        // Очистка через час (чтобы не накапливать)
        time.AfterFunc(time.Hour, func() {
            i.mu.Lock()
            delete(i.limiters, ip)
            i.mu.Unlock()
        })
    }
    
    return limiter
}

func NewRateLimiter(cfg *gateway.RateLimitConfig) *RateLimiter {
    return &RateLimiter{
        config: cfg,
        ipLimiter: NewIPRateLimiter(
            rate.Limit(cfg.IP)/60, // per minute
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
        // Получаем реальный IP
        ip := realIP(r)
        
        // Базовый rate limit по IP
        if !rl.ipLimiter.GetLimiter(ip).Allow() {
            logger.WarnCtx(r.Context(), "rate limit exceeded", "ip", ip)
            http.Error(w, "rate limit exceeded", http.StatusTooManyRequests)
            return
        }
        
        // Для операций с токенами - отдельный лимит
        if strings.Contains(r.URL.Path, "/download/") {
            if !rl.resolveLimiter.GetLimiter(ip).Allow() {
                logger.WarnCtx(r.Context(), "resolve rate limit exceeded", "ip", ip)
                http.Error(w, "too many download attempts", http.StatusTooManyRequests)
                return
            }
        }
        
        // Проверка авторизованных пользователей
        if token := r.Header.Get("X-API-Token"); token != "" {
            // Здесь будет проверка квот пользователя
            // (реализуется в quota manager)
        }
        
        next.ServeHTTP(w, r)
    })
}

// realIP извлекает реальный IP с учётом прокси
func realIP(r *http.Request) string {
    // Проверяем X-Forwarded-For
    if fwd := r.Header.Get("X-Forwarded-For"); fwd != "" {
        parts := strings.Split(fwd, ",")
        return strings.TrimSpace(parts[0])
    }
    
    // Проверяем X-Real-IP
    if real := r.Header.Get("X-Real-IP"); real != "" {
        return real
    }
    
    // Иначе берем RemoteAddr
    ip, _, err := net.SplitHostPort(r.RemoteAddr)
    if err != nil {
        return r.RemoteAddr
    }
    return ip
}
