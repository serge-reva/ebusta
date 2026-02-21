package middleware

import (
    "crypto/subtle"
    "net/http"
    "strings"

    "ebusta/internal/logger"
)

type SecurityHeaders struct{}

func (s *SecurityHeaders) Middleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        // Защитные заголовки
        w.Header().Set("X-Content-Type-Options", "nosniff")
        w.Header().Set("X-Frame-Options", "DENY")
        w.Header().Set("X-XSS-Protection", "1; mode=block")
        w.Header().Set("Referrer-Policy", "strict-origin-when-cross-origin")
        
        // HSTS (HTTP Strict Transport Security)
        w.Header().Set("Strict-Transport-Security", 
            "max-age=31536000; includeSubDomains; preload")
        
        // Content Security Policy
        w.Header().Set("Content-Security-Policy", 
            "default-src 'self'; script-src 'self'; object-src 'none';")
        
        next.ServeHTTP(w, r)
    })
}

type CSRFProtection struct {
    // В реальном приложении здесь будет хранилище сессий
}

func (c *CSRFProtection) Middleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        // Для GET запросов генерируем CSRF токен
        if r.Method == http.MethodGet {
            // Генерация токена и сохранение в сессии
            next.ServeHTTP(w, r)
            return
        }
        
        // Для остальных методов проверяем CSRF токен
        token := r.Header.Get("X-CSRF-Token")
        if token == "" {
            logger.WarnCtx(r.Context(), "missing CSRF token")
            http.Error(w, "CSRF token required", http.StatusForbidden)
            return
        }
        
        // Проверка токена (должно быть в сессии)
        // if subtle.ConstantTimeCompare([]byte(token), []byte(sessionToken)) != 1 {
        //     logger.WarnCtx(r.Context(), "invalid CSRF token")
        //     http.Error(w, "invalid CSRF token", http.StatusForbidden)
        //     return
        // }
        
        next.ServeHTTP(w, r)
    })
}

type ContentTypeValidation struct{}

func (c *ContentTypeValidation) Middleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        // Проверка Content-Type для POST/PUT
        if r.Method == http.MethodPost || r.Method == http.MethodPut {
            ct := r.Header.Get("Content-Type")
            if !strings.HasPrefix(ct, "application/json") {
                http.Error(w, "Content-Type must be application/json", 
                    http.StatusUnsupportedMediaType)
                return
            }
        }
        
        next.ServeHTTP(w, r)
    })
}
