package middleware

import (
    "net/http"
    "strconv"
    "strings"

    "ebusta/internal/config"
)

type CORS struct {
    config *config.GatewayCORSConfig
}

func NewCORS(cfg *config.GatewayCORSConfig) *CORS {
    return &CORS{config: cfg}
}

func (c *CORS) Middleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        origin := r.Header.Get("Origin")
        
        allowed := false
        for _, o := range c.config.AllowedOrigins {
            if o == "*" && !c.config.AllowCredentials {
                allowed = true
                break
            }
            if o == origin {
                allowed = true
                break
            }
        }
        
        if allowed {
            w.Header().Set("Access-Control-Allow-Origin", origin)
            w.Header().Set("Vary", "Origin")
            
            if c.config.AllowCredentials {
                w.Header().Set("Access-Control-Allow-Credentials", "true")
            }
        }
        
        if r.Method == http.MethodOptions {
            w.Header().Set("Access-Control-Allow-Methods", 
                strings.Join(c.config.AllowedMethods, ", "))
            w.Header().Set("Access-Control-Allow-Headers", 
                strings.Join(c.config.AllowedHeaders, ", "))
            w.Header().Set("Access-Control-Max-Age", 
                strconv.Itoa(c.config.MaxAge))
            w.WriteHeader(http.StatusNoContent)
            return
        }
        
        next.ServeHTTP(w, r)
    })
}
