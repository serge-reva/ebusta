package middleware

import (
    "net/http"
    "runtime/debug"

    "ebusta/internal/logger"
)

type Recover struct{}

func (r *Recover) Middleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
        defer func() {
            if err := recover(); err != nil {
                logger.ErrorCtx(req.Context(), "panic recovered", 
                    nil, "panic", err, "stack", string(debug.Stack()))
                
                http.Error(w, "internal server error", 
                    http.StatusInternalServerError)
            }
        }()
        
        next.ServeHTTP(w, req)
    })
}
