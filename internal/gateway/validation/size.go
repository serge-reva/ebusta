package validation

import (
    "encoding/json"
    "fmt"
    "net/http"

    "ebusta/internal/gateway/config"
)

type SizeLimiter struct {
    maxBodyBytes int64
    maxJSONDepth int
}

func NewSizeLimiter(cfg *config.ValidationConfig) *SizeLimiter {
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
    if int64(len(body)) > l.maxBodyBytes {
        return fmt.Errorf("body too large: %d > %d", len(body), l.maxBodyBytes)
    }
    
    var v interface{}
    if err := json.Unmarshal(body, &v); err != nil {
        return fmt.Errorf("invalid JSON: %w", err)
    }
    
    depth := jsonDepth(v)
    if depth > l.maxJSONDepth {
        return fmt.Errorf("JSON too deep: %d > %d", depth, l.maxJSONDepth)
    }
    
    return nil
}

func jsonDepth(v interface{}) int {
    maxDepth := 1
    
    switch val := v.(type) {
    case map[string]interface{}:
        for _, child := range val {
            if d := jsonDepth(child) + 1; d > maxDepth {
                maxDepth = d
            }
        }
    case []interface{}:
        for _, item := range val {
            if d := jsonDepth(item) + 1; d > maxDepth {
                maxDepth = d
            }
        }
    }
    
    return maxDepth
}
