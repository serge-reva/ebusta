package validation

import (
    "encoding/json"
    "fmt"
    "io"
    "net/http"

    "ebusta/internal/gateway"
)

type SizeLimiter struct {
    config *gateway.ValidationConfig
}

func NewSizeLimiter(cfg *gateway.ValidationConfig) *SizeLimiter {
    return &SizeLimiter{config: cfg}
}

func (l *SizeLimiter) LimitBody(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        // Ограничение размера тела
        r.Body = http.MaxBytesReader(w, r.Body, l.config.MaxBodyBytes)
        next.ServeHTTP(w, r)
    })
}

func (l *SizeLimiter) ValidateJSON(body []byte) error {
    if int64(len(body)) > l.config.MaxBodyBytes {
        return fmt.Errorf("body too large: %d > %d", len(body), l.config.MaxBodyBytes)
    }
    
    // Проверка глубины JSON
    var v interface{}
    if err := json.Unmarshal(body, &v); err != nil {
        return fmt.Errorf("invalid JSON: %w", err)
    }
    
    depth := jsonDepth(v)
    if depth > l.config.MaxJSONDepth {
        return fmt.Errorf("JSON too deep: %d > %d", depth, l.config.MaxJSONDepth)
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

func (l *SizeLimiter) ValidateQuery(query string) error {
    if len(query) > l.config.MaxQueryLength {
        return fmt.Errorf("query too long: %d > %d", len(query), l.config.MaxQueryLength)
    }
    return nil
}
