package gateway_test

import (
    "bytes"
    "encoding/json"
    "net/http"
    "net/http/httptest"
    "testing"
    "time"

    "ebusta/internal/gateway"
    "ebusta/internal/gateway/mapper"
    "ebusta/internal/gateway/validation"
)

func TestSearchEndpoint(t *testing.T) {
    // Создаём тестовый сервер
    cfg := &gateway.Config{
        Port: 8080,
        Validation: gateway.ValidationConfig{
            MaxBodyBytes:   1024 * 1024,
            MaxQueryLength: 500,
        },
        Mapper: gateway.MapperConfig{
            TTL:             time.Hour,
            MaxTokens:       100,
            CleanupInterval: time.Minute,
        },
    }
    
    validator := validation.NewValidator()
    
    // Тестовые запросы
    tests := []struct {
        name       string
        body       map[string]interface{}
        wantStatus int
    }{
        {
            name: "valid search",
            body: map[string]interface{}{
                "query": "author:king",
                "page":  1,
                "limit": 10,
            },
            wantStatus: http.StatusOK,
        },
        {
            name: "missing query",
            body: map[string]interface{}{
                "page": 1,
            },
            wantStatus: http.StatusBadRequest,
        },
        {
            name: "query too long",
            body: map[string]interface{}{
                "query": string(make([]byte, 600)),
            },
            wantStatus: http.StatusBadRequest,
        },
        {
            name: "invalid characters",
            body: map[string]interface{}{
                "query": "SELECT * FROM books; --",
            },
            wantStatus: http.StatusBadRequest,
        },
    }
    
    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            body, _ := json.Marshal(tt.body)
            
            req := httptest.NewRequest("POST", "/search", bytes.NewReader(body))
            req.Header.Set("Content-Type", "application/json")
            
            w := httptest.NewRecorder()
            
            // Валидация
            _, err := validator.ValidateSearch(body)
            if (err != nil) != (tt.wantStatus != http.StatusOK) {
                t.Errorf("ValidateSearch() error = %v, wantStatus %v", err, tt.wantStatus)
            }
        })
    }
}

func TestMapperIntegration(t *testing.T) {
    cfg := &gateway.MapperConfig{
        TTL:             time.Second,
        MaxTokens:       10,
        CleanupInterval: 100 * time.Millisecond,
    }
    
    m := mapper.NewMapper(cfg)
    defer m.Stop()
    
    // Генерация токена
    sha1 := "2fb481cc13771f6485091893858808e51a7718ff"
    token, err := m.GenerateToken(sha1, "")
    if err != nil {
        t.Fatalf("GenerateToken failed: %v", err)
    }
    
    // Резолвинг
    resolved, _, err := m.Resolve(token)
    if err != nil {
        t.Fatalf("Resolve failed: %v", err)
    }
    if resolved != sha1 {
        t.Errorf("Resolve = %s, want %s", resolved, sha1)
    }
    
    // Статистика
    stats := m.Stats()
    if stats["total_tokens"] != 1 {
        t.Errorf("expected 1 token, got %v", stats["total_tokens"])
    }
}
