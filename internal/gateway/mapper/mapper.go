package mapper

import (
    "crypto/rand"
    "encoding/base64"
    "errors"
    "sync"
    "time"

    "ebusta/internal/gateway/config"
    "ebusta/internal/logger"
)

var (
    ErrTokenExpired = errors.New("token expired")
    ErrTokenInvalid = errors.New("token invalid")
    ErrMapperFull   = errors.New("mapper storage full")
)

type Mapping struct {
    RealSHA1   string
    RealPath   string
    CreatedAt  time.Time
    ExpiresAt  time.Time
    Accessed   int
    LastAccess time.Time
}

type Mapper struct {
    config  *config.MapperConfig
    mu      sync.RWMutex
    tokens  map[string]*Mapping
    stopCh  chan struct{}
}

func NewMapper(cfg *config.MapperConfig) *Mapper {
    m := &Mapper{
        config: cfg,
        tokens: make(map[string]*Mapping),
        stopCh: make(chan struct{}),
    }
    
    go m.cleanupLoop()
    
    return m
}

func (m *Mapper) Stop() {
    close(m.stopCh)
}

func (m *Mapper) cleanupLoop() {
    ticker := time.NewTicker(m.config.CleanupInterval)
    defer ticker.Stop()
    
    for {
        select {
        case <-ticker.C:
            m.cleanup()
        case <-m.stopCh:
            return
        }
    }
}

func (m *Mapper) cleanup() {
    m.mu.Lock()
    defer m.mu.Unlock()
    
    now := time.Now()
    for token, mapping := range m.tokens {
        if now.After(mapping.ExpiresAt) {
            logger.Info("", "mapper: cleaning up expired token")
            delete(m.tokens, token)
        }
    }
}

func (m *Mapper) GenerateToken(sha1, path string) (string, error) {
    m.mu.Lock()
    defer m.mu.Unlock()
    
    if len(m.tokens) >= m.config.MaxTokens {
        m.cleanupLocked()
        
        if len(m.tokens) >= m.config.MaxTokens {
            return "", ErrMapperFull
        }
    }
    
    bytes := make([]byte, 32)
    if _, err := rand.Read(bytes); err != nil {
        return "", err
    }
    token := base64.RawURLEncoding.EncodeToString(bytes)
    
    now := time.Now()
    m.tokens[token] = &Mapping{
        RealSHA1:   sha1,
        RealPath:   path,
        CreatedAt:  now,
        ExpiresAt:  now.Add(m.config.TTL),
        Accessed:   0,
        LastAccess: now,
    }
    
    return token, nil
}

func (m *Mapper) cleanupLocked() {
    now := time.Now()
    for token, mapping := range m.tokens {
        if now.After(mapping.ExpiresAt) {
            delete(m.tokens, token)
        }
    }
}

func (m *Mapper) Resolve(token string) (string, string, error) {
    m.mu.Lock()
    defer m.mu.Unlock()
    
    mapping, exists := m.tokens[token]
    if !exists {
        return "", "", ErrTokenInvalid
    }
    
    now := time.Now()
    if now.After(mapping.ExpiresAt) {
        delete(m.tokens, token)
        return "", "", ErrTokenExpired
    }
    
    mapping.Accessed++
    mapping.LastAccess = now
    
    return mapping.RealSHA1, mapping.RealPath, nil
}

func (m *Mapper) Revoke(token string) {
    m.mu.Lock()
    defer m.mu.Unlock()
    delete(m.tokens, token)
}

func (m *Mapper) Stats() map[string]interface{} {
    m.mu.RLock()
    defer m.mu.RUnlock()
    
    total := len(m.tokens)
    var accessed int
    for _, mapping := range m.tokens {
        if mapping.Accessed > 0 {
            accessed++
        }
    }
    
    return map[string]interface{}{
        "total_tokens": total,
        "accessed":     accessed,
        "max_tokens":   m.config.MaxTokens,
        "usage_percent": float64(total) / float64(m.config.MaxTokens) * 100,
    }
}
