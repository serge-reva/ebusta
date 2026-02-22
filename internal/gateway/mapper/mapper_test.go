package mapper

import (
    "testing"
    "time"

    "ebusta/internal/config"
)

func TestMapperGenerateAndResolve(t *testing.T) {
    cfg := &config.GatewayMapperConfig{
        TTL:             time.Second,
        MaxTokens:       100,
        CleanupInterval: time.Hour, // большой интервал, чтобы не мешал
    }
    
    m := NewMapper(cfg)
    defer m.Stop()
    
    sha1 := "2fb481cc13771f6485091893858808e51a7718ff"
    path := "container/file.fb2"
    
    token, err := m.GenerateToken(sha1, path)
    if err != nil {
        t.Fatalf("GenerateToken failed: %v", err)
    }
    
    if len(token) != 43 {
        t.Errorf("expected token length 43, got %d", len(token))
    }
    
    resolvedSHA1, resolvedPath, err := m.Resolve(token)
    if err != nil {
        t.Fatalf("Resolve failed: %v", err)
    }
    
    if resolvedSHA1 != sha1 {
        t.Errorf("expected sha1 %s, got %s", sha1, resolvedSHA1)
    }
    
    if resolvedPath != path {
        t.Errorf("expected path %s, got %s", path, resolvedPath)
    }
}

func TestMapperExpiration(t *testing.T) {
    cfg := &config.GatewayMapperConfig{
        TTL:             100 * time.Millisecond,
        MaxTokens:       100,
        CleanupInterval: time.Hour, // большой интервал, чтобы не очищал до вызова Resolve
    }
    
    m := NewMapper(cfg)
    defer m.Stop()
    
    token, _ := m.GenerateToken("sha1", "path")
    
    // Сразу должно быть валидно
    if _, _, err := m.Resolve(token); err != nil {
        t.Errorf("expected token to be valid immediately: %v", err)
    }
    
    // Ждём истечения TTL
    time.Sleep(150 * time.Millisecond)
    
    // Теперь должно истечь
    _, _, err := m.Resolve(token)
    if err != ErrTokenExpired {
        t.Errorf("expected ErrTokenExpired, got %v", err)
    }
}

func TestMapperMaxTokens(t *testing.T) {
    cfg := &config.GatewayMapperConfig{
        TTL:             time.Hour,
        MaxTokens:       2,
        CleanupInterval: time.Hour,
    }
    
    m := NewMapper(cfg)
    defer m.Stop()
    
    if _, err := m.GenerateToken("sha1", "path1"); err != nil {
        t.Fatalf("first token failed: %v", err)
    }
    
    if _, err := m.GenerateToken("sha2", "path2"); err != nil {
        t.Fatalf("second token failed: %v", err)
    }
    
    if _, err := m.GenerateToken("sha3", "path3"); err != ErrMapperFull {
        t.Errorf("expected ErrMapperFull, got %v", err)
    }
}

func TestMapperRevoke(t *testing.T) {
    cfg := &config.GatewayMapperConfig{
        TTL:             time.Hour,
        MaxTokens:       100,
        CleanupInterval: time.Hour,
    }
    
    m := NewMapper(cfg)
    defer m.Stop()
    
    token, _ := m.GenerateToken("sha1", "path")
    
    if _, _, err := m.Resolve(token); err != nil {
        t.Fatalf("token should be valid: %v", err)
    }
    
    m.Revoke(token)
    
    if _, _, err := m.Resolve(token); err != ErrTokenInvalid {
        t.Errorf("expected ErrTokenInvalid after revoke, got %v", err)
    }
}

func TestMapperStats(t *testing.T) {
    cfg := &config.GatewayMapperConfig{
        TTL:             time.Hour,
        MaxTokens:       100,
        CleanupInterval: time.Hour,
    }
    
    m := NewMapper(cfg)
    defer m.Stop()
    
    m.GenerateToken("sha1", "path1")
    m.GenerateToken("sha2", "path2")
    
    stats := m.Stats()
    
    if stats["total_tokens"] != 2 {
        t.Errorf("expected total_tokens=2, got %v", stats["total_tokens"])
    }
    
    if stats["max_tokens"] != 100 {
        t.Errorf("expected max_tokens=100, got %v", stats["max_tokens"])
    }
}
