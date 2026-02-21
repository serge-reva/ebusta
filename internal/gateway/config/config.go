package config

import (
    "crypto/tls"
    "crypto/x509"
    "fmt"
    "os"
    "time"

    "ebusta/internal/config"
)

type GatewayConfig struct {
    Port           int
    TLSCert        string
    TLSKey         string
    
    RateLimit      RateLimitConfig
    Mapper         MapperConfig
    MTLS           MTLSConfig
    Validation     ValidationConfig
    CORS           CORSConfig
    Services       ServicesConfig
}

type RateLimitConfig struct {
    IP            int
    Authenticated int
    Download      int
    Resolve       int
}

type MapperConfig struct {
    TTL             time.Duration
    MaxTokens       int
    CleanupInterval time.Duration
}

type MTLSConfig struct {
    Enabled    bool
    CAFile     string
    CertFile   string
    KeyFile    string
    ServerName string
}

type ValidationConfig struct {
    MaxBodyBytes   int64
    MaxFileBytes   int64
    MaxQueryLength int
    MaxArrayItems  int
    MaxJSONDepth   int
}

type CORSConfig struct {
    AllowedOrigins   []string
    AllowedMethods   []string
    AllowedHeaders   []string
    AllowCredentials bool
    MaxAge           int
}

type ServicesConfig struct {
    Orchestrator string
    Downloader   string
    Auth         string
}

func LoadFromMainConfig(cfg *config.Config) *GatewayConfig {
    g := cfg.Gateway
    
    return &GatewayConfig{
        Port:    g.Port,
        TLSCert: g.TLSCert,
        TLSKey:  g.TLSKey,
        
        RateLimit: RateLimitConfig{
            IP:            g.RateLimit.IP,
            Authenticated: g.RateLimit.Authenticated,
            Download:      g.RateLimit.Download,
            Resolve:       g.RateLimit.Resolve,
        },
        
        Mapper: MapperConfig{
            TTL:             time.Duration(g.Mapper.TTL) * time.Second,
            MaxTokens:       g.Mapper.MaxTokens,
            CleanupInterval: time.Duration(g.Mapper.CleanupInterval) * time.Second,
        },
        
        MTLS: MTLSConfig{
            Enabled:    g.MTLS.Enabled,
            CAFile:     g.MTLS.CAFile,
            CertFile:   g.MTLS.CertFile,
            KeyFile:    g.MTLS.KeyFile,
            ServerName: g.MTLS.ServerName,
        },
        
        Validation: ValidationConfig{
            MaxBodyBytes:   g.Validation.MaxBodyBytes,
            MaxFileBytes:   g.Validation.MaxFileBytes,
            MaxQueryLength: g.Validation.MaxQueryLength,
            MaxArrayItems:  g.Validation.MaxArrayItems,
            MaxJSONDepth:   g.Validation.MaxJSONDepth,
        },
        
        CORS: CORSConfig{
            AllowedOrigins:   g.CORS.AllowedOrigins,
            AllowedMethods:   g.CORS.AllowedMethods,
            AllowedHeaders:   g.CORS.AllowedHeaders,
            AllowCredentials: g.CORS.AllowCredentials,
            MaxAge:           g.CORS.MaxAge,
        },
        
        Services: ServicesConfig{
            Orchestrator: g.Services.Orchestrator,
            Downloader:   g.Services.Downloader,
            Auth:         g.Services.Auth,
        },
    }
}

func (c *GatewayConfig) GetTLSConfig() (*tls.Config, error) {
    if !c.MTLS.Enabled {
        return &tls.Config{
            MinVersion: tls.VersionTLS12,
        }, nil
    }
    
    cert, err := tls.LoadX509KeyPair(c.MTLS.CertFile, c.MTLS.KeyFile)
    if err != nil {
        return nil, fmt.Errorf("failed to load client certificate: %w", err)
    }
    
    caCert, err := os.ReadFile(c.MTLS.CAFile)
    if err != nil {
        return nil, fmt.Errorf("failed to read CA certificate: %w", err)
    }
    
    caPool := x509.NewCertPool()
    if !caPool.AppendCertsFromPEM(caCert) {
        return nil, fmt.Errorf("failed to parse CA certificate")
    }
    
    return &tls.Config{
        Certificates: []tls.Certificate{cert},
        RootCAs:      caPool,
        ServerName:   c.MTLS.ServerName,
        MinVersion:   tls.VersionTLS12,
    }, nil
}
