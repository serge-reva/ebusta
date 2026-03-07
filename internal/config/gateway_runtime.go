package config

import (
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"os"
	"time"
)

// GatewayRuntimeConfig is a gateway-ready config with duration fields normalized.
type GatewayRuntimeConfig struct {
	ListenHost   string
	DownloadMode string
	Port         int
	TLSCert      string
	TLSKey       string

	RateLimit  GatewayRateLimitConfig
	Mapper     GatewayMapperConfig
	MTLS       GatewayMTLSConfig
	Validation GatewayValidationConfig
	CORS       GatewayCORSConfig
	Services   GatewayServicesRuntimeConfig
	EdgePolicy EdgePolicyConfig
}

type GatewayRateLimitConfig struct {
	IP            int
	Authenticated int
	Download      int
	Resolve       int
}

type GatewayMapperConfig struct {
	TTL             time.Duration
	MaxTokens       int
	CleanupInterval time.Duration
}

type GatewayMTLSConfig struct {
	Enabled    bool
	CAFile     string
	CertFile   string
	KeyFile    string
	ServerName string
}

type GatewayValidationConfig struct {
	MaxBodyBytes   int64
	MaxFileBytes   int64
	MaxQueryLength int
	MaxArrayItems  int
	MaxJSONDepth   int
}

type GatewayCORSConfig struct {
	AllowedOrigins   []string
	AllowedMethods   []string
	AllowedHeaders   []string
	AllowCredentials bool
	MaxAge           int
}

type GatewayServicesRuntimeConfig struct {
	Orchestrator string
	Downloader   string
	Auth         string
}

func LoadGatewayRuntimeConfig(cfg *Config) *GatewayRuntimeConfig {
	g := cfg.Gateway
	mode := g.DownloadMode
	if mode == "" {
		mode = "direct"
	}

	return &GatewayRuntimeConfig{
		ListenHost:   normalizeListenHost(g.ListenHost),
		DownloadMode: mode,
		Port:         g.Port,
		TLSCert:      g.TLSCert,
		TLSKey:       g.TLSKey,

		RateLimit: GatewayRateLimitConfig{
			IP:            g.RateLimit.IP,
			Authenticated: g.RateLimit.Authenticated,
			Download:      g.RateLimit.Download,
			Resolve:       g.RateLimit.Resolve,
		},

		Mapper: GatewayMapperConfig{
			TTL:             time.Duration(g.Mapper.TTL) * time.Second,
			MaxTokens:       g.Mapper.MaxTokens,
			CleanupInterval: time.Duration(g.Mapper.CleanupInterval) * time.Second,
		},

		MTLS: GatewayMTLSConfig{
			Enabled:    g.MTLS.Enabled,
			CAFile:     g.MTLS.CAFile,
			CertFile:   g.MTLS.CertFile,
			KeyFile:    g.MTLS.KeyFile,
			ServerName: g.MTLS.ServerName,
		},

		Validation: GatewayValidationConfig{
			MaxBodyBytes:   g.Validation.MaxBodyBytes,
			MaxFileBytes:   g.Validation.MaxFileBytes,
			MaxQueryLength: g.Validation.MaxQueryLength,
			MaxArrayItems:  g.Validation.MaxArrayItems,
			MaxJSONDepth:   g.Validation.MaxJSONDepth,
		},

		CORS: GatewayCORSConfig{
			AllowedOrigins:   g.CORS.AllowedOrigins,
			AllowedMethods:   g.CORS.AllowedMethods,
			AllowedHeaders:   g.CORS.AllowedHeaders,
			AllowCredentials: g.CORS.AllowCredentials,
			MaxAge:           g.CORS.MaxAge,
		},

		Services: GatewayServicesRuntimeConfig{
			Orchestrator: g.Services.Orchestrator,
			Downloader:   g.Services.Downloader,
			Auth:         g.Services.Auth,
		},
		EdgePolicy: cfg.EdgePolicyForSource("gateway"),
	}
}

func (c *GatewayRuntimeConfig) GetTLSConfig() (*tls.Config, error) {
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
