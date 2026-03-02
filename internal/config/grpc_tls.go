package config

import (
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"os"
	"strings"

	"google.golang.org/grpc/credentials"
	"google.golang.org/grpc/credentials/insecure"
)

type GRPCTLSConfig struct {
	Enabled    bool   `yaml:"enabled"`
	CAFile     string `yaml:"ca_file"`
	CertFile   string `yaml:"cert_file"`
	KeyFile    string `yaml:"key_file"`
	ServerName string `yaml:"server_name"`
}

func (c GRPCTLSConfig) Validate(prefix string) error {
	if !c.Enabled {
		return nil
	}
	if strings.TrimSpace(c.CAFile) == "" {
		return fmt.Errorf("%s.ca_file is required when enabled", prefix)
	}
	if strings.TrimSpace(c.CertFile) == "" {
		return fmt.Errorf("%s.cert_file is required when enabled", prefix)
	}
	if strings.TrimSpace(c.KeyFile) == "" {
		return fmt.Errorf("%s.key_file is required when enabled", prefix)
	}
	return nil
}

func (c GRPCTLSConfig) ClientTransportCredentials() (credentials.TransportCredentials, error) {
	if !c.Enabled {
		return insecure.NewCredentials(), nil
	}
	tlsCfg, err := c.clientTLSConfig()
	if err != nil {
		return nil, err
	}
	return credentials.NewTLS(tlsCfg), nil
}

func (c GRPCTLSConfig) ServerTransportCredentials() (credentials.TransportCredentials, error) {
	if !c.Enabled {
		return nil, nil
	}
	tlsCfg, err := c.serverTLSConfig()
	if err != nil {
		return nil, err
	}
	return credentials.NewTLS(tlsCfg), nil
}

func (c GRPCTLSConfig) clientTLSConfig() (*tls.Config, error) {
	cert, err := tls.LoadX509KeyPair(c.CertFile, c.KeyFile)
	if err != nil {
		return nil, fmt.Errorf("load client cert/key: %w", err)
	}

	caData, err := os.ReadFile(c.CAFile)
	if err != nil {
		return nil, fmt.Errorf("read CA file: %w", err)
	}
	pool := x509.NewCertPool()
	if !pool.AppendCertsFromPEM(caData) {
		return nil, fmt.Errorf("parse CA file: invalid PEM")
	}

	return &tls.Config{
		MinVersion:   tls.VersionTLS12,
		Certificates: []tls.Certificate{cert},
		RootCAs:      pool,
		ServerName:   c.ServerName,
	}, nil
}

func (c GRPCTLSConfig) serverTLSConfig() (*tls.Config, error) {
	cert, err := tls.LoadX509KeyPair(c.CertFile, c.KeyFile)
	if err != nil {
		return nil, fmt.Errorf("load server cert/key: %w", err)
	}

	caData, err := os.ReadFile(c.CAFile)
	if err != nil {
		return nil, fmt.Errorf("read CA file: %w", err)
	}
	pool := x509.NewCertPool()
	if !pool.AppendCertsFromPEM(caData) {
		return nil, fmt.Errorf("parse CA file: invalid PEM")
	}

	return &tls.Config{
		MinVersion:   tls.VersionTLS12,
		Certificates: []tls.Certificate{cert},
		ClientAuth:   tls.RequireAndVerifyClientCert,
		ClientCAs:    pool,
	}, nil
}
