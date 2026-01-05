package proxy

import (
	"bytes"
	"context"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"sync"
	"time"

	"github.com/sirupsen/logrus"

	"ebusta/internal/storage/datamanager/config"
)

type Proxy struct {
	cfg     config.Config
	client  *http.Client
	logger  *logrus.Logger
	baseURL string
	once    sync.Once
}

func New(cfg config.Config, logger *logrus.Logger) *Proxy {
	return &Proxy{
		cfg:    cfg,
		logger: logger,
		client: newHTTPClient(cfg),
	}
}

func newHTTPClient(cfg config.Config) *http.Client {
	t := &http.Transport{
		MaxIdleConns:        100,
		IdleConnTimeout:     90 * time.Second,
		DisableCompression:  false,
		ForceAttemptHTTP2:   true,
	}
	return &http.Client{Transport: t, Timeout: cfg.HTTPTimeout}
}

func (p *Proxy) BaseURL() string {
	p.once.Do(func() {
		p.baseURL = fmt.Sprintf("%s://%s:%s/%s/_search/template", p.cfg.OSScheme, p.cfg.OSHost, p.cfg.OSPort, p.cfg.OSIndex)
	})
	return p.baseURL
}

// Structured error envelope
type ErrorEnvelope struct {
	Error ErrorBody `json:"error"`
}
type ErrorBody struct {
	Code    string      `json:"code"`
	Message string      `json:"message"`
	Details interface{} `json:"details,omitempty"`
}

func WriteError(w http.ResponseWriter, status int, code, message string, details interface{}) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	_ = json.NewEncoder(w).Encode(ErrorEnvelope{
		Error: ErrorBody{Code: code, Message: message, Details: details},
	})
}

// DoTemplate executes a stored template by id with params.
func (p *Proxy) DoTemplate(ctx context.Context, id string, params map[string]any) ([]byte, int, error) {
	body := map[string]any{
		"id":     id,
		"params": params,
	}
	
	// This now only logs if LOG_LEVEL=DEBUG
	if p.logger.IsLevelEnabled(logrus.DebugLevel) {
		p.logger.WithFields(logrus.Fields{
			"template": id,
			"params":   params,
		}).Debug("os.request") // Changed from Info to Debug
	}
	
	buf, err := json.Marshal(body)
	if err != nil {
		return nil, 0, fmt.Errorf("marshal body: %w", err)
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodPost, p.BaseURL(), bytes.NewReader(buf))
	if err != nil {
		return nil, 0, fmt.Errorf("failed to create request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")
	if p.cfg.ESUser != "" || p.cfg.ESPass != "" {
		req.SetBasicAuth(p.cfg.ESUser, p.cfg.ESPass)
	}

	res, err := p.client.Do(req)
	if err != nil {
		return nil, 0, fmt.Errorf("upstream do: %w", err)
	}
	defer res.Body.Close()

	data, _ := io.ReadAll(res.Body)
	
	// This now only logs if LOG_LEVEL=DEBUG
	if p.logger.IsLevelEnabled(logrus.DebugLevel) {
		p.logger.WithFields(logrus.Fields{
			"template": id,
			"status": res.StatusCode,
			"response_body": string(data),
		}).Debug("os.response")
	}
	
	return data, res.StatusCode, nil
}

// DecodeAfter supports raw JSON or base64(JSON).
func DecodeAfter(s string) (any, error) {
	if s == "" {
		return nil, nil
	}
	// try raw JSON first
	var v any
	if json.Unmarshal([]byte(s), &v) == nil {
		return v, nil
	}
	// try base64
	b, err := base64.StdEncoding.DecodeString(s)
	if err != nil {
		return nil, fmt.Errorf("invalid after (not json or base64): %w", err)
	}
	if err := json.Unmarshal(b, &v); err != nil {
		return nil, fmt.Errorf("invalid after (bad json): %w", err)
	}
	return v, nil
}
