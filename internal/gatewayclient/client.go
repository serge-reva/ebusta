package gatewayclient

import (
	"bytes"
	"context"
	"encoding/json"
	"net/http"
	"strings"
	"time"

	"ebusta/internal/errutil"
)

type Client struct {
	baseURL    string
	httpClient *http.Client
}

type ClientOption func(*Client)

func WithTimeout(timeout time.Duration) ClientOption {
	return func(c *Client) {
		c.httpClient.Timeout = timeout
	}
}

func WithHTTPClient(httpClient *http.Client) ClientOption {
	return func(c *Client) {
		if httpClient != nil {
			c.httpClient = httpClient
		}
	}
}

func NewClient(baseURL string, opts ...ClientOption) *Client {
	c := &Client{
		baseURL: strings.TrimRight(baseURL, "/"),
		httpClient: &http.Client{
			Timeout: 10 * time.Second,
		},
	}
	for _, opt := range opts {
		opt(c)
	}
	return c
}

func (c *Client) Search(ctx context.Context, query string, page, limit int, traceID string) (*SearchResponse, error) {
	if traceID == "" {
		traceID = errutil.GenerateTraceID("gwc")
	}
	payload := map[string]interface{}{
		"query": query,
		"page":  page,
		"limit": limit,
	}
	body, err := json.Marshal(payload)
	if err != nil {
		return nil, errutil.New(errutil.CodeInternal, "failed to marshal gateway request").
			WithTrace(traceID).
			WithDetails(err.Error())
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodPost, c.baseURL+"/search", bytes.NewReader(body))
	if err != nil {
		return nil, errutil.New(errutil.CodeInternal, "failed to create gateway request").
			WithTrace(traceID).
			WithDetails(err.Error())
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("X-Trace-Id", traceID)

	var resp SearchResponse
	if err := c.doJSON(req, traceID, &resp); err != nil {
		return nil, err
	}
	if resp.TraceID == "" {
		resp.TraceID = traceID
	}
	return &resp, nil
}

func (c *Client) GetBook(_ context.Context, _ string, traceID string) (*BookDetails, error) {
	if traceID == "" {
		traceID = errutil.GenerateTraceID("gwc")
	}
	return nil, errutil.New(errutil.CodeInternal, "gateway get book is not implemented").
		WithTrace(traceID)
}

func (c *Client) doJSON(req *http.Request, traceID string, out interface{}) error {
	resp, err := c.httpClient.Do(req)
	if err != nil {
		return errutil.New(errutil.CodeBadGateway, "gateway request failed").
			WithTrace(traceID).
			WithDetails(err.Error())
	}
	defer resp.Body.Close()

	body, appErr := errutil.ReadBodyAndError(resp, traceID)
	if appErr != nil {
		return appErr
	}

	if err := json.Unmarshal(body, out); err != nil {
		return errutil.New(errutil.CodeBadGateway, "invalid gateway response").
			WithTrace(traceID).
			WithDetails(err.Error())
	}
	return nil
}
