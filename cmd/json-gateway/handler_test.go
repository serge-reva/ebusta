package main

import (
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"ebusta/internal/edge"
	"ebusta/internal/gatewayclient"
)

type tgRoundTripFunc func(*http.Request) (*http.Response, error)

func (f tgRoundTripFunc) RoundTrip(r *http.Request) (*http.Response, error) {
	return f(r)
}

func TestParseTGSearch(t *testing.T) {
	tests := []struct {
		name       string
		msg        string
		wantQuery  string
		wantPage   int
		shouldFail bool
	}{
		{"basic", "/search author:king", "author:king", 1, false},
		{"with_page", "/search title:hobbit page 2", "title:hobbit", 2, false},
		{"multi_words", "/search stephen king page 3", "stephen king", 3, false},
		{"bad_cmd", "/help", "", 1, true},
		{"empty", "/search", "", 1, true},
		{"bad_page", "/search q page x", "", 1, true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			q, p, err := parseTGSearch(tt.msg)
			if tt.shouldFail {
				if err == nil {
					t.Fatalf("expected error")
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if q != tt.wantQuery || p != tt.wantPage {
				t.Fatalf("got (%q,%d), want (%q,%d)", q, p, tt.wantQuery, tt.wantPage)
			}
		})
	}
}

func TestHandleUpdateSuccess(t *testing.T) {
	p := edge.DefaultPolicy("telegram")
	p.MaxBodyBytes = 1024
	p.MaxJSONDepth = 4
	p.MaxLineLength = 256
	p.Actions["command"] = edge.ActionPolicy{PerMinute: 10, Burst: 10}

	h := NewTGHandler("http://gateway.local", 5, edge.NewEngine(p, edge.NewLabelCounterHook()))
	h.gatewayClient = testGatewayClient(func(r *http.Request) (*http.Response, error) {
		if r.URL.Path != "/search" {
			t.Fatalf("unexpected path: %s", r.URL.Path)
		}
		body := `{"trace_id":"gw-1","total":1,"books":[{"id":"id1","title":"Book One","authors":["A"],"full_authors":"A"}],"page":1,"pages":1}`
		return &http.Response{
			StatusCode: http.StatusOK,
			Header:     http.Header{"Content-Type": []string{"application/json"}},
			Body:       io.NopCloser(strings.NewReader(body)),
		}, nil
	})

	reqBody := `{"user_id":"u1","message":"/search author:king"}`
	req := httptest.NewRequest(http.MethodPost, "/update", strings.NewReader(reqBody))
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()

	h.handleUpdate(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d", w.Code)
	}
	if got := w.Header().Get("X-Trace-Id"); got != "gw-1" {
		t.Fatalf("expected response trace header gw-1, got %q", got)
	}
	var resp UpdateResponse
	if err := json.Unmarshal(w.Body.Bytes(), &resp); err != nil {
		t.Fatalf("invalid response json: %v", err)
	}
	if !resp.OK || resp.Total != 1 || len(resp.Books) != 1 {
		t.Fatalf("unexpected response: %+v", resp)
	}
}

func TestHandleUpdateRateLimited(t *testing.T) {
	p := edge.DefaultPolicy("telegram")
	p.MaxBodyBytes = 1024
	p.MaxJSONDepth = 4
	p.MaxLineLength = 256
	p.Actions["command"] = edge.ActionPolicy{PerMinute: 1, Burst: 1}

	h := NewTGHandler("http://gateway.local", 5, edge.NewEngine(p, nil))
	h.gatewayClient = testGatewayClient(func(r *http.Request) (*http.Response, error) {
		body := `{"trace_id":"gw-1","total":0,"books":[],"page":1,"pages":0}`
		return &http.Response{
			StatusCode: http.StatusOK,
			Header:     http.Header{"Content-Type": []string{"application/json"}},
			Body:       io.NopCloser(strings.NewReader(body)),
		}, nil
	})

	reqBody := `{"user_id":"u-rate","message":"/search test"}`

	w1 := httptest.NewRecorder()
	h.handleUpdate(w1, httptest.NewRequest(http.MethodPost, "/update", strings.NewReader(reqBody)))
	if w1.Code != http.StatusOK {
		t.Fatalf("first request should pass, got %d", w1.Code)
	}

	w2 := httptest.NewRecorder()
	h.handleUpdate(w2, httptest.NewRequest(http.MethodPost, "/update", strings.NewReader(reqBody)))
	if w2.Code != http.StatusTooManyRequests {
		t.Fatalf("second request should be throttled, got %d", w2.Code)
	}
}

func TestHandleUpdateMalformedAndOversize(t *testing.T) {
	p := edge.DefaultPolicy("telegram")
	p.MaxBodyBytes = 30
	p.MaxJSONDepth = 3
	p.MaxLineLength = 64
	p.Actions["command"] = edge.ActionPolicy{PerMinute: 5, Burst: 5}

	h := NewTGHandler("http://gateway.local", 5, edge.NewEngine(p, nil))

	malformed := httptest.NewRequest(http.MethodPost, "/update", strings.NewReader(`{"user_id":`))
	w1 := httptest.NewRecorder()
	h.handleUpdate(w1, malformed)
	if w1.Code != http.StatusBadRequest {
		t.Fatalf("malformed json expected 400, got %d", w1.Code)
	}

	over := httptest.NewRequest(http.MethodPost, "/update", strings.NewReader(`{"user_id":"u1","message":"/search `+strings.Repeat("a", 200)+`"}`))
	w2 := httptest.NewRecorder()
	h.handleUpdate(w2, over)
	if w2.Code != http.StatusBadRequest {
		t.Fatalf("oversize body expected 400, got %d", w2.Code)
	}
}

func testGatewayClient(fn tgRoundTripFunc) *gatewayclient.Client {
	return gatewayclient.NewClient("http://gateway.local", gatewayclient.WithHTTPClient(&http.Client{Transport: fn}))
}
