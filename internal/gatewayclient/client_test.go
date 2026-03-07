package gatewayclient

import (
	"context"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"

	"ebusta/internal/errutil"
)

func TestSearchSuccessAndTracePropagation(t *testing.T) {
	var gotTraceID string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotTraceID = r.Header.Get("X-Trace-Id")
		if r.Method != http.MethodPost || r.URL.Path != "/search" {
			t.Fatalf("unexpected request: %s %s", r.Method, r.URL.Path)
		}
		w.Header().Set("Content-Type", "application/json")
		_, _ = w.Write([]byte(`{"trace_id":"gw-1","total":1,"books":[{"id":"id1","title":"Book","authors":["A"],"full_authors":"A","download_url":"/download/t1"}],"page":2,"pages":3}`))
	}))
	defer srv.Close()

	client := NewClient(srv.URL)
	resp, err := client.Search(context.Background(), "king", 2, 5, "trace-123")
	if err != nil {
		t.Fatalf("Search() error = %v", err)
	}
	if gotTraceID != "trace-123" {
		t.Fatalf("trace header = %q, want trace-123", gotTraceID)
	}
	if resp.TraceID != "gw-1" || resp.Total != 1 || len(resp.Books) != 1 {
		t.Fatalf("unexpected response: %+v", resp)
	}
}

func TestSearchReturnsGatewayAppError(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		errutil.WriteJSONError(w, errutil.New(errutil.CodeInvalidArgument, "bad query").WithTrace("gw-err"))
	}))
	defer srv.Close()

	client := NewClient(srv.URL)
	_, err := client.Search(context.Background(), "", 1, 5, "trace-err")
	if err == nil {
		t.Fatal("expected error")
	}
	appErr, ok := err.(*errutil.AppError)
	if !ok {
		t.Fatalf("expected *errutil.AppError, got %T", err)
	}
	if appErr.Code != errutil.CodeInvalidArgument || appErr.TraceID != "gw-err" {
		t.Fatalf("unexpected app error: %+v", appErr)
	}
}

func TestSearchInvalidJSON(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_, _ = w.Write([]byte(`{"trace_id":`))
	}))
	defer srv.Close()

	client := NewClient(srv.URL)
	_, err := client.Search(context.Background(), "king", 1, 5, "trace-json")
	if err == nil {
		t.Fatal("expected error")
	}
	appErr, ok := err.(*errutil.AppError)
	if !ok {
		t.Fatalf("expected *errutil.AppError, got %T", err)
	}
	if appErr.Code != errutil.CodeBadGateway {
		t.Fatalf("unexpected code: %s", appErr.Code)
	}
}

func TestSearchTimeout(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		time.Sleep(100 * time.Millisecond)
		w.Header().Set("Content-Type", "application/json")
		_, _ = w.Write([]byte(`{"trace_id":"gw-timeout","total":0,"books":[],"page":1,"pages":0}`))
	}))
	defer srv.Close()

	client := NewClient(srv.URL, WithTimeout(10*time.Millisecond))
	_, err := client.Search(context.Background(), "king", 1, 5, "trace-timeout")
	if err == nil {
		t.Fatal("expected timeout error")
	}
	appErr, ok := err.(*errutil.AppError)
	if !ok {
		t.Fatalf("expected *errutil.AppError, got %T", err)
	}
	if appErr.Code != errutil.CodeBadGateway {
		t.Fatalf("unexpected code: %s", appErr.Code)
	}
}
