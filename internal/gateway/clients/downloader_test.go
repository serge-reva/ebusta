package clients

import (
	"io"
	"net/http"
	"strings"
	"testing"

	"ebusta/internal/errutil"
)

type rtFunc func(*http.Request) (*http.Response, error)

func (f rtFunc) RoundTrip(r *http.Request) (*http.Response, error) {
	return f(r)
}

func TestDownloaderClientGetMetaWithTraceSuccess(t *testing.T) {
	c := &DownloaderClient{
		baseURL: "http://downloader.local",
		httpClient: &http.Client{
			Transport: rtFunc(func(r *http.Request) (*http.Response, error) {
				return &http.Response{
					StatusCode: http.StatusOK,
					Header:     http.Header{"Content-Type": []string{"application/json"}},
					Body:       io.NopCloser(strings.NewReader(`{"sha1":"abc","container":"c.zip","filename":"f.fb2","size":123,"title":"T"}`)),
				}, nil
			}),
		},
	}

	meta, err := c.GetMetaWithTrace("abc", "gw-1")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if meta.Filename != "f.fb2" || meta.Size != 123 {
		t.Fatalf("unexpected metadata: %+v", meta)
	}
}

func TestDownloaderClientGetMetaWithTracePropagatesAppError(t *testing.T) {
	c := &DownloaderClient{
		baseURL: "http://downloader.local",
		httpClient: &http.Client{
			Transport: rtFunc(func(r *http.Request) (*http.Response, error) {
				return &http.Response{
					StatusCode: http.StatusNotFound,
					Header:     http.Header{"Content-Type": []string{"application/json"}},
					Body:       io.NopCloser(strings.NewReader(`{"error":{"code":"NOT_FOUND","message":"NOT_FOUND","trace_id":"dl-123"}}`)),
				}, nil
			}),
		},
	}

	_, err := c.GetMetaWithTrace("missing", "gw-trace")
	if err == nil {
		t.Fatalf("expected error")
	}
	appErr, ok := err.(*errutil.AppError)
	if !ok {
		t.Fatalf("expected AppError, got %T", err)
	}
	if appErr.Code != errutil.CodeNotFound || appErr.TraceID != "dl-123" || appErr.HTTPCode != http.StatusNotFound {
		t.Fatalf("unexpected app error: %+v", appErr)
	}
}

func TestDownloaderClientGetMetaWithTraceInvalidJSON(t *testing.T) {
	c := &DownloaderClient{
		baseURL: "http://downloader.local",
		httpClient: &http.Client{
			Transport: rtFunc(func(r *http.Request) (*http.Response, error) {
				return &http.Response{
					StatusCode: http.StatusOK,
					Header:     http.Header{"Content-Type": []string{"application/json"}},
					Body:       io.NopCloser(strings.NewReader(`{"sha1":`)),
				}, nil
			}),
		},
	}

	_, err := c.GetMetaWithTrace("abc", "gw-trace")
	if err == nil {
		t.Fatalf("expected error")
	}
	appErr, ok := err.(*errutil.AppError)
	if !ok {
		t.Fatalf("expected AppError, got %T", err)
	}
	if appErr.Code != errutil.CodeBadGateway || appErr.TraceID != "gw-trace" {
		t.Fatalf("unexpected app error: %+v", appErr)
	}
}

func TestDownloaderClientStreamBookWithTrace(t *testing.T) {
	t.Run("success", func(t *testing.T) {
		c := &DownloaderClient{
			baseURL: "http://downloader.local",
			httpClient: &http.Client{
				Transport: rtFunc(func(r *http.Request) (*http.Response, error) {
					return &http.Response{
						StatusCode: http.StatusOK,
						Header:     http.Header{"Content-Type": []string{"application/octet-stream"}},
						Body:       io.NopCloser(strings.NewReader("book-bytes")),
					}, nil
				}),
			},
		}
		var b strings.Builder
		if err := c.StreamBookWithTrace("abc", &b, "gw-1"); err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if b.String() != "book-bytes" {
			t.Fatalf("unexpected stream body: %q", b.String())
		}
	})

	t.Run("error_propagation", func(t *testing.T) {
		c := &DownloaderClient{
			baseURL: "http://downloader.local",
			httpClient: &http.Client{
				Transport: rtFunc(func(r *http.Request) (*http.Response, error) {
					return &http.Response{
						StatusCode: http.StatusNotFound,
						Header:     http.Header{"Content-Type": []string{"application/json"}},
						Body:       io.NopCloser(strings.NewReader(`{"error":{"code":"NOT_FOUND","message":"NOT_FOUND","trace_id":"dl-777"}}`)),
					}, nil
				}),
			},
		}
		var b strings.Builder
		err := c.StreamBookWithTrace("missing", &b, "gw-trace")
		if err == nil {
			t.Fatalf("expected error")
		}
		appErr, ok := err.(*errutil.AppError)
		if !ok {
			t.Fatalf("expected AppError, got %T", err)
		}
		if appErr.Code != errutil.CodeNotFound || appErr.TraceID != "dl-777" || appErr.HTTPCode != http.StatusNotFound {
			t.Fatalf("unexpected app error: %+v", appErr)
		}
	})
}
