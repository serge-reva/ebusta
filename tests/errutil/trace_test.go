package errutil_test

import (
    "context"
    "net/http"
    "net/http/httptest"
    "testing"

    "ebusta/internal/errutil"
    "google.golang.org/grpc/metadata"
)

func TestGenerateTraceID(t *testing.T) {
    id := errutil.GenerateTraceID("test")
    if id == "" {
        t.Error("TraceID is empty")
    }
    if len(id) < 5 {
        t.Errorf("TraceID too short: %s", id)
    }
}

func TestGenerateTraceID_EmptyPrefix(t *testing.T) {
    id := errutil.GenerateTraceID("")
    if id == "" {
        t.Error("TraceID is empty")
    }
    // Должен использовать префикс по умолчанию "req"
    if id[:3] != "req" {
        t.Errorf("Expected default prefix 'req', got: %s", id[:3])
    }
}

func TestGenerateTraceID_Uniqueness(t *testing.T) {
    ids := make(map[string]bool)
    for i := 0; i < 1000; i++ {
        id := errutil.GenerateTraceID("test")
        if ids[id] {
            t.Errorf("Duplicate TraceID: %s", id)
        }
        ids[id] = true
    }
}

func TestTraceIDFromHeader(t *testing.T) {
    h := make(http.Header)
    h.Set("X-Trace-Id", "test-trace-123")

    id := errutil.TraceIDFromHeader(h)
    if id != "test-trace-123" {
        t.Errorf("TraceID = %q, want %q", id, "test-trace-123")
    }
}

func TestTraceIDFromHeader_AlternateName(t *testing.T) {
    h := make(http.Header)
    h.Set("Trace-Id", "alt-trace-456")

    id := errutil.TraceIDFromHeader(h)
    if id != "alt-trace-456" {
        t.Errorf("TraceID = %q, want %q", id, "alt-trace-456")
    }
}

func TestTraceIDFromHeader_Empty(t *testing.T) {
    h := make(http.Header)

    id := errutil.TraceIDFromHeader(h)
    if id != "" {
        t.Errorf("TraceID = %q, want empty", id)
    }
}

func TestTraceIDFromRequest_WithHeader(t *testing.T) {
    req := httptest.NewRequest("GET", "/test", nil)
    req.Header.Set("X-Trace-Id", "req-trace-789")

    id := errutil.TraceIDFromRequest(req)
    if id != "req-trace-789" {
        t.Errorf("TraceID = %q, want %q", id, "req-trace-789")
    }
}

func TestTraceIDFromRequest_WithoutHeader(t *testing.T) {
    req := httptest.NewRequest("GET", "/test", nil)

    id := errutil.TraceIDFromRequest(req)
    if id != "" {
        t.Errorf("TraceID = %q, want empty", id)
    }
}

func TestTraceIDFromContext(t *testing.T) {
    ctx := context.Background()
    // Without metadata
    id := errutil.TraceIDFromContext(ctx)
    if id != "" {
        t.Errorf("TraceID = %q, want empty", id)
    }
}

func TestTraceIDFromContext_Empty(t *testing.T) {
    ctx := context.Background()
    id := errutil.TraceIDFromContext(ctx)
    if id != "" {
        t.Errorf("Expected empty TraceID, got %q", id)
    }
}

func TestContextWithTraceID(t *testing.T) {
    ctx := context.Background()
    ctx = errutil.ContextWithTraceID(ctx, "outgoing-trace")

    md, ok := metadata.FromOutgoingContext(ctx)
    if !ok {
        t.Error("No metadata in context")
        return
    }

    v := md.Get("x-trace-id")
    if len(v) == 0 || v[0] != "outgoing-trace" {
        t.Errorf("x-trace-id = %v, want %q", md.Get("x-trace-id"), "outgoing-trace")
    }
}
