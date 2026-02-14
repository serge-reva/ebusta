package errutil

import (
    "context"
    "fmt"
    "net/http"
    "time"

    "google.golang.org/grpc/metadata"
)

// GenerateTraceID генерирует уникальный TraceID с префиксом
func GenerateTraceID(prefix string) string {
    if prefix == "" {
        prefix = "req"
    }
    return fmt.Sprintf("%s-%d", prefix, time.Now().UnixNano())
}

// TraceIDFromHeader извлекает TraceID из HTTP заголовка
func TraceIDFromHeader(h http.Header) string {
    tid := h.Get("X-Trace-Id")
    if tid == "" {
        tid = h.Get("Trace-Id")
    }
    return tid
}

// TraceIDFromRequest извлекает TraceID из HTTP запроса
func TraceIDFromRequest(r *http.Request) string {
    return TraceIDFromHeader(r.Header)
}

// TraceIDFromContext извлекает TraceID из контекста (gRPC metadata)
func TraceIDFromContext(ctx context.Context) string {
    // Из gRPC metadata
    if md, ok := metadata.FromIncomingContext(ctx); ok {
        if v := md.Get("x-trace-id"); len(v) > 0 {
            return v[0]
        }
        if v := md.Get("trace-id"); len(v) > 0 {
            return v[0]
        }
    }
    return ""
}

// ContextWithTraceID добавляет TraceID в контекст (gRPC outgoing metadata)
func ContextWithTraceID(ctx context.Context, traceID string) context.Context {
    md := metadata.Pairs("x-trace-id", traceID)
    return metadata.NewOutgoingContext(ctx, md)
}
