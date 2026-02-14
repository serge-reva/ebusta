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

// TraceIDFromContext извлекает TraceID из gRPC контекста
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

// TraceIDFromHeader извлекает TraceID из HTTP заголовка
func TraceIDFromHeader(r *http.Request) string {
    tid := r.Header.Get("X-Trace-Id")
    if tid == "" {
        tid = r.Header.Get("Trace-Id")
    }
    return tid
}

// TraceIDFromRequest извлекает TraceID из HTTP запроса или генерирует новый
func TraceIDFromRequest(r *http.Request, prefix string) string {
    tid := TraceIDFromHeader(r)
    if tid == "" {
        tid = GenerateTraceID(prefix)
    }
    return tid
}

// ContextWithTraceID добавляет TraceID в исходящий gRPC контекст
func ContextWithTraceID(ctx context.Context, traceID string) context.Context {
    md := metadata.Pairs("x-trace-id", traceID)
    return metadata.NewOutgoingContext(ctx, md)
}
