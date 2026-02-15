package logger

import (
	"context"
	"fmt"
	"net/http"
	"time"

	"google.golang.org/grpc/metadata"
)

// contextKey is the type for context keys
type contextKey string

const (
	// traceIDKey is the context key for TraceID
	traceIDKey contextKey = "trace_id"
	// loggerKey is the context key for Logger
	loggerKey contextKey = "logger"
)

// GenerateTraceID generates a unique TraceID with optional prefix
func GenerateTraceID(prefix string) string {
	if prefix == "" {
		prefix = "req"
	}
	return fmt.Sprintf("%s-%d", prefix, time.Now().UnixNano())
}

// ContextWithTraceID adds a TraceID to the context
func ContextWithTraceID(ctx context.Context, traceID string) context.Context {
	return context.WithValue(ctx, traceIDKey, traceID)
}

// TraceIDFromContext extracts TraceID from context
// Checks in order: context value, gRPC metadata
func TraceIDFromContext(ctx context.Context) string {
	// From context value
	if tid, ok := ctx.Value(traceIDKey).(string); ok && tid != "" {
		return tid
	}

	// From gRPC metadata
	if md, ok := metadata.FromIncomingContext(ctx); ok {
		if v := md.Get("x-trace-id"); len(v) > 0 && v[0] != "" {
			return v[0]
		}
		if v := md.Get("trace-id"); len(v) > 0 && v[0] != "" {
			return v[0]
		}
	}

	return ""
}

// ContextWithLogger adds a Logger to the context
func ContextWithLogger(ctx context.Context, logger *Logger) context.Context {
	return context.WithValue(ctx, loggerKey, logger)
}

// LoggerFromContext extracts Logger from context
// Returns nil if not found
func LoggerFromContext(ctx context.Context) *Logger {
	if l, ok := ctx.Value(loggerKey).(*Logger); ok {
		return l
	}
	return nil
}

// FromContext gets or creates a Logger for the context
// If a Logger exists in context, returns it with TraceID from context
// If not, returns the default logger
func FromContext(ctx context.Context) *Logger {
	if l := LoggerFromContext(ctx); l != nil {
		return l
	}
	return Default()
}

// --- HTTP helpers ---

// TraceIDFromHeader extracts TraceID from HTTP headers
func TraceIDFromHeader(h http.Header) string {
	tid := h.Get("X-Trace-Id")
	if tid == "" {
		tid = h.Get("Trace-Id")
	}
	return tid
}

// TraceIDFromRequest extracts TraceID from HTTP request
func TraceIDFromRequest(r *http.Request) string {
	return TraceIDFromHeader(r.Header)
}

// ContextFromRequest creates a context with TraceID from HTTP request
// Generates a new TraceID if not present
func ContextFromRequest(r *http.Request, prefix string) context.Context {
	ctx := r.Context()
	tid := TraceIDFromRequest(r)
	if tid == "" {
		tid = GenerateTraceID(prefix)
	}
	return ContextWithTraceID(ctx, tid)
}

// --- gRPC helpers ---

// GRPCContextWithTraceID adds TraceID to outgoing gRPC metadata
func GRPCContextWithTraceID(ctx context.Context, traceID string) context.Context {
	md := metadata.Pairs("x-trace-id", traceID)
	return metadata.NewOutgoingContext(ctx, md)
}

// --- Middleware helpers ---

// HTTPMiddleware creates an HTTP middleware that adds TraceID to context
func HTTPMiddleware(next http.Handler, logger *Logger) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		ctx := ContextFromRequest(r, "http")

		// Add logger to context
		if logger != nil {
			ctx = ContextWithLogger(ctx, logger)
		}

		// Add TraceID to response headers
		tid := TraceIDFromContext(ctx)
		if tid != "" {
			w.Header().Set("X-Trace-Id", tid)
		}

		next.ServeHTTP(w, r.WithContext(ctx))
	})
}

// RequestLogger returns a logger with request context information
func RequestLogger(r *http.Request, baseLogger *Logger) *Logger {
	ctx := r.Context()
	tid := TraceIDFromContext(ctx)
	if tid == "" {
		tid = TraceIDFromRequest(r)
	}

	if baseLogger == nil {
		baseLogger = Default()
	}

	return baseLogger.WithFields(map[string]interface{}{
		"method":     r.Method,
		"path":       r.URL.Path,
		"remote_addr": r.RemoteAddr,
		"trace_id":   tid,
	})
}
