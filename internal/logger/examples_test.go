package logger_test

import (
	"context"
	"fmt"
	"net/http"
	"os"

	logger "ebusta/internal/logger"
)

// Example_basic demonstrates basic logger usage
func Example_basic() {
	// Create logger with default settings
	log := logger.Default()

	// Simple logging with TraceID
	log.Info("req-123", "Starting request processing")

	// Formatted logging
	log.Infof("req-123", "Processing %d items", 42)

	// Warning
	log.Warn("req-123", "Rate limit approaching")

	// Error
	log.Error("req-123", "Database connection failed", fmt.Errorf("connection refused"))
}

// Example_withComponent demonstrates component-based logging
func Example_withComponent() {
	// Create logger for a specific component
	log := logger.DefaultWithComponent("web-frontend")

	// All logs will include [web-frontend]
	log.Info("req-123", "Search request received")

	// Or create from existing logger
	baseLog := logger.Default()
	plasmaLog := baseLog.WithComponent("plasma")
	plasmaLog.Info("req-456", "HIT GetMeta sha1=abc123")
}

// Example_context demonstrates context-aware logging
func Example_context() {
	ctx := context.Background()

	// Add TraceID to context
	ctx = logger.ContextWithTraceID(ctx, "req-789")

	// Log with TraceID from context
	log := logger.Default()
	log.InfoCtx(ctx, "Processing request")
	log.InfofCtx(ctx, "User ID: %s", "user-123")
}

// Example_multipleOutputs demonstrates writing to multiple outputs
func Example_multipleOutputs() {
	// Create file output
	fileOut, err := logger.NewFileOutput("/var/log/ebusta/app.log", true)
	if err != nil {
		panic(err)
	}
	defer fileOut.Close()

	// Create multi-output (stdout + file)
	multiOut := logger.NewMultiOutput(
		&logger.StdoutOutput{},
		fileOut,
	)

	// Create logger with multi-output
	log := logger.New(logger.Config{
		Level:     logger.INFO,
		Formatter: logger.NewTextFormatter(),
		Output:    multiOut,
	})

	// This will go to both stdout and file
	log.Info("req-123", "Application started")
}

// Example_jsonFormat demonstrates JSON formatted logging
func Example_jsonFormat() {
	log := logger.New(logger.Config{
		Level:     logger.INFO,
		Formatter: logger.NewJSONFormatter(),
		Output:    &logger.StdoutOutput{},
	})

	// Output will be JSON
	log.Info("req-123", "JSON formatted log")
	// Output: {"time":"2024-...","level":"INFO","trace_id":"req-123","message":"JSON formatted log"}
}

// Example_envConfiguration demonstrates environment-based configuration
func Example_envConfiguration() {
	// Set environment variables
	os.Setenv("LOG_LEVEL", "DEBUG")
	os.Setenv("LOG_FORMAT", "json")
	os.Setenv("LOG_OUTPUT", "stdout")

	// Create logger from environment
	log := logger.NewFromEnv()

	log.Debug("req-123", "This will be visible due to DEBUG level")
}

// Example_httpMiddleware demonstrates HTTP middleware integration
func Example_httpMiddleware() {
	log := logger.DefaultWithComponent("web-frontend")

	// Create middleware that adds TraceID to all requests
	middleware := logger.HTTPMiddleware(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Get TraceID from context
		ctx := r.Context()
		traceID := logger.TraceIDFromContext(ctx)

		// Use context-aware logging
		log.InfofCtx(ctx, "Handling %s %s", r.Method, r.URL.Path)

		// Your handler logic here
		w.WriteHeader(http.StatusOK)
		fmt.Fprintf(w, "TraceID: %s", traceID)
	}), log)

	// Use middleware in your HTTP server
	http.Handle("/", middleware)
}

// Example_migratingFromLogPrintf demonstrates migrating from log.Printf
func Example_migratingFromLogPrintf() {
	// Before (log.Printf):
	// log.Printf("[%s] [web-frontend] Search request: %s", traceID, query)

	// After (with logger):
	log := logger.DefaultWithComponent("web-frontend")
	traceID := "req-123"
	query := "tolstoy"

	// Option 1: Explicit TraceID
	log.Infof(traceID, "Search request: %s", query)

	// Option 2: Using Component helper (drop-in replacement style)
	logger.Componentf("web-frontend", traceID, "Search request: %s", query)
}

// Example_grpcIntegration demonstrates gRPC service integration
func Example_grpcIntegration() {
	// In your gRPC service:
	// func (s *MyService) GetMeta(ctx context.Context, req *Request) (*Response, error) {
	//     // TraceID from gRPC metadata
	//     traceID := logger.TraceIDFromContext(ctx)
	//     if traceID == "" {
	//         traceID = logger.GenerateTraceID("grpc")
	//     }
	//
	//     log := logger.DefaultWithComponent("tier-node")
	//     log.Infof(traceID, "GetMeta sha1=%s", req.GetSha1())
	//
	//     // ... business logic ...
	//
	//     return &Response{...}, nil
	// }
}

// Example_fieldLogging demonstrates structured field logging
func Example_fieldLogging() {
	log := logger.DefaultWithComponent("downloader")

	// Add fields to logger
	logWithFields := log.WithFields(map[string]interface{}{
		"user_id":   "user-123",
		"platform":  "web",
	})

	// All logs from this logger will include the fields
	logWithFields.Info("req-123", "Download started")

	// Add single field
	logWithUser := log.WithField("user_id", "user-456")
	logWithUser.Info("req-456", "Another request")
}
