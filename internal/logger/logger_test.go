package logger

import (
	"bytes"
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"
)

func TestLevel(t *testing.T) {
	tests := []struct {
		level    Level
		expected string
	}{
		{DEBUG, "DEBUG"},
		{INFO, "INFO"},
		{WARN, "WARN"},
		{ERROR, "ERROR"},
		{FATAL, "FATAL"},
	}

	for _, tt := range tests {
		t.Run(tt.expected, func(t *testing.T) {
			if tt.level.String() != tt.expected {
				t.Errorf("Level.String() = %s, want %s", tt.level.String(), tt.expected)
			}
		})
	}
}

func TestParseLevel(t *testing.T) {
	tests := []struct {
		input    string
		expected Level
	}{
		{"DEBUG", DEBUG},
		{"debug", DEBUG},
		{"INFO", INFO},
		{"info", INFO},
		{"WARN", WARN},
		{"WARNING", WARN},
		{"warn", WARN},
		{"ERROR", ERROR},
		{"error", ERROR},
		{"FATAL", FATAL},
		{"fatal", FATAL},
		{"", INFO},
		{"unknown", INFO},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			result := ParseLevel(tt.input)
			if result != tt.expected {
				t.Errorf("ParseLevel(%q) = %v, want %v", tt.input, result, tt.expected)
			}
		})
	}
}

func TestTextFormatter(t *testing.T) {
	f := NewTextFormatter()
	f.DisableColors = true

	entry := &Entry{
		Time:      time.Now(),
		Level:     INFO,
		TraceID:   "test-123",
		Component: "test",
		Message:   "hello world",
	}

	output, err := f.Format(entry)
	if err != nil {
		t.Fatalf("Format error: %v", err)
	}

	s := string(output)
	if !strings.Contains(s, "[INFO]") {
		t.Error("Output should contain [INFO]")
	}
	if !strings.Contains(s, "test-123") {
		t.Error("Output should contain trace ID")
	}
	if !strings.Contains(s, "test") {
		t.Error("Output should contain component")
	}
	if !strings.Contains(s, "hello world") {
		t.Error("Output should contain message")
	}
}

func TestJSONFormatter(t *testing.T) {
	f := NewJSONFormatter()

	entry := &Entry{
		Time:      time.Now(),
		Level:     INFO,
		TraceID:   "test-123",
		Component: "test",
		Message:   "hello world",
		Fields:    map[string]interface{}{"key": "value"},
	}

	output, err := f.Format(entry)
	if err != nil {
		t.Fatalf("Format error: %v", err)
	}

	var result map[string]interface{}
	if err := json.Unmarshal(output, &result); err != nil {
		t.Fatalf("JSON parse error: %v", err)
	}

	if result["level"] != "INFO" {
		t.Errorf("level = %v, want INFO", result["level"])
	}
	if result["trace_id"] != "test-123" {
		t.Errorf("trace_id = %v, want test-123", result["trace_id"])
	}
}

// MockOutput captures log output for testing
type MockOutput struct {
	buf bytes.Buffer
}

func (m *MockOutput) Write(p []byte) (n int, err error) {
	return m.buf.Write(p)
}

func (m *MockOutput) Close() error {
	return nil
}

func (m *MockOutput) Name() string {
	return "mock"
}

func (m *MockOutput) String() string {
	return m.buf.String()
}

func TestLogger(t *testing.T) {
	mock := &MockOutput{}
	log := New(DEBUG, NewTextFormatter(), mock, "")

	log.Debug("trace-1", "debug message")
	if !strings.Contains(mock.String(), "debug message") {
		t.Error("Should contain debug message")
	}

	mock.buf.Reset()
	log.Info("trace-1", "info message")
	if !strings.Contains(mock.String(), "info message") {
		t.Error("Should contain info message")
	}

	// Test level filtering
	mock.buf.Reset()
	log.SetLevel(WARN)
	log.Info("trace-1", "should not appear")
	if mock.String() != "" {
		t.Error("Info should be filtered at WARN level")
	}
}

func TestLoggerWithComponent(t *testing.T) {
	mock := &MockOutput{}
	log := New(INFO, NewTextFormatter(), mock, "")

	componentLog := log.WithComponent("mycomponent")
	componentLog.Info("trace-1", "test message")

	s := mock.String()
	if !strings.Contains(s, "[mycomponent]") {
		t.Error("Should contain component name")
	}
}

func TestContextWithTraceID(t *testing.T) {
	ctx := context.Background()
	ctx = ContextWithTraceID(ctx, "test-trace-123")

	traceID := TraceIDFromContext(ctx)
	if traceID != "test-trace-123" {
		t.Errorf("TraceID = %q, want test-trace-123", traceID)
	}
}

func TestContextFromRequest(t *testing.T) {
	req := httptest.NewRequest("GET", "/test", nil)
	req.Header.Set("X-Trace-Id", "header-trace-123")

	ctx := ContextFromRequest(req, "http")
	traceID := TraceIDFromContext(ctx)

	if traceID != "header-trace-123" {
		t.Errorf("TraceID = %q, want header-trace-123", traceID)
	}

	// Test without TraceID header
	req2 := httptest.NewRequest("GET", "/test", nil)
	ctx2 := ContextFromRequest(req2, "http")
	traceID2 := TraceIDFromContext(ctx2)

	if traceID2 == "" {
		t.Error("TraceID should be generated when not in header")
	}
	if !strings.HasPrefix(traceID2, "http-") {
		t.Errorf("Generated TraceID should have prefix 'http-', got %s", traceID2)
	}
}

func TestMultiOutput(t *testing.T) {
	mock1 := &MockOutput{}
	mock2 := &MockOutput{}

	multi := NewMultiOutput(mock1, mock2)
	log := New(INFO, NewTextFormatter(), multi, "")

	log.Info("trace-1", "multi output test")

	if !strings.Contains(mock1.String(), "multi output test") {
		t.Error("mock1 should contain message")
	}
	if !strings.Contains(mock2.String(), "multi output test") {
		t.Error("mock2 should contain message")
	}
}

func TestGenerateTraceID(t *testing.T) {
	traceID := GenerateTraceID("req")

	if !strings.HasPrefix(traceID, "req-") {
		t.Errorf("TraceID should start with 'req-', got %s", traceID)
	}

	traceID2 := GenerateTraceID("")
	if !strings.HasPrefix(traceID2, "req-") {
		t.Errorf("Default prefix should be 'req-', got %s", traceID2)
	}
}

func TestNewFromConfig(t *testing.T) {
	cfg := LoggerConfig{
		Level:  "DEBUG",
		Format: "json",
		Outputs: OutputsConfig{
			Console: ConsoleConfig{Enabled: true},
		},
	}

	log := NewFromConfig(cfg, "test-component")
	if log == nil {
		t.Fatal("Logger should not be nil")
	}

	// Check level
	if log.level != DEBUG {
		t.Errorf("Level = %v, want DEBUG", log.level)
	}

	// Check component
	if log.component != "test-component" {
		t.Errorf("Component = %q, want test-component", log.component)
	}
}

func TestDefaultConfig(t *testing.T) {
	cfg := DefaultConfig()

	if cfg.Level != "INFO" {
		t.Errorf("Default level = %q, want INFO", cfg.Level)
	}
	if cfg.Format != "text" {
		t.Errorf("Default format = %q, want text", cfg.Format)
	}
}

func TestMergeWithDefault(t *testing.T) {
	cfg := LoggerConfig{
		Level: "DEBUG",
		// Format is empty, should be filled from default
	}

	merged := cfg.MergeWithDefault()

	if merged.Level != "DEBUG" {
		t.Errorf("Level = %q, want DEBUG", merged.Level)
	}
	if merged.Format != "text" {
		t.Errorf("Format = %q, want text (default)", merged.Format)
	}
}

func TestHTTPMiddleware(t *testing.T) {
	mock := &MockOutput{}
	log := New(INFO, NewTextFormatter(), mock, "")

	handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		ctx := r.Context()
		traceID := TraceIDFromContext(ctx)
		if traceID == "" {
			t.Error("TraceID should be set in context")
		}
		w.WriteHeader(http.StatusOK)
	})

	middleware := HTTPMiddleware(handler, log)

	req := httptest.NewRequest("GET", "/test", nil)
	req.Header.Set("X-Trace-Id", "middleware-test")

	rec := httptest.NewRecorder()
	middleware.ServeHTTP(rec, req)

	if rec.Header().Get("X-Trace-Id") != "middleware-test" {
		t.Error("Response should contain X-Trace-Id header")
	}
}
