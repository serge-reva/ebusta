package logger

import (
    "bytes"
    "context"
    "encoding/json"
    "net/http"
    "net/http/httptest"
    "strings"
    "sync"
    "testing"
    "time"
)

// MockOutput captures log output for testing (thread-safe)
type MockOutput struct {
    mu  sync.Mutex
    buf bytes.Buffer
}

func (m *MockOutput) Write(p []byte) (n int, err error) {
    m.mu.Lock()
    defer m.mu.Unlock()
    return m.buf.Write(p)
}

func (m *MockOutput) Close() error {
    return nil
}

func (m *MockOutput) Name() string {
    return "mock"
}

func (m *MockOutput) String() string {
    m.mu.Lock()
    defer m.mu.Unlock()
    return m.buf.String()
}

func (m *MockOutput) Reset() {
    m.mu.Lock()
    defer m.mu.Unlock()
    m.buf.Reset()
}

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
            if ParseLevel(tt.input) != tt.expected {
                t.Errorf("ParseLevel(%q) = %v, want %v", tt.input, ParseLevel(tt.input), tt.expected)
            }
        })
    }
}

func TestTextFormatter(t *testing.T) {
    tf := NewTextFormatter()
    tf.DisableColors = true

    entry := &Entry{
        Time:      time.Now(),
        Level:     INFO,
        TraceID:   "test-trace",
        Component: "test-component",
        Message:   "test message",
        Fields:    map[string]interface{}{"key": "value"},
    }

    output, err := tf.Format(entry)
    if err != nil {
        t.Fatalf("Format error: %v", err)
    }

    if !strings.Contains(string(output), "[INFO]") {
        t.Error("Should contain level")
    }
    if !strings.Contains(string(output), "test-trace") {
        t.Error("Should contain trace ID")
    }
    if !strings.Contains(string(output), "test message") {
        t.Error("Should contain message")
    }
}

func TestJSONFormatter(t *testing.T) {
    jf := NewJSONFormatter()

    entry := &Entry{
        Time:      time.Now(),
        Level:     INFO,
        TraceID:   "json-trace",
        Component: "json-component",
        Message:   "json message",
        Fields:    map[string]interface{}{"key": "value"},
    }

    output, err := jf.Format(entry)
    if err != nil {
        t.Fatalf("Format error: %v", err)
    }

    var result map[string]interface{}
    if err := json.Unmarshal(output, &result); err != nil {
        t.Fatalf("Invalid JSON: %v", err)
    }

    if result["level"] != "INFO" {
        t.Errorf("level = %v, want INFO", result["level"])
    }
    if result["trace_id"] != "json-trace" {
        t.Errorf("trace_id = %v, want json-trace", result["trace_id"])
    }
}

func TestLogger(t *testing.T) {
    mock := &MockOutput{}
    log := New(DEBUG, NewTextFormatter(), mock, "")

    log.Debug("trace-1", "debug message")
    if !strings.Contains(mock.String(), "debug message") {
        t.Error("Should log debug message")
    }
}

func TestLoggerWithComponent(t *testing.T) {
    mock := &MockOutput{}
    log := New(INFO, NewTextFormatter(), mock, "main")
    componentLog := log.WithComponent("sub-component")

    componentLog.Info("trace-1", "component message")

    output := mock.String()
    if !strings.Contains(string(output), "[sub-component]") {
        t.Errorf("Should contain component name, got: %s", output)
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
    req.Header.Set("X-Trace-Id", "req-trace-456")

    ctx := ContextFromRequest(req, "api")
    traceID := TraceIDFromContext(ctx)

    if traceID != "req-trace-456" {
        t.Errorf("TraceID = %q, want req-trace-456", traceID)
    }
}

func TestMultiOutput(t *testing.T) {
    mock1 := &MockOutput{}
    mock2 := &MockOutput{}
    multi := NewMultiOutput(mock1, mock2)

    log := New(INFO, NewTextFormatter(), multi, "multi-test")
    log.Info("trace-1", "multi message")

    if !strings.Contains(mock1.String(), "multi message") {
        t.Error("First output should contain message")
    }
    if !strings.Contains(mock2.String(), "multi message") {
        t.Error("Second output should contain message")
    }
}

func TestGenerateTraceID(t *testing.T) {
    traceID := GenerateTraceID("http")

    if !strings.HasPrefix(traceID, "http-") {
        t.Errorf("TraceID should start with 'http-', got %s", traceID)
    }

    traceID2 := GenerateTraceID("http")
    if traceID == traceID2 {
        t.Error("TraceIDs should be unique")
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

    log := NewFromConfig(cfg, "test")
    if log == nil {
        t.Fatal("NewFromConfig returned nil")
    }
}

func TestDefaultConfig(t *testing.T) {
    cfg := DefaultConfig()
    if cfg.Level != "INFO" {
        t.Errorf("Default level = %s, want INFO", cfg.Level)
    }
    if cfg.Format != "text" {
        t.Errorf("Default format = %s, want text", cfg.Format)
    }
}

func TestMergeWithDefault(t *testing.T) {
    cfg := LoggerConfig{
        Format: "json",
    }

    merged := cfg.MergeWithDefault()
    if merged.Level != "INFO" {
        t.Errorf("Merged level = %s, want INFO", merged.Level)
    }
    if merged.Format != "json" {
        t.Errorf("Merged format = %s, want json", merged.Format)
    }
}

func TestHTTPMiddleware(t *testing.T) {
    mock := &MockOutput{}
    log := New(INFO, NewTextFormatter(), mock, "http-test")

    var capturedTraceID string
    handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        capturedTraceID = TraceIDFromContext(r.Context())
        w.WriteHeader(http.StatusOK)
    })

    middleware := HTTPMiddleware(handler, log)

    req := httptest.NewRequest("GET", "/test", nil)
    req.Header.Set("X-Trace-Id", "middleware-trace")

    rec := httptest.NewRecorder()
    middleware.ServeHTTP(rec, req)

    if capturedTraceID != "middleware-trace" {
        t.Errorf("TraceID = %q, want middleware-trace", capturedTraceID)
    }
}
