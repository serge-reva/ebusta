package logger

import (
    "bytes"
    "context"
    "encoding/json"
    "fmt"
    "net/http"
    "net/http/httptest"
    "os"
    "path/filepath"
    "regexp"
    "strings"
    "sync"
    "testing"
)

// =============================================================================
// Functional Test Helpers
// =============================================================================

// CaptureOutput captures stderr during test execution
func captureStderr(t *testing.T, fn func()) string {
    t.Helper()
    old := os.Stderr
    r, w, _ := os.Pipe()
    os.Stderr = w
    defer func() { os.Stderr = old }()

    var buf bytes.Buffer
    done := make(chan struct{})
    go func() {
        _, _ = buf.ReadFrom(r)
        close(done)
    }()

    fn()
    w.Close()
    <-done
    return buf.String()
}

// TestLoggerBuilder provides fluent interface for creating test loggers
type TestLoggerBuilder struct {
    level     Level
    formatter Formatter
    output    Output
    component string
}

func NewTestLoggerBuilder() *TestLoggerBuilder {
    return &TestLoggerBuilder{
        level:     DEBUG,
        formatter: NewTextFormatter(),
        output:    &StdoutOutput{},
        component: "test",
    }
}

func (b *TestLoggerBuilder) WithLevel(level Level) *TestLoggerBuilder {
    b.level = level
    return b
}

func (b *TestLoggerBuilder) WithJSONFormatter() *TestLoggerBuilder {
    b.formatter = NewJSONFormatter()
    return b
}

func (b *TestLoggerBuilder) WithTextFormatter(disableColors bool) *TestLoggerBuilder {
    tf := NewTextFormatter()
    tf.DisableColors = disableColors
    b.formatter = tf
    return b
}

func (b *TestLoggerBuilder) WithOutput(output Output) *TestLoggerBuilder {
    b.output = output
    return b
}

func (b *TestLoggerBuilder) WithComponent(component string) *TestLoggerBuilder {
    b.component = component
    return b
}

func (b *TestLoggerBuilder) Build() *Logger {
    return New(b.level, b.formatter, b.output, b.component)
}

// =============================================================================
// Scenario 1: Logger Lifecycle Tests
// =============================================================================

func TestScenario_LoggerLifecycle(t *testing.T) {
    t.Run("default_logger_creation", func(t *testing.T) {
        log := Default()
        if log == nil {
            t.Fatal("Default() returned nil")
        }
        if log.level != INFO {
            t.Errorf("Default level = %v, want INFO", log.level)
        }
    })

    t.Run("builder_pattern_creation", func(t *testing.T) {
        mock := &MockOutput{}
        log := NewTestLoggerBuilder().
            WithLevel(DEBUG).
            WithJSONFormatter().
            WithOutput(mock).
            WithComponent("lifecycle-test").
            Build()

        if log == nil {
            t.Fatal("Builder returned nil logger")
        }
        if log.level != DEBUG {
            t.Errorf("Level = %v, want DEBUG", log.level)
        }
        if log.component != "lifecycle-test" {
            t.Errorf("Component = %q, want 'lifecycle-test'", log.component)
        }

        log.Info("trace-1", "test message")
        if mock.String() == "" {
            t.Error("Expected output from logger")
        }
    })

    t.Run("level_change_runtime", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "level-change")

        log.Info("trace-1", "info message")
        if !strings.Contains(mock.String(), "info message") {
            t.Error("INFO should be logged at INFO level")
        }

        mock.Reset()

        log.Debug("trace-1", "debug message")
        if mock.String() != "" {
            t.Error("DEBUG should NOT be logged at INFO level")
        }

        log.SetLevel(DEBUG)
        mock.Reset()

        log.Debug("trace-1", "debug message")
        if !strings.Contains(mock.String(), "debug message") {
            t.Error("DEBUG should be logged after level change")
        }
    })

    t.Run("close_logger", func(t *testing.T) {
        tmpDir := t.TempDir()
        tmpFile := filepath.Join(tmpDir, "close-test.log")

        fo, err := NewFileOutput(tmpFile, false)
        if err != nil {
            t.Fatalf("Failed to create file output: %v", err)
        }

        log := New(INFO, NewTextFormatter(), fo, "close-test")
        log.Info("trace-1", "before close")

        if err := log.Close(); err != nil {
            t.Errorf("Close() error = %v", err)
        }

        data, err := os.ReadFile(tmpFile)
        if err != nil {
            t.Fatalf("Failed to read log file: %v", err)
        }
        if !strings.Contains(string(data), "before close") {
            t.Error("Log file should contain message")
        }
    })
}

// =============================================================================
// Scenario 2: Output Scenarios
// =============================================================================

func TestScenario_OutputSelection(t *testing.T) {
    t.Run("stdout_output", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "stdout-test")
        log.Info("trace-1", "stdout message")

        if !strings.Contains(mock.String(), "stdout message") {
            t.Error("StdoutOutput should write message")
        }
    })

    t.Run("stderr_output", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "stderr-test")
        log.Error("trace-1", "stderr error", fmt.Errorf("test error"))

        output := mock.String()
        if !strings.Contains(output, "stderr error") {
            t.Error("StderrOutput should write error message")
        }
        if !strings.Contains(output, "test error") {
            t.Error("Should contain error details")
        }
    })

    t.Run("file_output", func(t *testing.T) {
        tmpDir := t.TempDir()
        tmpFile := filepath.Join(tmpDir, "file-test.log")

        fo, err := NewFileOutput(tmpFile, false)
        if err != nil {
            t.Fatalf("Failed to create file output: %v", err)
        }

        log := New(INFO, NewTextFormatter(), fo, "file-test")
        log.Info("trace-1", "file message 1")
        log.Info("trace-2", "file message 2")
        _ = log.Close()

        data, err := os.ReadFile(tmpFile)
        if err != nil {
            t.Fatalf("Failed to read file: %v", err)
        }

        content := string(data)
        if !strings.Contains(content, "file message 1") {
            t.Error("Should contain first message")
        }
        if !strings.Contains(content, "file message 2") {
            t.Error("Should contain second message")
        }
    })

    t.Run("file_output_append_mode", func(t *testing.T) {
        tmpDir := t.TempDir()
        tmpFile := filepath.Join(tmpDir, "append-test.log")

        fo1, _ := NewFileOutput(tmpFile, false)
        log1 := New(INFO, NewTextFormatter(), fo1, "append-test")
        log1.Info("trace-1", "initial message")
        _ = log1.Close()

        fo2, _ := NewFileOutput(tmpFile, true)
        log2 := New(INFO, NewTextFormatter(), fo2, "append-test")
        log2.Info("trace-2", "appended message")
        _ = log2.Close()

        data, _ := os.ReadFile(tmpFile)
        content := string(data)

        if !strings.Contains(content, "initial message") {
            t.Error("Should contain initial message")
        }
        if !strings.Contains(content, "appended message") {
            t.Error("Should contain appended message")
        }
    })

    t.Run("multi_output", func(t *testing.T) {
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
    })

    t.Run("add_output_runtime", func(t *testing.T) {
        mock1 := &MockOutput{}
        mock2 := &MockOutput{}

        log := New(INFO, NewTextFormatter(), mock1, "add-output-test")
        log.Info("trace-1", "before add")

        log.AddOutput(mock2)
        log.Info("trace-2", "after add")

        if !strings.Contains(mock1.String(), "after add") {
            t.Error("Original output should still work")
        }
        if !strings.Contains(mock2.String(), "after add") {
            t.Error("Added output should receive messages")
        }
    })
}

// =============================================================================
// Scenario 3: Formatter Scenarios
// =============================================================================

func TestScenario_Formatters(t *testing.T) {
    t.Run("text_formatter_basic", func(t *testing.T) {
        mock := &MockOutput{}
        tf := NewTextFormatter()
        tf.DisableColors = true
        log := New(INFO, tf, mock, "text-component")

        log.Info("trace-abc123", "text message")

        output := mock.String()
        if !regexpMatch(`\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}`, output) {
            t.Error("Should contain timestamp")
        }
        if !strings.Contains(output, "[INFO]") {
            t.Error("Should contain [INFO]")
        }
        if !strings.Contains(output, "[trace-abc123]") {
            t.Error("Should contain trace ID")
        }
        if !strings.Contains(output, "[text-component]") {
            t.Error("Should contain component")
        }
        if !strings.Contains(output, "text message") {
            t.Error("Should contain message")
        }
    })

    t.Run("text_formatter_with_error", func(t *testing.T) {
        mock := &MockOutput{}
        tf := NewTextFormatter()
        tf.DisableColors = true
        log := New(ERROR, tf, mock, "error-component")

        log.Error("trace-err", "operation failed", fmt.Errorf("connection timeout"))

        output := mock.String()
        if !strings.Contains(output, "operation failed") {
            t.Error("Should contain message")
        }
        if !strings.Contains(output, "error=connection timeout") {
            t.Error("Should contain error details")
        }
    })

    t.Run("json_formatter_basic", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewJSONFormatter(), mock, "json-component")

        log.Info("trace-json-123", "json message")

        output := mock.String()
        var entry map[string]interface{}
        if err := json.Unmarshal([]byte(output), &entry); err != nil {
            t.Fatalf("Invalid JSON output: %v", err)
        }

        if entry["level"] != "INFO" {
            t.Errorf("level = %v, want INFO", entry["level"])
        }
        if entry["trace_id"] != "trace-json-123" {
            t.Errorf("trace_id = %v, want trace-json-123", entry["trace_id"])
        }
        if entry["component"] != "json-component" {
            t.Errorf("component = %v, want json-component", entry["component"])
        }
        if entry["message"] != "json message" {
            t.Errorf("message = %v, want 'json message'", entry["message"])
        }
    })

    t.Run("json_formatter_with_error", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(ERROR, NewJSONFormatter(), mock, "json-error")

        log.Error("trace-err", "operation failed", fmt.Errorf("db connection failed"))

        output := mock.String()
        var entry map[string]interface{}
        json.Unmarshal([]byte(output), &entry)

        if entry["error"] != "db connection failed" {
            t.Errorf("error = %v, want 'db connection failed'", entry["error"])
        }
    })

    t.Run("json_formatter_no_trace", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewJSONFormatter(), mock, "json-notrace")

        log.Info("", "message without trace")

        output := mock.String()
        var entry map[string]interface{}
        json.Unmarshal([]byte(output), &entry)

        if entry["trace_id"] != "no-trace" {
            t.Errorf("trace_id = %v, want 'no-trace'", entry["trace_id"])
        }
    })
}

// =============================================================================
// Scenario 4: Context and TraceID Tests
// =============================================================================

func TestScenario_ContextPropagation(t *testing.T) {
    t.Run("trace_id_in_context", func(t *testing.T) {
        ctx := context.Background()
        ctx = ContextWithTraceID(ctx, "manual-trace-123")

        traceID := TraceIDFromContext(ctx)
        if traceID != "manual-trace-123" {
            t.Errorf("TraceID = %q, want 'manual-trace-123'", traceID)
        }
    })

    t.Run("logger_in_context", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "ctx-logger")
        ctx := context.Background()
        ctx = ContextWithLogger(ctx, log)

        retrieved := LoggerFromContext(ctx)
        if retrieved == nil {
            t.Fatal("Logger not found in context")
        }

        retrieved.Info("trace-1", "from context")
        if !strings.Contains(mock.String(), "from context") {
            t.Error("Retrieved logger should work")
        }
    })

    t.Run("generate_trace_id", func(t *testing.T) {
        traceID := GenerateTraceID("http")

        if !strings.HasPrefix(traceID, "http-") {
            t.Errorf("TraceID should start with 'http-', got %s", traceID)
        }

        traceID2 := GenerateTraceID("http")
        if traceID == traceID2 {
            t.Error("TraceIDs should be unique")
        }
    })

    t.Run("trace_id_from_http_header", func(t *testing.T) {
        req := httptest.NewRequest("GET", "/test", nil)
        req.Header.Set("X-Trace-Id", "header-trace-456")

        traceID := TraceIDFromRequest(req)
        if traceID != "header-trace-456" {
            t.Errorf("TraceID = %q, want 'header-trace-456'", traceID)
        }
    })

    t.Run("generate_trace_if_missing", func(t *testing.T) {
        req := httptest.NewRequest("GET", "/test", nil)

        ctx := ContextFromRequest(req, "api")
        traceID := TraceIDFromContext(ctx)

        if !strings.HasPrefix(traceID, "api-") {
            t.Errorf("Generated TraceID should start with 'api-', got %s", traceID)
        }
    })

    t.Run("context_aware_logging", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(DEBUG, NewTextFormatter(), mock, "ctx-aware")

        ctx := context.Background()
        ctx = ContextWithTraceID(ctx, "ctx-trace-789")

        log.InfoCtx(ctx, "context aware message")

        if !strings.Contains(mock.String(), "ctx-trace-789") {
            t.Error("Should use trace ID from context")
        }
    })
}

// =============================================================================
// Scenario 5: HTTP Middleware Tests
// =============================================================================

func TestScenario_HTTPMiddleware(t *testing.T) {
    t.Run("middleware_sets_trace", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "http-middleware")

        handlerCalled := false
        var capturedTraceID string

        handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
            handlerCalled = true
            capturedTraceID = TraceIDFromContext(r.Context())
            w.WriteHeader(http.StatusOK)
        })

        middleware := HTTPMiddleware(handler, log)

        req := httptest.NewRequest("GET", "/api/test", nil)
        req.Header.Set("X-Trace-Id", "middleware-trace-123")

        rec := httptest.NewRecorder()
        middleware.ServeHTTP(rec, req)

        if !handlerCalled {
            t.Error("Handler should be called")
        }
        if capturedTraceID != "middleware-trace-123" {
            t.Errorf("TraceID = %q, want 'middleware-trace-123'", capturedTraceID)
        }
        if rec.Header().Get("X-Trace-Id") != "middleware-trace-123" {
            t.Error("Response should contain X-Trace-Id header")
        }
    })

    t.Run("middleware_generates_trace", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "http-middleware")

        var capturedTraceID string

        handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
            capturedTraceID = TraceIDFromContext(r.Context())
            w.WriteHeader(http.StatusOK)
        })

        middleware := HTTPMiddleware(handler, log)

        req := httptest.NewRequest("GET", "/api/test", nil)

        rec := httptest.NewRecorder()
        middleware.ServeHTTP(rec, req)

        if capturedTraceID == "" {
            t.Error("Should generate trace ID if missing")
        }
        if !strings.HasPrefix(capturedTraceID, "http-") {
            t.Errorf("Generated trace should start with 'http-', got %s", capturedTraceID)
        }
    })

    t.Run("full_request_lifecycle", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(DEBUG, NewTextFormatter(), mock, "api-handler")

        handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
            ctx := r.Context()
            log.InfoCtx(ctx, "processing request")
            log.DebugCtx(ctx, "debug details")

            ctxLogger := LoggerFromContext(ctx)
            if ctxLogger != nil {
                ctxLogger.WarnCtx(ctx, "warning from nested")
            }

            w.WriteHeader(http.StatusOK)
            _, _ = w.Write([]byte("OK"))
        })

        middleware := HTTPMiddleware(handler, log)

        req := httptest.NewRequest("POST", "/api/data", nil)
        req.Header.Set("X-Trace-Id", "full-lifecycle-123")

        rec := httptest.NewRecorder()
        middleware.ServeHTTP(rec, req)

        output := mock.String()

        if strings.Count(output, "full-lifecycle-123") < 3 {
            t.Error("All log messages should have same trace ID")
        }
        if !strings.Contains(output, "processing request") {
            t.Error("Should contain request log")
        }
        if !strings.Contains(output, "debug details") {
            t.Error("Should contain debug log")
        }
    })
}

// =============================================================================
// Scenario 6: Configuration Tests
// =============================================================================

func TestScenario_Configuration(t *testing.T) {
    t.Run("config_text_format", func(t *testing.T) {
        cfg := LoggerConfig{
            Level:         "DEBUG",
            Format:        "text",
            DisableColors: true,
            Outputs: OutputsConfig{
                Console: ConsoleConfig{Enabled: true},
            },
        }

        log := NewFromConfig(cfg, "config-text")
        if log.level != DEBUG {
            t.Errorf("Level = %v, want DEBUG", log.level)
        }
    })

    t.Run("config_json_format", func(t *testing.T) {
        cfg := LoggerConfig{
            Level:  "WARN",
            Format: "json",
            Outputs: OutputsConfig{
                Console: ConsoleConfig{Enabled: true},
            },
        }

        log := NewFromConfig(cfg, "config-json")
        if log.level != WARN {
            t.Errorf("Level = %v, want WARN", log.level)
        }
    })

    t.Run("config_stderr", func(t *testing.T) {
        cfg := LoggerConfig{
            Level:  "INFO",
            Format: "text",
            Outputs: OutputsConfig{
                Console: ConsoleConfig{Enabled: true, UseStderr: true},
            },
        }

        log := NewFromConfig(cfg, "config-stderr")
        if log.output.Name() != "stderr" {
            t.Errorf("Output name = %s, want 'stderr'", log.output.Name())
        }
    })

    t.Run("config_file_output", func(t *testing.T) {
        tmpDir := t.TempDir()
        tmpFile := filepath.Join(tmpDir, "config-test.log")

        cfg := LoggerConfig{
            Level:  "INFO",
            Format: "text",
            Outputs: OutputsConfig{
                Console: ConsoleConfig{Enabled: false},
                File: FileOutputConfig{
                    Enabled: true,
                    Path:    tmpFile,
                    Append:  false,
                },
            },
        }

        log := NewFromConfig(cfg, "config-file")
        log.Info("trace-1", "config file test")
        _ = log.Close()

        data, _ := os.ReadFile(tmpFile)
        if !strings.Contains(string(data), "config file test") {
            t.Error("File should contain log message")
        }
    })

    t.Run("config_merge_defaults", func(t *testing.T) {
        cfg := LoggerConfig{
            Format: "json",
        }

        merged := cfg.MergeWithDefault()
        if merged.Level != "INFO" {
            t.Errorf("Level = %q, want INFO", merged.Level)
        }
        if merged.Format != "json" {
            t.Errorf("Format = %q, want json", merged.Format)
        }
    })

    t.Run("global_init_from_config", func(t *testing.T) {
        cfg := LoggerConfig{
            Level:  "DEBUG",
            Format: "json",
            Outputs: OutputsConfig{
                Console: ConsoleConfig{Enabled: true},
            },
        }

        log := InitFromConfig(cfg, "global-config")

        if GetGlobal() != log {
            t.Error("InitFromConfig should set global logger")
        }
        if log.level != DEBUG {
            t.Errorf("Level = %v, want DEBUG", log.level)
        }
    })
}

// =============================================================================
// Scenario 7: Concurrency Tests
// =============================================================================

func TestScenario_Concurrency(t *testing.T) {
    t.Run("concurrent_logging", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "concurrent")

        var wg sync.WaitGroup
        numGoroutines := 100
        messagesPerGoroutine := 10

        for i := 0; i < numGoroutines; i++ {
            wg.Add(1)
            go func(id int) {
                defer wg.Done()
                for j := 0; j < messagesPerGoroutine; j++ {
                    traceID := fmt.Sprintf("trace-%d-%d", id, j)
                    log.Info(traceID, fmt.Sprintf("message %d-%d", id, j))
                }
            }(i)
        }

        wg.Wait()

        output := mock.String()
        messageCount := strings.Count(output, "message")

        expectedCount := numGoroutines * messagesPerGoroutine
        if messageCount != expectedCount {
            t.Errorf("Message count = %d, want %d", messageCount, expectedCount)
        }
    })

    t.Run("concurrent_level_change", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "concurrent-level")

        var wg sync.WaitGroup

        for i := 0; i < 50; i++ {
            wg.Add(1)
            go func(i int) {
                defer wg.Done()
                if i%2 == 0 {
                    log.SetLevel(DEBUG)
                } else {
                    log.SetLevel(INFO)
                }
            }(i)
        }

        for i := 0; i < 50; i++ {
            wg.Add(1)
            go func(i int) {
                defer wg.Done()
                log.Info("trace", fmt.Sprintf("concurrent %d", i))
            }(i)
        }

        wg.Wait()
    })

    t.Run("concurrent_with_fields", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "concurrent-fields")

        var wg sync.WaitGroup

        for i := 0; i < 100; i++ {
            wg.Add(1)
            go func(id int) {
                defer wg.Done()
                logger := log.WithField("goroutine", id)
                logger.Info("trace", "with field")
            }(i)
        }

        wg.Wait()
    })
}

// =============================================================================
// Scenario 8: Global Logger Tests
// =============================================================================

func TestScenario_GlobalLogger(t *testing.T) {
    t.Run("global_functions", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(DEBUG, NewTextFormatter(), mock, "global-test")
        SetGlobal(log)

        Info("global-trace", "global info message")
        if !strings.Contains(mock.String(), "global info message") {
            t.Error("Global Info should work")
        }

        mock.Reset()
        Debug("global-trace", "global debug message")
        if !strings.Contains(mock.String(), "global debug message") {
            t.Error("Global Debug should work")
        }
    })

    t.Run("component_helpers", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "")
        SetGlobal(log)

        Component("my-component", "comp-trace", "component message")
        output := mock.String()

        if !strings.Contains(output, "[my-component]") {
            t.Error("Should contain component name")
        }
        if !strings.Contains(output, "component message") {
            t.Error("Should contain message")
        }
    })

    t.Run("legacy_compatibility", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "legacy")
        SetGlobal(log)

        Printf("printf message %d", 123)
        if !strings.Contains(mock.String(), "printf message 123") {
            t.Error("Printf should work")
        }

        mock.Reset()
        Print("print message")
        if !strings.Contains(mock.String(), "print message") {
            t.Error("Print should work")
        }
    })
}

// =============================================================================
// Scenario 9: WithFields Tests
// =============================================================================

func TestScenario_WithFields(t *testing.T) {
    t.Run("single_field", func(t *testing.T) {
        mock := &MockOutput{}
        tf := NewTextFormatter()
        tf.DisableColors = true
        log := New(INFO, tf, mock, "fields-test")

        log.WithField("user_id", 12345).Info("trace-1", "user action")
        output := mock.String()

        if !strings.Contains(output, "user_id=12345") {
            t.Error("Should contain field")
        }
    })

    t.Run("multiple_fields", func(t *testing.T) {
        mock := &MockOutput{}
        tf := NewTextFormatter()
        tf.DisableColors = true
        log := New(INFO, tf, mock, "fields-test")

        log.WithFields(map[string]interface{}{
            "user_id": 12345,
            "action":  "login",
            "ip":      "192.168.1.1",
        }).Info("trace-1", "user action")

        output := mock.String()

        if !strings.Contains(output, "user_id=12345") {
            t.Error("Should contain user_id field")
        }
        if !strings.Contains(output, "action=login") {
            t.Error("Should contain action field")
        }
        if !strings.Contains(output, "ip=192.168.1.1") {
            t.Error("Should contain ip field")
        }
    })

    t.Run("nested_fields_chain", func(t *testing.T) {
        mock := &MockOutput{}
        tf := NewTextFormatter()
        tf.DisableColors = true
        log := New(INFO, tf, mock, "nested-fields")

        logger := log.WithField("request_id", "req-123")
        logger = logger.WithField("session_id", "sess-456")
        logger.Info("trace-1", "nested message")

        output := mock.String()

        if !strings.Contains(output, "request_id=req-123") {
            t.Error("Should preserve first field")
        }
        if !strings.Contains(output, "session_id=sess-456") {
            t.Error("Should add second field")
        }
    })

    t.Run("json_fields", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewJSONFormatter(), mock, "json-fields")

        log.WithFields(map[string]interface{}{
            "user_id": 12345,
            "meta": map[string]interface{}{
                "browser": "chrome",
                "os":      "linux",
            },
        }).Info("trace-1", "json with fields")

        output := mock.String()
        var entry map[string]interface{}
        json.Unmarshal([]byte(output), &entry)

        fields, ok := entry["fields"].(map[string]interface{})
        if !ok {
            t.Fatal("fields not found in JSON output")
        }
        if fields["user_id"].(float64) != 12345 {
            t.Error("Should contain user_id field")
        }
    })
}

// =============================================================================
// Scenario 10: Edge Cases
// =============================================================================

func TestScenario_EdgeCases(t *testing.T) {
    t.Run("empty_message", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "edge-test")

        log.Info("trace-1", "")
        if mock.String() == "" {
            t.Error("Should log even with empty message")
        }
    })

    t.Run("empty_trace", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "edge-test")

        log.Info("", "no trace message")
        output := mock.String()

        if !strings.Contains(output, "[no-trace]") {
            t.Error("Should show 'no-trace' for empty trace ID")
        }
    })

    t.Run("very_long_message", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "edge-test")

        longMessage := strings.Repeat("x", 10000)
        log.Info("trace-1", longMessage)

        if !strings.Contains(mock.String(), longMessage) {
            t.Error("Should handle long messages")
        }
    })

    t.Run("unicode_message", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "unicode-test")

        unicodeMessage := "Привет мир 🌍 日本語"
        log.Info("trace-1", unicodeMessage)

        if !strings.Contains(mock.String(), unicodeMessage) {
            t.Error("Should handle unicode")
        }
    })

    t.Run("nil_formatter_uses_default", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, nil, mock, "nil-formatter")

        log.Info("trace-1", "nil formatter message")

        if !strings.Contains(mock.String(), "nil formatter message") {
            t.Error("Should use default text formatter")
        }
    })

    t.Run("nil_output_uses_default", func(t *testing.T) {
        log := New(INFO, NewTextFormatter(), nil, "nil-output")

        if log.output.Name() != "stdout" {
            t.Error("Should use default stdout output")
        }
    })

    t.Run("level_zero_is_debug", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(Level(0), NewTextFormatter(), mock, "zero-level")

        // Level(0) = DEBUG, so DEBUG should be logged
        log.Debug("trace-1", "debug should appear")
        if !strings.Contains(mock.String(), "debug should appear") {
            t.Error("DEBUG should be logged when level is DEBUG (Level(0))")
        }

        mock.Reset()

        // INFO should also appear (INFO > DEBUG)
        log.Info("trace-1", "info should appear")
        if !strings.Contains(mock.String(), "info should appear") {
            t.Error("INFO should be logged")
        }
    })

    t.Run("special_characters_in_trace", func(t *testing.T) {
        mock := &MockOutput{}
        log := New(INFO, NewTextFormatter(), mock, "special-test")

        log.Info("trace-with-special-\n\t\"chars", "message")

        if mock.String() == "" {
            t.Error("Should log despite special chars in trace")
        }
    })
}

// =============================================================================
// Benchmark Tests
// =============================================================================

func BenchmarkLogger_TextFormatter(b *testing.B) {
    mock := &MockOutput{}
    log := New(INFO, NewTextFormatter(), mock, "benchmark")

    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        log.Info("trace", "benchmark message")
    }
}

func BenchmarkLogger_JSONFormatter(b *testing.B) {
    mock := &MockOutput{}
    log := New(INFO, NewJSONFormatter(), mock, "benchmark")

    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        log.Info("trace", "benchmark message")
    }
}

func BenchmarkLogger_WithFields(b *testing.B) {
    mock := &MockOutput{}
    log := New(INFO, NewTextFormatter(), mock, "benchmark")

    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        log.WithFields(map[string]interface{}{
            "user_id": 123,
            "action":  "test",
        }).Info("trace", "benchmark message")
    }
}

func BenchmarkLogger_Concurrent(b *testing.B) {
    mock := &MockOutput{}
    log := New(INFO, NewTextFormatter(), mock, "benchmark")

    b.RunParallel(func(pb *testing.PB) {
        for pb.Next() {
            log.Info("trace", "concurrent benchmark")
        }
    })
}

// =============================================================================
// Helper: Simple regex match for testing
// =============================================================================

func regexpMatch(pattern, s string) bool {
    matched, _ := regexp.MatchString(pattern, s)
    return matched
}
