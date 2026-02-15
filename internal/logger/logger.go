package logger

import (
	"context"
	"fmt"
	"os"
	"sync"
	"time"
)

// Logger is the main logger type
type Logger struct {
	mu        sync.RWMutex
	level     Level
	formatter Formatter
	output    Output
	component string
	fields    map[string]interface{}
}

// New creates a new Logger with the given configuration
func New(level Level, formatter Formatter, output Output, component string) *Logger {
	l := &Logger{
		level:     level,
		formatter: formatter,
		output:    output,
		component: component,
		fields:    make(map[string]interface{}),
	}

	// Set defaults
	if l.level == 0 {
		l.level = INFO
	}
	if l.formatter == nil {
		l.formatter = NewTextFormatter()
	}
	if l.output == nil {
		l.output = &StdoutOutput{}
	}
	if l.fields == nil {
		l.fields = make(map[string]interface{})
	}

	return l
}

// Default creates a new Logger with default settings (INFO level, stdout, text format)
func Default() *Logger {
	return New(INFO, NewTextFormatter(), &StdoutOutput{}, "")
}

// WithComponent returns a new Logger with the given component name
func (l *Logger) WithComponent(component string) *Logger {
	l.mu.RLock()
	defer l.mu.RUnlock()

	newLogger := &Logger{
		level:     l.level,
		formatter: l.formatter,
		output:    l.output,
		component: component,
		fields:    make(map[string]interface{}),
	}

	// Copy fields
	for k, v := range l.fields {
		newLogger.fields[k] = v
	}

	return newLogger
}

// WithField returns a new Logger with an additional field
func (l *Logger) WithField(key string, value interface{}) *Logger {
	return l.WithFields(map[string]interface{}{key: value})
}

// WithFields returns a new Logger with additional fields
func (l *Logger) WithFields(fields map[string]interface{}) *Logger {
	l.mu.RLock()
	defer l.mu.RUnlock()

	newLogger := &Logger{
		level:     l.level,
		formatter: l.formatter,
		output:    l.output,
		component: l.component,
		fields:    make(map[string]interface{}),
	}

	// Copy existing fields
	for k, v := range l.fields {
		newLogger.fields[k] = v
	}

	// Add new fields
	for k, v := range fields {
		newLogger.fields[k] = v
	}

	return newLogger
}

// SetLevel sets the minimum log level
func (l *Logger) SetLevel(level Level) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.level = level
}

// SetFormatter sets the log formatter
func (l *Logger) SetFormatter(formatter Formatter) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.formatter = formatter
}

// SetOutput sets the log output
func (l *Logger) SetOutput(output Output) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.output = output
}

// AddOutput adds an additional output (creates MultiOutput if needed)
func (l *Logger) AddOutput(output Output) {
	l.mu.Lock()
	defer l.mu.Unlock()

	if mo, ok := l.output.(*MultiOutput); ok {
		mo.Add(output)
	} else {
		l.output = NewMultiOutput(l.output, output)
	}
}

// Close closes the logger and its outputs
func (l *Logger) Close() error {
	l.mu.RLock()
	defer l.mu.RUnlock()
	return l.output.Close()
}

// log is the internal logging function
func (l *Logger) log(level Level, traceID, message string, err error, fields map[string]interface{}) {
	l.mu.RLock()
	defer l.mu.RUnlock()

	// Check level
	if level < l.level {
		return
	}

	// Build entry
	entry := &Entry{
		Time:      time.Now(),
		Level:     level,
		TraceID:   traceID,
		Component: l.component,
		Message:   message,
		Error:     err,
		Fields:    make(map[string]interface{}),
	}

	// Copy default fields
	for k, v := range l.fields {
		entry.Fields[k] = v
	}

	// Add entry-specific fields
	for k, v := range fields {
		entry.Fields[k] = v
	}

	// Format
	data, ferr := l.formatter.Format(entry)
	if ferr != nil {
		// Fallback: write error message
		fmt.Fprintf(os.Stderr, "logger format error: %v\n", ferr)
		return
	}

	// Write
	l.output.Write(data)

	// Handle FATAL
	if level == FATAL {
		os.Exit(1)
	}
}

// --- Logging methods ---

// Debug logs a debug message
func (l *Logger) Debug(traceID, message string) {
	l.log(DEBUG, traceID, message, nil, nil)
}

// Debugf logs a formatted debug message
func (l *Logger) Debugf(traceID, format string, args ...interface{}) {
	l.log(DEBUG, traceID, fmt.Sprintf(format, args...), nil, nil)
}

// Info logs an info message
func (l *Logger) Info(traceID, message string) {
	l.log(INFO, traceID, message, nil, nil)
}

// Infof logs a formatted info message
func (l *Logger) Infof(traceID, format string, args ...interface{}) {
	l.log(INFO, traceID, fmt.Sprintf(format, args...), nil, nil)
}

// Warn logs a warning message
func (l *Logger) Warn(traceID, message string) {
	l.log(WARN, traceID, message, nil, nil)
}

// Warnf logs a formatted warning message
func (l *Logger) Warnf(traceID, format string, args ...interface{}) {
	l.log(WARN, traceID, fmt.Sprintf(format, args...), nil, nil)
}

// Error logs an error message
func (l *Logger) Error(traceID, message string, err error) {
	l.log(ERROR, traceID, message, err, nil)
}

// Errorf logs a formatted error message
func (l *Logger) Errorf(traceID, format string, args ...interface{}) {
	l.log(ERROR, traceID, fmt.Sprintf(format, args...), nil, nil)
}

// Fatal logs a fatal message and exits
func (l *Logger) Fatal(traceID, message string, err error) {
	l.log(FATAL, traceID, message, err, nil)
}

// Fatalf logs a formatted fatal message and exits
func (l *Logger) Fatalf(traceID, format string, args ...interface{}) {
	l.log(FATAL, traceID, fmt.Sprintf(format, args...), nil, nil)
}

// --- Context-aware methods ---

// DebugCtx logs a debug message with TraceID from context
func (l *Logger) DebugCtx(ctx context.Context, message string) {
	l.log(DEBUG, TraceIDFromContext(ctx), message, nil, nil)
}

// DebugfCtx logs a formatted debug message with TraceID from context
func (l *Logger) DebugfCtx(ctx context.Context, format string, args ...interface{}) {
	l.log(DEBUG, TraceIDFromContext(ctx), fmt.Sprintf(format, args...), nil, nil)
}

// InfoCtx logs an info message with TraceID from context
func (l *Logger) InfoCtx(ctx context.Context, message string) {
	l.log(INFO, TraceIDFromContext(ctx), message, nil, nil)
}

// InfofCtx logs a formatted info message with TraceID from context
func (l *Logger) InfofCtx(ctx context.Context, format string, args ...interface{}) {
	l.log(INFO, TraceIDFromContext(ctx), fmt.Sprintf(format, args...), nil, nil)
}

// WarnCtx logs a warning message with TraceID from context
func (l *Logger) WarnCtx(ctx context.Context, message string) {
	l.log(WARN, TraceIDFromContext(ctx), message, nil, nil)
}

// WarnfCtx logs a formatted warning message with TraceID from context
func (l *Logger) WarnfCtx(ctx context.Context, format string, args ...interface{}) {
	l.log(WARN, TraceIDFromContext(ctx), fmt.Sprintf(format, args...), nil, nil)
}

// ErrorCtx logs an error message with TraceID from context
func (l *Logger) ErrorCtx(ctx context.Context, message string, err error) {
	l.log(ERROR, TraceIDFromContext(ctx), message, err, nil)
}

// ErrorfCtx logs a formatted error message with TraceID from context
func (l *Logger) ErrorfCtx(ctx context.Context, format string, args ...interface{}) {
	l.log(ERROR, TraceIDFromContext(ctx), fmt.Sprintf(format, args...), nil, nil)
}

// FatalCtx logs a fatal message with TraceID from context and exits
func (l *Logger) FatalCtx(ctx context.Context, message string, err error) {
	l.log(FATAL, TraceIDFromContext(ctx), message, err, nil)
}
