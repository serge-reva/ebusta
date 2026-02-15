package logger

import (
	"context"
	"fmt"
	"os"
	"sync"
)

var (
	globalLogger *Logger
	globalMu     sync.RWMutex
)

func init() {
	// Initialize global logger from environment
	globalLogger = NewFromEnv()
}

// SetGlobal sets the global logger
func SetGlobal(l *Logger) {
	globalMu.Lock()
	defer globalMu.Unlock()
	globalLogger = l
}

// GetGlobal returns the global logger
func GetGlobal() *Logger {
	globalMu.RLock()
	defer globalMu.RUnlock()
	return globalLogger
}

// --- Package-level convenience functions ---

// Debug logs a debug message using the global logger
func Debug(traceID, message string) {
	GetGlobal().Debug(traceID, message)
}

// Debugf logs a formatted debug message using the global logger
func Debugf(traceID, format string, args ...interface{}) {
	GetGlobal().Debugf(traceID, format, args...)
}

// Info logs an info message using the global logger
func Info(traceID, message string) {
	GetGlobal().Info(traceID, message)
}

// Infof logs a formatted info message using the global logger
func Infof(traceID, format string, args ...interface{}) {
	GetGlobal().Infof(traceID, format, args...)
}

// Warn logs a warning message using the global logger
func Warn(traceID, message string) {
	GetGlobal().Warn(traceID, message)
}

// Warnf logs a formatted warning message using the global logger
func Warnf(traceID, format string, args ...interface{}) {
	GetGlobal().Warnf(traceID, format, args...)
}

// Error logs an error message using the global logger
func Error(traceID, message string, err error) {
	GetGlobal().Error(traceID, message, err)
}

// Errorf logs a formatted error message using the global logger
func Errorf(traceID, format string, args ...interface{}) {
	GetGlobal().Errorf(traceID, format, args...)
}

// Fatal logs a fatal message and exits using the global logger
func Fatal(traceID, message string, err error) {
	GetGlobal().Fatal(traceID, message, err)
}

// Fatalf logs a formatted fatal message and exits using the global logger
func Fatalf(traceID, format string, args ...interface{}) {
	GetGlobal().Fatalf(traceID, format, args...)
}

// --- Context-aware package-level functions ---

// DebugCtx logs a debug message with TraceID from context
func DebugCtx(ctx context.Context, message string) {
	GetGlobal().Debug(TraceIDFromContext(ctx), message)
}

// DebugfCtx logs a formatted debug message with TraceID from context
func DebugfCtx(ctx context.Context, format string, args ...interface{}) {
	GetGlobal().Debugf(TraceIDFromContext(ctx), format, args...)
}

// InfoCtx logs an info message with TraceID from context
func InfoCtx(ctx context.Context, message string) {
	GetGlobal().Info(TraceIDFromContext(ctx), message)
}

// InfofCtx logs a formatted info message with TraceID from context
func InfofCtx(ctx context.Context, format string, args ...interface{}) {
	GetGlobal().Infof(TraceIDFromContext(ctx), format, args...)
}

// WarnCtx logs a warning message with TraceID from context
func WarnCtx(ctx context.Context, message string) {
	GetGlobal().Warn(TraceIDFromContext(ctx), message)
}

// WarnfCtx logs a formatted warning message with TraceID from context
func WarnfCtx(ctx context.Context, format string, args ...interface{}) {
	GetGlobal().Warnf(TraceIDFromContext(ctx), format, args...)
}

// ErrorCtx logs an error message with TraceID from context
func ErrorCtx(ctx context.Context, message string, err error) {
	GetGlobal().Error(TraceIDFromContext(ctx), message, err)
}

// ErrorfCtx logs a formatted error message with TraceID from context
func ErrorfCtx(ctx context.Context, format string, args ...interface{}) {
	GetGlobal().Errorf(TraceIDFromContext(ctx), format, args...)
}

// FatalCtx logs a fatal message with TraceID from context and exits
func FatalCtx(ctx context.Context, message string, err error) {
	GetGlobal().Fatal(TraceIDFromContext(ctx), message, err)
}

// --- Simple component-style logging (migration helper) ---
// These functions help migrate from log.Printf("[%s] [component] message...", traceID, args...)

// Component logs a message with component prefix: [traceID] [component] message
// This is a drop-in replacement for: log.Printf("[%s] [%s] %s", traceID, component, message)
func Component(component, traceID, message string) {
	l := GetGlobal().WithComponent(component)
	l.Info(traceID, message)
}

// Componentf logs a formatted message with component prefix
func Componentf(component, traceID, format string, args ...interface{}) {
	l := GetGlobal().WithComponent(component)
	l.Infof(traceID, format, args...)
}

// ComponentDebug logs a debug message with component prefix
func ComponentDebug(component, traceID, message string) {
	l := GetGlobal().WithComponent(component)
	l.Debug(traceID, message)
}

// ComponentDebugf logs a formatted debug message with component prefix
func ComponentDebugf(component, traceID, format string, args ...interface{}) {
	l := GetGlobal().WithComponent(component)
	l.Debugf(traceID, format, args...)
}

// ComponentError logs an error message with component prefix
func ComponentError(component, traceID, message string, err error) {
	l := GetGlobal().WithComponent(component)
	l.Error(traceID, message, err)
}

// ComponentErrorf logs a formatted error message with component prefix
func ComponentErrorf(component, traceID, format string, args ...interface{}) {
	l := GetGlobal().WithComponent(component)
	l.Errorf(traceID, format, args...)
}

// --- Legacy compatibility ---

// Printf is a compatibility function for log.Printf style logging
// Format: [traceID] message...
// This is NOT recommended for new code, use explicit level functions instead
func Printf(format string, args ...interface{}) {
	GetGlobal().Info("", fmt.Sprintf(format, args...))
}

// Print is a compatibility function for log.Print style logging
func Print(args ...interface{}) {
	GetGlobal().Info("", fmt.Sprint(args...))
}

// Println is a compatibility function for log.Println style logging
func Println(args ...interface{}) {
	GetGlobal().Info("", fmt.Sprintln(args...))
}

// --- Fatal compatibility ---

// Exit calls os.Exit with the given code
func Exit(code int) {
	os.Exit(code)
}
