package logger

import (
    "context"
    "fmt"
    "os"
    "sync"
    "time"
)

type Logger struct {
    mu        sync.RWMutex
    level     Level
    formatter Formatter
    output    Output
    component string
    fields    map[string]interface{}
}

func New(level Level, formatter Formatter, output Output, component string) *Logger {
    l := &Logger{
        level:     level,
        formatter: formatter,
        output:    output,
        component: component,
        fields:    make(map[string]interface{}),
    }
    if l.formatter == nil {
        l.formatter = NewTextFormatter()
    }
    if l.output == nil {
        l.output = &StdoutOutput{}
    }
    return l
}

func Default() *Logger {
    return New(INFO, NewTextFormatter(), &StdoutOutput{}, "")
}

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
    for k, v := range l.fields {
        newLogger.fields[k] = v
    }
    return newLogger
}

func (l *Logger) WithField(key string, value interface{}) *Logger {
    return l.WithFields(map[string]interface{}{key: value})
}

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
    for k, v := range l.fields {
        newLogger.fields[k] = v
    }
    for k, v := range fields {
        newLogger.fields[k] = v
    }
    return newLogger
}

func (l *Logger) SetLevel(level Level) {
    l.mu.Lock()
    defer l.mu.Unlock()
    l.level = level
}

func (l *Logger) SetOutput(output Output) {
    l.mu.Lock()
    defer l.mu.Unlock()
    l.output = output
}

func (l *Logger) AddOutput(output Output) {
    l.mu.Lock()
    defer l.mu.Unlock()
    if mo, ok := l.output.(*MultiOutput); ok {
        mo.Add(output)
    } else {
        l.output = NewMultiOutput(l.output, output)
    }
}

func (l *Logger) Close() error {
    l.mu.RLock()
    defer l.mu.RUnlock()
    return l.output.Close()
}

func (l *Logger) log(level Level, traceID, message string, err error, fields map[string]interface{}) {
    l.mu.RLock()
    defer l.mu.RUnlock()

    if level < l.level {
        return
    }

    entry := &Entry{
        Time:      time.Now(),
        Level:     level,
        TraceID:   traceID,
        Component: l.component,
        Message:   message,
        Error:     err,
        Fields:    make(map[string]interface{}),
    }
    for k, v := range l.fields {
        entry.Fields[k] = v
    }
    for k, v := range fields {
        entry.Fields[k] = v
    }

    data, ferr := l.formatter.Format(entry)
    if ferr != nil {
        fmt.Fprintf(os.Stderr, "logger format error: %v\n", ferr)
        return
    }
    l.output.Write(data)

    if level == FATAL {
        os.Exit(1)
    }
}

func (l *Logger) Debug(traceID, message string)                     { l.log(DEBUG, traceID, message, nil, nil) }
func (l *Logger) Debugf(traceID, format string, args ...interface{}) { l.log(DEBUG, traceID, fmt.Sprintf(format, args...), nil, nil) }
func (l *Logger) Info(traceID, message string)                      { l.log(INFO, traceID, message, nil, nil) }
func (l *Logger) Infof(traceID, format string, args ...interface{})  { l.log(INFO, traceID, fmt.Sprintf(format, args...), nil, nil) }
func (l *Logger) Warn(traceID, message string)                      { l.log(WARN, traceID, message, nil, nil) }
func (l *Logger) Warnf(traceID, format string, args ...interface{})  { l.log(WARN, traceID, fmt.Sprintf(format, args...), nil, nil) }
func (l *Logger) Error(traceID, message string, err error)          { l.log(ERROR, traceID, message, err, nil) }
func (l *Logger) Errorf(traceID, format string, args ...interface{}) { l.log(ERROR, traceID, fmt.Sprintf(format, args...), nil, nil) }
func (l *Logger) Fatal(traceID, message string, err error)          { l.log(FATAL, traceID, message, err, nil) }
func (l *Logger) Fatalf(traceID, format string, args ...interface{}) { l.log(FATAL, traceID, fmt.Sprintf(format, args...), nil, nil) }

// Context-aware methods
func (l *Logger) DebugCtx(ctx context.Context, message string)                     { l.log(DEBUG, TraceIDFromContext(ctx), message, nil, nil) }
func (l *Logger) DebugfCtx(ctx context.Context, format string, args ...interface{}) { l.log(DEBUG, TraceIDFromContext(ctx), fmt.Sprintf(format, args...), nil, nil) }
func (l *Logger) InfoCtx(ctx context.Context, message string)                      { l.log(INFO, TraceIDFromContext(ctx), message, nil, nil) }
func (l *Logger) InfofCtx(ctx context.Context, format string, args ...interface{})  { l.log(INFO, TraceIDFromContext(ctx), fmt.Sprintf(format, args...), nil, nil) }
func (l *Logger) WarnCtx(ctx context.Context, message string)                      { l.log(WARN, TraceIDFromContext(ctx), message, nil, nil) }
func (l *Logger) WarnfCtx(ctx context.Context, format string, args ...interface{})  { l.log(WARN, TraceIDFromContext(ctx), fmt.Sprintf(format, args...), nil, nil) }
func (l *Logger) ErrorCtx(ctx context.Context, message string, err error)          { l.log(ERROR, TraceIDFromContext(ctx), message, err, nil) }
func (l *Logger) ErrorfCtx(ctx context.Context, format string, args ...interface{}) { l.log(ERROR, TraceIDFromContext(ctx), fmt.Sprintf(format, args...), nil, nil) }
func (l *Logger) FatalCtx(ctx context.Context, message string, err error)          { l.log(FATAL, TraceIDFromContext(ctx), message, err, nil) }
