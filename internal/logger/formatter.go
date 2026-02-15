package logger

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"
)

// Formatter is an interface for formatting log entries
type Formatter interface {
	Format(entry *Entry) ([]byte, error)
}

// Entry represents a single log entry
type Entry struct {
	Time      time.Time
	Level     Level
	TraceID   string
	Component string
	Message   string
	Fields    map[string]interface{}
	Error     error
}

// TextFormatter formats log entries as human-readable text
type TextFormatter struct {
	// DisableColors disables ANSI color codes
	DisableColors bool
	// FullTimestamp uses full timestamp format
	FullTimestamp bool
	// TimestampFormat specifies the timestamp format (default: "2006-01-02 15:04:05.000")
	TimestampFormat string
}

// NewTextFormatter creates a new text formatter
func NewTextFormatter() *TextFormatter {
	return &TextFormatter{
		TimestampFormat: "2006-01-02 15:04:05.000",
	}
}

func (f *TextFormatter) Format(entry *Entry) ([]byte, error) {
	var sb strings.Builder

	// Timestamp
	tsFormat := f.TimestampFormat
	if tsFormat == "" {
		tsFormat = "2006-01-02 15:04:05.000"
	}
	sb.WriteString(entry.Time.Format(tsFormat))
	sb.WriteString(" ")

	// Level
	if !f.DisableColors {
		sb.WriteString(entry.Level.ColorCode())
	}
	sb.WriteString("[")
	sb.WriteString(entry.Level.Short())
	sb.WriteString("]")
	if !f.DisableColors {
		sb.WriteString(ResetCode)
	}
	sb.WriteString(" ")

	// TraceID (always included)
	sb.WriteString("[")
	if entry.TraceID != "" {
		sb.WriteString(entry.TraceID)
	} else {
		sb.WriteString("no-trace")
	}
	sb.WriteString("] ")
	sb.WriteString(" ")

	// Component (optional)
	if entry.Component != "" {
		sb.WriteString("[")
		sb.WriteString(entry.Component)
		sb.WriteString("] ")
	}

	// Message
	sb.WriteString(entry.Message)

	// Fields
	if len(entry.Fields) > 0 {
		for k, v := range entry.Fields {
			sb.WriteString(" ")
			sb.WriteString(k)
			sb.WriteString("=")
			sb.WriteString(fmt.Sprintf("%v", v))
		}
	}

	// Error
	if entry.Error != nil {
		sb.WriteString(" error=")
		sb.WriteString(entry.Error.Error())
	}

	sb.WriteString("\n")

	return []byte(sb.String()), nil
}

// JSONFormatter formats log entries as JSON
type JSONFormatter struct {
	// TimestampFormat specifies the timestamp format (default: time.RFC3339Nano)
	TimestampFormat string
}

// NewJSONFormatter creates a new JSON formatter
func NewJSONFormatter() *JSONFormatter {
	return &JSONFormatter{
		TimestampFormat: time.RFC3339Nano,
	}
}

// jsonEntry is the JSON representation of a log entry
type jsonEntry struct {
	Time      string                 `json:"time"`
	Level     string                 `json:"level"`
	TraceID   string                 `json:"trace_id"`
	Component string                 `json:"component,omitempty"`
	Message   string                 `json:"message"`
	Fields    map[string]interface{} `json:"fields,omitempty"`
	Error     string                 `json:"error,omitempty"`
}

func (f *JSONFormatter) Format(entry *Entry) ([]byte, error) {
	tsFormat := f.TimestampFormat
	if tsFormat == "" {
		tsFormat = time.RFC3339Nano
	}

	je := jsonEntry{
		Time:      entry.Time.Format(tsFormat),
		Level:     entry.Level.String(),
		TraceID:   entry.TraceID,
		Component: entry.Component,
		Message:   entry.Message,
		Fields:    entry.Fields,
	}

	if entry.Error != nil {
		je.Error = entry.Error.Error()
	}

	// Ensure trace_id is never empty
	if je.TraceID == "" {
		je.TraceID = "no-trace"
	}

	return json.Marshal(je)
}

// PrettyJSONFormatter formats log entries as indented JSON
type PrettyJSONFormatter struct {
	TimestampFormat string
}

// NewPrettyJSONFormatter creates a new pretty JSON formatter
func NewPrettyJSONFormatter() *PrettyJSONFormatter {
	return &PrettyJSONFormatter{
		TimestampFormat: time.RFC3339Nano,
	}
}

func (f *PrettyJSONFormatter) Format(entry *Entry) ([]byte, error) {
	tsFormat := f.TimestampFormat
	if tsFormat == "" {
		tsFormat = time.RFC3339Nano
	}

	je := jsonEntry{
		Time:      entry.Time.Format(tsFormat),
		Level:     entry.Level.String(),
		TraceID:   entry.TraceID,
		Component: entry.Component,
		Message:   entry.Message,
		Fields:    entry.Fields,
	}

	if entry.Error != nil {
		je.Error = entry.Error.Error()
	}

	if je.TraceID == "" {
		je.TraceID = "no-trace"
	}

	return json.MarshalIndent(je, "", "  ")
}
