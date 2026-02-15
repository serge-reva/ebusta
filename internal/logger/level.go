package logger

import "strings"

// Level represents log severity level
type Level int

const (
	// DEBUG level - detailed information for debugging
	DEBUG Level = iota
	// INFO level - general operational information
	INFO
	// WARN level - warning messages for potentially problematic situations
	WARN
	// ERROR level - error messages for failures that don't require immediate termination
	ERROR
	// FATAL level - severe errors that require program termination
	FATAL
)

// String returns the string representation of the level
func (l Level) String() string {
	switch l {
	case DEBUG:
		return "DEBUG"
	case INFO:
		return "INFO"
	case WARN:
		return "WARN"
	case ERROR:
		return "ERROR"
	case FATAL:
		return "FATAL"
	default:
		return "UNKNOWN"
	}
}

// ParseLevel parses a string into a Level
func ParseLevel(s string) Level {
	switch strings.ToUpper(strings.TrimSpace(s)) {
	case "DEBUG":
		return DEBUG
	case "INFO":
		return INFO
	case "WARN", "WARNING":
		return WARN
	case "ERROR":
		return ERROR
	case "FATAL":
		return FATAL
	default:
		return INFO // default to INFO
	}
}

// Short returns a 4-character abbreviation of the level
func (l Level) Short() string {
	switch l {
	case DEBUG:
		return "DEBG"
	case INFO:
		return "INFO"
	case WARN:
		return "WARN"
	case ERROR:
		return "ERRO"
	case FATAL:
		return "FATL"
	default:
		return "UNKN"
	}
}

// ColorCode returns ANSI color code for the level (for terminal output)
func (l Level) ColorCode() string {
	switch l {
	case DEBUG:
		return "\033[36m" // Cyan
	case INFO:
		return "\033[32m" // Green
	case WARN:
		return "\033[33m" // Yellow
	case ERROR:
		return "\033[31m" // Red
	case FATAL:
		return "\033[35m" // Magenta
	default:
		return "\033[0m" // Reset
	}
}

// ResetCode returns ANSI reset code
const ResetCode = "\033[0m"
