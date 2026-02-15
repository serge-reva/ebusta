package logger

import (
	"strings"
)

// LoggerConfig represents logger configuration section
// This should be added to internal/config/config.go as:
//
//	type Config struct {
//		...
//		Logger LoggerConfig `yaml:"logger"`
//	}
type LoggerConfig struct {
	// Level: DEBUG, INFO, WARN, ERROR, FATAL (default: INFO)
	Level string `yaml:"level"`
	// Format: text, json (default: text)
	Format string `yaml:"format"`
	// Outputs configuration
	Outputs OutputsConfig `yaml:"outputs"`
	// DisableColors disables ANSI color codes (default: false)
	DisableColors bool `yaml:"disable_colors"`
}

// OutputsConfig holds configuration for multiple outputs
type OutputsConfig struct {
	// Console output
	Console ConsoleConfig `yaml:"console"`
	// File outputs
	File FileOutputConfig `yaml:"file"`
}

// ConsoleConfig holds console output configuration
type ConsoleConfig struct {
	// Enabled enables console output (default: true)
	Enabled bool `yaml:"enabled"`
	// UseStderr uses stderr instead of stdout
	UseStderr bool `yaml:"use_stderr"`
}

// FileOutputConfig holds file output configuration
type FileOutputConfig struct {
	// Enabled enables file output (default: false)
	Enabled bool `yaml:"enabled"`
	// Path is the log file path
	Path string `yaml:"path"`
	// Append appends to existing file (default: false)
	Append bool `yaml:"append"`
}

// NewFromConfig creates a new Logger from LoggerConfig
// This is the main integration point with internal/config
func NewFromConfig(cfg LoggerConfig, component string) *Logger {
	// Parse level
	level := ParseLevel(cfg.Level)

	// Create formatter
	var formatter Formatter
	switch strings.ToLower(cfg.Format) {
	case "json":
		formatter = NewJSONFormatter()
	default:
		tf := NewTextFormatter()
		tf.DisableColors = cfg.DisableColors
		formatter = tf
	}

	// Create output
	output := createOutput(cfg.Outputs)

	return New(level, formatter, output, component)
}

// createOutput creates Output from OutputsConfig
func createOutput(cfg OutputsConfig) Output {
	outputs := make([]Output, 0)

	// Console output (enabled by default if not explicitly disabled)
	if cfg.Console.Enabled || (cfg.Console.Enabled == false && cfg.File.Enabled == false && cfg.File.Path == "") {
		if cfg.Console.UseStderr {
			outputs = append(outputs, &StderrOutput{})
		} else {
			outputs = append(outputs, &StdoutOutput{})
		}
	}

	// File output
	if cfg.File.Enabled && cfg.File.Path != "" {
		fo, err := NewFileOutput(cfg.File.Path, cfg.File.Append)
		if err == nil {
			outputs = append(outputs, fo)
		}
	}

	if len(outputs) == 0 {
		return &StdoutOutput{}
	}

	if len(outputs) == 1 {
		return outputs[0]
	}

	return NewMultiOutput(outputs...)
}

// DefaultConfig returns default LoggerConfig
func DefaultConfig() LoggerConfig {
	return LoggerConfig{
		Level:         "INFO",
		Format:        "text",
		DisableColors: false,
		Outputs: OutputsConfig{
			Console: ConsoleConfig{
				Enabled: true,
			},
		},
	}
}

// MergeWithDefault merges config with defaults
func (cfg LoggerConfig) MergeWithDefault() LoggerConfig {
	defaults := DefaultConfig()

	if cfg.Level == "" {
		cfg.Level = defaults.Level
	}
	if cfg.Format == "" {
		cfg.Format = defaults.Format
	}

	return cfg
}
