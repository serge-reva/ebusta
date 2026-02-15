package logger

import (
	"os"
	"strings"
)

// ConfigFromEnv creates a Config from environment variables
// Environment variables:
//   - LOG_LEVEL: DEBUG, INFO, WARN, ERROR, FATAL (default: INFO)
//   - LOG_FORMAT: text, json (default: text)
//   - LOG_OUTPUT: stdout, stderr, file:/path/to/file (default: stdout)
//   - LOG_FILE_APPEND: true/false (default: false)
//   - LOG_COLORS: true/false (default: true)
//   - LOG_COMPONENT: component name (default: empty)
func ConfigFromEnv() Config {
	cfg := Config{
		Level:     INFO,
		Formatter: NewTextFormatter(),
		Output:    &StdoutOutput{},
	}

	// Level
	if level := os.Getenv("LOG_LEVEL"); level != "" {
		cfg.Level = ParseLevel(level)
	}

	// Format
	format := os.Getenv("LOG_FORMAT")
	switch strings.ToLower(format) {
	case "json":
		cfg.Formatter = NewJSONFormatter()
	case "prettyjson", "pretty-json":
		cfg.Formatter = NewPrettyJSONFormatter()
	case "text", "":
		tf := NewTextFormatter()
		// Colors
		if colors := os.Getenv("LOG_COLORS"); colors == "false" || colors == "0" {
			tf.DisableColors = true
		}
		cfg.Formatter = tf
	}

	// Output
	output := os.Getenv("LOG_OUTPUT")
	switch {
	case output == "" || output == "stdout":
		cfg.Output = &StdoutOutput{}
	case output == "stderr":
		cfg.Output = &StderrOutput{}
	case strings.HasPrefix(output, "file:"):
		path := strings.TrimPrefix(output, "file:")
		append := os.Getenv("LOG_FILE_APPEND") == "true"
		fo, err := NewFileOutput(path, append)
		if err == nil {
			cfg.Output = fo
		}
	}

	// Component
	if component := os.Getenv("LOG_COMPONENT"); component != "" {
		cfg.Component = component
	}

	return cfg
}

// NewFromEnv creates a new Logger from environment variables
func NewFromEnv() *Logger {
	return New(ConfigFromEnv())
}

// ConfigFromFile creates a Config from a YAML file (requires yaml parsing)
// This is a helper for integration with the existing config system
type ConfigFile struct {
	Level      string            `yaml:"level"`
	Format     string            `yaml:"format"`
	Output     string            `yaml:"output"`
	FileAppend bool              `yaml:"file_append"`
	Colors     bool              `yaml:"colors"`
	Component  string            `yaml:"component"`
	Fields     map[string]string `yaml:"fields"`
}

// ConfigFromConfigFile creates a Config from ConfigFile struct
func ConfigFromConfigFile(cf ConfigFile) Config {
	cfg := Config{
		Level:     ParseLevel(cf.Level),
		Formatter: NewTextFormatter(),
		Output:    &StdoutOutput{},
		Component: cf.Component,
		Fields:    make(map[string]interface{}),
	}

	// Format
	switch strings.ToLower(cf.Format) {
	case "json":
		cfg.Formatter = NewJSONFormatter()
	case "prettyjson", "pretty-json":
		cfg.Formatter = NewPrettyJSONFormatter()
	case "text", "":
		tf := NewTextFormatter()
		if !cf.Colors {
			tf.DisableColors = true
		}
		cfg.Formatter = tf
	}

	// Output
	switch {
	case cf.Output == "" || cf.Output == "stdout":
		cfg.Output = &StdoutOutput{}
	case cf.Output == "stderr":
		cfg.Output = &StderrOutput{}
	case strings.HasPrefix(cf.Output, "file:"):
		path := strings.TrimPrefix(cf.Output, "file:")
		fo, err := NewFileOutput(path, cf.FileAppend)
		if err == nil {
			cfg.Output = fo
		}
	}

	// Convert string fields to interface{}
	for k, v := range cf.Fields {
		cfg.Fields[k] = v
	}

	return cfg
}

// MultiOutputConfig holds configuration for multiple outputs
type MultiOutputConfig struct {
	Console struct {
		Enabled  bool   `yaml:"enabled"`
		UseStderr bool  `yaml:"use_stderr"`
	} `yaml:"console"`
	File struct {
		Enabled bool   `yaml:"enabled"`
		Path    string `yaml:"path"`
		Append  bool   `yaml:"append"`
	} `yaml:"file"`
}

// NewMultiOutputFromConfig creates a MultiOutput from configuration
func NewMultiOutputFromConfig(cfg MultiOutputConfig) (Output, error) {
	outputs := make([]Output, 0)

	// Console output
	if cfg.Console.Enabled {
		if cfg.Console.UseStderr {
			outputs = append(outputs, &StderrOutput{})
		} else {
			outputs = append(outputs, &StdoutOutput{})
		}
	}

	// File output
	if cfg.File.Enabled && cfg.File.Path != "" {
		fo, err := NewFileOutput(cfg.File.Path, cfg.File.Append)
		if err != nil {
			return nil, err
		}
		outputs = append(outputs, fo)
	}

	if len(outputs) == 0 {
		return &StdoutOutput{}, nil
	}

	if len(outputs) == 1 {
		return outputs[0], nil
	}

	return NewMultiOutput(outputs...), nil
}
