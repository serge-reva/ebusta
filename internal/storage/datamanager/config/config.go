package config

import (
	"time"
	"github.com/kelseyhightower/envconfig"
)

type Config struct {
	BindAddr    string        `env:"BIND_ADDR" default:":8082"`
	OSScheme    string        `env:"OS_SCHEME" default:"http"`
	OSHost      string        `env:"OS_HOST" default:"mercury"`
	OSPort      string        `env:"OS_PORT" default:"9200"`
	OSIndex     string        `env:"OS_INDEX" default:"flibusta"`
	ESUser      string        `env:"ES_USER"`
	ESPass      string        `env:"ES_PASS"`
	HTTPTimeout time.Duration `env:"HTTP_TIMEOUT" default:"5s"`
	LogJSON     bool          `env:"LOG_JSON" default:"false"`
	LogLevel    string        `env:"LOG_LEVEL" default:"INFO"`
	LogPath     string        `env:"LOG_PATH"` // Default is empty, logs to stdout
}

func (c *Config) Validate() error {
	if c.BindAddr == "" {
		return ErrInvalid("bind address is required")
	}
	if c.OSHost == "" || c.OSPort == "" || c.OSIndex == "" {
		return ErrInvalid("OS_HOST/OS_PORT/OS_INDEX are required")
	}
	return nil
}

type invalidErr string
func (e invalidErr) Error() string { return string(e) }
func ErrInvalid(msg string) error { return invalidErr(msg) }

func Load() (Config, error) {
	var cfg Config
	if err := envconfig.Process("", &cfg); err != nil {
		return cfg, err
	}
	return cfg, cfg.Validate()
}
