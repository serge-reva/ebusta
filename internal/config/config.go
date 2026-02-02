package config

import (
	"fmt"
	"log"
	"os"
	"sync"

	"gopkg.in/yaml.v3"
)

var (
	once     sync.Once
	instance *Config
)

// ComponentConfig содержит базовые сетевые настройки для запуска сервиса
type ComponentConfig struct {
	Protocol string `yaml:"protocol"`
	Host     string `yaml:"host"`
	Port     int    `yaml:"port"`
	Debug    bool   `yaml:"debug"`
}

// CLIConfig настройки для CLI (не сервис)
type CLIConfig struct {
	Debug bool `yaml:"debug"`
}

// OpenSearchConfig настройки поискового движка
type OpenSearchConfig struct {
	URL       string `yaml:"url"`
	IndexName string `yaml:"index_name"`
	Debug     bool   `yaml:"debug"`
}

// MetricsConfig настройки для экспортера метрик
type MetricsConfig struct {
	Port int `yaml:"port"`
}

// Config корень дерева конфигурации, строго соответствующий ebusta.yaml
type Config struct {
	OpenSearch   OpenSearchConfig `yaml:"opensearch"`
	Datamanager  ComponentConfig  `yaml:"datamanager"`
	Orchestrator ComponentConfig  `yaml:"orchestrator"`
	WebAdapter   ComponentConfig  `yaml:"web_adapter"`
	CLI          CLIConfig        `yaml:"cli"`
	
	// Новые сервисы (согласно ebusta.yaml)
	DslScala     ComponentConfig  `yaml:"dsl_scala"`
	QueryBuilder ComponentConfig  `yaml:"query_builder"`
	
	Metrics      MetricsConfig    `yaml:"metrics"`
}

// Get возвращает инициализированный объект конфигурации (Singleton)
func Get() *Config {
	once.Do(func() {
		path := os.Getenv("EBUSTA_CONFIG")
		if path == "" {
			path = "ebusta.yaml"
		}

		f, err := os.ReadFile(path)
		if err != nil {
			log.Fatalf("[CONFIG ERROR] Could not read %s: %v", path, err)
		}

		instance = &Config{}
		if err := yaml.Unmarshal(f, instance); err != nil {
			log.Fatalf("[CONFIG ERROR] Failed to parse YAML: %v", err)
		}
	})
	return instance
}

// Address возвращает строку host:port (удобно для gRPC)
func (c ComponentConfig) Address() string {
	return fmt.Sprintf("%s:%d", c.Host, c.Port)
}

// FullURL возвращает строку protocol://host:port (удобно для HTTP/URL)
func (c ComponentConfig) FullURL() string {
	return fmt.Sprintf("%s://%s:%d", c.Protocol, c.Host, c.Port)
}
