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

type ComponentConfig struct {
	Protocol string `yaml:"protocol"`
	Host     string `yaml:"host"`
	Port     int    `yaml:"port"`
	Debug    bool   `yaml:"debug"`
}

type CLIConfig struct {
	Debug bool `yaml:"debug"`
}

type OpenSearchConfig struct {
	URL       string `yaml:"url"`
	IndexName string `yaml:"index_name"`
	Debug     bool   `yaml:"debug"`
}

type MetricsConfig struct {
	Port int `yaml:"port"`
}

type DownloadsConfig struct {
	ArchiveNode ArchiveNodeConfig `yaml:"archive_node"`
	TierNode    TierNodeConfig    `yaml:"tier_node"`
}

/* ---------- ARCHIVE ---------- */

type ArchiveNodeConfig struct {
	ListenPort int    `yaml:"listen_port"`
	ZipRoot    string `yaml:"zip_root"`
	Sqlite     string `yaml:"sqlite"`
}

func (c ArchiveNodeConfig) Validate() error {
	if c.ListenPort == 0 {
		return fmt.Errorf("downloads.archive_node.listen_port is required")
	}
	if c.ZipRoot == "" {
		return fmt.Errorf("downloads.archive_node.zip_root is required")
	}
	if c.Sqlite == "" {
		return fmt.Errorf("downloads.archive_node.sqlite is required")
	}
	return nil
}

func (c ArchiveNodeConfig) ListenAddr() string {
	return fmt.Sprintf(":%d", c.ListenPort)
}

/* ---------- TIER ---------- */

type TierNodeConfig struct {
	ListenPort int    `yaml:"listen_port"`
	RootPath   string `yaml:"root_path"`
	Sqlite     string `yaml:"sqlite"`
	ParentAddr string `yaml:"parent"`
}

func (c TierNodeConfig) Validate() error {
	if c.ListenPort == 0 {
		return fmt.Errorf("downloads.tier_node.listen_port is required")
	}
	if c.RootPath == "" {
		return fmt.Errorf("downloads.tier_node.root_path is required")
	}
	if c.Sqlite == "" {
		return fmt.Errorf("downloads.tier_node.sqlite is required")
	}
	if c.ParentAddr == "" {
		return fmt.Errorf("downloads.tier_node.parent is required")
	}
	return nil
}

func (c TierNodeConfig) ListenAddr() string {
	return fmt.Sprintf(":%d", c.ListenPort)
}

/* ---------- ROOT ---------- */

type Config struct {
	OpenSearch   OpenSearchConfig `yaml:"opensearch"`
	Datamanager  ComponentConfig  `yaml:"datamanager"`
	Orchestrator ComponentConfig  `yaml:"orchestrator"`
	WebAdapter   ComponentConfig  `yaml:"web_adapter"`
	CLI          CLIConfig        `yaml:"cli"`

	DslScala     ComponentConfig `yaml:"dsl_scala"`
	QueryBuilder ComponentConfig `yaml:"query_builder"`

	Metrics MetricsConfig `yaml:"metrics"`

	Downloads DownloadsConfig `yaml:"downloads"`
}

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

func (c ComponentConfig) Address() string {
	return fmt.Sprintf("%s:%d", c.Host, c.Port)
}

func (c ComponentConfig) FullURL() string {
	return fmt.Sprintf("%s://%s:%d", c.Protocol, c.Host, c.Port)
}
