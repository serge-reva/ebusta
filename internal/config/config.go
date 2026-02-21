package config

import (
        "fmt"
        "log"
        "os"
        "sync"

        "ebusta/internal/logger"
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

/* ---------- WEB FRONTEND ---------- */

type WebFrontendConfig struct {
        Port           int    `yaml:"port"`
        PageSize       int    `yaml:"page_size"`
        DownloaderAddr string `yaml:"downloader_addr"`
        Debug          bool   `yaml:"debug"`
}

func (c WebFrontendConfig) ListenAddr() string {
        return fmt.Sprintf(":%d", c.Port)
}

/* ---------- DOWNLOADS ROOT ---------- */

type DownloadsConfig struct {
        ArchiveNode ArchiveNodeConfig   `yaml:"archive_node"`
        TierNode    TierNodeConfig      `yaml:"tier_node"`
        PlasmaNode  PlasmaNodeConfig    `yaml:"plasma_node"`
        CLI         DownloadsCLIConfig  `yaml:"cli"`
        Downloader  DownloaderConfig    `yaml:"downloader"`
}

/* ---------- CLI (downloads) ---------- */

type DownloadsCLIConfig struct {
        DownloadDir string `yaml:"download_dir"`
}

/* ---------- DOWNLOADER (HTTP) ---------- */

type DownloaderConfig struct {
        ListenPort int    `yaml:"listen_port"`
        PlasmaAddr string `yaml:"plasma"`
}

func (c DownloaderConfig) Validate() error {
        if c.ListenPort == 0 {
                return fmt.Errorf("downloads.downloader.listen_port is required")
        }
        if c.PlasmaAddr == "" {
                return fmt.Errorf("downloads.downloader.plasma is required")
        }
        return nil
}

func (c DownloaderConfig) ListenAddr() string {
        return fmt.Sprintf(":%d", c.ListenPort)
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

/* ---------- PLASMA ---------- */

type PlasmaNodeConfig struct {
        ListenPort int    `yaml:"listen_port"`
        ParentAddr string `yaml:"parent"`
        MaxBytes   int64  `yaml:"max_bytes"`
        MaxItems   int    `yaml:"max_items"`
        DebugAddr  string `yaml:"debug"`
}

func (c PlasmaNodeConfig) Validate() error {
        if c.ListenPort == 0 {
                return fmt.Errorf("downloads.plasma_node.listen_port is required")
        }
        if c.ParentAddr == "" {
                return fmt.Errorf("downloads.plasma_node.parent is required")
        }
        if c.MaxBytes <= 0 {
                return fmt.Errorf("downloads.plasma_node.max_bytes must be > 0")
        }
        if c.MaxItems <= 0 {
                return fmt.Errorf("downloads.plasma_node.max_items must be > 0")
        }
        return nil
}

func (c PlasmaNodeConfig) ListenAddr() string {
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

        WebFrontend WebFrontendConfig `yaml:"web_frontend"`

        Metrics MetricsConfig `yaml:"metrics"`

        Downloads DownloadsConfig `yaml:"downloads"`

        Logger logger.LoggerConfig `yaml:"logger"`
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

                logger.InitFromConfig(instance.Logger, "config")
        })
        return instance
}

func (c ComponentConfig) Address() string {
        return fmt.Sprintf("%s:%d", c.Host, c.Port)
}

func (c ComponentConfig) FullURL() string {
        return fmt.Sprintf("%s://%s:%d", c.Protocol, c.Host, c.Port)
}

/* ---------- GATEWAY ---------- */

type GatewayConfig struct {
    Port           int            `yaml:"port"`
    TLSCert        string         `yaml:"tls_cert"`
    TLSKey         string         `yaml:"tls_key"`
    RateLimit      RateLimitConfig `yaml:"rate_limit"`
    Mapper         MapperConfig    `yaml:"mapper"`
    MTLS           MTLSConfig      `yaml:"mtls"`
    Validation     ValidationConfig `yaml:"validation"`
    CORS           CORSConfig      `yaml:"cors"`
    Services       ServicesConfig  `yaml:"services"`
}

type RateLimitConfig struct {
    IP            int `yaml:"ip"`              // requests per minute
    Authenticated int `yaml:"authenticated"`   // requests per minute
    Download      int `yaml:"download"`         // downloads per minute
    Resolve       int `yaml:"resolve"`          // token resolves per minute
}

type MapperConfig struct {
    TTL             int `yaml:"ttl"`              // token TTL in seconds
    MaxTokens       int `yaml:"max_tokens"`       // maximum active tokens
    CleanupInterval int `yaml:"cleanup_interval"` // cleanup interval in seconds
}

type MTLSConfig struct {
    Enabled    bool   `yaml:"enabled"`
    CAFile     string `yaml:"ca_file"`
    CertFile   string `yaml:"cert_file"`
    KeyFile    string `yaml:"key_file"`
    ServerName string `yaml:"server_name"`
}

type ValidationConfig struct {
    MaxBodyBytes    int64 `yaml:"max_body_bytes"`
    MaxFileBytes    int64 `yaml:"max_file_bytes"`
    MaxQueryLength  int   `yaml:"max_query_length"`
    MaxArrayItems   int   `yaml:"max_array_items"`
    MaxJSONDepth    int   `yaml:"max_json_depth"`
}

type CORSConfig struct {
    AllowedOrigins   []string `yaml:"allowed_origins"`
    AllowedMethods   []string `yaml:"allowed_methods"`
    AllowedHeaders   []string `yaml:"allowed_headers"`
    AllowCredentials bool     `yaml:"allow_credentials"`
    MaxAge           int      `yaml:"max_age"`
}

type ServicesConfig struct {
    Orchestrator string `yaml:"orchestrator"` // host:port
    Downloader   string `yaml:"downloader"`   // host:port
    Auth         string `yaml:"auth"`         // host:port
}

func (c *Config) GetGatewayConfig() GatewayConfig {
    return c.Gateway
}
