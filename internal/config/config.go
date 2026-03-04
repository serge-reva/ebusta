package config

import (
	"fmt"
	"log"
	"net/url"
	"os"
	"strings"
	"sync"

	"ebusta/internal/logger"
	"gopkg.in/yaml.v3"
)

var (
	once     sync.Once
	instance *Config
)

type ComponentConfig struct {
	Protocol      string        `yaml:"protocol"`
	ListenHost    string        `yaml:"listen_host"`
	AdvertiseHost string        `yaml:"advertise_host"`
	Host          string        `yaml:"host"` // legacy fallback
	Port          int           `yaml:"port"`
	Debug         bool          `yaml:"debug"`
	MTLS          GRPCTLSConfig `yaml:"mtls"`
}

type CLIConfig struct {
	Debug bool `yaml:"debug"`
}

type OpenSearchConfig struct {
	URL       string `yaml:"url"`
	IndexName string `yaml:"index_name"`
	Debug     bool   `yaml:"debug"`
}

func (c OpenSearchConfig) Validate() error {
	if strings.TrimSpace(c.URL) == "" {
		return fmt.Errorf("opensearch.url is required")
	}
	parsed, err := url.ParseRequestURI(c.URL)
	if err != nil || parsed.Scheme == "" || parsed.Host == "" {
		return fmt.Errorf("opensearch.url must be a valid URL")
	}
	if strings.TrimSpace(c.IndexName) == "" {
		return fmt.Errorf("opensearch.index_name is required")
	}
	return nil
}

type MetricsConfig struct {
	Port     int                 `yaml:"port"`
	Services MetricsServicePorts `yaml:"services"`
}

func (c MetricsConfig) Validate() error {
	if c.Port != 0 {
		if err := validatePort("metrics.port", c.Port); err != nil {
			return err
		}
	}
	if err := validatePort("metrics.services.gateway", c.Services.Gateway); err != nil {
		return err
	}
	if err := validatePort("metrics.services.orchestrator", c.Services.Orchestrator); err != nil {
		return err
	}
	if err := validatePort("metrics.services.datamanager", c.Services.Datamanager); err != nil {
		return err
	}
	if err := validatePort("metrics.services.downloader", c.Services.Downloader); err != nil {
		return err
	}
	if err := validatePort("metrics.services.archive_node", c.Services.ArchiveNode); err != nil {
		return err
	}
	if err := validatePort("metrics.services.tier_node", c.Services.TierNode); err != nil {
		return err
	}
	if err := validatePort("metrics.services.plasma_node", c.Services.PlasmaNode); err != nil {
		return err
	}
	if err := validatePort("metrics.services.auth_manager", c.Services.AuthManager); err != nil {
		return err
	}
	if err := validatePort("metrics.services.irc_adapter", c.Services.IRCAdapter); err != nil {
		return err
	}
	if err := validatePort("metrics.services.telegram_adapter", c.Services.Telegram); err != nil {
		return err
	}
	if err := validatePort("metrics.services.web_frontend", c.Services.WebFrontend); err != nil {
		return err
	}
	if err := validatePort("metrics.services.dsl_scala", c.Services.DslScala); err != nil {
		return err
	}
	if err := validatePort("metrics.services.query_builder", c.Services.QueryBuilder); err != nil {
		return err
	}
	return nil
}

type MetricsServicePorts struct {
	Gateway      int `yaml:"gateway"`
	Orchestrator int `yaml:"orchestrator"`
	Datamanager  int `yaml:"datamanager"`
	Downloader   int `yaml:"downloader"`
	ArchiveNode  int `yaml:"archive_node"`
	TierNode     int `yaml:"tier_node"`
	PlasmaNode   int `yaml:"plasma_node"`
	AuthManager  int `yaml:"auth_manager"`
	IRCAdapter   int `yaml:"irc_adapter"`
	Telegram     int `yaml:"telegram_adapter"`
	WebFrontend  int `yaml:"web_frontend"`
	DslScala     int `yaml:"dsl_scala"`
	QueryBuilder int `yaml:"query_builder"`
}

type EdgeActionConfig struct {
	PerMinute int `yaml:"per_minute"`
	Burst     int `yaml:"burst"`
}

type EdgePolicyConfig struct {
	MaxLineLength int                         `yaml:"max_line_length"`
	MaxBodyBytes  int64                       `yaml:"max_body_bytes"`
	MaxJSONDepth  int                         `yaml:"max_json_depth"`
	Actions       map[string]EdgeActionConfig `yaml:"actions"`
}

type EdgeConfig struct {
	ReloadMode string                      `yaml:"reload_mode"`
	Default    EdgePolicyConfig            `yaml:"default"`
	Sources    map[string]EdgePolicyConfig `yaml:"sources"`
}

type IRCAdapterConfig struct {
	ServerHost string   `yaml:"server_host"`
	ServerPort int      `yaml:"server_port"`
	Mode       string   `yaml:"mode"`
	Nick       string   `yaml:"nick"`
	User       string   `yaml:"user"`
	RealName   string   `yaml:"real_name"`
	Channels   []string `yaml:"channels"`
	GatewayURL string   `yaml:"gateway_url"`
	PageSize   int      `yaml:"page_size"`
	Debug      bool     `yaml:"debug"`

	BotServerHost       string   `yaml:"bot_server_host"`
	BotServerPort       int      `yaml:"bot_server_port"`
	BotUseTLS           bool     `yaml:"bot_use_tls"`
	BotPassword         string   `yaml:"bot_password"`
	BotNick             string   `yaml:"bot_nick"`
	BotUser             string   `yaml:"bot_user"`
	BotRealName         string   `yaml:"bot_real_name"`
	BotChannels         []string `yaml:"bot_channels"`
	BotReconnectSeconds int      `yaml:"bot_reconnect_seconds"`

	DCCEnabled    bool   `yaml:"dcc_enabled"`
	DCCPublicIP   string `yaml:"dcc_public_ip"`
	DCCPortMin    int    `yaml:"dcc_port_min"`
	DCCPortMax    int    `yaml:"dcc_port_max"`
	DCCTimeoutSec int    `yaml:"dcc_timeout_sec"`
}

func (c IRCAdapterConfig) Validate() error {
	mode := strings.ToLower(strings.TrimSpace(c.Mode))
	if mode != "server" && mode != "bot" {
		return fmt.Errorf("irc_adapter.mode must be either 'server' or 'bot'")
	}
	if strings.TrimSpace(c.ServerHost) == "" {
		return fmt.Errorf("irc_adapter.server_host is required")
	}
	if err := validatePort("irc_adapter.server_port", c.ServerPort); err != nil {
		return err
	}
	if strings.TrimSpace(c.GatewayURL) == "" {
		return fmt.Errorf("irc_adapter.gateway_url is required")
	}
	if err := validateURL("irc_adapter.gateway_url", c.GatewayURL); err != nil {
		return err
	}
	if c.PageSize <= 0 {
		return fmt.Errorf("irc_adapter.page_size must be > 0")
	}
	if mode == "bot" {
		if strings.TrimSpace(c.BotServerHost) == "" {
			return fmt.Errorf("irc_adapter.bot_server_host is required in bot mode")
		}
		if err := validatePort("irc_adapter.bot_server_port", c.BotServerPort); err != nil {
			return err
		}
	}
	return nil
}

func (c IRCAdapterConfig) Address() string {
	return fmt.Sprintf("%s:%d", c.ServerHost, c.ServerPort)
}

func (c IRCAdapterConfig) ServerAddress() string {
	return fmt.Sprintf("%s:%d", c.ServerHost, c.ServerPort)
}

func (c IRCAdapterConfig) BotAddress() string {
	return fmt.Sprintf("%s:%d", c.BotServerHost, c.BotServerPort)
}

type TelegramAdapterConfig struct {
	Host       string `yaml:"host"`
	Port       int    `yaml:"port"`
	GatewayURL string `yaml:"gateway_url"`
	PageSize   int    `yaml:"page_size"`
	Debug      bool   `yaml:"debug"`
}

func (c TelegramAdapterConfig) Validate() error {
	if strings.TrimSpace(c.Host) == "" {
		return fmt.Errorf("telegram_adapter.host is required")
	}
	if err := validatePort("telegram_adapter.port", c.Port); err != nil {
		return err
	}
	if strings.TrimSpace(c.GatewayURL) == "" {
		return fmt.Errorf("telegram_adapter.gateway_url is required")
	}
	if err := validateURL("telegram_adapter.gateway_url", c.GatewayURL); err != nil {
		return err
	}
	if c.PageSize <= 0 {
		return fmt.Errorf("telegram_adapter.page_size must be > 0")
	}
	return nil
}

func (c TelegramAdapterConfig) Address() string {
	return fmt.Sprintf("%s:%d", c.Host, c.Port)
}

/* ---------- WEB FRONTEND ---------- */

type WebFrontendConfig struct {
	ListenHost    string `yaml:"listen_host"`
	AdvertiseHost string `yaml:"advertise_host"`
	Port          int    `yaml:"port"`
	PageSize      int    `yaml:"page_size"`
	GatewayURL    string `yaml:"gateway_url"`
	Debug         bool   `yaml:"debug"`
}

func (c WebFrontendConfig) Validate() error {
	if err := validatePort("web_frontend.port", c.Port); err != nil {
		return err
	}
	if c.PageSize <= 0 {
		return fmt.Errorf("web_frontend.page_size must be > 0")
	}
	if strings.TrimSpace(c.GatewayURL) == "" {
		return fmt.Errorf("web_frontend.gateway_url is required")
	}
	if err := validateURL("web_frontend.gateway_url", c.GatewayURL); err != nil {
		return err
	}
	return nil
}

func (c WebFrontendConfig) ListenAddr() string {
	return buildAddr(normalizeListenHost(c.ListenHost), c.Port)
}

/* ---------- DOWNLOADS ROOT ---------- */

type DownloadsConfig struct {
	ArchiveNode ArchiveNodeConfig  `yaml:"archive_node"`
	TierNode    TierNodeConfig     `yaml:"tier_node"`
	PlasmaNode  PlasmaNodeConfig   `yaml:"plasma_node"`
	CLI         DownloadsCLIConfig `yaml:"cli"`
	Downloader  DownloaderConfig   `yaml:"downloader"`
}

/* ---------- CLI (downloads) ---------- */

type DownloadsCLIConfig struct {
	DownloadDir string `yaml:"download_dir"`
}

/* ---------- DOWNLOADER (HTTP) ---------- */

type DownloaderConfig struct {
	ListenHost    string        `yaml:"listen_host"`
	AdvertiseHost string        `yaml:"advertise_host"`
	ListenPort    int           `yaml:"listen_port"`
	PlasmaAddr    string        `yaml:"plasma"`
	MTLS          GRPCTLSConfig `yaml:"mtls"`
}

func (c DownloaderConfig) Validate() error {
	if err := validatePort("downloads.downloader.listen_port", c.ListenPort); err != nil {
		return err
	}
	if c.PlasmaAddr == "" {
		return fmt.Errorf("downloads.downloader.plasma is required")
	}
	if err := c.MTLS.Validate("downloads.downloader.mtls"); err != nil {
		return err
	}
	return nil
}

func (c DownloaderConfig) ListenAddr() string {
	return buildAddr(normalizeListenHost(c.ListenHost), c.ListenPort)
}

func (c DownloaderConfig) AdvertiseAddr() string {
	host := strings.TrimSpace(c.AdvertiseHost)
	if host == "" {
		host = "localhost"
	}
	return buildAddr(host, c.ListenPort)
}

/* ---------- ARCHIVE ---------- */

type ArchiveNodeConfig struct {
	ListenHost    string        `yaml:"listen_host"`
	AdvertiseHost string        `yaml:"advertise_host"`
	ListenPort    int           `yaml:"listen_port"`
	ZipRoot       string        `yaml:"zip_root"`
	Sqlite        string        `yaml:"sqlite"`
	MTLS          GRPCTLSConfig `yaml:"mtls"`
}

func (c ArchiveNodeConfig) Validate() error {
	if err := validatePort("downloads.archive_node.listen_port", c.ListenPort); err != nil {
		return err
	}
	if c.ZipRoot == "" {
		return fmt.Errorf("downloads.archive_node.zip_root is required")
	}
	if c.Sqlite == "" {
		return fmt.Errorf("downloads.archive_node.sqlite is required")
	}
	if err := c.MTLS.Validate("downloads.archive_node.mtls"); err != nil {
		return err
	}
	return nil
}

func (c ArchiveNodeConfig) ListenAddr() string {
	return buildAddr(normalizeListenHost(c.ListenHost), c.ListenPort)
}

func (c ArchiveNodeConfig) AdvertiseAddr() string {
	host := strings.TrimSpace(c.AdvertiseHost)
	if host == "" {
		host = "localhost"
	}
	return buildAddr(host, c.ListenPort)
}

/* ---------- TIER ---------- */

type TierNodeConfig struct {
	ListenHost    string        `yaml:"listen_host"`
	AdvertiseHost string        `yaml:"advertise_host"`
	ListenPort    int           `yaml:"listen_port"`
	RootPath      string        `yaml:"root_path"`
	Sqlite        string        `yaml:"sqlite"`
	ParentAddr    string        `yaml:"parent"`
	MTLS          GRPCTLSConfig `yaml:"mtls"`
}

func (c TierNodeConfig) Validate() error {
	if err := validatePort("downloads.tier_node.listen_port", c.ListenPort); err != nil {
		return err
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
	if err := c.MTLS.Validate("downloads.tier_node.mtls"); err != nil {
		return err
	}
	return nil
}

func (c TierNodeConfig) ListenAddr() string {
	return buildAddr(normalizeListenHost(c.ListenHost), c.ListenPort)
}

func (c TierNodeConfig) AdvertiseAddr() string {
	host := strings.TrimSpace(c.AdvertiseHost)
	if host == "" {
		host = "localhost"
	}
	return buildAddr(host, c.ListenPort)
}

/* ---------- PLASMA ---------- */

type PlasmaNodeConfig struct {
	ListenHost    string        `yaml:"listen_host"`
	AdvertiseHost string        `yaml:"advertise_host"`
	ListenPort    int           `yaml:"listen_port"`
	ParentAddr    string        `yaml:"parent"`
	MaxBytes      int64         `yaml:"max_bytes"`
	MaxItems      int           `yaml:"max_items"`
	DebugAddr     string        `yaml:"debug"`
	MTLS          GRPCTLSConfig `yaml:"mtls"`
}

func (c PlasmaNodeConfig) Validate() error {
	if err := validatePort("downloads.plasma_node.listen_port", c.ListenPort); err != nil {
		return err
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
	if err := c.MTLS.Validate("downloads.plasma_node.mtls"); err != nil {
		return err
	}
	return nil
}

func (c PlasmaNodeConfig) ListenAddr() string {
	return buildAddr(normalizeListenHost(c.ListenHost), c.ListenPort)
}

func (c PlasmaNodeConfig) AdvertiseAddr() string {
	host := strings.TrimSpace(c.AdvertiseHost)
	if host == "" {
		host = "localhost"
	}
	return buildAddr(host, c.ListenPort)
}

/* ---------- GATEWAY ---------- */

type GatewayConfig struct {
	ListenHost    string                `yaml:"listen_host"`
	AdvertiseHost string                `yaml:"advertise_host"`
	Port          int                   `yaml:"port"`
	TLSCert       string                `yaml:"tls_cert"`
	TLSKey        string                `yaml:"tls_key"`
	RateLimit     RateLimitConfig       `yaml:"rate_limit"`
	Mapper        MapperConfig          `yaml:"mapper"`
	MTLS          MTLSConfig            `yaml:"mtls"`
	Validation    ValidationConfig      `yaml:"validation"`
	CORS          CORSConfig            `yaml:"cors"`
	Services      GatewayServicesConfig `yaml:"services"`
}

func (c GatewayConfig) Validate() error {
	if err := validatePort("gateway.port", c.Port); err != nil {
		return err
	}
	if (strings.TrimSpace(c.TLSCert) == "") != (strings.TrimSpace(c.TLSKey) == "") {
		return fmt.Errorf("gateway.tls_cert and gateway.tls_key must be set together")
	}
	if c.Mapper.TTL <= 0 {
		return fmt.Errorf("gateway.mapper.ttl must be > 0")
	}
	if c.Mapper.MaxTokens <= 0 {
		return fmt.Errorf("gateway.mapper.max_tokens must be > 0")
	}
	if c.Mapper.CleanupInterval <= 0 {
		return fmt.Errorf("gateway.mapper.cleanup_interval must be > 0")
	}
	if c.Validation.MaxBodyBytes <= 0 {
		return fmt.Errorf("gateway.validation.max_body_bytes must be > 0")
	}
	if c.Validation.MaxFileBytes <= 0 {
		return fmt.Errorf("gateway.validation.max_file_bytes must be > 0")
	}
	if c.Validation.MaxQueryLength <= 0 {
		return fmt.Errorf("gateway.validation.max_query_length must be > 0")
	}
	if c.Validation.MaxArrayItems <= 0 {
		return fmt.Errorf("gateway.validation.max_array_items must be > 0")
	}
	if c.Validation.MaxJSONDepth <= 0 {
		return fmt.Errorf("gateway.validation.max_json_depth must be > 0")
	}
	if err := c.Services.Validate(); err != nil {
		return err
	}
	if c.MTLS.Enabled {
		if strings.TrimSpace(c.MTLS.CAFile) == "" || strings.TrimSpace(c.MTLS.CertFile) == "" || strings.TrimSpace(c.MTLS.KeyFile) == "" {
			return fmt.Errorf("gateway.mtls requires ca_file, cert_file and key_file")
		}
	}
	return nil
}

type RateLimitConfig struct {
	IP            int `yaml:"ip"`
	Authenticated int `yaml:"authenticated"`
	Download      int `yaml:"download"`
	Resolve       int `yaml:"resolve"`
}

type MapperConfig struct {
	TTL             int `yaml:"ttl"`
	MaxTokens       int `yaml:"max_tokens"`
	CleanupInterval int `yaml:"cleanup_interval"`
}

type MTLSConfig struct {
	Enabled    bool   `yaml:"enabled"`
	CAFile     string `yaml:"ca_file"`
	CertFile   string `yaml:"cert_file"`
	KeyFile    string `yaml:"key_file"`
	ServerName string `yaml:"server_name"`
}

type ValidationConfig struct {
	MaxBodyBytes   int64 `yaml:"max_body_bytes"`
	MaxFileBytes   int64 `yaml:"max_file_bytes"`
	MaxQueryLength int   `yaml:"max_query_length"`
	MaxArrayItems  int   `yaml:"max_array_items"`
	MaxJSONDepth   int   `yaml:"max_json_depth"`
}

type CORSConfig struct {
	AllowedOrigins   []string `yaml:"allowed_origins"`
	AllowedMethods   []string `yaml:"allowed_methods"`
	AllowedHeaders   []string `yaml:"allowed_headers"`
	AllowCredentials bool     `yaml:"allow_credentials"`
	MaxAge           int      `yaml:"max_age"`
}

type GatewayServicesConfig struct {
	Orchestrator string `yaml:"orchestrator"`
	Downloader   string `yaml:"downloader"`
	Auth         string `yaml:"auth"`
}

func (c GatewayServicesConfig) Validate() error {
	if strings.TrimSpace(c.Orchestrator) == "" {
		return fmt.Errorf("gateway.services.orchestrator is required")
	}
	if strings.TrimSpace(c.Downloader) == "" {
		return fmt.Errorf("gateway.services.downloader is required")
	}
	if strings.TrimSpace(c.Auth) == "" {
		return fmt.Errorf("gateway.services.auth is required")
	}
	return nil
}

/* ---------- ROOT ---------- */

type Config struct {
	OpenSearch      OpenSearchConfig      `yaml:"opensearch"`
	Datamanager     ComponentConfig       `yaml:"datamanager"`
	Orchestrator    ComponentConfig       `yaml:"orchestrator"`
	AuthManager     ComponentConfig       `yaml:"auth_manager"`
	WebAdapter      ComponentConfig       `yaml:"web_adapter"`
	IRCAdapter      IRCAdapterConfig      `yaml:"irc_adapter"`
	TelegramAdapter TelegramAdapterConfig `yaml:"telegram_adapter"`
	CLI             CLIConfig             `yaml:"cli"`

	DslScala     ComponentConfig `yaml:"dsl_scala"`
	QueryBuilder ComponentConfig `yaml:"query_builder"`

	WebFrontend WebFrontendConfig `yaml:"web_frontend"`

	Metrics MetricsConfig `yaml:"metrics"`

	Downloads DownloadsConfig `yaml:"downloads"`

	Gateway GatewayConfig `yaml:"gateway"`
	Edge    EdgeConfig    `yaml:"edge"`

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

func (c ComponentConfig) listenHost() string {
	return normalizeListenHost(c.ListenHost)
}

func (c ComponentConfig) advertiseHost() string {
	return normalizeAdvertiseHost(c.AdvertiseHost, c.Host)
}

func (c ComponentConfig) ListenAddress() string {
	return buildAddr(c.listenHost(), c.Port)
}

func (c ComponentConfig) DialAddress() string {
	return buildAddr(c.advertiseHost(), c.Port)
}

func (c ComponentConfig) Address() string {
	return c.DialAddress()
}

func (c ComponentConfig) Validate() error {
	if strings.TrimSpace(c.Protocol) == "" {
		return fmt.Errorf("component protocol is required")
	}
	if strings.TrimSpace(c.advertiseHost()) == "" {
		return fmt.Errorf("component advertise_host (or legacy host) is required")
	}
	if err := validatePort("component.port", c.Port); err != nil {
		return err
	}
	if err := c.MTLS.Validate("component.mtls"); err != nil {
		return err
	}
	return nil
}

func (c ComponentConfig) FullURL() string {
	return fmt.Sprintf("%s://%s:%d", c.Protocol, c.advertiseHost(), c.Port)
}

func (c *Config) EdgePolicyForSource(source string) EdgePolicyConfig {
	p := c.Edge.Default
	actions := make(map[string]EdgeActionConfig, len(p.Actions))
	for k, v := range p.Actions {
		actions[k] = v
	}
	p.Actions = actions
	if c.Edge.Sources == nil {
		return p
	}
	if src, ok := c.Edge.Sources[source]; ok {
		if src.MaxLineLength > 0 {
			p.MaxLineLength = src.MaxLineLength
		}
		if src.MaxBodyBytes > 0 {
			p.MaxBodyBytes = src.MaxBodyBytes
		}
		if src.MaxJSONDepth > 0 {
			p.MaxJSONDepth = src.MaxJSONDepth
		}
		if src.Actions != nil {
			for k, v := range src.Actions {
				p.Actions[k] = v
			}
		}
	}
	return p
}

func validatePort(field string, port int) error {
	if port <= 0 || port > 65535 {
		return fmt.Errorf("%s must be in range 1..65535", field)
	}
	return nil
}

func buildAddr(host string, port int) string {
	return fmt.Sprintf("%s:%d", host, port)
}

func normalizeListenHost(host string) string {
	host = strings.TrimSpace(host)
	if host == "" {
		return "0.0.0.0"
	}
	return host
}

func normalizeAdvertiseHost(advertiseHost, legacyHost string) string {
	host := strings.TrimSpace(advertiseHost)
	if host != "" {
		return host
	}
	return strings.TrimSpace(legacyHost)
}

func validateURL(field, raw string) error {
	parsed, err := url.ParseRequestURI(raw)
	if err != nil || parsed.Scheme == "" || parsed.Host == "" {
		return fmt.Errorf("%s must be a valid URL", field)
	}
	return nil
}
