package config

import "testing"

func TestTelegramBotConfigValidateDisabled(t *testing.T) {
	cfg := TelegramBotConfig{}
	if err := cfg.Validate(); err != nil {
		t.Fatalf("disabled config should be valid, got %v", err)
	}
}

func TestTelegramBotConfigValidatePolling(t *testing.T) {
	cfg := TelegramBotConfig{
		Enabled:     true,
		BotUsername: "ebusta_test_bot",
		Mode:        "polling",
		BotToken:    "token",
		GatewayURL:  "http://gateway:8443",
		PageSize:    5,
		Timeouts: TelegramBotTimeoutsConfig{
			ReadTimeoutSec:     5,
			WriteTimeoutSec:    10,
			ShutdownTimeoutSec: 10,
			PollTimeoutSec:     30,
		},
	}
	if err := cfg.Validate(); err != nil {
		t.Fatalf("polling config should be valid, got %v", err)
	}
}

func TestTelegramBotConfigValidateWebhookRequiresFields(t *testing.T) {
	cfg := TelegramBotConfig{
		Enabled:     true,
		BotUsername: "ebusta_test_bot",
		Mode:        "webhook",
		BotToken:    "token",
		GatewayURL:  "http://gateway:8443",
		PageSize:    5,
		Timeouts: TelegramBotTimeoutsConfig{
			ReadTimeoutSec:     5,
			WriteTimeoutSec:    10,
			ShutdownTimeoutSec: 10,
			PollTimeoutSec:     30,
		},
	}
	if err := cfg.Validate(); err == nil {
		t.Fatal("expected webhook config without listen_port/webhook_url to fail")
	}

	cfg.ListenPort = 8091
	cfg.WebhookURL = "https://bot.example.com/hook"
	if err := cfg.Validate(); err != nil {
		t.Fatalf("webhook config should be valid, got %v", err)
	}
}

func TestTelegramBotConfigListenAddrDefaultHost(t *testing.T) {
	cfg := TelegramBotConfig{ListenPort: 8091}
	if got := cfg.ListenAddr(); got != "0.0.0.0:8091" {
		t.Fatalf("unexpected listen addr: %s", got)
	}
}

func TestTelegramBotConfigRequiresBotUsername(t *testing.T) {
	cfg := TelegramBotConfig{
		Enabled:    true,
		Mode:       "polling",
		BotToken:   "token",
		GatewayURL: "http://gateway:8443",
		PageSize:   5,
		Timeouts: TelegramBotTimeoutsConfig{
			ReadTimeoutSec:     5,
			WriteTimeoutSec:    10,
			ShutdownTimeoutSec: 10,
			PollTimeoutSec:     30,
		},
	}
	if err := cfg.Validate(); err == nil {
		t.Fatal("expected missing bot_username to fail")
	}
}
