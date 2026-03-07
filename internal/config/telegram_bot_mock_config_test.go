package config

import "testing"

func TestTelegramBotConfigValidateMockModeWithoutToken(t *testing.T) {
	cfg := TelegramBotConfig{
		Enabled:    true,
		MockMode:   true,
		Mode:       "webhook",
		GatewayURL: "http://gateway:8443",
		PageSize:   5,
		ListenPort: 8088,
		WebhookURL: "http://localhost:8088/webhook",
		Timeouts: TelegramBotTimeoutsConfig{
			ReadTimeoutSec:     5,
			WriteTimeoutSec:    10,
			ShutdownTimeoutSec: 10,
			PollTimeoutSec:     30,
		},
	}
	if err := cfg.Validate(); err != nil {
		t.Fatalf("mock_mode config should be valid without token, got %v", err)
	}
}
