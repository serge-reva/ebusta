package main

import (
	"testing"

	"ebusta/internal/config"
)

func TestLoadIRCConfigDefaultsAndVerboseOverride(t *testing.T) {
	cfg := &config.Config{
		IRCAdapter: config.IRCAdapterConfig{
			ServerHost:          "127.0.0.1",
			ServerPort:          7000,
			Mode:                "bot",
			Nick:                "n1",
			User:                "u1",
			RealName:            "r1",
			GatewayURL:          "http://gw:8443",
			PageSize:            9,
			Debug:               false,
			BotServerHost:       "irc.example.org",
			BotServerPort:       6697,
			BotUseTLS:           true,
			BotPassword:         "secret",
			BotNick:             "bot1",
			BotUser:             "botuser",
			BotRealName:         "Bot Real",
			BotChannels:         []string{"#books"},
			BotReconnectSeconds: 17,
		},
	}

	irc := loadIRCConfig(cfg, true)
	if irc.ServerHost != "127.0.0.1" || irc.ServerPort != 7000 {
		t.Fatalf("expected configured host/port, got %s:%d", irc.ServerHost, irc.ServerPort)
	}
	if !irc.Debug {
		t.Fatalf("expected verbose flag to force debug=true")
	}
	if irc.PageSize != 9 {
		t.Fatalf("expected page_size from config, got %d", irc.PageSize)
	}
	if irc.Mode != "bot" {
		t.Fatalf("expected mode=bot, got %q", irc.Mode)
	}
	if irc.BotAddress() != "irc.example.org:6697" {
		t.Fatalf("expected bot address override, got %s", irc.BotAddress())
	}
	if !irc.BotUseTLS || irc.BotPassword != "secret" {
		t.Fatalf("expected explicit bot tls/password to stay set")
	}
	if irc.BotReconnectSeconds != 17 {
		t.Fatalf("expected reconnect override, got %d", irc.BotReconnectSeconds)
	}
}

func TestLoadIRCConfigFallbacks(t *testing.T) {
	cfg := &config.Config{
		IRCAdapter: config.IRCAdapterConfig{},
	}

	irc := loadIRCConfig(cfg, false)
	if irc.ServerHost != "0.0.0.0" {
		t.Fatalf("default host mismatch: %s", irc.ServerHost)
	}
	if irc.Mode != "server" {
		t.Fatalf("default mode mismatch: %s", irc.Mode)
	}
	if irc.ServerPort != 6667 {
		t.Fatalf("default port mismatch: %d", irc.ServerPort)
	}
	if irc.Nick != "ebusta-bot" || irc.User != "ebusta" {
		t.Fatalf("default nick/user mismatch: %s/%s", irc.Nick, irc.User)
	}
	if irc.RealName != "Ebusta Book Search Bot" {
		t.Fatalf("default real name mismatch: %s", irc.RealName)
	}
	if irc.GatewayURL != "http://localhost:8443" {
		t.Fatalf("default gateway mismatch: %s", irc.GatewayURL)
	}
	if irc.PageSize != 5 {
		t.Fatalf("default page size mismatch: %d", irc.PageSize)
	}
	if irc.Debug {
		t.Fatalf("expected debug=false by default")
	}
	if irc.BotAddress() != "127.0.0.1:6667" {
		t.Fatalf("default bot address mismatch: %s", irc.BotAddress())
	}
	if irc.BotNick != irc.Nick || irc.BotUser != irc.User || irc.BotRealName != irc.RealName {
		t.Fatalf("expected bot identity fallback from legacy nick/user/real_name")
	}
	if irc.BotReconnectSeconds != 5 {
		t.Fatalf("default bot reconnect mismatch: %d", irc.BotReconnectSeconds)
	}
}
