package main

import (
	"testing"

	"ebusta/internal/config"
)

func TestLoadIRCConfigDefaultsAndVerboseOverride(t *testing.T) {
	cfg := &config.Config{
		IRCAdapter: config.IRCAdapterConfig{
			ServerHost: "127.0.0.1",
			ServerPort: 7000,
			Nick:       "n1",
			User:       "u1",
			RealName:   "r1",
			GatewayURL: "http://gw:8443",
			PageSize:   9,
			Debug:      false,
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
}

func TestLoadIRCConfigFallbacks(t *testing.T) {
	cfg := &config.Config{
		IRCAdapter: config.IRCAdapterConfig{},
	}

	irc := loadIRCConfig(cfg, false)
	if irc.ServerHost != "0.0.0.0" {
		t.Fatalf("default host mismatch: %s", irc.ServerHost)
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
}
