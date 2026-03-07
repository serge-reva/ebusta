package main

import (
	"testing"

	"ebusta/internal/config"
)

func TestLoadTelegramConfigDefaultsAndVerbose(t *testing.T) {
	cfg := &config.Config{TelegramAdapter: config.TelegramAdapterConfig{}}
	tg := loadTelegramConfig(cfg, true)

	if tg.ListenHost != "0.0.0.0" {
		t.Fatalf("default listen_host mismatch: %s", tg.ListenHost)
	}
	if tg.Port != 8087 {
		t.Fatalf("default port mismatch: %d", tg.Port)
	}
	if tg.GatewayURL != "http://localhost:8443" {
		t.Fatalf("default gateway mismatch: %s", tg.GatewayURL)
	}
	if tg.PageSize != 5 {
		t.Fatalf("default page size mismatch: %d", tg.PageSize)
	}
	if !tg.Debug {
		t.Fatalf("verbose must set debug=true")
	}
}

func TestLoadTelegramConfigKeepsExplicitValues(t *testing.T) {
	cfg := &config.Config{TelegramAdapter: config.TelegramAdapterConfig{
		ListenHost: "127.0.0.1",
		Port:       8090,
		GatewayURL: "http://gw:8443",
		PageSize:   7,
		Debug:      false,
	}}
	tg := loadTelegramConfig(cfg, false)

	if tg.ListenHost != "127.0.0.1" || tg.Port != 8090 {
		t.Fatalf("expected explicit listen_host/port, got %s:%d", tg.ListenHost, tg.Port)
	}
	if tg.GatewayURL != "http://gw:8443" {
		t.Fatalf("expected explicit gateway_url, got %s", tg.GatewayURL)
	}
	if tg.PageSize != 7 {
		t.Fatalf("expected explicit page_size, got %d", tg.PageSize)
	}
	if tg.Debug {
		t.Fatalf("debug should stay false")
	}
}

func TestLoadTelegramConfigFallsBackToLegacyHost(t *testing.T) {
	cfg := &config.Config{TelegramAdapter: config.TelegramAdapterConfig{
		Host: "127.0.0.2",
		Port: 8087,
	}}
	tg := loadTelegramConfig(cfg, false)
	if tg.ListenHost != "127.0.0.2" {
		t.Fatalf("expected legacy host fallback, got %s", tg.ListenHost)
	}
}
