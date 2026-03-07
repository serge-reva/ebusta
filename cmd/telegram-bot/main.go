package main

import (
	"context"
	"net/http"
	"os/signal"
	"strings"
	"syscall"
	"time"

	"ebusta/internal/config"
	"ebusta/internal/edge"
	"ebusta/internal/gatewayclient"
	"ebusta/internal/logger"
	tgpresenter "ebusta/internal/telegrambot/presenter"
	"ebusta/internal/telegrambot/session"
	"ebusta/internal/telegrambot/transport"
	"ebusta/internal/telegrambot/usecase"

	"github.com/go-telegram/bot"
	"github.com/go-telegram/bot/models"
)

func main() {
	ctx, stop := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
	defer stop()

	cfg := config.Get()
	logger.InitFromConfig(cfg.Logger, "telegram-bot")

	tgCfg := loadTelegramBotConfig(cfg)
	if !tgCfg.Enabled {
		logger.GetGlobal().InfoCtx(ctx, "telegram-bot disabled; exiting")
		return
	}
	if err := tgCfg.Validate(); err != nil {
		logger.GetGlobal().FatalCtx(ctx, "telegram-bot config validation failed", err)
	}

	gc := gatewayclient.NewClient(
		tgCfg.GatewayURL,
		gatewayclient.WithTimeout(time.Duration(tgCfg.Timeouts.ReadTimeoutSec)*time.Second),
	)
	policy := edge.PolicyFromConfig(cfg, "telegram")
	engine := edge.NewEngine(policy, edge.NewMultiHook(edge.NewLabelCounterHook(), &edge.OTelHook{}))
	store := session.NewMemoryStore()
	formatter := tgpresenter.NewTelegramFormatter(4096, tgCfg.BotUsername)
	uc := usecase.NewHandler(gc, store, formatter, engine, tgCfg.PageSize, tgCfg.MessageUpdateMode)

	if tgCfg.MockMode {
		mockClient := transport.NewMockTelegramClient(logger.GetGlobal())
		adapter := transport.NewAdapter(mockClient, uc)
		server := &http.Server{
			Addr:         tgCfg.ListenAddr(),
			Handler:      transport.NewMockWebhookServer(adapter, mockClient, tgCfg.WebhookSecret).Handler(),
			ReadTimeout:  time.Duration(tgCfg.Timeouts.ReadTimeoutSec) * time.Second,
			WriteTimeout: time.Duration(tgCfg.Timeouts.WriteTimeoutSec) * time.Second,
			IdleTimeout:  30 * time.Second,
		}
		logger.GetGlobal().WithField("mode", "mock-webhook").InfoCtx(ctx, "telegram-bot starting")
		errCh := make(chan error, 1)
		go func() {
			if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
				errCh <- err
			}
		}()
		select {
		case <-ctx.Done():
			shutdownCtx, cancel := context.WithTimeout(context.Background(), time.Duration(tgCfg.Timeouts.ShutdownTimeoutSec)*time.Second)
			defer cancel()
			if err := server.Shutdown(shutdownCtx); err != nil {
				logger.GetGlobal().FatalCtx(ctx, "telegram-bot mock webhook shutdown failed", err)
			}
		case err := <-errCh:
			logger.GetGlobal().FatalCtx(ctx, "telegram-bot mock webhook failed", err)
		}
		logger.GetGlobal().InfoCtx(ctx, "telegram-bot stopped")
		return
	}

	var adapter *transport.Adapter
	client, err := transport.NewBotClient(
		tgCfg.BotToken,
		time.Duration(tgCfg.Timeouts.PollTimeoutSec)*time.Second,
		func(updateCtx context.Context, _ *bot.Bot, update *models.Update) {
			if adapter == nil {
				return
			}
			transport.NewUpdateHandler(adapter)(updateCtx, nil, update)
		},
		buildBotOptions(tgCfg)...,
	)
	if err != nil {
		logger.GetGlobal().FatalCtx(ctx, "telegram-bot initialization failed", err)
	}
	adapter = transport.NewAdapter(client, uc)

	logger.GetGlobal().WithField("mode", tgCfg.Mode).InfoCtx(ctx, "telegram-bot starting")

	switch strings.ToLower(strings.TrimSpace(tgCfg.Mode)) {
	case "webhook":
		if err := transport.ConfigureWebhook(ctx, client, tgCfg.WebhookURL); err != nil {
			logger.GetGlobal().FatalCtx(ctx, "telegram-bot webhook configuration failed", err)
		}
		server := transport.NewWebhookServer(tgCfg.ListenAddr(), client)
		if err := transport.RunWebhook(ctx, client, server); err != nil {
			logger.GetGlobal().FatalCtx(ctx, "telegram-bot webhook runner failed", err)
		}
		logger.GetGlobal().InfoCtx(ctx, "telegram-bot stopped")
	default:
		transport.RunPolling(ctx, client)
		logger.GetGlobal().InfoCtx(ctx, "telegram-bot stopped")
	}
}

func loadTelegramBotConfig(cfg *config.Config) config.TelegramBotConfig {
	tg := cfg.TelegramBot
	if strings.TrimSpace(tg.Mode) == "" {
		tg.Mode = "polling"
	}
	if strings.TrimSpace(tg.MessageUpdateMode) == "" {
		tg.MessageUpdateMode = "edit"
	}
	if strings.TrimSpace(tg.ListenHost) == "" {
		tg.ListenHost = "0.0.0.0"
	}
	if tg.ListenPort == 0 {
		tg.ListenPort = 8088
	}
	if tg.PageSize <= 0 {
		tg.PageSize = 5
	}
	if tg.Timeouts.ReadTimeoutSec <= 0 {
		tg.Timeouts.ReadTimeoutSec = 5
	}
	if tg.Timeouts.WriteTimeoutSec <= 0 {
		tg.Timeouts.WriteTimeoutSec = 10
	}
	if tg.Timeouts.ShutdownTimeoutSec <= 0 {
		tg.Timeouts.ShutdownTimeoutSec = 10
	}
	if tg.Timeouts.PollTimeoutSec <= 0 {
		tg.Timeouts.PollTimeoutSec = 30
	}
	return tg
}

func buildBotOptions(cfg config.TelegramBotConfig) []bot.Option {
	var opts []bot.Option
	if cfg.Debug {
		opts = append(opts, bot.WithDebug())
		opts = append(opts, bot.WithDebugHandler(func(format string, args ...any) {
			logger.GetGlobal().Debugf("", format, args...)
		}))
	}
	if strings.EqualFold(cfg.Mode, "webhook") && strings.TrimSpace(cfg.WebhookSecret) != "" {
		opts = append(opts, bot.WithWebhookSecretToken(cfg.WebhookSecret))
	}
	return opts
}
