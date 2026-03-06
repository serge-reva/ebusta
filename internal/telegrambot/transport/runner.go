package transport

import (
	"context"
	"errors"
	"net/http"
	"time"

	"ebusta/internal/errutil"
	"ebusta/internal/logger"

	"github.com/go-telegram/bot"
	"github.com/go-telegram/bot/models"
)

func NewUpdateHandler(adapter *Adapter) bot.HandlerFunc {
	return func(ctx context.Context, _ *bot.Bot, update *models.Update) {
		traceID := errutil.GenerateTraceID("tg")
		mapped, ok := MapUpdate(update, traceID)
		if !ok {
			logger.GetGlobal().Debug(traceID, "telegram-bot: ignoring unsupported update")
			return
		}
		if err := adapter.ProcessUpdate(ctx, mapped); err != nil {
			logger.GetGlobal().Error(traceID, "telegram-bot: failed to process update", err)
		}
	}
}

func RunPolling(ctx context.Context, client *BotClient) {
	client.Bot().Start(ctx)
}

func ConfigureWebhook(ctx context.Context, client *BotClient, webhookURL string) error {
	_, err := client.Bot().SetWebhook(ctx, &bot.SetWebhookParams{URL: webhookURL})
	return err
}

func RunWebhook(ctx context.Context, client *BotClient, server *http.Server) error {
	errCh := make(chan error, 1)
	go func() {
		client.Bot().StartWebhook(ctx)
	}()
	go func() {
		if err := server.ListenAndServe(); err != nil && !errors.Is(err, http.ErrServerClosed) {
			errCh <- err
		}
	}()

	select {
	case <-ctx.Done():
		shutdownCtx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		return server.Shutdown(shutdownCtx)
	case err := <-errCh:
		return err
	}
}

func NewWebhookServer(addr string, client *BotClient) *http.Server {
	mux := http.NewServeMux()
	mux.Handle("/", client.Bot().WebhookHandler())
	return &http.Server{
		Addr:    addr,
		Handler: mux,
	}
}
