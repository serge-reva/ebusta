package transport

import (
	"context"
	"strings"

	"ebusta/internal/botcommand"
	"ebusta/internal/errutil"
	"ebusta/internal/logger"
	"ebusta/internal/telegrambot/usecase"
)

type Adapter struct {
	client  TelegramClient
	handler UsecaseHandler
}

func NewAdapter(client TelegramClient, handler UsecaseHandler) *Adapter {
	return &Adapter{client: client, handler: handler}
}

func (a *Adapter) ProcessUpdate(ctx context.Context, update IncomingUpdate) error {
	traceID := strings.TrimSpace(update.TraceID)
	if traceID == "" {
		traceID = errutil.GenerateTraceID("tg")
	}
	ctx = logger.ContextWithTraceID(ctx, traceID)

	if update.CallbackID != "" {
		if err := a.client.AnswerCallback(ctx, update.CallbackID, ""); err != nil {
			return err
		}
		result, err := a.handler.HandleCallback(ctx, update.UserID, update.CallbackData, traceID)
		if err != nil {
			return err
		}
		return a.respond(ctx, update.UserID, update.ChatID, update.MessageID, result, true)
	}

	var (
		result *usecase.Result
		err    error
	)

	switch c := botcommand.Parse(update.Text).(type) {
	case botcommand.HelpCommand:
		result = a.handler.HandleHelp(traceID)
	case botcommand.SelectBookCommand:
		result, err = a.handler.HandleSelectBook(ctx, update.UserID, c.BookIndex, traceID)
	case botcommand.PageCommand:
		result, err = a.handler.HandlePage(ctx, update.UserID, c.Page, traceID)
	case botcommand.NextCommand:
		result, err = a.handler.HandleCallback(ctx, update.UserID, "page:next", traceID)
	case botcommand.PrevCommand:
		result, err = a.handler.HandleCallback(ctx, update.UserID, "page:prev", traceID)
	case botcommand.SearchCommand:
		result, err = a.handler.HandleSearch(ctx, update.UserID, update.Text, traceID)
	case botcommand.InvalidCommand:
		result, err = a.handler.HandleSearch(ctx, update.UserID, update.Text, traceID)
	default:
		result = a.handler.HandleHelp(traceID)
	}
	if err != nil {
		return err
	}
	return a.respond(ctx, update.UserID, update.ChatID, update.MessageID, result, false)
}

func (a *Adapter) respond(ctx context.Context, userID string, chatID int64, messageID int, result *usecase.Result, preferEdit bool) error {
	if result == nil {
		return nil
	}
	if result.Document != nil {
		_, err := a.client.SendDocument(ctx, chatID, result.Document.Filename, result.Document.Data, result.Document.Caption)
		return err
	}
	if result.DeleteAndSend && result.TargetMessageID != 0 {
		_ = a.client.DeleteMessage(ctx, chatID, result.TargetMessageID)
		sentMessageID, err := a.client.SendMessage(ctx, chatID, result.Text, result.Keyboard)
		if err != nil {
			return err
		}
		return a.handler.RememberBotMessage(ctx, userID, sentMessageID, result.StoreAsView)
	}
	if !result.ForceSend {
		targetMessageID := result.TargetMessageID
		if targetMessageID == 0 && preferEdit && chatID != 0 && messageID != 0 {
			targetMessageID = messageID
		}
		if targetMessageID != 0 {
			err := a.client.EditMessage(ctx, chatID, targetMessageID, result.Text, result.Keyboard)
			if err == nil {
				return a.handler.RememberBotMessage(ctx, userID, targetMessageID, result.StoreAsView)
			}
			if isIgnorableEditError(err) {
				return nil
			}
			if !shouldFallbackToSend(err) {
				return err
			}
		}
	}
	sentMessageID, err := a.client.SendMessage(ctx, chatID, result.Text, result.Keyboard)
	if err != nil {
		return err
	}
	return a.handler.RememberBotMessage(ctx, userID, sentMessageID, result.StoreAsView)
}

func shouldFallbackToSend(err error) bool {
	if err == nil {
		return false
	}
	message := strings.ToLower(err.Error())
	return strings.Contains(message, "message to edit not found") ||
		strings.Contains(message, "message can't be edited")
}

func isIgnorableEditError(err error) bool {
	if err == nil {
		return false
	}
	return strings.Contains(strings.ToLower(err.Error()), "message is not modified")
}
