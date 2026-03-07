package transport

import (
	"bytes"
	"context"
	"encoding/json"
	"net/http"
	"time"

	"ebusta/internal/logger"
	tgpresenter "ebusta/internal/telegrambot/presenter"

	"github.com/go-telegram/bot"
	"github.com/go-telegram/bot/models"
)

type BotClient struct {
	bot *bot.Bot
}

func NewBotClient(token string, pollTimeout time.Duration, defaultHandler bot.HandlerFunc, options ...bot.Option) (*BotClient, error) {
	opts := []bot.Option{
		bot.WithHTTPClient(pollTimeout, &http.Client{Timeout: pollTimeout}),
		bot.WithDefaultHandler(defaultHandler),
	}
	opts = append(opts, options...)
	b, err := bot.New(token, opts...)
	if err != nil {
		return nil, err
	}
	return &BotClient{bot: b}, nil
}

func (c *BotClient) Bot() *bot.Bot {
	return c.bot
}

func (c *BotClient) SendMessage(ctx context.Context, chatID int64, text string, keyboard *tgpresenter.InlineKeyboardMarkup) (int, error) {
	params := &bot.SendMessageParams{
		ChatID:    chatID,
		Text:      text,
		ParseMode: models.ParseModeHTML,
	}
	if replyMarkup := toTelegramKeyboard(keyboard); replyMarkup != nil {
		params.ReplyMarkup = replyMarkup
	}
	logTextRequest(ctx, "sendMessage", chatID, 0, text, keyboard)
	msg, err := c.bot.SendMessage(ctx, params)
	if err != nil {
		logger.GetGlobal().WithField("chat_id", chatID).ErrorCtx(ctx, "telegram-bot sendMessage failed", err)
		return 0, err
	}
	if msg == nil {
		return 0, nil
	}
	return msg.ID, nil
}

func (c *BotClient) EditMessage(ctx context.Context, chatID int64, messageID int, text string, keyboard *tgpresenter.InlineKeyboardMarkup) error {
	params := &bot.EditMessageTextParams{
		ChatID:    chatID,
		MessageID: messageID,
		Text:      text,
		ParseMode: models.ParseModeHTML,
	}
	if replyMarkup := toTelegramKeyboard(keyboard); replyMarkup != nil {
		params.ReplyMarkup = replyMarkup
	}
	logTextRequest(ctx, "editMessageText", chatID, messageID, text, keyboard)
	_, err := c.bot.EditMessageText(ctx, params)
	if err != nil {
		logger.GetGlobal().WithFields(map[string]interface{}{"chat_id": chatID, "message_id": messageID}).ErrorCtx(ctx, "telegram-bot editMessageText failed", err)
	}
	return err
}

func (c *BotClient) SendDocument(ctx context.Context, chatID int64, filename string, data *bytes.Reader, caption string) (int, error) {
	logger.GetGlobal().WithFields(map[string]interface{}{
		"chat_id":     chatID,
		"filename":    filename,
		"caption_len": len(caption),
		"size_bytes":  data.Len(),
	}).DebugCtx(ctx, "telegram-bot sendDocument request")
	msg, err := c.bot.SendDocument(ctx, &bot.SendDocumentParams{
		ChatID:   chatID,
		Document: &models.InputFileUpload{Filename: filename, Data: data},
		Caption:  caption,
	})
	if err != nil {
		logger.GetGlobal().WithFields(map[string]interface{}{"chat_id": chatID, "filename": filename}).ErrorCtx(ctx, "telegram-bot sendDocument failed", err)
		return 0, err
	}
	if msg == nil {
		return 0, nil
	}
	return msg.ID, nil
}

func (c *BotClient) AnswerCallback(ctx context.Context, callbackID, text string) error {
	logger.GetGlobal().WithFields(map[string]interface{}{"callback_id": callbackID, "text_len": len(text)}).DebugCtx(ctx, "telegram-bot answerCallbackQuery request")
	_, err := c.bot.AnswerCallbackQuery(ctx, &bot.AnswerCallbackQueryParams{
		CallbackQueryID: callbackID,
		Text:            text,
	})
	if err != nil {
		logger.GetGlobal().WithField("callback_id", callbackID).ErrorCtx(ctx, "telegram-bot answerCallbackQuery failed", err)
	}
	return err
}

func toTelegramKeyboard(keyboard *tgpresenter.InlineKeyboardMarkup) *models.InlineKeyboardMarkup {
	if keyboard == nil || len(keyboard.InlineKeyboard) == 0 {
		return nil
	}
	rows := make([][]models.InlineKeyboardButton, 0, len(keyboard.InlineKeyboard))
	for _, row := range keyboard.InlineKeyboard {
		if len(row) == 0 {
			continue
		}
		buttons := make([]models.InlineKeyboardButton, 0, len(row))
		for _, button := range row {
			if button.Text == "" {
				continue
			}
			buttons = append(buttons, models.InlineKeyboardButton{
				Text:         button.Text,
				CallbackData: button.CallbackData,
			})
		}
		if len(buttons) > 0 {
			rows = append(rows, buttons)
		}
	}
	if len(rows) == 0 {
		return nil
	}
	return &models.InlineKeyboardMarkup{InlineKeyboard: rows}
}

func logTextRequest(ctx context.Context, method string, chatID int64, messageID int, text string, keyboard *tgpresenter.InlineKeyboardMarkup) {
	payload := "null"
	if keyboard != nil {
		if b, err := json.Marshal(keyboard); err == nil {
			payload = string(b)
		}
	}
	logger.GetGlobal().WithFields(map[string]interface{}{
		"method":        method,
		"chat_id":       chatID,
		"message_id":    messageID,
		"text_len":      len(text),
		"has_keyboard":  keyboard != nil,
		"keyboard_json": payload,
	}).DebugCtx(ctx, "telegram-bot outbound request")
}
