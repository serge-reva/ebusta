package transport

import (
	"context"
	"net/http"
	"time"

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
	msg, err := c.bot.SendMessage(ctx, &bot.SendMessageParams{
		ChatID:      chatID,
		Text:        text,
		ParseMode:   models.ParseModeMarkdown,
		ReplyMarkup: toTelegramKeyboard(keyboard),
	})
	if err != nil {
		return 0, err
	}
	if msg == nil {
		return 0, nil
	}
	return msg.ID, nil
}

func (c *BotClient) EditMessage(ctx context.Context, chatID int64, messageID int, text string, keyboard *tgpresenter.InlineKeyboardMarkup) error {
	_, err := c.bot.EditMessageText(ctx, &bot.EditMessageTextParams{
		ChatID:      chatID,
		MessageID:   messageID,
		Text:        text,
		ParseMode:   models.ParseModeMarkdown,
		ReplyMarkup: toTelegramKeyboard(keyboard),
	})
	return err
}

func (c *BotClient) AnswerCallback(ctx context.Context, callbackID, text string) error {
	_, err := c.bot.AnswerCallbackQuery(ctx, &bot.AnswerCallbackQueryParams{
		CallbackQueryID: callbackID,
		Text:            text,
	})
	return err
}

func toTelegramKeyboard(keyboard *tgpresenter.InlineKeyboardMarkup) *models.InlineKeyboardMarkup {
	if keyboard == nil || len(keyboard.InlineKeyboard) == 0 {
		return nil
	}
	rows := make([][]models.InlineKeyboardButton, 0, len(keyboard.InlineKeyboard))
	for _, row := range keyboard.InlineKeyboard {
		buttons := make([]models.InlineKeyboardButton, 0, len(row))
		for _, button := range row {
			buttons = append(buttons, models.InlineKeyboardButton{
				Text:         button.Text,
				CallbackData: button.CallbackData,
			})
		}
		rows = append(rows, buttons)
	}
	return &models.InlineKeyboardMarkup{InlineKeyboard: rows}
}
