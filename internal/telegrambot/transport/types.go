package transport

import (
	"bytes"
	"context"

	tgpresenter "ebusta/internal/telegrambot/presenter"
	"ebusta/internal/telegrambot/usecase"
)

type IncomingUpdate struct {
	TraceID      string
	UserID       string
	ChatID       int64
	MessageID    int
	Text         string
	CallbackID   string
	CallbackData string
}

type TelegramClient interface {
	SendMessage(ctx context.Context, chatID int64, text string, keyboard *tgpresenter.InlineKeyboardMarkup) (int, error)
	EditMessage(ctx context.Context, chatID int64, messageID int, text string, keyboard *tgpresenter.InlineKeyboardMarkup) error
	DeleteMessage(ctx context.Context, chatID int64, messageID int) error
	SendDocument(ctx context.Context, chatID int64, filename string, data *bytes.Reader, caption string) (int, error)
	AnswerCallback(ctx context.Context, callbackID, text string) error
}

type UsecaseHandler interface {
	HandleSearch(ctx context.Context, userID, input, traceID string) (*usecase.Result, error)
	HandlePage(ctx context.Context, userID string, page int, traceID string) (*usecase.Result, error)
	HandleSelectBook(ctx context.Context, userID string, bookIndex int, traceID string) (*usecase.Result, error)
	HandleHelp(traceID string) *usecase.Result
	HandleCallback(ctx context.Context, userID, callbackData, traceID string) (*usecase.Result, error)
	RememberBotMessage(ctx context.Context, userID string, messageID int, view string) error
}
