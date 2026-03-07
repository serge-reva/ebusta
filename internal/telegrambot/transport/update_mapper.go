package transport

import (
	"strconv"
	"strings"

	"github.com/go-telegram/bot/models"
)

func MapUpdate(update *models.Update, traceID string) (IncomingUpdate, bool) {
	if update == nil {
		return IncomingUpdate{}, false
	}
	if update.Message != nil && update.Message.From != nil {
		return IncomingUpdate{
			TraceID:   traceID,
			UserID:    strconv.FormatInt(update.Message.From.ID, 10),
			ChatID:    update.Message.Chat.ID,
			MessageID: update.Message.ID,
			Text:      strings.TrimSpace(update.Message.Text),
		}, true
	}
	if update.CallbackQuery != nil {
		mapped := IncomingUpdate{
			TraceID:      traceID,
			UserID:       strconv.FormatInt(update.CallbackQuery.From.ID, 10),
			CallbackID:   update.CallbackQuery.ID,
			CallbackData: strings.TrimSpace(update.CallbackQuery.Data),
		}
		if update.CallbackQuery.Message.Message != nil {
			mapped.ChatID = update.CallbackQuery.Message.Message.Chat.ID
			mapped.MessageID = update.CallbackQuery.Message.Message.ID
		} else if update.CallbackQuery.Message.InaccessibleMessage != nil {
			mapped.ChatID = update.CallbackQuery.Message.InaccessibleMessage.Chat.ID
			mapped.MessageID = update.CallbackQuery.Message.InaccessibleMessage.MessageID
		}
		return mapped, true
	}
	return IncomingUpdate{}, false
}
