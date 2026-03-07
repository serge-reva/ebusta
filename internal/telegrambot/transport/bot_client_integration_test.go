package transport

import (
	"bytes"
	"context"
	"testing"
	"time"

	presenter "ebusta/internal/telegrambot/presenter"
	"ebusta/internal/telegrambot/testutil"

	"github.com/go-telegram/bot"
	"github.com/go-telegram/bot/models"
)

func TestBotClientUsesTelegramAPI(t *testing.T) {
	server := testutil.NewTelegramAPIServer()
	defer server.Close()

	client, err := NewBotClient(
		"test-token",
		2*time.Second,
		func(context.Context, *bot.Bot, *models.Update) {},
		bot.WithServerURL(server.URL()),
	)
	if err != nil {
		t.Fatalf("NewBotClient() error = %v", err)
	}

	keyboard := &presenter.InlineKeyboardMarkup{
		InlineKeyboard: [][]presenter.InlineKeyboardButton{{
			{Text: "Next", CallbackData: "page:next"},
		}},
	}

	msgID, err := client.SendMessage(context.Background(), 12345, "hello", keyboard)
	if err != nil {
		t.Fatalf("SendMessage() error = %v", err)
	}
	if msgID != 101 {
		t.Fatalf("unexpected message id: %d", msgID)
	}

	if err := client.EditMessage(context.Background(), 12345, 101, "updated", keyboard); err != nil {
		t.Fatalf("EditMessage() error = %v", err)
	}
	if err := client.AnswerCallback(context.Background(), "cb-1", "done"); err != nil {
		t.Fatalf("AnswerCallback() error = %v", err)
	}
	if err := client.DeleteMessage(context.Background(), 12345, 101); err != nil {
		t.Fatalf("DeleteMessage() error = %v", err)
	}
	if _, err := client.SendDocument(context.Background(), 12345, "book.fb2", bytes.NewReader([]byte("payload")), "caption"); err != nil {
		t.Fatalf("SendDocument() error = %v", err)
	}

	calls := server.Calls()
	if len(calls) < 6 {
		t.Fatalf("expected at least 6 calls, got %d", len(calls))
	}
	if !server.HasMethod("getMe") {
		t.Fatal("expected getMe to be called during client init")
	}
	if !server.HasMethod("sendMessage") {
		t.Fatal("expected sendMessage call")
	}
	if !server.HasMethod("editMessageText") {
		t.Fatal("expected editMessageText call")
	}
	if !server.HasMethod("answerCallbackQuery") {
		t.Fatal("expected answerCallbackQuery call")
	}
	if !server.HasMethod("deleteMessage") {
		t.Fatal("expected deleteMessage call")
	}
	if !server.HasMethod("sendDocument") {
		t.Fatal("expected sendDocument call")
	}
}

func TestBotClientOmitsReplyMarkupWhenKeyboardIsNil(t *testing.T) {
	server := testutil.NewTelegramAPIServer()
	defer server.Close()

	client, err := NewBotClient(
		"test-token",
		2*time.Second,
		func(context.Context, *bot.Bot, *models.Update) {},
		bot.WithServerURL(server.URL()),
	)
	if err != nil {
		t.Fatalf("NewBotClient() error = %v", err)
	}

	if _, err := client.SendMessage(context.Background(), 12345, "hello", nil); err != nil {
		t.Fatalf("SendMessage() error = %v", err)
	}

	call, ok := server.LastCall("sendMessage")
	if !ok {
		t.Fatal("expected sendMessage call")
	}
	if bytes.Contains(call.Body, []byte("reply_markup")) {
		t.Fatalf("reply_markup must be omitted for nil keyboard, got %s", string(call.Body))
	}
}

func TestBotClientEditOmitsReplyMarkupWhenKeyboardIsNil(t *testing.T) {
	server := testutil.NewTelegramAPIServer()
	defer server.Close()

	client, err := NewBotClient(
		"test-token",
		2*time.Second,
		func(context.Context, *bot.Bot, *models.Update) {},
		bot.WithServerURL(server.URL()),
	)
	if err != nil {
		t.Fatalf("NewBotClient() error = %v", err)
	}

	if err := client.EditMessage(context.Background(), 12345, 101, "updated", nil); err != nil {
		t.Fatalf("EditMessage() error = %v", err)
	}

	call, ok := server.LastCall("editMessageText")
	if !ok {
		t.Fatal("expected editMessageText call")
	}
	if bytes.Contains(call.Body, []byte("reply_markup")) {
		t.Fatalf("reply_markup must be omitted for nil keyboard, got %s", string(call.Body))
	}
}
