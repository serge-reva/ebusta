package transport

import (
	"testing"

	"github.com/go-telegram/bot/models"
)

func TestMapUpdateMessage(t *testing.T) {
	update := &models.Update{Message: &models.Message{
		ID:   42,
		Text: "/search dune",
		Chat: models.Chat{ID: 1001},
		From: &models.User{ID: 2002},
	}}

	mapped, ok := MapUpdate(update, "tg-trace")
	if !ok {
		t.Fatal("expected message update to be mapped")
	}
	if mapped.TraceID != "tg-trace" {
		t.Fatalf("trace mismatch: %q", mapped.TraceID)
	}
	if mapped.ChatID != 1001 {
		t.Fatalf("chat mismatch: %d", mapped.ChatID)
	}
	if mapped.UserID != "2002" {
		t.Fatalf("user mismatch: %q", mapped.UserID)
	}
	if mapped.Text != "/search dune" {
		t.Fatalf("text mismatch: %q", mapped.Text)
	}
	if mapped.MessageID != 42 {
		t.Fatalf("message id mismatch: %d", mapped.MessageID)
	}
}

func TestMapUpdateCallbackQuery(t *testing.T) {
	update := &models.Update{CallbackQuery: &models.CallbackQuery{
		ID:   "cb-1",
		Data: "page:next",
		From: models.User{ID: 3003},
		Message: models.MaybeInaccessibleMessage{Message: &models.Message{
			ID:   77,
			Chat: models.Chat{ID: 4004},
		}},
	}}

	mapped, ok := MapUpdate(update, "tg-trace")
	if !ok {
		t.Fatal("expected callback update to be mapped")
	}
	if mapped.CallbackID != "cb-1" {
		t.Fatalf("callback id mismatch: %q", mapped.CallbackID)
	}
	if mapped.CallbackData != "page:next" {
		t.Fatalf("callback data mismatch: %q", mapped.CallbackData)
	}
	if mapped.ChatID != 4004 {
		t.Fatalf("chat mismatch: %d", mapped.ChatID)
	}
	if mapped.MessageID != 77 {
		t.Fatalf("message id mismatch: %d", mapped.MessageID)
	}
	if mapped.UserID != "3003" {
		t.Fatalf("user mismatch: %q", mapped.UserID)
	}
}

func TestMapUpdateUnsupported(t *testing.T) {
	if _, ok := MapUpdate(&models.Update{}, "tg-trace"); ok {
		t.Fatal("expected unsupported update to be rejected")
	}
}
