package presenter

import (
	"strings"
	"testing"

	corepresenter "ebusta/internal/presenter"
)

func TestFormatBookDetailsBuildsMessageAndButtons(t *testing.T) {
	f := NewTelegramFormatter(4096, "ebusta_test_bot")
	text, keyboard := f.FormatBookDetails(&corepresenter.BookDTO{
		ID:          "sha1-1",
		Title:       `Book <One>`,
		FullAuthors: `A & B`,
	})

	if !strings.Contains(text, "<b>Book &lt;One&gt;</b>") {
		t.Fatalf("unexpected title in text: %s", text)
	}
	if !strings.Contains(text, "Authors: A &amp; B") {
		t.Fatalf("unexpected authors in text: %s", text)
	}
	if keyboard == nil || len(keyboard.InlineKeyboard) != 1 || len(keyboard.InlineKeyboard[0]) != 2 {
		t.Fatalf("unexpected keyboard: %+v", keyboard)
	}
	if keyboard.InlineKeyboard[0][0].CallbackData != "download:sha1-1" {
		t.Fatalf("unexpected download callback: %+v", keyboard.InlineKeyboard[0][0])
	}
	if keyboard.InlineKeyboard[0][1].CallbackData != "back" {
		t.Fatalf("unexpected back callback: %+v", keyboard.InlineKeyboard[0][1])
	}
}
