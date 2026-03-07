package presenter

import (
	"fmt"
	"html"
	"strings"

	corepresenter "ebusta/internal/presenter"
)

func (f *TelegramFormatter) FormatBookDetails(book *corepresenter.BookDTO) (string, *InlineKeyboardMarkup) {
	if book == nil {
		return "Book details are unavailable.", nil
	}

	title := html.EscapeString(strings.TrimSpace(book.Title))
	if title == "" {
		title = "Unknown Title"
	}

	authors := html.EscapeString(strings.TrimSpace(book.FullAuthors))
	if authors == "" && len(book.Authors) > 0 {
		authors = html.EscapeString(strings.Join(book.Authors, ", "))
	}
	if authors == "" {
		authors = "Unknown Author"
	}

	text := fmt.Sprintf("<b>%s</b>\n\nAuthors: %s", title, authors)
	if len(text) > f.maxMessageLen {
		text = text[:f.maxMessageLen-3] + "..."
	}

	return text, &InlineKeyboardMarkup{
		InlineKeyboard: [][]InlineKeyboardButton{{
			{Text: "📥 Скачать", CallbackData: "download:" + book.ID},
			{Text: "◀️ Назад", CallbackData: "back"},
		}},
	}
}
