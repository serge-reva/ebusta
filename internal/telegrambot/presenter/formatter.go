package presenter

import (
	"fmt"
	"html"
	"strings"

	corepresenter "ebusta/internal/presenter"
)

type InlineKeyboardButton struct {
	Text         string `json:"text"`
	CallbackData string `json:"callback_data,omitempty"`
}

type InlineKeyboardMarkup struct {
	InlineKeyboard [][]InlineKeyboardButton `json:"inline_keyboard"`
}

type TelegramFormatter struct {
	maxMessageLen int
	botUsername   string
}

func NewTelegramFormatter(maxMessageLen int, botUsername string) *TelegramFormatter {
	if maxMessageLen <= 0 {
		maxMessageLen = 4096
	}
	return &TelegramFormatter{maxMessageLen: maxMessageLen, botUsername: strings.TrimSpace(botUsername)}
}

func (f *TelegramFormatter) FormatSearchResult(result *corepresenter.PresenterResult, page int) (string, *InlineKeyboardMarkup, error) {
	if result == nil || result.SearchResult == nil {
		return "No results available.", nil, nil
	}
	if result.Total == 0 {
		return "No books found.", nil, nil
	}

	pg := result.Pagination
	if pg == nil {
		pg = corepresenter.NewPagination(result.Total, page, len(result.Books))
	}

	var lines []string
	lines = append(lines, fmt.Sprintf("Found %d books. Page %d/%d.", result.Total, pg.CurrentPage, pg.TotalPages))
	for i, book := range result.Books {
		globalIndex := (pg.CurrentPage-1)*pg.PageSize + i + 1
		title := html.EscapeString(strings.TrimSpace(book.Title))
		if title == "" {
			title = "Unknown Title"
		}
		authors := html.EscapeString(strings.TrimSpace(book.FullAuthors))
		if authors == "" {
			authors = "Unknown Author"
		}

		bookLine := fmt.Sprintf("%d. %s", globalIndex, title)
		if f.botUsername != "" {
			bookLine = fmt.Sprintf("<a href=\"https://t.me/%s?start=book_%d\">%s</a>", html.EscapeString(f.botUsername), globalIndex, bookLine)
		}
		lines = append(lines, fmt.Sprintf("%s\nby %s", bookLine, authors))
	}

	text := strings.Join(lines, "\n\n")
	if len(text) > f.maxMessageLen {
		text = text[:f.maxMessageLen-3] + "..."
	}

	return text, buildKeyboard(pg), nil
}

func (f *TelegramFormatter) FormatHelp() string {
	return strings.Join([]string{
		"Available commands:",
		"/search &lt;query&gt;",
		"/search &lt;query&gt; page &lt;n&gt;",
		"/page &lt;n&gt;",
		"/next",
		"/prev",
		"/help",
	}, "\n")
}

func (f *TelegramFormatter) FormatError(message, traceID string) string {
	safeMessage := html.EscapeString(message)
	if traceID == "" {
		return safeMessage
	}
	return fmt.Sprintf("%s\nTrace: %s", safeMessage, html.EscapeString(traceID))
}

func buildKeyboard(pg *corepresenter.Pagination) *InlineKeyboardMarkup {
	if pg == nil || pg.TotalPages <= 1 {
		return nil
	}
	var row []InlineKeyboardButton
	if pg.HasPrev {
		row = append(row, InlineKeyboardButton{Text: "Prev", CallbackData: "page:prev"})
	}
	row = append(row, InlineKeyboardButton{Text: fmt.Sprintf("%d/%d", pg.CurrentPage, pg.TotalPages), CallbackData: "page:current"})
	if pg.HasNext {
		row = append(row, InlineKeyboardButton{Text: "Next", CallbackData: "page:next"})
	}
	return &InlineKeyboardMarkup{InlineKeyboard: [][]InlineKeyboardButton{row}}
}
