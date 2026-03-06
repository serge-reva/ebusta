package presenter

import (
	"fmt"
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
}

func NewTelegramFormatter(maxMessageLen int) *TelegramFormatter {
	if maxMessageLen <= 0 {
		maxMessageLen = 4096
	}
	return &TelegramFormatter{maxMessageLen: maxMessageLen}
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
		title := escapeMarkdown(book.Title)
		if title == "" {
			title = "Unknown Title"
		}
		authors := escapeMarkdown(book.FullAuthors)
		if authors == "" {
			authors = "Unknown Author"
		}
		lines = append(lines, fmt.Sprintf("%d\\. %s\nby %s", globalIndex, title, authors))
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
		"/search <query>",
		"/search <query> page <n>",
		"/page <n>",
		"/next",
		"/prev",
		"/help",
	}, "\n")
}

func (f *TelegramFormatter) FormatError(message, traceID string) string {
	if traceID == "" {
		return message
	}
	return fmt.Sprintf("%s\nTrace: %s", message, traceID)
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

func escapeMarkdown(s string) string {
	replacer := strings.NewReplacer(
		"_", "\\_",
		"*", "\\*",
		"[", "\\[",
		"]", "\\]",
		"(", "\\(",
		")", "\\)",
		"~", "\\~",
		"`", "\\`",
		">", "\\>",
		"#", "\\#",
		"+", "\\+",
		"-", "\\-",
		"=", "\\=",
		"|", "\\|",
		"{", "\\{",
		"}", "\\}",
		".", "\\.",
		"!", "\\!",
	)
	return replacer.Replace(strings.TrimSpace(s))
}
