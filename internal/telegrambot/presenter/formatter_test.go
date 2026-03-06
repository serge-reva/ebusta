package presenter

import (
	"strings"
	"testing"

	corepresenter "ebusta/internal/presenter"
)

func TestFormatSearchResultBuildsKeyboard(t *testing.T) {
	f := NewTelegramFormatter(4096)
	result := &corepresenter.PresenterResult{
		SearchResult: &corepresenter.SearchResult{
			TraceId: "tg-1",
			Total:   12,
			Books: []corepresenter.BookDTO{
				{ID: "1", Title: "War & Peace", FullAuthors: "Leo Tolstoy", DownloadURL: "/download/t1"},
			},
		},
		Pagination: corepresenter.NewPagination(12, 2, 5),
	}

	text, keyboard, err := f.FormatSearchResult(result, 2)
	if err != nil {
		t.Fatalf("FormatSearchResult() error = %v", err)
	}
	if !strings.Contains(text, "Found 12 books") {
		t.Fatalf("unexpected text: %s", text)
	}
	if keyboard == nil || len(keyboard.InlineKeyboard) == 0 {
		t.Fatal("expected inline keyboard")
	}
}

func TestFormatSearchResultEscapesMarkdown(t *testing.T) {
	f := NewTelegramFormatter(4096)
	result := &corepresenter.PresenterResult{
		SearchResult: &corepresenter.SearchResult{
			TraceId: "tg-1",
			Total:   1,
			Books: []corepresenter.BookDTO{
				{ID: "1", Title: "Book_[1]", FullAuthors: "A_B", DownloadURL: "/download/t1"},
			},
		},
		Pagination: corepresenter.NewPagination(1, 1, 5),
	}

	text, _, err := f.FormatSearchResult(result, 1)
	if err != nil {
		t.Fatalf("FormatSearchResult() error = %v", err)
	}
	if strings.Contains(text, "Book_[1]") {
		t.Fatalf("expected markdown escaping, got %s", text)
	}
	if !strings.Contains(text, "Book\\_\\[1\\]") {
		t.Fatalf("expected escaped markdown, got %s", text)
	}
}

func TestFormatSearchResultRespectsLengthLimit(t *testing.T) {
	f := NewTelegramFormatter(80)
	result := &corepresenter.PresenterResult{
		SearchResult: &corepresenter.SearchResult{
			TraceId: "tg-1",
			Total:   1,
			Books: []corepresenter.BookDTO{
				{ID: "1", Title: strings.Repeat("A", 100), FullAuthors: "Author"},
			},
		},
		Pagination: corepresenter.NewPagination(1, 1, 5),
	}

	text, _, err := f.FormatSearchResult(result, 1)
	if err != nil {
		t.Fatalf("FormatSearchResult() error = %v", err)
	}
	if len(text) > 80 {
		t.Fatalf("expected capped text length, got %d", len(text))
	}
}
