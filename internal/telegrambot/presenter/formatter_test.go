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
	if !strings.Contains(text, "Found 12 books. Page 2/3.") {
		t.Fatalf("unexpected text: %s", text)
	}
	if keyboard == nil || len(keyboard.InlineKeyboard) == 0 {
		t.Fatal("expected inline keyboard")
	}
}

func TestFormatSearchResultEscapesHTML(t *testing.T) {
	f := NewTelegramFormatter(4096)
	result := &corepresenter.PresenterResult{
		SearchResult: &corepresenter.SearchResult{
			TraceId: "tg-1",
			Total:   1,
			Books: []corepresenter.BookDTO{
				{ID: "1", Title: `Book <One> & Co`, FullAuthors: `A > B & C`, DownloadURL: "/download/t1"},
			},
		},
		Pagination: corepresenter.NewPagination(1, 1, 5),
	}

	text, _, err := f.FormatSearchResult(result, 1)
	if err != nil {
		t.Fatalf("FormatSearchResult() error = %v", err)
	}
	if strings.Contains(text, `Book <One> & Co`) {
		t.Fatalf("expected HTML escaping, got %s", text)
	}
	if !strings.Contains(text, `Book &lt;One&gt; &amp; Co`) {
		t.Fatalf("expected escaped title, got %s", text)
	}
	if !strings.Contains(text, `A &gt; B &amp; C`) {
		t.Fatalf("expected escaped authors, got %s", text)
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

func TestFormatErrorEscapesHTML(t *testing.T) {
	f := NewTelegramFormatter(4096)
	text := f.FormatError(`bad <error> & reason`, `tg-1<2>`)
	if !strings.Contains(text, `bad &lt;error&gt; &amp; reason`) {
		t.Fatalf("expected escaped message, got %s", text)
	}
	if !strings.Contains(text, `Trace: tg-1&lt;2&gt;`) {
		t.Fatalf("expected escaped trace id, got %s", text)
	}
}
