package presenter

import (
	"strings"
	"testing"
)

func TestIRCFormatterFormatSearchResultWithPagination(t *testing.T) {
	f := NewIRCFormatter(5)
	result := &PresenterResult{
		SearchResult: &SearchResult{
			Total: 12,
			Books: []BookDTO{
				{Title: "Book 1", FullAuthors: "A1"},
				{Title: "Book 2", FullAuthors: "A2"},
			},
		},
		Pagination: &Pagination{
			CurrentPage: 2,
			TotalPages:  3,
			PageSize:    5,
			TotalItems:  12,
			HasPrev:     true,
			HasNext:     true,
		},
	}

	lines, page := f.FormatSearchResult(result, 2)
	joined := strings.Join(lines, "\n")
	if !strings.Contains(joined, "Found 12 books") {
		t.Fatalf("expected count in output, got: %v", lines)
	}
	if !strings.Contains(joined, "page 2/3") || !strings.Contains(joined, "limit 5") {
		t.Fatalf("expected page/limit in output, got: %v", lines)
	}
	if !strings.Contains(joined, "6. \"Book 1\"") {
		t.Fatalf("expected global item numbering, got: %v", lines)
	}
	if !strings.Contains(joined, "!prev") || !strings.Contains(joined, "!next") || !strings.Contains(joined, "!page <n>") {
		t.Fatalf("expected navigation hints, got: %v", lines)
	}
	if !strings.Contains(joined, "Use !info <number>") {
		t.Fatalf("expected !info hint, got: %v", lines)
	}
	if page == nil || page.CurrentPage != 2 || page.TotalPages != 3 {
		t.Fatalf("unexpected page metadata: %+v", page)
	}
}

func TestIRCFormatterFormatSearchResultNoBooks(t *testing.T) {
	f := NewIRCFormatter(5)
	lines, page := f.FormatSearchResult(&PresenterResult{
		SearchResult: &SearchResult{Total: 0, Books: nil},
	}, 1)
	if len(lines) != 1 || lines[0] != "📚 No books found" {
		t.Fatalf("unexpected empty output: %v", lines)
	}
	if page != nil {
		t.Fatalf("expected nil page for empty result")
	}
}
