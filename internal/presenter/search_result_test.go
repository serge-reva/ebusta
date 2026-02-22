package presenter

import "testing"

func TestNewPagination(t *testing.T) {
	tests := []struct {
		name           string
		totalItems     int
		page           int
		pageSize       int
		wantPage       int
		wantTotalPages int
		wantHasPrev    bool
		wantHasNext    bool
	}{
		{
			name:           "normal",
			totalItems:     25,
			page:           2,
			pageSize:       10,
			wantPage:       2,
			wantTotalPages: 3,
			wantHasPrev:    true,
			wantHasNext:    true,
		},
		{
			name:           "empty_total",
			totalItems:     0,
			page:           1,
			pageSize:       10,
			wantPage:       1,
			wantTotalPages: 1,
			wantHasPrev:    false,
			wantHasNext:    false,
		},
		{
			name:           "page_too_small",
			totalItems:     10,
			page:           0,
			pageSize:       5,
			wantPage:       1,
			wantTotalPages: 2,
			wantHasPrev:    false,
			wantHasNext:    true,
		},
		{
			name:           "page_too_large",
			totalItems:     10,
			page:           99,
			pageSize:       3,
			wantPage:       4,
			wantTotalPages: 4,
			wantHasPrev:    true,
			wantHasNext:    false,
		},
		{
			name:           "invalid_page_size",
			totalItems:     7,
			page:           1,
			pageSize:       0,
			wantPage:       1,
			wantTotalPages: 7,
			wantHasPrev:    false,
			wantHasNext:    true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pg := NewPagination(tt.totalItems, tt.page, tt.pageSize)
			if pg.CurrentPage != tt.wantPage {
				t.Fatalf("CurrentPage = %d, want %d", pg.CurrentPage, tt.wantPage)
			}
			if pg.TotalPages != tt.wantTotalPages {
				t.Fatalf("TotalPages = %d, want %d", pg.TotalPages, tt.wantTotalPages)
			}
			if pg.HasPrev != tt.wantHasPrev {
				t.Fatalf("HasPrev = %v, want %v", pg.HasPrev, tt.wantHasPrev)
			}
			if pg.HasNext != tt.wantHasNext {
				t.Fatalf("HasNext = %v, want %v", pg.HasNext, tt.wantHasNext)
			}
		})
	}
}

func TestNewPresenterResultUsesNewPagination(t *testing.T) {
	sr := &SearchResult{Total: 42, Books: []BookDTO{}}
	pr := NewPresenterResult(sr, 3, 10)
	if pr.Pagination == nil {
		t.Fatalf("expected pagination")
	}
	if pr.Pagination.CurrentPage != 3 || pr.Pagination.TotalPages != 5 {
		t.Fatalf("unexpected pagination: %+v", pr.Pagination)
	}
}
