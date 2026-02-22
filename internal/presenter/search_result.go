package presenter

// BookDTO дублирует search.BookDTO для избежания циклического импорта
type BookDTO struct {
	ID          string   `json:"id"`
	Title       string   `json:"title"`
	Authors     []string `json:"authors"`
	Container   string   `json:"container"`
	Filename    string   `json:"filename"`
	FullAuthors string   `json:"full_authors"`
	DownloadURL string   `json:"download_url,omitempty"`
}

// SearchResult содержит агрегированный результат поиска (БЕЗ пагинации)
type SearchResult struct {
	TraceId string    `json:"trace_id"`
	Total   int       `json:"total"`
	Books   []BookDTO `json:"books"`
}

// PresenterResult представляет обогащённый результат поиска с пагинацией
type PresenterResult struct {
	*SearchResult
	Pagination *Pagination `json:"pagination"`
}

// Pagination содержит информацию для пагинации
type Pagination struct {
	CurrentPage int   `json:"current_page"`
	TotalPages  int   `json:"total_pages"`
	PageSize    int   `json:"page_size"`
	TotalItems  int   `json:"total_items"`
	HasPrev     bool  `json:"has_prev"`
	HasNext     bool  `json:"has_next"`
	Pages       []int `json:"pages"`
}

// NewPresenterResult создаёт общий результат с пагинацией
func NewPresenterResult(sr *SearchResult, page, pageSize int) *PresenterResult {
	return &PresenterResult{
		SearchResult: sr,
		Pagination:   NewPagination(sr.Total, page, pageSize),
	}
}

// NewPagination строит единый объект пагинации для всех транспортов.
func NewPagination(totalItems, page, pageSize int) *Pagination {
	if pageSize <= 0 {
		pageSize = 1
	}
	totalPages := (totalItems + pageSize - 1) / pageSize
	if totalPages == 0 {
		totalPages = 1
	}
	if page < 1 {
		page = 1
	}
	if page > totalPages {
		page = totalPages
	}
	return &Pagination{
		CurrentPage: page,
		TotalPages:  totalPages,
		PageSize:    pageSize,
		TotalItems:  totalItems,
		HasPrev:     page > 1,
		HasNext:     page < totalPages,
		Pages:       generatePageNumbers(page, totalPages),
	}
}

// generatePageNumbers генерирует номера страниц для пагинации
func generatePageNumbers(current, total int) []int {
	if total <= 7 {
		pages := make([]int, total)
		for i := 0; i < total; i++ {
			pages[i] = i + 1
		}
		return pages
	}

	var pages []int
	pages = append(pages, 1)

	start := current - 1
	end := current + 1

	// At boundaries show a wider set near the edge instead of empty gaps.
	if current <= 1 {
		start, end = 2, 3
	} else if current >= total {
		start, end = total-2, total-1
	}

	if start < 2 {
		start = 2
	}
	if end > total-1 {
		end = total - 1
	}

	if start > 2 {
		pages = append(pages, 0) // 0 означает "..."
	}
	for i := start; i <= end; i++ {
		pages = append(pages, i)
	}
	if end < total-1 {
		pages = append(pages, 0) // 0 означает "..."
	}

	if total > 1 {
		pages = append(pages, total)
	}

	return pages
}

// ConvertFromSearch преобразует search.SearchResult в presenter.SearchResult
func ConvertFromSearch(sr *SearchResult) *SearchResult {
	return sr
}
