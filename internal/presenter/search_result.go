package presenter

// BookDTO дублирует search.BookDTO для избежания циклического импорта
type BookDTO struct {
    ID          string
    Title       string
    Authors     []string
    Container   string
    Filename    string
    FullAuthors string
}

// SearchResult содержит агрегированный результат поиска (БЕЗ пагинации)
type SearchResult struct {
    TraceId string
    Total   int
    Books   []BookDTO
}

// PresenterResult представляет обогащённый результат поиска с пагинацией
type PresenterResult struct {
    *SearchResult
    Pagination *Pagination
}

// Pagination содержит информацию для пагинации
type Pagination struct {
    CurrentPage int
    TotalPages  int
    PageSize    int
    TotalItems  int
    HasPrev     bool
    HasNext     bool
    Pages       []int // номера страниц для отображения (0 означает "...")
}

// NewPresenterResult создаёт общий результат с пагинацией
func NewPresenterResult(sr *SearchResult, page, pageSize int) *PresenterResult {
    totalPages := (sr.Total + pageSize - 1) / pageSize
    if totalPages == 0 {
        totalPages = 1
    }

    return &PresenterResult{
        SearchResult: sr,
        Pagination: &Pagination{
            CurrentPage: page,
            TotalPages:  totalPages,
            PageSize:    pageSize,
            TotalItems:  sr.Total,
            HasPrev:     page > 1,
            HasNext:     page < totalPages,
            Pages:       generatePageNumbers(page, totalPages),
        },
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

    if current > 3 {
        pages = append(pages, 0) // 0 означает "..."
    }

    for i := current - 1; i <= current+1; i++ {
        if i > 1 && i < total {
            pages = append(pages, i)
        }
    }

    if current < total-2 {
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
