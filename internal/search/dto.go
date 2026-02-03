package search

// BookDTO представляет данные книги, оптимизированные для отображения
type BookDTO struct {
	ID          string
	Title       string
	Authors     []string
	Container   string
	Filename    string
	FullAuthors string // Склеенная строка авторов для простого рендеринга
}

// SearchResult содержит агрегированный результат поиска
type SearchResult struct {
	Total int
	Books []BookDTO
}
