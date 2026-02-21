package presenter

import (
    "fmt"
    "io"
    "strings"
)

// TextFormatter форматирует результаты в текстовый табличный вид (для CLI)
type TextFormatter struct{}

func (f *TextFormatter) Format(result *PresenterResult, w io.Writer) error {
    if result.Total == 0 {
        fmt.Fprintln(w, "No results found.")
        return nil
    }

    fmt.Fprintf(w, "\nFound %d books:\n", result.Total)
    fmt.Fprintf(w, "%-40s | %-32s | %-20s | %s\n", "ID", "Title", "Authors", "File")
    fmt.Fprintln(w, strings.Repeat("-", 120))

    for _, b := range result.Books {
        // Обрезаем длинные строки для читаемости
        title := b.Title
        if len(title) > 32 {
            title = title[:29] + "..."
        }
        
        authors := b.FullAuthors
        if len(authors) > 20 {
            authors = authors[:17] + "..."
        }

        fmt.Fprintf(w, "%-40s | %-32s | %-20s | %s/%s\n",
            b.ID, title, authors, b.Container, b.Filename)
    }
    
    if result.Pagination != nil && result.Pagination.TotalPages > 1 {
        fmt.Fprintf(w, "\nPage %d of %d", result.Pagination.CurrentPage, result.Pagination.TotalPages)
        
        // Показываем навигацию
        if result.Pagination.HasPrev {
            fmt.Fprintf(w, " (use --page=%d for previous)", result.Pagination.CurrentPage-1)
        }
        if result.Pagination.HasNext {
            fmt.Fprintf(w, " (use --page=%d for next)", result.Pagination.CurrentPage+1)
        }
        fmt.Fprintln(w)
    }
    
    return nil
}
