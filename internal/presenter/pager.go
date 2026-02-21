package presenter

import (
    "fmt"
    "strings"
)

// Page представляет страницу результатов
type Page struct {
    Items       []string
    CurrentPage int
    TotalPages  int
    TotalItems  int
    PageSize    int
    HasPrev     bool
    HasNext     bool
    FirstIndex  int
    LastIndex   int
}

// Pager управляет пагинацией для разных форматов вывода
type Pager struct {
    pageSize int
}

func NewPager(pageSize int) *Pager {
    return &Pager{
        pageSize: pageSize,
    }
}

// CreatePage создаёт страницу из списка элементов
func (p *Pager) CreatePage(items []string, page int) *Page {
    total := len(items)
    totalPages := (total + p.pageSize - 1) / p.pageSize
    if totalPages == 0 {
        totalPages = 1
    }
    
    if page < 1 {
        page = 1
    }
    if page > totalPages {
        page = totalPages
    }
    
    start := (page - 1) * p.pageSize
    end := start + p.pageSize
    if end > total {
        end = total
    }
    
    pageItems := items[start:end]
    
    return &Page{
        Items:       pageItems,
        CurrentPage: page,
        TotalPages:  totalPages,
        TotalItems:  total,
        PageSize:    p.pageSize,
        HasPrev:     page > 1,
        HasNext:     page < totalPages,
        FirstIndex:  start + 1,
        LastIndex:   end,
    }
}

// FormatPageNumbers форматирует номера страниц для навигации
func FormatPageNumbers(current, total int) string {
    if total <= 1 {
        return ""
    }
    
    var parts []string
    
    // Всегда показываем первую страницу
    parts = append(parts, formatPageLink(1, current))
    
    if current > 3 {
        parts = append(parts, "...")
    }
    
    // Страницы вокруг текущей
    for i := current - 1; i <= current+1; i++ {
        if i > 1 && i < total {
            parts = append(parts, formatPageLink(i, current))
        }
    }
    
    if current < total-2 {
        parts = append(parts, "...")
    }
    
    // Последняя страница
    if total > 1 {
        parts = append(parts, formatPageLink(total, current))
    }
    
    return strings.Join(parts, " ")
}

func formatPageLink(page, current int) string {
    if page == current {
        return fmt.Sprintf("[%d]", page)
    }
    return fmt.Sprintf("%d", page)
}

// FormatPageNav форматирует навигацию для разных форматов
func (p *Page) FormatNav(prefix string) string {
    var nav []string
    
    if p.HasPrev {
        nav = append(nav, fmt.Sprintf("%s%d", prefix, p.CurrentPage-1))
    }
    
    if p.TotalPages > 1 {
        pageInfo := fmt.Sprintf("page %d/%d", p.CurrentPage, p.TotalPages)
        if p.TotalItems > 0 {
            pageInfo = fmt.Sprintf("%s (%d-%d of %d)", 
                pageInfo, p.FirstIndex, p.LastIndex, p.TotalItems)
        }
        nav = append(nav, pageInfo)
    }
    
    if p.HasNext {
        nav = append(nav, fmt.Sprintf("%s%d", prefix, p.CurrentPage+1))
    }
    
    return strings.Join(nav, " • ")
}
