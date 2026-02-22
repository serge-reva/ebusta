package presenter

import (
    "fmt"
)

// IRCFormatter форматирует результаты для IRC
type IRCFormatter struct {
    pager *Pager
}

func NewIRCFormatter(pageSize int) *IRCFormatter {
    return &IRCFormatter{
        pager: NewPager(pageSize),
    }
}

// FormatSearchResult форматирует результаты поиска для IRC
func (f *IRCFormatter) FormatSearchResult(result *PresenterResult, page int) ([]string, *Page) {
    if result.Total == 0 {
        return []string{"📚 No books found"}, nil
    }
    
    // Подготавливаем элементы для пагинации
    items := make([]string, len(result.Books))
    for i, book := range result.Books {
        authors := book.FullAuthors
        if authors == "" {
            authors = "Unknown Author"
        }
        
        title := book.Title
        if title == "" {
            title = "Unknown Title"
        }
        
        // Убираем обрезание, показываем полностью
        items[i] = fmt.Sprintf("%d. \"%s\" by %s", i+1, title, authors)
    }
    
    // Создаём страницу
    pageData := f.pager.CreatePage(items, page)
    
    // Формируем вывод
    var output []string
    
    // Заголовок
    output = append(output, fmt.Sprintf("📚 Found %d books:", result.Total))
    
    // Элементы страницы
    output = append(output, pageData.Items...)
    
    // Навигация
    if nav := pageData.FormatNav("!search page "); nav != "" {
        output = append(output, "🔍 "+nav)
    }
    
    // Подсказка по скачиванию
    if len(pageData.Items) > 0 {
        output = append(output, "📥 Use !get <number> to download")
    }
    
    return output, pageData
}

// FormatBookInfo форматирует информацию о книге для IRC
func (f *IRCFormatter) FormatBookInfo(book *BookDTO, downloadURL string) []string {
    var output []string
    
    title := book.Title
    if title == "" {
        title = "Unknown Title"
    }
    output = append(output, fmt.Sprintf("📖 %s", title))
    
    authors := book.FullAuthors
    if authors == "" {
        authors = "Unknown Author"
    }
    output = append(output, fmt.Sprintf("👤 %s", authors))
    
    if book.Container != "" && book.Filename != "" {
        output = append(output, fmt.Sprintf("📁 %s/%s", book.Container, book.Filename))
    }
    
    if downloadURL != "" {
        output = append(output, fmt.Sprintf("📥 Download: %s", downloadURL))
    }
    
    return output
}

// FormatError форматирует ошибку для IRC
func (f *IRCFormatter) FormatError(err error) []string {
    return []string{fmt.Sprintf("❌ Error: %v", err)}
}

// FormatHelp форматирует справку
func (f *IRCFormatter) FormatHelp() []string {
    return []string{
        "🤖 Ebusta Book Search Bot - Commands:",
        "!help                    - Show this help",
        "!search <query>          - Search books (e.g., !search author:king)",
        "!search page <n>         - Go to page N of last search",
        "!get <number>            - Show book information",
        "",
        "📝 Examples:",
        "  !search author:king",
        "  !search title:hobbit",
        "  !search id:bd6525...",
        "  !get 1",
    }
}

// FormatStats форматирует статистику
func (f *IRCFormatter) FormatStats(stats map[string]interface{}) []string {
    var output []string
    output = append(output, "📊 Ebusta Bot Statistics:")
    
    for k, v := range stats {
        output = append(output, fmt.Sprintf("  • %s: %v", k, v))
    }
    
    return output
}
