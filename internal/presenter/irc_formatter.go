package presenter

import (
	"fmt"
	"strings"
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

	pg := result.Pagination
	if pg == nil {
		pg = NewPagination(result.Total, page, len(result.Books))
	}

	var output []string
	output = append(output,
		fmt.Sprintf("📚 Found %d books • page %d/%d • limit %d",
			result.Total, pg.CurrentPage, pg.TotalPages, pg.PageSize))

	for i, book := range result.Books {
		authors := book.FullAuthors
		if authors == "" {
			authors = "Unknown Author"
		}
		title := book.Title
		if title == "" {
			title = "Unknown Title"
		}
		// Numbering inside current page.
		output = append(output, fmt.Sprintf("%d. \"%s\" by %s", i+1, title, authors))
	}

	nav := []string{}
	if pg.HasPrev {
		nav = append(nav, "!prev")
	}
	if pg.HasNext {
		nav = append(nav, "!next")
	}
	if pg.TotalPages > 1 {
		nav = append(nav, "!page <n>")
	}
	if len(nav) > 0 {
		output = append(output, "🔍 Navigation: "+strings.Join(nav, " | "))
	}
	if len(result.Books) > 0 {
		output = append(output, "📥 Use !get <number> to show book information")
	}

	return output, &Page{
		Items:       nil,
		CurrentPage: pg.CurrentPage,
		TotalPages:  pg.TotalPages,
		TotalItems:  pg.TotalItems,
		PageSize:    pg.PageSize,
		HasPrev:     pg.HasPrev,
		HasNext:     pg.HasNext,
	}
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
		"!page <n>                - Go to page N of last search",
		"!next / !prev            - Navigate search pages",
		"!lines <n>               - Set books per page and reset to page 1",
		"!get <number>            - Show book information",
		"",
		"📝 Examples:",
		"  !search author:king",
		"  !page 2",
		"  !next",
		"  !lines 10",
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
