package botcommand

import (
	"strconv"
	"strings"
)

func Parse(input string) Command {
	trimmed := strings.TrimSpace(input)
	if strings.HasPrefix(strings.ToLower(trimmed), "/start book_") {
		value := strings.TrimSpace(strings.TrimPrefix(trimmed, "/start"))
		value = strings.TrimSpace(strings.TrimPrefix(value, "book_"))
		bookIndex, err := strconv.Atoi(value)
		if err != nil || bookIndex <= 0 {
			return InvalidCommand{Reason: "invalid book index"}
		}
		return SelectBookCommand{BookIndex: bookIndex}
	}

	parts := strings.Fields(trimmed)
	if len(parts) == 0 {
		return InvalidCommand{Reason: "empty command"}
	}

	switch strings.ToLower(parts[0]) {
	case "/help", "/start":
		return HelpCommand{}
	case "/next":
		return NextCommand{}
	case "/prev":
		return PrevCommand{}
	case "/page":
		if len(parts) != 2 {
			return InvalidCommand{Reason: "usage: /page <n>"}
		}
		page, err := strconv.Atoi(parts[1])
		if err != nil || page <= 0 {
			return InvalidCommand{Reason: "invalid page"}
		}
		return PageCommand{Page: page}
	case "/search":
		if len(parts) < 2 {
			return InvalidCommand{Reason: "usage: /search <query> [page <n>]"}
		}
		page := 1
		queryParts := parts[1:]
		if len(queryParts) >= 3 && strings.EqualFold(queryParts[len(queryParts)-2], "page") {
			p, err := strconv.Atoi(queryParts[len(queryParts)-1])
			if err != nil || p <= 0 {
				return InvalidCommand{Reason: "invalid page"}
			}
			page = p
			queryParts = queryParts[:len(queryParts)-2]
		}
		query := strings.TrimSpace(strings.Join(queryParts, " "))
		if query == "" {
			return InvalidCommand{Reason: "empty query"}
		}
		return SearchCommand{Query: query, Page: page}
	default:
		return InvalidCommand{Reason: "unknown command"}
	}
}
