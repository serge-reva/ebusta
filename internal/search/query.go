package search

import (
	"errors"
	"strings"
)

// Normalize приводит строку запроса к каноническому виду.
func Normalize(raw string) string {
	s := strings.TrimSpace(raw)
	if s == "" {
		return ""
	}
	return strings.Join(strings.Fields(s), " ")
}

func Validate(normalized string) error {
	if normalized == "" {
		return errors.New("empty search query")
	}
	return nil
}
