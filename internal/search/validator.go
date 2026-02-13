package search

import (
        "errors"
        "strings"
)

var (
        ErrEmptyQuery   = errors.New("запрос не может быть пустым")
        ErrQueryTooLong = errors.New("запрос слишком длинный (макс. 500 символов)")
)

// ValidateQuery проверяет поисковый запрос
func ValidateQuery(query string) error {
        if strings.TrimSpace(query) == "" {
                return ErrEmptyQuery
        }
        if len(query) > 500 {
                return ErrQueryTooLong
        }
        return nil
}

// ValidateSHA1 проверяет формат SHA1-хеша (40 hex-символов)
func ValidateSHA1(sha1 string) bool {
        if len(sha1) != 40 {
                return false
        }
        for _, c := range sha1 {
                if !isHexDigit(c) {
                        return false
                }
        }
        return true
}

func isHexDigit(c rune) bool {
        return (c >= '0' && c <= '9') ||
                (c >= 'a' && c <= 'f') ||
                (c >= 'A' && c <= 'F')
}

// ValidatePage проверяет и нормализует номер страницы
func ValidatePage(page, totalPages int) int {
        if page < 1 {
                return 1
        }
        if totalPages > 0 && page > totalPages {
                return totalPages
        }
        return page
}
