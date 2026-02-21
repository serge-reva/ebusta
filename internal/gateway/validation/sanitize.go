package validation

import (
    "html"
    "regexp"
    "strings"

    "github.com/microcosm-cc/bluemonday"
)

type Sanitizer struct {
    htmlPolicy *bluemonday.Policy
    sqlPattern *regexp.Regexp
    cmdPattern *regexp.Regexp
}

func NewSanitizer() *Sanitizer {
    // HTML политика: разрешаем только безопасные теги
    p := bluemonday.UGCPolicy()
    
    // SQL injection паттерны
    sqlPattern := regexp.MustCompile(`(?i)(\bSELECT\b|\bINSERT\b|\bUPDATE\b|\bDELETE\b|\bDROP\b|\bUNION\b|--|;)`)
    
    // Command injection паттерны
    cmdPattern := regexp.MustCompile(`[;&|` + "`" + `$(){}\[\]<>]`)
    
    return &Sanitizer{
        htmlPolicy: p,
        sqlPattern: sqlPattern,
        cmdPattern: cmdPattern,
    }
}

func (s *Sanitizer) Sanitize(input string) string {
    if input == "" {
        return ""
    }
    
    // HTML экранирование
    safe := s.htmlPolicy.Sanitize(input)
    
    // Дополнительное экранирование для HTML
    safe = html.EscapeString(safe)
    
    return safe
}

func (s *Sanitizer) IsSQLSafe(input string) bool {
    return !s.sqlPattern.MatchString(input)
}

func (s *Sanitizer) IsCmdSafe(input string) bool {
    return !s.cmdPattern.MatchString(input)
}

func (s *Sanitizer) SanitizeForLog(input string) string {
    // Удаляем конфиденциальные данные из логов
    input = strings.ReplaceAll(input, "\n", "\\n")
    input = strings.ReplaceAll(input, "\r", "\\r")
    input = strings.ReplaceAll(input, "\t", "\\t")
    
    // Ограничиваем длину
    if len(input) > 200 {
        input = input[:200] + "..."
    }
    
    return input
}
