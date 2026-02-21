package validation

import (
    "encoding/json"
    "fmt"
    "regexp"
    "strings"

    "github.com/xeipuuv/gojsonschema"
)

var (
    searchSchema = `{
        "type": "object",
        "properties": {
            "query": {
                "type": "string",
                "minLength": 1,
                "maxLength": 500,
                "pattern": "^[a-zA-Zа-яА-Я0-9\\s:\\\"\\-]+$"
            },
            "page": {
                "type": "integer",
                "minimum": 1,
                "maximum": 1000
            },
            "limit": {
                "type": "integer",
                "minimum": 1,
                "maximum": 100
            }
        },
        "required": ["query"]
    }`

    sha1Regex = regexp.MustCompile(`^[a-f0-9]{40}$`)
)

type Validator struct {
    searchLoader gojsonschema.JSONLoader
}

func NewValidator() *Validator {
    return &Validator{
        searchLoader: gojsonschema.NewStringLoader(searchSchema),
    }
}

type SearchRequest struct {
    Query string `json:"query"`
    Page  int    `json:"page,omitempty"`
    Limit int    `json:"limit,omitempty"`
}

func (v *Validator) ValidateSearch(body []byte) (*SearchRequest, error) {
    // Базовый синтаксис JSON
    var req SearchRequest
    if err := json.Unmarshal(body, &req); err != nil {
        return nil, fmt.Errorf("invalid JSON: %w", err)
    }
    
    // Валидация по схеме
    documentLoader := gojsonschema.NewBytesLoader(body)
    result, err := gojsonschema.Validate(v.searchLoader, documentLoader)
    if err != nil {
        return nil, fmt.Errorf("schema validation failed: %w", err)
    }
    
    if !result.Valid() {
        var errors []string
        for _, desc := range result.Errors() {
            errors = append(errors, desc.String())
        }
        return nil, fmt.Errorf("validation failed: %s", strings.Join(errors, "; "))
    }
    
    // Нормализация
    req.Query = strings.TrimSpace(req.Query)
    
    return &req, nil
}

func (v *Validator) ValidateSHA1(sha1 string) bool {
    return sha1Regex.MatchString(sha1)
}

func (v *Validator) ValidateToken(token string) bool {
    // Токены генерируются как base64.RawURLEncoding.EncodeToString (32 байта)
    // Длина может быть 43 символа (стандартно) или короче (без padding)
    if len(token) < 32 || len(token) > 64 {
        return false
    }
    
    // Проверяем, что токен содержит только допустимые символы base64 URL
    for _, c := range token {
        if !((c >= 'a' && c <= 'z') || 
             (c >= 'A' && c <= 'Z') || 
             (c >= '0' && c <= '9') || 
             c == '-' || c == '_') {
            return false
        }
    }
    return true
}
