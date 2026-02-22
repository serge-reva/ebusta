package validation

import (
	"encoding/json"
	"fmt"
	"strings"

	"ebusta/internal/edge"
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
	return edge.ValidateSHA1(sha1)
}

func (v *Validator) ValidateToken(token string) bool {
	return edge.ValidateToken(token)
}
