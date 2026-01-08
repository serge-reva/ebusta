package delivery

import (
	"ebusta/api/proto/v1"
	"encoding/json"
	"log"
)

func MapOSResponseToGrpc(body []byte) ([]*libraryv1.Book, int32) {
	// Total выносим в interface{}, так как OS может вернуть и число, и объект
	var raw struct {
		Hits struct {
			Total interface{} `json:"total"`
			Hits  []struct {
				ID     string `json:"_id"`
				Source struct {
					Title   string   `json:"title"`
					Authors []string `json:"authors"`
				} `json:"_source"`
			} `json:"hits"`
		} `json:"hits"`
	}

	if err := json.Unmarshal(body, &raw); err != nil {
		log.Printf("❌ DataManager Parsing Error: %v", err)
		return nil, 0
	}

	var totalValue int32
	// Гибкое извлечение Total (поддержка объекта и числа)
	switch v := raw.Hits.Total.(type) {
	case float64:
		totalValue = int32(v)
	case map[string]interface{}:
		if val, ok := v["value"].(float64); ok {
			totalValue = int32(val)
		}
	}

	var books []*libraryv1.Book
	for _, h := range raw.Hits.Hits {
		// Защита от пустых авторов
		authors := h.Source.Authors
		if authors == nil {
			authors = []string{"Unknown"}
		}
		
		books = append(books, &libraryv1.Book{
			Id:      h.ID,
			Title:   h.Source.Title,
			Authors: authors,
		})
	}

	// Если хиты есть, а total 0 (бывает при определенных настройках OS)
	if totalValue == 0 && len(books) > 0 {
		totalValue = int32(len(books))
	}

	return books, totalValue
}
