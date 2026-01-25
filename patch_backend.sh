#!/bin/bash
set -e

echo "🛠 1. Обновление internal/storage/datamanager/delivery/handlers.go..."
cat << 'INNER_EOF' > internal/storage/datamanager/delivery/handlers.go
package delivery

import (
	"ebusta/api/proto/v1"
	"encoding/json"
	"log"
)

func MapOSResponseToGrpc(body []byte) ([]*libraryv1.Book, int32) {
	var raw struct {
		Hits struct {
			Total interface{} `json:"total"`
			Hits  []struct {
				InnerHits struct {
					Best struct {
						Hits struct {
							Hits []struct {
								ID     string `json:"_id"`
								Source struct {
									Title     string   `json:"title"`
									Authors   []string `json:"authors"`
									FileInfo  struct {
										Container string `json:"container"`
										Filename  string `json:"filename"`
									} `json:"fileInfo"`
								} `json:"_source"`
							} `json:"hits"`
						} `json:"hits"`
					} `json:"best"`
				} `json:"inner_hits"`
			} `json:"hits"`
		} `json:"hits"`
	}

	if err := json.Unmarshal(body, &raw); err != nil {
		log.Printf("❌ DataManager Parsing Error: %v", err)
		return nil, 0
	}

	var totalValue int32
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
		if len(h.InnerHits.Best.Hits.Hits) > 0 {
			best := h.InnerHits.Best.Hits.Hits[0]
			authors := best.Source.Authors
			if authors == nil {
				authors = []string{"Unknown"}
			}
			books = append(books, &libraryv1.Book{
				Id:        best.ID,
				Title:     best.Source.Title,
				Authors:   authors,
				Container: best.Source.FileInfo.Container,
				Filename:  best.Source.FileInfo.Filename,
			})
		}
	}

	if totalValue == 0 && len(books) > 0 {
		totalValue = int32(len(books))
	}
	return books, totalValue
}
INNER_EOF

echo "🛠 2. Обновление cmd/datamanager/main.go..."
# Здесь мы правим только структуру парсинга внутри SearchBooks
sed -i '/var osRaw struct {/,/}/c\	var osRaw struct {\n		Hits struct {\n			Total interface{} `json:"total"`\n			Hits  []struct {\n				InnerHits struct {\n					Best struct {\n						Hits struct {\n							Hits []struct {\n								ID     string `json:"_id"`\n								Source struct {\n									Title   string   `json:"title"`\n									Authors []string `json:"authors"`\n								} `json:"_source"`\n							} `json:"hits"`\n						} `json:"hits"`\n					} `json:"best"`\n				} `json:"inner_hits"`\n			} `json:"hits"`\n		} `json:"hits"`\n	}' cmd/datamanager/main.go

# Обновляем цикл сборки результатов в cmd/datamanager/main.go
sed -i '/for _, hit := range osRaw.Hits.Hits {/,/}/c\	for _, hit := range osRaw.Hits.Hits {\n		if len(hit.InnerHits.Best.Hits.Hits) > 0 {\n			best := hit.InnerHits.Best.Hits.Hits[0]\n			res.Books = append(res.Books, &libraryv1.Book{\n				Id:      best.ID,\n				Title:   best.Source.Title,\n				Authors: best.Source.Authors,\n			})\n		}\n	}' cmd/datamanager/main.go

echo "🛠 3. Обновление internal/storage/datamanager/shaping/shaping.go..."
cat << 'INNER_EOF' > internal/storage/datamanager/shaping/shaping.go
package shaping

import (
	"encoding/json"
	"fmt"
)

type searchResp struct {
	Hits struct {
		Total struct {
			Value int `json:"value"`
		} `json:"total"`
		Hits []struct {
			InnerHits struct {
				Best struct {
					Hits struct {
						Hits []struct {
							Source struct {
								Title    string   `json:"title"`
								Authors  []string `json:"authors"`
								FileInfo struct {
									Container string `json:"container"`
									Filename  string `json:"filename"`
								} `json:"fileInfo"`
							} `json:"_source"`
						} `json:"hits"`
					} `json:"hits"`
				} `json:"best"`
			} `json:"inner_hits"`
		} `json:"hits"`
	} `json:"hits"`
}

func ShapeSearch(data []byte, from, size int) ([]byte, error) {
	var r searchResp
	if err := json.Unmarshal(data, &r); err != nil {
		return nil, fmt.Errorf("decode hits: %w", err)
	}
	type item struct {
		Title    string   `json:"title"`
		Authors  []string `json:"authors"`
		Download string   `json:"download,omitempty"`
	}
	out := struct {
		Total    int    `json:"total"`
		From     int    `json:"from"`
		Size     int    `json:"size"`
		NextFrom int    `json:"next_from"`
		Items    []item `json:"items"`
	}{
		Total:    r.Hits.Total.Value,
		From:     from,
		Size:     size,
		NextFrom: from + size,
		Items:    make([]item, 0, len(r.Hits.Hits)),
	}
	for _, h := range r.Hits.Hits {
		if len(h.InnerHits.Best.Hits.Hits) > 0 {
			best := h.InnerHits.Best.Hits.Hits[0]
			dl := ""
			if best.Source.FileInfo.Container != "" && best.Source.FileInfo.Filename != "" {
				dl = best.Source.FileInfo.Container + "/" + best.Source.FileInfo.Filename
			}
			out.Items = append(out.Items, item{
				Title:    best.Source.Title,
				Authors:  best.Source.Authors,
				Download: dl,
			})
		}
	}
	return json.MarshalIndent(out, "", "  ")
}

// Composite/aggregation shaping (оставляем без изменений)
type composite struct {
	AfterKey any `json:"after_key"`
	Buckets  any `json:"buckets"`
}
type aggResp struct {
	Aggregations map[string]composite `json:"aggregations"`
}

func ShapeComposite(data []byte) ([]byte, error) {
	var r aggResp
	if err := json.Unmarshal(data, &r); err != nil {
		return nil, fmt.Errorf("decode aggregations: %w", err)
	}
	if len(r.Aggregations) == 0 { return data, nil }
	for name, c := range r.Aggregations {
		out := map[string]any{
			"name":      name,
			"buckets":   c.Buckets,
			"after_key": c.AfterKey,
		}
		return json.MarshalIndent(out, "", "  ")
	}
	return data, nil
}
INNER_EOF

echo "✅ Все файлы обновлены. Запускаю сборку и тесты..."
