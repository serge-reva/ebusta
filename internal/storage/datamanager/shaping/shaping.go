package shaping

import (
	"encoding/json"
	"fmt"
)

// --- Search shaping ---
type searchHit struct {
	Source struct {
		Title   string   `json:"title"`
		Authors []string `json:"authors"`
		FileInfo struct {
			Container string `json:"container"`
			Filename  string `json:"filename"`
		} `json:"fileInfo"`
	} `json:"_source"`
}
type searchResp struct {
	Hits struct {
		Total struct{ Value int `json:"value"` } `json:"total"`
		Hits  []searchHit `json:"hits"`
	} `json:"hits"`
}

// ShapeSearch flattens OpenSearch hits into a smaller payload.
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
		Items:    make([]item, 0, len(r.Hits.Hits)), // ensure [] not null
	}
	for _, h := range r.Hits.Hits {
		dl := ""
		if h.Source.FileInfo.Container != "" && h.Source.FileInfo.Filename != "" {
			dl = h.Source.FileInfo.Container + "/" + h.Source.FileInfo.Filename
		}
		out.Items = append(out.Items, item{
			Title:    h.Source.Title,
			Authors:  h.Source.Authors,
			Download: dl,
		})
	}
	return json.MarshalIndent(out, "", "  ")
}

// --- Composite/aggregation shaping (best-effort generic) ---
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
	if len(r.Aggregations) == 0 {
		// pass-through
		return data, nil
	}
	// pick first aggregation
	for name, c := range r.Aggregations {
		out := map[string]any{
			"name":       name,
			"buckets":    c.Buckets,
			"after_key":  c.AfterKey,
		}
		return json.MarshalIndent(out, "", "  ")
	}
	return data, nil
}
