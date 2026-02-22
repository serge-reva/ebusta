package main

import (
	"encoding/json"
	"testing"
)

func TestApplyPaginationDSL(t *testing.T) {
	in := `{"query":{"match_all":{}}}`
	out, err := applyPagination(in, 10, 20)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var m map[string]interface{}
	if err := json.Unmarshal([]byte(out), &m); err != nil {
		t.Fatalf("invalid json: %v", err)
	}

	if int(m["size"].(float64)) != 10 {
		t.Fatalf("size mismatch: %v", m["size"])
	}
	if int(m["from"].(float64)) != 20 {
		t.Fatalf("from mismatch: %v", m["from"])
	}
}

func TestApplyPaginationTemplate(t *testing.T) {
	in := `{"id":"fl_smart_general","params":{"query":"author:king"}}`
	out, err := applyPagination(in, 5, 15)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var m map[string]interface{}
	if err := json.Unmarshal([]byte(out), &m); err != nil {
		t.Fatalf("invalid json: %v", err)
	}

	params, ok := m["params"].(map[string]interface{})
	if !ok {
		t.Fatalf("params is not an object: %T", m["params"])
	}
	if int(params["size"].(float64)) != 5 {
		t.Fatalf("size mismatch: %v", params["size"])
	}
	if int(params["from"].(float64)) != 15 {
		t.Fatalf("from mismatch: %v", params["from"])
	}
}

func TestBuildSearchStatus(t *testing.T) {
	tests := []struct {
		name     string
		query    string
		execType string
		want     string
	}{
		{
			name:     "template broad",
			query:    "author:булгаков",
			execType: "TEMPLATE",
			want:     "ok;exec=TEMPLATE;match=broad",
		},
		{
			name:     "template exact quoted",
			query:    `author:"михаил булгаков"`,
			execType: "TEMPLATE",
			want:     "ok;exec=TEMPLATE;match=exact",
		},
		{
			name:     "id exact",
			query:    "id:2fb481cc13771f6485091893858808e51a7718ff",
			execType: "TEMPLATE",
			want:     "ok;exec=TEMPLATE;match=exact",
		},
		{
			name:     "dsl broad",
			query:    "author:king AND title:it",
			execType: "DSL",
			want:     "ok;exec=DSL;match=broad",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := buildSearchStatus(tt.query, tt.execType)
			if got != tt.want {
				t.Fatalf("buildSearchStatus() = %q, want %q", got, tt.want)
			}
		})
	}
}
