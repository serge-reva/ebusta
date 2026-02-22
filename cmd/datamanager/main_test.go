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
