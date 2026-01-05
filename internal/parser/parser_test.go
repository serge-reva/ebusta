package parser

import (
	"ebusta/api/proto/v1"
	"testing"
)

func TestParser(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		validate func(*testing.T, *libraryv1.SearchQuery)
	}{
		{
			name:  "Simple Any Search",
			input: "Unix",
			validate: func(t *testing.T, q *libraryv1.SearchQuery) {
				f := q.GetFilter()
				if f == nil || f.Field != "any" || f.Value != "Unix" {
					t.Errorf("Expected any:Unix, got %+v", f)
				}
			},
		},
		{
			name:  "Field Search",
			input: "author:Кинг",
			validate: func(t *testing.T, q *libraryv1.SearchQuery) {
				f := q.GetFilter()
				if f == nil || f.Field != "author" || f.Value != "Кинг" {
					t.Errorf("Expected author:Кинг, got %+v", f)
				}
			},
		},
		{
			name:  "Logical AND",
			input: "author:Кинг AND title:Оно",
			validate: func(t *testing.T, q *libraryv1.SearchQuery) {
				l := q.GetLogical()
				if l == nil || l.Op != libraryv1.LogicalOp_AND || len(l.Nodes) != 2 {
					t.Errorf("Expected AND with 2 nodes, got %+v", l)
				}
			},
		},
		{
			name:  "Negation NOT",
			input: "NOT title:Куджо",
			validate: func(t *testing.T, q *libraryv1.SearchQuery) {
				n := q.GetNegation()
				if n == nil {
					t.Fatalf("Expected negation node, got nil")
				}
				f := n.Node.GetFilter()
				if f.Field != "title" || f.Value != "Куджо" {
					t.Errorf("Expected NOT title:Куджо, got %s:%s", f.Field, f.Value)
				}
			},
		},
		{
			name:  "Regex Detection",
			input: "title:/^Unix.*/",
			validate: func(t *testing.T, q *libraryv1.SearchQuery) {
				f := q.GetFilter()
				if f.Operator != libraryv1.Operator_OP_REGEX {
					t.Errorf("Expected REGEX operator, got %v", f.Operator)
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := NewParser(tt.input)
			query := p.Parse()
			tt.validate(t, query)
		})
	}
}
