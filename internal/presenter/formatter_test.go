package presenter

import (
    "bytes"
    "strings"
    "testing"
)

func TestTextFormatter(t *testing.T) {
    tests := []struct {
        name     string
        result   *PresenterResult
        contains []string
    }{
        {
            name: "empty result",
            result: NewPresenterResult(&SearchResult{
                Total: 0,
                Books: []BookDTO{},
            }, 1, 10),
            contains: []string{"No results found"},
        },
        {
            name: "single result",
            result: NewPresenterResult(&SearchResult{
                Total: 1,
                Books: []BookDTO{
                    {
                        ID:          "123",
                        Title:       "Test Book",
                        FullAuthors: "Test Author",
                        Container:   "test.zip",
                        Filename:    "test.fb2",
                    },
                },
            }, 1, 10),
            contains: []string{"Found 1 books", "123", "Test Book", "Test Author"},
        },
        {
            name: "with pagination",
            result: NewPresenterResult(&SearchResult{
                Total: 25,
                Books: make([]BookDTO, 10),
            }, 2, 10),
            contains: []string{"Page 2 of 3", "previous", "next"},
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            formatter := &TextFormatter{}
            buf := &bytes.Buffer{}
            
            err := formatter.Format(tt.result, buf)
            if err != nil {
                t.Fatalf("Format error: %v", err)
            }
            
            output := buf.String()
            for _, s := range tt.contains {
                if !strings.Contains(output, s) {
                    t.Errorf("Expected output to contain %q, got:\n%s", s, output)
                }
            }
        })
    }
}

func TestPaginationGeneration(t *testing.T) {
    tests := []struct {
        current, total int
        expected       []int
    }{
        {1, 1, []int{1}},
        {1, 5, []int{1, 2, 3, 4, 5}},
        {5, 10, []int{1, 0, 4, 5, 6, 0, 10}},
        {1, 10, []int{1, 2, 3, 0, 10}},
        {10, 10, []int{1, 0, 8, 9, 10}},
    }

    for _, tt := range tests {
        t.Run("", func(t *testing.T) {
            result := generatePageNumbers(tt.current, tt.total)
            if !equalSlices(result, tt.expected) {
                t.Errorf("generatePageNumbers(%d, %d) = %v, want %v", 
                    tt.current, tt.total, result, tt.expected)
            }
        })
    }
}

func equalSlices(a, b []int) bool {
    if len(a) != len(b) {
        return false
    }
    for i := range a {
        if a[i] != b[i] {
            return false
        }
    }
    return true
}
