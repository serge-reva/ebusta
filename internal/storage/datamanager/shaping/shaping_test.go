package shaping

import "testing"

func TestShapeSearch(t *testing.T) {
	jsonIn := []byte(`{"hits":{"total":{"value":2},"hits":[{"_source":{"title":"A","authors":["X"],"fileInfo":{"container":"c","filename":"a.fb2"}}},{"_source":{"title":"B","authors":["Y"],"fileInfo":{"container":"d","filename":"b.fb2"}}}]}}`)
	out, err := ShapeSearch(jsonIn, 0, 10)
	if err != nil { t.Fatalf("unexpected error: %v", err) }
	mustContain(t, string(out), `"total": 2`)
	mustContain(t, string(out), `"items": [`)
	mustContain(t, string(out), `"download": "c/a.fb2"`)
}

func mustContain(t *testing.T, s, sub string) {
	t.Helper()
	if !contains(s, sub) { t.Fatalf("expected substring %q in %s", sub, s) }
}

func contains(s, sub string) bool { return len(s) >= len(sub) && (s == sub || (len(sub) > 0 && (stringIndex(s, sub) >= 0))) }
func stringIndex(s, sub string) int {
	for i := 0; i+len(sub) <= len(s); i++ {
		if s[i:i+len(sub)] == sub { return i }
	}
	return -1
}
