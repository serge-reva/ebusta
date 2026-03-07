package lokiclient

import (
	"context"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

func TestQueryTraceBuildsQueryAndParsesEntries(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if got := r.URL.Query().Get("query"); got != `{compose_project="ebusta"} |= "gw-123"` {
			t.Fatalf("unexpected query: %s", got)
		}
		_, _ = w.Write([]byte(`{
			"status":"success",
			"data":{"result":[
				{"stream":{"compose_service":"gateway"},"values":[["1700000000000000000","2026-03-07 10:00:00.000 [INFO] [gw-123] [gateway] request accepted"]]},
				{"stream":{"compose_service":"orchestrator"},"values":[["1700000001000000000","2026-03-07 10:00:01.000 [ERRO] [gw-123] [orchestrator] failed lookup error=boom"]]}
			]}
		}`))
	}))
	defer server.Close()

	client := New(server.URL)
	entries, err := client.QueryTrace(context.Background(), "gw-123")
	if err != nil {
		t.Fatalf("QueryTrace() error = %v", err)
	}
	if len(entries) != 2 {
		t.Fatalf("expected 2 entries, got %d", len(entries))
	}
	if entries[0].Service != "gateway" || entries[0].Level != "INFO" {
		t.Fatalf("unexpected first entry: %+v", entries[0])
	}
	if entries[1].Service != "orchestrator" || entries[1].Error != "boom" {
		t.Fatalf("unexpected second entry: %+v", entries[1])
	}
}

func TestQueryTraceRejectsEmptyTraceID(t *testing.T) {
	client := New("http://example")
	if _, err := client.QueryTrace(context.Background(), " "); err == nil {
		t.Fatal("expected error for empty trace id")
	}
}

func TestParseLogLineKeepsRawWhenPatternDoesNotMatch(t *testing.T) {
	entry := parseLogLine("plain line")
	if entry.Message != "plain line" {
		t.Fatalf("unexpected entry: %+v", entry)
	}
}

func TestQueryTraceSupportsCustomSelector(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if got := r.URL.Query().Get("query"); !strings.Contains(got, `{compose_service="gateway"}`) {
			t.Fatalf("unexpected query: %s", got)
		}
		_, _ = w.Write([]byte(`{"status":"success","data":{"result":[]}}`))
	}))
	defer server.Close()

	client := New(server.URL, WithSelector(`{compose_service="gateway"}`))
	if _, err := client.QueryTrace(context.Background(), "gw-1"); err != nil {
		t.Fatalf("QueryTrace() error = %v", err)
	}
}
