package main

import (
	"context"
	"database/sql"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	ds "ebusta/internal/downloads/sqlite"
	"ebusta/internal/logger"

	_ "modernc.org/sqlite"
)

func TestRunImportSuccessfulImportAndDuplicates(t *testing.T) {
	dir := t.TempDir()
	jsonl := filepath.Join(dir, "books.jsonl")
	sqlitePath := filepath.Join(dir, "books.sqlite")
	stderr := &strings.Builder{}

	writeJSONL(t, jsonl,
		actionLine(), docJSON("sha1-a", "c1.zip", "a.fb2", 11, "Book A"),
		actionLine(), docJSON("sha1-a", "c1.zip", "a.fb2", 11, "Book A"),
		actionLine(), docJSON("sha1-b", "c1.zip", "b.fb2", 12, "Book B"),
	)

	app, err := newApp(cliOptions{JSONLPath: jsonl, SQLitePath: sqlitePath, Quiet: true, LogLevel: "INFO"}, stderr)
	if err != nil {
		t.Fatalf("newApp() error = %v", err)
	}
	defer app.Close()

	stats, err := app.Run(logger.ContextWithTraceID(context.Background(), "dli-test-1"))
	if err != nil {
		t.Fatalf("Run() error = %v", err)
	}
	if stats.Processed != 3 || stats.Inserted != 2 || stats.Duplicates != 1 || stats.Errors != 0 {
		t.Fatalf("unexpected stats: %+v", stats)
	}
	if got := countRows(t, sqlitePath); got != 2 {
		t.Fatalf("expected 2 rows, got %d", got)
	}
}

func TestRunImportContinueOnError(t *testing.T) {
	dir := t.TempDir()
	jsonl := filepath.Join(dir, "books.jsonl")
	sqlitePath := filepath.Join(dir, "books.sqlite")
	stderr := &strings.Builder{}

	writeJSONL(t, jsonl,
		actionLine(), docJSON("sha1-a", "c1.zip", "a.fb2", 11, "Book A"),
		actionLine(), `{"fileInfo":`,
		actionLine(), docJSON("sha1-b", "c1.zip", "b.fb2", 12, "Book B"),
	)

	app, err := newApp(cliOptions{JSONLPath: jsonl, SQLitePath: sqlitePath, Quiet: true, LogLevel: "INFO", ContinueOnError: true}, stderr)
	if err != nil {
		t.Fatalf("newApp() error = %v", err)
	}
	defer app.Close()

	stats, err := app.Run(logger.ContextWithTraceID(context.Background(), "dli-test-2"))
	if err != nil {
		t.Fatalf("Run() error = %v", err)
	}
	if stats.Processed != 3 || stats.Inserted != 2 || stats.Errors != 1 {
		t.Fatalf("unexpected stats: %+v", stats)
	}
	if got := countRows(t, sqlitePath); got != 2 {
		t.Fatalf("expected 2 rows, got %d", got)
	}
}

func TestCommitIntervalPersistsBeforeFailure(t *testing.T) {
	t.Run("single transaction rolls back", func(t *testing.T) {
		dir := t.TempDir()
		jsonl := filepath.Join(dir, "books.jsonl")
		sqlitePath := filepath.Join(dir, "books.sqlite")
		stderr := &strings.Builder{}
		writeJSONL(t, jsonl,
			actionLine(), docJSON("sha1-a", "c1.zip", "a.fb2", 11, "Book A"),
			actionLine(), `{"fileInfo":`,
		)
		app, err := newApp(cliOptions{JSONLPath: jsonl, SQLitePath: sqlitePath, Quiet: true}, stderr)
		if err != nil {
			t.Fatalf("newApp() error = %v", err)
		}
		defer app.Close()
		if _, err := app.Run(logger.ContextWithTraceID(context.Background(), "dli-test-3a")); err == nil {
			t.Fatal("expected failure")
		}
		if got := countRows(t, sqlitePath); got != 0 {
			t.Fatalf("expected rollback to leave 0 rows, got %d", got)
		}
	})

	t.Run("commit interval keeps prior inserts", func(t *testing.T) {
		dir := t.TempDir()
		jsonl := filepath.Join(dir, "books.jsonl")
		sqlitePath := filepath.Join(dir, "books.sqlite")
		stderr := &strings.Builder{}
		writeJSONL(t, jsonl,
			actionLine(), docJSON("sha1-a", "c1.zip", "a.fb2", 11, "Book A"),
			actionLine(), `{"fileInfo":`,
		)
		app, err := newApp(cliOptions{JSONLPath: jsonl, SQLitePath: sqlitePath, Quiet: true, CommitInterval: 1}, stderr)
		if err != nil {
			t.Fatalf("newApp() error = %v", err)
		}
		defer app.Close()
		if _, err := app.Run(logger.ContextWithTraceID(context.Background(), "dli-test-3b")); err == nil {
			t.Fatal("expected failure")
		}
		if got := countRows(t, sqlitePath); got != 1 {
			t.Fatalf("expected first row to remain committed, got %d", got)
		}
	})
}

func TestQuietAndLogFile(t *testing.T) {
	dir := t.TempDir()
	jsonl := filepath.Join(dir, "books.jsonl")
	sqlitePath := filepath.Join(dir, "books.sqlite")
	logFile := filepath.Join(dir, "downloads-import.log")
	stderr := &strings.Builder{}
	writeJSONL(t, jsonl,
		actionLine(), docJSON("sha1-a", "c1.zip", "a.fb2", 11, "Book A"),
	)
	app, err := newApp(cliOptions{JSONLPath: jsonl, SQLitePath: sqlitePath, Quiet: true, LogFile: logFile}, stderr)
	if err != nil {
		t.Fatalf("newApp() error = %v", err)
	}
	defer app.Close()
	if _, err := app.Run(logger.ContextWithTraceID(context.Background(), "dli-test-4")); err != nil {
		t.Fatalf("Run() error = %v", err)
	}
	if stderr.Len() != 0 {
		t.Fatalf("expected quiet mode to suppress stderr, got %q", stderr.String())
	}
	data, err := os.ReadFile(logFile)
	if err != nil {
		t.Fatalf("ReadFile() error = %v", err)
	}
	if !strings.Contains(string(data), "downloads-import finished") {
		t.Fatalf("expected summary in log file, got %q", string(data))
	}
}

func TestCountDocLinesRejectsDanglingActionLine(t *testing.T) {
	dir := t.TempDir()
	jsonl := filepath.Join(dir, "broken.jsonl")
	writeJSONL(t, jsonl, actionLine())
	if _, err := countDocLines(jsonl); err == nil {
		t.Fatal("expected dangling action line to fail")
	}
}

func actionLine() string { return `{"index":{"_index":"books"}}` }

func docJSON(sha1, container, filename string, size int64, title string) string {
	return fmt.Sprintf(`{"fileInfo":{"container":"%s","sha1":"%s","filename":"%s","size":%d},"title":"%s"}`, container, sha1, filename, size, title)
}

func writeJSONL(t *testing.T, path string, lines ...string) {
	t.Helper()
	content := strings.Join(lines, "\n") + "\n"
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("WriteFile() error = %v", err)
	}
}

func countRows(t *testing.T, sqlitePath string) int {
	t.Helper()
	db, err := sql.Open("sqlite", sqlitePath)
	if err != nil {
		t.Fatalf("sql.Open() error = %v", err)
	}
	defer db.Close()
	if err := ds.EnsureSchema(db); err != nil {
		t.Fatalf("EnsureSchema() error = %v", err)
	}
	var n int
	if err := db.QueryRow(`SELECT COUNT(*) FROM books`).Scan(&n); err != nil {
		t.Fatalf("QueryRow() error = %v", err)
	}
	return n
}
