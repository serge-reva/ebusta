package main

import (
	"bufio"
	"database/sql"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"time"

	ds "ebusta/internal/downloads/sqlite"

	_ "modernc.org/sqlite"
)

type docLine struct {
	FileInfo struct {
		Container string `json:"container"`
		Sha1      string `json:"sha1"`
		Filename  string `json:"filename"`
		Size      int64  `json:"size"`
	} `json:"fileInfo"`
	Title string `json:"title"`
}

func main() {
	var (
		jsonlPath  = flag.String("jsonl", "", "path to f2bulker jsonl (bulk format: action line + doc line)")
		sqlitePath = flag.String("sqlite", "", "path to sqlite db")
	)
	flag.Parse()

	if *jsonlPath == "" || *sqlitePath == "" {
		fmt.Fprintf(os.Stderr, "usage: downloads-import -jsonl <file.jsonl> -sqlite <meta.sqlite>\n")
		os.Exit(2)
	}

	db, err := sql.Open("sqlite", *sqlitePath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "open sqlite: %v\n", err)
		os.Exit(1)
	}
	defer db.Close()

	if err := ds.EnsureSchema(db); err != nil {
		fmt.Fprintf(os.Stderr, "ensure schema: %v\n", err)
		os.Exit(1)
	}

	f, err := os.Open(*jsonlPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "open jsonl: %v\n", err)
		os.Exit(1)
	}
	defer f.Close()

	sc := bufio.NewScanner(f)
	sc.Buffer(make([]byte, 64*1024), 64*1024*1024)

	now := time.Now().Format(time.RFC3339Nano)

	tx, err := db.Begin()
	if err != nil {
		fmt.Fprintf(os.Stderr, "begin tx: %v\n", err)
		os.Exit(1)
	}
	defer func() { _ = tx.Rollback() }()

	stmt, err := tx.Prepare(`
INSERT OR IGNORE INTO books(sha1, container, filename, size, title, created_at, updated_at)
VALUES(?, ?, ?, ?, ?, ?, ?)
`)
	if err != nil {
		fmt.Fprintf(os.Stderr, "prepare: %v\n", err)
		os.Exit(1)
	}
	defer stmt.Close()

	lineNo := 0
	inserted := 0
	skipped := 0

	for {
		// action line
		if !sc.Scan() {
			break
		}
		lineNo++
		// doc line
		if !sc.Scan() {
			fmt.Fprintf(os.Stderr, "unexpected EOF: missing doc line after action at line %d\n", lineNo)
			os.Exit(1)
		}
		lineNo++

		var doc docLine
		if err := json.Unmarshal(sc.Bytes(), &doc); err != nil {
			fmt.Fprintf(os.Stderr, "json parse error at line %d: %v\n", lineNo, err)
			os.Exit(1)
		}

		sha1 := doc.FileInfo.Sha1
		if sha1 == "" || doc.FileInfo.Container == "" || doc.FileInfo.Filename == "" {
			skipped++
			continue
		}

		_, err := stmt.Exec(
			sha1,
			doc.FileInfo.Container,
			doc.FileInfo.Filename,
			doc.FileInfo.Size,
			doc.Title,
			now,
			now,
		)
		if err != nil {
			fmt.Fprintf(os.Stderr, "sqlite insert error at line %d: %v\n", lineNo, err)
			os.Exit(1)
		}
		inserted++
	}

	if err := sc.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "scan error: %v\n", err)
		os.Exit(1)
	}

	if err := tx.Commit(); err != nil {
		fmt.Fprintf(os.Stderr, "commit: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("import done: inserted=%d skipped=%d\n", inserted, skipped)
}
