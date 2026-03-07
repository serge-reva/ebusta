package main

import (
	"bufio"
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"time"

	ds "ebusta/internal/downloads/sqlite"
	"ebusta/internal/errutil"
	"ebusta/internal/logger"

	"github.com/schollz/progressbar/v3"
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

type stats struct {
	Processed  int
	Inserted   int
	Duplicates int
	Errors     int
}

type app struct {
	opts   cliOptions
	log    *logger.Logger
	stderr io.Writer
}

type txState struct {
	tx   *sql.Tx
	stmt *sql.Stmt
}

func newApp(opts cliOptions, stderr io.Writer) (*app, error) {
	log, err := newCLIReporter(opts, stderr)
	if err != nil {
		return nil, err
	}
	return &app{opts: opts, log: log, stderr: stderr}, nil
}

func (a *app) Close() error {
	if a.log != nil {
		return a.log.Close()
	}
	return nil
}

func (a *app) Run(ctx context.Context) (stats, error) {
	var st stats

	db, err := sql.Open("sqlite", a.opts.SQLitePath)
	if err != nil {
		return st, errutil.New(errutil.CodeInternal, "open sqlite failed").WithTrace(errutil.TraceIDFromContext(ctx)).WithDetails(err.Error())
	}
	defer db.Close()

	if err := ds.EnsureSchema(db); err != nil {
		return st, errutil.New(errutil.CodeInternal, "ensure schema failed").WithTrace(errutil.TraceIDFromContext(ctx)).WithDetails(err.Error())
	}

	totalDocs, err := countDocLines(a.opts.JSONLPath)
	if err != nil {
		return st, errutil.New(errutil.CodeInvalidArgument, "count input records failed").WithTrace(errutil.TraceIDFromContext(ctx)).WithDetails(err.Error())
	}

	a.log.WithFields(map[string]interface{}{
		"jsonl":           a.opts.JSONLPath,
		"sqlite":          a.opts.SQLitePath,
		"total_docs":      totalDocs,
		"commit_interval": a.opts.CommitInterval,
		"continue_on_err": a.opts.ContinueOnError,
	}).InfoCtx(ctx, "downloads-import started")

	progress := newProgress(totalDocs, a.stderr, a.opts.Quiet)
	defer func() {
		if progress != nil {
			_ = progress.Finish()
		}
	}()

	f, err := os.Open(a.opts.JSONLPath)
	if err != nil {
		return st, errutil.New(errutil.CodeNotFound, "open jsonl failed").WithTrace(errutil.TraceIDFromContext(ctx)).WithDetails(err.Error())
	}
	defer f.Close()

	tx, err := beginTx(db)
	if err != nil {
		return st, errutil.New(errutil.CodeInternal, "begin transaction failed").WithTrace(errutil.TraceIDFromContext(ctx)).WithDetails(err.Error())
	}
	defer rollbackTx(tx)

	now := time.Now().Format(time.RFC3339Nano)
	sc := bufio.NewScanner(f)
	sc.Buffer(make([]byte, 64*1024), 64*1024*1024)

	lineNo := 0
	insertedSinceCommit := 0
	for {
		if !sc.Scan() {
			break
		}
		lineNo++

		if !sc.Scan() {
			return st, errutil.New(errutil.CodeInvalidArgument, fmt.Sprintf("unexpected EOF after action line %d", lineNo)).WithTrace(errutil.TraceIDFromContext(ctx))
		}
		lineNo++
		docBytes := append([]byte(nil), sc.Bytes()...)
		st.Processed++

		if progress != nil {
			_ = progress.Add(1)
		}

		var doc docLine
		if err := json.Unmarshal(docBytes, &doc); err != nil {
			if e := a.handleRecordError(ctx, &st, lineNo, "json parse error", errutil.New(errutil.CodeInvalidArgument, "invalid JSON document line").WithDetails(err.Error())); e != nil {
				return st, e
			}
			continue
		}

		if doc.FileInfo.Sha1 == "" || doc.FileInfo.Container == "" || doc.FileInfo.Filename == "" {
			if e := a.handleRecordError(ctx, &st, lineNo, "record validation error", errutil.New(errutil.CodeInvalidArgument, "required fileInfo fields are missing")); e != nil {
				return st, e
			}
			continue
		}

		res, err := tx.stmt.Exec(
			doc.FileInfo.Sha1,
			doc.FileInfo.Container,
			doc.FileInfo.Filename,
			doc.FileInfo.Size,
			doc.Title,
			now,
			now,
		)
		if err != nil {
			if e := a.handleRecordError(ctx, &st, lineNo, "sqlite insert error", errutil.New(errutil.CodeInternal, "insert into sqlite failed").WithDetails(err.Error())); e != nil {
				return st, e
			}
			continue
		}

		rows, err := res.RowsAffected()
		if err != nil {
			if e := a.handleRecordError(ctx, &st, lineNo, "sqlite rows affected error", errutil.New(errutil.CodeInternal, "rows affected lookup failed").WithDetails(err.Error())); e != nil {
				return st, e
			}
			continue
		}
		if rows == 0 {
			st.Duplicates++
			continue
		}

		st.Inserted++
		insertedSinceCommit++
		if a.opts.CommitInterval > 0 && insertedSinceCommit >= a.opts.CommitInterval {
			if err := commitAndReopen(db, &tx); err != nil {
				return st, errutil.New(errutil.CodeInternal, "periodic commit failed").WithTrace(errutil.TraceIDFromContext(ctx)).WithDetails(err.Error())
			}
			insertedSinceCommit = 0
		}
	}

	if err := sc.Err(); err != nil {
		return st, errutil.New(errutil.CodeInternal, "scan input failed").WithTrace(errutil.TraceIDFromContext(ctx)).WithDetails(err.Error())
	}

	if err := tx.tx.Commit(); err != nil {
		return st, errutil.New(errutil.CodeInternal, "final commit failed").WithTrace(errutil.TraceIDFromContext(ctx)).WithDetails(err.Error())
	}
	tx.tx = nil

	a.log.WithFields(map[string]interface{}{
		"processed":  st.Processed,
		"inserted":   st.Inserted,
		"duplicates": st.Duplicates,
		"errors":     st.Errors,
	}).InfoCtx(ctx, "downloads-import finished")

	return st, nil
}

func (a *app) handleRecordError(ctx context.Context, st *stats, lineNo int, message string, appErr *errutil.AppError) error {
	st.Errors++
	a.log.WithFields(map[string]interface{}{
		"line":   lineNo,
		"code":   appErr.Code,
		"detail": appErr.Details,
	}).ErrorCtx(ctx, message, appErr)
	if a.opts.ContinueOnError {
		return nil
	}
	return appErr.WithTrace(errutil.TraceIDFromContext(ctx))
}

func newCLIReporter(opts cliOptions, stderr io.Writer) (*logger.Logger, error) {
	formatter := logger.NewTextFormatter()
	formatter.DisableColors = true

	outputs := make([]logger.Output, 0, 2)
	if !opts.Quiet {
		outputs = append(outputs, logger.NewWriterOutput(stderr, "stderr"))
	}
	if opts.LogFile != "" {
		if err := os.MkdirAll(filepath.Dir(opts.LogFile), 0o755); err != nil && filepath.Dir(opts.LogFile) != "." {
			return nil, err
		}
		fo, err := logger.NewFileOutput(opts.LogFile, true)
		if err != nil {
			return nil, err
		}
		outputs = append(outputs, fo)
	}
	if len(outputs) == 0 {
		outputs = append(outputs, logger.NewWriterOutput(io.Discard, "discard"))
	}

	var output logger.Output
	if len(outputs) == 1 {
		output = outputs[0]
	} else {
		output = logger.NewMultiOutput(outputs...)
	}
	log := logger.New(logger.ParseLevel(opts.LogLevel), formatter, output, "downloads-import")
	logger.SetGlobal(log)
	return log, nil
}

func newProgress(total int, stderr io.Writer, quiet bool) *progressbar.ProgressBar {
	if quiet || total <= 0 {
		return nil
	}
	return progressbar.NewOptions(total,
		progressbar.OptionSetWriter(stderr),
		progressbar.OptionSetDescription("importing"),
		progressbar.OptionShowCount(),
		progressbar.OptionSetWidth(20),
		progressbar.OptionClearOnFinish(),
	)
}

func countDocLines(path string) (int, error) {
	f, err := os.Open(path)
	if err != nil {
		return 0, err
	}
	defer f.Close()

	sc := bufio.NewScanner(f)
	sc.Buffer(make([]byte, 64*1024), 64*1024*1024)
	count := 0
	lineNo := 0
	for {
		if !sc.Scan() {
			break
		}
		lineNo++
		if !sc.Scan() {
			return 0, fmt.Errorf("unexpected EOF after action line %d", lineNo)
		}
		lineNo++
		count++
	}
	if err := sc.Err(); err != nil {
		return 0, err
	}
	return count, nil
}

func beginTx(db *sql.DB) (*txState, error) {
	tx, err := db.Begin()
	if err != nil {
		return nil, err
	}
	stmt, err := tx.Prepare(`
INSERT OR IGNORE INTO books(sha1, container, filename, size, title, created_at, updated_at)
VALUES(?, ?, ?, ?, ?, ?, ?)
`)
	if err != nil {
		_ = tx.Rollback()
		return nil, err
	}
	return &txState{tx: tx, stmt: stmt}, nil
}

func rollbackTx(state *txState) {
	if state == nil {
		return
	}
	if state.stmt != nil {
		_ = state.stmt.Close()
	}
	if state.tx != nil {
		_ = state.tx.Rollback()
	}
}

func commitAndReopen(db *sql.DB, state **txState) error {
	if *state == nil {
		return nil
	}
	if (*state).stmt != nil {
		if err := (*state).stmt.Close(); err != nil {
			return err
		}
		(*state).stmt = nil
	}
	if err := (*state).tx.Commit(); err != nil {
		return err
	}
	(*state).tx = nil
	next, err := beginTx(db)
	if err != nil {
		return err
	}
	*state = next
	return nil
}
