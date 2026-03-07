package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"os"

	"ebusta/internal/errutil"
	"ebusta/internal/logger"
)

type cliOptions struct {
	JSONLPath       string
	SQLitePath      string
	LogLevel        string
	LogFile         string
	ContinueOnError bool
	Quiet           bool
	CommitInterval  int
}

func parseFlags(args []string) (cliOptions, error) {
	fs := flag.NewFlagSet("downloads-import", flag.ContinueOnError)
	fs.SetOutput(os.Stderr)

	opts := cliOptions{}
	fs.StringVar(&opts.JSONLPath, "jsonl", "", "path to f2bulker JSONL bulk file (action line + doc line)")
	fs.StringVar(&opts.SQLitePath, "sqlite", "", "path to sqlite metadata database")
	fs.StringVar(&opts.LogLevel, "log-level", "INFO", "log level: DEBUG, INFO, WARN, ERROR")
	fs.StringVar(&opts.LogFile, "log-file", "", "optional file to append logs to")
	fs.BoolVar(&opts.ContinueOnError, "continue-on-error", false, "log per-record errors and continue processing")
	fs.BoolVar(&opts.Quiet, "quiet", false, "suppress stderr output including progress bar and informational logs")
	fs.IntVar(&opts.CommitInterval, "commit-interval", 0, "commit every N successfully inserted records; 0 keeps one transaction for the whole file")

	if err := fs.Parse(args); err != nil {
		return cliOptions{}, err
	}
	if opts.JSONLPath == "" || opts.SQLitePath == "" {
		return cliOptions{}, errors.New("usage: downloads-import -jsonl <file.jsonl> -sqlite <meta.sqlite>")
	}
	if opts.CommitInterval < 0 {
		return cliOptions{}, errors.New("commit-interval must be >= 0")
	}
	return opts, nil
}

func main() {
	opts, err := parseFlags(os.Args[1:])
	if err != nil {
		fmt.Fprintf(os.Stderr, "downloads-import: %v\n", err)
		os.Exit(2)
	}

	traceID := errutil.GenerateTraceID("dli")
	ctx := logger.ContextWithTraceID(context.Background(), traceID)

	app, err := newApp(opts, os.Stderr)
	if err != nil {
		fmt.Fprintf(os.Stderr, "downloads-import: %v\n", err)
		os.Exit(1)
	}
	defer app.Close()

	if _, err := app.Run(ctx); err != nil {
		fmt.Fprintf(os.Stderr, "downloads-import: %v\n", err)
		os.Exit(1)
	}
}
