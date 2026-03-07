package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"net/http"
	"os"
	"strings"
	"text/tabwriter"

	"ebusta/internal/logger"
	"ebusta/internal/lokiclient"
	"ebusta/internal/traceviewer"
)

type commonOptions struct {
	LokiURL  string
	Selector string
	LogLevel string
}

func main() {
	if len(os.Args) > 1 && os.Args[1] == "get" {
		if err := runGet(os.Args[2:]); err != nil {
			fmt.Fprintf(os.Stderr, "trace-viewer: %v\n", err)
			os.Exit(1)
		}
		return
	}
	if err := runServe(os.Args[1:]); err != nil {
		fmt.Fprintf(os.Stderr, "trace-viewer: %v\n", err)
		os.Exit(1)
	}
}

func runServe(args []string) error {
	flags := flag.NewFlagSet("trace-viewer", flag.ContinueOnError)
	traceID := flags.String("trace-id", "", "print logs for trace_id instead of starting the server")
	jsonOutput := flags.Bool("json", false, "print CLI output as JSON when used with --trace-id")
	port := flags.Int("port", 8080, "HTTP listen port")
	options := addCommonFlags(flags)
	if err := flags.Parse(args); err != nil {
		return err
	}

	client, log := buildDependencies(options)
	if strings.TrimSpace(*traceID) != "" {
		return printTrace(context.Background(), client, strings.TrimSpace(*traceID), *jsonOutput)
	}

	service := traceviewer.New(client, log)
	address := fmt.Sprintf(":%d", *port)
	log.WithField("addr", address).Info("", "trace-viewer starting")
	return http.ListenAndServe(address, service.Handler())
}

func runGet(args []string) error {
	flags := flag.NewFlagSet("trace-viewer get", flag.ContinueOnError)
	traceID := flags.String("trace-id", "", "trace_id to query")
	jsonOutput := flags.Bool("json", false, "print JSON instead of a table")
	options := addCommonFlags(flags)
	if err := flags.Parse(args); err != nil {
		return err
	}
	if *traceID == "" && flags.NArg() > 0 {
		*traceID = flags.Arg(0)
	}
	if strings.TrimSpace(*traceID) == "" {
		return fmt.Errorf("trace_id is required")
	}

	client, _ := buildDependencies(options)
	return printTrace(context.Background(), client, strings.TrimSpace(*traceID), *jsonOutput)
}

func addCommonFlags(flags *flag.FlagSet) commonOptions {
	options := commonOptions{}
	flags.StringVar(&options.LokiURL, "loki-url", "http://loki:3100", "base URL of Loki")
	flags.StringVar(&options.Selector, "selector", `{compose_project="ebusta"}`, "LogQL selector used before trace_id filtering")
	flags.StringVar(&options.LogLevel, "log-level", "INFO", "logger level")
	return options
}

func buildDependencies(options commonOptions) (*lokiclient.Client, *logger.Logger) {
	formatter := logger.NewTextFormatter()
	formatter.DisableColors = true
	log := logger.New(logger.ParseLevel(options.LogLevel), formatter, &logger.StderrOutput{}, "trace-viewer")
	client := lokiclient.New(options.LokiURL, lokiclient.WithSelector(options.Selector))
	return client, log
}

func printTrace(ctx context.Context, client *lokiclient.Client, traceID string, jsonOutput bool) error {
	entries, err := client.QueryTrace(ctx, traceID)
	if err != nil {
		return err
	}
	if jsonOutput {
		encoder := json.NewEncoder(os.Stdout)
		encoder.SetIndent("", "  ")
		return encoder.Encode(struct {
			TraceID string             `json:"trace_id"`
			Count   int                `json:"count"`
			Entries []lokiclient.Entry `json:"entries"`
		}{
			TraceID: traceID,
			Count:   len(entries),
			Entries: entries,
		})
	}

	writer := tabwriter.NewWriter(os.Stdout, 0, 4, 2, ' ', 0)
	fmt.Fprintln(writer, "TIME\tLEVEL\tSERVICE\tMESSAGE\tERROR")
	for _, entry := range entries {
		fmt.Fprintf(writer, "%s\t%s\t%s\t%s\t%s\n",
			entry.Timestamp.Format("2006-01-02 15:04:05.000"),
			entry.Level,
			entry.Service,
			entry.Message,
			entry.Error,
		)
	}
	return writer.Flush()
}
