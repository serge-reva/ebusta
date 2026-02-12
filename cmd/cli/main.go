package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"
	"ebusta/internal/search"

	"github.com/peterh/liner"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

func main() {
	// --- download modes (non-interactive) ---
	// ./bin/ebusta-cli get <sha1>         -> save to downloads.cli.download_dir
	// ./bin/ebusta-cli get-stdout <sha1>  -> write raw bytes to stdout
	if len(os.Args) >= 3 && os.Args[1] == "get" {
		doDownloadToFile(os.Args[2])
		return
	}
	if len(os.Args) >= 3 && os.Args[1] == "get-stdout" {
		doDownloadToStdout(os.Args[2])
		return
	}

	// --- search mode ---
	svc := search.NewService()
	if svc == nil {
		fmt.Fprintln(os.Stderr, "Error: could not connect to search service")
		os.Exit(1)
	}
	defer svc.Close()

	line := liner.NewLiner()
	defer line.Close()

	// non-interactive search: ./bin/ebusta-cli "author:Кинг"
	if len(os.Args) > 1 {
		doSearch(svc, strings.Join(os.Args[1:], " "))
		return
	}

	// interactive REPL
	for {
		if input, err := line.Prompt("ebusta> "); err == nil {
			input = strings.TrimSpace(input)
			if input == "" {
				continue
			}
			if input == "exit" || input == "quit" {
				break
			}

			// interactive download: ebusta> get <sha1>
			if strings.HasPrefix(input, "get ") {
				doDownloadToFile(strings.TrimSpace(strings.TrimPrefix(input, "get ")))
				line.AppendHistory(input)
				continue
			}

			// interactive stdout download: ebusta> get-stdout <sha1>
			if strings.HasPrefix(input, "get-stdout ") {
				doDownloadToStdout(strings.TrimSpace(strings.TrimPrefix(input, "get-stdout ")))
				line.AppendHistory(input)
				continue
			}

			doSearch(svc, input)
			line.AppendHistory(input)
		} else {
			break
		}
	}
}

func doSearch(svc *search.Service, query string) {
	tid := fmt.Sprintf("cli-%d", time.Now().UnixNano())

	resp, err := svc.Search(context.Background(), query, 10, tid)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v (Trace: %s)\n", err, tid)
		return
	}

	fmt.Printf("\n[TRACE: %s] Found %d books:\n", resp.TraceId, resp.Total)
	fmt.Printf("%-40s | %-32s | %-20s | %s\n", "ID", "Title", "Authors", "File")
	fmt.Println(strings.Repeat("-", 120))

	for _, b := range resp.Books {
		fmt.Printf("%-40s | %-32.32s | %-20.20s | %s/%s\n",
			b.ID, b.Title, b.FullAuthors, b.Container, b.Filename)
	}
	fmt.Println()
}

func dialPlasma(cfg *config.Config) (*grpc.ClientConn, libraryv1.StorageNodeClient, error) {
	plasma := cfg.Downloads.PlasmaNode
	addr := fmt.Sprintf("localhost:%d", plasma.ListenPort)

	conn, err := grpc.Dial(addr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		return nil, nil, fmt.Errorf("plasma connect error: %w", err)
	}
	client := libraryv1.NewStorageNodeClient(conn)
	return conn, client, nil
}

func streamToWriter(client libraryv1.StorageNodeClient, sha1 string, w io.Writer) error {
	ctx := context.Background()

	stream, err := client.GetStream(ctx, &libraryv1.GetStreamRequest{
		Id: &libraryv1.BookId{Sha1: sha1},
	})
	if err != nil {
		return fmt.Errorf("GetStream error: %w", err)
	}

	for {
		chunk, err := stream.Recv()
		if err != nil {
			// для простоты: считаем любой recv-err концом (как было раньше)
			break
		}
		if _, err := w.Write(chunk.Data); err != nil {
			return fmt.Errorf("write error: %w", err)
		}
	}
	return nil
}

func doDownloadToFile(sha1 string) {
	if sha1 == "" {
		fmt.Fprintln(os.Stderr, "usage: get <sha1>")
		return
	}

	cfg := config.Get()

	downloadDir := strings.TrimSpace(cfg.Downloads.CLI.DownloadDir)
	if downloadDir == "" {
		downloadDir = "."
	}
	if err := os.MkdirAll(downloadDir, 0o755); err != nil {
		fmt.Fprintf(os.Stderr, "cannot create download_dir %q: %v\n", downloadDir, err)
		return
	}

	conn, client, err := dialPlasma(cfg)
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		return
	}
	defer conn.Close()

	ctx := context.Background()
	metaResp, err := client.GetMeta(ctx, &libraryv1.GetMetaRequest{
		Id: &libraryv1.BookId{Sha1: sha1},
	})
	if err != nil {
		fmt.Fprintf(os.Stderr, "GetMeta error: %v\n", err)
		return
	}

	outName := metaResp.Meta.Filename
	outPath := filepath.Join(downloadDir, outName)

	f, err := os.Create(outPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "cannot create file: %v\n", err)
		return
	}
	defer f.Close()

	if err := streamToWriter(client, sha1, f); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		return
	}

	fmt.Printf("Downloaded %s\n", outPath)
}

func doDownloadToStdout(sha1 string) {
	if sha1 == "" {
		fmt.Fprintln(os.Stderr, "usage: get-stdout <sha1>")
		return
	}

	cfg := config.Get()

	conn, client, err := dialPlasma(cfg)
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		return
	}
	defer conn.Close()

	// stdout must be raw bytes only; all messages go to stderr.
	if err := streamToWriter(client, sha1, os.Stdout); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		return
	}
}
