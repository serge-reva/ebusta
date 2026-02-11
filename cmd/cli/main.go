package main

import (
	"context"
	"fmt"
	"os"
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
	// --- download mode (non-interactive): ./bin/ebusta-cli get <sha1> ---
	if len(os.Args) >= 3 && os.Args[1] == "get" {
		doDownload(os.Args[2])
		return
	}

	// --- search mode ---
	svc := search.NewService()
	if svc == nil {
		fmt.Println("Error: could not connect to search service")
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
				doDownload(strings.TrimSpace(strings.TrimPrefix(input, "get ")))
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
		fmt.Printf("Error: %v (Trace: %s)\n", err, tid)
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

func doDownload(sha1 string) {
	if sha1 == "" {
		fmt.Println("usage: get <sha1>")
		return
	}

	cfg := config.Get()
	plasma := cfg.Downloads.PlasmaNode
	addr := fmt.Sprintf("localhost:%d", plasma.ListenPort)

	conn, err := grpc.Dial(addr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		fmt.Printf("plasma connect error: %v\n", err)
		return
	}
	defer conn.Close()

	client := libraryv1.NewStorageNodeClient(conn)
	ctx := context.Background()

	metaResp, err := client.GetMeta(ctx, &libraryv1.GetMetaRequest{
		Id: &libraryv1.BookId{Sha1: sha1},
	})
	if err != nil {
		fmt.Printf("GetMeta error: %v\n", err)
		return
	}

	outName := metaResp.Meta.Filename
	f, err := os.Create(outName)
	if err != nil {
		fmt.Printf("cannot create file: %v\n", err)
		return
	}
	defer f.Close()

	stream, err := client.GetStream(ctx, &libraryv1.GetStreamRequest{
		Id: &libraryv1.BookId{Sha1: sha1},
	})
	if err != nil {
		fmt.Printf("GetStream error: %v\n", err)
		return
	}

	for {
		chunk, err := stream.Recv()
		if err != nil {
			break
		}
		// IMPORTANT: Chunk.data is bytes -> []byte in Go. No base64 here.
		if _, err := f.Write(chunk.Data); err != nil {
			fmt.Printf("write error: %v\n", err)
			return
		}
	}

	fmt.Printf("Downloaded %s\n", outName)
}
