package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"
	"time"

	"ebusta/api/proto/v1"
	"github.com/peterh/liner"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

var (
	debugMode   bool
	historyPath = filepath.Join(os.TempDir(), ".ebusta_history")
)

func main() {
	if os.Getenv("DEBUG") != "" {
		debugMode = true
		log.Println("🐞 DEBUG MODE: ENABLED")
	}

	conn, err := grpc.Dial("localhost:50054", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("❌ Failed to connect to Orchestrator: %v", err)
	}
	defer conn.Close()

	client := libraryv1.NewOrchestratorServiceClient(conn)

	if len(os.Args) > 1 {
		query := strings.Join(os.Args[1:], " ")
		runSearch(client, query)
	} else {
		runInteractiveLoop(client)
	}
}

func runInteractiveLoop(client libraryv1.OrchestratorServiceClient) {
	line := liner.NewLiner()
	defer line.Close()

	line.SetCtrlCAborts(true)

	// Загружаем историю из файла, если он есть
	if f, err := os.Open(historyPath); err == nil {
		line.ReadHistory(f)
		f.Close()
	}

	fmt.Println("🚀 Ebusta CLI Interactive Mode (with History Support)")
	fmt.Println("Use UP/DOWN arrows for history. Type 'exit' to stop.")
	fmt.Println("---------------------------------")

	for {
		if text, err := line.Prompt("ebusta> "); err == nil {
			text = strings.TrimSpace(text)
			if text == "" {
				continue
			}
			if text == "exit" || text == "quit" {
				fmt.Println("Bye!")
				break
			}

			// Добавляем в историю и сохраняем
			line.AppendHistory(text)
			runSearch(client, text)

			// Сохраняем историю после каждого успешного ввода
			if f, err := os.Create(historyPath); err == nil {
				line.WriteHistory(f)
				f.Close()
			}
		} else if err == liner.ErrPromptAborted {
			fmt.Println("Aborted")
			break
		} else {
			log.Print("Error reading line: ", err)
			break
		}
	}
}

func runSearch(client libraryv1.OrchestratorServiceClient, query string) {
	if debugMode {
		log.Printf("📡 Sending query: '%s'", query)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	resp, err := client.Search(ctx, &libraryv1.SearchRequest{
		Query:   query,
		TraceId: "cli-user",
	})

	if err != nil {
		log.Printf("❌ Error: %v", err)
		return
	}

	if resp.Total == 0 {
		fmt.Println("No results found.")
		return
	}

	fmt.Printf("%-40s | %-40s | %s\n", "ID", "Title", "Authors")
	fmt.Println(strings.Repeat("-", 100))

	for _, b := range resp.Books {
		fmt.Printf("%-40s | %-40s | %s\n", 
			b.Id, 
			truncate(b.Title, 38), 
			truncate(strings.Join(b.Authors, ", "), 30),
		)
	}
}

func truncate(s string, max int) string {
	runes := []rune(s)
	if len(runes) > max {
		return string(runes[:max]) + "..."
	}
	return s
}
