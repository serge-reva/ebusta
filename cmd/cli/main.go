package main

import (
	"bufio"
	"context"
	"fmt"
	"log"
	"os"
	"strings"
	"time"

	"ebusta/api/proto/v1"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

var debugMode bool

func main() {
	// 0. Проверяем режим отладки
	if os.Getenv("DEBUG") != "" {
		debugMode = true
		log.Println("🐞 DEBUG MODE: ENABLED")
	}

	// 1. Подключение к Orchestrator
	conn, err := grpc.Dial("localhost:50054", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("❌ Failed to connect to Orchestrator: %v", err)
	}
	defer conn.Close()

	client := libraryv1.NewOrchestratorServiceClient(conn)

	// 2. Логика запуска: Аргументы VS Интерактив
	if len(os.Args) > 1 {
		// --- One-Shot Mode (для скриптов) ---
		query := strings.Join(os.Args[1:], " ")
		runSearch(client, query)
	} else {
		// --- Interactive Mode (для людей) ---
		runInteractiveLoop(client)
	}
}

func runInteractiveLoop(client libraryv1.OrchestratorServiceClient) {
	reader := bufio.NewReader(os.Stdin)
	fmt.Println("🚀 Ebusta CLI Interactive Mode")
	fmt.Println("Type 'exit' or 'quit' to stop.")
	fmt.Println("---------------------------------")

	for {
		fmt.Print("ebusta> ")
		text, _ := reader.ReadString('\n')
		text = strings.TrimSpace(text)

		if text == "" {
			continue
		}
		if text == "exit" || text == "quit" {
			fmt.Println("Bye!")
			break
		}

		runSearch(client, text)
	}
}

func runSearch(client libraryv1.OrchestratorServiceClient, query string) {
	if debugMode {
		log.Printf("📡 Sending query: '%s'", query)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	resp, err := client.HandleInput(ctx, &libraryv1.UserRequest{
		RawInput: query,
		UserId:   "cli-user",
		Platform: "cli",
	})

	if err != nil {
		log.Printf("❌ Error: %v", err)
		return
	}

	if resp.TotalFound == 0 {
		fmt.Println("No results found.")
		return
	}

	// Вывод заголовка
	fmt.Printf("%-40s | %-40s | %s\n", "ID", "Title", "Authors")
	fmt.Println(strings.Repeat("-", 100))

	// Вывод строк
	for _, b := range resp.Books {
		title := truncate(b.Title, 38)
		authors := truncate(strings.Join(b.Authors, ", "), 30)
		fmt.Printf("%-40s | %-40s | %s\n", b.Id, title, authors)
	}
	if debugMode {
		fmt.Printf("\n[Total: %d]\n", resp.TotalFound)
	}
}

func truncate(s string, max int) string {
	if len([]rune(s)) > max {
		return string([]rune(s)[:max]) + "..."
	}
	return s
}
