package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"strings"
	"time"

	"ebusta/api/proto/v1"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

func main() {
	// 1. Подключение к Orchestrator (порт 50053)
	orchHost := os.Getenv("ORCHESTRATOR_HOST")
	if orchHost == "" {
		orchHost = "localhost:50053"
	}

	conn, err := grpc.Dial(orchHost, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}
	defer conn.Close()

	// ИСПРАВЛЕНИЕ 1: Правильное имя клиента (OrchestratorServiceClient)
	client := libraryv1.NewOrchestratorServiceClient(conn)

	http.HandleFunc("/input", func(w http.ResponseWriter, r *http.Request) {
		query := r.URL.Query().Get("msg")
		if query == "" {
			query = r.URL.Query().Get("q")
		}

		if query == "" {
			http.Error(w, "Please provide 'msg' parameter", http.StatusBadRequest)
			return
		}

		log.Printf("🌍 Web Adapter received: %s", query)

		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()

		// ИСПРАВЛЕНИЕ 2: Используем SearchRequest и метод Search
		resp, err := client.Search(ctx, &libraryv1.SearchRequest{
			Query: query,
		})

		if err != nil {
			http.Error(w, fmt.Sprintf("Error calling Orchestrator: %v", err), http.StatusInternalServerError)
			return
		}

		// Форматируем простой текстовый ответ
		w.Header().Set("Content-Type", "text/plain; charset=utf-8")

		if len(resp.Books) == 0 {
			fmt.Fprintf(w, "No books found for: %s\n", query)
			return
		}

		fmt.Fprintf(w, "Found %d books:\n", len(resp.Books))
		fmt.Fprintln(w, strings.Repeat("-", 40))
		for _, b := range resp.Books {
			authors := strings.Join(b.Authors, ", ")
			fmt.Fprintf(w, "[%s] %s — %s\n", b.Id, b.Title, authors)
		}
	})

	port := "50080"
	log.Printf("🌍 Web Adapter started on :%s", port)
	if err := http.ListenAndServe(":"+port, nil); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
