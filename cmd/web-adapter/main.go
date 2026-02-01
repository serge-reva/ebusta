package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"time"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

func main() {
	cfg := config.Get()

	orchAddr := cfg.Orchestrator.Address()
	conn, err := grpc.Dial(orchAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("failed to connect to orchestrator: %v", err)
	}
	defer conn.Close()

	client := libraryv1.NewOrchestratorServiceClient(conn)

	http.HandleFunc("/input", func(w http.ResponseWriter, r *http.Request) {
		query := r.URL.Query().Get("msg")
		if query == "" {
			http.Error(w, "missing 'msg' query parameter", http.StatusBadRequest)
			return
		}

		ctx, cancel := context.WithTimeout(r.Context(), 10*time.Second)
		defer cancel()

		resp, err := client.Search(ctx, &libraryv1.SearchRequest{Query: query, Limit: 5})
		if err != nil {
			http.Error(w, fmt.Sprintf("Search error: %v", err), http.StatusInternalServerError)
			return
		}

		if resp.GetTotal() == 0 || len(resp.GetBooks()) == 0 {
			fmt.Fprintf(w, "No books found for: %s\n", query)
			return
		}

		fmt.Fprintf(w, "Found %d books:\n", resp.GetTotal())
		fmt.Fprintln(w, "----------------------------------------")
		for i, b := range resp.GetBooks() {
			if i >= 5 {
				break
			}
			authors := "Unknown"
			if len(b.GetAuthors()) > 0 {
				authors = b.GetAuthors()[0]
				if len(b.GetAuthors()) > 1 {
					authors = fmt.Sprintf("%s, %s", b.GetAuthors()[0], b.GetAuthors()[1])
				}
			}
			fmt.Fprintf(w, "[%s] %s — %s\n", b.GetId(), b.GetTitle(), authors)
		}
	})

	addr := cfg.WebAdapter.Address()
	log.Printf("🌐 Web Adapter started on http://%s", addr)
	if err := http.ListenAndServe(addr, nil); err != nil {
		log.Fatalf("failed to start web server: %v", err)
	}
}
