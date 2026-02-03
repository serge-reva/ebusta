package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"time"

	"ebusta/internal/config"
	"ebusta/internal/search"
)

func main() {
	cfg := config.Get()

	searchSvc, err := search.New()
	if err != nil {
		log.Fatalf("failed to initialize search service: %v", err)
	}
	defer searchSvc.Close()

	http.HandleFunc("/input", func(w http.ResponseWriter, r *http.Request) {
		query := r.URL.Query().Get("msg")
		if query == "" {
			http.Error(w, "missing 'msg' query parameter", http.StatusBadRequest)
			return
		}

		ctx, cancel := context.WithTimeout(r.Context(), 10*time.Second)
		defer cancel()

		res, err := searchSvc.Search(ctx, query, 5)
		if err != nil {
			http.Error(w, fmt.Sprintf("Search error: %v", err), http.StatusInternalServerError)
			return
		}

		if res.Total == 0 || len(res.Books) == 0 {
			fmt.Fprintf(w, "No books found for: %s\n", query)
			return
		}

		fmt.Fprintf(w, "Found %d books:\n", res.Total)
		fmt.Fprintln(w, "----------------------------------------")
		for _, b := range res.Books {
			fmt.Fprintf(w, "[%s] %s — %s\n", b.ID, b.Title, b.FullAuthors)
		}
	})

	addr := cfg.WebAdapter.Address()
	log.Printf("🌐 Web Adapter started on http://%s", addr)
	if err := http.ListenAndServe(addr, nil); err != nil {
		log.Fatalf("failed to start web server: %v", err)
	}
}
