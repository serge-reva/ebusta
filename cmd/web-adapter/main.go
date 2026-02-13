package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"time"

	"ebusta/internal/config"
	"ebusta/internal/search"
)

func main() {
	cfg := config.Get()
	svc := search.NewService()

	http.HandleFunc("/search", func(w http.ResponseWriter, r *http.Request) {
		query := r.URL.Query().Get("q")
		
		tid := r.Header.Get("X-Trace-Id")
		if tid == "" {
			tid = fmt.Sprintf("web-%d", time.Now().UnixNano())
		}

		log.Printf("[%s] Incoming search: %s", tid, query)

		resp, err := svc.Search(r.Context(), query, 20, tid)
		if err != nil {
			log.Printf("[%s] Error: %v", tid, err)
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}

		w.Header().Set("X-Trace-Id", resp.TraceId)
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(resp)
	})

	addr := fmt.Sprintf(":%d", cfg.WebAdapter.Port)
	log.Printf("🌐 Web-Adapter with TraceID on %s", addr)
	log.Fatal(http.ListenAndServe(addr, nil))
}
