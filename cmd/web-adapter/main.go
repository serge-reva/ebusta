package main

import (
	"context"
	"encoding/json"
	"log"
	"net/http"
	"time"

	"ebusta/api/proto/v1"
	"github.com/google/uuid"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

type Gateway struct {
	orchClient libraryv1.OrchestratorClient
}

func main() {
	conn, err := grpc.Dial("localhost:50054", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("did not connect to orchestrator: %v", err)
	}
	defer conn.Close()

	gw := &Gateway{
		orchClient: libraryv1.NewOrchestratorClient(conn),
	}

	http.HandleFunc("/input", gw.handleInput)
	log.Println("Web-Adapter (The Door) started on :8080")
	log.Fatal(http.ListenAndServe(":8080", nil))
}

func (gw *Gateway) handleInput(w http.ResponseWriter, r *http.Request) {
	msg := r.URL.Query().Get("msg")
	traceID := uuid.New().String()

	ctx, cancel := context.WithTimeout(r.Context(), 5*time.Second)
	defer cancel()

	// Просто пересылаем всё в оркестратор
	resp, err := gw.orchClient.Execute(ctx, &libraryv1.ExecuteRequest{
		RawInput: msg,
		TraceId:  traceID,
	})

	if err != nil {
		log.Printf("[%s] Orchestrator error: %v", traceID, err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("X-Trace-Id", traceID)
	json.NewEncoder(w).Encode(resp)
}
