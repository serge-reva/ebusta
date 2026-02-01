package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"strings"
	"time"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

var debugMode bool

func init() {
	debugMode = os.Getenv("DEBUG") == "1"
}

func debugLog(format string, args ...interface{}) {
	if debugMode {
		log.Printf("🐛 "+format, args...)
	}
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: ebusta-cli \"search query\"")
		return
	}

	query := strings.Join(os.Args[1:], " ")

	cfg := config.Get()
	orchAddr := cfg.Orchestrator.Address()

	debugLog("Connecting to %s", orchAddr)
	conn, err := grpc.Dial(orchAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("Failed to connect to orchestrator: %v", err)
	}
	defer conn.Close()

	client := libraryv1.NewOrchestratorServiceClient(conn)

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	resp, err := client.Search(ctx, &libraryv1.SearchRequest{
		Query: query,
		Limit: 10,
	})
	if err != nil {
		log.Fatalf("Search error: %v", err)
	}

	if len(resp.GetBooks()) == 0 {
		fmt.Println("No results found.")
		return
	}

	fmt.Printf("%-40s | %-40s | %s\n", "ID", "Title", "Authors")
	fmt.Println(strings.Repeat("-", 100))

	for _, b := range resp.GetBooks() {
		fmt.Printf("%-40s | %-40s | %s\n", b.GetId(), b.GetTitle(), strings.Join(b.GetAuthors(), ", "))
	}
}
