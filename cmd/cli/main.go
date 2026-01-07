package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"strings"
	"time"

	"ebusta/api/proto/v1"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	"google.golang.org/grpc/metadata"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: ebusta-cli \"query\"")
		os.Exit(1)
	}
	
	query := os.Args[1]
	debugMode := os.Getenv("DEBUG")
	if debugMode == "" { debugMode = "0" }

	// CLI -> Orchestrator
	conn, err := grpc.Dial("localhost:50054", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}
	defer conn.Close()

	c := libraryv1.NewOrchestratorServiceClient(conn)

	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()

	md := metadata.Pairs("x-debug", debugMode)
	ctx = metadata.NewOutgoingContext(ctx, md)

	r, err := c.Search(ctx, &libraryv1.SearchRequest{Query: query})
	if err != nil {
		fmt.Printf("API Error: %v\n", err)
		return
	}

	if len(r.Books) == 0 {
		fmt.Println("No results found.")
		return
	}

	fmt.Printf("%-40s | %-40s | %s\n", "ID", "Title", "Authors")
	fmt.Println(strings.Repeat("-", 100))
	for _, b := range r.Books {
		authors := strings.Join(b.Authors, ", ")
		title := b.Title
		if len(title) > 38 { title = title[:35] + "..." }
		if len(authors) > 30 { authors = authors[:27] + "..." }
		fmt.Printf("%-40s | %-40s | %s\n", b.Id, title, authors)
	}
}
