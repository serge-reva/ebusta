package main

import (
	"context"
	"log"
	"time"

	"ebusta/api/proto/v1"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

func main() {
	// Подключаемся к серверу на порту 50051
	conn, err := grpc.Dial("localhost:50051", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}
	defer conn.Close()

	c := libraryv1.NewLibraryServiceClient(conn)

	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()

	log.Println("--- Ebusta gRPC Client: Sending Search Request ---")
	r, err := c.Search(ctx, &libraryv1.SearchRequest{
		Query: "Flibusta rules",
	})
	if err != nil {
		log.Fatalf("could not search: %v", err)
	}

	log.Printf("Response from server: Found %d books", r.GetTotalFound())
	for _, book := range r.GetBooks() {
		log.Printf("-> Book: [%s] %s (Authors: %v)", book.GetId(), book.GetTitle(), book.GetAuthors())
	}
}
