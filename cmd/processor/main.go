package main

import (
	"context"
	"log"
	"net"
	"strings"

	"ebusta/api/proto/v1"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

type processorServer struct {
	libraryv1.UnimplementedProcessorServiceServer
	storageClient libraryv1.StorageServiceClient
}

func (s *processorServer) Process(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	// Нормализация (Brute Force очистка для OpenSearch)
	cleanedQuery := req.Query
	cleanedQuery = strings.ReplaceAll(cleanedQuery, "authors:", "")
	cleanedQuery = strings.ReplaceAll(cleanedQuery, "author:", "")
	cleanedQuery = strings.ReplaceAll(cleanedQuery, "title:", "")
	cleanedQuery = strings.TrimSpace(cleanedQuery)

	log.Printf("🧠 Processor normalized: '%s' -> '%s'", req.Query, cleanedQuery)

	return s.storageClient.SearchBooks(ctx, &libraryv1.SearchRequest{
		Query: cleanedQuery,
		Limit: 10,
	})
}

func main() {
	// Processor -> Storage (DataManager)
	conn, err := grpc.Dial("localhost:50051", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("failed to connect to storage: %v", err)
	}

	lis, err := net.Listen("tcp", ":50053")
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	libraryv1.RegisterProcessorServiceServer(s, &processorServer{
		storageClient: libraryv1.NewStorageServiceClient(conn),
	})

	log.Println("🧠 Processor started on :50053")
	s.Serve(lis)
}
