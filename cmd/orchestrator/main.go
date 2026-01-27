package main

import (
	"context"
	"log"
	"net"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"

	libraryv1 "ebusta/api/proto/v1"
)

type orchestratorServer struct {
	libraryv1.UnimplementedOrchestratorServiceServer
	dslClient     libraryv1.MessageConverterClient
	storageClient libraryv1.StorageServiceClient
}

func (s *orchestratorServer) Search(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	log.Printf("🎼 Orchestrator received: %s", req.Query)
	
	// 1. Парсим через DSL
	log.Printf("🎼 Orchestrator -> DSL-Converter")
	dslResp, err := s.dslClient.Convert(ctx, &libraryv1.ConvertRequest{
		RawQuery: req.Query,
	})
	
	if err != nil {
		log.Printf("❌ DSL Error: %v", err)
		return nil, err
	}
	
	log.Printf("✅ DSL Parsed: %s", dslResp.CanonicalForm)
	
	// 2. Передаем canonical_form (распарсенный запрос) в DataManager
	searchReq := &libraryv1.SearchRequest{
		Query: dslResp.CanonicalForm, // Используем распарсенный запрос!
	}
	
	return s.storageClient.SearchBooks(ctx, searchReq)
}

func main() {
	log.Println("=== [ORCHESTRATOR] Starting on :50053 ===")
	
	dslConn, err := grpc.Dial("localhost:50052", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("failed to connect to dsl: %v", err)
	}
	
	storageConn, err := grpc.Dial("localhost:50051", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("failed to connect to storage: %v", err)
	}
	
	lis, err := net.Listen("tcp", ":50053")
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	libraryv1.RegisterOrchestratorServiceServer(s, &orchestratorServer{
		dslClient:     libraryv1.NewMessageConverterClient(dslConn),
		storageClient: libraryv1.NewStorageServiceClient(storageConn),
	})

	log.Println("🎼 Orchestrator service registered")
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
