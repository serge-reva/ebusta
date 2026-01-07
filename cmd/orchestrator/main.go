package main

import (
	"context"
	"log"
	"net"

	"ebusta/api/proto/v1"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

type orchestratorServer struct {
	libraryv1.UnimplementedOrchestratorServiceServer
	processorClient libraryv1.ProcessorServiceClient
}

func (s *orchestratorServer) Search(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	log.Printf("🎼 Orchestrator received: %s", req.Query)
	return s.processorClient.Process(ctx, req)
}

func main() {
	// Orchestrator -> Processor
	conn, err := grpc.Dial("localhost:50053", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("failed to connect to processor: %v", err)
	}

	lis, err := net.Listen("tcp", ":50054")
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	libraryv1.RegisterOrchestratorServiceServer(s, &orchestratorServer{
		processorClient: libraryv1.NewProcessorServiceClient(conn),
	})

	log.Println("🎼 Orchestrator started on :50054")
	s.Serve(lis)
}
