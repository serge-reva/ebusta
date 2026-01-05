package main

import (
	"context"
	"log"
	"net"

	"ebusta/api/proto/v1"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

type processorServer struct {
	libraryv1.UnimplementedProcessorServiceServer
	dataClient libraryv1.DataServiceClient
}

func (s *processorServer) HandleCommand(ctx context.Context, msg *libraryv1.UnmarshaledMessage) (*libraryv1.Response, error) {
	log.Printf("Processing query from source: %s", msg.Meta.Source)

	// Теперь мы действительно передаем дерево запроса в Data-Manager
	dataResp, err := s.dataClient.GetData(ctx, &libraryv1.DataRequest{
		Query: msg.Query,
	})
	if err != nil {
		log.Printf("DataService error: %v", err)
		return nil, err
	}

	return &libraryv1.Response{
		Status: "OK",
		Books:  dataResp.Books,
		Meta: &libraryv1.ResponseMeta{
			CanonicalForm: msg.Meta.CanonicalForm,
		},
	}, nil
}

func main() {
	lis, _ := net.Listen("tcp", ":50053")
	srv := grpc.NewServer()

	conn, _ := grpc.Dial("localhost:50051", grpc.WithTransportCredentials(insecure.NewCredentials()))
	defer conn.Close()

	libraryv1.RegisterProcessorServiceServer(srv, &processorServer{
		dataClient: libraryv1.NewDataServiceClient(conn),
	})

	log.Println("Processor started on :50053")
	srv.Serve(lis)
}
