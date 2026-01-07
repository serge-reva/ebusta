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
	libraryClient libraryv1.LibraryServiceClient
}

func (s *processorServer) HandleCommand(ctx context.Context, msg *libraryv1.UnmarshaledMessage) (*libraryv1.Response, error) {
	searchTerm := msg.Meta.CanonicalForm
	template := "fl_mixed_search" // По умолчанию самый гибкий поиск

	// Анализ AST
	if filter := msg.Query.GetFilter(); filter != nil {
		searchTerm = filter.Value
		
		// Если пользователь явно ищет по названию, используем префиксный шаблон
		if filter.Field == "title" {
			template = "fl_title_prefix"
		}
		// Для авторов оставляем mixed_search, так как он лучше справляется с фамилиями
	}

	log.Printf("🎯 Routing to [%s] with term: %s", template, searchTerm)

	dataResp, err := s.libraryClient.SearchBooks(ctx, &libraryv1.SearchRequest{
		Query:      searchTerm,
		TemplateId: template,
		Limit:      10,
	})
	
	if err != nil {
		log.Printf("❌ LibraryService error: %v", err)
		return nil, err
	}

	return &libraryv1.Response{
		Status: "OK",
		Books:  dataResp.Books,
		Meta:   &libraryv1.ResponseMeta{
			TraceId:       msg.Meta.TraceId,
			CanonicalForm: msg.Meta.CanonicalForm,
		},
	}, nil
}

func main() {
	lis, err := net.Listen("tcp", ":50053")
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}
	
	srv := grpc.NewServer()
	conn, err := grpc.Dial("localhost:50051", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("failed to connect to datamanager: %v", err)
	}

	libraryv1.RegisterProcessorServiceServer(srv, &processorServer{
		libraryClient: libraryv1.NewLibraryServiceClient(conn),
	})

	log.Println("🧠 Processor started on :50053")
	srv.Serve(lis)
}
