package main

import (
	"context"
	"log"
	"net"
	"strings"

	"ebusta/api/proto/v1"
	"google.golang.org/grpc"
)

type processorServer struct {
	libraryv1.UnimplementedProcessorServiceServer
	storage libraryv1.StorageServiceClient
}

func (s *processorServer) Process(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	fullQuery := req.Query
	qLower := strings.ToLower(fullQuery)
	log.Printf("🧠 Processor: Handling '%s'", fullQuery)

	// 1. Обработка сложных запросов (AND/OR)
	if strings.Contains(qLower, " and ") || strings.Contains(qLower, " or ") {
		// ОЧИСТКА: OpenSearch не знает про наши префиксы author: и title:
		// Мы заменяем их на пустые строки для mixed_search
		cleanQuery := fullQuery
		cleanQuery = strings.ReplaceAll(cleanQuery, "author:", "")
		cleanQuery = strings.ReplaceAll(cleanQuery, "title:", "")
		cleanQuery = strings.ReplaceAll(cleanQuery, "Author:", "")
		cleanQuery = strings.ReplaceAll(cleanQuery, "Title:", "")
		
		log.Printf("🧠 Processor: Complex query cleaned: '%s'", cleanQuery)
		
		subReq := &libraryv1.SearchRequest{
			Query:      strings.TrimSpace(cleanQuery),
			TemplateId: "fl_mixed_search",
			Limit:      req.Limit,
			TraceId:    req.TraceId,
		}
		return s.storage.SearchBooks(ctx, subReq)
	}

	// 2. Обработка простых Smart-префиксов
	subReq := &libraryv1.SearchRequest{
		Limit:   req.Limit,
		TraceId: req.TraceId,
	}

	if strings.HasPrefix(qLower, "author:") {
		subReq.Query = strings.TrimSpace(strings.TrimPrefix(fullQuery, "author:"))
		subReq.TemplateId = "fl_author_exact"
		resp, err := s.storage.SearchBooks(ctx, subReq)
		if err == nil && resp.Total > 0 {
			return resp, nil
		}
		log.Printf("⚠️ Switching to fuzzy for: %s", subReq.Query)
		subReq.TemplateId = "fl_author_fuzzy"
		return s.storage.SearchBooks(ctx, subReq)
	}

	if strings.HasPrefix(qLower, "title:") {
		subReq.Query = strings.TrimSpace(strings.TrimPrefix(fullQuery, "title:"))
		subReq.TemplateId = "fl_title_substring"
		return s.storage.SearchBooks(ctx, subReq)
	}

	// По умолчанию
	return s.storage.SearchBooks(ctx, req)
}

func main() {
	lis, err := net.Listen("tcp", ":50053")
	if err != nil { log.Fatal(err) }
	conn, err := grpc.Dial("localhost:50051", grpc.WithInsecure())
	if err != nil { log.Fatal(err) }
	defer conn.Close()
	s := grpc.NewServer()
	libraryv1.RegisterProcessorServiceServer(s, &processorServer{storage: libraryv1.NewStorageServiceClient(conn)})
	log.Println("🧠 Ebusta Processor started on :50053")
	s.Serve(lis)
}
