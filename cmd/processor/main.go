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

	// 1. Сложные запросы (AND/OR)
	if strings.Contains(qLower, " and ") || strings.Contains(qLower, " or ") {
		cleanQuery := fullQuery
		for _, prefix := range []string{"author:", "title:", "Author:", "Title:"} {
			cleanQuery = strings.ReplaceAll(cleanQuery, prefix, "")
		}
		log.Printf("🧠 Processor: Complex query cleaned: '%s'", cleanQuery)
		return s.storage.SearchBooks(ctx, &libraryv1.SearchRequest{
			Query:      strings.TrimSpace(cleanQuery),
			TemplateId: "fl_mixed_search",
			Limit:      req.Limit,
			TraceId:    req.TraceId,
		})
	}

	// 2. Обработка префикса title: (Каскадный поиск)
	if strings.HasPrefix(qLower, "title:") {
		cleanTitle := strings.TrimSpace(strings.TrimPrefix(fullQuery, "title:"))
		
		// Попытка 1: Строгий substring
		subReq := &libraryv1.SearchRequest{
			Query:      cleanTitle,
			TemplateId: "fl_title_substring",
			Limit:      req.Limit,
			TraceId:    req.TraceId,
		}
		resp, err := s.storage.SearchBooks(ctx, subReq)
		
		if err == nil && resp.Total > 0 {
			return resp, nil
		}

		// Попытка 2: Умный Match (анализатор разберется с регистром)
		log.Printf("⚠️ Substring search found 0, switching to fl_title_match for: %s", cleanTitle)
		subReq.TemplateId = "fl_title_match"
		return s.storage.SearchBooks(ctx, subReq)
	}

	// 3. Обработка префикса author: (уже настроена)
	if strings.HasPrefix(qLower, "author:") {
		cleanAuthor := strings.TrimSpace(strings.TrimPrefix(fullQuery, "author:"))
		subReq := &libraryv1.SearchRequest{
			Query:      cleanAuthor,
			TemplateId: "fl_author_exact",
			Limit:      req.Limit,
			TraceId:    req.TraceId,
		}
		resp, err := s.storage.SearchBooks(ctx, subReq)
		if err == nil && resp.Total > 0 {
			return resp, nil
		}
		log.Printf("⚠️ Switching to fuzzy for: %s", cleanAuthor)
		subReq.TemplateId = "fl_author_fuzzy"
		return s.storage.SearchBooks(ctx, subReq)
	}

	return s.storage.SearchBooks(ctx, req)
}

func main() {
	lis, err := net.Listen("tcp", ":50054")
	if err != nil { log.Fatal(err) }
	conn, err := grpc.Dial("localhost:50051", grpc.WithInsecure())
	if err != nil { log.Fatal(err) }
	defer conn.Close()
	s := grpc.NewServer()
	libraryv1.RegisterProcessorServiceServer(s, &processorServer{storage: libraryv1.NewStorageServiceClient(conn)})
	log.Println("🧠 Ebusta Processor started on :50053")
	s.Serve(lis)
}
