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
	storageClient   libraryv1.StorageServiceClient
	converterClient libraryv1.MessageConverterServiceClient
}

func (s *processorServer) Process(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	// 1. AST Analysis
	convResp, err := s.converterClient.Convert(ctx, &libraryv1.RawInput{
		Data:    req.Query,
		TraceId: req.TraceId,
	})

	var targetTemplate string
	var targetQuery string

	if err != nil {
		targetTemplate = "fl_mixed_search"
		targetQuery = basicCleanup(req.Query)
	} else {
		targetTemplate, targetQuery = selectStrategy(convResp.Query, req.Query)
	}

	log.Printf("👉 Strategy 1 (Primary): Template=[%s], Query=[%s]", targetTemplate, targetQuery)

	// 2. Первая попытка поиска
	resp, err := s.storageClient.SearchBooks(ctx, &libraryv1.SearchRequest{
		Query:      targetQuery,
		TemplateId: targetTemplate,
		Limit:      req.Limit,
	})

	// 3. FALLBACK LOGIC (План Б)
	// Если искали точного автора и ничего не нашли -> пробуем нечеткий поиск
	if (err == nil && resp.Total == 0) && targetTemplate == "fl_author_exact" {
		
		log.Printf("⚠️ Primary strategy returned 0 results. Switching to FALLBACK: fl_author_fuzzy")
		
		resp, err = s.storageClient.SearchBooks(ctx, &libraryv1.SearchRequest{
			Query:      targetQuery,
			TemplateId: "fl_author_fuzzy", // <-- Подмена шаблона
			Limit:      req.Limit,
		})
	}

	return resp, err
}

func selectStrategy(ast *libraryv1.SearchQuery, rawQuery string) (string, string) {
	if ast == nil {
		return "fl_mixed_search", basicCleanup(rawQuery)
	}

	switch node := ast.Node.(type) {
	case *libraryv1.SearchQuery_Filter:
		f := node.Filter
		switch f.Field {
		case "author":
			// Сначала пробуем строгий поиск!
			return "fl_author_exact", f.Value
		case "title":
			return "fl_title_substring", f.Value
		default:
			return "fl_mixed_search", f.Value
		}
	default:
		return "fl_mixed_search", basicCleanup(rawQuery)
	}
}

func basicCleanup(q string) string {
	cleaned := q
	cleaned = strings.ReplaceAll(cleaned, "authors:", "")
	cleaned = strings.ReplaceAll(cleaned, "author:", "")
	cleaned = strings.ReplaceAll(cleaned, "title:", "")
	return strings.TrimSpace(cleaned)
}

func main() {
	connStorage, err := grpc.Dial("localhost:50051", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil { log.Fatalf("failed to connect to storage: %v", err) }
	
	connConverter, err := grpc.Dial("localhost:50052", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil { log.Fatalf("failed to connect to converter: %v", err) }

	lis, err := net.Listen("tcp", ":50053")
	if err != nil { log.Fatalf("failed to listen: %v", err) }

	s := grpc.NewServer()
	
	libraryv1.RegisterProcessorServiceServer(s, &processorServer{
		storageClient:   libraryv1.NewStorageServiceClient(connStorage),
		converterClient: libraryv1.NewMessageConverterServiceClient(connConverter),
	})

	log.Println("🧠 Processor started on :50053 (Fallback Logic Enabled)")
	s.Serve(lis)
}
