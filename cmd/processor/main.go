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
	log.Printf("🧠 Processor received raw: '%s'", req.Query)

	// ШАГ 1: Обращаемся к "Мозгам" (Message Converter)
	// Мы отправляем сырой текст, чтобы получить AST
	convResp, err := s.converterClient.Convert(ctx, &libraryv1.RawInput{
		Data:    req.Query,
		TraceId: req.TraceId,
	})

	var finalQuery string

	if err != nil {
		log.Printf("⚠️ Converter failed (fallback to basic): %v", err)
		// Fallback: старая логика, если конвертер упал
		finalQuery = basicCleanup(req.Query)
	} else {
		// УСПЕХ: Мы получили AST!
		// Пока что мы просто логируем план запроса, чтобы убедиться, что Plan B работает.
		log.Printf("🧩 AST Analysis Success! Plan: %s", convResp.Meta.AstPlan)
		
		// В будущем здесь будет сложная логика трансформации AST -> Elastic Query
		// Пока берем каноническую форму или просто очищенный запрос
		finalQuery = basicCleanup(req.Query) 
	}

	// ШАГ 2: Отправляем в Хранилище (Storage)
	return s.storageClient.SearchBooks(ctx, &libraryv1.SearchRequest{
		Query: finalQuery,
		Limit: req.Limit, // Пробрасываем лимит от клиента
	})
}

// Простая функция очистки (как было раньше)
func basicCleanup(q string) string {
	cleaned := q
	cleaned = strings.ReplaceAll(cleaned, "authors:", "")
	cleaned = strings.ReplaceAll(cleaned, "author:", "")
	cleaned = strings.ReplaceAll(cleaned, "title:", "")
	return strings.TrimSpace(cleaned)
}

func main() {
	// 1. Подключаемся к STORAGE (:50051)
	connStorage, err := grpc.Dial("localhost:50051", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("failed to connect to storage: %v", err)
	}
	defer connStorage.Close()

	// 2. Подключаемся к CONVERTER (:50052)
	connConverter, err := grpc.Dial("localhost:50052", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("failed to connect to converter: %v", err)
	}
	defer connConverter.Close()

	// 3. Запускаем сервер PROCESSOR (:50053)
	lis, err := net.Listen("tcp", ":50053")
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	
	libraryv1.RegisterProcessorServiceServer(s, &processorServer{
		storageClient:   libraryv1.NewStorageServiceClient(connStorage),
		converterClient: libraryv1.NewMessageConverterServiceClient(connConverter),
	})

	log.Println("🧠 Processor started on :50053 (with Brains connected)")
	s.Serve(lis)
}
