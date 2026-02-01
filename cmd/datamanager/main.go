package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net"
	"net/http"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"

	"google.golang.org/grpc"
)

// storageServer теперь просто проксирует запросы
type storageServer struct {
	libraryv1.UnimplementedStorageServiceServer
	osBaseURL string
	indexName string
	debug     bool
}

// Структуры для парсинга ответа OpenSearch (минимально необходимые)
type osResponse struct {
	Hits struct {
		Total interface{} `json:"total"` // Может быть числом или объектом {value: N}
		Hits  []struct {
			Source struct {
				Title   string   `json:"title"`
				Authors []string `json:"authors"`
			} `json:"_source"`
			ID string `json:"_id"`
		} `json:"hits"`
	} `json:"hits"`
}

// extractTotal извлекает количество найденных книг, учитывая разные форматы ответов ES/OS
func extractTotal(osRaw *osResponse) int32 {
	var totalValue int32
	switch v := osRaw.Hits.Total.(type) {
	case float64:
		totalValue = int32(v)
	case map[string]interface{}:
		if val, ok := v["value"].(float64); ok {
			totalValue = int32(val)
		}
	}
	return totalValue
}

// buildResponse конвертирует ответ OS в gRPC
func buildResponse(osRaw *osResponse) *libraryv1.SearchResponse {
	totalValue := extractTotal(osRaw)
	res := &libraryv1.SearchResponse{Status: "ok", Total: totalValue}

	for _, hit := range osRaw.Hits.Hits {
		res.Books = append(res.Books, &libraryv1.Book{
			Id:      hit.ID,
			Title:   hit.Source.Title,
			Authors: hit.Source.Authors,
		})
	}
	return res
}

// SearchBooks - единственная точка входа. 
// Больше никакой логики AST, только пересылка JSON.
func (s *storageServer) SearchBooks(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	// 1. Проверяем наличие готового JSON от query-builder
	if req.DebugOpenSearchJson == "" {
		return nil, fmt.Errorf("empty debug_open_search_json")
	}

	// 2. Определяем URL (шаблон или прямой DSL запрос)
	var targetURL string
	switch req.ExecutionType {
	case "TEMPLATE":
		targetURL = fmt.Sprintf("%s/%s/_search/template", s.osBaseURL, s.indexName)
	case "DSL":
		targetURL = fmt.Sprintf("%s/%s/_search", s.osBaseURL, s.indexName)
	default:
		// Фолбэк на обычный поиск
		targetURL = fmt.Sprintf("%s/%s/_search", s.osBaseURL, s.indexName)
	}

	if s.debug {
		log.Printf("📤 [OS-REQ] TYPE=%s URL=%s BODY=%s", req.ExecutionType, targetURL, req.DebugOpenSearchJson)
	}

	// 3. Отправляем запрос в OpenSearch
	resp, err := http.Post(targetURL, "application/json", bytes.NewBuffer([]byte(req.DebugOpenSearchJson)))
	if err != nil {
		log.Printf("❌ HTTP Error: %v", err)
		return &libraryv1.SearchResponse{Status: "error"}, err
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)

	// 4. Обрабатываем ошибки OpenSearch
	if resp.StatusCode >= 400 {
		log.Printf("❌ OpenSearch Error (%d): %s", resp.StatusCode, string(body))
		return &libraryv1.SearchResponse{Status: "error"}, fmt.Errorf("opensearch error: %s", body)
	}

	// 5. Декодируем ответ
	var osRaw osResponse
	if err := json.Unmarshal(body, &osRaw); err != nil {
		log.Printf("❌ JSON Unmarshal Error: %v", err)
		return &libraryv1.SearchResponse{Status: "error"}, err
	}

	if s.debug {
		log.Printf("📥 [OS-RESP] Found: %d books", extractTotal(&osRaw))
	}

	return buildResponse(&osRaw), nil
}

func main() {
	cfg := config.Get()

	// Используем настройки Datamanager
	lis, err := net.Listen(cfg.Datamanager.Protocol, cfg.Datamanager.Address())
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	libraryv1.RegisterStorageServiceServer(s, &storageServer{
		osBaseURL: cfg.OpenSearch.URL,
		indexName: cfg.OpenSearch.IndexName,
		debug:     cfg.OpenSearch.Debug,
	})

	log.Printf("💾 DataManager started on %s (%s)", cfg.Datamanager.Address(), cfg.Datamanager.Protocol)
	
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
