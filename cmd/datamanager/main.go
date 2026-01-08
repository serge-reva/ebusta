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
	"os"

	"ebusta/api/proto/v1"
	"github.com/spf13/viper"
	"google.golang.org/grpc"
)

type storageServer struct {
	libraryv1.UnimplementedStorageServiceServer
	osBaseURL string
	indexName string
	debug     bool // Флаг отладки
}

func (s *storageServer) SearchBooks(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	templateID := req.TemplateId
	if templateID == "" {
		templateID = "fl_mixed_search"
	}
	
	// === ЛОГИКА ВЫБОРА ПАРАМЕТРА ===
	var paramName string
	switch templateID {
	case "fl_author_exact", "fl_author_fuzzy":
		paramName = "author"
	case "fl_title_substring", "fl_titles_all":
		paramName = "q"
	default:
		paramName = "q"
	}

	// Формируем тело запроса
	osReqBody := map[string]interface{}{
		"id": templateID,
		"params": map[string]interface{}{
			paramName: req.Query,
			"from":    0,
			"size":    req.Limit,
		},
	}
	
	if osReqBody["params"].(map[string]interface{})["size"] == int32(0) {
		osReqBody["params"].(map[string]interface{})["size"] = 10
	}

	jsonData, _ := json.Marshal(osReqBody)
	targetURL := fmt.Sprintf("%s/%s/_search/template", s.osBaseURL, s.indexName)

	// 🔥 DEBUG: Логируем запрос (если включен режим или всегда, для надежности сейчас оставим всегда)
	log.Printf("📤 [OS-REQ] URL: %s | BODY: %s", targetURL, string(jsonData))

	resp, err := http.Post(targetURL, "application/json", bytes.NewBuffer(jsonData))
	if err != nil {
		log.Printf("❌ Storage connection error: %v", err)
		return nil, err
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)
	
	// 🔥 DEBUG: Логируем ответ
	// Обрезаем ответ, если он слишком огромный, чтобы не засорять консоль совсем уж жестко
	debugBody := string(body)
	if len(debugBody) > 1000 {
		debugBody = debugBody[:1000] + "... (truncated)"
	}
	log.Printf("📥 [OS-RESP] %s", debugBody)

	var osResp struct {
		Hits struct {
			Total struct { Value int32 `json:"value"` } `json:"total"`
			Hits []struct {
				Source struct {
					Title    string   `json:"title"`
					Authors  []string `json:"authors"`
				} `json:"_source"`
				ID string `json:"_id"`
			} `json:"hits"`
		} `json:"hits"`
	}

	if err := json.Unmarshal(body, &osResp); err != nil {
		log.Printf("❌ Storage parse error: %v | Body: %s", err, string(body))
		return &libraryv1.SearchResponse{Status: "error"}, nil
	}

	res := &libraryv1.SearchResponse{Total: osResp.Hits.Total.Value}
	for _, hit := range osResp.Hits.Hits {
		res.Books = append(res.Books, &libraryv1.Book{
			Id:      hit.ID,
			Title:   hit.Source.Title,
			Authors: hit.Source.Authors,
		})
	}
	return res, nil
}

func main() {
	viper.SetConfigName("ebusta")
	viper.SetConfigType("yaml")
	viper.AddConfigPath(".")
	viper.ReadInConfig()

	osBaseURL := viper.GetString("datamanager.opensearch_url")
	indexName := viper.GetString("datamanager.index_name")
	
	// Проверяем ENV переменную DEBUG
	debug := os.Getenv("DEBUG") != ""

	lis, err := net.Listen("tcp", ":50051")
	if err != nil { log.Fatalf("failed to listen: %v", err) }

	s := grpc.NewServer()
	
	libraryv1.RegisterStorageServiceServer(s, &storageServer{
		osBaseURL: osBaseURL,
		indexName: indexName,
		debug:     debug,
	})

	log.Println("💾 DataManager (Storage) started on :50051 (Debug Logs Enabled)")
	s.Serve(lis)
}
