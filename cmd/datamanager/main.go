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
	debug     bool
}

func (s *storageServer) SearchBooks(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	templateID := req.TemplateId
	if templateID == "" {
		templateID = "fl_mixed_search"
	}
	
	var paramName string
	switch templateID {
	case "fl_author_exact", "fl_author_fuzzy":
		paramName = "author"
	case "fl_title_substring", "fl_titles_all":
		paramName = "query"
	default:
		paramName = "query"
	}

	osReqBody := map[string]interface{}{
		"id": templateID,
		"params": map[string]interface{}{
			paramName: req.Query,
			"from":    0,
			"size":    req.Limit,
		},
	}
	
	if val, ok := osReqBody["params"].(map[string]interface{})["size"].(int32); ok && val == 0 {
		osReqBody["params"].(map[string]interface{})["size"] = 10
	}

	jsonData, _ := json.Marshal(osReqBody)
	targetURL := fmt.Sprintf("%s/%s/_search/template", s.osBaseURL, s.indexName)
	log.Printf("📤 [OS-REQ] URL: %s | BODY: %s", targetURL, string(jsonData))

	resp, err := http.Post(targetURL, "application/json", bytes.NewBuffer(jsonData))
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)
	
	// ГИБКИЙ ПАРСИНГ: Total может быть числом, объектом или отсутствовать
	var osRaw struct {
		Hits struct {
			Total interface{} `json:"total"`
			Hits  []struct {
				Source struct {
					Title   string   `json:"title"`
					Authors []string `json:"authors"`
				} `json:"_source"`
				ID string `json:"_id"`
			} `json:"hits"`
		} `json:"hits"`
	}

	if err := json.Unmarshal(body, &osRaw); err != nil {
		log.Printf("❌ Storage parse error: %v", err)
		return &libraryv1.SearchResponse{Status: "error"}, nil
	}

	var totalValue int32
	switch v := osRaw.Hits.Total.(type) {
	case float64:
		totalValue = int32(v)
	case map[string]interface{}:
		if val, ok := v["value"].(float64); ok {
			totalValue = int32(val)
		}
	}

	res := &libraryv1.SearchResponse{}
	for _, hit := range osRaw.Hits.Hits {
		res.Books = append(res.Books, &libraryv1.Book{
			Id:      hit.ID,
			Title:   hit.Source.Title,
			Authors: hit.Source.Authors,
		})
	}

	// FALLBACK: Если хиты есть, а total 0 или не распарсился
	if totalValue == 0 && len(res.Books) > 0 {
		totalValue = int32(len(res.Books))
	}
	res.Total = totalValue

	log.Printf("📥 [OS-RESP] Found: %d books", totalValue)
	return res, nil
}

func main() {
	viper.SetConfigName("ebusta")
	viper.SetConfigType("yaml")
	viper.AddConfigPath(".")
	viper.ReadInConfig()

	osBaseURL := viper.GetString("datamanager.opensearch_url")
	indexName := viper.GetString("datamanager.index_name")
	debug := os.Getenv("DEBUG") != ""

	lis, err := net.Listen("tcp", ":50051")
	if err != nil { log.Fatalf("failed to listen: %v", err) }

	s := grpc.NewServer()
	libraryv1.RegisterStorageServiceServer(s, &storageServer{
		osBaseURL: osBaseURL,
		indexName: indexName,
		debug:     debug,
	})

	log.Println("💾 DataManager (Storage) started on :50051")
	s.Serve(lis)
}
