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

	"ebusta/api/proto/v1"
	"github.com/spf13/viper"
	"google.golang.org/grpc"
)

type storageServer struct {
	libraryv1.UnimplementedStorageServiceServer
	osBaseURL string
	indexName string
}

func (s *storageServer) SearchBooks(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	template := "fl_mixed_search"
	log.Printf("💾 Storage searching: %s", req.Query)

	osReqBody := map[string]interface{}{
		"id": template,
		"params": map[string]interface{}{
			"q":    req.Query,
			"from": 0,
			"size": req.Limit,
		},
	}
	// Default limit override
	if osReqBody["params"].(map[string]interface{})["size"] == int32(0) {
		osReqBody["params"].(map[string]interface{})["size"] = 10
	}

	jsonData, _ := json.Marshal(osReqBody)
	targetURL := fmt.Sprintf("%s/%s/_search/template", s.osBaseURL, s.indexName)

	resp, err := http.Post(targetURL, "application/json", bytes.NewBuffer(jsonData))
	if err != nil {
		log.Printf("❌ Storage connection error: %v", err)
		return nil, err
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)
	
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
		log.Printf("❌ Storage parse error: %v", err)
		return &libraryv1.SearchResponse{}, nil
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

	lis, err := net.Listen("tcp", ":50051")
	if err != nil { log.Fatalf("failed to listen: %v", err) }

	s := grpc.NewServer()
	
	libraryv1.RegisterStorageServiceServer(s, &storageServer{
		osBaseURL: osBaseURL,
		indexName: indexName,
	})

	log.Println("💾 DataManager (Storage) started on :50051")
	s.Serve(lis)
}
