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

type storageServer struct {
	libraryv1.UnimplementedStorageServiceServer
	cfg *config.Config
}

type osSource struct {
	Title    string `json:"title"`
	Authors  []string `json:"authors"`
	FileInfo struct {
		Container string `json:"container"`
		Filename  string `json:"filename"`
	} `json:"fileInfo"`
}

type osHit struct {
	ID     string   `json:"_id"`
	Source osSource `json:"_source"`
}

type osResponse struct {
	Hits struct {
		Total struct {
			Value int `json:"value"`
		} `json:"total"`
		Hits []osHit `json:"hits"`
	} `json:"hits"`
}

func (s *storageServer) SearchBooks(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	searchJSON := req.GetDebugOpenSearchJson()
	if searchJSON == "" {
		return nil, fmt.Errorf("datamanager: debug_open_search_json is empty")
	}

	// По умолчанию идем на /_search
	url := fmt.Sprintf("%s/%s/_search", s.cfg.OpenSearch.URL, s.cfg.OpenSearch.IndexName)
	
	// Если в JSON есть "id", значит QueryBuilder подготовил вызов шаблона
	if bytes.Contains([]byte(searchJSON), []byte(`"id":`)) {
		url = fmt.Sprintf("%s/%s/_search/template", s.cfg.OpenSearch.URL, s.cfg.OpenSearch.IndexName)
	}

	resp, err := http.Post(url, "application/json", bytes.NewBufferString(searchJSON))
	if err != nil {
		return nil, fmt.Errorf("opensearch request failed: %v", err)
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("opensearch error: %s", string(body))
	}

	var osResp osResponse
	if err := json.Unmarshal(body, &osResp); err != nil {
		return nil, fmt.Errorf("failed to decode response: %v", err)
	}

	pbBooks := make([]*libraryv1.Book, 0, len(osResp.Hits.Hits))
	for _, hit := range osResp.Hits.Hits {
		pbBooks = append(pbBooks, &libraryv1.Book{
			Id:        hit.ID,
			Title:     hit.Source.Title,
			Authors:   hit.Source.Authors,
			Container: hit.Source.FileInfo.Container,
			Filename:  hit.Source.FileInfo.Filename,
		})
	}

	return &libraryv1.SearchResponse{
		Status: "ok",
		Total:  int32(osResp.Hits.Total.Value),
		Books:  pbBooks,
	}, nil
}

func main() {
	cfg := config.Get()
	lis, err := net.Listen("tcp", cfg.Datamanager.Address())
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	libraryv1.RegisterStorageServiceServer(s, &storageServer{cfg: cfg})

	log.Printf("💾 DataManager (with Template support) started on %s", cfg.Datamanager.Address())
	s.Serve(lis)
}
