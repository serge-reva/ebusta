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
	"time"

	"ebusta/api/proto/v1"
	"google.golang.org/grpc"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"
	"github.com/prometheus/client_golang/prometheus/promhttp"
)

var (
	searchRequests = promauto.NewCounterVec(prometheus.CounterOpts{
		Name: "datamanager_search_total",
		Help: "Total searches by template",
	}, []string{"template"})

	searchDuration = promauto.NewHistogramVec(prometheus.HistogramOpts{
		Name:    "datamanager_search_seconds",
		Help:    "Search latency in seconds",
		Buckets: prometheus.DefBuckets,
	}, []string{"template"})
)

const osURL = "http://192.168.1.179:9200"

type server struct {
	libraryv1.UnimplementedLibraryServiceServer
}

func (s *server) SearchBooks(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	start := time.Now()
	
	template := req.TemplateId
	if template == "" {
		template = "fl_mixed_search"
	}

	osReqBody := map[string]interface{}{
		"id": template,
		"params": map[string]interface{}{
			"q":    req.Query,
			"from": req.Offset,
			"size": req.Limit,
		},
	}

	jsonData, _ := json.Marshal(osReqBody)
	resp, err := http.Post(fmt.Sprintf("%s/flibusta_merged_index/_search/template", osURL), "application/json", bytes.NewBuffer(jsonData))
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)
	log.Printf("🔍 [%s] OS Response: %s", template, string(body))

	var osResp struct {
		Hits struct {
			Total struct { Value int32 } `json:"total"`
			Hits  []struct {
				Source struct {
					Title    string   `json:"title"`
					Authors  []string `json:"authors"`
					FileInfo struct {
						Container string `json:"container"`
						Filename  string `json:"filename"`
					} `json:"fileInfo"`
				} `json:"_source"`
				ID string `json:"_id"`
			} `json:"hits"`
		} `json:"hits"`
	}

	json.Unmarshal(body, &osResp)

	res := &libraryv1.SearchResponse{Total: osResp.Hits.Total.Value}
	for _, hit := range osResp.Hits.Hits {
		res.Books = append(res.Books, &libraryv1.Book{
			Id: hit.ID, Title: hit.Source.Title, Authors: hit.Source.Authors,
			Container: hit.Source.FileInfo.Container, Filename: hit.Source.FileInfo.Filename,
		})
	}

	duration := time.Since(start).Seconds()
	searchRequests.WithLabelValues(template).Inc()
	searchDuration.WithLabelValues(template).Observe(duration)

	return res, nil
}

func (s *server) GetAuthors(ctx context.Context, req *libraryv1.ListRequest) (*libraryv1.ListResponse, error) {
	return &libraryv1.ListResponse{}, nil
}

func main() {
	go func() {
		http.Handle("/metrics", promhttp.Handler())
		log.Println("📊 Metrics started on :9091")
		http.ListenAndServe(":9091", nil)
	}()

	lis, _ := net.Listen("tcp", ":50051")
	s := grpc.NewServer()
	libraryv1.RegisterLibraryServiceServer(s, &server{})
	log.Println("📚 Datamanager started on :50051")
	s.Serve(lis)
}
