package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"sync"
	"syscall"
	"time"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"
	"ebusta/internal/errutil"
	"ebusta/internal/logger"
	"ebusta/internal/metrics"

	"google.golang.org/grpc"
	"google.golang.org/grpc/health"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"
)

type storageServer struct {
	libraryv1.UnimplementedStorageServiceServer
	cfg *config.Config
}

type osSource struct {
	Title    string   `json:"title"`
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

func detectMatchMode(query, execType string) string {
	q := strings.TrimSpace(strings.ToLower(query))
	if strings.Contains(q, "\"") {
		return "exact"
	}
	if strings.HasPrefix(q, "id:") {
		return "exact"
	}
	if strings.EqualFold(strings.TrimSpace(execType), "TEMPLATE") {
		return "broad"
	}
	return "broad"
}

func buildSearchStatus(query, execType string) string {
	mode := strings.ToUpper(strings.TrimSpace(execType))
	if mode == "" {
		mode = "UNKNOWN"
	}
	return fmt.Sprintf("ok;exec=%s;match=%s", mode, detectMatchMode(query, mode))
}

func applyPagination(searchJSON string, limit, offset int32) (string, error) {
	if searchJSON == "" {
		return "", nil
	}

	var payload map[string]interface{}
	if err := json.Unmarshal([]byte(searchJSON), &payload); err != nil {
		return "", err
	}

	if _, isTemplate := payload["id"]; isTemplate {
		params, ok := payload["params"].(map[string]interface{})
		if !ok || params == nil {
			params = map[string]interface{}{}
		}
		if limit > 0 {
			params["size"] = limit
		}
		if offset >= 0 {
			params["from"] = offset
		}
		payload["params"] = params
	} else {
		if limit > 0 {
			payload["size"] = limit
		}
		if offset >= 0 {
			payload["from"] = offset
		}
	}

	out, err := json.Marshal(payload)
	if err != nil {
		return "", err
	}
	return string(out), nil
}

func (s *storageServer) SearchBooks(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	traceID := errutil.TraceIDFromContext(ctx)
	if traceID == "" {
		traceID = errutil.GenerateTraceID("dm")
	}

	searchJSON := req.GetDebugOpenSearchJson()
	if searchJSON == "" {
		return nil, fmt.Errorf("datamanager: debug_open_search_json is empty")
	}

	patchedJSON, err := applyPagination(searchJSON, req.GetLimit(), req.GetOffset())
	if err != nil {
		return nil, fmt.Errorf("datamanager: failed to apply pagination: %w", err)
	}
	searchJSON = patchedJSON

	url := fmt.Sprintf("%s/%s/_search", s.cfg.OpenSearch.URL, s.cfg.OpenSearch.IndexName)

	if bytes.Contains([]byte(searchJSON), []byte(`"id":`)) {
		url = fmt.Sprintf("%s/%s/_search/template", s.cfg.OpenSearch.URL, s.cfg.OpenSearch.IndexName)
	}

	l := logger.GetGlobal().WithField("trace_id", traceID).WithField("url", url).WithField("execution_type", req.GetExecutionType())
	l.InfoCtx(ctx, "[datamanager] forwarding to OpenSearch")

	resp, err := http.Post(url, "application/json", bytes.NewBufferString(searchJSON))
	if err != nil {
		l.ErrorCtx(ctx, "[datamanager] opensearch request failed", err)
		return nil, fmt.Errorf("opensearch request failed: %v", err)
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)
	if resp.StatusCode != http.StatusOK {
		l.WithField("status", resp.StatusCode).WithField("body", string(body)).ErrorCtx(ctx, "[datamanager] opensearch error", nil)
		return nil, fmt.Errorf("opensearch error: %s", string(body))
	}

	var osResp osResponse
	if err := json.Unmarshal(body, &osResp); err != nil {
		l.ErrorCtx(ctx, "[datamanager] failed to decode response", err)
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

	l.WithField("total", osResp.Hits.Total.Value).WithField("returned", len(pbBooks)).InfoCtx(ctx, "[datamanager] search completed")

	return &libraryv1.SearchResponse{
		Status: buildSearchStatus(req.GetQuery(), req.GetExecutionType()),
		Total:  int32(osResp.Hits.Total.Value),
		Books:  pbBooks,
	}, nil
}

func main() {
	cfg := config.Get()
	logger.InitFromConfig(cfg.Logger, "datamanager")
	if err := cfg.Datamanager.Validate(); err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "datamanager config validation failed", err)
	}
	if err := cfg.OpenSearch.Validate(); err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "opensearch config validation failed", err)
	}
	if err := cfg.Metrics.Validate(); err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "metrics config validation failed", err)
	}
	metricsSrv := metrics.Start("datamanager", cfg.Metrics.Services.Datamanager)

	lis, err := net.Listen("tcp", cfg.Datamanager.ListenAddress())
	if err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "failed to listen", err)
	}

	grpcOpts := []grpc.ServerOption{}
	if cfg.Datamanager.MTLS.Enabled {
		creds, tlsErr := cfg.Datamanager.MTLS.ServerTransportCredentials()
		if tlsErr != nil {
			logger.GetGlobal().FatalCtx(context.Background(), "failed to configure datamanager mTLS", tlsErr)
		}
		grpcOpts = append(grpcOpts, grpc.Creds(creds))
	}
	s := grpc.NewServer(grpcOpts...)
	hs := health.NewServer()
	hs.SetServingStatus("", healthpb.HealthCheckResponse_SERVING)
	healthpb.RegisterHealthServer(s, hs)
	libraryv1.RegisterStorageServiceServer(s, &storageServer{cfg: cfg})

	logger.GetGlobal().WithField("addr", cfg.Datamanager.ListenAddress()).InfoCtx(context.Background(), "[datamanager] started")
	serveErr := make(chan error, 1)
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		if err := s.Serve(lis); err != nil {
			serveErr <- err
		}
	}()

	stop := make(chan os.Signal, 1)
	signal.Notify(stop, os.Interrupt, syscall.SIGTERM)

	select {
	case sig := <-stop:
		logger.GetGlobal().WithField("signal", sig).InfoCtx(context.Background(), "[datamanager] shutting down")
		mctx, mcancel := context.WithTimeout(context.Background(), 5*time.Second)
		metrics.Shutdown(mctx, metricsSrv)
		mcancel()
		done := make(chan struct{})
		go func() {
			s.GracefulStop()
			close(done)
		}()
		select {
		case <-done:
		case <-time.After(10 * time.Second):
			logger.GetGlobal().WarnCtx(context.Background(), "[datamanager] graceful stop timeout, forcing stop")
			s.Stop()
		}
	case err := <-serveErr:
		logger.GetGlobal().FatalCtx(context.Background(), "failed to serve", err)
	}

	wg.Wait()
	logger.GetGlobal().InfoCtx(context.Background(), "[datamanager] stopped")
}
