package main

import (
	"context"
	"fmt"
	"log"
	"net"
	"net/http"
	"sync/atomic"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"

	dsl "ebusta/api/gen/dsl"
	libraryv1 "ebusta/api/proto/v1"
)

var orchestratorRequestsTotal uint64

type orchestratorServer struct {
	libraryv1.UnimplementedOrchestratorServiceServer
	dslClient     dsl.MessageConverterClient
	storageClient libraryv1.StorageServiceClient
}

func (s *orchestratorServer) Search(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	atomic.AddUint64(&orchestratorRequestsTotal, 1)
	log.Printf("🎼 Orchestrator received: %s", req.Query)

	log.Printf("🎼 Orchestrator -> DSL-Converter")
	dslResp, err := s.dslClient.Convert(ctx, &dsl.ConvertRequest{
		RawQuery: req.Query,
	})

	if err != nil {
		log.Printf("❌ DSL Error: %v", err)
		return s.storageClient.SearchBooks(ctx, req)
	}

	log.Printf("✅ DSL Parsed: %s", dslResp.CanonicalForm)

	searchReq := &libraryv1.SearchRequest{
		Query:      dslResp.CanonicalForm,
		TemplateId: req.TemplateId,
		Limit:      req.Limit,
		Offset:     req.Offset,
		TraceId:    req.TraceId,
	}

	return s.storageClient.SearchBooks(ctx, searchReq)
}

func main() {
	log.Println("=== [ORCHESTRATOR] Starting on :50053 ===")

	dslConn, err := grpc.Dial("localhost:50052", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("failed to connect to dsl: %v", err)
	}

	storageConn, err := grpc.Dial("localhost:50051", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("failed to connect to storage: %v", err)
	}

	lis, err := net.Listen("tcp", ":50053")
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	libraryv1.RegisterOrchestratorServiceServer(s, &orchestratorServer{
		dslClient:     dsl.NewMessageConverterClient(dslConn),
		storageClient: libraryv1.NewStorageServiceClient(storageConn),
	})

	log.Println("🎼 Orchestrator service registered")

	// METRICS_50090: Prometheus text endpoint on dedicated HTTP port
	go func() {
		mux := http.NewServeMux()
		mux.HandleFunc("/metrics", func(w http.ResponseWriter, r *http.Request) {
			w.Header().Set("Content-Type", "text/plain; version=0.0.4; charset=utf-8")
			fmt.Fprintln(w, "# HELP orchestrator_up 1 if orchestrator process is running")
			fmt.Fprintln(w, "# TYPE orchestrator_up gauge")
			fmt.Fprintln(w, "orchestrator_up 1")
			fmt.Fprintln(w, "# HELP orchestrator_requests_total Total Search requests handled")
			fmt.Fprintln(w, "# TYPE orchestrator_requests_total counter")
			fmt.Fprintf(w, "orchestrator_requests_total %d\n", atomic.LoadUint64(&orchestratorRequestsTotal))
		})
		addr := ":50090"
		log.Printf("📈 Metrics listening on %s/metrics", addr)
		if err := http.ListenAndServe(addr, mux); err != nil && err != http.ErrServerClosed {
			log.Printf("metrics serve error: %v", err)
		}
	}()

	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
