package main

import (
	"context"
	"fmt"
	"log"
	"net"
	"net/http"
	"sync/atomic"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

var orchestratorRequestsTotal uint64

type orchestratorServer struct {
	libraryv1.UnimplementedOrchestratorServiceServer
	dslClient     libraryv1.MessageConverterClient
	storageClient libraryv1.StorageServiceClient
}

func (s *orchestratorServer) Search(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	atomic.AddUint64(&orchestratorRequestsTotal, 1)
	log.Printf("🎼 Orchestrator received: %s", req.Query)

	log.Printf("🎼 Orchestrator -> DSL-Converter")
	ast, err := s.dslClient.Convert(ctx, &libraryv1.ConvertRequest{
		RawQuery: req.Query,
	})
	if err != nil {
		log.Printf("❌ DSL Error: %v", err)
		return nil, err
	}

	log.Printf("🎼 DSL CanonicalForm: %s", ast.GetCanonicalForm())

	// Orchestrator owns the DTO: raw query stays raw; structured AST goes separately.
	searchReq := &libraryv1.SearchRequest{
		Query:      req.Query,
		Ast:        ast,
		TemplateId: req.TemplateId,
		Limit:      req.Limit,
		Offset:     req.Offset,
		TraceId:    req.TraceId,
	}

	log.Printf("🎼 Orchestrator -> Storage (DataManager)")
	return s.storageClient.SearchBooks(ctx, searchReq)
}

func main() {
	cfg := config.Get()

	orchAddr := cfg.Orchestrator.Address()
	log.Printf("=== [ORCHESTRATOR] Starting on %s ===", orchAddr)

	dslAddr := cfg.LispConverter.Address()
	storageAddr := cfg.Datamanager.Address()

	dslConn, err := grpc.Dial(dslAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("failed to connect to dsl: %v", err)
	}

	storageConn, err := grpc.Dial(storageAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("failed to connect to storage: %v", err)
	}

	lis, err := net.Listen(cfg.Orchestrator.Protocol, orchAddr)
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	libraryv1.RegisterOrchestratorServiceServer(s, &orchestratorServer{
		dslClient:     libraryv1.NewMessageConverterClient(dslConn),
		storageClient: libraryv1.NewStorageServiceClient(storageConn),
	})

	log.Println("🎼 Orchestrator service registered")

	// Prometheus text endpoint on dedicated HTTP port
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
		addr := fmt.Sprintf(":%d", cfg.Metrics.Port)
		log.Printf("📈 Metrics listening on %s/metrics", addr)
		if err := http.ListenAndServe(addr, mux); err != nil && err != http.ErrServerClosed {
			log.Printf("metrics serve error: %v", err)
		}
	}()

	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
