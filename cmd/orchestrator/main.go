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
	
	// Клиенты к микросервисам
	dslClient     libraryv1.DslTransformerClient
	qbClient      libraryv1.QueryBuilderClient
	storageClient libraryv1.StorageServiceClient
}

func (s *orchestratorServer) Search(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	atomic.AddUint64(&orchestratorRequestsTotal, 1)
	log.Printf("🎼 [1/4] Orchestrator received: '%s' (limit=%d)", req.Query, req.Limit)

	// --- ШАГ 1: DSL Service (Text -> AST) ---
	log.Printf("🎼 [2/4] Calling DSL Service...")
	dslResp, err := s.dslClient.Transform(ctx, &libraryv1.DslRequest{
		Query: req.Query,
	})
	if err != nil {
		log.Printf("❌ DSL RPC Error: %v", err)
		return nil, err
	}
	if !dslResp.IsSuccess {
		log.Printf("⚠️ DSL Logic Error: %s", dslResp.ErrorMsg)
		// Возвращаем пустой ответ с ошибкой в статусе (или можно вернуть error gRPC)
		return &libraryv1.SearchResponse{Status: "error_dsl: " + dslResp.ErrorMsg, Total: 0}, nil
	}

	// --- ШАГ 2: Query Builder (AST -> JSON) ---
	log.Printf("🎼 [3/4] Calling Query Builder...")
	qbResp, err := s.qbClient.Build(ctx, &libraryv1.BuildRequest{
		Ast:  dslResp.Ast,
		Size: req.Limit,
		From: req.Offset,
	})
	if err != nil {
		log.Printf("❌ QB RPC Error: %v", err)
		return nil, err
	}
	if !qbResp.IsSuccess {
		log.Printf("⚠️ QB Logic Error: %s", qbResp.ErrorMsg)
		return &libraryv1.SearchResponse{Status: "error_qb: " + qbResp.ErrorMsg, Total: 0}, nil
	}

	// Определяем тип выполнения для логов/отладки
	execType := "DSL"
	if qbResp.Type == libraryv1.QueryType_TEMPLATE {
		execType = "TEMPLATE"
	}
	log.Printf("✅ QB generated JSON (type=%s, len=%d)", execType, len(qbResp.BodyJson))

	// --- ШАГ 3: Data Manager (JSON -> Books) ---
	log.Printf("🎼 [4/4] Calling Data Manager...")
	
	// DataManager теперь выступает как глупый исполнитель JSON-запроса
	dmReq := &libraryv1.SearchRequest{
		DebugOpenSearchJson: qbResp.BodyJson, // Ключевое поле
		ExecutionType:       execType,
		TraceId:             req.TraceId,
	}

	return s.storageClient.SearchBooks(ctx, dmReq)
}

func main() {
	cfg := config.Get()

	orchAddr := cfg.Orchestrator.Address()
	log.Printf("=== [ORCHESTRATOR] Starting on %s ===", orchAddr)

	// Получаем адреса зависимых сервисов из обновленного конфига
	dslAddr := cfg.DslScala.Address()
	qbAddr := cfg.QueryBuilder.Address()
	storageAddr := cfg.Datamanager.Address()

	log.Printf("    -> DSL Service:  %s", dslAddr)
	log.Printf("    -> Query Builder: %s", qbAddr)
	log.Printf("    -> Data Manager:  %s", storageAddr)

	opts := []grpc.DialOption{grpc.WithTransportCredentials(insecure.NewCredentials())}

	// 1. Подключение к DSL
	dslConn, err := grpc.Dial(dslAddr, opts...)
	if err != nil {
		log.Fatalf("failed to connect to dsl: %v", err)
	}

	// 2. Подключение к Query Builder
	qbConn, err := grpc.Dial(qbAddr, opts...)
	if err != nil {
		log.Fatalf("failed to connect to qb: %v", err)
	}

	// 3. Подключение к Data Manager
	storageConn, err := grpc.Dial(storageAddr, opts...)
	if err != nil {
		log.Fatalf("failed to connect to storage: %v", err)
	}

	lis, err := net.Listen(cfg.Orchestrator.Protocol, orchAddr)
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	libraryv1.RegisterOrchestratorServiceServer(s, &orchestratorServer{
		dslClient:     libraryv1.NewDslTransformerClient(dslConn),
		qbClient:      libraryv1.NewQueryBuilderClient(qbConn),
		storageClient: libraryv1.NewStorageServiceClient(storageConn),
	})

	log.Println("🎼 Orchestrator service registered")

	// Метрики
	go func() {
		mux := http.NewServeMux()
		mux.HandleFunc("/metrics", func(w http.ResponseWriter, r *http.Request) {
			w.Header().Set("Content-Type", "text/plain; version=0.0.4; charset=utf-8")
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
