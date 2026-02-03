package main

import (
	"context"
	"log"
	"net"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

type orchestratorServer struct {
	libraryv1.UnimplementedOrchestratorServiceServer
	dslClient libraryv1.DslTransformerClient
	qbClient  libraryv1.QueryBuilderClient
	dmClient  libraryv1.StorageServiceClient
}

func (s *orchestratorServer) Search(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	log.Printf("🎼 Search request: %s", req.GetQuery())

	// 1. Поход в Scala DSL (Трансформация строки в AST)
	dslResp, err := s.dslClient.Transform(ctx, &libraryv1.DslRequest{
		Query: req.GetQuery(),
	})
	if err != nil {
		log.Printf("❌ DSL Error: %v", err)
		return nil, err
	}

	// 2. Поход в Query Builder (Генерация JSON для OpenSearch)
	qbResp, err := s.qbClient.Build(ctx, &libraryv1.BuildRequest{
		Ast:  dslResp.GetAst(),
		Size: req.GetLimit(),
	})
	if err != nil {
		log.Printf("❌ QueryBuilder Error: %v", err)
		return nil, err
	}

	// 3. Поход в DataManager с готовым JSON запросом
	return s.dmClient.SearchBooks(ctx, &libraryv1.SearchRequest{
		Query:               req.GetQuery(),
		Ast:                 dslResp.GetAst(),
		Limit:               req.GetLimit(),
		DebugOpenSearchJson: qbResp.GetBodyJson(), // Исправлено: GetBodyJson вместо GetJsonQuery
	})
}

func main() {
	cfg := config.Get()

	// Коннекты к сервисам через новый конфиг 
	dslConn, _ := grpc.Dial(cfg.DslScala.Address(), grpc.WithTransportCredentials(insecure.NewCredentials()))
	qbConn, _ := grpc.Dial(cfg.QueryBuilder.Address(), grpc.WithTransportCredentials(insecure.NewCredentials()))
	dmConn, _ := grpc.Dial(cfg.Datamanager.Address(), grpc.WithTransportCredentials(insecure.NewCredentials()))

	lis, err := net.Listen("tcp", cfg.Orchestrator.Address())
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	libraryv1.RegisterOrchestratorServiceServer(s, &orchestratorServer{
		dslClient: libraryv1.NewDslTransformerClient(dslConn),
		qbClient:  libraryv1.NewQueryBuilderClient(qbConn),
		dmClient:  libraryv1.NewStorageServiceClient(dmConn),
	})

	log.Printf("🚀 Orchestrator (Full Chain) started on %s", cfg.Orchestrator.Address())
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
