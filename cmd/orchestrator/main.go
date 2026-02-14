package main

import (
    "context"
    "log"
    "net"

    libraryv1 "ebusta/api/proto/v1"
    "ebusta/internal/config"
    "ebusta/internal/errutil"

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
    // Извлекаем TraceID из контекста
    traceID := errutil.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("orch")
    }

    log.Printf("[%s] 🎼 Search request: %s", traceID, req.GetQuery())

    // 1. DSL Transformer
    dslResp, err := s.dslClient.Transform(ctx, &libraryv1.DslRequest{
        Query: req.GetQuery(),
    })
    if err != nil {
        appErr := errutil.FromGRPCError(err, traceID)
        log.Printf("[%s] DSL connection error: %v", traceID, err)
        return nil, errutil.ToGRPCError(appErr)
    }
    if !dslResp.GetIsSuccess() {
        appErr := errutil.New(errutil.CodeInvalidArgument, dslResp.GetErrorMsg()).WithTrace(traceID)
        log.Printf("[%s] DSL parse error: %s", traceID, dslResp.GetErrorMsg())
        return nil, errutil.ToGRPCError(appErr)
    }

    // 2. Query Builder
    qbResp, err := s.qbClient.Build(ctx, &libraryv1.BuildRequest{
        Ast:  dslResp.GetAst(),
        Size: req.GetLimit(),
    })
    if err != nil {
        appErr := errutil.FromGRPCError(err, traceID)
        log.Printf("[%s] QueryBuilder connection error: %v", traceID, err)
        return nil, errutil.ToGRPCError(appErr)
    }
    if !qbResp.GetIsSuccess() {
        appErr := errutil.New(errutil.CodeInternal, qbResp.GetErrorMsg()).WithTrace(traceID)
        log.Printf("[%s] QueryBuilder build error: %s", traceID, qbResp.GetErrorMsg())
        return nil, errutil.ToGRPCError(appErr)
    }

    // 3. Определяем тип исполнения (GetType вместо GetQueryType)
    execType := "DSL"
    if qbResp.GetType() == libraryv1.QueryType_TEMPLATE {
        execType = "TEMPLATE"
    }

    // 4. Data Manager
    resp, err := s.dmClient.SearchBooks(ctx, &libraryv1.SearchRequest{
        Query:               req.GetQuery(),
        Ast:                 dslResp.GetAst(),
        Limit:               req.GetLimit(),
        DebugOpenSearchJson: qbResp.GetBodyJson(),
        ExecutionType:       execType,
    })
    if err != nil {
        appErr := errutil.FromGRPCError(err, traceID)
        log.Printf("[%s] DataManager error: %v", traceID, err)
        return nil, errutil.ToGRPCError(appErr)
    }

    log.Printf("[%s] ✅ Search complete: %d results", traceID, len(resp.GetBooks()))
    return resp, nil
}

func main() {
    cfg := config.Get()

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

    log.Printf("🚀 Orchestrator with errutil started on %s", cfg.Orchestrator.Address())
    if err := s.Serve(lis); err != nil {
        log.Fatalf("failed to serve: %v", err)
    }
}
