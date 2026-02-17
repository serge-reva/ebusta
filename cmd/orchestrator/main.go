package main

import (
    "context"
    "net"

    libraryv1 "ebusta/api/proto/v1"
    "ebusta/internal/config"
    "ebusta/internal/errutil"
    "ebusta/internal/logger"

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
    traceID := errutil.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("orch")
    }

    logger.GetGlobal().WithField("query", req.GetQuery()).
        WithField("trace_id", traceID).
        InfoCtx(ctx, "[orchestrator] search request")

    dslResp, err := s.dslClient.Transform(ctx, &libraryv1.DslRequest{
        Query: req.GetQuery(),
    })
    if err != nil {
        appErr := errutil.FromGRPCError(err, traceID)
        logger.GetGlobal().WithField("query", req.GetQuery()).
            ErrorCtx(ctx, "[orchestrator] DSL connection error", err)
        return nil, errutil.ToGRPCError(appErr)
    }
    if !dslResp.GetIsSuccess() {
        appErr := errutil.New(errutil.CodeInvalidArgument, dslResp.GetErrorMsg()).WithTrace(traceID)
        logger.GetGlobal().WithField("error", dslResp.GetErrorMsg()).
            WithField("query", req.GetQuery()).
            WarnCtx(ctx, "[orchestrator] DSL parse error")
        return nil, errutil.ToGRPCError(appErr)
    }

    qbResp, err := s.qbClient.Build(ctx, &libraryv1.BuildRequest{
        Ast:  dslResp.GetAst(),
        Size: req.GetLimit(),
    })
    if err != nil {
        appErr := errutil.FromGRPCError(err, traceID)
        logger.GetGlobal().ErrorCtx(ctx, "[orchestrator] QueryBuilder connection error", err)
        return nil, errutil.ToGRPCError(appErr)
    }
    if !qbResp.GetIsSuccess() {
        appErr := errutil.New(errutil.CodeInternal, qbResp.GetErrorMsg()).WithTrace(traceID)
        logger.GetGlobal().WithField("error", qbResp.GetErrorMsg()).
            ErrorCtx(ctx, "[orchestrator] QueryBuilder build error", nil)
        return nil, errutil.ToGRPCError(appErr)
    }

    execType := "DSL"
    if qbResp.GetType() == libraryv1.QueryType_TEMPLATE {
        execType = "TEMPLATE"
    }

    resp, err := s.dmClient.SearchBooks(ctx, &libraryv1.SearchRequest{
        Query:               req.GetQuery(),
        Ast:                 dslResp.GetAst(),
        Limit:               req.GetLimit(),
        DebugOpenSearchJson: qbResp.GetBodyJson(),
        ExecutionType:       execType,
    })
    if err != nil {
        appErr := errutil.FromGRPCError(err, traceID)
        logger.GetGlobal().ErrorCtx(ctx, "[orchestrator] DataManager error", err)
        return nil, errutil.ToGRPCError(appErr)
    }

    logger.GetGlobal().WithField("results", len(resp.GetBooks())).
        WithField("trace_id", traceID).
        InfoCtx(ctx, "[orchestrator] search complete")
    return resp, nil
}

func main() {
    cfg := config.Get()
    logger.InitFromConfig(cfg.Logger, "orchestrator")

    dslConn, _ := grpc.Dial(cfg.DslScala.Address(), grpc.WithTransportCredentials(insecure.NewCredentials()))
    qbConn, _ := grpc.Dial(cfg.QueryBuilder.Address(), grpc.WithTransportCredentials(insecure.NewCredentials()))
    dmConn, _ := grpc.Dial(cfg.Datamanager.Address(), grpc.WithTransportCredentials(insecure.NewCredentials()))

    lis, err := net.Listen("tcp", cfg.Orchestrator.Address())
    if err != nil {
        logger.GetGlobal().FatalCtx(context.Background(), "failed to listen", err)
    }

    s := grpc.NewServer()
    libraryv1.RegisterOrchestratorServiceServer(s, &orchestratorServer{
        dslClient: libraryv1.NewDslTransformerClient(dslConn),
        qbClient:  libraryv1.NewQueryBuilderClient(qbConn),
        dmClient:  libraryv1.NewStorageServiceClient(dmConn),
    })

    logger.GetGlobal().WithField("addr", cfg.Orchestrator.Address()).
        InfoCtx(context.Background(), "[orchestrator] started")
    if err := s.Serve(lis); err != nil {
        logger.GetGlobal().FatalCtx(context.Background(), "failed to serve", err)
    }
}
