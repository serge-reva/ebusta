package main

import (
	"context"
	"net"
	"os"
	"os/signal"
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

	outCtx := errutil.ContextWithTraceID(ctx, traceID)
	dslResp, err := s.dslClient.Transform(outCtx, &libraryv1.DslRequest{
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

	qbResp, err := s.qbClient.Build(outCtx, &libraryv1.BuildRequest{
		Ast:  dslResp.GetAst(),
		Size: req.GetLimit(),
		From: req.GetOffset(),
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

	resp, err := s.dmClient.SearchBooks(outCtx, &libraryv1.SearchRequest{
		Query:               req.GetQuery(),
		Ast:                 dslResp.GetAst(),
		Limit:               req.GetLimit(),
		Offset:              req.GetOffset(),
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
	if err := cfg.Orchestrator.Validate(); err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "orchestrator config validation failed", err)
	}
	if err := cfg.DslScala.Validate(); err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "dsl_scala config validation failed", err)
	}
	if err := cfg.QueryBuilder.Validate(); err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "query_builder config validation failed", err)
	}
	if err := cfg.Datamanager.Validate(); err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "datamanager config validation failed", err)
	}
	if err := cfg.Metrics.Validate(); err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "metrics config validation failed", err)
	}
	metricsSrv := metrics.Start("orchestrator", cfg.Metrics.Services.Orchestrator)

	clientCreds, err := cfg.Orchestrator.MTLS.ClientTransportCredentials()
	if err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "failed to configure orchestrator mTLS client", err)
	}
	dslConn, err := grpc.Dial(cfg.DslScala.DialAddress(), grpc.WithTransportCredentials(clientCreds))
	if err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "failed to connect to dsl_scala", err)
	}
	qbConn, err := grpc.Dial(cfg.QueryBuilder.DialAddress(), grpc.WithTransportCredentials(clientCreds))
	if err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "failed to connect to query_builder", err)
	}
	dmConn, err := grpc.Dial(cfg.Datamanager.DialAddress(), grpc.WithTransportCredentials(clientCreds))
	if err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "failed to connect to datamanager", err)
	}
	defer dslConn.Close()
	defer qbConn.Close()
	defer dmConn.Close()

	lis, err := net.Listen("tcp", cfg.Orchestrator.ListenAddress())
	if err != nil {
		logger.GetGlobal().FatalCtx(context.Background(), "failed to listen", err)
	}

	grpcOpts := []grpc.ServerOption{}
	if cfg.Orchestrator.MTLS.Enabled {
		serverCreds, tlsErr := cfg.Orchestrator.MTLS.ServerTransportCredentials()
		if tlsErr != nil {
			logger.GetGlobal().FatalCtx(context.Background(), "failed to configure orchestrator mTLS server", tlsErr)
		}
		grpcOpts = append(grpcOpts, grpc.Creds(serverCreds))
	}
	s := grpc.NewServer(grpcOpts...)
	hs := health.NewServer()
	hs.SetServingStatus("", healthpb.HealthCheckResponse_SERVING)
	healthpb.RegisterHealthServer(s, hs)
	libraryv1.RegisterOrchestratorServiceServer(s, &orchestratorServer{
		dslClient: libraryv1.NewDslTransformerClient(dslConn),
		qbClient:  libraryv1.NewQueryBuilderClient(qbConn),
		dmClient:  libraryv1.NewStorageServiceClient(dmConn),
	})

	logger.GetGlobal().WithField("addr", cfg.Orchestrator.ListenAddress()).
		InfoCtx(context.Background(), "[orchestrator] started")

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
		logger.GetGlobal().WithField("signal", sig).InfoCtx(context.Background(), "[orchestrator] shutting down")
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
			logger.GetGlobal().WarnCtx(context.Background(), "[orchestrator] graceful stop timeout, forcing stop")
			s.Stop()
		}
	case err := <-serveErr:
		logger.GetGlobal().FatalCtx(context.Background(), "failed to serve", err)
	}

	wg.Wait()
	logger.GetGlobal().InfoCtx(context.Background(), "[orchestrator] stopped")
}
