package main

import (
	"context"
	"log"
	"net"
	"net/http"

	"ebusta/api/proto/v1"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	"google.golang.org/grpc/metadata"
)

var (
	requestsTotal = prometheus.NewCounterVec(
		prometheus.CounterOpts{Name: "orchestrator_requests_total"},
		[]string{"status"},
	)
)

func init() { prometheus.MustRegister(requestsTotal) }

type orchServer struct {
	libraryv1.UnimplementedOrchestratorServer
	converter libraryv1.MessageConverterServiceClient
	processor libraryv1.ProcessorServiceClient
}

func (s *orchServer) Execute(ctx context.Context, req *libraryv1.ExecuteRequest) (*libraryv1.Response, error) {
	traceID := req.TraceId
	if traceID == "" {
		traceID = "internal-" + req.RawInput[:3]
	}
	
	log.Printf("[%s] Incoming request: %s", traceID, req.RawInput)
	ctx = metadata.AppendToOutgoingContext(ctx, "x-trace-id", traceID)

	// 1. Конвертация
	unmarshaled, err := s.converter.Convert(ctx, &libraryv1.RawInput{Data: req.RawInput})
	if err != nil {
		log.Printf("[%s] Convert error: %v", traceID, err)
		requestsTotal.WithLabelValues("error_conv").Inc()
		return nil, err
	}

	// Обогащаем метаданные для процессора
	unmarshaled.Meta.Source = "Orchestrator"

	// 2. Вызов процессора
	resp, err := s.processor.HandleCommand(ctx, unmarshaled)
	if err != nil {
		log.Printf("[%s] Processor error: %v", traceID, err)
		requestsTotal.WithLabelValues("error_proc").Inc()
		return nil, err
	}

	if resp.Meta == nil {
		resp.Meta = &libraryv1.ResponseMeta{}
	}
	resp.Meta.TraceId = traceID
	resp.Meta.CanonicalForm = unmarshaled.Meta.CanonicalForm

	requestsTotal.WithLabelValues("success").Inc()
	return resp, nil
}

func main() {
	go func() {
		http.Handle("/metrics", promhttp.Handler())
		http.ListenAndServe(":9090", nil)
	}()

	lis, _ := net.Listen("tcp", ":50054")
	srv := grpc.NewServer()

	connConv, _ := grpc.Dial("localhost:50052", grpc.WithTransportCredentials(insecure.NewCredentials()))
	connProc, _ := grpc.Dial("localhost:50053", grpc.WithTransportCredentials(insecure.NewCredentials()))

	libraryv1.RegisterOrchestratorServer(srv, &orchServer{
		converter: libraryv1.NewMessageConverterServiceClient(connConv),
		processor: libraryv1.NewProcessorServiceClient(connProc),
	})

	log.Println("Orchestrator started on :50054")
	srv.Serve(lis)
}
