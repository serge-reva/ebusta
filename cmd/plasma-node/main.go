package main

import (
	"context"
	"fmt"
	"log"
	"net"
	"net/http"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"
	"ebusta/internal/downloads/plasma"
	"ebusta/internal/logger"
	"ebusta/internal/metrics"

	_ "expvar"

	"google.golang.org/grpc"
	"google.golang.org/grpc/health"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"
)

func main() {
	rootCfg := config.Get()
	if err := rootCfg.Metrics.Validate(); err != nil {
		log.Fatalf("plasma-node metrics config validation failed: %v", err)
	}
	cfg := rootCfg.Downloads.PlasmaNode
	if err := cfg.Validate(); err != nil {
		fmt.Fprintf(os.Stderr, "plasma-node config error: %v\n", err)
		os.Exit(2)
	}

	logger.InitFromConfig(rootCfg.Logger, "plasma")
	metricsSrv := metrics.Start("plasma-node", rootCfg.Metrics.Services.PlasmaNode)

	var debugSrv *http.Server
	if cfg.DebugAddr != "" {
		debugSrv = &http.Server{
			Addr:              cfg.DebugAddr,
			Handler:           http.DefaultServeMux,
			ReadHeaderTimeout: 3 * time.Second,
		}
		go func() {
			l := logger.GetGlobal().WithField("addr", cfg.DebugAddr)
			l.InfoCtx(context.Background(), "[plasma] debug http listening on /debug/vars")
			if err := debugSrv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
				l.ErrorCtx(context.Background(), "[plasma] debug http error", err)
			}
		}()
	}

	node, err := plasma.New(plasma.Config{
		ParentAddr: cfg.ParentAddr,
		MaxBytes:   cfg.MaxBytes,
		MaxItems:   cfg.MaxItems,
	})
	if err != nil {
		fmt.Fprintf(os.Stderr, "plasma-node init failed: %v\n", err)
		os.Exit(1)
	}
	defer node.Close()

	lis, err := net.Listen("tcp", cfg.ListenAddr())
	if err != nil {
		fmt.Fprintf(os.Stderr, "listen failed: %v\n", err)
		os.Exit(1)
	}

	s := grpc.NewServer()
	hs := health.NewServer()
	hs.SetServingStatus("", healthpb.HealthCheckResponse_SERVING)
	healthpb.RegisterHealthServer(s, hs)
	libraryv1.RegisterStorageNodeServer(s, node)

	l := logger.GetGlobal().WithField("addr", cfg.ListenAddr()).
		WithField("parent", cfg.ParentAddr).
		WithField("max_bytes", cfg.MaxBytes).
		WithField("max_items", cfg.MaxItems)
	l.InfoCtx(context.Background(), "[plasma] grpc listening")

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
		l.WithField("signal", sig).InfoCtx(context.Background(), "[plasma] shutting down")
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
			l.WarnCtx(context.Background(), "[plasma] graceful stop timeout, forcing stop")
			s.Stop()
		}

		if debugSrv != nil {
			dctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			_ = debugSrv.Shutdown(dctx)
			cancel()
		}
	case err := <-serveErr:
		fmt.Fprintf(os.Stderr, "serve failed: %v\n", err)
		os.Exit(1)
	}

	wg.Wait()
	l.InfoCtx(context.Background(), "[plasma] stopped")
}
