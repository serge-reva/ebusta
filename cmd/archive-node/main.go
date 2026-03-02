package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"net"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"
	"ebusta/internal/downloads/archive"
	"ebusta/internal/metrics"

	"google.golang.org/grpc"
	"google.golang.org/grpc/health"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"
)

func main() {
	var (
		flagListen = flag.String("listen", "", "override listen address, e.g. :50110")
		flagZip    = flag.String("zip-root", "", "override zip root directory")
		flagSqlite = flag.String("sqlite", "", "override sqlite path")
	)
	flag.Parse()

	cfg := config.Get()
	if err := cfg.Metrics.Validate(); err != nil {
		log.Fatalf("archive-node metrics config validation failed: %v", err)
	}
	metricsSrv := metrics.Start("archive-node", cfg.Metrics.Services.ArchiveNode)
	arch := cfg.Downloads.ArchiveNode

	// overrides
	if *flagListen != "" {
		fmt.Sscanf(*flagListen, ":%d", &arch.ListenPort)
	}
	if *flagZip != "" {
		arch.ZipRoot = *flagZip
	}
	if *flagSqlite != "" {
		arch.Sqlite = *flagSqlite
	}

	if err := arch.Validate(); err != nil {
		fmt.Fprintf(os.Stderr, "archive-node config error: %v\n", err)
		os.Exit(2)
	}

	node, err := archive.New(arch.ZipRoot, arch.Sqlite)
	if err != nil {
		fmt.Fprintf(os.Stderr, "archive-node init: %v\n", err)
		os.Exit(1)
	}
	defer node.Close()

	lis, err := net.Listen("tcp", arch.ListenAddr())
	if err != nil {
		fmt.Fprintf(os.Stderr, "archive-node listen: %v\n", err)
		os.Exit(1)
	}

	s := grpc.NewServer()
	hs := health.NewServer()
	hs.SetServingStatus("", healthpb.HealthCheckResponse_SERVING)
	healthpb.RegisterHealthServer(s, hs)
	libraryv1.RegisterStorageNodeServer(s, node)

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
		fmt.Fprintf(os.Stderr, "archive-node received %s, shutting down\n", sig)
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
			fmt.Fprintln(os.Stderr, "archive-node graceful stop timeout, forcing stop")
			s.Stop()
		}
	case err := <-serveErr:
		fmt.Fprintf(os.Stderr, "archive-node serve: %v\n", err)
		os.Exit(1)
	}

	wg.Wait()
}
