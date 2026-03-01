package main

import (
	"context"
	"flag"
	"fmt"
	"net"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"
	"ebusta/internal/downloads/tier"
	"ebusta/internal/metrics"

	"google.golang.org/grpc"
	"google.golang.org/grpc/health"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"
)

func main() {
	flagListen := flag.String("listen", "", "override listen address (:PORT)")
	flagRoot := flag.String("root", "", "override root directory")
	flagSqlite := flag.String("sqlite", "", "override sqlite path")
	flagParent := flag.String("parent", "", "override parent addr host:port")
	flag.Parse()

	cfg := config.Get()
	metricsSrv := metrics.Start("tier-node", cfg.Metrics.Services.TierNode)
	tcfg := cfg.Downloads.TierNode

	if *flagListen != "" {
		fmt.Sscanf(*flagListen, ":%d", &tcfg.ListenPort)
	}
	if *flagRoot != "" {
		tcfg.RootPath = *flagRoot
	}
	if *flagSqlite != "" {
		tcfg.Sqlite = *flagSqlite
	}
	if *flagParent != "" {
		tcfg.ParentAddr = *flagParent
	}

	if err := tcfg.Validate(); err != nil {
		fmt.Fprintf(os.Stderr, "tier-node config error: %v\n", err)
		os.Exit(2)
	}
	if !tier.IsValidAddr(tcfg.ParentAddr) {
		fmt.Fprintf(os.Stderr, "invalid parent addr: %s\n", tcfg.ParentAddr)
		os.Exit(2)
	}

	node, err := tier.New(tier.Config{
		RootPath:   tcfg.RootPath,
		SqlitePath: tcfg.Sqlite,
		ParentAddr: tcfg.ParentAddr,
	})
	if err != nil {
		fmt.Fprintf(os.Stderr, "tier-node init failed: %v\n", err)
		os.Exit(1)
	}
	defer node.Close()

	lis, err := net.Listen("tcp", tcfg.ListenAddr())
	if err != nil {
		fmt.Fprintf(os.Stderr, "tier-node listen failed: %v\n", err)
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
		fmt.Fprintf(os.Stderr, "tier-node received %s, shutting down\n", sig)
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
			fmt.Fprintln(os.Stderr, "tier-node graceful stop timeout, forcing stop")
			s.Stop()
		}
	case err := <-serveErr:
		fmt.Fprintf(os.Stderr, "tier-node serve failed: %v\n", err)
		os.Exit(1)
	}

	wg.Wait()
}
