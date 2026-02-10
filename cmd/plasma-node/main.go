package main

import (
	"fmt"
	"log"
	"net"
	"net/http"
	"os"
	"time"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"
	"ebusta/internal/downloads/plasma"

	_ "expvar"

	"google.golang.org/grpc"
)

func main() {
	cfg := config.Get().Downloads.PlasmaNode
	if err := cfg.Validate(); err != nil {
		fmt.Fprintf(os.Stderr, "plasma-node config error: %v\n", err)
		os.Exit(2)
	}

	if cfg.DebugAddr != "" {
		go func() {
			srv := &http.Server{
				Addr:              cfg.DebugAddr,
				Handler:           http.DefaultServeMux,
				ReadHeaderTimeout: 3 * time.Second,
			}
			log.Printf("[plasma] debug http listening on %s (/debug/vars)", cfg.DebugAddr)
			if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
				log.Printf("[plasma] debug http error: %v", err)
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
	libraryv1.RegisterStorageNodeServer(s, node)

	log.Printf(
		"[plasma] grpc listening on %s parent=%s max_bytes=%d max_items=%d",
		cfg.ListenAddr(),
		cfg.ParentAddr,
		cfg.MaxBytes,
		cfg.MaxItems,
	)

	if err := s.Serve(lis); err != nil {
		fmt.Fprintf(os.Stderr, "serve failed: %v\n", err)
		os.Exit(1)
	}
}
