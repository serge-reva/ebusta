package main

import (
	"flag"
	"fmt"
	"log"
	"net"
	"net/http"
	"os"
	"time"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/downloads/plasma"

	_ "expvar" // registers /debug/vars on DefaultServeMux
	"google.golang.org/grpc"
)

func main() {
	listen := flag.String("listen", "", "gRPC listen addr, e.g. :50112")
	parent := flag.String("parent", "", "parent StorageNode addr, e.g. localhost:50111")
	maxBytes := flag.Int64("max-bytes", 0, "max bytes in plasma")
	maxItems := flag.Int("max-items", 0, "max items in plasma")
	debugAddr := flag.String("debug", "", "optional debug HTTP addr for /debug/vars, e.g. :8091 (empty disables)")
	flag.Parse()

	if *listen == "" || *parent == "" || *maxBytes <= 0 || *maxItems <= 0 {
		fmt.Fprintf(os.Stderr,
			"usage: plasma-node -listen :PORT -parent host:port -max-bytes N -max-items N [-debug :8091]\n")
		os.Exit(2)
	}

	if *debugAddr != "" {
		go func() {
			srv := &http.Server{
				Addr:              *debugAddr,
				Handler:           http.DefaultServeMux,
				ReadHeaderTimeout: 3 * time.Second,
			}
			log.Printf("[plasma] debug http listening on %s (/debug/vars)", *debugAddr)
			if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
				log.Printf("[plasma] debug http error: %v", err)
			}
		}()
	}

	node, err := plasma.New(plasma.Config{
		ParentAddr: *parent,
		MaxBytes:   *maxBytes,
		MaxItems:   *maxItems,
	})
	if err != nil {
		fmt.Fprintf(os.Stderr, "plasma-node init failed: %v\n", err)
		os.Exit(1)
	}
	defer node.Close()

	lis, err := net.Listen("tcp", *listen)
	if err != nil {
		fmt.Fprintf(os.Stderr, "listen failed: %v\n", err)
		os.Exit(1)
	}

	s := grpc.NewServer()
	libraryv1.RegisterStorageNodeServer(s, node)

	log.Printf("[plasma] grpc listening on %s parent=%s max_bytes=%d max_items=%d", *listen, *parent, *maxBytes, *maxItems)

	if err := s.Serve(lis); err != nil {
		fmt.Fprintf(os.Stderr, "serve failed: %v\n", err)
		os.Exit(1)
	}
}
