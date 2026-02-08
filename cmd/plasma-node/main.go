package main

import (
	"flag"
	"fmt"
	"net"
	"os"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/downloads/plasma"

	"google.golang.org/grpc"
)

func main() {
	listen := flag.String("listen", "", "gRPC listen addr, e.g. :50112")
	parent := flag.String("parent", "", "parent StorageNode addr")
	maxBytes := flag.Int64("max-bytes", 0, "max bytes in plasma")
	maxItems := flag.Int("max-items", 0, "max items in plasma")
	flag.Parse()

	if *listen == "" || *parent == "" || *maxBytes <= 0 || *maxItems <= 0 {
		fmt.Fprintf(os.Stderr,
			"usage: plasma-node -listen :PORT -parent host:port -max-bytes N -max-items N\n")
		os.Exit(2)
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

	if err := s.Serve(lis); err != nil {
		fmt.Fprintf(os.Stderr, "serve failed: %v\n", err)
		os.Exit(1)
	}
}
