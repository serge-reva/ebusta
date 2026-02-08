package main

import (
	"flag"
	"fmt"
	"net"
	"os"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/downloads/tier"

	"google.golang.org/grpc"
)

func main() {
	listen := flag.String("listen", "", "gRPC listen address, e.g. :50111")
	rootPath := flag.String("root", "", "root directory for this tier storage")
	sqlitePath := flag.String("sqlite", "", "sqlite path for this tier meta cache")
	parentAddr := flag.String("parent", "", "parent StorageNode address, e.g. localhost:50110")
	flag.Parse()

	if *listen == "" || *rootPath == "" || *sqlitePath == "" || *parentAddr == "" {
		fmt.Fprintf(os.Stderr, "usage: tier-node -listen :PORT -root <dir> -sqlite <file.sqlite> -parent host:port\n")
		os.Exit(2)
	}
	if !tier.IsValidAddr(*parentAddr) {
		fmt.Fprintf(os.Stderr, "invalid -parent addr: %s\n", *parentAddr)
		os.Exit(2)
	}

	node, err := tier.New(tier.Config{
		RootPath:   *rootPath,
		SqlitePath: *sqlitePath,
		ParentAddr: *parentAddr,
	})
	if err != nil {
		fmt.Fprintf(os.Stderr, "tier-node: init failed: %v\n", err)
		os.Exit(1)
	}
	defer node.Close()

	lis, err := net.Listen("tcp", *listen)
	if err != nil {
		fmt.Fprintf(os.Stderr, "tier-node: listen failed: %v\n", err)
		os.Exit(1)
	}

	s := grpc.NewServer()
	libraryv1.RegisterStorageNodeServer(s, node)

	if err := s.Serve(lis); err != nil {
		fmt.Fprintf(os.Stderr, "tier-node: serve failed: %v\n", err)
		os.Exit(1)
		os.Exit(1)
	}
}
