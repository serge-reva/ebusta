package main

import (
	"flag"
	"fmt"
	"net"
	"os"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/downloads/archive"

	"google.golang.org/grpc"
)

func main() {
	var (
		addr       = flag.String("listen", "", "gRPC listen address, e.g. :50110")
		zipRoot    = flag.String("zip-root", "", "path to directory with zip containers")
		sqlitePath = flag.String("sqlite", "", "path to sqlite index file")
	)
	flag.Parse()

	if *addr == "" || *zipRoot == "" || *sqlitePath == "" {
		fmt.Fprintf(os.Stderr, "usage: archive-node -listen :PORT -zip-root <dir> -sqlite <meta.sqlite>\n")
		os.Exit(2)
	}

	node, err := archive.New(*zipRoot, *sqlitePath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "archive-node: init failed: %v\n", err)
		os.Exit(1)
	}
	defer node.Close()

	lis, err := net.Listen("tcp", *addr)
	if err != nil {
		fmt.Fprintf(os.Stderr, "archive-node: listen failed: %v\n", err)
		os.Exit(1)
	}

	s := grpc.NewServer()
	libraryv1.RegisterStorageNodeServer(s, node)

	if err := s.Serve(lis); err != nil {
		fmt.Fprintf(os.Stderr, "archive-node: serve failed: %v\n", err)
		os.Exit(1)
	}
}
