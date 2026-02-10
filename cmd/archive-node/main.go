package main

import (
	"flag"
	"fmt"
	"net"
	"os"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"
	"ebusta/internal/downloads/archive"

	"google.golang.org/grpc"
)

func main() {
	var (
		flagListen = flag.String("listen", "", "override listen address, e.g. :50110")
		flagZip    = flag.String("zip-root", "", "override zip root directory")
		flagSqlite = flag.String("sqlite", "", "override sqlite path")
	)
	flag.Parse()

	cfg := config.Get()
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
	libraryv1.RegisterStorageNodeServer(s, node)

	if err := s.Serve(lis); err != nil {
		fmt.Fprintf(os.Stderr, "archive-node serve: %v\n", err)
		os.Exit(1)
	}
}
