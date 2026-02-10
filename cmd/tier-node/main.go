package main

import (
	"flag"
	"fmt"
	"net"
	"os"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"
	"ebusta/internal/downloads/tier"

	"google.golang.org/grpc"
)

func main() {
	flagListen := flag.String("listen", "", "override listen address (:PORT)")
	flagRoot := flag.String("root", "", "override root directory")
	flagSqlite := flag.String("sqlite", "", "override sqlite path")
	flagParent := flag.String("parent", "", "override parent addr host:port")
	flag.Parse()

	cfg := config.Get()
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
	libraryv1.RegisterStorageNodeServer(s, node)

	if err := s.Serve(lis); err != nil {
		fmt.Fprintf(os.Stderr, "tier-node serve failed: %v\n", err)
		os.Exit(1)
	}
}
