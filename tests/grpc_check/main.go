package main

import (
	"context"
	"flag"
	"fmt"
	"os"
	"time"

	libraryv1 "ebusta/api/proto/v1"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

func fail(format string, args ...any) {
	fmt.Fprintf(os.Stderr, "FAIL: "+format+"\n", args...)
	os.Exit(1)
}

func main() {
	target := flag.String("target", "", "datamanager|orchestrator")
	addr := flag.String("addr", "", "host:port (default depends on target)")
	query := flag.String("query", "Кинг", "search query")
	limit := flag.Int("limit", 1, "limit")
	timeout := flag.Duration("timeout", 5*time.Second, "timeout (e.g. 3s, 10s)")
	flag.Parse()

	if *target == "" {
		fail("--target is required (datamanager|orchestrator)")
	}

	if *addr == "" {
		switch *target {
		case "datamanager":
			*addr = "localhost:50051"
		case "orchestrator":
			*addr = "localhost:50053"
		default:
			fail("unknown target: %s", *target)
		}
	}

	ctx, cancel := context.WithTimeout(context.Background(), *timeout)
	defer cancel()

	conn, err := grpc.DialContext(ctx, *addr,
		grpc.WithTransportCredentials(insecure.NewCredentials()),
		grpc.WithBlock(),
	)
	if err != nil {
		fail("dial %s (%s): %v", *addr, *target, err)
	}
	defer conn.Close()

	req := &libraryv1.SearchRequest{
		Query: *query,
		Limit: int32(*limit),
	}

	switch *target {
	case "datamanager":
		c := libraryv1.NewStorageServiceClient(conn)
		resp, err := c.SearchBooks(ctx, req)
		if err != nil {
			fail("StorageService/SearchBooks: %v", err)
		}
		if resp.GetStatus() == "error" {
			fail("StorageService/SearchBooks returned status=error")
		}
		fmt.Printf("PASS: datamanager SearchBooks ok (total=%d)\n", resp.GetTotal())

	case "orchestrator":
		c := libraryv1.NewOrchestratorServiceClient(conn)
		resp, err := c.Search(ctx, req)
		if err != nil {
			fail("OrchestratorService/Search: %v", err)
		}
		if resp.GetStatus() == "error" {
			fail("OrchestratorService/Search returned status=error")
		}
		fmt.Printf("PASS: orchestrator Search ok (total=%d)\n", resp.GetTotal())

	default:
		fail("unknown target: %s", *target)
	}
}
