package main

import (
	"context"
	"fmt"
	"log"
	"time"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
	pb "ebusta/api/gen/dsl" 
)

func findFilter(resp *pb.SearchQuery, field string) *pb.FilterNode {
	if f := resp.GetFilter(); f != nil && f.Field == field {
		return f
	}
	if l := resp.GetLogical(); l != nil {
		for _, node := range l.Nodes {
			if f := findFilter(node, field); f != nil {
				return f
			}
		}
	}
	return nil
}

func main() {
	conn, err := grpc.Dial("localhost:50052", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil { log.Fatalf("Conn failed: %v", err) }
	defer conn.Close()
	client := pb.NewMessageConverterClient(conn)

	tests := []struct {
		name, query, expF, expV string
	}{
		{"UR 1.1 (Numeric ID)", "101", "id", "101"},
		{"UR 1.1 (Default any)", "linux", "any", "linux"},
		{"UR 2.1 (Quoted Author)", "author:\"Стивен Кинг\"", "author", "Стивен Кинг"},
		{"UR 2.1 (Greedy Author)", "author:Стивен Кинг AND year:2026", "author", "Стивен Кинг"},
	}

	fmt.Println("=== Running Native Go Compliance Tests ===")
	for _, tc := range tests {
		fmt.Printf("[TEST] %-25s... ", tc.name)
		ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
		resp, err := client.Convert(ctx, &pb.ConvertRequest{RawQuery: tc.query})
		cancel()

		if err != nil { fmt.Printf("❌ RPC ERROR: %v\n", err); continue }

		f := findFilter(resp, tc.expF)
		if f != nil && f.Value == tc.expV {
			fmt.Println("✅ PASSED")
		} else if f == nil {
			fmt.Printf("❌ FAILED (Field %s not found)\n", tc.expF)
		} else {
			fmt.Printf("❌ FAILED (Got value '%s', Expected '%s')\n", f.Value, tc.expV)
		}
	}
}
