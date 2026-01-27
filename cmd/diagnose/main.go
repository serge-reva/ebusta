package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"

	libraryv1 "ebusta/api/proto/v1"
	dsl "ebusta/api/proto/v1"
)

func main() {
	fmt.Println("🔍 === STARTING COMPONENT DIAGNOSTICS ===")
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	// 1. TEST DSL (LISP)
	fmt.Println("\n[1] Testing Lisp DSL Service (:50052)...")
	checkDSL(ctx)

	// 2. TEST ORCHESTRATOR
	fmt.Println("\n[2] Testing Orchestrator (:50053)...")
	checkOrchestrator(ctx)

	// 3. TEST WEB ADAPTER
	fmt.Println("\n[3] Testing Web Adapter (:50080)...")
	checkWeb()

	fmt.Println("\n🏁 === DIAGNOSTICS COMPLETE ===")
}

func checkDSL(ctx context.Context) {
	conn, err := grpc.Dial("localhost:50052", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Printf("❌ Failed to connect to DSL: %v", err)
		return
	}
	defer conn.Close()

	client := dsl.NewMessageConverterClient(conn)
	req := &dsl.ConvertRequest{RawQuery: "author:\"King\""}
	resp, err := client.Convert(ctx, req)
	if err != nil {
		log.Printf("❌ DSL Convert failed: %v", err)
		return
	}
	fmt.Printf("✅ PASS. Canonical: '%s', RequestID: '%s'\n", resp.CanonicalForm, resp.RequestId)
}

func checkOrchestrator(ctx context.Context) {
	conn, err := grpc.Dial("localhost:50053", grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Printf("❌ Failed to connect to Orchestrator: %v", err)
		return
	}
	defer conn.Close()

	client := libraryv1.NewOrchestratorServiceClient(conn)
	resp, err := client.Search(ctx, &libraryv1.SearchRequest{Query: "author:\"King\""})
	if err != nil {
		log.Printf("❌ Orchestrator Search failed: %v", err)
		return
	}
	fmt.Printf("✅ PASS. Status: '%s', Books Found: %d\n", resp.Status, len(resp.Books))
	if len(resp.Books) == 0 {
		fmt.Println("   (Note: 0 books is expected if DataManager is not connected yet)")
	}
}

func checkWeb() {
	url := "http://localhost:50080/input?msg=author:King"
	resp, err := http.Get(url)
	if err != nil {
		log.Printf("❌ Web Adapter failed: %v", err)
		return
	}
	defer resp.Body.Close()
	
	if resp.StatusCode == 200 {
		fmt.Printf("✅ PASS. HTTP Status: %d\n", resp.StatusCode)
	} else {
		fmt.Printf("⚠️ WARNING. HTTP Status: %d\n", resp.StatusCode)
	}
}
