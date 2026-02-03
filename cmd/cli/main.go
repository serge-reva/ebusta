package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"strings"
	"time"

	"ebusta/internal/search"
	"github.com/peterh/liner"
)

func main() {
	searchSvc, err := search.New()
	if err != nil {
		log.Fatalf("Failed to initialize search service: %v", err)
	}
	defer searchSvc.Close()

	// Режим 1: Запуск с аргументами (One-shot)
	if len(os.Args) > 1 {
		query := strings.Join(os.Args[1:], " ")
		performSearch(searchSvc, query)
		return
	}

	// Режим 2: Интерактивный режим (REPL)
	line := liner.NewLiner()
	defer line.Close()

	line.SetCtrlCAborts(true)
	fmt.Println("Ebusta Interactive CLI. Type 'exit' to quit.")

	for {
		if query, err := line.Prompt("ebusta> "); err == nil {
			query = strings.TrimSpace(query)
			if query == "" {
				continue
			}
			if query == "exit" || query == "quit" {
				break
			}
			
			line.AppendHistory(query)
			performSearch(searchSvc, query)
		} else if err == liner.ErrPromptAborted {
			break
		} else {
			log.Print("Error reading line: ", err)
			break
		}
	}
}

func performSearch(svc *search.Service, query string) {
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	res, err := svc.Search(ctx, query, 10)
	if err != nil {
		fmt.Printf("❌ Search error: %v\n", err)
		return
	}

	if len(res.Books) == 0 {
		fmt.Printf("No results found for: %s\n", query)
		return
	}

	fmt.Printf("\nFound %d books:\n", res.Total)
	headerFmt := "%-40s | %-30s | %-25s | %-10s | %s\n"
	fmt.Printf(headerFmt, "ID", "Title", "Authors", "Container", "Filename")
	fmt.Println(strings.Repeat("-", 140))

	for _, b := range res.Books {
		fmt.Printf(headerFmt, b.ID, b.Title, b.FullAuthors, b.Container, b.Filename)
	}
	fmt.Println()
}
