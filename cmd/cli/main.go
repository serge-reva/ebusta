package main

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"strings"
	"time"

	"github.com/chzyer/readline"
	"github.com/spf13/viper"
)

type Response struct {
	Status string `json:"status"`
	Meta   struct { CanonicalForm string `json:"canonical_form"` } `json:"meta"`
	Books  []struct {
		Id      string   `json:"id"`
		Title   string   `json:"title"`
		Authors []string `json:"authors"`
	} `json:"books"`
}

func init() {
	viper.SetConfigName("ebusta")
	viper.SetConfigType("yaml")
	viper.AddConfigPath(".")
	viper.SetDefault("gateway.url", "http://localhost:8080")
	viper.ReadInConfig()
}

func main() {
	if len(os.Args) > 1 {
		executeRequest(strings.Join(os.Args[1:], " "))
		return
	}

	// Настройка readline (история и стрелки)
	rl, err := readline.NewEx(&readline.Config{
		Prompt:          "\033[32mebusta>\033[0m ",
		HistoryFile:     ".ebusta_history",
		InterruptPrompt: "^C",
		EOFPrompt:       "exit",
	})
	if err != nil {
		panic(err)
	}
	defer rl.Close()

	fmt.Println("Ebusta Interactive Shell")
	fmt.Println("Type 'help' for commands, 'exit' to quit.")

	for {
		line, err := rl.Readline()
		if err != nil { // Handle Ctrl+C, Ctrl+D
			break
		}
		
		input := strings.TrimSpace(line)
		if input == "" { continue }

		switch input {
		case "help":
			printHelp()
		case "exit", "quit":
			return
		case "clear":
			fmt.Print("\033[H\033[2J")
		default:
			executeRequest(input)
		}
	}
}

func printHelp() {
	fmt.Println("\nAvailable commands:")
	fmt.Println("  help             - Show this message")
	fmt.Println("  clear            - Clear terminal screen")
	fmt.Println("  exit/quit        - Close the shell")
	fmt.Println("\nSearch Syntax:")
	fmt.Println("  author:Name      - Filter by author")
	fmt.Println("  title:Name       - Filter by title")
	fmt.Println("  Term1 AND Term2  - Logical AND")
	fmt.Println("  NOT Term         - Logical NOT")
	fmt.Println("  Arrows Up/Down   - Browse command history\n")
}

func executeRequest(query string) {
	apiURL := viper.GetString("gateway.url") + "/input?msg=" + url.QueryEscape(query)
	client := http.Client{Timeout: 5 * time.Second}
	
	resp, err := client.Get(apiURL)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)
	var res Response
	if err := json.Unmarshal(body, &res); err != nil {
		fmt.Printf("API Error: %s\n", string(body))
		return
	}

	fmt.Printf("\n[Plan]: %s\n", res.Meta.CanonicalForm)
	fmt.Printf("[Trace]: %s\n", resp.Header.Get("X-Trace-ID"))

	if len(res.Books) > 0 {
		fmt.Printf("%-5s | %-25s | %-20s\n", "ID", "Title", "Authors")
		fmt.Println(strings.Repeat("-", 60))
		for _, b := range res.Books {
			fmt.Printf("%-5s | %-25s | %-20s\n", b.Id, b.Title, strings.Join(b.Authors, ", "))
		}
	} else {
		fmt.Println("No results found.")
	}
	fmt.Println()
}
