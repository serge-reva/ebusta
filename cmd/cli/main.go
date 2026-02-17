package main

import (
    "context"
    "fmt"
    "io"
    "os"
    "path/filepath"
    "strconv"
    "strings"
    "time"

    libraryv1 "ebusta/api/proto/v1"
    "ebusta/internal/config"
    "ebusta/internal/presenter"
    "ebusta/internal/search"

    "github.com/peterh/liner"
    "google.golang.org/grpc"
    "google.golang.org/grpc/credentials/insecure"
)

type dlOpts struct {
    Progress bool
    Verify   bool
}

func main() {
    // --- download modes (non-interactive) ---
    if len(os.Args) >= 2 && os.Args[1] == "get" {
        sha1, opts, err := parseDownloadArgs(os.Args[2:])
        if err != nil {
            fmt.Fprintln(os.Stderr, err.Error())
            os.Exit(2)
        }
        if err := doDownloadToFile(sha1, opts); err != nil {
            fmt.Fprintln(os.Stderr, err.Error())
            os.Exit(1)
        }
        return
    }
    if len(os.Args) >= 2 && os.Args[1] == "get-stdout" {
        sha1, opts, err := parseDownloadArgs(os.Args[2:])
        if err != nil {
            fmt.Fprintln(os.Stderr, err.Error())
            os.Exit(2)
        }
        if err := doDownloadToStdout(sha1, opts); err != nil {
            fmt.Fprintln(os.Stderr, err.Error())
            os.Exit(1)
        }
        return
    }

    svc := search.NewService()
    if svc == nil {
        fmt.Fprintln(os.Stderr, "Error: could not connect to search service")
        os.Exit(1)
    }
    defer svc.Close()

    line := liner.NewLiner()
    defer line.Close()

    // non-interactive search with pagination support
    if len(os.Args) > 1 {
        query, page := parseSearchArgs(os.Args[1:])
        if query == "" {
            fmt.Fprintln(os.Stderr, "Error: empty search query")
            os.Exit(1)
        }
        doSearch(svc, query, page)
        return
    }

    // interactive REPL
    for {
        if input, err := line.Prompt("ebusta> "); err == nil {
            input = strings.TrimSpace(input)
            if input == "" {
                continue
            }
            if input == "exit" || input == "quit" {
                break
            }

            if strings.HasPrefix(input, "get ") {
                sha1 := strings.TrimSpace(strings.TrimPrefix(input, "get "))
                _ = doDownloadToFile(sha1, dlOpts{})
                line.AppendHistory(input)
                continue
            }

            if strings.HasPrefix(input, "get-stdout ") {
                sha1 := strings.TrimSpace(strings.TrimPrefix(input, "get-stdout "))
                _ = doDownloadToStdout(sha1, dlOpts{})
                line.AppendHistory(input)
                continue
            }

            doSearch(svc, input, 1)
            line.AppendHistory(input)
        } else {
            break
        }
    }
}

// parseSearchArgs извлекает запрос и номер страницы из аргументов командной строки
func parseSearchArgs(args []string) (query string, page int) {
    page = 1
    var queryParts []string
    
    for i := 0; i < len(args); i++ {
        arg := args[i]
        
        // Check for --page=2 format
        if strings.HasPrefix(arg, "--page=") {
            pageStr := strings.TrimPrefix(arg, "--page=")
            if p, err := strconv.Atoi(pageStr); err == nil && p > 0 {
                page = p
            }
            continue
        }
        
        // Check for --page 2 format
        if arg == "--page" && i+1 < len(args) {
            if p, err := strconv.Atoi(args[i+1]); err == nil && p > 0 {
                page = p
                i++ // skip next argument
            }
            continue
        }
        
        // Skip any other flag that starts with --
        if strings.HasPrefix(arg, "--") {
            continue
        }
        
        // Not a flag, add to query
        queryParts = append(queryParts, arg)
    }
    
    query = strings.Join(queryParts, " ")
    return
}

func doSearch(svc *search.Service, query string, page int) {
    tid := fmt.Sprintf("cli-%d", time.Now().UnixNano())
    pageSize := 10
    offset := (page - 1) * pageSize

    fmt.Fprintf(os.Stderr, "Searching for: %q (page %d, offset %d)\n", query, page, offset)

    result, err := svc.SearchWithPagination(context.Background(), query, pageSize, offset, page, tid)
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error: %v (Trace: %s)\n", err, tid)
        return
    }

    formatter := &presenter.TextFormatter{}
    if err := formatter.Format(result, os.Stdout); err != nil {
        fmt.Fprintf(os.Stderr, "Error formatting results: %v\n", err)
    }
}

func parseDownloadArgs(args []string) (string, dlOpts, error) {
    var opts dlOpts
    var sha1 string

    for _, a := range args {
        switch a {
        case "--progress":
            opts.Progress = true
        case "--verify":
            opts.Verify = true
        default:
            if strings.HasPrefix(a, "-") {
                return "", dlOpts{}, fmt.Errorf("unknown flag: %s", a)
            }
            if sha1 != "" {
                return "", dlOpts{}, fmt.Errorf("too many args: expected single <sha1>")
            }
            sha1 = a
        }
    }

    if sha1 == "" {
        return "", dlOpts{}, fmt.Errorf("usage: get [--progress] [--verify] <sha1>\n       get-stdout [--progress] [--verify] <sha1>")
    }

    return sha1, opts, nil
}

func dialPlasma(cfg *config.Config) (*grpc.ClientConn, libraryv1.StorageNodeClient, error) {
    plasma := cfg.Downloads.PlasmaNode
    addr := fmt.Sprintf("localhost:%d", plasma.ListenPort)

    conn, err := grpc.Dial(addr, grpc.WithTransportCredentials(insecure.NewCredentials()))
    if err != nil {
        return nil, nil, fmt.Errorf("plasma connect error: %w", err)
    }
    client := libraryv1.NewStorageNodeClient(conn)
    return conn, client, nil
}

func streamToWriter(client libraryv1.StorageNodeClient, sha1 string, w io.Writer, opts dlOpts, expectedTotal int64) (int64, error) {
    ctx := context.Background()

    stream, err := client.GetStream(ctx, &libraryv1.GetStreamRequest{
        Id: &libraryv1.BookId{Sha1: sha1},
    })
    if err != nil {
        return 0, fmt.Errorf("GetStream error: %w", err)
    }

    var written int64
    var lastReport time.Time
    reportEvery := int64(1 << 20)
    nextBytes := reportEvery

    for {
        chunk, err := stream.Recv()
        if err != nil {
            break
        }

        n, err := w.Write(chunk.Data)
        if err != nil {
            return written, fmt.Errorf("write error: %w", err)
        }
        written += int64(n)

        if opts.Progress && written >= nextBytes {
            if !lastReport.IsZero() && time.Since(lastReport) < 250*time.Millisecond {
                // avoid flooding stderr
            } else {
                if expectedTotal > 0 {
                    pct := float64(written) * 100.0 / float64(expectedTotal)
                    fmt.Fprintf(os.Stderr, "[progress] %d/%d bytes (%.1f%%)\n", written, expectedTotal, pct)
                } else {
                    fmt.Fprintf(os.Stderr, "[progress] %d bytes\n", written)
                }
                lastReport = time.Now()
            }
            for written >= nextBytes {
                nextBytes += reportEvery
            }
        }
    }

    return written, nil
}

func doDownloadToFile(sha1 string, opts dlOpts) error {
    if sha1 == "" {
        return fmt.Errorf("usage: get [--progress] [--verify] <sha1>")
    }

    cfg := config.Get()

    downloadDir := strings.TrimSpace(cfg.Downloads.CLI.DownloadDir)
    if downloadDir == "" {
        downloadDir = "."
    }
    if err := os.MkdirAll(downloadDir, 0o755); err != nil {
        return fmt.Errorf("cannot create download_dir %q: %v", downloadDir, err)
    }

    conn, client, err := dialPlasma(cfg)
    if err != nil {
        return err
    }
    defer conn.Close()

    ctx := context.Background()
    metaResp, err := client.GetMeta(ctx, &libraryv1.GetMetaRequest{
        Id: &libraryv1.BookId{Sha1: sha1},
    })
    if err != nil {
        return fmt.Errorf("GetMeta error: %v", err)
    }

    outName := metaResp.Meta.Filename
    outPath := filepath.Join(downloadDir, outName)

    expected := metaResp.Meta.GetSize()

    if opts.Progress {
        if expected > 0 {
            fmt.Fprintf(os.Stderr, "[start] downloading sha1=%s -> %s (expected %d bytes)\n", sha1, outPath, expected)
        } else {
            fmt.Fprintf(os.Stderr, "[start] downloading sha1=%s -> %s\n", sha1, outPath)
        }
    }

    f, err := os.Create(outPath)
    if err != nil {
        return fmt.Errorf("cannot create file: %v", err)
    }
    defer f.Close()

    written, err := streamToWriter(client, sha1, f, opts, expected)
    if err != nil {
        return err
    }

    if opts.Verify {
        if expected <= 0 {
            return fmt.Errorf("verify failed: meta.size is not set (>0)")
        }
        if written != expected {
            return fmt.Errorf("verify failed: written %d bytes, expected %d bytes", written, expected)
        }
        fmt.Fprintf(os.Stderr, "[verify] OK: %d bytes\n", written)
    }

    fmt.Printf("Downloaded %s\n", outPath)
    return nil
}

func doDownloadToStdout(sha1 string, opts dlOpts) error {
    if sha1 == "" {
        return fmt.Errorf("usage: get-stdout [--progress] [--verify] <sha1>")
    }

    cfg := config.Get()

    conn, client, err := dialPlasma(cfg)
    if err != nil {
        return err
    }
    defer conn.Close()

    expected := int64(0)
    if opts.Progress || opts.Verify {
        ctx := context.Background()
        metaResp, err := client.GetMeta(ctx, &libraryv1.GetMetaRequest{
            Id: &libraryv1.BookId{Sha1: sha1},
        })
        if err != nil {
            return fmt.Errorf("GetMeta error: %v", err)
        }
        expected = metaResp.Meta.GetSize()
    }

    written, err := streamToWriter(client, sha1, os.Stdout, opts, expected)
    if err != nil {
        return err
    }

    if opts.Verify {
        if expected <= 0 {
            return fmt.Errorf("verify failed: meta.size is not set (>0)")
        }
        if written != expected {
            return fmt.Errorf("verify failed: written %d bytes, expected %d bytes", written, expected)
        }
        fmt.Fprintf(os.Stderr, "[verify] OK: %d bytes\n", written)
    }

    return nil
}
