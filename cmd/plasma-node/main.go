package main

import (
    "context"
    "fmt"
    "net"
    "net/http"
    "os"
    "time"

    libraryv1 "ebusta/api/proto/v1"
    "ebusta/internal/config"
    "ebusta/internal/downloads/plasma"
    "ebusta/internal/logger"

    _ "expvar"

    "google.golang.org/grpc"
)

func main() {
    cfg := config.Get().Downloads.PlasmaNode
    if err := cfg.Validate(); err != nil {
        fmt.Fprintf(os.Stderr, "plasma-node config error: %v\n", err)
        os.Exit(2)
    }

    logger.InitFromConfig(config.Get().Logger, "plasma")

    if cfg.DebugAddr != "" {
        go func() {
            srv := &http.Server{
                Addr:              cfg.DebugAddr,
                Handler:           http.DefaultServeMux,
                ReadHeaderTimeout: 3 * time.Second,
            }
            l := logger.GetGlobal().WithField("addr", cfg.DebugAddr)
            l.InfoCtx(context.Background(), "[plasma] debug http listening on /debug/vars")
            if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
                l.ErrorCtx(context.Background(), "[plasma] debug http error", err)
            }
        }()
    }

    node, err := plasma.New(plasma.Config{
        ParentAddr: cfg.ParentAddr,
        MaxBytes:   cfg.MaxBytes,
        MaxItems:   cfg.MaxItems,
    })
    if err != nil {
        fmt.Fprintf(os.Stderr, "plasma-node init failed: %v\n", err)
        os.Exit(1)
    }
    defer node.Close()

    lis, err := net.Listen("tcp", cfg.ListenAddr())
    if err != nil {
        fmt.Fprintf(os.Stderr, "listen failed: %v\n", err)
        os.Exit(1)
    }

    s := grpc.NewServer()
    libraryv1.RegisterStorageNodeServer(s, node)

    l := logger.GetGlobal().WithField("addr", cfg.ListenAddr()).
        WithField("parent", cfg.ParentAddr).
        WithField("max_bytes", cfg.MaxBytes).
        WithField("max_items", cfg.MaxItems)
    l.InfoCtx(context.Background(), "[plasma] grpc listening")

    if err := s.Serve(lis); err != nil {
        fmt.Fprintf(os.Stderr, "serve failed: %v\n", err)
        os.Exit(1)
    }
}
