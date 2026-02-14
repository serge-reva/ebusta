package main

import (
    "context"
    "log"
    "net/http"
    "os"
    "os/signal"
    "syscall"
    "time"

    "ebusta/internal/config"
    "ebusta/internal/search"
)

func main() {
    cfg := config.Get()
    wfCfg := cfg.WebFrontend

    svc, err := search.New()
    if err != nil {
        log.Fatalf("[web-frontend] failed to connect to orchestrator: %v", err)
    }
    defer svc.Close()

    h := &Handler{
        searchSvc:      svc,
        downloaderAddr: wfCfg.DownloaderAddr,
        pageSize:       wfCfg.PageSize,
    }

    // Роутинг
    mux := http.NewServeMux()
    mux.HandleFunc("/", h.handleIndex)
    mux.HandleFunc("/search", h.handleSearch)
    mux.HandleFunc("/download/", h.handleDownload)
    mux.HandleFunc("/healthz", h.handleHealth)

    // HTTP сервер
    addr := wfCfg.ListenAddr()
    server := &http.Server{
        Addr:    addr,
        Handler: mux,
    }

    // Канал для graceful shutdown
    stop := make(chan os.Signal, 1)
    signal.Notify(stop, syscall.SIGINT, syscall.SIGTERM)

    // Запуск сервера в горутине
    go func() {
        log.Printf("[web-frontend] listening on %s", addr)
        if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
            log.Fatalf("[web-frontend] server error: %v", err)
        }
    }()

    // Ожидание сигнала
    sig := <-stop
    log.Printf("[web-frontend] received signal %v, shutting down...", sig)

    // Graceful shutdown с таймаутом
    ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
    defer cancel()

    if err := server.Shutdown(ctx); err != nil {
        log.Printf("[web-frontend] shutdown error: %v", err)
    }

    log.Println("[web-frontend] stopped")
}
