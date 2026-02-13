package main

import (
        "log"
        "net/http"

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

        http.HandleFunc("/", h.handleIndex)
        http.HandleFunc("/search", h.handleSearch)
        http.HandleFunc("/download/", h.handleDownload)

        addr := wfCfg.ListenAddr()
        log.Printf("[web-frontend] listening on %s", addr)
        log.Fatal(http.ListenAndServe(addr, nil))
}
