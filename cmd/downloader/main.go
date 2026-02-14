package main

import (
    "encoding/json"
    "fmt"
    "io"
    "log"
    "net/http"
    "strings"
    "time"

    libraryv1 "ebusta/api/proto/v1"
    "ebusta/internal/config"
    "ebusta/internal/errutil"

    "google.golang.org/grpc"
    "google.golang.org/grpc/credentials/insecure"
)

func main() {
    cfg := config.Get()

    dcfg := cfg.Downloads.Downloader
    if err := dcfg.Validate(); err != nil {
        log.Printf("[downloader] config error: %v", err)
        panic(err)
    }

    conn, err := grpc.Dial(dcfg.PlasmaAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
    if err != nil {
        log.Printf("[downloader] plasma connect error: %v", err)
        panic(err)
    }
    defer conn.Close()

    client := libraryv1.NewStorageNodeClient(conn)

    mux := http.NewServeMux()

    mux.HandleFunc("/books/", func(w http.ResponseWriter, r *http.Request) {

        start := time.Now()

        // Получаем или генерируем TraceID
        tid := errutil.TraceIDFromRequest(r)
        if tid == "" {
            tid = errutil.GenerateTraceID("dl")
        }
        w.Header().Set("X-Trace-Id", tid)

        sha1 := strings.TrimPrefix(r.URL.Path, "/books/")
        sha1 = strings.Trim(sha1, "/")
        if sha1 == "" {
            errutil.WriteJSONError(w, errutil.New(
                errutil.CodeInvalidArgument,
                "missing id",
            ).WithTrace(tid))
            return
        }

        if !isValidSHA1(sha1) {
            errutil.WriteJSONError(w, errutil.New(
                errutil.CodeInvalidArgument,
                "invalid sha1 (expected 40 hex)",
            ).WithTrace(tid))
            return
        }

        ctx := errutil.ContextWithTraceID(r.Context(), tid)

        metaResp, err := client.GetMeta(ctx, &libraryv1.GetMetaRequest{
            Id: &libraryv1.BookId{Sha1: sha1},
        })
        if err != nil {
            appErr := errutil.FromGRPCError(err, tid)
            errutil.WriteJSONError(w, appErr)
            return
        }

        if metaResp.GetError() != nil {
            appErr := errutil.New(
                metaResp.GetError().GetCode(),
                metaResp.GetError().GetMessage(),
            ).WithTrace(tid)
            appErr.HTTPCode = errutil.CodeToHTTP(appErr.Code)
            if appErr.HTTPCode == 0 {
                appErr.HTTPCode = 502
            }
            errutil.WriteJSONError(w, appErr)
            return
        }

        meta := metaResp.GetMeta()
        if meta == nil {
            errutil.WriteJSONError(w, errutil.New(
                errutil.CodeBadGateway,
                "empty meta",
            ).WithTrace(tid))
            return
        }

        // HEAD support
        if r.Method == http.MethodHead {
            w.Header().Set("Content-Length", fmt.Sprintf("%d", meta.GetSize()))
            w.Header().Set("Content-Type", "application/octet-stream")
            w.Header().Set("Content-Disposition", fmt.Sprintf("attachment; filename=%q", meta.GetFilename()))
            w.WriteHeader(http.StatusOK)
            log.Printf("[%s] HEAD %s 200 (%d bytes) %dms",
                tid, sha1, meta.GetSize(), time.Since(start).Milliseconds())
            return
        }

        // meta-only mode
        if r.URL.Query().Get("meta") == "1" {
            w.Header().Set("Content-Type", "application/json")
            _ = json.NewEncoder(w).Encode(meta)
            log.Printf("[%s] GET(meta) %s 200 %dms",
                tid, sha1, time.Since(start).Milliseconds())
            return
        }

        stream, err := client.GetStream(ctx, &libraryv1.GetStreamRequest{
            Id: &libraryv1.BookId{Sha1: sha1},
        })
        if err != nil {
            appErr := errutil.FromGRPCError(err, tid)
            errutil.WriteJSONError(w, appErr)
            return
        }

        w.Header().Set("Content-Type", "application/octet-stream")
        w.Header().Set("Content-Disposition", fmt.Sprintf("attachment; filename=%q", meta.GetFilename()))
        w.Header().Set("Content-Length", fmt.Sprintf("%d", meta.GetSize()))

        flusher, _ := w.(http.Flusher)

        var written int64

        for {
            ch, rerr := stream.Recv()
            if rerr == io.EOF {
                break
            }
            if rerr != nil {
                log.Printf("[%s] stream error: %v", tid, rerr)
                return
            }

            n, _ := w.Write(ch.GetData())
            written += int64(n)

            if flusher != nil {
                flusher.Flush()
            }
        }

        log.Printf("[%s] GET %s 200 (%d bytes) %dms",
            tid, sha1, written, time.Since(start).Milliseconds())
    })

    addr := dcfg.ListenAddr()
    log.Printf("[downloader] http listening on %s (plasma=%s)", addr, dcfg.PlasmaAddr)
    log.Fatal(http.ListenAndServe(addr, mux))
}

func isValidSHA1(s string) bool {
    if len(s) != 40 {
        return false
    }
    for _, c := range s {
        if !((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) {
            return false
        }
    }
    return true
}
