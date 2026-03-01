package main

import (
    "context"
    "encoding/json"
    "fmt"
    "io"
    "net/http"
    "os"
    "os/signal"
    "strings"
    "syscall"
    "time"

    libraryv1 "ebusta/api/proto/v1"
    "ebusta/internal/config"
    "ebusta/internal/errutil"
    "ebusta/internal/logger"

    "google.golang.org/grpc"
    "google.golang.org/grpc/credentials/insecure"
)

func main() {
    cfg := config.Get()
    logger.InitFromConfig(cfg.Logger, "downloader")

    dcfg := cfg.Downloads.Downloader
    if err := dcfg.Validate(); err != nil {
        logger.GetGlobal().FatalCtx(context.Background(), "[downloader] config error", err)
    }

    conn, err := grpc.Dial(dcfg.PlasmaAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
    if err != nil {
        logger.GetGlobal().FatalCtx(context.Background(), "[downloader] plasma connect error", err)
    }
    defer conn.Close()

    client := libraryv1.NewStorageNodeClient(conn)

    mux := http.NewServeMux()

    mux.HandleFunc("/books/", func(w http.ResponseWriter, r *http.Request) {
        start := time.Now()

        tid := errutil.TraceIDFromRequest(r)
        if tid == "" {
            tid = errutil.GenerateTraceID("dl")
        }
        w.Header().Set("X-Trace-Id", tid)

        sha1 := strings.TrimPrefix(r.URL.Path, "/books/")
        sha1 = strings.Trim(sha1, "/")
        if sha1 == "" {
            logger.GetGlobal().WithField("trace_id", tid).WarnCtx(r.Context(), "[downloader] missing id")
            errutil.WriteJSONError(w, errutil.New(
                errutil.CodeInvalidArgument,
                "missing id",
            ).WithTrace(tid))
            return
        }

        if !isValidSHA1(sha1) {
            logger.GetGlobal().WithField("sha1", sha1).WithField("trace_id", tid).WarnCtx(r.Context(), "[downloader] invalid sha1")
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
            logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[downloader] GetMeta error", err)
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
            logger.GetGlobal().WithField("code", appErr.Code).WithField("message", appErr.Message).WarnCtx(ctx, "[downloader] meta error")
            errutil.WriteJSONError(w, appErr)
            return
        }

        meta := metaResp.GetMeta()
        if meta == nil {
            logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[downloader] empty meta", nil)
            errutil.WriteJSONError(w, errutil.New(
                errutil.CodeBadGateway,
                "empty meta",
            ).WithTrace(tid))
            return
        }

        if r.Method == http.MethodHead {
            w.Header().Set("Content-Length", fmt.Sprintf("%d", meta.GetSize()))
            w.Header().Set("Content-Type", "application/octet-stream")
            w.Header().Set("Content-Disposition", fmt.Sprintf("attachment; filename=%q", meta.GetFilename()))
            w.WriteHeader(http.StatusOK)
            logger.GetGlobal().WithField("sha1", sha1).
                WithField("size", meta.GetSize()).
                WithField("duration_ms", time.Since(start).Milliseconds()).
                InfoCtx(ctx, "[downloader] HEAD")
            return
        }

        if r.URL.Query().Get("meta") == "1" {
            w.Header().Set("Content-Type", "application/json")
            _ = json.NewEncoder(w).Encode(meta)
            logger.GetGlobal().WithField("sha1", sha1).
                WithField("duration_ms", time.Since(start).Milliseconds()).
                InfoCtx(ctx, "[downloader] GET(meta)")
            return
        }

        stream, err := client.GetStream(ctx, &libraryv1.GetStreamRequest{
            Id: &libraryv1.BookId{Sha1: sha1},
        })
        if err != nil {
            appErr := errutil.FromGRPCError(err, tid)
            logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[downloader] GetStream error", err)
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
                logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[downloader] stream error", rerr)
                return
            }

            n, _ := w.Write(ch.GetData())
            written += int64(n)

            if flusher != nil {
                flusher.Flush()
            }
        }

        logger.GetGlobal().WithField("sha1", sha1).
            WithField("size", written).
            WithField("duration_ms", time.Since(start).Milliseconds()).
            InfoCtx(ctx, "[downloader] GET")
    })

    addr := dcfg.ListenAddr()
    server := &http.Server{
        Addr:    addr,
        Handler: mux,
    }

    serveErr := make(chan error, 1)
    go func() {
        logger.GetGlobal().WithField("addr", addr).WithField("plasma", dcfg.PlasmaAddr).InfoCtx(context.Background(), "[downloader] listening")
        if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
            serveErr <- err
        }
    }()

    stop := make(chan os.Signal, 1)
    signal.Notify(stop, os.Interrupt, syscall.SIGTERM)

    select {
    case sig := <-stop:
        logger.GetGlobal().WithField("signal", sig).InfoCtx(context.Background(), "[downloader] shutting down")
        ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
        defer cancel()
        if err := server.Shutdown(ctx); err != nil {
            logger.GetGlobal().ErrorCtx(context.Background(), "[downloader] shutdown error", err)
        }
    case err := <-serveErr:
        logger.GetGlobal().FatalCtx(context.Background(), "[downloader] serve error", err)
    }

    logger.GetGlobal().InfoCtx(context.Background(), "[downloader] stopped")
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
