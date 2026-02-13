package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"strings"
	"time"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"

	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/credentials/insecure"
	"google.golang.org/grpc/metadata"
	"google.golang.org/grpc/status"
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
		// TraceID — как в web-adapter
		tid := r.Header.Get("X-Trace-Id")
		if tid == "" {
			tid = fmt.Sprintf("dl-%d", time.Now().UnixNano())
		}
		w.Header().Set("X-Trace-Id", tid)

		sha1 := strings.TrimPrefix(r.URL.Path, "/books/")
		sha1 = strings.Trim(sha1, "/")
		if sha1 == "" {
			http.Error(w, "missing id", http.StatusBadRequest)
			return
		}
		if !isValidSHA1(sha1) {
			http.Error(w, "invalid sha1 (expected 40 hex)", http.StatusBadRequest)
			return
		}

		ctx := withTraceID(r.Context(), tid)

		// meta-only mode
		if r.URL.Query().Get("meta") == "1" {
			metaResp, err := client.GetMeta(ctx, &libraryv1.GetMetaRequest{
				Id: &libraryv1.BookId{Sha1: sha1},
			})
			if err != nil {
				writeGRPCError(w, err)
				return
			}
			if metaResp.GetError() != nil {
				writeUpstreamPayloadError(w, metaResp.GetError())
				return
			}
			w.Header().Set("Content-Type", "application/json")
			_ = json.NewEncoder(w).Encode(metaResp.GetMeta())
			return
		}

		// For HEAD and GET (download bytes): we need meta first
		metaResp, err := client.GetMeta(ctx, &libraryv1.GetMetaRequest{
			Id: &libraryv1.BookId{Sha1: sha1},
		})
		if err != nil {
			writeGRPCError(w, err)
			return
		}
		if metaResp.GetError() != nil {
			writeUpstreamPayloadError(w, metaResp.GetError())
			return
		}

		meta := metaResp.GetMeta()
		if meta == nil {
			http.Error(w, "upstream error: empty meta", http.StatusBadGateway)
			return
		}

		expected := meta.GetSize()
		// Заголовки для скачивания
		w.Header().Set("Content-Type", "application/octet-stream")
		w.Header().Set("Content-Disposition", fmt.Sprintf("attachment; filename=%q", meta.GetFilename()))
		if expected > 0 {
			w.Header().Set("Content-Length", fmt.Sprintf("%d", expected))
		}

		// HEAD: только заголовки, без тела
		if r.Method == http.MethodHead {
			w.WriteHeader(http.StatusOK)
			return
		}

		// GET: stream file bytes
		st, err := client.GetStream(ctx, &libraryv1.GetStreamRequest{
			Id: &libraryv1.BookId{Sha1: sha1},
		})
		if err != nil {
			writeGRPCError(w, err)
			return
		}

		flusher, _ := w.(http.Flusher)

		var written int64
		for {
			ch, rerr := st.Recv()
			if rerr != nil {
				if rerr == io.EOF {
					break
				}
				if rerr == context.Canceled {
					return
				}
				log.Printf("[%s] stream recv error: %v", tid, rerr)
				return
			}
			if len(ch.GetData()) == 0 {
				continue
			}
			n, _ := w.Write(ch.GetData())
			written += int64(n)
			if flusher != nil {
				flusher.Flush()
			}
		}

		if expected > 0 && written != expected {
			log.Printf("[%s] size mismatch sha1=%s expected=%d written=%d", tid, sha1, expected, written)
		}
	})

	addr := dcfg.ListenAddr()
	log.Printf("[downloader] http listening on %s (plasma=%s)", addr, dcfg.PlasmaAddr)
	log.Fatal(http.ListenAndServe(addr, mux))
}

func withTraceID(ctx context.Context, tid string) context.Context {
	md := metadata.Pairs("x-trace-id", tid)
	return metadata.NewOutgoingContext(ctx, md)
}

func writeUpstreamPayloadError(w http.ResponseWriter, e *libraryv1.Error) {
	// Upstream payload envelope: code is string (e.g. NOT_FOUND/INTERNAL)
	switch strings.ToUpper(strings.TrimSpace(e.GetCode())) {
	case "NOT_FOUND":
		http.Error(w, e.GetMessage(), http.StatusNotFound)
	case "INVALID_ARGUMENT":
		http.Error(w, e.GetMessage(), http.StatusBadRequest)
	case "UNAVAILABLE":
		http.Error(w, e.GetMessage(), http.StatusServiceUnavailable)
	default:
		http.Error(w, e.GetMessage(), http.StatusBadGateway)
	}
}

func writeGRPCError(w http.ResponseWriter, err error) {
	st, ok := status.FromError(err)
	if !ok {
		http.Error(w, err.Error(), http.StatusBadGateway)
		return
	}

	switch st.Code() {
	case codes.InvalidArgument:
		http.Error(w, st.Message(), http.StatusBadRequest)
	case codes.NotFound:
		http.Error(w, st.Message(), http.StatusNotFound)
	case codes.Unavailable:
		http.Error(w, st.Message(), http.StatusServiceUnavailable)
	case codes.DeadlineExceeded:
		http.Error(w, st.Message(), http.StatusGatewayTimeout)
	default:
		http.Error(w, st.Message(), http.StatusBadGateway)
	}
}

func isValidSHA1(s string) bool {
	if len(s) != 40 {
		return false
	}
	for i := 0; i < len(s); i++ {
		c := s[i]
		if (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') {
			continue
		}
		return false
	}
	return true
}
