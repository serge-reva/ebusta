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
			if perr := metaResp.GetError(); perr != nil {
				writePayloadError(w, perr)
				return
			}
			w.Header().Set("Content-Type", "application/json")
			_ = json.NewEncoder(w).Encode(metaResp.GetMeta())
			return
		}

		// default: stream file bytes
		metaResp, err := client.GetMeta(ctx, &libraryv1.GetMetaRequest{
			Id: &libraryv1.BookId{Sha1: sha1},
		})
		if err != nil {
			writeGRPCError(w, err)
			return
		}
		if perr := metaResp.GetError(); perr != nil {
			writePayloadError(w, perr)
			return
		}
		meta := metaResp.GetMeta()
		if meta == nil {
			http.Error(w, "upstream error: empty meta", http.StatusBadGateway)
			return
		}

		st, err := client.GetStream(ctx, &libraryv1.GetStreamRequest{
			Id: &libraryv1.BookId{Sha1: sha1},
		})
		if err != nil {
			writeGRPCError(w, err)
			return
		}

		// Заголовки для скачивания
		w.Header().Set("Content-Type", "application/octet-stream")
		w.Header().Set("Content-Disposition", fmt.Sprintf("attachment; filename=%q", meta.GetFilename()))

		flusher, _ := w.(http.Flusher)

		for {
			ch, rerr := st.Recv()
			if rerr != nil {
				if rerr == io.EOF {
					// нормальное завершение стрима
					return
				}
				if rerr == context.Canceled {
					return
				}
				if _, ok := status.FromError(rerr); ok {
					// после начала стрима заголовки уже отданы — статусом HTTP не исправить
					log.Printf("[%s] stream error after headers: %v", tid, rerr)
					return
				}
				log.Printf("[%s] stream recv error: %v", tid, rerr)
				return
			}
			if len(ch.GetData()) == 0 {
				continue
			}
			_, _ = w.Write(ch.GetData())
			if flusher != nil {
				flusher.Flush()
			}
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

func isValidSHA1(s string) bool {
	if len(s) != 40 {
		return false
	}
	for _, c := range s {
		switch {
		case c >= '0' && c <= '9':
		case c >= 'a' && c <= 'f':
		case c >= 'A' && c <= 'F':
		default:
			return false
		}
	}
	return true
}

func writePayloadError(w http.ResponseWriter, e *libraryv1.Error) {
	code := strings.ToUpper(strings.TrimSpace(e.GetCode()))
	msg := strings.TrimSpace(e.GetMessage())
	statusCode := http.StatusBadGateway

	switch code {
	case "NOT_FOUND":
		statusCode = http.StatusNotFound
	case "INVALID_ARGUMENT":
		statusCode = http.StatusBadRequest
	case "UNAVAILABLE":
		statusCode = http.StatusServiceUnavailable
	case "DEADLINE_EXCEEDED":
		statusCode = http.StatusGatewayTimeout
	case "INTERNAL":
		// в plasma сейчас может приходить INTERNAL, но message содержит grpc code
		m := strings.ToLower(msg)
		switch {
		case strings.Contains(m, "code = invalidargument"):
			statusCode = http.StatusBadRequest
		case strings.Contains(m, "code = notfound"):
			statusCode = http.StatusNotFound
		case strings.Contains(m, "code = unavailable"):
			statusCode = http.StatusServiceUnavailable
		case strings.Contains(m, "code = deadlineexceeded"):
			statusCode = http.StatusGatewayTimeout
		default:
			statusCode = http.StatusBadGateway
		}
	default:
		statusCode = http.StatusBadGateway
	}

	if msg == "" {
		msg = code
	}
	http.Error(w, msg, statusCode)
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
