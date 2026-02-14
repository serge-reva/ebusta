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

type errorEnvelope struct {
	Error errorBody `json:"error"`
}

type errorBody struct {
	Code    string `json:"code"`
	Message string `json:"message"`
	TraceID string `json:"trace_id"`
}

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

		tid := r.Header.Get("X-Trace-Id")
		if tid == "" {
			tid = fmt.Sprintf("dl-%d", time.Now().UnixNano())
		}
		w.Header().Set("X-Trace-Id", tid)

		sha1 := strings.TrimPrefix(r.URL.Path, "/books/")
		sha1 = strings.Trim(sha1, "/")
		if sha1 == "" {
			writeJSONError(w, http.StatusBadRequest, "INVALID_ARGUMENT", "missing id", tid)
			return
		}

		if !isValidSHA1(sha1) {
			writeJSONError(w, http.StatusBadRequest, "INVALID_ARGUMENT", "invalid sha1 (expected 40 hex)", tid)
			return
		}

		ctx := withTraceID(r.Context(), tid)

		metaResp, err := client.GetMeta(ctx, &libraryv1.GetMetaRequest{
			Id: &libraryv1.BookId{Sha1: sha1},
		})
		if err != nil {
			writeGRPCError(w, err, tid)
			return
		}

		if metaResp.GetError() != nil {
			writeJSONError(w, http.StatusBadGateway, metaResp.GetError().GetCode(), metaResp.GetError().GetMessage(), tid)
			return
		}

		meta := metaResp.GetMeta()
		if meta == nil {
			writeJSONError(w, http.StatusBadGateway, "UPSTREAM_ERROR", "empty meta", tid)
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
			writeGRPCError(w, err, tid)
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
				if stErr, ok := status.FromError(rerr); ok {
					log.Printf("[%s] stream error: %v", tid, stErr.Message())
					return
				}
				log.Printf("[%s] stream recv error: %v", tid, rerr)
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

func withTraceID(ctx context.Context, tid string) context.Context {
	md := metadata.Pairs("x-trace-id", tid)
	return metadata.NewOutgoingContext(ctx, md)
}

func writeGRPCError(w http.ResponseWriter, err error, tid string) {
	st, ok := status.FromError(err)
	if !ok {
		writeJSONError(w, http.StatusBadGateway, "BAD_GATEWAY", err.Error(), tid)
		return
	}

	switch st.Code() {
	case codes.InvalidArgument:
		writeJSONError(w, http.StatusBadRequest, "INVALID_ARGUMENT", st.Message(), tid)
	case codes.NotFound:
		writeJSONError(w, http.StatusNotFound, "NOT_FOUND", st.Message(), tid)
	case codes.Unavailable:
		writeJSONError(w, http.StatusServiceUnavailable, "UNAVAILABLE", st.Message(), tid)
	case codes.DeadlineExceeded:
		writeJSONError(w, http.StatusGatewayTimeout, "DEADLINE_EXCEEDED", st.Message(), tid)
	default:
		writeJSONError(w, http.StatusBadGateway, "UPSTREAM_ERROR", st.Message(), tid)
	}
}

func writeJSONError(w http.ResponseWriter, statusCode int, code, message, tid string) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(statusCode)

	resp := errorEnvelope{
		Error: errorBody{
			Code:    code,
			Message: message,
			TraceID: tid,
		},
	}

	_ = json.NewEncoder(w).Encode(resp)
}
