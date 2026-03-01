//go:build ignore
// +build ignore

// Legacy archived service. MessageConverterService was removed from api/proto/v1/library.proto.
// This file is kept only for historical reference and must not be built.
package main

import (
	"os"
	"context"
	"fmt"
	"log"
	"net"

	"ebusta/api/proto/v1"
	"ebusta/internal/parser"
	"google.golang.org/grpc"
)

type server struct {
	libraryv1.UnimplementedMessageConverterServiceServer
}

func (s *server) Convert(ctx context.Context, req *libraryv1.RawInput) (*libraryv1.UnmarshaledMessage, error) {
	log.Printf("🔄 Converter parsing: %s", req.Data)

	// Теперь эта функция существует в internal/parser/parser.go
	queryAst := parser.Parse(req.Data)

	return &libraryv1.UnmarshaledMessage{
		Meta: &libraryv1.MessageMeta{
			TraceId:       req.TraceId,
			CanonicalForm: req.Data,
			// Преобразуем структуру AST в строку для логов/отладки
			AstPlan:       fmt.Sprintf("%v", queryAst),
		},
		Query: queryAst,
	}, nil
}

func main() {
	// Legacy guard: DSL must be served by Lisp module. Prevent accidental port clash on :50052.
	if os.Getenv("ENABLE_LEGACY_CONVERTER") != "1" {
		log.Println("cmd/message-converter is legacy and disabled by default (set ENABLE_LEGACY_CONVERTER=1 to run).")
		return
	}

	lis, err := net.Listen("tcp", ":50052")
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	libraryv1.RegisterMessageConverterServiceServer(s, &server{})

	log.Println("🔄 MessageConverter started on :50052")
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
