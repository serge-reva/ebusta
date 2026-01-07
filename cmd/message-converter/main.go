package main

import (
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
