package main

import (
	"context"
	"net"

	"ebusta/api/proto/v1"
	"ebusta/internal/logger"
	"ebusta/internal/parser"

	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
)

type server struct {
	libraryv1.UnimplementedMessageConverterServiceServer
}

func (s *server) Convert(ctx context.Context, req *libraryv1.RawInput) (*libraryv1.UnmarshaledMessage, error) {
	defer logger.Track(ctx, "Converter: AST Parsing")()

	p := parser.NewParser(req.GetData())
	query := p.Parse()

	return &libraryv1.UnmarshaledMessage{
		// ГЛАВНЫЙ ФИКС: заполняем основное поле Query
		Query: query, 
		Meta: &libraryv1.MessageMeta{
			CanonicalForm: req.GetData(),
			AstPlan:       query, // Оставляем для совместимости, если нужно
		},
	}, nil
}

func main() {
	lis, err := net.Listen("tcp", ":50052")
	if err != nil {
		logrus.Fatalf("failed to listen: %v", err)
	}
	s := grpc.NewServer()
	libraryv1.RegisterMessageConverterServiceServer(s, &server{})
	logrus.Info("AST Translator started on :50052")
	if err := s.Serve(lis); err != nil {
		logrus.Fatalf("failed to serve: %v", err)
	}
}
