package search

import (
	"context"
	"strings"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

// Service инкапсулирует логику поиска и общения с оркестратором
type Service struct {
	client libraryv1.OrchestratorServiceClient
	conn   *grpc.ClientConn
}

// New создает новый экземпляр сервиса поиска, используя глобальный конфиг
func New() (*Service, error) {
	cfg := config.Get()
	addr := cfg.Orchestrator.Address()

	conn, err := grpc.Dial(addr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		return nil, err
	}

	return &Service{
		client: libraryv1.NewOrchestratorServiceClient(conn),
		conn:   conn,
	}, nil
}

// Close закрывает gRPC соединение
func (s *Service) Close() error {
	if s.conn != nil {
		return s.conn.Close()
	}
	return nil
}

// Search выполняет запрос к оркестратору и мапит результат в DTO
func (s *Service) Search(ctx context.Context, query string, limit int) (*SearchResult, error) {
	resp, err := s.client.Search(ctx, &libraryv1.SearchRequest{
		Query: query,
		Limit: int32(limit),
	})
	if err != nil {
		return nil, err
	}

	res := &SearchResult{
		Total: int(resp.GetTotal()),
		Books: make([]BookDTO, 0, len(resp.GetBooks())),
	}

	for _, b := range resp.GetBooks() {
		authors := b.GetAuthors()
		fullAuthors := strings.Join(authors, ", ")
		if fullAuthors == "" {
			fullAuthors = "Unknown"
		}

		res.Books = append(res.Books, BookDTO{
			ID:          b.GetId(),
			Title:       b.GetTitle(),
			Authors:     authors,
			Container:   b.GetContainer(),
			Filename:    b.GetFilename(),
			FullAuthors: fullAuthors,
		})
	}

	return res, nil
}
