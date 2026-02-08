package search

import (
	"context"
	"errors"
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

// NewService — удобный конструктор для адаптеров (CLI/web-adapter):
// возвращает nil при ошибке подключения.
func NewService() *Service {
	s, err := New()
	if err != nil {
		return nil
	}
	return s
}

// Close закрывает gRPC соединение
func (s *Service) Close() error {
	if s != nil && s.conn != nil {
		return s.conn.Close()
	}
	return nil
}

// Search выполняет запрос к оркестратору и мапит результат в DTO.
// traceID генерируется на edge и здесь НЕ создаётся.
func (s *Service) Search(ctx context.Context, query string, limit int, traceID string) (*SearchResult, error) {
	if s == nil {
		return nil, errors.New("search service is nil")
	}
	if traceID == "" {
		return nil, errors.New("traceID is empty")
	}

	q := Normalize(query)
	if err := Validate(q); err != nil {
		return nil, err
	}

	resp, err := s.client.Search(ctx, &libraryv1.SearchRequest{
		Query:   q,
		Limit:  int32(limit),
		TraceId: traceID,
	})
	if err != nil {
		return nil, err
	}

	res := &SearchResult{
		TraceId: traceID, // возвращаем тот же TraceID, который отправили
		Total:   int(resp.GetTotal()),
		Books:   make([]BookDTO, 0, len(resp.GetBooks())),
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
