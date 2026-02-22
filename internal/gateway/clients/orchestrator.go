package clients

import (
	"context"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

type OrchestratorClient struct {
	conn   *grpc.ClientConn
	client libraryv1.OrchestratorServiceClient
}

func NewOrchestratorClient(cfg *config.GatewayRuntimeConfig) (*OrchestratorClient, error) {
	var opts []grpc.DialOption

	if cfg.MTLS.Enabled {
		// Здесь будет mTLS, пока не реализовано
	}

	opts = append(opts, grpc.WithTransportCredentials(insecure.NewCredentials()))

	conn, err := grpc.Dial(cfg.Services.Orchestrator, opts...)
	if err != nil {
		return nil, err
	}

	return &OrchestratorClient{
		conn:   conn,
		client: libraryv1.NewOrchestratorServiceClient(conn),
	}, nil
}

func (c *OrchestratorClient) Close() error {
	return c.conn.Close()
}

type SearchRequest struct {
	Query   string
	Page    int
	Limit   int
	TraceID string
}

type Book struct {
	ID      string
	Title   string
	Authors []string
}

type SearchResult struct {
	TraceID string
	Status  string
	Total   int
	Books   []Book
}

func (c *OrchestratorClient) Search(ctx context.Context, req *SearchRequest) (*SearchResult, error) {
	limit := req.Limit
	if limit == 0 {
		limit = 20
	}

	offset := (req.Page - 1) * limit

	resp, err := c.client.Search(ctx, &libraryv1.SearchRequest{
		Query:   req.Query,
		Limit:   int32(limit),
		Offset:  int32(offset),
		TraceId: req.TraceID,
	})
	if err != nil {
		return nil, err
	}

	result := &SearchResult{
		TraceID: req.TraceID, // используем TraceID из запроса
		Status:  resp.GetStatus(),
		Total:   int(resp.GetTotal()),
		Books:   make([]Book, 0, len(resp.GetBooks())),
	}

	for _, b := range resp.GetBooks() {
		result.Books = append(result.Books, Book{
			ID:      b.GetId(),
			Title:   b.GetTitle(),
			Authors: b.GetAuthors(),
		})
	}

	return result, nil
}
