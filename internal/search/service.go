package search

import (
    "context"
    "strings"

    libraryv1 "ebusta/api/proto/v1"
    "ebusta/internal/config"
    "ebusta/internal/errutil"
    "ebusta/internal/presenter"

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

// NewService — удобный конструктор для адаптеров (CLI/web-adapter)
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

// Search выполняет запрос к оркестратору и мапит результат в DTO
func (s *Service) Search(ctx context.Context, query string, limit int, offset int, traceID string) (*SearchResult, error) {
    if s == nil {
        return nil, errutil.New(errutil.CodeInternal, "search service is nil").WithTrace(traceID)
    }
    if traceID == "" {
        return nil, errutil.New(errutil.CodeInternal, "traceID is empty")
    }

    q := Normalize(query)
    if err := Validate(q); err != nil {
        return nil, errutil.New(errutil.CodeInvalidArgument, err.Error()).WithTrace(traceID)
    }

    outCtx := errutil.ContextWithTraceID(ctx, traceID)
    resp, err := s.client.Search(outCtx, &libraryv1.SearchRequest{
        Query:   q,
        Limit:   int32(limit),
        Offset:  int32(offset),
        TraceId: traceID,
    })
    if err != nil {
        return nil, errutil.FromGRPCError(err, traceID)
    }

    res := &SearchResult{
        TraceId: traceID,
        Total:   int(resp.GetTotal()),
        Books:   make([]BookDTO, 0, len(resp.GetBooks())),
    }

    for _, b := range resp.GetBooks() {
        authors := b.GetAuthors()
        var fullAuthors string
        if len(authors) > 0 {
            fullAuthors = strings.Join(authors, ", ")
        } else {
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

// convertToPresenter преобразует search.SearchResult в presenter.SearchResult
func convertToPresenter(sr *SearchResult) *presenter.SearchResult {
    books := make([]presenter.BookDTO, len(sr.Books))
    for i, b := range sr.Books {
        books[i] = presenter.BookDTO{
            ID:          b.ID,
            Title:       b.Title,
            Authors:     b.Authors,
            Container:   b.Container,
            Filename:    b.Filename,
            FullAuthors: b.FullAuthors,
        }
    }
    
    return &presenter.SearchResult{
        TraceId: sr.TraceId,
        Total:   sr.Total,
        Books:   books,
    }
}

// SearchWithPagination выполняет поиск и возвращает результат с пагинацией
func (s *Service) SearchWithPagination(ctx context.Context, query string, pageSize, offset, page int, traceID string) (*presenter.PresenterResult, error) {
    result, err := s.Search(ctx, query, pageSize, offset, traceID)
    if err != nil {
        return nil, err
    }
    
    presenterResult := convertToPresenter(result)
    return presenter.NewPresenterResult(presenterResult, page, pageSize), nil
}
