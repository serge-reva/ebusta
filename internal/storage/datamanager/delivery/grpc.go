package delivery

import (
	"context"
	"ebusta/api/proto/v1"
	"ebusta/internal/logger"
)

type DataManagerServer struct {
	libraryv1.UnimplementedLibraryServiceServer
}

// ИСПРАВЛЕНИЕ: Метод должен называться SearchBooks, чтобы соответствовать интерфейсу
func (s *DataManagerServer) SearchBooks(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	// Если логгер еще не настроен, используем простой принт или заглушку
	if logger.For(ctx) != nil {
		defer logger.Track(ctx, "Storage: DB Search Operation")()
	}

	// Моковые данные для теста
	books := []*libraryv1.Book{
		{
			Id:      "101",
			Title:   "The Art of Unix Programming",
			Authors: []string{"Eric S. Raymond"},
		},
	}

	return &libraryv1.SearchResponse{
		Books: books,
		Total: int32(len(books)),
	}, nil
}

func (s *DataManagerServer) GetAuthors(ctx context.Context, req *libraryv1.ListRequest) (*libraryv1.ListResponse, error) {
	return &libraryv1.ListResponse{Items: []string{"King", "Tolkien"}}, nil
}
