package delivery

import (
	"context"
	"ebusta/api/proto/v1"
	"ebusta/internal/logger"
)

type DataManagerServer struct {
	libraryv1.UnimplementedLibraryServiceServer
}

func (s *DataManagerServer) Search(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	defer logger.Track(ctx, "Storage: DB Search Operation")()

	books := []*libraryv1.Book{
		{
			Id:      "101",
			Title:   "The Art of Unix Programming",
			Authors: []string{"Eric S. Raymond"},
		},
	}

	return &libraryv1.SearchResponse{
		Books: books,
	}, nil
}
