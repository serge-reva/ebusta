package main

import (
	"context"
	"encoding/json"
	"log"
	"net"
	"os"
	"strings"

	"ebusta/api/proto/v1"
	"google.golang.org/grpc"
)

type server struct {
	libraryv1.UnimplementedDataServiceServer
}

func matchBook(book *libraryv1.Book, q *libraryv1.SearchQuery) bool {
	if q == nil {
		return true
	}

	switch n := q.Node.(type) {
	case *libraryv1.SearchQuery_Filter:
		f := n.Filter
		field := strings.ToLower(f.Field)
		val := strings.ToLower(f.Value)
		
		// Логика "Any" поиска или пустого поля
		if field == "any" || field == "" {
			// Ищем везде: в ID, в Названии и в Списке авторов
			allText := book.Id + " " + book.Title + " " + strings.Join(book.Authors, " ")
			return strings.Contains(strings.ToLower(allText), val)
		}

		// Поиск по конкретным полям
		target := ""
		if field == "author" {
			target = strings.Join(book.Authors, " ")
		} else if field == "title" {
			target = book.Title
		} else if field == "id" {
			target = book.Id
		}
		
		return strings.Contains(strings.ToLower(target), val)

	case *libraryv1.SearchQuery_Logical:
		l := n.Logical
		if len(l.Nodes) == 0 {
			return true
		}
		if l.Op == libraryv1.LogicalOp_OR {
			for _, sub := range l.Nodes {
				if matchBook(book, sub) {
					return true
				}
			}
			return false
		} else { // AND
			for _, sub := range l.Nodes {
				if !matchBook(book, sub) {
					return false
				}
			}
			return true
		}

	case *libraryv1.SearchQuery_Negation:
		return !matchBook(book, n.Negation.Node)
	}

	return true
}

func (s *server) GetData(ctx context.Context, req *libraryv1.DataRequest) (*libraryv1.DataResponse, error) {
	data, err := os.ReadFile("books.json")
	if err != nil {
		return nil, err
	}

	var allBooks []libraryv1.Book
	if err := json.Unmarshal(data, &allBooks); err != nil {
		return nil, err
	}

	var filteredBooks []*libraryv1.Book
	for i := range allBooks {
		if req.Query == nil || matchBook(&allBooks[i], req.Query) {
			filteredBooks = append(filteredBooks, &allBooks[i])
		}
	}

	return &libraryv1.DataResponse{Status: "OK", Books: filteredBooks}, nil
}

func main() {
	lis, _ := net.Listen("tcp", ":50051")
	srv := grpc.NewServer()
	libraryv1.RegisterDataServiceServer(srv, &server{})
	log.Println("Mercury (Data Manager) GLOBAL-SEARCH started on :50051")
	srv.Serve(lis)
}
