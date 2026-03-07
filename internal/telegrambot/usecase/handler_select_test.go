package usecase

import (
	"context"
	"strings"
	"testing"
	"time"

	"ebusta/internal/edge"
	"ebusta/internal/gatewayclient"
	corepresenter "ebusta/internal/presenter"
	tgpresenter "ebusta/internal/telegrambot/presenter"
	"ebusta/internal/telegrambot/session"
)

func TestHandleSelectBookUsesLastResult(t *testing.T) {
	store := session.NewMemoryStore()
	_ = store.Put(context.Background(), &session.Session{
		UserID:      "u1",
		Query:       "dune",
		PageSize:    5,
		CurrentPage: 1,
		LastResult: &corepresenter.PresenterResult{
			SearchResult: &corepresenter.SearchResult{
				Total: 1,
				Books: []corepresenter.BookDTO{{
					ID:          "sha1-1",
					Title:       "Dune",
					FullAuthors: "Frank Herbert",
					DownloadURL: "/download/t1",
				}},
			},
			Pagination: corepresenter.NewPagination(1, 1, 5),
		},
		UpdatedAt: time.Now(),
	})
	p := edge.DefaultPolicy("telegram")
	h := NewHandler(fakeSearcher{}, store, tgpresenter.NewTelegramFormatter(4096, "ebusta_test_bot"), edge.NewEngine(p, nil), 5)

	result, err := h.HandleSelectBook(context.Background(), "u1", 1, "tg-select")
	if err != nil {
		t.Fatalf("HandleSelectBook() error = %v", err)
	}
	if !result.ForceSend {
		t.Fatal("expected detail screen to be sent as new message")
	}
	if !strings.Contains(result.Text, "Dune") {
		t.Fatalf("unexpected result text: %s", result.Text)
	}
	if result.Keyboard == nil || result.Keyboard.InlineKeyboard[0][0].CallbackData != "download:sha1-1" {
		t.Fatalf("unexpected keyboard: %+v", result.Keyboard)
	}
}

func TestHandleDownloadSendsDocumentForSmallFiles(t *testing.T) {
	store := session.NewMemoryStore()
	_ = store.Put(context.Background(), &session.Session{
		UserID:      "u1",
		Query:       "dune",
		PageSize:    5,
		CurrentPage: 1,
		LastResult: &corepresenter.PresenterResult{
			SearchResult: &corepresenter.SearchResult{
				Total: 1,
				Books: []corepresenter.BookDTO{{
					ID:          "sha1-1",
					Title:       "Dune",
					FullAuthors: "Frank Herbert",
					DownloadURL: "/download/t1",
				}},
			},
			Pagination: corepresenter.NewPagination(1, 1, 5),
		},
		UpdatedAt: time.Now(),
	})
	p := edge.DefaultPolicy("telegram")
	h := NewHandler(fakeSearcher{
		meta: &gatewayclient.FileMeta{Size: 1024, Filename: "dune.fb2"},
		body: []byte("payload"),
	}, store, tgpresenter.NewTelegramFormatter(4096, "ebusta_test_bot"), edge.NewEngine(p, nil), 5)

	result, err := h.HandleDownload(context.Background(), "u1", "sha1-1", "tg-dl")
	if err != nil {
		t.Fatalf("HandleDownload() error = %v", err)
	}
	if result.Document == nil {
		t.Fatal("expected document payload")
	}
	if result.Document.Filename != "dune.fb2" {
		t.Fatalf("unexpected filename: %s", result.Document.Filename)
	}
}

func TestHandleDownloadRejectsLargeFiles(t *testing.T) {
	store := session.NewMemoryStore()
	_ = store.Put(context.Background(), &session.Session{
		UserID:      "u1",
		Query:       "dune",
		PageSize:    5,
		CurrentPage: 1,
		LastResult: &corepresenter.PresenterResult{
			SearchResult: &corepresenter.SearchResult{
				Total: 1,
				Books: []corepresenter.BookDTO{{
					ID:          "sha1-1",
					Title:       "Dune",
					FullAuthors: "Frank Herbert",
					DownloadURL: "/download/t1",
				}},
			},
			Pagination: corepresenter.NewPagination(1, 1, 5),
		},
		UpdatedAt: time.Now(),
	})
	p := edge.DefaultPolicy("telegram")
	h := NewHandler(fakeSearcher{
		meta: &gatewayclient.FileMeta{Size: 30 * 1024 * 1024, Filename: "dune.fb2"},
	}, store, tgpresenter.NewTelegramFormatter(4096, "ebusta_test_bot"), edge.NewEngine(p, nil), 5)

	result, err := h.HandleDownload(context.Background(), "u1", "sha1-1", "tg-dl")
	if err != nil {
		t.Fatalf("HandleDownload() error = %v", err)
	}
	if result.Document != nil {
		t.Fatal("did not expect document for large file")
	}
	if !strings.Contains(result.Text, "too large") {
		t.Fatalf("unexpected result text: %s", result.Text)
	}
}
