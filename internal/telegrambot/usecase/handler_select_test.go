package usecase

import (
	"context"
	"strings"
	"testing"
	"time"

	"ebusta/internal/edge"
	"ebusta/internal/errutil"
	"ebusta/internal/gatewayclient"
	corepresenter "ebusta/internal/presenter"
	tgpresenter "ebusta/internal/telegrambot/presenter"
	"ebusta/internal/telegrambot/session"
)

func TestHandleSelectBookUsesLastResult(t *testing.T) {
	store := session.NewMemoryStore()
	_ = store.Put(context.Background(), &session.Session{
		UserID:            "u1",
		Query:             "dune",
		PageSize:          5,
		CurrentPage:       1,
		LastListMessageID: 321,
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
	if !strings.Contains(result.Text, "Dune") {
		t.Fatalf("unexpected result text: %s", result.Text)
	}
	if result.Keyboard == nil || result.Keyboard.InlineKeyboard[0][0].CallbackData != "download:sha1-1" {
		t.Fatalf("unexpected keyboard: %+v", result.Keyboard)
	}
	if result.TargetMessageID != 321 || result.ForceSend {
		t.Fatalf("expected detail view to edit existing list message, got target=%d force=%v", result.TargetMessageID, result.ForceSend)
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

func TestHandleDownloadMapsMetaNotFoundError(t *testing.T) {
	store := session.NewMemoryStore()
	_ = putDownloadSession(store)
	p := edge.DefaultPolicy("telegram")
	h := NewHandler(fakeSearcher{
		err: errutil.New(errutil.CodeNotFound, "missing in storage").WithTrace("tg-notfound"),
	}, store, tgpresenter.NewTelegramFormatter(4096, "ebusta_test_bot"), edge.NewEngine(p, nil), 5)

	result, err := h.HandleDownload(context.Background(), "u1", "sha1-1", "tg-notfound")
	if err != nil {
		t.Fatalf("HandleDownload() error = %v", err)
	}
	if result.Document != nil {
		t.Fatal("did not expect document on meta error")
	}
	if !strings.Contains(result.Text, "Книга не найдена в хранилище") {
		t.Fatalf("unexpected result text: %s", result.Text)
	}
}

func TestHandleDownloadMapsMetaUnavailableError(t *testing.T) {
	store := session.NewMemoryStore()
	_ = putDownloadSession(store)
	p := edge.DefaultPolicy("telegram")
	h := NewHandler(fakeSearcher{
		err: errutil.New(errutil.CodeUnavailable, "gateway timeout").WithTrace("tg-unavail"),
	}, store, tgpresenter.NewTelegramFormatter(4096, "ebusta_test_bot"), edge.NewEngine(p, nil), 5)

	result, err := h.HandleDownload(context.Background(), "u1", "sha1-1", "tg-unavail")
	if err != nil {
		t.Fatalf("HandleDownload() error = %v", err)
	}
	if result.Document != nil {
		t.Fatal("did not expect document on meta error")
	}
	if !strings.Contains(result.Text, "Сервис временно недоступен") {
		t.Fatalf("unexpected result text: %s", result.Text)
	}
}

func TestHandleDownloadMapsMetaInternalError(t *testing.T) {
	store := session.NewMemoryStore()
	_ = putDownloadSession(store)
	p := edge.DefaultPolicy("telegram")
	h := NewHandler(fakeSearcher{
		err: errutil.New(errutil.CodeInternal, "boom").WithTrace("tg-int"),
	}, store, tgpresenter.NewTelegramFormatter(4096, "ebusta_test_bot"), edge.NewEngine(p, nil), 5)

	result, err := h.HandleDownload(context.Background(), "u1", "sha1-1", "tg-int")
	if err != nil {
		t.Fatalf("HandleDownload() error = %v", err)
	}
	if result.Document != nil {
		t.Fatal("did not expect document on meta error")
	}
	if !strings.Contains(result.Text, "Внутренняя ошибка") {
		t.Fatalf("unexpected result text: %s", result.Text)
	}
}

func TestHandleDownloadMapsUnknownMetaErrorCode(t *testing.T) {
	store := session.NewMemoryStore()
	_ = putDownloadSession(store)
	p := edge.DefaultPolicy("telegram")
	h := NewHandler(fakeSearcher{
		err: errutil.New(errutil.CodeForbidden, "blocked").WithTrace("tg-forbidden"),
	}, store, tgpresenter.NewTelegramFormatter(4096, "ebusta_test_bot"), edge.NewEngine(p, nil), 5)

	result, err := h.HandleDownload(context.Background(), "u1", "sha1-1", "tg-forbidden")
	if err != nil {
		t.Fatalf("HandleDownload() error = %v", err)
	}
	if result.Document != nil {
		t.Fatal("did not expect document on meta error")
	}
	if !strings.Contains(result.Text, errutil.CodeForbidden) {
		t.Fatalf("unexpected result text: %s", result.Text)
	}
}

func TestHandleBackTargetsStoredBookMessage(t *testing.T) {
	store := session.NewMemoryStore()
	_ = store.Put(context.Background(), &session.Session{
		UserID:            "u1",
		Query:             "dune",
		PageSize:          5,
		CurrentPage:       1,
		LastListMessageID: 111,
		LastBookMessageID: 222,
		LastResult: &corepresenter.PresenterResult{
			SearchResult: &corepresenter.SearchResult{
				Total: 1,
				Books: []corepresenter.BookDTO{{ID: "sha1-1", Title: "Dune", FullAuthors: "Frank Herbert", DownloadURL: "/download/t1"}},
			},
			Pagination: corepresenter.NewPagination(1, 1, 5),
		},
		UpdatedAt: time.Now(),
	})
	p := edge.DefaultPolicy("telegram")
	h := NewHandler(fakeSearcher{}, store, tgpresenter.NewTelegramFormatter(4096, "ebusta_test_bot"), edge.NewEngine(p, nil), 5)

	result, err := h.HandleBack(context.Background(), "u1", "tg-back")
	if err != nil {
		t.Fatalf("HandleBack() error = %v", err)
	}
	if result.TargetMessageID != 222 || result.ForceSend {
		t.Fatalf("expected back to edit stored book message, got target=%d force=%v", result.TargetMessageID, result.ForceSend)
	}
}

func putDownloadSession(store session.Store) error {
	return store.Put(context.Background(), &session.Session{
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
}
