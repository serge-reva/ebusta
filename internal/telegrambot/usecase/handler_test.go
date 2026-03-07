package usecase

import (
	"context"
	"strings"
	"testing"

	"ebusta/internal/edge"
	"ebusta/internal/errutil"
	"ebusta/internal/gatewayclient"
	corepresenter "ebusta/internal/presenter"
	tgpresenter "ebusta/internal/telegrambot/presenter"
	"ebusta/internal/telegrambot/session"
)

type fakeSearcher struct {
	resp *gatewayclient.SearchResponse
	err  error
	meta *gatewayclient.FileMeta
	body []byte
}

func (f fakeSearcher) Search(_ context.Context, _ string, _ int, _ int, _ string) (*gatewayclient.SearchResponse, error) {
	return f.resp, f.err
}

func (f fakeSearcher) GetMeta(_ context.Context, _ string, _ string) (*gatewayclient.FileMeta, error) {
	if f.err != nil {
		return nil, f.err
	}
	if f.meta != nil {
		return f.meta, nil
	}
	return &gatewayclient.FileMeta{}, nil
}

func (f fakeSearcher) DownloadBook(_ context.Context, _ string, _ string) ([]byte, *gatewayclient.FileMeta, error) {
	if f.err != nil {
		return nil, nil, f.err
	}
	return f.body, f.meta, nil
}

type fakeFormatter struct{}

func (fakeFormatter) FormatSearchResult(result *corepresenter.PresenterResult, _ int) (string, *tgpresenter.InlineKeyboardMarkup, error) {
	return result.Books[0].Title, &tgpresenter.InlineKeyboardMarkup{}, nil
}
func (fakeFormatter) FormatBookDetails(book *corepresenter.BookDTO) (string, *tgpresenter.InlineKeyboardMarkup) {
	return book.Title, &tgpresenter.InlineKeyboardMarkup{}
}
func (fakeFormatter) FormatHelp() string { return "help" }
func (fakeFormatter) FormatError(message, traceID string) string {
	if traceID == "" {
		return message
	}
	return message + " (" + traceID + ")"
}

func testHandler(searcher SearchService, store session.Store) *Handler {
	p := edge.DefaultPolicy("telegram")
	p.Actions["command"] = edge.ActionPolicy{PerMinute: 30, Burst: 30}
	return NewHandler(searcher, store, fakeFormatter{}, edge.NewEngine(p, nil), 5)
}

func TestHandleSearchSuccess(t *testing.T) {
	h := testHandler(fakeSearcher{
		resp: &gatewayclient.SearchResponse{
			TraceID: "tg-1",
			Total:   1,
			Books: []gatewayclient.SearchBook{
				{ID: "1", Title: "Book", FullAuthors: "Author"},
			},
			Page:  1,
			Pages: 1,
		},
	}, session.NewMemoryStore())

	result, err := h.HandleSearch(context.Background(), "u1", "/search book", "tg-1")
	if err != nil {
		t.Fatalf("HandleSearch() error = %v", err)
	}
	if result.Text != "Book" || result.TraceID != "tg-1" {
		t.Fatalf("unexpected result: %+v", result)
	}
	if !result.ForceSend || result.StoreAsView != "list" {
		t.Fatalf("expected new search to send and store list view, got %+v", result)
	}
}

func TestHandleSearchGatewayError(t *testing.T) {
	h := testHandler(fakeSearcher{
		err: errutil.New(errutil.CodeUnavailable, "gateway unavailable").WithTrace("tg-2"),
	}, session.NewMemoryStore())

	result, err := h.HandleSearch(context.Background(), "u1", "/search book", "tg-2")
	if err != nil {
		t.Fatalf("HandleSearch() error = %v", err)
	}
	if result.TraceID != "tg-2" || result.Text == "" {
		t.Fatalf("unexpected result: %+v", result)
	}
}

func TestHandlePageWithoutSession(t *testing.T) {
	h := testHandler(fakeSearcher{}, session.NewMemoryStore())
	result, err := h.HandlePage(context.Background(), "u1", 2, "tg-3")
	if err != nil {
		t.Fatalf("HandlePage() error = %v", err)
	}
	if result.Text == "" {
		t.Fatal("expected user-facing error text")
	}
}

func TestHandleCallbackNext(t *testing.T) {
	store := session.NewMemoryStore()
	_ = store.Put(context.Background(), &session.Session{
		UserID:            "u1",
		Query:             "book",
		PageSize:          5,
		CurrentPage:       1,
		LastListMessageID: 444,
	})
	h := testHandler(fakeSearcher{
		resp: &gatewayclient.SearchResponse{
			TraceID: "tg-4",
			Total:   1,
			Books: []gatewayclient.SearchBook{
				{ID: "1", Title: "Book", FullAuthors: "Author"},
			},
			Page:  2,
			Pages: 3,
		},
	}, store)

	result, err := h.HandleCallback(context.Background(), "u1", "page:next", "tg-4")
	if err != nil {
		t.Fatalf("HandleCallback() error = %v", err)
	}
	if result.TraceID != "tg-4" {
		t.Fatalf("unexpected result: %+v", result)
	}
	if result.TargetMessageID != 444 || result.StoreAsView != "list" {
		t.Fatalf("expected callback pagination to target stored list message, got %+v", result)
	}
}

func TestHandleCallbackCurrentPageIsNoop(t *testing.T) {
	h := testHandler(fakeSearcher{}, session.NewMemoryStore())
	result, err := h.HandleCallback(context.Background(), "u1", "page:current", "tg-current")
	if err != nil {
		t.Fatalf("HandleCallback() error = %v", err)
	}
	if result != nil {
		t.Fatalf("expected nil result for current page callback, got %+v", result)
	}
}

func TestHandleHelp(t *testing.T) {
	h := testHandler(fakeSearcher{}, session.NewMemoryStore())
	result := h.HandleHelp("tg-help")
	if result.Text != "help" || result.TraceID != "tg-help" {
		t.Fatalf("unexpected result: %+v", result)
	}
}

func TestHandleSearchFullCycleFormatsResult(t *testing.T) {
	store := session.NewMemoryStore()
	formatter := tgpresenter.NewTelegramFormatter(4096, "ebusta_test_bot")
	p := edge.DefaultPolicy("telegram")
	p.Actions["command"] = edge.ActionPolicy{PerMinute: 30, Burst: 30}
	h := NewHandler(fakeSearcher{
		resp: &gatewayclient.SearchResponse{
			TraceID: "tg-full",
			Total:   6,
			Books: []gatewayclient.SearchBook{{
				ID:          "1",
				Title:       "Dune",
				FullAuthors: "Frank Herbert",
			}},
			Page:  1,
			Pages: 2,
		},
	}, store, formatter, edge.NewEngine(p, nil), 5)

	result, err := h.HandleSearch(context.Background(), "u1", "/search dune", "tg-full")
	if err != nil {
		t.Fatalf("HandleSearch() error = %v", err)
	}
	if result.TraceID != "tg-full" {
		t.Fatalf("unexpected trace id: %q", result.TraceID)
	}
	if result.Keyboard == nil || len(result.Keyboard.InlineKeyboard) == 0 {
		t.Fatal("expected pagination keyboard")
	}
	if got := result.Text; got == "" || !strings.Contains(got, "Dune") || !strings.Contains(got, "Frank Herbert") {
		t.Fatalf("unexpected formatted text: %q", got)
	}
}
