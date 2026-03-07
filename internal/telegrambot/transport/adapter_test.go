package transport

import (
	"bytes"
	"context"
	"errors"
	"testing"

	tgpresenter "ebusta/internal/telegrambot/presenter"
	"ebusta/internal/telegrambot/usecase"
)

type fakeUsecase struct {
	helpCalls     int
	searchInput   string
	searchTraceID string
	pageInput     int
	selectIndex   int
	callbackInput string
	result        *usecase.Result
	err           error
}

func (f *fakeUsecase) HandleSearch(ctx context.Context, userID, input, traceID string) (*usecase.Result, error) {
	f.searchInput = input
	f.searchTraceID = traceID
	return f.result, f.err
}

func (f *fakeUsecase) HandlePage(ctx context.Context, userID string, page int, traceID string) (*usecase.Result, error) {
	f.pageInput = page
	return f.result, f.err
}

func (f *fakeUsecase) HandleSelectBook(ctx context.Context, userID string, bookIndex int, traceID string) (*usecase.Result, error) {
	f.selectIndex = bookIndex
	return f.result, f.err
}

func (f *fakeUsecase) HandleHelp(traceID string) *usecase.Result {
	f.helpCalls++
	return f.result
}

func (f *fakeUsecase) HandleCallback(ctx context.Context, userID, callbackData, traceID string) (*usecase.Result, error) {
	f.callbackInput = callbackData
	return f.result, f.err
}

type fakeTelegramClient struct {
	sentChatID    int64
	sentText      string
	sentDocument  string
	editChatID    int64
	editMessageID int
	editText      string
	callbackID    string
	callbackText  string
	callOrder     []string
	err           error
}

func (f *fakeTelegramClient) SendMessage(ctx context.Context, chatID int64, text string, keyboard *tgpresenter.InlineKeyboardMarkup) (int, error) {
	f.sentChatID = chatID
	f.sentText = text
	f.callOrder = append(f.callOrder, "sendMessage")
	return 123, f.err
}

func (f *fakeTelegramClient) EditMessage(ctx context.Context, chatID int64, messageID int, text string, keyboard *tgpresenter.InlineKeyboardMarkup) error {
	f.editChatID = chatID
	f.editMessageID = messageID
	f.editText = text
	f.callOrder = append(f.callOrder, "editMessage")
	return f.err
}

func (f *fakeTelegramClient) SendDocument(ctx context.Context, chatID int64, filename string, data *bytes.Reader, caption string) (int, error) {
	f.sentChatID = chatID
	f.sentDocument = filename
	f.callOrder = append(f.callOrder, "sendDocument")
	return 124, f.err
}

func (f *fakeTelegramClient) AnswerCallback(ctx context.Context, callbackID, text string) error {
	f.callbackID = callbackID
	f.callbackText = text
	f.callOrder = append(f.callOrder, "answerCallback")
	return f.err
}

func TestAdapterProcessMessageRoutesHelp(t *testing.T) {
	uc := &fakeUsecase{result: &usecase.Result{Text: "help text", TraceID: "tg-1"}}
	client := &fakeTelegramClient{}
	adapter := NewAdapter(client, uc)

	err := adapter.ProcessUpdate(context.Background(), IncomingUpdate{
		TraceID: "tg-1",
		UserID:  "42",
		ChatID:  99,
		Text:    "/help",
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if uc.helpCalls != 1 {
		t.Fatalf("expected help to be called once, got %d", uc.helpCalls)
	}
	if client.sentChatID != 99 || client.sentText != "help text" {
		t.Fatalf("unexpected send payload: chat=%d text=%q", client.sentChatID, client.sentText)
	}
}

func TestAdapterProcessMessageRoutesSearch(t *testing.T) {
	uc := &fakeUsecase{result: &usecase.Result{Text: "search text", TraceID: "tg-2"}}
	client := &fakeTelegramClient{}
	adapter := NewAdapter(client, uc)

	err := adapter.ProcessUpdate(context.Background(), IncomingUpdate{
		TraceID: "tg-2",
		UserID:  "42",
		ChatID:  99,
		Text:    "/search dune",
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if uc.searchInput != "/search dune" {
		t.Fatalf("search input mismatch: %q", uc.searchInput)
	}
	if uc.searchTraceID != "tg-2" {
		t.Fatalf("trace mismatch: %q", uc.searchTraceID)
	}
}

func TestAdapterProcessMessageRoutesSelectBook(t *testing.T) {
	uc := &fakeUsecase{result: &usecase.Result{Text: "details", TraceID: "tg-2b"}}
	client := &fakeTelegramClient{}
	adapter := NewAdapter(client, uc)

	err := adapter.ProcessUpdate(context.Background(), IncomingUpdate{
		TraceID: "tg-2b",
		UserID:  "42",
		ChatID:  99,
		Text:    "/start book_7",
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if uc.selectIndex != 7 {
		t.Fatalf("select index mismatch: %d", uc.selectIndex)
	}
}

func TestAdapterRespondSendsDocumentWhenPresent(t *testing.T) {
	uc := &fakeUsecase{result: &usecase.Result{
		Document: &usecase.Document{Filename: "book.fb2", Data: bytes.NewReader([]byte("abc"))},
		TraceID:  "tg-doc",
	}}
	client := &fakeTelegramClient{}
	adapter := NewAdapter(client, uc)

	err := adapter.ProcessUpdate(context.Background(), IncomingUpdate{
		TraceID:      "tg-doc",
		UserID:       "42",
		ChatID:       99,
		MessageID:    11,
		CallbackID:   "cb-1",
		CallbackData: "download:sha1-1",
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if client.sentDocument != "book.fb2" {
		t.Fatalf("expected document send, got %q", client.sentDocument)
	}
	if len(client.callOrder) < 2 || client.callOrder[0] != "answerCallback" || client.callOrder[1] != "sendDocument" {
		t.Fatalf("unexpected callback/document order: %v", client.callOrder)
	}
}

func TestAdapterProcessCallbackEditsMessage(t *testing.T) {
	uc := &fakeUsecase{result: &usecase.Result{Text: "page text", TraceID: "tg-3"}}
	client := &fakeTelegramClient{}
	adapter := NewAdapter(client, uc)

	err := adapter.ProcessUpdate(context.Background(), IncomingUpdate{
		TraceID:      "tg-3",
		UserID:       "42",
		ChatID:       99,
		MessageID:    11,
		CallbackID:   "cb-1",
		CallbackData: "page:next",
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if uc.callbackInput != "page:next" {
		t.Fatalf("callback mismatch: %q", uc.callbackInput)
	}
	if client.callbackID != "cb-1" {
		t.Fatalf("callback answer mismatch: %q", client.callbackID)
	}
	if client.editChatID != 99 || client.editMessageID != 11 || client.editText != "page text" {
		t.Fatalf("unexpected edit payload: chat=%d msg=%d text=%q", client.editChatID, client.editMessageID, client.editText)
	}
}

func TestAdapterProcessCurrentPageCallbackIsNoop(t *testing.T) {
	uc := &fakeUsecase{result: nil}
	client := &fakeTelegramClient{}
	adapter := NewAdapter(client, uc)

	err := adapter.ProcessUpdate(context.Background(), IncomingUpdate{
		TraceID:      "tg-current",
		UserID:       "42",
		ChatID:       99,
		MessageID:    11,
		CallbackID:   "cb-current",
		CallbackData: "page:current",
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if client.callbackID != "cb-current" {
		t.Fatalf("expected callback answer, got %q", client.callbackID)
	}
	if client.editMessageID != 0 || client.sentChatID != 0 {
		t.Fatalf("current callback must not send/edit: edit=%d sendChat=%d order=%v", client.editMessageID, client.sentChatID, client.callOrder)
	}
}

func TestAdapterProcessUpdatePropagatesClientError(t *testing.T) {
	uc := &fakeUsecase{result: &usecase.Result{Text: "help text", TraceID: "tg-4"}}
	client := &fakeTelegramClient{err: errors.New("send failed")}
	adapter := NewAdapter(client, uc)

	err := adapter.ProcessUpdate(context.Background(), IncomingUpdate{
		TraceID: "tg-4",
		UserID:  "42",
		ChatID:  99,
		Text:    "/help",
	})
	if err == nil || err.Error() != "send failed" {
		t.Fatalf("expected send failure, got %v", err)
	}
}
