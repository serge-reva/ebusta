package transport

import (
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
	editChatID    int64
	editMessageID int
	editText      string
	callbackID    string
	callbackText  string
	err           error
}

func (f *fakeTelegramClient) SendMessage(ctx context.Context, chatID int64, text string, keyboard *tgpresenter.InlineKeyboardMarkup) (int, error) {
	f.sentChatID = chatID
	f.sentText = text
	return 123, f.err
}

func (f *fakeTelegramClient) EditMessage(ctx context.Context, chatID int64, messageID int, text string, keyboard *tgpresenter.InlineKeyboardMarkup) error {
	f.editChatID = chatID
	f.editMessageID = messageID
	f.editText = text
	return f.err
}

func (f *fakeTelegramClient) AnswerCallback(ctx context.Context, callbackID, text string) error {
	f.callbackID = callbackID
	f.callbackText = text
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
	uc := &fakeUsecase{result: &usecase.Result{Text: "page text", TraceID: "tg-4"}}
	client := &fakeTelegramClient{}
	adapter := NewAdapter(client, uc)

	err := adapter.ProcessUpdate(context.Background(), IncomingUpdate{
		TraceID:      "tg-4",
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
		t.Fatalf("current page callback must not edit/send message: edit=%d send=%d", client.editMessageID, client.sentChatID)
	}
}

func TestAdapterProcessUpdatePropagatesClientError(t *testing.T) {
	uc := &fakeUsecase{result: &usecase.Result{Text: "help text", TraceID: "tg-5"}}
	client := &fakeTelegramClient{err: errors.New("send failed")}
	adapter := NewAdapter(client, uc)

	err := adapter.ProcessUpdate(context.Background(), IncomingUpdate{
		TraceID: "tg-5",
		UserID:  "42",
		ChatID:  99,
		Text:    "/help",
	})
	if err == nil || err.Error() != "send failed" {
		t.Fatalf("expected send failure, got %v", err)
	}
}
