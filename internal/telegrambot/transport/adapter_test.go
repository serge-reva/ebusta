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
	rememberUser  string
	rememberMsgID int
	rememberView  string
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

func (f *fakeUsecase) RememberBotMessage(ctx context.Context, userID string, messageID int, view string) error {
	f.rememberUser = userID
	f.rememberMsgID = messageID
	f.rememberView = view
	return f.err
}

type fakeTelegramClient struct {
	sentChatID    int64
	sentText      string
	sentDocument  string
	deletedChatID int64
	deletedMsgID  int
	editChatID    int64
	editMessageID int
	editText      string
	callbackID    string
	callbackText  string
	callOrder     []string
	err           error
	sendErr       error
	editErr       error
}

func (f *fakeTelegramClient) SendMessage(ctx context.Context, chatID int64, text string, keyboard *tgpresenter.InlineKeyboardMarkup) (int, error) {
	f.sentChatID = chatID
	f.sentText = text
	f.callOrder = append(f.callOrder, "sendMessage")
	if f.sendErr != nil {
		return 0, f.sendErr
	}
	return 123, f.err
}

func (f *fakeTelegramClient) EditMessage(ctx context.Context, chatID int64, messageID int, text string, keyboard *tgpresenter.InlineKeyboardMarkup) error {
	f.editChatID = chatID
	f.editMessageID = messageID
	f.editText = text
	f.callOrder = append(f.callOrder, "editMessage")
	if f.editErr != nil {
		return f.editErr
	}
	return f.err
}

func (f *fakeTelegramClient) DeleteMessage(ctx context.Context, chatID int64, messageID int) error {
	f.deletedChatID = chatID
	f.deletedMsgID = messageID
	f.callOrder = append(f.callOrder, "deleteMessage")
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
	uc := &fakeUsecase{result: &usecase.Result{Text: "help text", TraceID: "tg-1", ForceSend: true, StoreAsView: "list"}}
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
	if uc.rememberUser != "42" || uc.rememberMsgID != 123 || uc.rememberView != "list" {
		t.Fatalf("unexpected remember call: user=%q msg=%d view=%q", uc.rememberUser, uc.rememberMsgID, uc.rememberView)
	}
}

func TestAdapterProcessMessageRoutesSearch(t *testing.T) {
	uc := &fakeUsecase{result: &usecase.Result{Text: "search text", TraceID: "tg-2", ForceSend: true, StoreAsView: "list"}}
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
	uc := &fakeUsecase{result: &usecase.Result{Text: "details", TraceID: "tg-2b", TargetMessageID: 555, StoreAsView: "book"}}
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
	if client.editMessageID != 555 || client.editText != "details" {
		t.Fatalf("expected select book to edit stored message, got edit=%d text=%q", client.editMessageID, client.editText)
	}
	if uc.rememberMsgID != 555 || uc.rememberView != "book" {
		t.Fatalf("unexpected remember call after select: msg=%d view=%q", uc.rememberMsgID, uc.rememberView)
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
	if client.sentText != "" || client.editMessageID != 0 {
		t.Fatalf("download callback must not send extra text or edit message: text=%q edit=%d", client.sentText, client.editMessageID)
	}
	if len(client.callOrder) < 2 || client.callOrder[0] != "answerCallback" || client.callOrder[1] != "sendDocument" {
		t.Fatalf("unexpected callback/document order: %v", client.callOrder)
	}
}

func TestAdapterProcessCallbackEditsMessage(t *testing.T) {
	uc := &fakeUsecase{result: &usecase.Result{Text: "page text", TraceID: "tg-3", StoreAsView: "list"}}
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
	if uc.rememberMsgID != 11 || uc.rememberView != "list" {
		t.Fatalf("unexpected remember after edit: msg=%d view=%q", uc.rememberMsgID, uc.rememberView)
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
	uc := &fakeUsecase{result: &usecase.Result{Text: "help text", TraceID: "tg-4", ForceSend: true}}
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

func TestAdapterFallsBackToSendWhenEditTargetMissing(t *testing.T) {
	uc := &fakeUsecase{result: &usecase.Result{Text: "details", TraceID: "tg-fallback", TargetMessageID: 777, StoreAsView: "book"}}
	client := &fakeTelegramClient{editErr: errors.New("Bad Request: message to edit not found")}
	adapter := NewAdapter(client, uc)

	err := adapter.ProcessUpdate(context.Background(), IncomingUpdate{
		TraceID: "tg-fallback",
		UserID:  "42",
		ChatID:  99,
		Text:    "/start book_7",
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if client.sentText != "details" {
		t.Fatalf("expected fallback send, got text=%q order=%v", client.sentText, client.callOrder)
	}
	if uc.rememberMsgID != 123 || uc.rememberView != "book" {
		t.Fatalf("unexpected remember after fallback send: msg=%d view=%q", uc.rememberMsgID, uc.rememberView)
	}
}

func TestAdapterDeleteAndSendModeDeletesThenSends(t *testing.T) {
	uc := &fakeUsecase{result: &usecase.Result{
		Text:            "page text",
		TraceID:         "tg-delete-send",
		TargetMessageID: 777,
		StoreAsView:     "list",
		DeleteAndSend:   true,
	}}
	client := &fakeTelegramClient{}
	adapter := NewAdapter(client, uc)

	err := adapter.ProcessUpdate(context.Background(), IncomingUpdate{
		TraceID:      "tg-delete-send",
		UserID:       "42",
		ChatID:       99,
		MessageID:    11,
		CallbackID:   "cb-1",
		CallbackData: "page:next",
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if client.deletedChatID != 99 || client.deletedMsgID != 777 {
		t.Fatalf("expected delete of target message, got chat=%d msg=%d", client.deletedChatID, client.deletedMsgID)
	}
	if client.sentText != "page text" {
		t.Fatalf("expected new send after delete, got %q", client.sentText)
	}
	if got := client.callOrder; len(got) < 3 || got[0] != "answerCallback" || got[1] != "deleteMessage" || got[2] != "sendMessage" {
		t.Fatalf("unexpected operation order: %v", got)
	}
	if uc.rememberMsgID != 123 || uc.rememberView != "list" {
		t.Fatalf("unexpected remember after delete-and-send: msg=%d view=%q", uc.rememberMsgID, uc.rememberView)
	}
}
