package transport

import (
	"bytes"
	"context"
	"encoding/json"
	"net/http"
	"sync"

	"ebusta/internal/errutil"
	"ebusta/internal/logger"
	tgpresenter "ebusta/internal/telegrambot/presenter"

	"github.com/go-telegram/bot/models"
)

type MockOperation struct {
	Kind       string                            `json:"kind"`
	ChatID     int64                             `json:"chat_id,omitempty"`
	MessageID  int                               `json:"message_id,omitempty"`
	CallbackID string                            `json:"callback_id,omitempty"`
	Text       string                            `json:"text,omitempty"`
	Keyboard   *tgpresenter.InlineKeyboardMarkup `json:"keyboard,omitempty"`
}

type MockTelegramClient struct {
	mu        sync.Mutex
	ops       []MockOperation
	nextMsgID int
	log       *logger.Logger
}

func NewMockTelegramClient(log *logger.Logger) *MockTelegramClient {
	if log == nil {
		log = logger.GetGlobal()
	}
	return &MockTelegramClient{nextMsgID: 1, log: log.WithComponent("telegram-bot-mock")}
}

func (c *MockTelegramClient) SendMessage(ctx context.Context, chatID int64, text string, keyboard *tgpresenter.InlineKeyboardMarkup) (int, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	id := c.nextMsgID
	c.nextMsgID++
	op := MockOperation{Kind: "send", ChatID: chatID, MessageID: id, Text: text, Keyboard: keyboard}
	c.ops = append(c.ops, op)
	c.log.WithFields(map[string]interface{}{"chat_id": chatID, "message_id": id}).Info(errutil.TraceIDFromContext(ctx), text)
	return id, nil
}

func (c *MockTelegramClient) EditMessage(ctx context.Context, chatID int64, messageID int, text string, keyboard *tgpresenter.InlineKeyboardMarkup) error {
	c.mu.Lock()
	defer c.mu.Unlock()
	op := MockOperation{Kind: "edit", ChatID: chatID, MessageID: messageID, Text: text, Keyboard: keyboard}
	c.ops = append(c.ops, op)
	c.log.WithFields(map[string]interface{}{"chat_id": chatID, "message_id": messageID}).Info(errutil.TraceIDFromContext(ctx), text)
	return nil
}

func (c *MockTelegramClient) DeleteMessage(ctx context.Context, chatID int64, messageID int) error {
	c.mu.Lock()
	defer c.mu.Unlock()
	op := MockOperation{Kind: "delete", ChatID: chatID, MessageID: messageID}
	c.ops = append(c.ops, op)
	c.log.WithFields(map[string]interface{}{"chat_id": chatID, "message_id": messageID}).Info(errutil.TraceIDFromContext(ctx), "message deleted")
	return nil
}

func (c *MockTelegramClient) SendDocument(ctx context.Context, chatID int64, filename string, data *bytes.Reader, caption string) (int, error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	id := c.nextMsgID
	c.nextMsgID++
	op := MockOperation{Kind: "document", ChatID: chatID, MessageID: id, Text: filename}
	c.ops = append(c.ops, op)
	c.log.WithFields(map[string]interface{}{"chat_id": chatID, "message_id": id, "filename": filename, "caption": caption}).Info(errutil.TraceIDFromContext(ctx), "document sent")
	return id, nil
}

func (c *MockTelegramClient) AnswerCallback(ctx context.Context, callbackID, text string) error {
	c.mu.Lock()
	defer c.mu.Unlock()
	op := MockOperation{Kind: "callback", CallbackID: callbackID, Text: text}
	c.ops = append(c.ops, op)
	c.log.WithField("callback_id", callbackID).Info(errutil.TraceIDFromContext(ctx), "callback answered")
	return nil
}

func (c *MockTelegramClient) Snapshot() []MockOperation {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]MockOperation, len(c.ops))
	copy(out, c.ops)
	return out
}

type MockWebhookServer struct {
	adapter *Adapter
	client  *MockTelegramClient
	secret  string
}

func NewMockWebhookServer(adapter *Adapter, client *MockTelegramClient, secret string) *MockWebhookServer {
	return &MockWebhookServer{adapter: adapter, client: client, secret: secret}
}

func (s *MockWebhookServer) Handler() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/health", func(w http.ResponseWriter, _ *http.Request) {
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write([]byte("ok"))
	})
	mux.HandleFunc("/webhook", s.handleWebhook)
	mux.HandleFunc("/_mock/messages", s.handleMessages)
	return mux
}

func (s *MockWebhookServer) handleWebhook(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "method not allowed", http.StatusMethodNotAllowed)
		return
	}
	if s.secret != "" && r.Header.Get("X-Telegram-Bot-Api-Secret-Token") != s.secret {
		http.Error(w, "forbidden", http.StatusForbidden)
		return
	}
	var update models.Update
	if err := json.NewDecoder(r.Body).Decode(&update); err != nil {
		http.Error(w, "bad request", http.StatusBadRequest)
		return
	}
	traceID := errutil.GenerateTraceID("tg")
	ctx := context.WithValue(r.Context(), struct{}{}, traceID)
	mapped, ok := MapUpdate(&update, traceID)
	if !ok {
		w.WriteHeader(http.StatusOK)
		return
	}
	if err := s.adapter.ProcessUpdate(ctx, mapped); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.WriteHeader(http.StatusOK)
	_, _ = w.Write([]byte(`{"ok":true}`))
}

func (s *MockWebhookServer) handleMessages(w http.ResponseWriter, _ *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	_ = json.NewEncoder(w).Encode(map[string]interface{}{"operations": s.client.Snapshot()})
}
