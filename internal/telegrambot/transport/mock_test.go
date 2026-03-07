package transport

import (
	"bytes"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"

	"ebusta/internal/logger"
	"ebusta/internal/telegrambot/usecase"
)

func TestMockWebhookServerProcessesUpdateAndExposesMessages(t *testing.T) {
	uc := &fakeUsecase{result: &usecase.Result{Text: "help text", TraceID: "tg-mock-1"}}
	client := NewMockTelegramClient(logger.GetGlobal())
	adapter := NewAdapter(client, uc)
	server := httptest.NewServer(NewMockWebhookServer(adapter, client, "secret").Handler())
	defer server.Close()

	payload := map[string]any{
		"update_id": 1,
		"message": map[string]any{
			"message_id": 10,
			"from":       map[string]any{"id": 42, "is_bot": false, "first_name": "Test"},
			"chat":       map[string]any{"id": 42, "type": "private"},
			"date":       1700000000,
			"text":       "/help",
		},
	}
	body, _ := json.Marshal(payload)
	req, _ := http.NewRequest(http.MethodPost, server.URL+"/webhook", bytes.NewReader(body))
	req.Header.Set("X-Telegram-Bot-Api-Secret-Token", "secret")
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatalf("webhook request failed: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("unexpected status: %d", resp.StatusCode)
	}

	resp, err = http.Get(server.URL + "/_mock/messages")
	if err != nil {
		t.Fatalf("messages request failed: %v", err)
	}
	defer resp.Body.Close()
	data, _ := io.ReadAll(resp.Body)
	if !bytes.Contains(data, []byte("help text")) {
		t.Fatalf("expected help text in mock messages, got %s", string(data))
	}
}
