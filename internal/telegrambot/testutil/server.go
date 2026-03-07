package testutil

import (
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
)

type Call struct {
	Method string
	Body   []byte
	Path   string
}

type TelegramAPIServer struct {
	server *httptest.Server
	mu     sync.Mutex
	calls  []Call
}

func NewTelegramAPIServer() *TelegramAPIServer {
	ts := &TelegramAPIServer{}
	ts.server = httptest.NewServer(http.HandlerFunc(ts.handle))
	return ts
}

func (s *TelegramAPIServer) URL() string {
	return s.server.URL
}

func (s *TelegramAPIServer) Close() {
	s.server.Close()
}

func (s *TelegramAPIServer) Calls() []Call {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]Call, len(s.calls))
	copy(out, s.calls)
	return out
}

func (s *TelegramAPIServer) HasMethod(method string) bool {
	for _, call := range s.Calls() {
		if call.Method == method {
			return true
		}
	}
	return false
}

func (s *TelegramAPIServer) handle(w http.ResponseWriter, r *http.Request) {
	body, _ := io.ReadAll(r.Body)
	_ = r.Body.Close()
	method := apiMethod(r.URL.Path)
	s.mu.Lock()
	s.calls = append(s.calls, Call{Method: method, Body: body, Path: r.URL.Path})
	s.mu.Unlock()

	w.Header().Set("Content-Type", "application/json")
	switch method {
	case "getMe":
		_ = json.NewEncoder(w).Encode(map[string]any{
			"ok":     true,
			"result": map[string]any{"id": 1, "is_bot": true, "first_name": "test", "username": "ebusta_test_bot"},
		})
	case "sendMessage":
		_ = json.NewEncoder(w).Encode(map[string]any{
			"ok":     true,
			"result": map[string]any{"message_id": 101},
		})
	case "editMessageText":
		_ = json.NewEncoder(w).Encode(map[string]any{
			"ok":     true,
			"result": map[string]any{"message_id": 101},
		})
	case "answerCallbackQuery", "setWebhook", "deleteWebhook":
		_ = json.NewEncoder(w).Encode(map[string]any{"ok": true, "result": true})
	default:
		w.WriteHeader(http.StatusNotFound)
		_ = json.NewEncoder(w).Encode(map[string]any{"ok": false, "description": "unknown method"})
	}
}

func apiMethod(path string) string {
	idx := strings.LastIndex(path, "/")
	if idx == -1 || idx == len(path)-1 {
		return path
	}
	return path[idx+1:]
}

func (s *TelegramAPIServer) LastCall(method string) (Call, bool) {
	calls := s.Calls()
	for i := len(calls) - 1; i >= 0; i-- {
		if calls[i].Method == method {
			return calls[i], true
		}
	}
	return Call{}, false
}
