package transport

import (
	"context"
	"strings"
	"testing"

	"ebusta/internal/edge"
	"ebusta/internal/gatewayclient"
	tgpresenter "ebusta/internal/telegrambot/presenter"
	"ebusta/internal/telegrambot/session"
	"ebusta/internal/telegrambot/usecase"
)

type integrationSearcher struct{}

func (integrationSearcher) Search(_ context.Context, query string, page, limit int, traceID string) (*gatewayclient.SearchResponse, error) {
	return &gatewayclient.SearchResponse{
		TraceID: traceID,
		Total:   6,
		Page:    page,
		Pages:   2,
		Books: []gatewayclient.SearchBook{{
			ID:          "1",
			Title:       "Dune",
			FullAuthors: "Frank Herbert",
		}},
	}, nil
}

func TestAdapterProcessUpdateEndToEndSearch(t *testing.T) {
	formatter := tgpresenter.NewTelegramFormatter(4096)
	policy := edge.DefaultPolicy("telegram")
	policy.Actions["command"] = edge.ActionPolicy{PerMinute: 30, Burst: 30}
	handler := usecase.NewHandler(integrationSearcher{}, session.NewMemoryStore(), formatter, edge.NewEngine(policy, nil), 5)
	client := &fakeTelegramClient{}
	adapter := NewAdapter(client, handler)

	err := adapter.ProcessUpdate(context.Background(), IncomingUpdate{
		TraceID: "tg-int-1",
		UserID:  "42",
		ChatID:  99,
		Text:    "/search dune",
	})
	if err != nil {
		t.Fatalf("ProcessUpdate() error = %v", err)
	}
	if client.sentText == "" || !strings.Contains(client.sentText, "Dune") || !strings.Contains(client.sentText, "Frank Herbert") {
		t.Fatalf("unexpected sent text: %q", client.sentText)
	}
}
