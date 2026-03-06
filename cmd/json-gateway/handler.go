package main

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strconv"
	"strings"

	"ebusta/internal/edge"
	"ebusta/internal/errutil"
	"ebusta/internal/gatewayclient"
	"ebusta/internal/logger"
	"ebusta/internal/presenter"
)

type UpdateRequest struct {
	UserID  string `json:"user_id"`
	Message string `json:"message"`
}

type UpdateResponse struct {
	OK      bool                `json:"ok"`
	TraceID string              `json:"trace_id,omitempty"`
	Message string              `json:"message,omitempty"`
	Books   []presenter.BookDTO `json:"books,omitempty"`
	Total   int                 `json:"total,omitempty"`
	Page    int                 `json:"page,omitempty"`
	Pages   int                 `json:"pages,omitempty"`
}

type TGHandler struct {
	pageSize      int
	gatewayClient *gatewayclient.Client
	engine        *edge.Engine
}

func NewTGHandler(gatewayURL string, pageSize int, engine *edge.Engine) *TGHandler {
	if engine == nil {
		p := edge.DefaultPolicy("telegram")
		p.Actions["command"] = edge.ActionPolicy{PerMinute: 30, Burst: 30}
		engine = edge.NewEngine(p, edge.NewLabelCounterHook())
	}
	return &TGHandler{
		pageSize:      pageSize,
		gatewayClient: gatewayclient.NewClient(gatewayURL),
		engine:        engine,
	}
}

func (h *TGHandler) handleHealth(w http.ResponseWriter, r *http.Request) {
	traceID := errutil.TraceIDFromRequest(r)
	if traceID == "" {
		traceID = errutil.GenerateTraceID("jg")
	}
	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("X-Trace-Id", traceID)
	w.WriteHeader(http.StatusOK)
	_ = json.NewEncoder(w).Encode(map[string]string{"status": "ok"})
}

func (h *TGHandler) handleUpdate(w http.ResponseWriter, r *http.Request) {
	traceID := errutil.TraceIDFromRequest(r)
	if traceID == "" {
		traceID = errutil.GenerateTraceID("tg")
	}

	body, err := io.ReadAll(r.Body)
	if err != nil {
		errutil.WriteJSONErrorSimple(w, http.StatusBadRequest, errutil.CodeInvalidArgument, "invalid body", traceID)
		return
	}
	if err := h.engine.ValidateJSON(r.Context(), "update", body); err != nil {
		errutil.WriteJSONErrorSimple(w, http.StatusBadRequest, errutil.CodeInvalidArgument, err.Error(), traceID)
		return
	}

	var req UpdateRequest
	if err := json.Unmarshal(body, &req); err != nil {
		errutil.WriteJSONErrorSimple(w, http.StatusBadRequest, errutil.CodeInvalidArgument, "invalid json", traceID)
		return
	}
	req.Message = strings.TrimSpace(req.Message)
	req.UserID = strings.TrimSpace(req.UserID)
	if req.UserID == "" {
		errutil.WriteJSONErrorSimple(w, http.StatusBadRequest, errutil.CodeInvalidArgument, "missing user_id", traceID)
		return
	}

	if err := h.engine.ValidateLine(r.Context(), "command", req.Message); err != nil {
		errutil.WriteJSONErrorSimple(w, http.StatusBadRequest, errutil.CodeInvalidArgument, err.Error(), traceID)
		return
	}
	if !h.engine.Allow(r.Context(), "command", req.UserID) {
		errutil.WriteJSONErrorSimple(w, http.StatusTooManyRequests, errutil.CodeUnavailable, "rate limit exceeded", traceID)
		return
	}

	query, page, perr := parseTGSearch(req.Message)
	if perr != nil {
		resp := UpdateResponse{OK: true, TraceID: traceID, Message: "Usage: /search <query> [page <n>]"}
		writeTGResponse(w, resp)
		return
	}

	resp, err := h.gatewayClient.Search(r.Context(), query, page, h.pageSize, traceID)
	if err != nil {
		logger.GetGlobal().WithField("trace_id", traceID).Error(traceID, "json gateway request failed", err)
		if appErr, ok := err.(*errutil.AppError); ok {
			errutil.WriteJSONError(w, appErr)
			return
		}
		errutil.WriteJSONErrorSimple(w, http.StatusBadGateway, errutil.CodeBadGateway, "gateway unavailable", traceID)
		return
	}

	writeTGResponse(w, UpdateResponse{
		OK:      true,
		TraceID: resp.TraceID,
		Total:   resp.Total,
		Books:   toPresenterBooks(resp.Books),
		Page:    resp.Page,
		Pages:   resp.Pages,
	})
}

func writeTGResponse(w http.ResponseWriter, resp UpdateResponse) {
	w.Header().Set("Content-Type", "application/json")
	if resp.TraceID != "" {
		w.Header().Set("X-Trace-Id", resp.TraceID)
	}
	w.WriteHeader(http.StatusOK)
	_ = json.NewEncoder(w).Encode(resp)
}

func toPresenterBooks(books []gatewayclient.SearchBook) []presenter.BookDTO {
	result := make([]presenter.BookDTO, 0, len(books))
	for _, book := range books {
		result = append(result, presenter.BookDTO{
			ID:          book.ID,
			Title:       book.Title,
			Authors:     book.Authors,
			Container:   book.Container,
			Filename:    book.Filename,
			FullAuthors: book.FullAuthors,
			DownloadURL: book.DownloadURL,
		})
	}
	return result
}

func parseTGSearch(msg string) (query string, page int, err error) {
	parts := strings.Fields(strings.TrimSpace(msg))
	if len(parts) < 2 || parts[0] != "/search" {
		return "", 1, fmt.Errorf("invalid command")
	}
	page = 1
	query = strings.Join(parts[1:], " ")
	if len(parts) >= 4 && strings.EqualFold(parts[len(parts)-2], "page") {
		p, convErr := strconv.Atoi(parts[len(parts)-1])
		if convErr != nil || p <= 0 {
			return "", 1, fmt.Errorf("invalid page")
		}
		page = p
		query = strings.Join(parts[1:len(parts)-2], " ")
	}
	query = strings.TrimSpace(query)
	if query == "" {
		return "", 1, fmt.Errorf("empty query")
	}
	return query, page, nil
}
