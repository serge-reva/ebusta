package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strconv"
	"strings"
	"time"

	"ebusta/internal/edge"
	"ebusta/internal/errutil"
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
	gatewayURL string
	pageSize   int
	httpClient *http.Client
	engine     *edge.Engine
}

func NewTGHandler(gatewayURL string, pageSize int, engine *edge.Engine) *TGHandler {
	if engine == nil {
		p := edge.DefaultPolicy("telegram")
		p.Actions["command"] = edge.ActionPolicy{PerMinute: 30, Burst: 30}
		engine = edge.NewEngine(p, edge.NewLabelCounterHook())
	}
	return &TGHandler{
		gatewayURL: gatewayURL,
		pageSize:   pageSize,
		httpClient: &http.Client{Timeout: 10 * time.Second},
		engine:     engine,
	}
}

func (h *TGHandler) handleHealth(w http.ResponseWriter, _ *http.Request) {
	w.Header().Set("Content-Type", "application/json")
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

	gwReq := map[string]interface{}{
		"query": query,
		"page":  page,
		"limit": h.pageSize,
	}
	j, _ := json.Marshal(gwReq)
	httpReq, _ := http.NewRequest(http.MethodPost, h.gatewayURL+"/search", bytes.NewBuffer(j))
	httpReq.Header.Set("Content-Type", "application/json")
	httpReq.Header.Set("X-Trace-Id", traceID)

	resp, err := h.httpClient.Do(httpReq)
	if err != nil {
		logger.GetGlobal().WithField("trace_id", traceID).Error(traceID, "telegram gateway request failed", err)
		errutil.WriteJSONErrorSimple(w, http.StatusBadGateway, errutil.CodeBadGateway, "gateway unavailable", traceID)
		return
	}
	defer resp.Body.Close()

	respBody, appErr := errutil.ReadBodyAndError(resp, traceID)
	if appErr != nil {
		errutil.WriteJSONError(w, appErr)
		return
	}

	var sr struct {
		TraceID string              `json:"trace_id"`
		Total   int                 `json:"total"`
		Books   []presenter.BookDTO `json:"books"`
		Page    int                 `json:"page"`
		Pages   int                 `json:"pages"`
	}
	if err := json.Unmarshal(respBody, &sr); err != nil {
		errutil.WriteJSONErrorSimple(w, http.StatusBadGateway, errutil.CodeBadGateway, "invalid gateway response", traceID)
		return
	}

	writeTGResponse(w, UpdateResponse{
		OK:      true,
		TraceID: sr.TraceID,
		Total:   sr.Total,
		Books:   sr.Books,
		Page:    sr.Page,
		Pages:   sr.Pages,
	})
}

func writeTGResponse(w http.ResponseWriter, resp UpdateResponse) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	_ = json.NewEncoder(w).Encode(resp)
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
