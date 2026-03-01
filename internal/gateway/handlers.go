package gateway

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strings"

	"ebusta/internal/errutil"
	"ebusta/internal/gateway/clients"
	"ebusta/internal/gateway/mapper"
	"ebusta/internal/logger"
	"ebusta/internal/presenter"
)

type SearchResponse struct {
	TraceID   string         `json:"trace_id"`
	Total     int            `json:"total"`
	Books     []BookResponse `json:"books"`
	Page      int            `json:"page"`
	Pages     int            `json:"pages"`
	ExecMode  string         `json:"exec_mode,omitempty"`
	MatchMode string         `json:"match_mode,omitempty"`
}

type BookResponse struct {
	Title       string   `json:"title"`
	Authors     []string `json:"authors"`
	FullAuthors string   `json:"full_authors"`
	DownloadURL string   `json:"download_url"`
}

type DownloadResponse struct {
	Token     string `json:"token"`
	ExpiresIn int64  `json:"expires_in"`
	Size      int64  `json:"size"`
	Filename  string `json:"filename"`
}

func (s *Server) handleSearch(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()
	traceID := errutil.TraceIDFromRequest(r)
	if traceID == "" {
		traceID = logger.GenerateTraceID("gw")
	}

	body, err := io.ReadAll(r.Body)
	if err != nil {
		errutil.WriteJSONError(w, errutil.New(
			errutil.CodeInvalidArgument,
			"failed to read request body",
		).WithTrace(traceID))
		return
	}

	if err := s.sizeLimiter.ValidateJSON(body); err != nil {
		errutil.WriteJSONError(w, errutil.New(
			errutil.CodeInvalidArgument,
			err.Error(),
		).WithTrace(traceID))
		return
	}

	req, err := s.validator.ValidateSearch(body)
	if err != nil {
		errutil.WriteJSONError(w, errutil.New(
			errutil.CodeInvalidArgument,
			err.Error(),
		).WithTrace(traceID))
		return
	}

	query := strings.TrimSpace(req.Query)
	if !s.sanitizer.IsSQLSafe(query) {
		logger.GetGlobal().WithField("query", s.sanitizer.SanitizeForLog(query)).WarnCtx(ctx, "possible SQL injection blocked")
		errutil.WriteJSONError(w, errutil.New(
			errutil.CodeInvalidArgument,
			"invalid query",
		).WithTrace(traceID))
		return
	}

	result, err := s.orchestrator.Search(ctx, &clients.SearchRequest{
		Query:   query,
		Page:    req.Page,
		Limit:   req.Limit,
		TraceID: traceID,
	})
	if err != nil {
		logger.GetGlobal().WithField("query", s.sanitizer.SanitizeForLog(query)).ErrorCtx(ctx, "orchestrator search failed", err)
		errutil.WriteJSONError(w, errutil.FromGRPCError(err, traceID))
		return
	}

	response := SearchResponse{
		TraceID: traceID,
		Total:   result.Total,
		Books:   make([]BookResponse, 0, len(result.Books)),
		Page:    req.Page,
		Pages:   1,
	}
	response.ExecMode, response.MatchMode = parseSearchDiagnostics(result.Status)
	pg := presenter.NewPagination(result.Total, req.Page, req.Limit)
	response.Page = pg.CurrentPage
	response.Pages = pg.TotalPages

	for _, book := range result.Books {
		token, err := s.mapper.GenerateToken(book.ID, "")
		if err != nil {
			logger.GetGlobal().WithField("book_id", book.ID).ErrorCtx(ctx, "failed to generate token", err)
			continue
		}

		response.Books = append(response.Books, BookResponse{
			Title:       book.Title,
			Authors:     book.Authors,
			FullAuthors: strings.Join(book.Authors, ", "),
			DownloadURL: fmt.Sprintf("/download/%s", token),
		})
	}

	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("X-Trace-Id", traceID)
	json.NewEncoder(w).Encode(response)
}

func parseSearchDiagnostics(status string) (string, string) {
	parts := strings.Split(status, ";")
	var execMode, matchMode string
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if strings.HasPrefix(p, "exec=") {
			execMode = strings.TrimPrefix(p, "exec=")
		}
		if strings.HasPrefix(p, "match=") {
			matchMode = strings.TrimPrefix(p, "match=")
		}
	}
	return execMode, matchMode
}

func (s *Server) handleDownload(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()
	traceID := errutil.TraceIDFromRequest(r)
	if traceID == "" {
		traceID = logger.GenerateTraceID("gw-dl")
	}

	token := strings.TrimPrefix(r.URL.Path, "/download/")
	token = strings.TrimSpace(token)

	if token == "" {
		errutil.WriteJSONError(w, errutil.New(
			errutil.CodeInvalidArgument,
			"missing download token",
		).WithTrace(traceID))
		return
	}

	if !s.validator.ValidateToken(token) {
		logger.GetGlobal().WithField("token", s.sanitizer.SanitizeForLog(token)).WarnCtx(ctx, "invalid token format")
		errutil.WriteJSONError(w, errutil.New(
			errutil.CodeInvalidArgument,
			"invalid token format",
		).WithTrace(traceID))
		return
	}

	sha1, _, err := s.mapper.Resolve(token)
	if err != nil {
		logger.GetGlobal().WithField("token", s.sanitizer.SanitizeForLog(token)).WithField("error", err.Error()).WarnCtx(ctx, "token resolve failed")

		switch err {
		case mapper.ErrTokenExpired:
			errutil.WriteJSONError(w, errutil.New(
				errutil.CodeNotFound,
				"download token expired",
			).WithTrace(traceID))
		default:
			errutil.WriteJSONError(w, errutil.New(
				errutil.CodeNotFound,
				"invalid download token",
			).WithTrace(traceID))
		}
		return
	}

	if r.Method == http.MethodHead {
		meta, err := s.downloader.GetMetaWithTrace(sha1, traceID)
		if err != nil {
			logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "failed to get meta", err)
			var appErr *errutil.AppError
			if errors.As(err, &appErr) {
				errutil.WriteJSONError(w, appErr)
				return
			}
			errutil.WriteJSONError(w, errutil.New(
				errutil.CodeInternal,
				"failed to get book metadata",
			).WithTrace(traceID).WithDetails(err.Error()))
			return
		}

		w.Header().Set("Content-Type", "application/octet-stream")
		w.Header().Set("Content-Disposition", fmt.Sprintf("attachment; filename=%q", meta.Filename))
		w.Header().Set("Content-Length", fmt.Sprintf("%d", meta.Size))
		w.Header().Set("X-Trace-Id", traceID)
		w.WriteHeader(http.StatusOK)
		return
	}

	w.Header().Set("Content-Type", "application/octet-stream")
	w.Header().Set("X-Trace-Id", traceID)

	if err := s.downloader.StreamBookWithTrace(sha1, w, traceID); err != nil {
		logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "failed to stream book", err)
		var appErr *errutil.AppError
		if errors.As(err, &appErr) {
			errutil.WriteJSONError(w, appErr)
			return
		}
		errutil.WriteJSONError(w, errutil.New(
			errutil.CodeInternal,
			"failed to stream book",
		).WithTrace(traceID).WithDetails(err.Error()))
		return
	}

	if r.URL.Query().Get("single_use") == "1" {
		s.mapper.Revoke(token)
	}
}

func (s *Server) handleDownloadToken(w http.ResponseWriter, r *http.Request) {
	traceID := errutil.TraceIDFromRequest(r)
	if traceID == "" {
		traceID = logger.GenerateTraceID("gw-token")
	}

	token := strings.TrimPrefix(r.URL.Path, "/download/token/")

	sha1, _, err := s.mapper.Resolve(token)
	if err != nil {
		errutil.WriteJSONError(w, errutil.New(
			errutil.CodeNotFound,
			"invalid token",
		).WithTrace(traceID))
		return
	}

	meta, err := s.downloader.GetMetaWithTrace(sha1, traceID)
	if err != nil {
		logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(r.Context(), "failed to get metadata", err)
		var appErr *errutil.AppError
		if errors.As(err, &appErr) {
			errutil.WriteJSONError(w, appErr)
			return
		}
		errutil.WriteJSONError(w, errutil.New(
			errutil.CodeInternal,
			"failed to get metadata",
		).WithTrace(traceID).WithDetails(err.Error()))
		return
	}

	expiresIn := int64(s.config.Mapper.TTL.Seconds())

	response := DownloadResponse{
		Token:     token,
		ExpiresIn: expiresIn,
		Size:      meta.Size,
		Filename:  meta.Filename,
	}

	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("X-Trace-Id", traceID)
	json.NewEncoder(w).Encode(response)
}

func (s *Server) handleDebug(w http.ResponseWriter, r *http.Request) {
	if r.URL.Path != "/debug/mapper" {
		http.NotFound(w, r)
		return
	}

	stats := s.mapper.Stats()

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(stats)
}
