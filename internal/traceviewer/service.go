package traceviewer

import (
	"context"
	"encoding/json"
	"html/template"
	"net/http"
	"strings"

	"ebusta/internal/logger"
	"ebusta/internal/lokiclient"
)

type TraceReader interface {
	QueryTrace(ctx context.Context, traceID string) ([]lokiclient.Entry, error)
}

type Service struct {
	reader TraceReader
	log    *logger.Logger
}

func New(reader TraceReader, log *logger.Logger) *Service {
	return &Service{reader: reader, log: log}
}

func (s *Service) Handler() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/", s.handleIndex)
	mux.HandleFunc("/health", s.handleHealth)
	mux.HandleFunc("/trace/", s.handleTrace)
	return mux
}

func (s *Service) handleHealth(w http.ResponseWriter, _ *http.Request) {
	w.WriteHeader(http.StatusOK)
	_, _ = w.Write([]byte("ok"))
}

func (s *Service) handleIndex(w http.ResponseWriter, r *http.Request) {
	if r.URL.Path != "/" {
		http.NotFound(w, r)
		return
	}
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	_ = indexTemplate.Execute(w, nil)
}

func (s *Service) handleTrace(w http.ResponseWriter, r *http.Request) {
	traceID := strings.TrimPrefix(r.URL.Path, "/trace/")
	traceID = strings.TrimSpace(traceID)
	if traceID == "" {
		http.Error(w, `{"error":"trace_id is required"}`, http.StatusBadRequest)
		return
	}

	entries, err := s.reader.QueryTrace(r.Context(), traceID)
	if err != nil {
		s.log.ErrorCtx(r.Context(), "trace lookup failed", err)
		http.Error(w, `{"error":"trace lookup failed"}`, http.StatusBadGateway)
		return
	}

	response := struct {
		TraceID string             `json:"trace_id"`
		Count   int                `json:"count"`
		Entries []lokiclient.Entry `json:"entries"`
	}{
		TraceID: traceID,
		Count:   len(entries),
		Entries: entries,
	}

	w.Header().Set("Content-Type", "application/json")
	_ = json.NewEncoder(w).Encode(response)
}

var indexTemplate = template.Must(template.New("trace-viewer").Parse(`<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Trace Viewer</title>
  <style>
    :root { color-scheme: light; --bg:#f4efe7; --ink:#1f1f1f; --accent:#124559; --card:#fffaf2; --muted:#6f6f6f; --line:#d8ccb8; --error:#7f1d1d; }
    body { margin:0; font-family: Georgia, serif; background:linear-gradient(180deg, #efe4d2, #f8f5ef); color:var(--ink); }
    main { max-width:960px; margin:40px auto; padding:24px; }
    h1 { margin:0 0 8px; font-size:40px; }
    p { color:var(--muted); }
    form { display:flex; gap:12px; margin:24px 0; flex-wrap:wrap; }
    input { flex:1 1 320px; padding:14px 16px; border:1px solid var(--line); border-radius:12px; font-size:16px; background:var(--card); }
    button { padding:14px 20px; border:0; border-radius:12px; background:var(--accent); color:white; font-size:16px; cursor:pointer; }
    .panel { border:1px solid var(--line); border-radius:18px; background:rgba(255,250,242,0.92); padding:18px; box-shadow:0 8px 32px rgba(18,69,89,0.08); }
    .entry { border-top:1px solid var(--line); padding:12px 0; }
    .entry:first-child { border-top:0; }
    .meta { font-family: ui-monospace, monospace; font-size:13px; color:var(--muted); margin-bottom:6px; }
    .service { display:inline-block; min-width:120px; color:var(--accent); }
    .error { color:var(--error); white-space:pre-wrap; }
    .message { white-space:pre-wrap; }
  </style>
</head>
<body>
  <main>
    <h1>Trace Viewer</h1>
    <p>Search aggregated stage logs by trace_id.</p>
    <form id="trace-form">
      <input id="trace-id" name="trace-id" placeholder="gw-1772922568796072643" autocomplete="off">
      <button type="submit">Search</button>
    </form>
    <div id="status" class="panel">Enter a trace_id to load logs.</div>
  </main>
  <script>
    const form = document.getElementById('trace-form');
    const traceInput = document.getElementById('trace-id');
    const status = document.getElementById('status');

    function renderEntries(traceId, entries) {
      if (!entries.length) {
        status.innerHTML = '<strong>No logs found.</strong>';
        return;
      }
      status.innerHTML = '<strong>' + traceId + '</strong><div>' + entries.map((entry) => {
        return '<div class="entry">' +
          '<div class="meta"><span class="service">' + (entry.service || 'unknown') + '</span>' +
          (entry.timestamp || '') + ' [' + (entry.level || '') + ']</div>' +
          '<div class="message">' + (entry.message || '') + '</div>' +
          (entry.error ? '<div class="error">error=' + entry.error + '</div>' : '') +
        '</div>';
      }).join('') + '</div>';
    }

    form.addEventListener('submit', async (event) => {
      event.preventDefault();
      const traceId = traceInput.value.trim();
      if (!traceId) {
        status.textContent = 'trace_id is required';
        return;
      }
      status.textContent = 'Loading...';
      const response = await fetch('/trace/' + encodeURIComponent(traceId));
      if (!response.ok) {
        status.textContent = 'Trace lookup failed';
        return;
      }
      const payload = await response.json();
      renderEntries(payload.trace_id, payload.entries || []);
    });
  </script>
</body>
</html>`))
