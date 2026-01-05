package delivery

import (
	"context"
	"encoding/json"
	"net/http"
	"strconv"
	"time"

	"github.com/sirupsen/logrus"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"

	"ebusta/internal/storage/datamanager/proxy"
	"ebusta/internal/storage/datamanager/shaping"
)

var (
	requestsTotal = prometheus.NewCounterVec(
		prometheus.CounterOpts{
			Name: "requests_total",
			Help: "Total number of HTTP requests",
		},
		[]string{"endpoint", "code"},
	)
)

func init() {
	prometheus.MustRegister(requestsTotal)
}

type Server struct {
	Log   *logrus.Logger
	Proxy *proxy.Proxy
}

func (s *Server) Health(w http.ResponseWriter, r *http.Request) {
	writeJSON(w, http.StatusOK, map[string]any{"ok": true})
	requestsTotal.WithLabelValues("health", "200").Inc()
}

func (s *Server) Metrics() http.Handler { return promhttp.Handler() }

// --- Search endpoints (hits) ---

// GET /search/author/exact
func (s *Server) SearchAuthorExact(w http.ResponseWriter, r *http.Request) {
	q := r.URL.Query()
	author := q.Get("author")
	if author == "" {
		proxy.WriteError(w, http.StatusBadRequest, "bad_request", "author parameter is required", nil)
		requestsTotal.WithLabelValues("author_exact", "400").Inc()
		return
	}
	from := atoiDefault(q.Get("from"), 0)
	size := atoiDefault(q.Get("size"), 10)

	ctx, cancel := context.WithTimeout(r.Context(), 10*time.Second); defer cancel()
	params := map[string]any{"author": author, "from": from, "size": size}
	data, code, err := s.Proxy.DoTemplate(ctx, "fl_author_exact", params)
	if err != nil {
		s.Log.WithError(err).WithField("template", "fl_author_exact").Error("os.request.failed")
		proxy.WriteError(w, http.StatusBadGateway, "upstream_error", "failed to execute template", err.Error())
		requestsTotal.WithLabelValues("author_exact", "502").Inc()
		return
	}
	if code >= 300 {
		proxy.WriteError(w, http.StatusBadGateway, "upstream_error", "opensearch returned error", jsonRaw(data))
		requestsTotal.WithLabelValues("author_exact", "502").Inc()
		return
	}
	if q.Get("format") == "full" {
		writeRawJSON(w, http.StatusOK, data); requestsTotal.WithLabelValues("author_exact", "200").Inc(); return
	}
	out, err := shaping.ShapeSearch(data, from, size)
	if err != nil { writeRawJSON(w, http.StatusOK, data); requestsTotal.WithLabelValues("author_exact", "200").Inc(); return }
	writeRawJSON(w, http.StatusOK, out); requestsTotal.WithLabelValues("author_exact", "200").Inc()
}

// GET /search/title/prefix?q=...&from=0&size=10
func (s *Server) SearchTitlePrefix(w http.ResponseWriter, r *http.Request) {
	s.genericSearch(w, r, "fl_title_prefix", "title_prefix")
}

// GET /search/title/contains?q=...&from=0&size=10
func (s *Server) SearchTitleContains(w http.ResponseWriter, r *http.Request) {
	s.genericSearch(w, r, "fl_title_substring", "title_contains")
}

// GET /search/mixed?q=...&from=0&size=10
func (s *Server) SearchMixed(w http.ResponseWriter, r *http.Request) {
	s.genericSearch(w, r, "fl_mixed_search", "mixed")
}

// GET /search/names?q=...&size=10
func (s *Server) SearchNames(w http.ResponseWriter, r *http.Request) {
	s.genericSearch(w, r, "fl_names_token_prefix", "names")
}

func (s *Server) genericSearch(w http.ResponseWriter, r *http.Request, templateID, metric string) {
	q := r.URL.Query()
	query := q.Get("q")
	if query == "" {
		proxy.WriteError(w, http.StatusBadRequest, "bad_request", "q parameter is required", nil)
		requestsTotal.WithLabelValues(metric, "400").Inc()
		return
	}
	from := atoiDefault(q.Get("from"), 0)
	size := atoiDefault(q.Get("size"), 10)

	ctx, cancel := context.WithTimeout(r.Context(), 10*time.Second); defer cancel()
	params := map[string]any{"q": query, "from": from, "size": size}
	data, code, err := s.Proxy.DoTemplate(ctx, templateID, params)
	if err != nil {
		s.Log.WithError(err).WithField("template", templateID).Error("os.request.failed")
		proxy.WriteError(w, http.StatusBadGateway, "upstream_error", "failed to execute template", err.Error())
		requestsTotal.WithLabelValues(metric, "502").Inc()
		return
	}
	if code >= 300 {
		proxy.WriteError(w, http.StatusBadGateway, "upstream_error", "opensearch returned error", jsonRaw(data))
		requestsTotal.WithLabelValues(metric, "502").Inc()
		return
	}
	if q.Get("format") == "full" {
		writeRawJSON(w, http.StatusOK, data); requestsTotal.WithLabelValues(metric, "200").Inc(); return
	}
	out, err := shaping.ShapeSearch(data, from, size)
	if err != nil { writeRawJSON(w, http.StatusOK, data); requestsTotal.WithLabelValues(metric, "200").Inc(); return }
	writeRawJSON(w, http.StatusOK, out); requestsTotal.WithLabelValues(metric, "200").Inc()
}

// --- List endpoints (composite aggs) ---

// GET /list/authors?size=1000&after=<base64 or json>
func (s *Server) ListAuthors(w http.ResponseWriter, r *http.Request) {
	s.genericList(w, r, "fl_authors_all", "list_authors")
}

// GET /list/titles?size=1000&after=<base64 or json>
func (s *Server) ListTitles(w http.ResponseWriter, r *http.Request) {
	s.genericList(w, r, "fl_titles_all", "list_titles")
}

func (s *Server) genericList(w http.ResponseWriter, r *http.Request, templateID, metric string) {
	q := r.URL.Query()
	size := atoiDefault(q.Get("size"), 1000)
	afterParam := q.Get("after")
	after, err := proxy.DecodeAfter(afterParam)
	if err != nil {
		proxy.WriteError(w, http.StatusBadRequest, "bad_request", "invalid 'after' parameter", err.Error())
		requestsTotal.WithLabelValues(metric, "400").Inc()
		return
	}

	ctx, cancel := context.WithTimeout(r.Context(), 10*time.Second); defer cancel()
	params := map[string]any{"size": size}
	if after != nil { params["after"] = after }
	data, code, err := s.Proxy.DoTemplate(ctx, templateID, params)
	if err != nil {
		s.Log.WithError(err).WithField("template", templateID).Error("os.request.failed")
		proxy.WriteError(w, http.StatusBadGateway, "upstream_error", "failed to execute template", err.Error())
		requestsTotal.WithLabelValues(metric, "502").Inc()
		return
	}
	if code >= 300 {
		proxy.WriteError(w, http.StatusBadGateway, "upstream_error", "opensearch returned error", jsonRaw(data))
		requestsTotal.WithLabelValues(metric, "502").Inc()
		return
	}
	if q.Get("format") == "full" {
		writeRawJSON(w, http.StatusOK, data); requestsTotal.WithLabelValues(metric, "200").Inc(); return
	}
	out, err := shaping.ShapeComposite(data)
	if err != nil { writeRawJSON(w, http.StatusOK, data); requestsTotal.WithLabelValues(metric, "200").Inc(); return }
	writeRawJSON(w, http.StatusOK, out); requestsTotal.WithLabelValues(metric, "200").Inc()
}

// --- Template passthrough ---

// POST /template/{id}  body: {"params": {...}, "format":"full"}
func (s *Server) TemplateExec(w http.ResponseWriter, r *http.Request) {
	id := lastPath(r.URL.Path)
	var body struct {
		Params map[string]any `json:"params"`
		Format string         `json:"format"`
	}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		proxy.WriteError(w, http.StatusBadRequest, "bad_request", "invalid json body", err.Error())
		requestsTotal.WithLabelValues("template_exec", "400").Inc()
		return
	}
	ctx, cancel := context.WithTimeout(r.Context(), 10*time.Second); defer cancel()
	data, code, err := s.Proxy.DoTemplate(ctx, id, body.Params)
	if err != nil {
		s.Log.WithError(err).Error("template exec failed")
		proxy.WriteError(w, http.StatusBadGateway, "upstream_error", "failed to execute template", err.Error())
		requestsTotal.WithLabelValues("template_exec", "502").Inc()
		return
	}
	if code >= 300 {
		proxy.WriteError(w, http.StatusBadGateway, "upstream_error", "opensearch returned error", jsonRaw(data))
		requestsTotal.WithLabelValues("template_exec", "502").Inc()
		return
	}
	if body.Format == "full" {
		writeRawJSON(w, http.StatusOK, data); requestsTotal.WithLabelValues("template_exec", "200").Inc(); return
	}
	out, err := shaping.ShapeSearch(data, 0, 0)
	if err != nil { writeRawJSON(w, http.StatusOK, data); requestsTotal.WithLabelValues("template_exec", "200").Inc(); return }
	writeRawJSON(w, http.StatusOK, out); requestsTotal.WithLabelValues("template_exec", "200").Inc()
}

// --- helpers ---

func writeJSON(w http.ResponseWriter, status int, v any) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	_ = json.NewEncoder(w).Encode(v)
}
func writeRawJSON(w http.ResponseWriter, status int, b []byte) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	_, _ = w.Write(b)
}
func atoiDefault(s string, d int) int {
	if s == "" { return d }
	v, err := strconv.Atoi(s); if err != nil { return d }; return v
}
func lastPath(p string) string {
	for len(p) > 0 && p[len(p)-1] == '/' { p = p[:len(p)-1] }
	for i := len(p)-1; i >= 0; i-- {
		if p[i] == '/' { return p[i+1:] }
	}
	return p
}

type jsonRaw []byte
func (j jsonRaw) MarshalJSON() ([]byte, error) {
	var v any
	if json.Unmarshal([]byte(j), &v) == nil { return json.Marshal(v) }
	return json.Marshal(string(j))
}
