package main

import (
	"bufio"
	"bytes"
	"io"
	"net"
	"net/http"
	"strings"
	"sync"
	"testing"
	"time"

	"ebusta/internal/presenter"
)

type roundTripFunc func(*http.Request) (*http.Response, error)

func (f roundTripFunc) RoundTrip(r *http.Request) (*http.Response, error) {
	return f(r)
}

type captureConn struct {
	mu  sync.Mutex
	buf bytes.Buffer
}

func (c *captureConn) Read(_ []byte) (n int, err error)  { return 0, io.EOF }
func (c *captureConn) Close() error                      { return nil }
func (c *captureConn) LocalAddr() net.Addr               { return dummyAddr("local") }
func (c *captureConn) RemoteAddr() net.Addr              { return dummyAddr("remote") }
func (c *captureConn) SetDeadline(_ time.Time) error     { return nil }
func (c *captureConn) SetReadDeadline(_ time.Time) error { return nil }
func (c *captureConn) SetWriteDeadline(_ time.Time) error {
	return nil
}
func (c *captureConn) Write(p []byte) (n int, err error) {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.buf.Write(p)
}
func (c *captureConn) String() string {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.buf.String()
}

type dummyAddr string

func (d dummyAddr) Network() string { return "test" }
func (d dummyAddr) String() string  { return string(d) }

func newTestIRCClient(t *testing.T, nick string) (*IRCClient, *captureConn) {
	t.Helper()
	conn := &captureConn{}
	client := &IRCClient{
		conn:     conn,
		writer:   bufio.NewWriter(conn),
		reader:   bufio.NewReader(conn),
		nick:     nick,
		user:     "u",
		realName: "r",
		channels: map[string]bool{},
		done:     make(chan struct{}),
	}
	return client, conn
}

func readIRCOutboundLines(t *testing.T, conn *captureConn) []string {
	t.Helper()
	raw := conn.String()
	r := bufio.NewReader(strings.NewReader(raw))
	var lines []string
	for {
		line, err := r.ReadString('\n')
		if err != nil {
			break
		}
		lines = append(lines, strings.TrimSpace(line))
	}
	return lines
}

func hasLineContaining(lines []string, needle string) bool {
	for _, ln := range lines {
		if strings.Contains(ln, needle) {
			return true
		}
	}
	return false
}

func TestParseSearchCommand(t *testing.T) {
	tests := []struct {
		name       string
		parts      []string
		wantQuery  string
		wantPage   int
		shouldFail bool
	}{
		{"basic", []string{"!search", "author:king"}, "author:king", 1, false},
		{"with_page_suffix", []string{"!search", "author:king", "page", "3"}, "author:king", 3, false},
		{"with_spaces", []string{"!search", "author:stephen", "king", "page", "2"}, "author:stephen king", 2, false},
		{"invalid_page", []string{"!search", "author:king", "page", "x"}, "", 1, true},
		{"empty_query", []string{"!search"}, "", 1, true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			q, p, err := parseSearchCommand(tt.parts)
			if tt.shouldFail {
				if err == nil {
					t.Fatalf("expected error, got nil")
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if q != tt.wantQuery || p != tt.wantPage {
				t.Fatalf("got (%q,%d), want (%q,%d)", q, p, tt.wantQuery, tt.wantPage)
			}
		})
	}
}

func TestCmdSearchSuccessStoresSessionAndReplies(t *testing.T) {
	h := NewIRCHandler("http://gw.local", 5, false)
	h.httpClient = &http.Client{
		Transport: roundTripFunc(func(r *http.Request) (*http.Response, error) {
			body := `{"trace_id":"gw-1","total":1,"books":[{"id":"id1","title":"Book One","authors":["A"],"full_authors":"A"}],"page":1,"pages":1}`
			return &http.Response{
				StatusCode: http.StatusOK,
				Header:     http.Header{"Content-Type": []string{"application/json"}},
				Body:       io.NopCloser(strings.NewReader(body)),
			}, nil
		}),
	}
	client, peer := newTestIRCClient(t, "nick1")

	h.cmdSearch(client, "nick1", "nick1", []string{"!search", "author:king"})

	lines := readIRCOutboundLines(t, peer)
	if !hasLineContaining(lines, "Searching for: author:king") {
		t.Fatalf("missing search confirmation line, got: %v", lines)
	}
	if !hasLineContaining(lines, "Found 1 books") {
		t.Fatalf("missing result line, got: %v", lines)
	}

	h.mu.RLock()
	_, ok := h.sessions["nick1"]
	h.mu.RUnlock()
	if !ok {
		t.Fatalf("expected session to be stored")
	}
}

func TestCmdSearchGatewayErrorReturnsTraceMessage(t *testing.T) {
	h := NewIRCHandler("http://gw.local", 5, false)
	h.httpClient = &http.Client{
		Transport: roundTripFunc(func(r *http.Request) (*http.Response, error) {
			body := `{"error":{"code":"UNAVAILABLE","message":"rate limit exceeded","trace_id":"gw-rate-1","http_code":429}}`
			return &http.Response{
				StatusCode: http.StatusTooManyRequests,
				Header:     http.Header{"Content-Type": []string{"application/json"}},
				Body:       io.NopCloser(strings.NewReader(body)),
			}, nil
		}),
	}
	client, peer := newTestIRCClient(t, "nick2")

	h.cmdSearch(client, "nick2", "nick2", []string{"!search", "author:king"})
	lines := readIRCOutboundLines(t, peer)
	if !hasLineContaining(lines, "rate limit exceeded (trace: gw-rate-1)") {
		t.Fatalf("expected gateway error with trace, got: %v", lines)
	}
}

func TestCmdGetUsesSession(t *testing.T) {
	h := NewIRCHandler("http://unused", 5, false)
	client, peer := newTestIRCClient(t, "nick3")

	h.mu.Lock()
	h.sessions["nick3"] = &SearchSession{
		Query: "q",
		Results: &presenter.PresenterResult{
			SearchResult: &presenter.SearchResult{
				Books: []presenter.BookDTO{
					{Title: "T1", FullAuthors: "A1"},
				},
			},
		},
	}
	h.mu.Unlock()

	h.cmdGet(client, "nick3", "nick3", []string{"!info", "1"})
	lines := readIRCOutboundLines(t, peer)
	if !hasLineContaining(lines, "📖 T1") || !hasLineContaining(lines, "👤 A1") {
		t.Fatalf("expected book info lines, got: %v", lines)
	}
}

func TestCheckRateLimitDeniesAfterBurst(t *testing.T) {
	h := NewIRCHandler("http://unused", 5, false)
	for i := 0; i < 30; i++ {
		if !h.checkRateLimit("nick-burst") {
			t.Fatalf("request %d should pass", i+1)
		}
	}
	if h.checkRateLimit("nick-burst") {
		t.Fatalf("31st request should be denied")
	}
}

func TestHandleMessageRejectsTooLongCommand(t *testing.T) {
	h := NewIRCHandler("http://unused", 5, false)
	client, peer := newTestIRCClient(t, "nick-long")

	long := "!" + strings.Repeat("a", 600)
	h.handleMessage(client, "nick-long", "nick-long", long)

	lines := readIRCOutboundLines(t, peer)
	if !hasLineContaining(lines, "Command too long") {
		t.Fatalf("expected command too long response, got: %v", lines)
	}
}
