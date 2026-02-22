package main

import (
	"context"
	"io"
	"net"
	"net/http"
	"strings"
	"testing"
	"time"

	"ebusta/internal/edge"
)

func readLineTimeout(t *testing.T, c net.Conn, d time.Duration) string {
	t.Helper()
	_ = c.SetReadDeadline(time.Now().Add(d))
	buf := make([]byte, 4096)
	n, err := c.Read(buf)
	if err != nil {
		t.Fatalf("read timeout/error: %v", err)
	}
	return string(buf[:n])
}

func TestParseIRCLineAndNick(t *testing.T) {
	prefix, cmd, params := parseIRCLine(":alice!u@h PRIVMSG #books :!help")
	if prefix != "alice!u@h" || cmd != "PRIVMSG" {
		t.Fatalf("unexpected parse: prefix=%q cmd=%q", prefix, cmd)
	}
	if len(params) != 2 || params[0] != "#books" || params[1] != "!help" {
		t.Fatalf("unexpected params: %#v", params)
	}
	if ircNickFromPrefix(prefix) != "alice" {
		t.Fatalf("unexpected nick parse: %q", ircNickFromPrefix(prefix))
	}
}

func TestStartBotHandshakePingJoinAndCommand(t *testing.T) {
	h := NewIRCHandler("http://unused", 5, false)
	clientConn, serverConn := net.Pipe()
	defer serverConn.Close()

	bot := NewIRCBotClient(clientConn, "ebusta-bot", "ebusta", "Ebusta Bot", "", h, false)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	errCh := make(chan error, 1)
	go func() {
		errCh <- bot.StartBot(ctx, []string{"#books"})
	}()

	first := readLineTimeout(t, serverConn, 2*time.Second)
	if !strings.Contains(first, "NICK ebusta-bot") {
		t.Fatalf("expected NICK, got %q", first)
	}

	second := readLineTimeout(t, serverConn, 2*time.Second)
	if !strings.Contains(second, "USER ebusta 0 * :Ebusta Bot") {
		t.Fatalf("expected USER, got %q", second)
	}

	_, _ = serverConn.Write([]byte(":irc.local 001 ebusta-bot :welcome\r\n"))
	join := readLineTimeout(t, serverConn, 2*time.Second)
	if !strings.Contains(join, "JOIN #books") {
		t.Fatalf("expected JOIN, got %q", join)
	}

	_, _ = serverConn.Write([]byte("PING :123\r\n"))
	pong := readLineTimeout(t, serverConn, 2*time.Second)
	if !strings.Contains(pong, "PONG :123") && !strings.Contains(pong, "PONG 123") {
		t.Fatalf("expected PONG, got %q", pong)
	}

	_, _ = serverConn.Write([]byte(":alice!u@h PRIVMSG #books :!unknown\r\n"))
	reply := readLineTimeout(t, serverConn, 2*time.Second)
	if !strings.Contains(reply, "PRIVMSG #books") || !strings.Contains(reply, "Unknown command") {
		t.Fatalf("expected bot command response, got %q", reply)
	}

	cancel()
	select {
	case err := <-errCh:
		if err != nil {
			t.Fatalf("unexpected bot exit error: %v", err)
		}
	case <-time.After(2 * time.Second):
		t.Fatalf("bot did not exit after cancel")
	}
}

func TestBotModeRateLimitIsPerSender(t *testing.T) {
	p := edge.DefaultPolicy("irc")
	p.Actions["command"] = edge.ActionPolicy{PerMinute: 1, Burst: 1}
	h := NewIRCHandlerWithPolicy("http://gw.local", 5, false, p, nil)
	h.httpClient = &http.Client{
		Transport: roundTripFunc(func(r *http.Request) (*http.Response, error) {
			body := `{"trace_id":"gw-1","total":0,"books":[],"page":1,"pages":0}`
			return &http.Response{
				StatusCode: http.StatusOK,
				Header:     http.Header{"Content-Type": []string{"application/json"}},
				Body:       io.NopCloser(strings.NewReader(body)),
			}, nil
		}),
	}
	client, peer := newTestIRCClient(t, "ebusta-bot")

	h.HandlePrivateMessageFrom(client, "alice", "!search author:king")
	h.HandlePrivateMessageFrom(client, "alice", "!search author:king")
	h.HandlePrivateMessageFrom(client, "bob", "!search author:king")

	lines := readIRCOutboundLines(t, peer)
	aliceDenied := hasLineContaining(lines, "Too many requests")
	if !aliceDenied {
		t.Fatalf("expected alice to be rate-limited on second request")
	}
	if !hasLineContaining(lines, "No books found") {
		t.Fatalf("expected successful search response for non-limited sender")
	}
}
