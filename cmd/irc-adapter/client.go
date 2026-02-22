package main

import (
	"bufio"
	"context"
	"fmt"
	"net"
	"os"
	"strings"
	"sync"
	"time"

	"ebusta/internal/edge"
)

type IRCClient struct {
	conn     net.Conn
	writer   *bufio.Writer
	reader   *bufio.Reader
	nick     string
	user     string
	realName string
	password string
	channels map[string]bool
	verbose  bool
	asServer bool

	mu      sync.RWMutex
	done    chan struct{}
	handler *IRCHandler
}

func NewIRCClient(conn net.Conn, nick, user, realName string, handler *IRCHandler, verbose bool) *IRCClient {
	addr := conn.RemoteAddr()
	if verbose {
		fmt.Fprintf(os.Stderr, "  👤 New client: %s\n", addr)
	}

	return &IRCClient{
		conn:     conn,
		writer:   bufio.NewWriter(conn),
		reader:   bufio.NewReader(conn),
		nick:     nick,
		user:     user,
		realName: realName,
		channels: make(map[string]bool),
		done:     make(chan struct{}),
		handler:  handler,
		verbose:  verbose,
		asServer: true,
	}
}

func NewIRCBotClient(conn net.Conn, nick, user, realName, password string, handler *IRCHandler, verbose bool) *IRCClient {
	return &IRCClient{
		conn:     conn,
		writer:   bufio.NewWriter(conn),
		reader:   bufio.NewReader(conn),
		nick:     nick,
		user:     user,
		realName: realName,
		password: password,
		channels: make(map[string]bool),
		done:     make(chan struct{}),
		handler:  handler,
		verbose:  verbose,
		asServer: false,
	}
}

func (c *IRCClient) Start() {
	defer c.Close()

	if c.verbose {
		fmt.Fprintf(os.Stderr, "  ▶️  Client started: %s\n", c.conn.RemoteAddr())
	}

	c.SendNotice("Ebusta Book Search Bot ready")
	c.conn.SetReadDeadline(time.Now().Add(5 * time.Minute))

	for {
		select {
		case <-c.done:
			return
		default:
			line, err := c.reader.ReadString('\n')
			if err != nil {
				if c.verbose {
					fmt.Fprintf(os.Stderr, "  ⚠️  Client %s disconnected: %v\n", c.conn.RemoteAddr(), err)
				}
				return
			}

			line = strings.TrimSpace(line)
			if err := edge.ValidateLineLength(line, 1024); err != nil {
				c.SendNotice("Command too long")
				continue
			}
			if c.verbose && line != "" {
				fmt.Fprintf(os.Stderr, "  📨 [%s] Received: %q\n", c.conn.RemoteAddr(), line)
			}

			if strings.HasPrefix(line, "!") || strings.HasPrefix(line, "/") {
				c.handler.HandlePrivateMessage(c, line)
			}

			c.conn.SetReadDeadline(time.Now().Add(5 * time.Minute))
		}
	}
}

func (c *IRCClient) StartBot(ctx context.Context, channels []string) error {
	defer c.Close()

	if c.password != "" {
		c.Send("PASS", c.password)
	}
	c.Send("NICK", c.nick)
	c.Send("USER", c.user, "0", "*", c.realName)

	joinSent := false
	closeOnDone := make(chan struct{})
	go func() {
		select {
		case <-ctx.Done():
			c.Close()
		case <-closeOnDone:
		}
	}()
	defer close(closeOnDone)

	for {
		line, err := c.reader.ReadString('\n')
		if err != nil {
			if ctx.Err() != nil {
				return nil
			}
			return err
		}
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}

		if c.verbose {
			fmt.Fprintf(os.Stderr, "  📨 [bot] %q\n", line)
		}

		prefix, command, params := parseIRCLine(line)
		switch command {
		case "PING":
			if len(params) > 0 {
				c.Send("PONG", params[len(params)-1])
			}
		case "001":
			if !joinSent {
				for _, ch := range channels {
					ch = strings.TrimSpace(ch)
					if ch == "" {
						continue
					}
					c.Send("JOIN", ch)
				}
				joinSent = true
			}
		case "PRIVMSG":
			if len(params) < 2 {
				continue
			}
			target := params[0]
			message := params[1]
			sender := ircNickFromPrefix(prefix)
			if sender == "" {
				sender = target
			}
			if strings.HasPrefix(target, "#") {
				c.handler.HandleChannelMessageFrom(c, target, sender, message)
			} else {
				c.handler.HandlePrivateMessageFrom(c, sender, message)
			}
		}
	}
}

func parseIRCLine(line string) (prefix, command string, params []string) {
	rest := strings.TrimSpace(line)
	if rest == "" {
		return "", "", nil
	}
	if strings.HasPrefix(rest, ":") {
		idx := strings.IndexByte(rest, ' ')
		if idx < 0 {
			return strings.TrimPrefix(rest, ":"), "", nil
		}
		prefix = strings.TrimPrefix(rest[:idx], ":")
		rest = strings.TrimSpace(rest[idx+1:])
	}

	if rest == "" {
		return prefix, "", nil
	}

	trailing := ""
	if idx := strings.Index(rest, " :"); idx >= 0 {
		trailing = rest[idx+2:]
		rest = rest[:idx]
	} else if strings.HasPrefix(rest, ":") {
		trailing = strings.TrimPrefix(rest, ":")
		rest = ""
	}

	fields := strings.Fields(rest)
	if len(fields) > 0 {
		command = strings.ToUpper(fields[0])
		if len(fields) > 1 {
			params = append(params, fields[1:]...)
		}
	}
	if trailing != "" {
		params = append(params, trailing)
	}
	return prefix, command, params
}

func ircNickFromPrefix(prefix string) string {
	if prefix == "" {
		return ""
	}
	if i := strings.Index(prefix, "!"); i >= 0 {
		return prefix[:i]
	}
	return prefix
}

func (c *IRCClient) Send(command string, args ...string) {
	c.mu.Lock()
	defer c.mu.Unlock()

	var msg strings.Builder
	if c.asServer {
		msg.WriteString(":")
		msg.WriteString(c.nick)
		msg.WriteString(" ")
	}
	msg.WriteString(command)

	for i, arg := range args {
		msg.WriteString(" ")
		if i == len(args)-1 {
			if strings.HasPrefix(arg, ":") {
				msg.WriteString(arg)
			} else if strings.Contains(arg, " ") {
				msg.WriteString(":")
				msg.WriteString(arg)
			} else {
				msg.WriteString(arg)
			}
			continue
		}
		msg.WriteString(arg)
	}
	msg.WriteString("\r\n")

	fullMsg := msg.String()
	if c.verbose {
		fmt.Fprintf(os.Stderr, "  📤 [%s] Sending (%d bytes): %q", c.conn.RemoteAddr(), len(fullMsg), fullMsg)
	}

	n, err := c.writer.WriteString(fullMsg)
	if err != nil {
		fmt.Fprintf(os.Stderr, "  ❌ [%s] Write error: %v\n", c.conn.RemoteAddr(), err)
		return
	}

	err = c.writer.Flush()
	if err != nil {
		fmt.Fprintf(os.Stderr, "  ❌ [%s] Flush error: %v\n", c.conn.RemoteAddr(), err)
		return
	}

	if c.verbose {
		fmt.Fprintf(os.Stderr, "  ✅ [%s] Sent %d bytes\n", c.conn.RemoteAddr(), n)
	}
}

func (c *IRCClient) SendMessage(target, text string) {
	if c.verbose {
		fmt.Fprintf(os.Stderr, "  📤 [%s] SendMessage to %s: %q\n", c.conn.RemoteAddr(), target, text)
	}
	c.Send("PRIVMSG", target, text)
}

func (c *IRCClient) SendNotice(text string) {
	c.SendMessage(c.nick, text)
}

func (c *IRCClient) SendWelcome() {
	c.Send("001", c.nick, "Welcome to Ebusta IRC Gateway")
	c.Send("002", c.nick, "Your host is ebusta.local")
	c.Send("003", c.nick, "This server was created today")
	c.Send("004", c.nick, "ebusta.local 1.0")
	c.Send("005", c.nick, "PREFIX=(ov)@+ CHANTYPES=# CHANMODES=,,, CASEMAPPING=ascii")
	c.Send("375", c.nick, ":- Message of the Day -")
	c.Send("372", c.nick, "Welcome to Ebusta Book Search")
	c.Send("376", c.nick, "End of MOTD command")
}

func (c *IRCClient) Close() {
	c.mu.Lock()
	defer c.mu.Unlock()

	select {
	case <-c.done:
		return
	default:
		if c.verbose {
			fmt.Fprintf(os.Stderr, "  🔚 [%s] Closing connection\n", c.conn.RemoteAddr())
		}
		close(c.done)
		_ = c.conn.Close()
	}
}
