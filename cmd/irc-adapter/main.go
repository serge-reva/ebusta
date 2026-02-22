package main

import (
	"context"
	"flag"
	"fmt"
	"net"
	"os"
	"os/signal"
	"syscall"
	"time"

	"ebusta/internal/config"
	"ebusta/internal/edge"
	"ebusta/internal/logger"
)

func main() {
	var configPath string
	var verbose bool

	flag.StringVar(&configPath, "config", "ebusta.yaml", "path to config file")
	flag.BoolVar(&verbose, "verbose", false, "enable verbose output")
	flag.Parse()

	os.Setenv("EBUSTA_CONFIG", configPath)
	cfg := config.Get()
	ircCfg := loadIRCConfig(cfg, verbose)

	logCfg := cfg.Logger
	if ircCfg.Debug {
		logCfg.Level = "DEBUG"
	}
	logger.InitFromConfig(logCfg, "irc")

	fmt.Fprintf(os.Stderr, "🚀 IRC adapter starting on %s (verbose: %v)\n",
		ircCfg.Address(), ircCfg.Debug)

	policy := edge.PolicyFromConfig(cfg, "irc")
	handler := NewIRCHandlerWithPolicy(ircCfg.GatewayURL, ircCfg.PageSize, ircCfg.Debug, policy, edge.NewLabelCounterHook())

	listener, err := net.Listen("tcp", ircCfg.Address())
	if err != nil {
		fmt.Fprintf(os.Stderr, "❌ Failed to listen: %v\n", err)
		logger.FatalCtx(context.Background(), "failed to listen", err)
	}
	defer listener.Close()

	fmt.Fprintf(os.Stderr, "✅ IRC adapter listening on %s\n", ircCfg.Address())
	logger.InfoCtx(context.Background(), "IRC adapter started")

	stop := make(chan os.Signal, 1)
	signal.Notify(stop, syscall.SIGINT, syscall.SIGTERM, syscall.SIGHUP)

	go func() {
		for {
			conn, err := listener.Accept()
			if err != nil {
				select {
				case <-stop:
					return
				default:
					fmt.Fprintf(os.Stderr, "❌ Accept error: %v\n", err)
					logger.ErrorCtx(context.Background(), "accept error", err)
					continue
				}
			}

			fmt.Fprintf(os.Stderr, "🔌 New connection from %s\n", conn.RemoteAddr())

			client := NewIRCClient(conn, ircCfg.Nick, ircCfg.User, ircCfg.RealName, handler, ircCfg.Debug)
			go client.Start()
		}
	}()

	fmt.Fprintf(os.Stderr, "📡 Waiting for connections...\n")

	sig := <-stop
	if sig == syscall.SIGHUP {
		if cfg.Edge.ReloadMode == "sighup" {
			logger.InfoCtx(context.Background(), "SIGHUP received: safe restart is required for reload")
		} else {
			logger.InfoCtx(context.Background(), "SIGHUP received: reload_mode is restart-based")
		}
		return
	}
	fmt.Fprintf(os.Stderr, "\n🛑 Shutting down...\n")
	logger.InfoCtx(context.Background(), "shutting down")

	time.Sleep(1 * time.Second)
	fmt.Fprintf(os.Stderr, "✅ Shutdown complete\n")
}

func loadIRCConfig(cfg *config.Config, verboseFlag bool) *config.IRCAdapterConfig {
	irc := cfg.IRCAdapter

	if irc.ServerHost == "" {
		irc.ServerHost = "0.0.0.0"
	}
	if irc.ServerPort == 0 {
		irc.ServerPort = 6667
	}
	if irc.Nick == "" {
		irc.Nick = "ebusta-bot"
	}
	if irc.User == "" {
		irc.User = "ebusta"
	}
	if irc.RealName == "" {
		irc.RealName = "Ebusta Book Search Bot"
	}
	if irc.GatewayURL == "" {
		irc.GatewayURL = "http://localhost:8443"
	}
	if irc.PageSize <= 0 {
		irc.PageSize = 5
	}
	if verboseFlag {
		irc.Debug = true
	}
	return &irc
}
