package main

import (
	"context"
	"crypto/tls"
	"flag"
	"fmt"
	"net"
	"os"
	"os/signal"
	"strings"
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

	policy := edge.PolicyFromConfig(cfg, "irc")
	handler := NewIRCHandlerWithPolicy(ircCfg.GatewayURL, ircCfg.PageSize, ircCfg.Debug, policy, edge.NewLabelCounterHook())
	handler.SetDCCConfig(ircCfg.DCCEnabled, ircCfg.DCCPublicIP, ircCfg.DCCPortMin, ircCfg.DCCPortMax, ircCfg.DCCTimeoutSec)

	stop := make(chan os.Signal, 1)
	signal.Notify(stop, syscall.SIGINT, syscall.SIGTERM, syscall.SIGHUP)

	mode := strings.ToLower(strings.TrimSpace(ircCfg.Mode))
	if mode == "" {
		mode = "server"
	}

	switch mode {
	case "bot":
		runBotMode(cfg, ircCfg, handler, stop)
	default:
		runServerMode(cfg, ircCfg, handler, stop)
	}
}

func runServerMode(cfg *config.Config, ircCfg *config.IRCAdapterConfig, handler *IRCHandler, stop <-chan os.Signal) {
	fmt.Fprintf(os.Stderr, "🚀 IRC adapter starting in server mode on %s (verbose: %v)\n", ircCfg.ServerAddress(), ircCfg.Debug)

	listener, err := net.Listen("tcp", ircCfg.ServerAddress())
	if err != nil {
		fmt.Fprintf(os.Stderr, "❌ Failed to listen: %v\n", err)
		logger.FatalCtx(context.Background(), "failed to listen", err)
	}
	defer listener.Close()

	fmt.Fprintf(os.Stderr, "✅ IRC adapter listening on %s\n", ircCfg.ServerAddress())
	logger.InfoCtx(context.Background(), "IRC adapter started in server mode")

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
	handleShutdownSignal(cfg, <-stop)
}

func runBotMode(cfg *config.Config, ircCfg *config.IRCAdapterConfig, handler *IRCHandler, stop <-chan os.Signal) {
	fmt.Fprintf(os.Stderr, "🚀 IRC adapter starting in bot mode -> %s (verbose: %v)\n", ircCfg.BotAddress(), ircCfg.Debug)
	logger.GetGlobal().WithField("mode", "bot").WithField("server", ircCfg.BotAddress()).InfoCtx(context.Background(), "IRC adapter started")

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := make(chan struct{})

	go func() {
		defer close(done)
		for {
			select {
			case <-ctx.Done():
				return
			default:
			}

			conn, err := dialIRCBot(ircCfg)
			if err != nil {
				logger.GetGlobal().WithField("server", ircCfg.BotAddress()).ErrorCtx(context.Background(), "bot dial failed", err)
				if !sleepOrDone(ctx, time.Duration(ircCfg.BotReconnectSeconds)*time.Second) {
					return
				}
				continue
			}

			client := NewIRCBotClient(conn, ircCfg.BotNick, ircCfg.BotUser, ircCfg.BotRealName, ircCfg.BotPassword, handler, ircCfg.Debug)
			err = client.StartBot(ctx, ircCfg.BotChannels)
			if err != nil && ctx.Err() == nil {
				logger.GetGlobal().WithField("server", ircCfg.BotAddress()).ErrorCtx(context.Background(), "bot disconnected", err)
			}
			if !sleepOrDone(ctx, time.Duration(ircCfg.BotReconnectSeconds)*time.Second) {
				return
			}
		}
	}()

	sig := <-stop
	handleShutdownSignal(cfg, sig)
	cancel()
	<-done
}

func dialIRCBot(ircCfg *config.IRCAdapterConfig) (net.Conn, error) {
	dialer := &net.Dialer{Timeout: 10 * time.Second}
	if ircCfg.BotUseTLS {
		return tls.DialWithDialer(dialer, "tcp", ircCfg.BotAddress(), &tls.Config{MinVersion: tls.VersionTLS12})
	}
	return dialer.Dial("tcp", ircCfg.BotAddress())
}

func sleepOrDone(ctx context.Context, d time.Duration) bool {
	if d <= 0 {
		d = 1 * time.Second
	}
	t := time.NewTimer(d)
	defer t.Stop()
	select {
	case <-ctx.Done():
		return false
	case <-t.C:
		return true
	}
}

func handleShutdownSignal(cfg *config.Config, sig os.Signal) {
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

	if irc.Mode == "" {
		irc.Mode = "server"
	}

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

	if irc.BotServerHost == "" {
		irc.BotServerHost = "127.0.0.1"
	}
	if irc.BotServerPort == 0 {
		irc.BotServerPort = 6667
	}
	if irc.BotNick == "" {
		irc.BotNick = irc.Nick
	}
	if irc.BotUser == "" {
		irc.BotUser = irc.User
	}
	if irc.BotRealName == "" {
		irc.BotRealName = irc.RealName
	}
	if len(irc.BotChannels) == 0 && len(irc.Channels) > 0 {
		irc.BotChannels = append([]string(nil), irc.Channels...)
	}
	if irc.BotReconnectSeconds <= 0 {
		irc.BotReconnectSeconds = 5
	}
	if irc.DCCPortMin <= 0 {
		irc.DCCPortMin = 40000
	}
	if irc.DCCPortMax <= 0 {
		irc.DCCPortMax = 40100
	}
	if irc.DCCPortMax < irc.DCCPortMin {
		irc.DCCPortMax = irc.DCCPortMin
	}
	if irc.DCCTimeoutSec <= 0 {
		irc.DCCTimeoutSec = 180
	}

	if verboseFlag {
		irc.Debug = true
	}
	return &irc
}
