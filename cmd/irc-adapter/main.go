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

    "ebusta/internal/logger"
)

type Config struct {
    ServerHost string   `yaml:"server_host"`
    ServerPort int      `yaml:"server_port"`
    Nick       string   `yaml:"nick"`
    User       string   `yaml:"user"`
    RealName   string   `yaml:"real_name"`
    Channels   []string `yaml:"channels"`
    GatewayURL string   `yaml:"gateway_url"`
    PageSize   int      `yaml:"page_size"`
    Verbose    bool     `yaml:"verbose"`
}

func (c *Config) Address() string {
    return fmt.Sprintf("%s:%d", c.ServerHost, c.ServerPort)
}

func main() {
    var configPath string
    var verbose bool
    
    flag.StringVar(&configPath, "config", "ebusta.yaml", "path to config file")
    flag.BoolVar(&verbose, "verbose", false, "enable verbose output")
    flag.Parse()
    
    // Конфиг IRC
    ircCfg := &Config{
        ServerHost: "0.0.0.0",
        ServerPort: 6667,
        Nick:       "ebusta-bot",
        User:       "ebusta",
        RealName:   "Ebusta Book Search Bot",
        GatewayURL: "http://localhost:8443",
        PageSize:   5,
        Verbose:    verbose, // флаг имеет приоритет
    }
    
    // Настраиваем логгер на stdout
    logCfg := logger.DefaultConfig()
    if ircCfg.Verbose {
        logCfg.Level = "DEBUG"
    }
    logger.InitFromConfig(logCfg, "irc")
    
    fmt.Fprintf(os.Stderr, "🚀 IRC adapter starting on %s (verbose: %v)\n", 
        ircCfg.Address(), ircCfg.Verbose)
    
    // Создаём обработчик
    handler := NewIRCHandler(ircCfg.GatewayURL, ircCfg.PageSize, ircCfg.Verbose)
    
    // Запускаем сервер
    listener, err := net.Listen("tcp", ircCfg.Address())
    if err != nil {
        fmt.Fprintf(os.Stderr, "❌ Failed to listen: %v\n", err)
        logger.FatalCtx(context.Background(), "failed to listen", err)
    }
    defer listener.Close()
    
    fmt.Fprintf(os.Stderr, "✅ IRC adapter listening on %s\n", ircCfg.Address())
    logger.InfoCtx(context.Background(), "IRC adapter started")
    
    // Канал для graceful shutdown
    stop := make(chan os.Signal, 1)
    signal.Notify(stop, syscall.SIGINT, syscall.SIGTERM)
    
    // Принимаем соединения
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
            
            // Запускаем клиента
            client := NewIRCClient(conn, ircCfg.Nick, ircCfg.User, ircCfg.RealName, handler, ircCfg.Verbose)
            go client.Start()
        }
    }()
    
    fmt.Fprintf(os.Stderr, "📡 Waiting for connections...\n")
    
    // Ожидаем сигнал
    <-stop
    fmt.Fprintf(os.Stderr, "\n🛑 Shutting down...\n")
    logger.InfoCtx(context.Background(), "shutting down")
    
    // Даём клиентам время завершиться
    time.Sleep(1 * time.Second)
    fmt.Fprintf(os.Stderr, "✅ Shutdown complete\n")
}
