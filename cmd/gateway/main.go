package main

import (
    "context"
    "flag"
    "fmt"
    "os"
    "os/signal"
    "syscall"
    "time"

    "ebusta/internal/config"
    "ebusta/internal/gateway"
    "ebusta/internal/logger"
)

func main() {
    var configPath string
    flag.StringVar(&configPath, "config", "ebusta.yaml", "path to config file")
    flag.Parse()
    
    // Загружаем конфигурацию
    os.Setenv("EBUSTA_CONFIG", configPath)
    cfg := config.Get()
    
    // Инициализируем логгер
    logger.InitFromConfig(cfg.Logger, "gateway")
    
    // Загружаем конфиг gateway
    gatewayCfg := gateway.LoadConfig(cfg)
    
    // Создаём сервер
    server, err := gateway.NewServer(gatewayCfg)
    if err != nil {
        logger.FatalCtx(context.Background(), "failed to create gateway server", err)
    }
    
    // Канал для graceful shutdown
    stop := make(chan os.Signal, 1)
    signal.Notify(stop, syscall.SIGINT, syscall.SIGTERM)
    
    // Запуск сервера в горутине
    go func() {
        if err := server.Run(); err != nil {
            logger.FatalCtx(context.Background(), "gateway server failed", err)
        }
    }()
    
    logger.InfoCtx(context.Background(), "gateway started",
        "port", gatewayCfg.Port)
    
    // Ожидание сигнала
    sig := <-stop
    logger.InfoCtx(context.Background(), "shutting down", "signal", sig)
    
    // Graceful shutdown
    ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
    defer cancel()
    
    if err := server.Shutdown(ctx); err != nil {
        logger.ErrorCtx(context.Background(), "shutdown error", err)
    }
    
    logger.InfoCtx(context.Background(), "gateway stopped")
}
