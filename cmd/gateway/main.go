package main

import (
    "context"
    "flag"
    "os"
    "os/signal"
    "syscall"
    "time"

    "ebusta/internal/config"
    gatewaycfg "ebusta/internal/gateway/config"
    "ebusta/internal/gateway"
    "ebusta/internal/logger"
)

func main() {
    var configPath string
    flag.StringVar(&configPath, "config", "ebusta.yaml", "path to config file")
    flag.Parse()
    
    os.Setenv("EBUSTA_CONFIG", configPath)
    cfg := config.Get()
    
    logger.InitFromConfig(cfg.Logger, "gateway")
    
    gatewayCfg := gatewaycfg.LoadFromMainConfig(cfg)
    
    server, err := gateway.NewServer(gatewayCfg)
    if err != nil {
        logger.FatalCtx(context.Background(), "failed to create gateway server", err)
    }
    
    stop := make(chan os.Signal, 1)
    signal.Notify(stop, syscall.SIGINT, syscall.SIGTERM)
    
    go func() {
        if err := server.Run(); err != nil {
            logger.FatalCtx(context.Background(), "gateway server failed", err)
        }
    }()
    
    logger.GetGlobal().WithField("port", gatewayCfg.Port).InfoCtx(context.Background(), "gateway started")
    
    sig := <-stop
    logger.GetGlobal().WithField("signal", sig).InfoCtx(context.Background(), "shutting down")
    
    ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
    defer cancel()
    
    if err := server.Shutdown(ctx); err != nil {
        logger.GetGlobal().ErrorCtx(context.Background(), "shutdown error", err)
    }
    
    logger.GetGlobal().InfoCtx(context.Background(), "gateway stopped")
}
