package main

import (
    "context"
    "net"
    "os"
    "os/signal"
    "sync"
    "syscall"
    "time"

    "ebusta/api/proto/v1"
    "ebusta/internal/config"
    "ebusta/internal/errutil"
    "ebusta/internal/logger"

    "google.golang.org/grpc"
    "gopkg.in/yaml.v3"
)

type UserEntry struct {
    ID       string `yaml:"id"`
    Platform string `yaml:"platform"`
    Role     string `yaml:"role"`
}

type Whitelist struct {
    Users []UserEntry `yaml:"users"`
}

type authServer struct {
    libraryv1.UnimplementedAuthServiceServer
    whitelist Whitelist
}

func (s *authServer) CheckAccess(ctx context.Context, req *libraryv1.AccessRequest) (*libraryv1.AccessResponse, error) {
    traceID := errutil.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = req.TraceId
    }
    if traceID == "" {
        traceID = errutil.GenerateTraceID("auth")
    }

    logger.GetGlobal().WithField("user", req.UserId).
        WithField("platform", req.Platform).
        WithField("trace_id", traceID).
        InfoCtx(ctx, "[auth] checking access")

    for _, u := range s.whitelist.Users {
        if u.ID == req.UserId && u.Platform == req.Platform {
            logger.GetGlobal().WithField("user", req.UserId).
                WithField("role", u.Role).
                WithField("trace_id", traceID).
                InfoCtx(ctx, "[auth] access granted")
            return &libraryv1.AccessResponse{
                Allowed:  true,
                UserRole: u.Role,
            }, nil
        }
    }

    logger.GetGlobal().WithField("user", req.UserId).
        WithField("platform", req.Platform).
        WithField("trace_id", traceID).
        WarnCtx(ctx, "[auth] access denied")
    return &libraryv1.AccessResponse{
        Allowed: false,
        Reason:  "Access denied: user not in whitelist for this platform",
    }, nil
}

func main() {
    logger.InitFromConfig(config.Get().Logger, "auth")

    data, err := os.ReadFile("cmd/auth-manager/whitelist.yaml")
    if err != nil {
        logger.GetGlobal().FatalCtx(context.Background(), "failed to read whitelist", err)
    }

    var wl Whitelist
    if err := yaml.Unmarshal(data, &wl); err != nil {
        logger.GetGlobal().FatalCtx(context.Background(), "failed to parse whitelist", err)
    }

    lis, err := net.Listen("tcp", ":50055")
    if err != nil {
        logger.GetGlobal().FatalCtx(context.Background(), "failed to listen", err)
    }

    s := grpc.NewServer()
    libraryv1.RegisterAuthServiceServer(s, &authServer{whitelist: wl})

    logger.GetGlobal().WithField("addr", ":50055").InfoCtx(context.Background(), "[auth] started")
    serveErr := make(chan error, 1)
    var wg sync.WaitGroup
    wg.Add(1)
    go func() {
        defer wg.Done()
        if err := s.Serve(lis); err != nil {
            serveErr <- err
        }
    }()

    stop := make(chan os.Signal, 1)
    signal.Notify(stop, os.Interrupt, syscall.SIGTERM)

    select {
    case sig := <-stop:
        logger.GetGlobal().WithField("signal", sig).InfoCtx(context.Background(), "[auth] shutting down")
        done := make(chan struct{})
        go func() {
            s.GracefulStop()
            close(done)
        }()
        select {
        case <-done:
        case <-time.After(10 * time.Second):
            logger.GetGlobal().WarnCtx(context.Background(), "[auth] graceful stop timeout, forcing stop")
            s.Stop()
        }
    case err := <-serveErr:
        logger.GetGlobal().FatalCtx(context.Background(), "failed to serve", err)
    }

    wg.Wait()
    logger.GetGlobal().InfoCtx(context.Background(), "[auth] stopped")
}
