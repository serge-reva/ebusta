package main

import (
    "context"
    "net"
    "os"

    "ebusta/api/proto/v1"
    "ebusta/internal/config"
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
    logger.GetGlobal().WithField("user", req.UserId).
        WithField("platform", req.Platform).
        WithField("trace_id", req.TraceId).
        InfoCtx(ctx, "[auth] checking access")

    for _, u := range s.whitelist.Users {
        if u.ID == req.UserId && u.Platform == req.Platform {
            logger.GetGlobal().WithField("user", req.UserId).
                WithField("role", u.Role).
                InfoCtx(ctx, "[auth] access granted")
            return &libraryv1.AccessResponse{
                Allowed:  true,
                UserRole: u.Role,
            }, nil
        }
    }

    logger.GetGlobal().WithField("user", req.UserId).
        WithField("platform", req.Platform).
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
    if err := s.Serve(lis); err != nil {
        logger.GetGlobal().FatalCtx(context.Background(), "failed to serve", err)
    }
}
