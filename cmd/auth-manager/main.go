package main

import (
	"context"
	"log"
	"net"
	"os"

	"ebusta/api/proto/v1"
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
	log.Printf("[%s] Auth check: user=%s platform=%s", req.TraceId, req.UserId, req.Platform)

	for _, u := range s.whitelist.Users {
		if u.ID == req.UserId && u.Platform == req.Platform {
			return &libraryv1.AccessResponse{
				Allowed:  true,
				UserRole: u.Role,
			}, nil
		}
	}

	return &libraryv1.AccessResponse{
		Allowed: false,
		Reason:  "Access denied: user not in whitelist for this platform",
	}, nil
}

func main() {
	data, err := os.ReadFile("cmd/auth-manager/whitelist.yaml")
	if err != nil {
		log.Fatalf("Failed to read whitelist: %v", err)
	}

	var wl Whitelist
	if err := yaml.Unmarshal(data, &wl); err != nil {
		log.Fatalf("Failed to parse whitelist: %v", err)
	}

	lis, err := net.Listen("tcp", ":50055")
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	libraryv1.RegisterAuthServiceServer(s, &authServer{whitelist: wl})

	log.Println("🛡  Auth-Manager started on :50055")
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
