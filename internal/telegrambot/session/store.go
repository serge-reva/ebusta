package session

import "context"

type Store interface {
	Get(ctx context.Context, userID string) (*Session, bool)
	Put(ctx context.Context, s *Session) error
	Delete(ctx context.Context, userID string) error
}
