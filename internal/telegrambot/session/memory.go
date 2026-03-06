package session

import (
	"context"
	"sync"
	"time"
)

type MemoryStore struct {
	mu       sync.RWMutex
	sessions map[string]*Session
}

func NewMemoryStore() *MemoryStore {
	return &MemoryStore{
		sessions: make(map[string]*Session),
	}
}

func (s *MemoryStore) Get(_ context.Context, userID string) (*Session, bool) {
	s.mu.RLock()
	defer s.mu.RUnlock()
	session, ok := s.sessions[userID]
	if !ok {
		return nil, false
	}
	copy := *session
	return &copy, true
}

func (s *MemoryStore) Put(_ context.Context, session *Session) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	copy := *session
	if copy.UpdatedAt.IsZero() {
		copy.UpdatedAt = time.Now()
	}
	s.sessions[copy.UserID] = &copy
	return nil
}

func (s *MemoryStore) Delete(_ context.Context, userID string) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	delete(s.sessions, userID)
	return nil
}
