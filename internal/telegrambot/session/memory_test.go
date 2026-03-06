package session

import (
	"context"
	"strconv"
	"sync"
	"testing"
)

func TestMemoryStoreCRUD(t *testing.T) {
	store := NewMemoryStore()
	ctx := context.Background()

	if _, ok := store.Get(ctx, "u1"); ok {
		t.Fatal("expected empty store")
	}

	err := store.Put(ctx, &Session{UserID: "u1", Query: "tolstoy", PageSize: 5, CurrentPage: 1})
	if err != nil {
		t.Fatalf("Put() error = %v", err)
	}

	got, ok := store.Get(ctx, "u1")
	if !ok {
		t.Fatal("expected stored session")
	}
	if got.Query != "tolstoy" || got.PageSize != 5 {
		t.Fatalf("unexpected session: %+v", got)
	}

	if err := store.Delete(ctx, "u1"); err != nil {
		t.Fatalf("Delete() error = %v", err)
	}
	if _, ok := store.Get(ctx, "u1"); ok {
		t.Fatal("expected deleted session")
	}
}

func TestMemoryStoreConcurrentAccess(t *testing.T) {
	store := NewMemoryStore()
	ctx := context.Background()
	var wg sync.WaitGroup

	for i := 0; i < 50; i++ {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			userID := "u" + strconv.Itoa(i)
			_ = store.Put(ctx, &Session{UserID: userID, Query: "q", PageSize: 5, CurrentPage: 1})
			if _, ok := store.Get(ctx, userID); !ok {
				t.Errorf("expected session for %s", userID)
			}
		}(i)
	}
	wg.Wait()
}
