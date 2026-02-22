package edge

import (
	"testing"
	"time"
)

func TestThrottlerAllowByActionAndKey(t *testing.T) {
	th := NewThrottler()
	th.Register("cmd", ThrottleRule{
		PerMinute: 2,
		Burst:     2,
		KeyTTL:    time.Second,
	})

	if !th.Allow("cmd", "u1") {
		t.Fatalf("first allow should pass")
	}
	if !th.Allow("cmd", "u1") {
		t.Fatalf("second allow should pass")
	}
	if th.Allow("cmd", "u1") {
		t.Fatalf("third allow should be denied")
	}
	if !th.Allow("cmd", "u2") {
		t.Fatalf("different key should have separate bucket")
	}
}

func TestThrottlerUnknownActionPasses(t *testing.T) {
	th := NewThrottler()
	if !th.Allow("unknown", "u1") {
		t.Fatalf("unknown action should pass")
	}
}

func TestThrottlerKeyExpiryResets(t *testing.T) {
	th := NewThrottler()
	th.Register("cmd", ThrottleRule{
		PerMinute: 1,
		Burst:     1,
		KeyTTL:    30 * time.Millisecond,
	})

	if !th.Allow("cmd", "u1") {
		t.Fatalf("first allow should pass")
	}
	if th.Allow("cmd", "u1") {
		t.Fatalf("second allow should be denied before TTL")
	}
	time.Sleep(35 * time.Millisecond)
	if !th.Allow("cmd", "u1") {
		t.Fatalf("allow should pass after key TTL cleanup")
	}
}
