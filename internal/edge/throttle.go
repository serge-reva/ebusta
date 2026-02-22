package edge

import (
	"sync"
	"time"

	"golang.org/x/time/rate"
)

// ThrottleRule configures one action limiter.
type ThrottleRule struct {
	PerMinute int
	Burst     int
	KeyTTL    time.Duration
}

type keyLimiter struct {
	limiter  *rate.Limiter
	expires  time.Time
	lastSeen time.Time
}

type actionLimiter struct {
	rule ThrottleRule
	keys map[string]*keyLimiter
}

// Throttler keeps independent limiter buckets per action and key.
type Throttler struct {
	mu      sync.Mutex
	actions map[string]*actionLimiter
}

func NewThrottler() *Throttler {
	return &Throttler{
		actions: make(map[string]*actionLimiter),
	}
}

func (t *Throttler) Register(action string, rule ThrottleRule) {
	if action == "" {
		return
	}
	if rule.KeyTTL <= 0 {
		rule.KeyTTL = time.Hour
	}
	t.mu.Lock()
	defer t.mu.Unlock()
	t.actions[action] = &actionLimiter{
		rule: rule,
		keys: make(map[string]*keyLimiter),
	}
}

func (t *Throttler) Allow(action, key string) bool {
	if action == "" || key == "" {
		return false
	}

	t.mu.Lock()
	defer t.mu.Unlock()

	al, ok := t.actions[action]
	if !ok {
		return true
	}
	if al.rule.PerMinute <= 0 {
		return true
	}

	now := time.Now()
	for k, v := range al.keys {
		if now.After(v.expires) {
			delete(al.keys, k)
		}
	}

	kl, ok := al.keys[key]
	if !ok {
		burst := al.rule.Burst
		if burst <= 0 {
			burst = al.rule.PerMinute
		}
		kl = &keyLimiter{
			limiter:  rate.NewLimiter(rate.Limit(al.rule.PerMinute)/60, burst),
			expires:  now.Add(al.rule.KeyTTL),
			lastSeen: now,
		}
		al.keys[key] = kl
	} else {
		kl.expires = now.Add(al.rule.KeyTTL)
		kl.lastSeen = now
	}

	return kl.limiter.Allow()
}
