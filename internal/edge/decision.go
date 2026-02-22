package edge

import (
	"context"
	"time"
)

type DecisionType string

const (
	DecisionAllow    DecisionType = "allow"
	DecisionDeny     DecisionType = "deny"
	DecisionThrottle DecisionType = "throttle"
)

type ReasonCode string

const (
	ReasonOK            ReasonCode = "ok"
	ReasonRateLimit     ReasonCode = "rate_limit"
	ReasonInputTooLong  ReasonCode = "input_too_long"
	ReasonBodyTooLarge  ReasonCode = "body_too_large"
	ReasonJSONTooDeep   ReasonCode = "json_too_deep"
	ReasonMalformedJSON ReasonCode = "malformed_json"
	ReasonInvalidToken  ReasonCode = "invalid_token"
	ReasonInvalidSHA1   ReasonCode = "invalid_sha1"
)

type Decision struct {
	Source     string
	Action     string
	Decision   DecisionType
	ReasonCode ReasonCode
	Allowed    bool
	Start      time.Time
	Duration   time.Duration
}

type Hook interface {
	OnDecision(ctx context.Context, d Decision)
}

type MultiHook struct {
	hooks []Hook
}

func NewMultiHook(hooks ...Hook) *MultiHook {
	return &MultiHook{hooks: hooks}
}

func (m *MultiHook) OnDecision(ctx context.Context, d Decision) {
	for _, h := range m.hooks {
		if h != nil {
			h.OnDecision(ctx, d)
		}
	}
}

type noopHook struct{}

func (noopHook) OnDecision(context.Context, Decision) {}

func NoopHook() Hook {
	return noopHook{}
}
