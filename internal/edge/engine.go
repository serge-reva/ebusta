package edge

import (
	"context"
	"strings"
	"time"
)

type Engine struct {
	source    string
	policy    Policy
	throttler *Throttler
	hook      Hook
}

func NewEngine(policy Policy, hook Hook) *Engine {
	if hook == nil {
		hook = NoopHook()
	}
	th := NewThrottler()
	for action, ap := range policy.Actions {
		th.Register(action, ThrottleRule{
			PerMinute: ap.PerMinute,
			Burst:     ap.Burst,
		})
	}
	return &Engine{
		source:    policy.Source,
		policy:    policy,
		throttler: th,
		hook:      hook,
	}
}

func (e *Engine) Policy() Policy {
	return e.policy
}

func (e *Engine) Allow(ctx context.Context, action, key string) bool {
	start := time.Now()
	ok := e.throttler.Allow(action, key)
	d := Decision{
		Source:     e.source,
		Action:     action,
		Allowed:    ok,
		ReasonCode: ReasonOK,
		Decision:   DecisionAllow,
		Start:      start,
		Duration:   time.Since(start),
	}
	if !ok {
		d.Decision = DecisionThrottle
		d.ReasonCode = ReasonRateLimit
	}
	e.hook.OnDecision(ctx, d)
	return ok
}

func (e *Engine) ValidateLine(ctx context.Context, action, line string) error {
	start := time.Now()
	err := ValidateLineLength(line, e.policy.MaxLineLength)
	d := Decision{
		Source:     e.source,
		Action:     action,
		Allowed:    err == nil,
		ReasonCode: ReasonOK,
		Decision:   DecisionAllow,
		Start:      start,
		Duration:   time.Since(start),
	}
	if err != nil {
		d.Decision = DecisionDeny
		d.ReasonCode = ReasonInputTooLong
	}
	e.hook.OnDecision(ctx, d)
	return err
}

func (e *Engine) ValidateJSON(ctx context.Context, action string, body []byte) error {
	start := time.Now()
	err := ValidateJSONPayload(body, e.policy.MaxBodyBytes, e.policy.MaxJSONDepth)
	d := Decision{
		Source:     e.source,
		Action:     action,
		Allowed:    err == nil,
		ReasonCode: ReasonOK,
		Decision:   DecisionAllow,
		Start:      start,
		Duration:   time.Since(start),
	}
	if err != nil {
		d.Decision = DecisionDeny
		switch {
		case strings.Contains(err.Error(), "body too large"):
			d.ReasonCode = ReasonBodyTooLarge
		case strings.Contains(err.Error(), "JSON too deep"):
			d.ReasonCode = ReasonJSONTooDeep
		default:
			d.ReasonCode = ReasonMalformedJSON
		}
	}
	e.hook.OnDecision(ctx, d)
	return err
}
