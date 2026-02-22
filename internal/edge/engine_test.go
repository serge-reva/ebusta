package edge

import (
	"context"
	"testing"
)

func TestEngineThrottleAndDecisionLabels(t *testing.T) {
	hook := NewLabelCounterHook()
	p := DefaultPolicy("irc")
	p.Actions["command"] = ActionPolicy{PerMinute: 1, Burst: 1}
	e := NewEngine(p, hook)

	if !e.Allow(context.Background(), "command", "u1") {
		t.Fatalf("first allow should pass")
	}
	if e.Allow(context.Background(), "command", "u1") {
		t.Fatalf("second allow should throttle")
	}

	keys := hook.Keys()
	if len(keys) == 0 {
		t.Fatalf("expected decision metrics")
	}
	for _, k := range keys {
		if !IsSafeLabelKey(k) {
			t.Fatalf("found unsafe label key: %s", k)
		}
	}
}

func TestEngineValidateLineAndJSON(t *testing.T) {
	hook := NewLabelCounterHook()
	p := DefaultPolicy("gateway")
	p.MaxLineLength = 5
	p.MaxBodyBytes = 30
	p.MaxJSONDepth = 2
	e := NewEngine(p, hook)

	if err := e.ValidateLine(context.Background(), "command", "abc"); err != nil {
		t.Fatalf("unexpected line error: %v", err)
	}
	if err := e.ValidateLine(context.Background(), "command", "123456"); err == nil {
		t.Fatalf("expected line length error")
	}

	if err := e.ValidateJSON(context.Background(), "search", []byte(`{"a":1}`)); err != nil {
		t.Fatalf("unexpected json error: %v", err)
	}
	if err := e.ValidateJSON(context.Background(), "search", []byte(`{"a":{"b":{"c":1}}}`)); err == nil {
		t.Fatalf("expected depth error")
	}

	if err := e.ValidateJSON(context.Background(), "search", []byte(`{"a":`)); err == nil {
		t.Fatalf("expected malformed json error")
	}

	snap := hook.Snapshot()
	if snap["edge_decisions_total|source=gateway|action=command|decision=deny|reason_code=input_too_long"] == 0 {
		t.Fatalf("expected deny metric for input_too_long")
	}
	if snap["edge_decisions_total|source=gateway|action=search|decision=deny|reason_code=json_too_deep"] == 0 {
		t.Fatalf("expected deny metric for json_too_deep")
	}
	if snap["edge_decisions_total|source=gateway|action=search|decision=deny|reason_code=malformed_json"] == 0 {
		t.Fatalf("expected deny metric for malformed_json")
	}
}
