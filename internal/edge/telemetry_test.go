package edge

import (
	"context"
	"testing"
)

func TestLabelCounterHookMetrics(t *testing.T) {
	h := NewLabelCounterHook()
	h.OnDecision(context.Background(), Decision{
		Source:     "gateway",
		Action:     "search",
		Decision:   DecisionAllow,
		ReasonCode: ReasonOK,
	})
	h.OnDecision(context.Background(), Decision{
		Source:     "gateway",
		Action:     "search",
		Decision:   DecisionThrottle,
		ReasonCode: ReasonRateLimit,
	})

	snap := h.Snapshot()
	if len(snap) < 2 {
		t.Fatalf("expected multiple counters, got %d", len(snap))
	}
	for k := range snap {
		if !IsSafeLabelKey(k) {
			t.Fatalf("unsafe metrics key: %s", k)
		}
	}
}

func TestOTelHookCallback(t *testing.T) {
	called := false
	h := &OTelHook{
		Record: func(_ context.Context, attrs map[string]string) {
			called = true
			if attrs["edge.source"] != "irc" {
				t.Fatalf("unexpected source attr: %s", attrs["edge.source"])
			}
		},
	}
	h.OnDecision(context.Background(), Decision{
		Source:     "irc",
		Action:     "command",
		Decision:   DecisionDeny,
		ReasonCode: ReasonInputTooLong,
	})
	if !called {
		t.Fatalf("expected otel callback to be called")
	}
}
