package edge

import (
	"context"
	"fmt"
	"github.com/prometheus/client_golang/prometheus"
	"sort"
	"strings"
	"sync"
)

var (
	registerPromOnce sync.Once

	edgeDecisionsTotal = prometheus.NewCounterVec(
		prometheus.CounterOpts{
			Name: "edge_decisions_total",
			Help: "Total number of edge decisions by source/action/decision/reason_code.",
		},
		[]string{"source", "action", "decision", "reason_code"},
	)

	edgeThrottleDroppedTotal = prometheus.NewCounterVec(
		prometheus.CounterOpts{
			Name: "edge_throttle_dropped_total",
			Help: "Total number of throttled edge requests by source/action/reason_code.",
		},
		[]string{"source", "action", "reason_code"},
	)
)

// LabelCounterHook stores Prometheus-style counters by safe low-cardinality labels.
type LabelCounterHook struct {
	mu       sync.Mutex
	counters map[string]uint64
}

func NewLabelCounterHook() *LabelCounterHook {
	registerPromOnce.Do(func() {
		prometheus.MustRegister(edgeDecisionsTotal, edgeThrottleDroppedTotal)
	})
	return &LabelCounterHook{
		counters: make(map[string]uint64),
	}
}

func (h *LabelCounterHook) OnDecision(_ context.Context, d Decision) {
	h.mu.Lock()
	defer h.mu.Unlock()

	key := fmt.Sprintf("edge_decisions_total|source=%s|action=%s|decision=%s|reason_code=%s",
		d.Source, d.Action, d.Decision, d.ReasonCode)
	h.counters[key]++
	edgeDecisionsTotal.WithLabelValues(d.Source, d.Action, string(d.Decision), string(d.ReasonCode)).Inc()

	if d.Decision == DecisionThrottle {
		dropKey := fmt.Sprintf("edge_throttle_dropped_total|source=%s|action=%s|reason_code=%s",
			d.Source, d.Action, d.ReasonCode)
		h.counters[dropKey]++
		edgeThrottleDroppedTotal.WithLabelValues(d.Source, d.Action, string(d.ReasonCode)).Inc()
	}
}

func (h *LabelCounterHook) Snapshot() map[string]uint64 {
	h.mu.Lock()
	defer h.mu.Unlock()
	out := make(map[string]uint64, len(h.counters))
	for k, v := range h.counters {
		out[k] = v
	}
	return out
}

func (h *LabelCounterHook) Keys() []string {
	h.mu.Lock()
	defer h.mu.Unlock()
	keys := make([]string, 0, len(h.counters))
	for k := range h.counters {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	return keys
}

// OTelHook is a generic callback hook for OpenTelemetry bridge.
// It does not import otel dependencies and keeps edge core dependency-free.
type OTelHook struct {
	Record func(ctx context.Context, attrs map[string]string)
}

func (h *OTelHook) OnDecision(ctx context.Context, d Decision) {
	if h == nil || h.Record == nil {
		return
	}
	h.Record(ctx, map[string]string{
		"edge.source":      d.Source,
		"edge.action":      d.Action,
		"edge.decision":    string(d.Decision),
		"edge.reason_code": string(d.ReasonCode),
	})
}

func IsSafeLabelKey(metricLine string) bool {
	// Defensive check used in tests: block obvious high-cardinality fields.
	blocked := []string{"trace_id", "user_id", "query", "token", "path_raw"}
	for _, b := range blocked {
		if strings.Contains(metricLine, b+"=") {
			return false
		}
	}
	return true
}
