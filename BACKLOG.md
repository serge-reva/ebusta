# Backlog

- [ ] Ports: canonicalize ports for all components (datamanager/dsl/orchestrator/processor/web/cli/diagnose/tests).
      Remove conflicting flags/envs, document canonical table, add tests for each port.

- [ ] Metrics: expose Prometheus /metrics for other components (datamanager/processor/web-adapter/DSL). Use dedicated ports (50091+) or a consistent scheme; document it.

- [ ] Metrics: expose Prometheus metrics in other components (datamanager/processor/web-adapter/DSL) using canonical ports (e.g., 50090+). Document scheme and add smoke checks.
