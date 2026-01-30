# Backlog

- [ ] Ports: canonicalize ports for all components (datamanager/dsl/orchestrator/processor/web/cli/diagnose/tests).
      Remove conflicting flags/envs, document canonical table, add tests for each port.

- [ ] Metrics: expose Prometheus /metrics for other components (datamanager/processor/web-adapter/DSL/cli/diagnose). Use a consistent port scheme (50091+) and add smoke checks.
