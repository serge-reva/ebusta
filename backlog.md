
## DSL Parser Metrics Integration
- [ ] Интегрировать `cl-prometheus` и `hunchentoot` в `dsl-service.lisp`.
- [ ] Реализовать HTTP-эндпоинт `/metrics` на порту `50053`.
- [ ] Добавить счетчики `dsl_requests_total` и `dsl_errors_total`.
- [ ] Добавить гистограмму `dsl_parse_duration_seconds` (бакеты от 0.1ms до 10ms).
- [ ] Добавить Gauge для мониторинга SBCL heap и активных воркеров.
- [ ] Обеспечить изоляцию: сбой сервера метрик не должен аффектить gRPC.
