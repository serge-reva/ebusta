# Ebusta Project Backlog

## Ingesting (f2bulker)
- [ ] **Issue #1**: Ошибка парсинга UTF-16 (BOM ÿþ). Файл `547782.fb2` падает с `XML syntax error: invalid UTF-8`. Необходимо доработать `charsetReader` для корректной десериализации UTF-16 Little Endian. [cite: 440-442]
- [ ] **Feature**: Поддержка группировки в DSL (скобки). [cite: 141]

## System
- [ ] **Auth**: Интеграция Auth-Manager в Orchestrator. [cite: 219-220]
- [ ] **OS**: Переход с мока `books.json` на реальные поисковые шаблоны OpenSearch. [cite: 221]
