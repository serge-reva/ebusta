# PROTO_IMMUTABILITY.md

## Цель
Поддерживать неизменяемость wire-контрактов и детерминированную генерацию кода из `api/proto/v1/*.proto`.

## Правила изменения proto
- Не изменять существующие номера полей.
- Не изменять типы существующих полей.
- Не удалять поля напрямую; использовать `reserved`.
- Новые поля добавлять только с новыми номерами.
- Ломающие изменения вносить только в новый пакет версии (например, `libraryv2`).

## Команды
- `make proto-generate`
  - Генерирует Go-код из всех proto-файлов в `api/proto/v1/`.
  - Проверяет наличие `protoc`, `protoc-gen-go`, `protoc-gen-go-grpc`.
- `make proto-lint`
  - `buf lint`.
- `make proto-breaking`
  - Проверка обратной совместимости: `buf breaking --against '.git#branch=main'`.
- `make proto-verify`
  - Запускает `proto-generate`.
  - Проверяет, что после генерации нет изменений в `*.pb.go` и `*_grpc.pb.go`.
  - Запускает `proto-lint` и `proto-breaking`.

## CI
- `make ci-check` включает `proto-verify`.
- Если `proto-verify` падает по diff, нужно выполнить `make proto-generate` и закоммитить обновлённые generated-файлы.

## Требуемые утилиты
- `protoc`
- `protoc-gen-go`
- `protoc-gen-go-grpc`
- `buf`

Пример установки генераторов:
```bash
go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
```
