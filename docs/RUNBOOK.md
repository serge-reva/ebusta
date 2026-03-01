# RUNBOOK.md

## Быстрый старт (HOST)

```bash
# Установка зависимостей (Go, Scala, protoc)
make deps

# Генерация proto и сборка
make proto
make build

# Запуск всех сервисов
make up

# Проверка работоспособности (end-to-end тесты на хосте)
make e2e-host

# Остановка
make down

Быстрый старт (DOCKER runtime)
bash

# Сборка образов (без компиляции внутри контейнеров)
make docker-build

# Запуск контейнеров
make docker-up

# Проверка в Docker
make e2e-docker

# Остановка
make docker-down

Порты сервисов (по умолчанию из ebusta.yaml)
Сервис	Порт (host)	Протокол
datamanager	50051	gRPC
dsl-scala	50052	gRPC
query-builder	50053	gRPC
orchestrator	50054	gRPC
gateway	8443	HTTPS
downloader	50081	HTTP
web-frontend	3000	HTTP
archive-node	50110	gRPC
tier-node	50111	gRPC
plasma-node	50112	gRPC
irc-adapter	6667	TCP
telegram-adapter	8087	HTTP
Логи

    Все сервисы пишут логи в ./logs/ (при запуске через make up).

    Формат логов настраивается в ebusta.yaml (секция logger).

Диагностика

    Проверить, что все gRPC-сервисы отвечают:
    bash

    grpcurl -plaintext localhost:50051 list
    grpcurl -plaintext localhost:50052 list
    ...

    Проверить health gateway: curl http://localhost:8443/health
