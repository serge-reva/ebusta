# Logger Module for ebusta

Интеграция с `internal/config`.

## Установка

```bash
cp -r logger /path/to/ebusta/internal/logger
```

## Интеграция с internal/config

### 1. Добавить в internal/config/config.go

```go
type LoggerConfig struct {
        Level         string        `yaml:"level"`
        Format        string        `yaml:"format"`
        DisableColors bool          `yaml:"disable_colors"`
        Outputs       OutputsConfig `yaml:"outputs"`
}

type OutputsConfig struct {
        Console ConsoleConfig    `yaml:"console"`
        File    FileOutputConfig `yaml:"file"`
}

type ConsoleConfig struct {
        Enabled   bool `yaml:"enabled"`
        UseStderr bool `yaml:"use_stderr"`
}

type FileOutputConfig struct {
        Enabled bool   `yaml:"enabled"`
        Path    string `yaml:"path"`
        Append  bool   `yaml:"append"`
}

// Добавить в type Config struct { ... }
type Config struct {
        // ... existing fields ...
        Logger LoggerConfig `yaml:"logger"`
}
```

### 2. Добавить в ebusta.yaml

```yaml
logger:
  level: INFO
  format: text
  outputs:
    console:
      enabled: true
    file:
      enabled: true
      path: /var/log/ebusta/app.log
      append: true
```

### 3. Использование

```go
import (
        "ebusta/internal/config"
        "ebusta/internal/logger"
)

func main() {
        cfg := config.Get()
        log := logger.InitFromConfig(cfg.Logger, "my-component")

        log.Info("req-123", "Application started")
}
```

## API

```go
// Уровни
log.Debug(traceID, "message")
log.Info(traceID, "message")
log.Warn(traceID, "message")
log.Error(traceID, "message", err)
log.Fatal(traceID, "message", err)  // os.Exit(1)

// Форматирование
log.Infof(traceID, "found %d items", count)

// С контекстом
log.InfoCtx(ctx, "message")

// С компонентом
log := logger.Default().WithComponent("plasma")
log.Info(traceID, "HIT GetMeta")

// Package-level
logger.Info("req-123", "message")
logger.Componentf("plasma", traceID, "HIT sha1=%s", sha1)
```

## Форматы

**Text:** `2024-02-15 12:00:00 [INFO] [req-123] [plasma] HIT GetMeta`

**JSON:** `{"time":"...","level":"INFO","trace_id":"req-123","component":"plasma","message":"HIT GetMeta"}`

## Миграция

```go
// Было:
log.Printf("[%s] [plasma] HIT sha1=%s", traceID, sha1)

// Стало:
logger.Componentf("plasma", traceID, "HIT sha1=%s", sha1)
```

## Тестирование

### Unit-тесты

```bash
go test -v ./...
```

### Функциональные тесты

Функциональные тесты находятся в файле `logger_functional_test.go` и покрывают следующие сценарии:

#### Сценарий 1: Жизненный цикл логгера
- Создание логгера по умолчанию
- Создание через builder pattern
- Изменение уровня логирования во время работы
- Закрытие логгера

#### Сценарий 2: Вывод в различные目的地
- Stdout / Stderr
- Файловый вывод (с перезаписью и добавлением)
- Multi-output (одновременный вывод в несколько мест)
- Динамическое добавление output

#### Сценарий 3: Форматтеры
- TextFormatter (с цветами и без)
- JSONFormatter
- Обработка ошибок

#### Сценарий 4: Контекст и TraceID
- Сохранение TraceID в context.Context
- Извлечение из HTTP заголовков
- Генерация уникальных ID
- Контекстно-зависимое логирование

#### Сценарий 5: HTTP Middleware
- Установка TraceID из заголовка
- Генерация TraceID при отсутствии
- Полный жизненный цикл HTTP запроса

#### Сценарий 6: Конфигурация
- Загрузка из LoggerConfig
- Text vs JSON формат
- Stderr vs Stdout
- Файловый вывод из конфигурации
- Слияние с настройками по умолчанию

#### Сценарий 7: Конкурентность
- Параллельное логирование (100 goroutines × 10 сообщений)
- Параллельное изменение уровня
- Параллельное использование WithField

#### Сценарий 8: Глобальный логгер
- Package-level функции
- Component helpers
- Legacy совместимость (Printf, Print)

#### Сценарий 9: WithFields
- Одиночное поле
- Множественные поля
- Цепочка полей
- Вложенные поля в JSON

#### Сценарий 10: Граничные случаи
- Пустые сообщения
- Отсутствующий TraceID
- Очень длинные сообщения
- Unicode
- Nil formatter/output
- Специальные символы

### Бенчмарки

```bash
go test -bench=. -benchmem ./...
```

- BenchmarkLogger_TextFormatter
- BenchmarkLogger_JSONFormatter  
- BenchmarkLogger_WithFields
- BenchmarkLogger_Concurrent

### Покрытие кода

```bash
go test -coverprofile=coverage.out ./...
go tool cover -html=coverage.out
```
