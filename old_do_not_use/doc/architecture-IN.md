cat << 'EOF' > ebusta_arch_spec.md
# Техническая спецификация: Архитектура Ebusta (Orchestration Model)

**Дата:** 05.01.2026
**Статус:** Утверждено
**Модель взаимодействия:** Централизованная оркестрация (Orchestration)

## 1. Обзор архитектуры
Система строится на базе центрального компонента (**Orchestrator**), который координирует работу «тонких» адаптеров, парсера DSL и сервиса данных на удаленном хосте Mercury.

### Ключевые узлы:
1. **Adapters (The Door)**: SSH/BBS, Telegram, Web. Принимают ввод, передают его в Core, получают результат и рендерят его.
2. **Orchestrator (Core)**: Логический центр. Управляет жизненным циклом запроса.
3. **Parser**: Библиотека для конвертации строки в `libraryv1.SearchQuery`.
4. **Data Manager (Mercury Proxy)**: gRPC-сервис, транслирующий запросы в OpenSearch (Docker на Mercury).

## 2. Спецификация UnifiedMessage
`UnifiedMessage` является единым транспортным контейнером внутри системы.

```protobuf
message UnifiedMessage {
    string request_id = 1;
    
    // Метаданные источника для обратной маршрутизации
    message Context {
        string client_id = 1;
        enum SourceType {
            BBS = 0;
            TELEGRAM = 1;
            WEB = 2;
        }
        SourceType source = 2;
    }
    Context context = 2;

    // Полезная нагрузка (Payload)
    oneof content {
        libraryv1.SearchQuery query = 3;  // Структурированный запрос
        SearchResult result = 4;          // Результаты из OpenSearch
        string error = 5;                 // Описание ошибки
    }
}
