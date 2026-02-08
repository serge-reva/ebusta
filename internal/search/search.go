package search

// NOTE:
// Этот файл оставлен для совместимости истории/контекста.
// Ранее здесь была альтернативная реализация Service/SearchResult,
// которая конфликтовала с канонической (service.go + dto.go).
//
// Канонический API пакета search теперь определяется в:
//   - service.go (Service, New/NewService, Search(..., traceID))
//   - dto.go (BookDTO, SearchResult)
//   - query.go (Normalize/Validate)
//
// В этом файле не должно быть объявлений типов/функций, дублирующих канон.
