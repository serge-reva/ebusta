package errutil

// AppError — структурированная ошибка приложения
type AppError struct {
    Code      string `json:"code"`               // INTERNAL_ERROR, NOT_FOUND, etc.
    Message   string `json:"message"`            // Человекочитаемое сообщение
    TraceID   string `json:"trace_id"`           // Для диагностики
    Details   string `json:"details,omitempty"`  // Технические детали (опционально)
    HTTPCode  int    `json:"-"`                  // HTTP статус (для маппинга)
}

// Error реализует интерфейс error
func (e *AppError) Error() string {
    return e.Message
}

// New создаёт новую ошибку с кодом и сообщением
func New(code, message string) *AppError {
    return &AppError{
        Code:     code,
        Message:  message,
        HTTPCode: CodeToHTTP(code),
    }
}

// WithTrace добавляет TraceID и возвращает ошибку
func (e *AppError) WithTrace(traceID string) *AppError {
    e.TraceID = traceID
    return e
}

// WithDetails добавляет детали и возвращает ошибку
func (e *AppError) WithDetails(details string) *AppError {
    e.Details = details
    return e
}
