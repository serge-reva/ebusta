package errutil

import (
    "encoding/json"
    "io"
    "net/http"
    "strings"
)

// JSONEnvelope — обёртка для JSON-ответа
type JSONEnvelope struct {
    Error AppError `json:"error"`
}

// WriteJSONError отправляет JSON-ошибку в HTTP response
func WriteJSONError(w http.ResponseWriter, err *AppError) {
    w.Header().Set("Content-Type", "application/json")
    w.Header().Set("X-Trace-Id", err.TraceID)
    w.WriteHeader(err.HTTPCode)

    json.NewEncoder(w).Encode(JSONEnvelope{Error: *err})
}

// WriteJSONErrorSimple — упрощённая версия с явными параметрами
func WriteJSONErrorSimple(w http.ResponseWriter, statusCode int, code, message, traceID string) {
    WriteJSONError(w, &AppError{
        Code:     code,
        Message:  message,
        TraceID:  traceID,
        HTTPCode: statusCode,
    })
}

// ParseDownloaderError парсит JSON-ошибку от downloader
func ParseDownloaderError(body []byte, traceID string) *AppError {
    var env JSONEnvelope
    // ИСПРАВЛЕНО: анмаршал в &env, а не в &env.Error
    if err := json.Unmarshal(body, &env); err != nil {
        return &AppError{
            Code:     CodeBadGateway,
            Message:  "Ошибка связи с сервисом скачивания",
            TraceID:  traceID,
            HTTPCode: http.StatusBadGateway,
        }
    }
    env.Error.HTTPCode = CodeToHTTP(env.Error.Code)
    if env.Error.TraceID == "" {
        env.Error.TraceID = traceID
    }
    return &env.Error
}

// FromHTTPResponse создаёт ошибку из HTTP ответа
func FromHTTPResponse(resp *http.Response, body []byte, traceID string) *AppError {
    // Попытка парсить как JSON
    ct := resp.Header.Get("Content-Type")
    // ИСПРАВЛЕНО: проверяем Content-Type более гибко
    if ct == "application/json" || strings.Contains(ct, "application/json") {
        return ParseDownloaderError(body, traceID)
    }

    // Определяем код по HTTP статусу
    code := CodeInternal
    if resp.StatusCode >= 400 && resp.StatusCode < 500 {
        code = CodeInvalidArgument
    } else if resp.StatusCode == http.StatusServiceUnavailable {
        code = CodeUnavailable
    } else if resp.StatusCode == http.StatusGatewayTimeout {
        code = CodeTimeout
    } else if resp.StatusCode == http.StatusBadGateway {
        code = CodeBadGateway
    }

    return &AppError{
        Code:     code,
        Message:  string(body),
        TraceID:  traceID,
        HTTPCode: resp.StatusCode,
    }
}

// ReadBodyAndError читает тело ответа и создаёт AppError при ошибке
// Возвращает (body, nil) если статус 2xx, иначе (nil, AppError)
func ReadBodyAndError(resp *http.Response, traceID string) ([]byte, *AppError) {
    body, err := io.ReadAll(resp.Body)
    if err != nil {
        return nil, &AppError{
            Code:     CodeInternal,
            Message:  "Ошибка чтения ответа",
            TraceID:  traceID,
            HTTPCode: http.StatusInternalServerError,
        }
    }

    if resp.StatusCode >= 200 && resp.StatusCode < 300 {
        return body, nil
    }

    return nil, FromHTTPResponse(resp, body, traceID)
}
