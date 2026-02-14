package errutil_test

import (
    "encoding/json"
    "net/http"
    "net/http/httptest"
    "testing"

    "ebusta/internal/errutil"
)

func TestWriteJSONError(t *testing.T) {
    w := httptest.NewRecorder()
    appErr := errutil.New(errutil.CodeNotFound, "книга не найдена").
        WithTrace("trace-123")

    errutil.WriteJSONError(w, appErr)

    if w.Code != http.StatusNotFound {
        t.Errorf("Status = %d, want %d", w.Code, http.StatusNotFound)
    }

    if w.Header().Get("Content-Type") != "application/json" {
        t.Errorf("Content-Type = %q, want %q", w.Header().Get("Content-Type"), "application/json")
    }

    if w.Header().Get("X-Trace-Id") != "trace-123" {
        t.Errorf("X-Trace-Id = %q, want %q", w.Header().Get("X-Trace-Id"), "trace-123")
    }

    var resp errutil.JSONEnvelope
    if err := json.Unmarshal(w.Body.Bytes(), &resp); err != nil {
        t.Fatalf("Failed to parse response: %v", err)
    }

    if resp.Error.Code != errutil.CodeNotFound {
        t.Errorf("Error.Code = %q, want %q", resp.Error.Code, errutil.CodeNotFound)
    }
    if resp.Error.Message != "книга не найдена" {
        t.Errorf("Error.Message = %q, want %q", resp.Error.Message, "книга не найдена")
    }
}

func TestWriteJSONErrorSimple(t *testing.T) {
    w := httptest.NewRecorder()

    errutil.WriteJSONErrorSimple(w, http.StatusBadRequest, errutil.CodeInvalidArgument, "неверный аргумент", "trace-456")

    if w.Code != http.StatusBadRequest {
        t.Errorf("Status = %d, want %d", w.Code, http.StatusBadRequest)
    }
}

func TestParseDownloaderError(t *testing.T) {
    // Валидный JSON от downloader
    jsonBody := `{"error":{"code":"NOT_FOUND","message":"книга не найдена","trace_id":"dl-123"}}`
    body := []byte(jsonBody)

    appErr := errutil.ParseDownloaderError(body, "trace-456")

    if appErr.Code != errutil.CodeNotFound {
        t.Errorf("Code = %q, want %q", appErr.Code, errutil.CodeNotFound)
    }
    if appErr.Message != "книга не найдена" {
        t.Errorf("Message = %q, want %q", appErr.Message, "книга не найдена")
    }
    if appErr.HTTPCode != http.StatusNotFound {
        t.Errorf("HTTPCode = %d, want %d", appErr.HTTPCode, http.StatusNotFound)
    }
}

func TestParseDownloaderErrorInvalidJSON(t *testing.T) {
    body := []byte("not a json")

    appErr := errutil.ParseDownloaderError(body, "trace-789")

    if appErr.Code != errutil.CodeBadGateway {
        t.Errorf("Code = %q, want %q", appErr.Code, errutil.CodeBadGateway)
    }
    if appErr.HTTPCode != http.StatusBadGateway {
        t.Errorf("HTTPCode = %d, want %d", appErr.HTTPCode, http.StatusBadGateway)
    }
}

func TestFromHTTPResponse_JSON(t *testing.T) {
    resp := &http.Response{
        StatusCode: http.StatusNotFound,
        Header:     make(http.Header),
    }
    resp.Header.Set("Content-Type", "application/json")

    jsonBody := `{"error":{"code":"NOT_FOUND","message":"не найдено","trace_id":""}}`
    body := []byte(jsonBody)

    appErr := errutil.FromHTTPResponse(resp, body, "trace-111")

    if appErr.Code != errutil.CodeNotFound {
        t.Errorf("Code = %q, want %q", appErr.Code, errutil.CodeNotFound)
    }
    if appErr.Message != "не найдено" {
        t.Errorf("Message = %q, want %q", appErr.Message, "не найдено")
    }
}

func TestFromHTTPResponse_JSONWithCharset(t *testing.T) {
    resp := &http.Response{
        StatusCode: http.StatusNotFound,
        Header:     make(http.Header),
    }
    // Content-Type с charset
    resp.Header.Set("Content-Type", "application/json; charset=utf-8")

    jsonBody := `{"error":{"code":"NOT_FOUND","message":"не найдено","trace_id":""}}`
    body := []byte(jsonBody)

    appErr := errutil.FromHTTPResponse(resp, body, "trace-222")

    if appErr.Code != errutil.CodeNotFound {
        t.Errorf("Code = %q, want %q", appErr.Code, errutil.CodeNotFound)
    }
}

func TestFromHTTPResponse_PlainText(t *testing.T) {
    resp := &http.Response{
        StatusCode: http.StatusInternalServerError,
        Header:     make(http.Header),
    }
    resp.Header.Set("Content-Type", "text/plain")

    body := []byte("internal server error")

    appErr := errutil.FromHTTPResponse(resp, body, "trace-333")

    if appErr.Code != errutil.CodeInternal {
        t.Errorf("Code = %q, want %q", appErr.Code, errutil.CodeInternal)
    }
    if appErr.Message != "internal server error" {
        t.Errorf("Message = %q, want %q", appErr.Message, "internal server error")
    }
}

func TestJSONEnvelope_Marshal(t *testing.T) {
    env := errutil.JSONEnvelope{
        Error: errutil.AppError{
            Code:     errutil.CodeNotFound,
            Message:  "не найдено",
            TraceID:  "trace-123",
            HTTPCode: http.StatusNotFound,
        },
    }

    data, err := json.Marshal(env)
    if err != nil {
        t.Fatalf("Failed to marshal: %v", err)
    }

    // Проверяем структуру JSON
    var parsed struct {
        Error struct {
            Code     string `json:"code"`
            Message  string `json:"message"`
            TraceID  string `json:"trace_id"`
            HTTPCode int    `json:"http_code"`
        } `json:"error"`
    }
    if err := json.Unmarshal(data, &parsed); err != nil {
        t.Fatalf("Failed to unmarshal: %v", err)
    }

    if parsed.Error.Code != errutil.CodeNotFound {
        t.Errorf("Code = %q, want %q", parsed.Error.Code, errutil.CodeNotFound)
    }
}
