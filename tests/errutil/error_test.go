package errutil_test

import (
    "testing"

    "ebusta/internal/errutil"
)

func TestAppError_Error(t *testing.T) {
    err := &errutil.AppError{
        Code:    errutil.CodeNotFound,
        Message: "книга не найдена",
    }

    if err.Error() != "книга не найдена" {
        t.Errorf("Error() = %q, want %q", err.Error(), "книга не найдена")
    }
}

func TestNew(t *testing.T) {
    err := errutil.New(errutil.CodeNotFound, "не найдено")

    if err.Code != errutil.CodeNotFound {
        t.Errorf("Code = %q, want %q", err.Code, errutil.CodeNotFound)
    }
    if err.Message != "не найдено" {
        t.Errorf("Message = %q, want %q", err.Message, "не найдено")
    }
    if err.HTTPCode != 404 {
        t.Errorf("HTTPCode = %d, want %d", err.HTTPCode, 404)
    }
}

func TestWithTrace(t *testing.T) {
    err := errutil.New(errutil.CodeInternal, "ошибка")
    err = err.WithTrace("trace-123")

    if err.TraceID != "trace-123" {
        t.Errorf("TraceID = %q, want %q", err.TraceID, "trace-123")
    }
}

func TestWithDetails(t *testing.T) {
    err := errutil.New(errutil.CodeInternal, "ошибка")
    err = err.WithDetails("детали ошибки")

    if err.Details != "детали ошибки" {
        t.Errorf("Details = %q, want %q", err.Details, "детали ошибки")
    }
}

func TestChainedMethods(t *testing.T) {
    err := errutil.New(errutil.CodeNotFound, "не найдено").
        WithTrace("trace-456").
        WithDetails("дополнительная информация")

    if err.Code != errutil.CodeNotFound {
        t.Errorf("Code = %q, want %q", err.Code, errutil.CodeNotFound)
    }
    if err.TraceID != "trace-456" {
        t.Errorf("TraceID = %q, want %q", err.TraceID, "trace-456")
    }
    if err.Details != "дополнительная информация" {
        t.Errorf("Details = %q, want %q", err.Details, "дополнительная информация")
    }
}
