package errutil

import (
    "net/http"

    "google.golang.org/grpc/codes"
)

// Коды ошибок приложения
const (
    // Клиентские ошибки (4xx)
    CodeInvalidArgument = "INVALID_ARGUMENT"
    CodeNotFound        = "NOT_FOUND"
    CodeUnauthorized    = "UNAUTHORIZED"
    CodeForbidden       = "FORBIDDEN"
    CodeTooLarge        = "REQUEST_TOO_LARGE"

    // Серверные ошибки (5xx)
    CodeInternal      = "INTERNAL_ERROR"
    CodeUnavailable   = "SERVICE_UNAVAILABLE"
    CodeTimeout       = "TIMEOUT"
    CodeUpstreamError = "UPSTREAM_ERROR"
    CodeBadGateway    = "BAD_GATEWAY"

    // Специфичные для ebusta
    CodeDownloaderError = "DOWNLOADER_ERROR"
    CodeSearchError     = "SEARCH_ERROR"
    CodeZipError        = "ZIP_ERROR"
)

// CodeToHTTP маппит код приложения в HTTP статус
func CodeToHTTP(code string) int {
    switch code {
    case CodeInvalidArgument:
        return http.StatusBadRequest
    case CodeNotFound:
        return http.StatusNotFound
    case CodeUnauthorized:
        return http.StatusUnauthorized
    case CodeForbidden:
        return http.StatusForbidden
    case CodeTooLarge:
        return http.StatusRequestEntityTooLarge
    case CodeUnavailable, CodeTimeout:
        return http.StatusServiceUnavailable
    case CodeUpstreamError, CodeBadGateway:
        return http.StatusBadGateway
    default:
        return http.StatusInternalServerError
    }
}

// GRPCToCode маппит gRPC code в код приложения
func GRPCToCode(code codes.Code) string {
    switch code {
    case codes.InvalidArgument:
        return CodeInvalidArgument
    case codes.NotFound:
        return CodeNotFound
    case codes.Unauthenticated:
        return CodeUnauthorized
    case codes.PermissionDenied:
        return CodeForbidden
    case codes.Unavailable:
        return CodeUnavailable
    case codes.DeadlineExceeded:
        return CodeTimeout
    default:
        return CodeInternal
    }
}

// CodeToGRPC маппит код приложения в gRPC code
func CodeToGRPC(code string) codes.Code {
    switch code {
    case CodeInvalidArgument:
        return codes.InvalidArgument
    case CodeNotFound:
        return codes.NotFound
    case CodeUnauthorized:
        return codes.Unauthenticated
    case CodeForbidden:
        return codes.PermissionDenied
    case CodeUnavailable:
        return codes.Unavailable
    case CodeTimeout:
        return codes.DeadlineExceeded
    default:
        return codes.Internal
    }
}
