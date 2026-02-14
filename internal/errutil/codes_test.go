package errutil

import (
    "net/http"
    "testing"

    "google.golang.org/grpc/codes"
)

func TestCodeToHTTP(t *testing.T) {
    tests := []struct {
        code     string
        expected int
    }{
        {CodeInvalidArgument, http.StatusBadRequest},
        {CodeNotFound, http.StatusNotFound},
        {CodeUnauthorized, http.StatusUnauthorized},
        {CodeForbidden, http.StatusForbidden},
        {CodeTooLarge, http.StatusRequestEntityTooLarge},
        {CodeUnavailable, http.StatusServiceUnavailable},
        {CodeTimeout, http.StatusServiceUnavailable},
        {CodeUpstreamError, http.StatusBadGateway},
        {CodeBadGateway, http.StatusBadGateway},
        {CodeInternal, http.StatusInternalServerError},
        {"UNKNOWN_CODE", http.StatusInternalServerError},
    }

    for _, tt := range tests {
        t.Run(tt.code, func(t *testing.T) {
            result := CodeToHTTP(tt.code)
            if result != tt.expected {
                t.Errorf("CodeToHTTP(%q) = %d, want %d", tt.code, result, tt.expected)
            }
        })
    }
}

func TestGRPCToCode(t *testing.T) {
    tests := []struct {
        grpcCode codes.Code
        expected string
    }{
        {codes.InvalidArgument, CodeInvalidArgument},
        {codes.NotFound, CodeNotFound},
        {codes.Unauthenticated, CodeUnauthorized},
        {codes.PermissionDenied, CodeForbidden},
        {codes.Unavailable, CodeUnavailable},
        {codes.DeadlineExceeded, CodeTimeout},
        {codes.Internal, CodeInternal},
        {codes.Unknown, CodeInternal},
    }

    for _, tt := range tests {
        t.Run(tt.grpcCode.String(), func(t *testing.T) {
            result := GRPCToCode(tt.grpcCode)
            if result != tt.expected {
                t.Errorf("GRPCToCode(%v) = %q, want %q", tt.grpcCode, result, tt.expected)
            }
        })
    }
}

func TestCodeToGRPC(t *testing.T) {
    tests := []struct {
        code     string
        expected codes.Code
    }{
        {CodeInvalidArgument, codes.InvalidArgument},
        {CodeNotFound, codes.NotFound},
        {CodeUnauthorized, codes.Unauthenticated},
        {CodeForbidden, codes.PermissionDenied},
        {CodeUnavailable, codes.Unavailable},
        {CodeTimeout, codes.DeadlineExceeded},
        {CodeInternal, codes.Internal},
        {"UNKNOWN_CODE", codes.Internal},
    }

    for _, tt := range tests {
        t.Run(tt.code, func(t *testing.T) {
            result := CodeToGRPC(tt.code)
            if result != tt.expected {
                t.Errorf("CodeToGRPC(%q) = %v, want %v", tt.code, result, tt.expected)
            }
        })
    }
}

func TestCodeRoundTrip(t *testing.T) {
    // Проверяем что маппинг обратно совместим
    grpcCodes := []codes.Code{
        codes.InvalidArgument,
        codes.NotFound,
        codes.Unauthenticated,
        codes.PermissionDenied,
        codes.Unavailable,
        codes.DeadlineExceeded,
    }

    for _, gc := range grpcCodes {
        appCode := GRPCToCode(gc)
        backToGRPC := CodeToGRPC(appCode)
        if backToGRPC != gc {
            t.Errorf("Round trip failed: %v -> %q -> %v", gc, appCode, backToGRPC)
        }
    }
}
