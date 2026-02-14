package errutil_test

import (
    "net/http"
    "testing"

    "ebusta/internal/errutil"

    "google.golang.org/grpc/codes"
)

func TestCodeToHTTP(t *testing.T) {
    tests := []struct {
        code     string
        expected int
    }{
        {errutil.CodeInvalidArgument, http.StatusBadRequest},
        {errutil.CodeNotFound, http.StatusNotFound},
        {errutil.CodeUnauthorized, http.StatusUnauthorized},
        {errutil.CodeForbidden, http.StatusForbidden},
        {errutil.CodeTooLarge, http.StatusRequestEntityTooLarge},
        {errutil.CodeUnavailable, http.StatusServiceUnavailable},
        {errutil.CodeTimeout, http.StatusServiceUnavailable},
        {errutil.CodeUpstreamError, http.StatusBadGateway},
        {errutil.CodeBadGateway, http.StatusBadGateway},
        {errutil.CodeInternal, http.StatusInternalServerError},
        {"UNKNOWN_CODE", http.StatusInternalServerError},
    }

    for _, tt := range tests {
        t.Run(tt.code, func(t *testing.T) {
            result := errutil.CodeToHTTP(tt.code)
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
        {codes.InvalidArgument, errutil.CodeInvalidArgument},
        {codes.NotFound, errutil.CodeNotFound},
        {codes.Unauthenticated, errutil.CodeUnauthorized},
        {codes.PermissionDenied, errutil.CodeForbidden},
        {codes.Unavailable, errutil.CodeUnavailable},
        {codes.DeadlineExceeded, errutil.CodeTimeout},
        {codes.Internal, errutil.CodeInternal},
        {codes.Unknown, errutil.CodeInternal},
    }

    for _, tt := range tests {
        t.Run(tt.grpcCode.String(), func(t *testing.T) {
            result := errutil.GRPCToCode(tt.grpcCode)
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
        {errutil.CodeInvalidArgument, codes.InvalidArgument},
        {errutil.CodeNotFound, codes.NotFound},
        {errutil.CodeUnauthorized, codes.Unauthenticated},
        {errutil.CodeForbidden, codes.PermissionDenied},
        {errutil.CodeUnavailable, codes.Unavailable},
        {errutil.CodeTimeout, codes.DeadlineExceeded},
        {errutil.CodeInternal, codes.Internal},
        {"UNKNOWN_CODE", codes.Internal},
    }

    for _, tt := range tests {
        t.Run(tt.code, func(t *testing.T) {
            result := errutil.CodeToGRPC(tt.code)
            if result != tt.expected {
                t.Errorf("CodeToGRPC(%q) = %v, want %v", tt.code, result, tt.expected)
            }
        })
    }
}

func TestCodeRoundTrip(t *testing.T) {
    grpcCodes := []codes.Code{
        codes.InvalidArgument,
        codes.NotFound,
        codes.Unauthenticated,
        codes.PermissionDenied,
        codes.Unavailable,
        codes.DeadlineExceeded,
    }

    for _, gc := range grpcCodes {
        appCode := errutil.GRPCToCode(gc)
        backToGRPC := errutil.CodeToGRPC(appCode)
        if backToGRPC != gc {
            t.Errorf("Round trip failed: %v -> %q -> %v", gc, appCode, backToGRPC)
        }
    }
}
