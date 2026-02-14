package errutil_test

import (
    "testing"

    "ebusta/internal/errutil"

    "google.golang.org/grpc/codes"
    "google.golang.org/grpc/status"
)

func TestFromGRPCError(t *testing.T) {
    grpcErr := status.Error(codes.NotFound, "книга не найдена")

    appErr := errutil.FromGRPCError(grpcErr, "trace-123")

    if appErr.Code != errutil.CodeNotFound {
        t.Errorf("Code = %q, want %q", appErr.Code, errutil.CodeNotFound)
    }
    if appErr.Message != "книга не найдена" {
        t.Errorf("Message = %q, want %q", appErr.Message, "книга не найдена")
    }
    if appErr.TraceID != "trace-123" {
        t.Errorf("TraceID = %q, want %q", appErr.TraceID, "trace-123")
    }
}

func TestFromGRPCError_NonGRPC(t *testing.T) {
    // Обычная ошибка (не gRPC status)
    normalErr := status.Error(codes.Internal, "some error")

    appErr := errutil.FromGRPCError(normalErr, "trace-456")

    if appErr.Code != errutil.CodeInternal {
        t.Errorf("Code = %q, want %q", appErr.Code, errutil.CodeInternal)
    }
}

func TestToGRPCError(t *testing.T) {
    appErr := errutil.New(errutil.CodeNotFound, "не найдено").WithTrace("trace-789")

    grpcErr := errutil.ToGRPCError(appErr)

    st, ok := status.FromError(grpcErr)
    if !ok {
        t.Fatalf("Expected gRPC status error")
    }

    if st.Code() != codes.NotFound {
        t.Errorf("Code = %v, want %v", st.Code(), codes.NotFound)
    }
    if st.Message() != "не найдено" {
        t.Errorf("Message = %q, want %q", st.Message(), "не найдено")
    }
}

func TestGRPCRoundTrip(t *testing.T) {
    testCases := []struct {
        grpcCode codes.Code
        appCode  string
    }{
        {codes.InvalidArgument, errutil.CodeInvalidArgument},
        {codes.NotFound, errutil.CodeNotFound},
        {codes.Unavailable, errutil.CodeUnavailable},
        {codes.DeadlineExceeded, errutil.CodeTimeout},
    }

    for _, tc := range testCases {
        t.Run(tc.grpcCode.String(), func(t *testing.T) {
            // gRPC -> AppError -> gRPC
            grpcErr := status.Error(tc.grpcCode, "test message")
            appErr := errutil.FromGRPCError(grpcErr, "trace")
            backToGRPC := errutil.ToGRPCError(appErr)

            st, _ := status.FromError(backToGRPC)
            if st.Code() != tc.grpcCode {
                t.Errorf("Round trip: %v -> %q -> %v", tc.grpcCode, appErr.Code, st.Code())
            }
        })
    }
}
