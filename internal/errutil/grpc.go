package errutil

import (
    "google.golang.org/grpc/status"
)

// FromGRPCError конвертирует gRPC ошибку в AppError
func FromGRPCError(err error, traceID string) *AppError {
    st, ok := status.FromError(err)
    if !ok {
        return &AppError{
            Code:     CodeInternal,
            Message:  err.Error(),
            TraceID:  traceID,
            HTTPCode: 500,
        }
    }

    return &AppError{
        Code:     GRPCToCode(st.Code()),
        Message:  st.Message(),
        TraceID:  traceID,
        HTTPCode: CodeToHTTP(GRPCToCode(st.Code())),
    }
}

// ToGRPCError конвертирует AppError в gRPC ошибку
func ToGRPCError(err *AppError) error {
    return status.Error(CodeToGRPC(err.Code), err.Message)
}
