package traceviewer

import (
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	"ebusta/internal/logger"
	"ebusta/internal/lokiclient"
)

type fakeReader struct {
	entries []lokiclient.Entry
	err     error
	traceID string
}

func (f *fakeReader) QueryTrace(_ context.Context, traceID string) ([]lokiclient.Entry, error) {
	f.traceID = traceID
	return f.entries, f.err
}

func newTestService(reader TraceReader) *Service {
	log := logger.New(logger.INFO, logger.NewTextFormatter(), logger.NewWriterOutput(&strings.Builder{}, "buf"), "trace-viewer")
	return New(reader, log)
}

func TestHandleTraceReturnsJSON(t *testing.T) {
	reader := &fakeReader{entries: []lokiclient.Entry{{Timestamp: time.Unix(0, 1), Service: "gateway", Message: "ok"}}}
	recorder := httptest.NewRecorder()
	request := httptest.NewRequest(http.MethodGet, "/trace/gw-1", nil)

	newTestService(reader).Handler().ServeHTTP(recorder, request)

	if recorder.Code != http.StatusOK {
		t.Fatalf("unexpected status: %d", recorder.Code)
	}
	if reader.traceID != "gw-1" {
		t.Fatalf("unexpected trace lookup: %q", reader.traceID)
	}
	if !strings.Contains(recorder.Body.String(), `"trace_id":"gw-1"`) {
		t.Fatalf("unexpected body: %s", recorder.Body.String())
	}
}

func TestHandleTraceRejectsEmptyTraceID(t *testing.T) {
	recorder := httptest.NewRecorder()
	request := httptest.NewRequest(http.MethodGet, "/trace/", nil)

	newTestService(&fakeReader{}).Handler().ServeHTTP(recorder, request)

	if recorder.Code != http.StatusBadRequest {
		t.Fatalf("unexpected status: %d", recorder.Code)
	}
}

func TestHandleTraceMapsReaderError(t *testing.T) {
	recorder := httptest.NewRecorder()
	request := httptest.NewRequest(http.MethodGet, "/trace/gw-2", nil)

	newTestService(&fakeReader{err: errors.New("boom")}).Handler().ServeHTTP(recorder, request)

	if recorder.Code != http.StatusBadGateway {
		t.Fatalf("unexpected status: %d", recorder.Code)
	}
}

func TestHandleIndexRendersPage(t *testing.T) {
	recorder := httptest.NewRecorder()
	request := httptest.NewRequest(http.MethodGet, "/", nil)

	newTestService(&fakeReader{}).Handler().ServeHTTP(recorder, request)

	if recorder.Code != http.StatusOK {
		t.Fatalf("unexpected status: %d", recorder.Code)
	}
	if !strings.Contains(recorder.Body.String(), "Trace Viewer") {
		t.Fatalf("unexpected body: %s", recorder.Body.String())
	}
}
