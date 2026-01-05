package logger

import (
	"context"
	"time"
	"github.com/sirupsen/logrus"
)

type ctxKey string
const RequestIDKey ctxKey = "requestId"

func init() {
	logrus.SetFormatter(&logrus.TextFormatter{
		FullTimestamp:   true,
		TimestampFormat: "15:04:05",
		ForceColors:     true,
		DisableColors:   false,
	})
}

func For(ctx context.Context) *logrus.Entry {
	id, ok := ctx.Value(RequestIDKey).(string)
	if !ok {
		return logrus.NewEntry(logrus.StandardLogger())
	}
	return logrus.WithField("request_id", id)
}

func ContextWithID(ctx context.Context, id string) context.Context {
	return context.WithValue(ctx, RequestIDKey, id)
}

func Track(ctx context.Context, msg string) func() {
	start := time.Now()
	return func() {
		dur := time.Since(start)
		entry := For(ctx).WithField("duration", dur.String())
		
		if dur > 500*time.Millisecond {
			entry.Warnf("%s completed (SLOW)", msg)
		} else {
			entry.Infof("%s completed", msg)
		}
	}
}
