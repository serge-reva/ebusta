package middleware

import (
	"net/http"
	"time"

	"github.com/sirupsen/logrus"
)

// RequestLogger logs incoming requests at the INFO level.
func RequestLogger(log *logrus.Logger) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			start := time.Now()
			
			// Serve the request
			next.ServeHTTP(w, r)
			
			// Log the request
			log.WithFields(logrus.Fields{
				"method": r.Method,
				"path":   r.URL.Path,
//				"query":  r.URL.RawQuery,
				"query":  r.URL.Query(),
				"remote": r.RemoteAddr,
				"agent":  r.UserAgent(),
				"took":   time.Since(start),
			}).Info("http.request")
		})
	}
}
