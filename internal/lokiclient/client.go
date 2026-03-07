package lokiclient

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"

	"ebusta/internal/errutil"
)

const defaultSelector = `{compose_project="ebusta"}`

var textLogPattern = regexp.MustCompile(`^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{3} \[([A-Z]+)\] \[[^\]]+\](?: \[([^\]]+)\])? (.*)$`)
var ansiPattern = regexp.MustCompile(`\x1b\[[0-9;]*m`)

type Client struct {
	baseURL    string
	selector   string
	httpClient *http.Client
}

type Entry struct {
	Timestamp time.Time         `json:"timestamp"`
	Level     string            `json:"level,omitempty"`
	Service   string            `json:"service,omitempty"`
	Message   string            `json:"message"`
	Error     string            `json:"error,omitempty"`
	Labels    map[string]string `json:"labels,omitempty"`
	Raw       string            `json:"raw"`
}

type Option func(*Client)

func WithHTTPClient(httpClient *http.Client) Option {
	return func(client *Client) {
		if httpClient != nil {
			client.httpClient = httpClient
		}
	}
}

func WithSelector(selector string) Option {
	return func(client *Client) {
		if strings.TrimSpace(selector) != "" {
			client.selector = selector
		}
	}
}

func New(baseURL string, opts ...Option) *Client {
	client := &Client{
		baseURL:    strings.TrimRight(baseURL, "/"),
		selector:   defaultSelector,
		httpClient: &http.Client{Timeout: 10 * time.Second},
	}
	for _, opt := range opts {
		opt(client)
	}
	return client
}

func (c *Client) QueryTrace(ctx context.Context, traceID string) ([]Entry, error) {
	traceID = strings.TrimSpace(traceID)
	if traceID == "" {
		return nil, errutil.New(errutil.CodeInvalidArgument, "trace_id is required")
	}

	queryURL, err := url.Parse(c.baseURL + "/loki/api/v1/query_range")
	if err != nil {
		return nil, errutil.New(errutil.CodeInternal, "invalid loki url").WithDetails(err.Error())
	}

	params := queryURL.Query()
	params.Set("query", fmt.Sprintf(`%s |= %q`, c.selector, traceID))
	params.Set("direction", "forward")
	params.Set("limit", "5000")
	queryURL.RawQuery = params.Encode()

	request, err := http.NewRequestWithContext(ctx, http.MethodGet, queryURL.String(), nil)
	if err != nil {
		return nil, errutil.New(errutil.CodeInternal, "build loki request failed").WithDetails(err.Error())
	}

	response, err := c.httpClient.Do(request)
	if err != nil {
		return nil, errutil.New(errutil.CodeUnavailable, "loki request failed").WithDetails(err.Error())
	}
	defer response.Body.Close()

	if response.StatusCode != http.StatusOK {
		return nil, errutil.New(errutil.CodeBadGateway, "loki returned non-200 response").WithDetails(response.Status)
	}

	var payload lokiResponse
	if err := json.NewDecoder(response.Body).Decode(&payload); err != nil {
		return nil, errutil.New(errutil.CodeInternal, "decode loki response failed").WithDetails(err.Error())
	}
	if payload.Status != "success" {
		return nil, errutil.New(errutil.CodeBadGateway, "loki query failed").WithDetails(payload.Status)
	}

	entries := make([]Entry, 0)
	for _, result := range payload.Data.Result {
		for _, value := range result.Values {
			if len(value) != 2 {
				continue
			}
			timestamp, err := parseLokiTimestamp(value[0])
			if err != nil {
				continue
			}
			line := value[1]
			entry := parseLogLine(line)
			entry.Timestamp = timestamp
			entry.Raw = line
			entry.Labels = cloneLabels(result.Stream)
			if entry.Service == "" {
				entry.Service = firstNonEmpty(result.Stream["compose_service"], result.Stream["service_name"], result.Stream["container_name"])
			}
			entries = append(entries, entry)
		}
	}

	sort.Slice(entries, func(i, j int) bool {
		return entries[i].Timestamp.Before(entries[j].Timestamp)
	})
	return entries, nil
}

type lokiResponse struct {
	Status string `json:"status"`
	Data   struct {
		Result []struct {
			Stream map[string]string `json:"stream"`
			Values [][]string        `json:"values"`
		} `json:"result"`
	} `json:"data"`
}

func parseLokiTimestamp(raw string) (time.Time, error) {
	nanoseconds, err := strconv.ParseInt(raw, 10, 64)
	if err != nil {
		return time.Time{}, err
	}
	return time.Unix(0, nanoseconds), nil
}

func parseLogLine(line string) Entry {
	line = ansiPattern.ReplaceAllString(line, "")
	matches := textLogPattern.FindStringSubmatch(line)
	if len(matches) != 4 {
		return Entry{Message: line}
	}
	message := matches[3]
	errorText := ""
	if idx := strings.LastIndex(message, " error="); idx >= 0 {
		errorText = strings.TrimSpace(message[idx+7:])
		message = strings.TrimSpace(message[:idx])
	}
	return Entry{
		Level:   matches[1],
		Service: matches[2],
		Message: message,
		Error:   errorText,
	}
}

func cloneLabels(labels map[string]string) map[string]string {
	if len(labels) == 0 {
		return nil
	}
	clone := make(map[string]string, len(labels))
	for key, value := range labels {
		clone[key] = value
	}
	return clone
}

func firstNonEmpty(values ...string) string {
	for _, value := range values {
		if strings.TrimSpace(value) != "" {
			return value
		}
	}
	return ""
}
