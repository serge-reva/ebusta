package clients

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"time"

	"ebusta/internal/errutil"
)

type DownloaderClient struct {
	baseURL    string
	httpClient *http.Client
}

func NewDownloaderClient(baseURL string) *DownloaderClient {
	return &DownloaderClient{
		baseURL: fmt.Sprintf("http://%s", baseURL),
		httpClient: &http.Client{
			Timeout: 30 * time.Second,
		},
	}
}

type BookMeta struct {
	Sha1      string `json:"sha1"`
	Container string `json:"container"`
	Filename  string `json:"filename"`
	Size      int64  `json:"size"`
	Title     string `json:"title"`
}

func (c *DownloaderClient) GetMeta(sha1 string) (*BookMeta, error) {
	return c.getMeta(sha1, "")
}

func (c *DownloaderClient) GetMetaWithTrace(sha1, traceID string) (*BookMeta, error) {
	return c.getMeta(sha1, traceID)
}

func (c *DownloaderClient) getMeta(sha1, traceID string) (*BookMeta, error) {
	url := fmt.Sprintf("%s/books/%s?meta=1", c.baseURL, sha1)

	req, err := http.NewRequest(http.MethodGet, url, nil)
	if err != nil {
		return nil, err
	}
	req.Header.Set("X-Trace-Id", traceID)

	resp, err := c.httpClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	body, appErr := errutil.ReadBodyAndError(resp, traceID)
	if appErr != nil {
		return nil, appErr
	}

	var meta BookMeta
	if err := json.Unmarshal(body, &meta); err != nil {
		return nil, errutil.New(errutil.CodeBadGateway, "invalid downloader metadata response").
			WithTrace(traceID).
			WithDetails(err.Error())
	}

	return &meta, nil
}

func (c *DownloaderClient) StreamBook(sha1 string, w io.Writer) error {
	return c.streamBook(sha1, w, "")
}

func (c *DownloaderClient) StreamBookWithTrace(sha1 string, w io.Writer, traceID string) error {
	return c.streamBook(sha1, w, traceID)
}

func (c *DownloaderClient) streamBook(sha1 string, w io.Writer, traceID string) error {
	url := fmt.Sprintf("%s/books/%s", c.baseURL, sha1)

	req, err := http.NewRequest(http.MethodGet, url, nil)
	if err != nil {
		return err
	}
	req.Header.Set("X-Trace-Id", traceID)

	resp, err := c.httpClient.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		_, appErr := errutil.ReadBodyAndError(resp, traceID)
		if appErr != nil {
			return appErr
		}
		return fmt.Errorf("downloader returned %d", resp.StatusCode)
	}

	_, err = io.Copy(w, resp.Body)
	return err
}
