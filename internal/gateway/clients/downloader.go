package clients

import (
    "encoding/json"
    "fmt"
    "io"
    "net/http"
    "time"
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
    url := fmt.Sprintf("%s/books/%s?meta=1", c.baseURL, sha1)
    
    resp, err := c.httpClient.Get(url)
    if err != nil {
        return nil, err
    }
    defer resp.Body.Close()
    
    if resp.StatusCode != http.StatusOK {
        return nil, fmt.Errorf("downloader returned %d", resp.StatusCode)
    }
    
    var meta BookMeta
    if err := json.NewDecoder(resp.Body).Decode(&meta); err != nil {
        return nil, err
    }
    
    return &meta, nil
}

func (c *DownloaderClient) StreamBook(sha1 string, w io.Writer) error {
    url := fmt.Sprintf("%s/books/%s", c.baseURL, sha1)
    
    resp, err := c.httpClient.Get(url)
    if err != nil {
        return err
    }
    defer resp.Body.Close()
    
    if resp.StatusCode != http.StatusOK {
        return fmt.Errorf("downloader returned %d", resp.StatusCode)
    }
    
    _, err = io.Copy(w, resp.Body)
    return err
}
