package gatewayclient

type SearchBook struct {
	ID          string   `json:"id"`
	Title       string   `json:"title"`
	Authors     []string `json:"authors"`
	Container   string   `json:"container"`
	Filename    string   `json:"filename"`
	FullAuthors string   `json:"full_authors"`
	DownloadURL string   `json:"download_url,omitempty"`
}

type SearchResponse struct {
	TraceID string       `json:"trace_id"`
	Total   int          `json:"total"`
	Books   []SearchBook `json:"books"`
	Page    int          `json:"page"`
	Pages   int          `json:"pages"`
}

type BookDetails struct {
	Sha1      string   `json:"sha1"`
	Title     string   `json:"title"`
	Authors   []string `json:"authors"`
	Container string   `json:"container"`
	Filename  string   `json:"filename"`
	Size      int64    `json:"size"`
}

type FileMeta struct {
	Size     int64
	Filename string
}
