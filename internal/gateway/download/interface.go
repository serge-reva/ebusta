package download

import (
	"context"
	"io"
)

type BookMeta struct {
	Sha1      string
	Container string
	Filename  string
	Size      int64
	Title     string
}

type DownloadBackend interface {
	GetFile(ctx context.Context, token string, w io.Writer) error
	GetMeta(ctx context.Context, token string) (*BookMeta, error)
	GenerateLink(ctx context.Context, token string) (string, error)
}
