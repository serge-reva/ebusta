package download

import (
	"context"
	"io"

	"ebusta/internal/errutil"
	"ebusta/internal/gateway/clients"
	"ebusta/internal/gateway/mapper"
)

type DirectBackend struct {
	mapper     *mapper.Mapper
	downloader *clients.DownloaderClient
}

func NewDirectBackend(m *mapper.Mapper, d *clients.DownloaderClient) *DirectBackend {
	return &DirectBackend{
		mapper:     m,
		downloader: d,
	}
}

func (b *DirectBackend) GetFile(ctx context.Context, token string, w io.Writer) error {
	sha1, err := b.resolveSHA1(token)
	if err != nil {
		return err
	}
	return b.downloader.StreamBookWithTrace(sha1, w, errutil.TraceIDFromContext(ctx))
}

func (b *DirectBackend) GetMeta(ctx context.Context, token string) (*BookMeta, error) {
	sha1, err := b.resolveSHA1(token)
	if err != nil {
		return nil, err
	}
	meta, err := b.downloader.GetMetaWithTrace(sha1, errutil.TraceIDFromContext(ctx))
	if err != nil {
		return nil, err
	}
	return &BookMeta{
		Sha1:      meta.Sha1,
		Container: meta.Container,
		Filename:  meta.Filename,
		Size:      meta.Size,
		Title:     meta.Title,
	}, nil
}

func (b *DirectBackend) GenerateLink(context.Context, string) (string, error) {
	return "", nil
}

func (b *DirectBackend) resolveSHA1(token string) (string, error) {
	sha1, _, err := b.mapper.Resolve(token)
	if err != nil {
		return "", err
	}
	return sha1, nil
}
