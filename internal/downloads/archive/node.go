package archive

import (
	"archive/zip"
	"context"
	"database/sql"
	"io"
	"path/filepath"

	libraryv1 "ebusta/api/proto/v1"
	ds "ebusta/internal/downloads/sqlite"

	_ "modernc.org/sqlite"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/metadata"
	"google.golang.org/grpc/status"
)

type Node struct {
	libraryv1.UnimplementedStorageNodeServer

	zipRoot string
	db      *sql.DB
}

func New(zipRoot, sqlitePath string) (*Node, error) {
	db, err := sql.Open("sqlite", sqlitePath)
	if err != nil {
		return nil, err
	}
	if err := ds.EnsureSchema(db); err != nil {
		_ = db.Close()
		return nil, err
	}
	return &Node{zipRoot: zipRoot, db: db}, nil
}

func (n *Node) Close() error {
	if n.db != nil {
		return n.db.Close()
	}
	return nil
}

func requestIDFromCtx(ctx context.Context) string {
	md, ok := metadata.FromIncomingContext(ctx)
	if !ok {
		return ""
	}
	if v := md.Get("x-request-id"); len(v) > 0 {
		return v[0]
	}
	if v := md.Get("request-id"); len(v) > 0 {
		return v[0]
	}
	return ""
}

func isValidSha1Hex40(s string) bool {
	if len(s) != 40 {
		return false
	}
	for i := 0; i < len(s); i++ {
		c := s[i]
		if (c >= '0' && c <= '9') ||
			(c >= 'a' && c <= 'f') ||
			(c >= 'A' && c <= 'F') {
			continue
		}
		return false
	}
	return true
}

type metaRow struct {
	sha1      string
	container string
	filename  string
	size      int64
	title     string
}

func (n *Node) getMeta(ctx context.Context, sha1 string) (*metaRow, error) {
	var r metaRow
	err := n.db.QueryRowContext(ctx,
		`SELECT sha1, container, filename, size, title FROM books WHERE sha1 = ?`,
		sha1,
	).Scan(&r.sha1, &r.container, &r.filename, &r.size, &r.title)
	if err != nil {
		return nil, err
	}
	return &r, nil
}

func (n *Node) Has(ctx context.Context, req *libraryv1.HasRequest) (*libraryv1.HasResponse, error) {
	sha1 := req.GetId().GetSha1()
	if !isValidSha1Hex40(sha1) {
		return nil, status.Error(codes.InvalidArgument, "invalid sha1 (expected 40 hex)")
	}

	_, err := n.getMeta(ctx, sha1)
	if err == sql.ErrNoRows {
		return &libraryv1.HasResponse{Exists: false}, nil
	}
	if err != nil {
		return nil, status.Errorf(codes.Internal, "INTERNAL: %v", err)
	}
	return &libraryv1.HasResponse{Exists: true}, nil
}

func (n *Node) GetMeta(ctx context.Context, req *libraryv1.GetMetaRequest) (*libraryv1.GetMetaResponse, error) {
	sha1 := req.GetId().GetSha1()
	if !isValidSha1Hex40(sha1) {
		return nil, status.Error(codes.InvalidArgument, "invalid sha1 (expected 40 hex)")
	}

	r, err := n.getMeta(ctx, sha1)
	if err == sql.ErrNoRows {
		return nil, status.Error(codes.NotFound, "NOT_FOUND")
	}
	if err != nil {
		return nil, status.Errorf(codes.Internal, "INTERNAL: %v", err)
	}

	return &libraryv1.GetMetaResponse{
		Meta: &libraryv1.BookMeta{
			Sha1:      r.sha1,
			Container: r.container,
			Filename:  r.filename,
			Size:      r.size,
			Title:     r.title,
		},
	}, nil
}

func (n *Node) GetStream(req *libraryv1.GetStreamRequest, stream libraryv1.StorageNode_GetStreamServer) error {
	ctx := stream.Context()
	sha1 := req.GetId().GetSha1()

	// request_id сейчас используется только для логов/ошибок higher-level; для gRPC status — стандартные коды
	_ = requestIDFromCtx(ctx)

	if !isValidSha1Hex40(sha1) {
		return status.Error(codes.InvalidArgument, "invalid sha1 (expected 40 hex)")
	}

	r, err := n.getMeta(ctx, sha1)
	if err == sql.ErrNoRows {
		return status.Error(codes.NotFound, "NOT_FOUND")
	}
	if err != nil {
		return status.Errorf(codes.Internal, "INTERNAL: %v", err)
	}

	zipPath := filepath.Join(n.zipRoot, r.container)
	zr, err := zip.OpenReader(zipPath)
	if err != nil {
		return status.Errorf(codes.Internal, "ZIP_ERROR: open %s: %v", zipPath, err)
	}
	defer zr.Close()

	var zf *zip.File
	for _, f := range zr.File {
		if f.Name == r.filename {
			zf = f
			break
		}
	}
	if zf == nil {
		return status.Errorf(codes.Internal, "ZIP_ERROR: entry %s not found in %s", r.filename, r.container)
	}

	rc, err := zf.Open()
	if err != nil {
		return status.Errorf(codes.Internal, "ZIP_ERROR: open entry %s: %v", r.filename, err)
	}
	defer rc.Close()

	const chunkSize = 256 * 1024
	buf := make([]byte, chunkSize)

	for {
		nn, rerr := rc.Read(buf)
		if nn > 0 {
			out := make([]byte, nn)
			copy(out, buf[:nn])
			if err := stream.Send(&libraryv1.Chunk{Data: out}); err != nil {
				return err
			}
		}
		if rerr == io.EOF {
			return nil
		}
		if rerr != nil {
			return status.Errorf(codes.Internal, "ZIP_ERROR: read entry: %v", rerr)
		}
	}
}

func (n *Node) Put(stream libraryv1.StorageNode_PutServer) error {
	_ = stream
	return status.Error(codes.Unimplemented, "NOT_IMPLEMENTED: archive Put not supported")
}
