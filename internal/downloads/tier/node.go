package tier

import (
	"context"
	"database/sql"
	"errors"
	"io"
	"net"
	"os"
	"path/filepath"
	"sync"

	libraryv1 "ebusta/api/proto/v1"
	ds "ebusta/internal/downloads/sqlite"

	_ "modernc.org/sqlite"

	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/credentials/insecure"
	"google.golang.org/grpc/status"
)

type Node struct {
	libraryv1.UnimplementedStorageNodeServer

	rootPath string
	db       *sql.DB

	parentAddr string
	parentConn *grpc.ClientConn
	parent     libraryv1.StorageNodeClient

	mu sync.Mutex
	sf map[string]*inflight
}

type inflight struct {
	wg  sync.WaitGroup
	err error
}

type Config struct {
	RootPath   string
	SqlitePath string
	ParentAddr string // required for disk tiers
}

func New(cfg Config) (*Node, error) {
	if cfg.RootPath == "" {
		return nil, errors.New("RootPath is empty")
	}
	if cfg.SqlitePath == "" {
		return nil, errors.New("SqlitePath is empty")
	}
	if cfg.ParentAddr == "" {
		return nil, errors.New("ParentAddr is empty")
	}

	db, err := sql.Open("sqlite", cfg.SqlitePath)
	if err != nil {
		return nil, err
	}
	if err := ds.EnsureSchema(db); err != nil {
		_ = db.Close()
		return nil, err
	}

	conn, err := grpc.Dial(cfg.ParentAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		_ = db.Close()
		return nil, err
	}

	return &Node{
		rootPath:   cfg.RootPath,
		db:         db,
		parentAddr: cfg.ParentAddr,
		parentConn: conn,
		parent:     libraryv1.NewStorageNodeClient(conn),
		sf:         make(map[string]*inflight),
	}, nil
}

func (n *Node) Close() error {
	if n.parentConn != nil {
		_ = n.parentConn.Close()
	}
	if n.db != nil {
		return n.db.Close()
	}
	return nil
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

func (n *Node) filePath(container, filename string) string {
	return filepath.Join(n.rootPath, container, filename)
}

func (n *Node) hasLocalFile(meta *libraryv1.BookMeta) bool {
	if meta == nil {
		return false
	}
	p := n.filePath(meta.GetContainer(), meta.GetFilename())
	st, err := os.Stat(p)
	if err != nil {
		return false
	}
	return st.Mode().IsRegular()
}

func (n *Node) loadMeta(ctx context.Context, sha1 string) (*libraryv1.BookMeta, error) {
	var m libraryv1.BookMeta
	err := n.db.QueryRowContext(ctx,
		`SELECT sha1, container, filename, size, title FROM books WHERE sha1 = ?`,
		sha1,
	).Scan(&m.Sha1, &m.Container, &m.Filename, &m.Size, &m.Title)
	if err != nil {
		return nil, err
	}
	return &m, nil
}

func (n *Node) saveMeta(ctx context.Context, m *libraryv1.BookMeta) error {
	_, err := n.db.ExecContext(ctx, `
INSERT INTO books(sha1, container, filename, size, title, created_at, updated_at)
VALUES(?, ?, ?, ?, ?, datetime('now'), datetime('now'))
ON CONFLICT(sha1) DO UPDATE SET
  container=excluded.container,
  filename=excluded.filename,
  size=excluded.size,
  title=excluded.title,
  updated_at=datetime('now')
`,
		m.GetSha1(), m.GetContainer(), m.GetFilename(), m.GetSize(), m.GetTitle(),
	)
	return err
}

func (n *Node) Has(ctx context.Context, req *libraryv1.HasRequest) (*libraryv1.HasResponse, error) {
	sha1 := req.GetId().GetSha1()
	if !isValidSha1Hex40(sha1) {
		return nil, status.Error(codes.InvalidArgument, "invalid sha1 (expected 40 hex)")
	}

	m, err := n.loadMeta(ctx, sha1)
	if err == sql.ErrNoRows {
		return &libraryv1.HasResponse{Exists: false}, nil
	}
	if err != nil {
		return nil, status.Errorf(codes.Internal, "INTERNAL: %v", err)
	}
	if !n.hasLocalFile(m) {
		return &libraryv1.HasResponse{Exists: false}, nil
	}
	return &libraryv1.HasResponse{Exists: true}, nil
}

func (n *Node) GetMeta(ctx context.Context, req *libraryv1.GetMetaRequest) (*libraryv1.GetMetaResponse, error) {
	sha1 := req.GetId().GetSha1()
	if !isValidSha1Hex40(sha1) {
		return nil, status.Error(codes.InvalidArgument, "invalid sha1 (expected 40 hex)")
	}

	m, err := n.loadMeta(ctx, sha1)
	// miss в sqlite или нет файла на диске -> пытаемся подтянуть у parent
	if err == sql.ErrNoRows || (err == nil && !n.hasLocalFile(m)) {
		if err2 := n.ensureLocal(ctx, sha1); err2 != nil {
			// IMPORTANT: не перекодируем — просто отдаём gRPC статус parent/ensureLocal
			return nil, err2
		}

		m2, err2 := n.loadMeta(ctx, sha1)
		if err2 == sql.ErrNoRows {
			// странно, ensureLocal прошло, но meta нет — трактуем как NotFound
			return nil, status.Error(codes.NotFound, "NOT_FOUND")
		}
		if err2 != nil {
			return nil, status.Errorf(codes.Internal, "INTERNAL: %v", err2)
		}
		if !n.hasLocalFile(m2) {
			return nil, status.Error(codes.NotFound, "NOT_FOUND")
		}
		return &libraryv1.GetMetaResponse{Meta: m2}, nil
	}

	if err != nil {
		return nil, status.Errorf(codes.Internal, "INTERNAL: %v", err)
	}

	if !n.hasLocalFile(m) {
		return nil, status.Error(codes.NotFound, "NOT_FOUND")
	}

	return &libraryv1.GetMetaResponse{Meta: m}, nil
}

func (n *Node) GetStream(req *libraryv1.GetStreamRequest, stream libraryv1.StorageNode_GetStreamServer) error {
	ctx := stream.Context()
	sha1 := req.GetId().GetSha1()

	if err := n.ensureLocal(ctx, sha1); err != nil {
		return err
	}

	m, err := n.loadMeta(ctx, sha1)
	if err != nil {
		if err == sql.ErrNoRows {
			return status.Error(codes.NotFound, "NOT_FOUND")
		}
		return status.Errorf(codes.Internal, "INTERNAL: %v", err)
	}
	if !n.hasLocalFile(m) {
		return status.Error(codes.NotFound, "NOT_FOUND")
	}

	p := n.filePath(m.GetContainer(), m.GetFilename())
	f, err := os.Open(p)
	if err != nil {
		return status.Errorf(codes.Internal, "IO_ERROR: open %s: %v", p, err)
	}
	defer f.Close()

	const chunkSize = 256 * 1024
	buf := make([]byte, chunkSize)

	for {
		nn, rerr := f.Read(buf)
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
			return status.Errorf(codes.Internal, "IO_ERROR: read: %v", rerr)
		}
	}
}

func (n *Node) Put(stream libraryv1.StorageNode_PutServer) error {
	ctx := stream.Context()

	first, err := stream.Recv()
	if err != nil {
		return status.Errorf(codes.InvalidArgument, "missing first message: %v", err)
	}
	meta := first.GetMeta()
	if meta == nil || meta.GetSha1() == "" || meta.GetContainer() == "" || meta.GetFilename() == "" {
		return status.Error(codes.InvalidArgument, "first message must include meta with sha1/container/filename")
	}
	if !isValidSha1Hex40(meta.GetSha1()) {
		return status.Error(codes.InvalidArgument, "invalid sha1 (expected 40 hex)")
	}

	dir := filepath.Join(n.rootPath, meta.GetContainer())
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return status.Errorf(codes.Internal, "IO_ERROR: mkdir %s: %v", dir, err)
	}

	finalPath := n.filePath(meta.GetContainer(), meta.GetFilename())
	tmpPath := finalPath + ".tmp"

	tf, err := os.OpenFile(tmpPath, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0o644)
	if err != nil {
		return status.Errorf(codes.Internal, "IO_ERROR: create tmp: %v", err)
	}

	written := int64(0)

	if b := first.GetData(); len(b) > 0 {
		nn, werr := tf.Write(b)
		written += int64(nn)
		if werr != nil {
			_ = tf.Close()
			return status.Errorf(codes.Internal, "IO_ERROR: write: %v", werr)
		}
	}

	for {
		msg, rerr := stream.Recv()
		if rerr == io.EOF {
			break
		}
		if rerr != nil {
			_ = tf.Close()
			return status.Errorf(codes.Internal, "IO_ERROR: recv: %v", rerr)
		}
		if len(msg.GetData()) == 0 {
			continue
		}
		nn, werr := tf.Write(msg.GetData())
		written += int64(nn)
		if werr != nil {
			_ = tf.Close()
			return status.Errorf(codes.Internal, "IO_ERROR: write: %v", werr)
		}
	}

	if err := tf.Close(); err != nil {
		return status.Errorf(codes.Internal, "IO_ERROR: close tmp: %v", err)
	}

	if meta.GetSize() > 0 && written != meta.GetSize() {
		_ = os.Remove(tmpPath)
		return status.Errorf(codes.InvalidArgument, "SIZE_MISMATCH: written=%d meta.size=%d", written, meta.GetSize())
	}

	if _, err := os.Stat(finalPath); err == nil {
		_ = os.Remove(tmpPath)
		_ = n.saveMeta(ctx, meta)
		return stream.SendAndClose(&libraryv1.PutResponse{Stored: false, Meta: meta})
	}

	if err := os.Rename(tmpPath, finalPath); err != nil {
		_ = os.Remove(tmpPath)
		return status.Errorf(codes.Internal, "IO_ERROR: rename: %v", err)
	}

	if err := n.saveMeta(ctx, meta); err != nil {
		return status.Errorf(codes.Internal, "INTERNAL: save meta: %v", err)
	}

	return stream.SendAndClose(&libraryv1.PutResponse{Stored: true, Meta: meta})
}

func (n *Node) ensureLocal(ctx context.Context, sha1 string) error {
	if !isValidSha1Hex40(sha1) {
		return status.Error(codes.InvalidArgument, "invalid sha1 (expected 40 hex)")
	}

	if m, err := n.loadMeta(ctx, sha1); err == nil && n.hasLocalFile(m) {
		return nil
	}

	n.mu.Lock()
	if in, ok := n.sf[sha1]; ok {
		n.mu.Unlock()
		in.wg.Wait()
		return in.err
	}
	in := &inflight{}
	in.wg.Add(1)
	n.sf[sha1] = in
	n.mu.Unlock()

	in.err = n.fetchFromParent(ctx, sha1)

	n.mu.Lock()
	delete(n.sf, sha1)
	n.mu.Unlock()

	in.wg.Done()
	return in.err
}

func (n *Node) fetchFromParent(ctx context.Context, sha1 string) error {
	// IMPORTANT: parent теперь возвращает gRPC status codes.
	metaResp, err := n.parent.GetMeta(ctx, &libraryv1.GetMetaRequest{Id: &libraryv1.BookId{Sha1: sha1}})
	if err != nil {
		return err // прокидываем NotFound/Unavailable/DeadlineExceeded/etc
	}
	meta := metaResp.GetMeta()
	if meta == nil {
		return status.Error(codes.Internal, "UPSTREAM_ERROR: empty meta")
	}

	dir := filepath.Join(n.rootPath, meta.GetContainer())
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return status.Errorf(codes.Internal, "IO_ERROR: mkdir %s: %v", dir, err)
	}
	finalPath := n.filePath(meta.GetContainer(), meta.GetFilename())
	tmpPath := finalPath + ".tmp"

	if _, err := os.Stat(finalPath); err == nil {
		_ = n.saveMeta(ctx, meta)
		return nil
	}

	tf, err := os.OpenFile(tmpPath, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0o644)
	if err != nil {
		return status.Errorf(codes.Internal, "IO_ERROR: create tmp: %v", err)
	}

	st, err := n.parent.GetStream(ctx, &libraryv1.GetStreamRequest{Id: &libraryv1.BookId{Sha1: sha1}})
	if err != nil {
		_ = tf.Close()
		_ = os.Remove(tmpPath)
		return err // прокидываем gRPC статус
	}

	written := int64(0)
	for {
		ch, rerr := st.Recv()
		if rerr == io.EOF {
			break
		}
		if rerr != nil {
			_ = tf.Close()
			_ = os.Remove(tmpPath)

			// если это gRPC status от parent — прокидываем как есть
			if _, ok := status.FromError(rerr); ok {
				return rerr
			}
			return status.Errorf(codes.Internal, "UPSTREAM_STREAM_ERROR: %v", rerr)
		}
		if len(ch.GetData()) == 0 {
			continue
		}
		nn, werr := tf.Write(ch.GetData())
		written += int64(nn)
		if werr != nil {
			_ = tf.Close()
			_ = os.Remove(tmpPath)
			return status.Errorf(codes.Internal, "IO_ERROR: write: %v", werr)
		}
	}

	if err := tf.Close(); err != nil {
		_ = os.Remove(tmpPath)
		return status.Errorf(codes.Internal, "IO_ERROR: close tmp: %v", err)
	}

	if meta.GetSize() > 0 && written != meta.GetSize() {
		_ = os.Remove(tmpPath)
		return status.Errorf(codes.Internal, "SIZE_MISMATCH: written=%d meta.size=%d", written, meta.GetSize())
	}

	if err := os.Rename(tmpPath, finalPath); err != nil {
		_ = os.Remove(tmpPath)
		return status.Errorf(codes.Internal, "IO_ERROR: rename: %v", err)
	}

	if err := n.saveMeta(ctx, meta); err != nil {
		return status.Errorf(codes.Internal, "INTERNAL: save meta: %v", err)
	}

	return nil
}

func IsValidAddr(addr string) bool {
	_, _, err := net.SplitHostPort(addr)
	return err == nil
}
