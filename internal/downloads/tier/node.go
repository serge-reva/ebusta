package tier

import (
    "context"
    "database/sql"
    "errors"
    "fmt"
    "io"
    "net"
    "os"
    "path/filepath"
    "sync"

    libraryv1 "ebusta/api/proto/v1"
    "ebusta/internal/config"
    "ebusta/internal/errutil"
    "ebusta/internal/logger"
    ds "ebusta/internal/downloads/sqlite"

    _ "modernc.org/sqlite"

    "google.golang.org/grpc"
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
    ParentAddr string
    MTLS       config.GRPCTLSConfig
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

    creds, err := cfg.MTLS.ClientTransportCredentials()
    if err != nil {
        _ = db.Close()
        return nil, err
    }
    conn, err := grpc.Dial(cfg.ParentAddr, grpc.WithTransportCredentials(creds))
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
    traceID := errutil.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("tier")
    }

    sha1 := req.GetId().GetSha1()
    if !isValidSha1Hex40(sha1) {
        logger.GetGlobal().WithField("sha1", sha1).WarnCtx(ctx, "[tier] Has: invalid sha1")
        return nil, errutil.ToGRPCError(errutil.New(
            errutil.CodeInvalidArgument,
            "invalid sha1 (expected 40 hex)",
        ).WithTrace(traceID))
    }

    m, err := n.loadMeta(ctx, sha1)
    if err == sql.ErrNoRows {
        return &libraryv1.HasResponse{Exists: false}, nil
    }
    if err != nil {
        logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[tier] Has: db error", err)
        return nil, errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            err.Error(),
        ).WithTrace(traceID))
    }
    if !n.hasLocalFile(m) {
        return &libraryv1.HasResponse{Exists: false}, nil
    }
    return &libraryv1.HasResponse{Exists: true}, nil
}

func (n *Node) GetMeta(ctx context.Context, req *libraryv1.GetMetaRequest) (*libraryv1.GetMetaResponse, error) {
    traceID := errutil.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("tier")
    }

    sha1 := req.GetId().GetSha1()
    if !isValidSha1Hex40(sha1) {
        logger.GetGlobal().WithField("sha1", sha1).WarnCtx(ctx, "[tier] GetMeta: invalid sha1")
        return nil, errutil.ToGRPCError(errutil.New(
            errutil.CodeInvalidArgument,
            "invalid sha1 (expected 40 hex)",
        ).WithTrace(traceID))
    }

    m, err := n.loadMeta(ctx, sha1)
    if err == sql.ErrNoRows || (err == nil && !n.hasLocalFile(m)) {
        if err2 := n.ensureLocal(ctx, sha1, traceID); err2 != nil {
            return nil, err2
        }

        m2, err2 := n.loadMeta(ctx, sha1)
        if err2 == sql.ErrNoRows {
            logger.GetGlobal().WithField("sha1", sha1).InfoCtx(ctx, "[tier] GetMeta: not found after ensure")
            return nil, errutil.ToGRPCError(errutil.New(
                errutil.CodeNotFound,
                "NOT_FOUND",
            ).WithTrace(traceID))
        }
        if err2 != nil {
            logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[tier] GetMeta: db error after ensure", err2)
            return nil, errutil.ToGRPCError(errutil.New(
                errutil.CodeInternal,
                err2.Error(),
            ).WithTrace(traceID))
        }
        if !n.hasLocalFile(m2) {
            return nil, errutil.ToGRPCError(errutil.New(
                errutil.CodeNotFound,
                "NOT_FOUND",
            ).WithTrace(traceID))
        }
        return &libraryv1.GetMetaResponse{Meta: m2}, nil
    }

    if err != nil {
        logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[tier] GetMeta: db error", err)
        return nil, errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            err.Error(),
        ).WithTrace(traceID))
    }

    return &libraryv1.GetMetaResponse{Meta: m}, nil
}

func (n *Node) GetStream(req *libraryv1.GetStreamRequest, stream libraryv1.StorageNode_GetStreamServer) error {
    ctx := stream.Context()
    traceID := errutil.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("tier")
    }

    sha1 := req.GetId().GetSha1()
    if !isValidSha1Hex40(sha1) {
        logger.GetGlobal().WithField("sha1", sha1).WarnCtx(ctx, "[tier] GetStream: invalid sha1")
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInvalidArgument,
            "invalid sha1 (expected 40 hex)",
        ).WithTrace(traceID))
    }

    m, err := n.loadMeta(ctx, sha1)
    if err == sql.ErrNoRows || (err == nil && !n.hasLocalFile(m)) {
        if err2 := n.ensureLocal(ctx, sha1, traceID); err2 != nil {
            return err2
        }
        m, err = n.loadMeta(ctx, sha1)
    }

    if err == sql.ErrNoRows {
        logger.GetGlobal().WithField("sha1", sha1).InfoCtx(ctx, "[tier] GetStream: not found")
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeNotFound,
            "NOT_FOUND",
        ).WithTrace(traceID))
    }
    if err != nil {
        logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[tier] GetStream: db error", err)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            err.Error(),
        ).WithTrace(traceID))
    }

    if !n.hasLocalFile(m) {
        logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[tier] GetStream: file missing on disk", nil)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeNotFound,
            "FILE_NOT_FOUND",
        ).WithTrace(traceID))
    }

    path := n.filePath(m.GetContainer(), m.GetFilename())
    f, err := os.Open(path)
    if err != nil {
        logger.GetGlobal().WithField("path", path).ErrorCtx(ctx, "[tier] GetStream: open error", err)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "cannot open file",
        ).WithTrace(traceID).WithDetails(err.Error()))
    }
    defer f.Close()

    buf := make([]byte, 64*1024)
    for {
        nread, rerr := f.Read(buf)
        if nread > 0 {
            if err := stream.Send(&libraryv1.Chunk{Data: buf[:nread]}); err != nil {
                logger.GetGlobal().ErrorCtx(ctx, "[tier] GetStream: send error", err)
                return errutil.ToGRPCError(errutil.New(
                    errutil.CodeInternal,
                    "stream send error",
                ).WithTrace(traceID).WithDetails(err.Error()))
            }
        }
        if rerr == io.EOF {
            break
        }
        if rerr != nil {
            logger.GetGlobal().ErrorCtx(ctx, "[tier] GetStream: read error", rerr)
            return errutil.ToGRPCError(errutil.New(
                errutil.CodeInternal,
                "file read error",
            ).WithTrace(traceID).WithDetails(rerr.Error()))
        }
    }

    logger.GetGlobal().WithField("sha1", sha1).WithField("size", m.GetSize()).InfoCtx(ctx, "[tier] GetStream: sent")
    return nil
}

func (n *Node) ensureLocal(ctx context.Context, sha1, traceID string) error {
    if !isValidSha1Hex40(sha1) {
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInvalidArgument,
            "invalid sha1 (expected 40 hex)",
        ).WithTrace(traceID))
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

    in.err = n.fetchFromParent(ctx, sha1, traceID)

    n.mu.Lock()
    delete(n.sf, sha1)
    n.mu.Unlock()

    in.wg.Done()
    return in.err
}

func (n *Node) fetchFromParent(ctx context.Context, sha1, traceID string) error {
    outCtx := errutil.ContextWithTraceID(ctx, traceID)
    metaResp, err := n.parent.GetMeta(outCtx, &libraryv1.GetMetaRequest{Id: &libraryv1.BookId{Sha1: sha1}})
    if err != nil {
        appErr := errutil.FromGRPCError(err, traceID)
        logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[tier] fetchFromParent: GetMeta error", err)
        return errutil.ToGRPCError(appErr)
    }
    meta := metaResp.GetMeta()
    if meta == nil {
        logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[tier] fetchFromParent: empty meta", nil)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "UPSTREAM_ERROR: empty meta",
        ).WithTrace(traceID))
    }

    dir := filepath.Join(n.rootPath, meta.GetContainer())
    if err := os.MkdirAll(dir, 0o755); err != nil {
        logger.GetGlobal().WithField("dir", dir).ErrorCtx(ctx, "[tier] fetchFromParent: mkdir error", err)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "IO_ERROR: mkdir",
        ).WithTrace(traceID).WithDetails(err.Error()))
    }
    finalPath := n.filePath(meta.GetContainer(), meta.GetFilename())
    tmpPath := finalPath + ".tmp"

    if _, err := os.Stat(finalPath); err == nil {
        _ = n.saveMeta(ctx, meta)
        return nil
    }

    tf, err := os.OpenFile(tmpPath, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0o644)
    if err != nil {
        logger.GetGlobal().WithField("tmp", tmpPath).ErrorCtx(ctx, "[tier] fetchFromParent: create tmp error", err)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "IO_ERROR: create tmp",
        ).WithTrace(traceID).WithDetails(err.Error()))
    }

    st, err := n.parent.GetStream(outCtx, &libraryv1.GetStreamRequest{Id: &libraryv1.BookId{Sha1: sha1}})
    if err != nil {
        _ = tf.Close()
        _ = os.Remove(tmpPath)
        appErr := errutil.FromGRPCError(err, traceID)
        logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[tier] fetchFromParent: GetStream error", err)
        return errutil.ToGRPCError(appErr)
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
            appErr := errutil.FromGRPCError(rerr, traceID)
            logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[tier] fetchFromParent: stream error", rerr)
            return errutil.ToGRPCError(appErr)
        }
        if len(ch.GetData()) == 0 {
            continue
        }
        nn, werr := tf.Write(ch.GetData())
        written += int64(nn)
        if werr != nil {
            _ = tf.Close()
            _ = os.Remove(tmpPath)
            logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[tier] fetchFromParent: write error", werr)
            return errutil.ToGRPCError(errutil.New(
                errutil.CodeInternal,
                "IO_ERROR: write",
            ).WithTrace(traceID).WithDetails(werr.Error()))
        }
    }

    if err := tf.Close(); err != nil {
        _ = os.Remove(tmpPath)
        logger.GetGlobal().WithField("tmp", tmpPath).ErrorCtx(ctx, "[tier] fetchFromParent: close tmp error", err)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "IO_ERROR: close tmp",
        ).WithTrace(traceID).WithDetails(err.Error()))
    }

    if meta.GetSize() > 0 && written != meta.GetSize() {
        _ = os.Remove(tmpPath)
        logger.GetGlobal().WithField("written", written).WithField("expected", meta.GetSize()).ErrorCtx(ctx, "[tier] fetchFromParent: size mismatch", nil)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "SIZE_MISMATCH",
        ).WithTrace(traceID).WithDetails(
            fmt.Sprintf("written=%d meta.size=%d", written, meta.GetSize())))
    }

    if err := os.Rename(tmpPath, finalPath); err != nil {
        _ = os.Remove(tmpPath)
        logger.GetGlobal().WithField("tmp", tmpPath).WithField("final", finalPath).ErrorCtx(ctx, "[tier] fetchFromParent: rename error", err)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "IO_ERROR: rename",
        ).WithTrace(traceID).WithDetails(err.Error()))
    }

    if err := n.saveMeta(ctx, meta); err != nil {
        logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[tier] fetchFromParent: save meta error", err)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "INTERNAL: save meta",
        ).WithTrace(traceID).WithDetails(err.Error()))
    }

    logger.GetGlobal().WithField("sha1", sha1).WithField("size", written).InfoCtx(ctx, "[tier] fetchFromParent: fetched")
    return nil
}

func IsValidAddr(addr string) bool {
    _, _, err := net.SplitHostPort(addr)
    return err == nil
}
