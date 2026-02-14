package tier

import (
    "context"
    "database/sql"
    "errors"
    "fmt"
    "io"
    "log"
    "net"
    "os"
    "path/filepath"
    "sync"

    libraryv1 "ebusta/api/proto/v1"
    "ebusta/internal/errutil"
    ds "ebusta/internal/downloads/sqlite"

    _ "modernc.org/sqlite"

    "google.golang.org/grpc"
    "google.golang.org/grpc/credentials/insecure"
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
    traceID := errutil.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("tier")
    }

    sha1 := req.GetId().GetSha1()
    if !isValidSha1Hex40(sha1) {
        log.Printf("[%s] Has: invalid sha1", traceID)
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
        log.Printf("[%s] Has: db error: %v", traceID, err)
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
        log.Printf("[%s] GetMeta: invalid sha1", traceID)
        return nil, errutil.ToGRPCError(errutil.New(
            errutil.CodeInvalidArgument,
            "invalid sha1 (expected 40 hex)",
        ).WithTrace(traceID))
    }

    m, err := n.loadMeta(ctx, sha1)
    // miss в sqlite или нет файла на диске -> пытаемся подтянуть у parent
    if err == sql.ErrNoRows || (err == nil && !n.hasLocalFile(m)) {
        if err2 := n.ensureLocal(ctx, sha1, traceID); err2 != nil {
            return nil, err2
        }

        m2, err2 := n.loadMeta(ctx, sha1)
        if err2 == sql.ErrNoRows {
            log.Printf("[%s] GetMeta: not found after ensure sha1=%s", traceID, sha1)
            return nil, errutil.ToGRPCError(errutil.New(
                errutil.CodeNotFound,
                "NOT_FOUND",
            ).WithTrace(traceID))
        }
        if err2 != nil {
            log.Printf("[%s] GetMeta: db error after ensure: %v", traceID, err2)
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
        log.Printf("[%s] GetMeta: db error: %v", traceID, err)
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
        log.Printf("[%s] GetStream: invalid sha1", traceID)
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
        log.Printf("[%s] GetStream: not found sha1=%s", traceID, sha1)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeNotFound,
            "NOT_FOUND",
        ).WithTrace(traceID))
    }
    if err != nil {
        log.Printf("[%s] GetStream: db error: %v", traceID, err)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            err.Error(),
        ).WithTrace(traceID))
    }

    if !n.hasLocalFile(m) {
        log.Printf("[%s] GetStream: file missing on disk sha1=%s", traceID, sha1)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeNotFound,
            "FILE_NOT_FOUND",
        ).WithTrace(traceID))
    }

    path := n.filePath(m.GetContainer(), m.GetFilename())
    f, err := os.Open(path)
    if err != nil {
        log.Printf("[%s] GetStream: open error: %v", traceID, err)
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
                log.Printf("[%s] GetStream: send error: %v", traceID, err)
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
            log.Printf("[%s] GetStream: read error: %v", traceID, rerr)
            return errutil.ToGRPCError(errutil.New(
                errutil.CodeInternal,
                "file read error",
            ).WithTrace(traceID).WithDetails(rerr.Error()))
        }
    }

    log.Printf("[%s] GetStream: sent sha1=%s size=%d", traceID, sha1, m.GetSize())
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
    // IMPORTANT: parent возвращает gRPC status codes.
    metaResp, err := n.parent.GetMeta(ctx, &libraryv1.GetMetaRequest{Id: &libraryv1.BookId{Sha1: sha1}})
    if err != nil {
        appErr := errutil.FromGRPCError(err, traceID)
        log.Printf("[%s] fetchFromParent: GetMeta error: %v", traceID, err)
        return errutil.ToGRPCError(appErr)
    }
    meta := metaResp.GetMeta()
    if meta == nil {
        log.Printf("[%s] fetchFromParent: empty meta", traceID)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "UPSTREAM_ERROR: empty meta",
        ).WithTrace(traceID))
    }

    dir := filepath.Join(n.rootPath, meta.GetContainer())
    if err := os.MkdirAll(dir, 0o755); err != nil {
        log.Printf("[%s] fetchFromParent: mkdir error: %v", traceID, err)
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
        log.Printf("[%s] fetchFromParent: create tmp error: %v", traceID, err)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "IO_ERROR: create tmp",
        ).WithTrace(traceID).WithDetails(err.Error()))
    }

    st, err := n.parent.GetStream(ctx, &libraryv1.GetStreamRequest{Id: &libraryv1.BookId{Sha1: sha1}})
    if err != nil {
        _ = tf.Close()
        _ = os.Remove(tmpPath)
        appErr := errutil.FromGRPCError(err, traceID)
        log.Printf("[%s] fetchFromParent: GetStream error: %v", traceID, err)
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
            log.Printf("[%s] fetchFromParent: stream error: %v", traceID, rerr)
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
            log.Printf("[%s] fetchFromParent: write error: %v", traceID, werr)
            return errutil.ToGRPCError(errutil.New(
                errutil.CodeInternal,
                "IO_ERROR: write",
            ).WithTrace(traceID).WithDetails(werr.Error()))
        }
    }

    if err := tf.Close(); err != nil {
        _ = os.Remove(tmpPath)
        log.Printf("[%s] fetchFromParent: close tmp error: %v", traceID, err)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "IO_ERROR: close tmp",
        ).WithTrace(traceID).WithDetails(err.Error()))
    }

    if meta.GetSize() > 0 && written != meta.GetSize() {
        _ = os.Remove(tmpPath)
        log.Printf("[%s] fetchFromParent: size mismatch written=%d meta.size=%d", traceID, written, meta.GetSize())
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "SIZE_MISMATCH",
        ).WithTrace(traceID).WithDetails(
            fmt.Sprintf("written=%d meta.size=%d", written, meta.GetSize())))
    }

    if err := os.Rename(tmpPath, finalPath); err != nil {
        _ = os.Remove(tmpPath)
        log.Printf("[%s] fetchFromParent: rename error: %v", traceID, err)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "IO_ERROR: rename",
        ).WithTrace(traceID).WithDetails(err.Error()))
    }

    if err := n.saveMeta(ctx, meta); err != nil {
        log.Printf("[%s] fetchFromParent: save meta error: %v", traceID, err)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "INTERNAL: save meta",
        ).WithTrace(traceID).WithDetails(err.Error()))
    }

    log.Printf("[%s] fetchFromParent: fetched sha1=%s size=%d", traceID, sha1, written)
    return nil
}

func IsValidAddr(addr string) bool {
    _, _, err := net.SplitHostPort(addr)
    return err == nil
}
