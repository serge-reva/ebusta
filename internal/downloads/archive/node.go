package archive

import (
    "archive/zip"
    "context"
    "database/sql"
    "io"
    "log"
    "path/filepath"

    libraryv1 "ebusta/api/proto/v1"
    "ebusta/internal/errutil"
    ds "ebusta/internal/downloads/sqlite"

    _ "modernc.org/sqlite"
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
    traceID := errutil.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("arch")
    }

    sha1 := req.GetId().GetSha1()
    if !isValidSha1Hex40(sha1) {
        log.Printf("[%s] Has: invalid sha1", traceID)
        return nil, errutil.ToGRPCError(errutil.New(
            errutil.CodeInvalidArgument,
            "invalid sha1 (expected 40 hex)",
        ).WithTrace(traceID))
    }

    _, err := n.getMeta(ctx, sha1)
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
    return &libraryv1.HasResponse{Exists: true}, nil
}

func (n *Node) GetMeta(ctx context.Context, req *libraryv1.GetMetaRequest) (*libraryv1.GetMetaResponse, error) {
    traceID := errutil.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("arch")
    }

    sha1 := req.GetId().GetSha1()
    if !isValidSha1Hex40(sha1) {
        log.Printf("[%s] GetMeta: invalid sha1", traceID)
        return nil, errutil.ToGRPCError(errutil.New(
            errutil.CodeInvalidArgument,
            "invalid sha1 (expected 40 hex)",
        ).WithTrace(traceID))
    }

    r, err := n.getMeta(ctx, sha1)
    if err == sql.ErrNoRows {
        log.Printf("[%s] GetMeta: not found sha1=%s", traceID, sha1)
        return nil, errutil.ToGRPCError(errutil.New(
            errutil.CodeNotFound,
            "NOT_FOUND",
        ).WithTrace(traceID))
    }
    if err != nil {
        log.Printf("[%s] GetMeta: db error: %v", traceID, err)
        return nil, errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            err.Error(),
        ).WithTrace(traceID))
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
    traceID := errutil.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("arch")
    }

    sha1 := req.GetId().GetSha1()
    if !isValidSha1Hex40(sha1) {
        log.Printf("[%s] GetStream: invalid sha1", traceID)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInvalidArgument,
            "invalid sha1 (expected 40 hex)",
        ).WithTrace(traceID))
    }

    r, err := n.getMeta(ctx, sha1)
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

    zipPath := filepath.Join(n.zipRoot, r.container)
    zr, err := zip.OpenReader(zipPath)
    if err != nil {
        log.Printf("[%s] GetStream: zip open error: %v", traceID, err)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "cannot open archive",
        ).WithTrace(traceID).WithDetails(err.Error()))
    }
    defer zr.Close()

    var file *zip.File
    for _, f := range zr.File {
        if f.Name == r.filename {
            file = f
            break
        }
    }
    if file == nil {
        log.Printf("[%s] GetStream: file not found in archive: %s/%s", traceID, r.container, r.filename)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeNotFound,
            "file not found in archive",
        ).WithTrace(traceID))
    }

    rc, err := file.Open()
    if err != nil {
        log.Printf("[%s] GetStream: file open error: %v", traceID, err)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "cannot open file in archive",
        ).WithTrace(traceID).WithDetails(err.Error()))
    }
    defer rc.Close()

    buf := make([]byte, 64*1024)
    for {
        n, err := rc.Read(buf)
        if n > 0 {
            if err := stream.Send(&libraryv1.Chunk{Data: buf[:n]}); err != nil {
                log.Printf("[%s] GetStream: send error: %v", traceID, err)
                return errutil.ToGRPCError(errutil.New(
                    errutil.CodeInternal,
                    "stream send error",
                ).WithTrace(traceID).WithDetails(err.Error()))
            }
        }
        if err == io.EOF {
            break
        }
        if err != nil {
            log.Printf("[%s] GetStream: read error: %v", traceID, err)
            return errutil.ToGRPCError(errutil.New(
                errutil.CodeInternal,
                "file read error",
            ).WithTrace(traceID).WithDetails(err.Error()))
        }
    }

    log.Printf("[%s] GetStream: sent sha1=%s size=%d", traceID, sha1, r.size)
    return nil
}
