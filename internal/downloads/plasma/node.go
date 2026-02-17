package plasma

import (
    "context"
    "errors"
    "expvar"
    "io"
    "sync"
    "time"

    libraryv1 "ebusta/api/proto/v1"
    "ebusta/internal/errutil"
    "ebusta/internal/logger"

    "google.golang.org/grpc"
    "google.golang.org/grpc/credentials/insecure"
)

var (
    hitsTotal      = expvar.NewInt("plasma_hits_total")
    missesTotal    = expvar.NewInt("plasma_misses_total")
    evictionsTotal = expvar.NewInt("plasma_evictions_total")

    itemsGauge     = expvar.NewInt("plasma_items")
    usedBytesGauge = expvar.NewInt("plasma_used_bytes")

    maxBytesGauge = expvar.NewInt("plasma_max_bytes")
    maxItemsGauge = expvar.NewInt("plasma_max_items")
)

type entry struct {
    meta      *libraryv1.BookMeta
    data      []byte
    hits      int64
    createdAt time.Time
}

type inflight struct {
    wg  sync.WaitGroup
    err error
}

type Config struct {
    ParentAddr string
    MaxBytes   int64
    MaxItems   int
}

type Node struct {
    libraryv1.UnimplementedStorageNodeServer

    parentAddr string
    parentConn *grpc.ClientConn
    parent     libraryv1.StorageNodeClient

    maxBytes int64
    maxItems int

    mu    sync.Mutex
    items map[string]*entry
    used  int64
    sf    map[string]*inflight
}

func New(cfg Config) (*Node, error) {
    if cfg.ParentAddr == "" {
        return nil, errors.New("ParentAddr is empty")
    }
    if cfg.MaxBytes <= 0 {
        return nil, errors.New("MaxBytes must be > 0")
    }
    if cfg.MaxItems <= 0 {
        return nil, errors.New("MaxItems must be > 0")
    }

    conn, err := grpc.Dial(cfg.ParentAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
    if err != nil {
        return nil, err
    }

    maxBytesGauge.Set(cfg.MaxBytes)
    maxItemsGauge.Set(int64(cfg.MaxItems))

    n := &Node{
        parentAddr: cfg.ParentAddr,
        parentConn: conn,
        parent:     libraryv1.NewStorageNodeClient(conn),
        maxBytes:   cfg.MaxBytes,
        maxItems:   cfg.MaxItems,
        items:      make(map[string]*entry),
        sf:         make(map[string]*inflight),
    }
    n.syncGaugesLocked()
    return n, nil
}

func (n *Node) Close() error {
    if n.parentConn != nil {
        return n.parentConn.Close()
    }
    return nil
}

func (n *Node) Has(ctx context.Context, req *libraryv1.HasRequest) (*libraryv1.HasResponse, error) {
    sha1 := req.GetId().GetSha1()

    n.mu.Lock()
    _, ok := n.items[sha1]
    n.mu.Unlock()

    return &libraryv1.HasResponse{Exists: ok}, nil
}

func (n *Node) GetMeta(ctx context.Context, req *libraryv1.GetMetaRequest) (*libraryv1.GetMetaResponse, error) {
    traceID := errutil.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("plasma")
    }

    sha1 := req.GetId().GetSha1()

    n.mu.Lock()
    if e, ok := n.items[sha1]; ok {
        e.hits++
        hitsTotal.Add(1)
        m := *e.meta
        n.syncGaugesLocked()
        n.mu.Unlock()
        l := logger.GetGlobal().WithField("sha1", sha1).WithField("hits", e.hits).WithField("size", len(e.data))
        l.InfoCtx(ctx, "[plasma] HIT GetMeta")
        return &libraryv1.GetMetaResponse{Meta: &m}, nil
    }
    missesTotal.Add(1)
    n.syncGaugesLocked()
    n.mu.Unlock()
    logger.GetGlobal().WithField("sha1", sha1).InfoCtx(ctx, "[plasma] MISS GetMeta")

    if err := n.ensureLocal(ctx, sha1, traceID); err != nil {
        appErr := errutil.FromGRPCError(err, traceID)
        if appErr.Code == errutil.CodeNotFound {
            return &libraryv1.GetMetaResponse{Error: &libraryv1.Error{Code: "NOT_FOUND", Message: appErr.Message}}, nil
        }
        return &libraryv1.GetMetaResponse{Error: &libraryv1.Error{Code: "INTERNAL", Message: appErr.Message}}, nil
    }

    n.mu.Lock()
    e := n.items[sha1]
    e.hits++
    hitsTotal.Add(1)
    m := *e.meta
    n.syncGaugesLocked()
    n.mu.Unlock()
    l := logger.GetGlobal().WithField("sha1", sha1).WithField("hits", e.hits).WithField("size", len(e.data))
    l.InfoCtx(ctx, "[plasma] HIT(after-fill) GetMeta")
    return &libraryv1.GetMetaResponse{Meta: &m}, nil
}

func (n *Node) GetStream(req *libraryv1.GetStreamRequest, stream libraryv1.StorageNode_GetStreamServer) error {
    ctx := stream.Context()
    traceID := errutil.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("plasma")
    }

    sha1 := req.GetId().GetSha1()

    n.mu.Lock()
    _, ok := n.items[sha1]
    if ok {
        hitsTotal.Add(1)
        n.syncGaugesLocked()
        n.mu.Unlock()
        logger.GetGlobal().WithField("sha1", sha1).InfoCtx(ctx, "[plasma] HIT GetStream")
    } else {
        missesTotal.Add(1)
        n.syncGaugesLocked()
        n.mu.Unlock()
        logger.GetGlobal().WithField("sha1", sha1).InfoCtx(ctx, "[plasma] MISS GetStream")
    }

    if err := n.ensureLocal(ctx, sha1, traceID); err != nil {
        return err
    }

    n.mu.Lock()
    e := n.items[sha1]
    e.hits++
    data := e.data
    n.syncGaugesLocked()
    n.mu.Unlock()

    const chunkSize = 256 * 1024
    for off := 0; off < len(data); off += chunkSize {
        end := off + chunkSize
        if end > len(data) {
            end = len(data)
        }
        if err := stream.Send(&libraryv1.Chunk{Data: data[off:end]}); err != nil {
            logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[plasma] GetStream send error", err)
            return errutil.ToGRPCError(errutil.New(
                errutil.CodeInternal,
                "stream send error",
            ).WithTrace(traceID).WithDetails(err.Error()))
        }
    }

    logger.GetGlobal().WithField("sha1", sha1).WithField("size", len(data)).InfoCtx(ctx, "[plasma] GetStream sent")
    return nil
}

func (n *Node) Put(stream libraryv1.StorageNode_PutServer) error {
    ctx := stream.Context()
    traceID := errutil.TraceIDFromContext(ctx)
    if traceID == "" {
        traceID = errutil.GenerateTraceID("plasma")
    }

    first, err := stream.Recv()
    if err != nil {
        logger.GetGlobal().ErrorCtx(ctx, "[plasma] Put recv error", err)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInvalidArgument,
            "missing first message",
        ).WithTrace(traceID).WithDetails(err.Error()))
    }
    meta := first.GetMeta()
    if meta == nil || meta.GetSha1() == "" || meta.GetContainer() == "" || meta.GetFilename() == "" {
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInvalidArgument,
            "meta with sha1/container/filename required",
        ).WithTrace(traceID))
    }

    buf := make([]byte, 0, meta.GetSize())
    if b := first.GetData(); len(b) > 0 {
        buf = append(buf, b...)
    }

    for {
        msg, rerr := stream.Recv()
        if rerr == io.EOF {
            break
        }
        if rerr != nil {
            logger.GetGlobal().ErrorCtx(ctx, "[plasma] Put stream error", rerr)
            return errutil.ToGRPCError(errutil.New(
                errutil.CodeInternal,
                "stream recv error",
            ).WithTrace(traceID).WithDetails(rerr.Error()))
        }
        if len(msg.GetData()) > 0 {
            buf = append(buf, msg.GetData()...)
        }
    }

    n.store(meta, buf)
    l := logger.GetGlobal().WithField("sha1", meta.GetSha1()).WithField("size", len(buf)).WithField("title", meta.GetTitle())
    l.InfoCtx(ctx, "[plasma] PUT stored")

    ps, perr := n.parent.Put(ctx)
    if perr != nil {
        logger.GetGlobal().ErrorCtx(ctx, "[plasma] PUT proxy error", perr)
        return stream.SendAndClose(&libraryv1.PutResponse{Stored: true, Meta: meta})
    }
    _ = ps.Send(&libraryv1.PutRequest{Meta: meta, Data: buf})
    _, _ = ps.CloseAndRecv()

    return stream.SendAndClose(&libraryv1.PutResponse{Stored: true, Meta: meta})
}

func (n *Node) ensureLocal(ctx context.Context, sha1, traceID string) error {
    n.mu.Lock()
    if _, ok := n.items[sha1]; ok {
        n.mu.Unlock()
        return nil
    }
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
    metaResp, err := n.parent.GetMeta(ctx, &libraryv1.GetMetaRequest{Id: &libraryv1.BookId{Sha1: sha1}})
    if err != nil {
        appErr := errutil.FromGRPCError(err, traceID)
        logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[plasma] fetchFromParent GetMeta error", err)
        return errutil.ToGRPCError(appErr)
    }
    if metaResp.GetError() != nil {
        if metaResp.GetError().GetCode() == "NOT_FOUND" {
            logger.GetGlobal().WithField("sha1", sha1).InfoCtx(ctx, "[plasma] fetchFromParent NOT_FOUND")
            return errutil.ToGRPCError(errutil.New(
                errutil.CodeNotFound,
                "NOT_FOUND",
            ).WithTrace(traceID))
        }
        logger.GetGlobal().WithField("sha1", sha1).WithField("message", metaResp.GetError().GetMessage()).ErrorCtx(ctx, "[plasma] fetchFromParent error", nil)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            metaResp.GetError().GetMessage(),
        ).WithTrace(traceID))
    }
    meta := metaResp.GetMeta()
    if meta == nil {
        logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[plasma] fetchFromParent empty meta", nil)
        return errutil.ToGRPCError(errutil.New(
            errutil.CodeInternal,
            "UPSTREAM_ERROR: empty meta",
        ).WithTrace(traceID))
    }

    st, err := n.parent.GetStream(ctx, &libraryv1.GetStreamRequest{Id: &libraryv1.BookId{Sha1: sha1}})
    if err != nil {
        appErr := errutil.FromGRPCError(err, traceID)
        logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[plasma] fetchFromParent GetStream error", err)
        return errutil.ToGRPCError(appErr)
    }

    buf := make([]byte, 0, meta.GetSize())
    for {
        ch, rerr := st.Recv()
        if rerr == io.EOF {
            break
        }
        if rerr != nil {
            appErr := errutil.FromGRPCError(rerr, traceID)
            logger.GetGlobal().WithField("sha1", sha1).ErrorCtx(ctx, "[plasma] fetchFromParent stream error", rerr)
            return errutil.ToGRPCError(appErr)
        }
        if len(ch.GetData()) > 0 {
            buf = append(buf, ch.GetData()...)
        }
    }

    n.store(meta, buf)
    l := logger.GetGlobal().WithField("sha1", sha1).WithField("size", len(buf)).WithField("title", meta.GetTitle())
    l.InfoCtx(ctx, "[plasma] FILL from parent")
    return nil
}

func (n *Node) store(meta *libraryv1.BookMeta, data []byte) {
    sha1 := meta.GetSha1()
    size := int64(len(data))

    n.mu.Lock()
    defer n.mu.Unlock()

    if old, ok := n.items[sha1]; ok {
        n.used -= int64(len(old.data))
        delete(n.items, sha1)
    }

    for (n.used+size > n.maxBytes) || (len(n.items)+1 > n.maxItems) {
        n.evictOneLocked()
    }

    n.items[sha1] = &entry{
        meta:      meta,
        data:      data,
        hits:      0,
        createdAt: time.Now(),
    }
    n.used += size
    n.syncGaugesLocked()
}

func (n *Node) evictOneLocked() {
    var victimKey string
    var victim *entry

    for k, e := range n.items {
        if victim == nil ||
            e.hits < victim.hits ||
            (e.hits == victim.hits && e.createdAt.Before(victim.createdAt)) {
            victimKey = k
            victim = e
        }
    }

    if victim == nil {
        return
    }

    delete(n.items, victimKey)
    n.used -= int64(len(victim.data))
    evictionsTotal.Add(1)
    n.syncGaugesLocked()
    logger.GetGlobal().WithField("sha1", victimKey).WithField("hits", victim.hits).WithField("size", len(victim.data)).Info("", "[plasma] EVICT")
}

func (n *Node) syncGaugesLocked() {
    itemsGauge.Set(int64(len(n.items)))
    usedBytesGauge.Set(n.used)
}
