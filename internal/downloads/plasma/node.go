package plasma

import (
	"context"
	"errors"
	"expvar"
	"io"
	"log"
	"sync"
	"time"

	libraryv1 "ebusta/api/proto/v1"

	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/credentials/insecure"
	"google.golang.org/grpc/status"
)

// expvar: будет видно в /debug/vars сразу после старта процесса
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
		items:     make(map[string]*entry),
		sf:        make(map[string]*inflight),
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
	_ = ctx
	sha1 := req.GetId().GetSha1()

	n.mu.Lock()
	_, ok := n.items[sha1]
	n.mu.Unlock()

	return &libraryv1.HasResponse{Exists: ok}, nil
}

func (n *Node) GetMeta(ctx context.Context, req *libraryv1.GetMetaRequest) (*libraryv1.GetMetaResponse, error) {
	sha1 := req.GetId().GetSha1()

	n.mu.Lock()
	if e, ok := n.items[sha1]; ok {
		e.hits++
		hitsTotal.Add(1)
		m := *e.meta
		n.syncGaugesLocked()
		n.mu.Unlock()
		log.Printf("[plasma] HIT GetMeta sha1=%s hits=%d size=%d", sha1, e.hits, len(e.data))
		return &libraryv1.GetMetaResponse{Meta: &m}, nil
	}
	missesTotal.Add(1)
	n.syncGaugesLocked()
	n.mu.Unlock()
	log.Printf("[plasma] MISS GetMeta sha1=%s", sha1)

	if err := n.ensureLocal(ctx, sha1); err != nil {
		if status.Code(err) == codes.NotFound {
			return &libraryv1.GetMetaResponse{Error: &libraryv1.Error{Code: "NOT_FOUND", Message: err.Error()}}, nil
		}
		return &libraryv1.GetMetaResponse{Error: &libraryv1.Error{Code: "INTERNAL", Message: err.Error()}}, nil
	}

	n.mu.Lock()
	e := n.items[sha1]
	e.hits++
	hitsTotal.Add(1)
	m := *e.meta
	n.syncGaugesLocked()
	n.mu.Unlock()
	log.Printf("[plasma] HIT(after-fill) GetMeta sha1=%s hits=%d size=%d", sha1, e.hits, len(e.data))
	return &libraryv1.GetMetaResponse{Meta: &m}, nil
}

func (n *Node) GetStream(req *libraryv1.GetStreamRequest, stream libraryv1.StorageNode_GetStreamServer) error {
	ctx := stream.Context()
	sha1 := req.GetId().GetSha1()

	n.mu.Lock()
	_, ok := n.items[sha1]
	if ok {
		hitsTotal.Add(1)
		n.syncGaugesLocked()
		n.mu.Unlock()
		log.Printf("[plasma] HIT GetStream sha1=%s", sha1)
	} else {
		missesTotal.Add(1)
		n.syncGaugesLocked()
		n.mu.Unlock()
		log.Printf("[plasma] MISS GetStream sha1=%s", sha1)
	}

	if err := n.ensureLocal(ctx, sha1); err != nil {
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
			return err
		}
	}
	return nil
}

func (n *Node) Put(stream libraryv1.StorageNode_PutServer) error {
	ctx := stream.Context()

	first, err := stream.Recv()
	if err != nil {
		return status.Errorf(codes.InvalidArgument, "missing first message: %v", err)
	}
	meta := first.GetMeta()
	if meta == nil || meta.GetSha1() == "" || meta.GetContainer() == "" || meta.GetFilename() == "" {
		return status.Error(codes.InvalidArgument, "meta with sha1/container/filename required")
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
			return rerr
		}
		if len(msg.GetData()) > 0 {
			buf = append(buf, msg.GetData()...)
		}
	}

	n.store(meta, buf)
	log.Printf("[plasma] PUT stored sha1=%s size=%d title=%q", meta.GetSha1(), len(buf), meta.GetTitle())

	// Проксируем PUT вниз, чтобы пройти “по пути tiers” (plasma -> parent -> ...)
	ps, perr := n.parent.Put(ctx)
	if perr != nil {
		log.Printf("[plasma] PUT proxy error: %v", perr)
		return stream.SendAndClose(&libraryv1.PutResponse{Stored: true, Meta: meta})
	}
	_ = ps.Send(&libraryv1.PutRequest{Meta: meta, Data: buf})
	_, _ = ps.CloseAndRecv()

	return stream.SendAndClose(&libraryv1.PutResponse{Stored: true, Meta: meta})
}

func (n *Node) ensureLocal(ctx context.Context, sha1 string) error {
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

	in.err = n.fetchFromParent(ctx, sha1)

	n.mu.Lock()
	delete(n.sf, sha1)
	n.mu.Unlock()

	in.wg.Done()
	return in.err
}

func (n *Node) fetchFromParent(ctx context.Context, sha1 string) error {
	metaResp, err := n.parent.GetMeta(ctx, &libraryv1.GetMetaRequest{Id: &libraryv1.BookId{Sha1: sha1}})
	if err != nil {
		return err
	}
	if metaResp.GetError() != nil {
		if metaResp.GetError().GetCode() == "NOT_FOUND" {
			return status.Error(codes.NotFound, "NOT_FOUND")
		}
		return status.Errorf(codes.Internal, "UPSTREAM_ERROR: %s", metaResp.GetError().GetMessage())
	}
	meta := metaResp.GetMeta()
	if meta == nil {
		return status.Error(codes.Internal, "UPSTREAM_ERROR: empty meta")
	}

	st, err := n.parent.GetStream(ctx, &libraryv1.GetStreamRequest{Id: &libraryv1.BookId{Sha1: sha1}})
	if err != nil {
		return err
	}

	buf := make([]byte, 0, meta.GetSize())
	for {
		ch, rerr := st.Recv()
		if rerr == io.EOF {
			break
		}
		if rerr != nil {
			return rerr
		}
		if len(ch.GetData()) > 0 {
			buf = append(buf, ch.GetData()...)
		}
	}

	n.store(meta, buf)
	log.Printf("[plasma] FILL from parent sha1=%s size=%d title=%q", sha1, len(buf), meta.GetTitle())
	return nil
}

func (n *Node) store(meta *libraryv1.BookMeta, data []byte) {
	sha1 := meta.GetSha1()
	size := int64(len(data))

	n.mu.Lock()
	defer n.mu.Unlock()

	// если ключ уже был — корректно пересчитать used
	if old, ok := n.items[sha1]; ok {
		n.used -= int64(len(old.data))
		delete(n.items, sha1)
	}

	// освобождаем место (hits + FIFO)
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
	log.Printf("[plasma] EVICT sha1=%s hits=%d size=%d", victimKey, victim.hits, len(victim.data))
}

func (n *Node) syncGaugesLocked() {
	itemsGauge.Set(int64(len(n.items)))
	usedBytesGauge.Set(n.used)
}
