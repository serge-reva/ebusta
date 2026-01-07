package main

import (
	"archive/zip"
	"bufio"
	"bytes"
	"crypto/sha1"
	"encoding/hex"
	"encoding/json"
	"encoding/xml"
	"flag"
	"io"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/push"
	"github.com/sirupsen/logrus"
	"golang.org/x/text/encoding/htmlindex"
	"gopkg.in/yaml.v3"
)

var (
	registry = prometheus.NewRegistry()
	processedFiles = prometheus.NewCounterVec(prometheus.CounterOpts{
		Name: "f2bulker_processed_files_total",
		Help: "Total number of processed FB2 files",
	}, []string{"container", "status"})

	processingDuration = prometheus.NewHistogram(prometheus.HistogramOpts{
		Name:    "f2bulker_file_processing_duration_seconds",
		Help:    "Time spent processing a single FB2 file",
		Buckets: prometheus.DefBuckets,
	})
)

func init() {
	registry.MustRegister(processedFiles, processingDuration)
}

type Config struct {
	OpenSearch struct {
		IndexName string `yaml:"index_name"`
	} `yaml:"opensearch"`
	Paths struct {
		WarnDir string `yaml:"warn_dir"`
	} `yaml:"paths"`
	Processing struct {
		Threads int `yaml:"threads"`
	} `yaml:"processing"`
	Logging struct {
		LogPath string `yaml:"log_path"`
	} `yaml:"logging"`
	Metrics struct {
		PushgatewayURL string `yaml:"pushgateway_url"`
	} `yaml:"metrics"`
}

type docOut struct {
	DocId      string    `json:"docId,omitempty"`
	Title      string    `json:"title"`
	Authors    []string  `json:"authors,omitempty"`
	Genres     []string  `json:"genres,omitempty"`
	Languages  []string  `json:"languages,omitempty"`
	IngestedAt time.Time `json:"ingestedAt"`
	FileInfo   struct {
		Container string `json:"container"`
		Filename  string `json:"filename"`
		Sha1      string `json:"sha1"`
		Size      int64  `json:"size"`
	} `json:"fileInfo"`
}

var (
	cfg          Config
	log          = logrus.New()
	outFile      *os.File
	outMu        sync.Mutex
	processedMap = make(map[string]bool)
	workDone     int32
)

func initLogger(path string) {
	log.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true, TimestampFormat: "2006-01-02 15:04:05", ForceColors: path == "",
	})
	writers := []io.Writer{os.Stdout}
	if path != "" {
		if f, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644); err == nil {
			writers = append(writers, f)
		}
	}
	log.SetOutput(io.MultiWriter(writers...))
}

func loadExistingResults(path string) int {
	f, err := os.Open(path)
	if err != nil { return 0 }
	defer f.Close()
	scanner := bufio.NewScanner(f)
	count := 0
	for scanner.Scan() {
		line := scanner.Bytes()
		if bytes.Contains(line, []byte("\"fileInfo\"")) {
			var doc docOut
			if err := json.Unmarshal(line, &doc); err == nil {
				processedMap[doc.FileInfo.Filename] = true
				count++
			}
		}
	}
	return count
}

func main() {
	configPath := flag.String("config", "./config.yaml", "Path to config file")
	srcPath := flag.String("src", "", "Path to ZIP file")
	dstPath := flag.String("out", "", "Path to output JSONL file")
	rescan := flag.Bool("rescan", false, "Force rescan")
	flag.Parse()

	cFile, _ := os.ReadFile(*configPath)
	_ = yaml.Unmarshal(cFile, &cfg)
	initLogger(cfg.Logging.LogPath)

	if *srcPath == "" || *dstPath == "" { os.Exit(1) }

	containerName := filepath.Base(*srcPath)
	if !*rescan {
		count := loadExistingResults(*dstPath)
		if count > 0 {
			log.WithField("container", containerName).Infof("Incremental: %d files already in %s. Skipping.", count, filepath.Base(*dstPath))
		}
	}

	mode := os.O_APPEND | os.O_CREATE | os.O_WRONLY
	if *rescan { mode = os.O_CREATE | os.O_TRUNC | os.O_WRONLY }

	var errOpen error
	outFile, errOpen = os.OpenFile(*dstPath, mode, 0644)
	if errOpen != nil { os.Exit(1) }
	defer outFile.Close()

	z, err := zip.OpenReader(*srcPath)
	if err != nil { os.Exit(1) }
	defer z.Close()

	jobs := make(chan *zip.File)
	var wg sync.WaitGroup
	for i := 0; i < cfg.Processing.Threads; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for f := range jobs { processFile(f, *srcPath) }
		}()
	}

	fb2Found := 0
	for _, f := range z.File {
		if strings.HasSuffix(strings.ToLower(f.Name), ".fb2") {
			fb2Found++
			if !*rescan && processedMap[f.Name] {
				processedFiles.WithLabelValues(containerName, "skipped").Inc()
				continue
			}
			jobs <- f
		}
	}
	close(jobs)
	wg.Wait()

	if cfg.Metrics.PushgatewayURL != "" {
		_ = push.New(cfg.Metrics.PushgatewayURL, "f2bulker").Gatherer(registry).Grouping("container", containerName).Push()
	}

	if fb2Found > 0 && atomic.LoadInt32(&workDone) == 0 {
		log.WithField("container", containerName).Info("No new files to process.")
		os.Exit(10)
	}
}

func processFile(f *zip.File, zipPath string) {
	start := time.Now()
	rc, err := f.Open()
	if err != nil { return }
	defer rc.Close()

	raw, _ := io.ReadAll(rc)
	doc, err := parseFB2(raw)
	if err != nil {
		saveToWarn(f.Name, raw)
		processedFiles.WithLabelValues(filepath.Base(zipPath), "error_parse").Inc()
		return
	}

	atomic.AddInt32(&workDone, 1)
	
	sum := sha1.Sum(raw)
	sha := hex.EncodeToString(sum[:])
	doc.IngestedAt = time.Now()
	doc.FileInfo.Container = filepath.Base(zipPath)
	doc.FileInfo.Filename = f.Name
	doc.FileInfo.Sha1 = sha
	doc.FileInfo.Size = int64(len(raw))

	action, _ := json.Marshal(map[string]map[string]any{"index": {"_index": cfg.OpenSearch.IndexName, "_id": sha}})
	data, _ := json.Marshal(doc)

	outMu.Lock()
	_, _ = outFile.Write(action); _, _ = outFile.WriteString("\n")
	_, _ = outFile.Write(data); _, _ = outFile.WriteString("\n")
	outMu.Unlock()

	processedFiles.WithLabelValues(filepath.Base(zipPath), "success").Inc()
	processingDuration.Observe(time.Since(start).Seconds())
}

func saveToWarn(filename string, data []byte) {
	if cfg.Paths.WarnDir == "" { return }
	_ = os.MkdirAll(cfg.Paths.WarnDir, 0755)
	_ = os.WriteFile(filepath.Join(cfg.Paths.WarnDir, filename), data, 0644)
}

func parseFB2(data []byte) (*docOut, error) {
	d := xml.NewDecoder(bytes.NewReader(data))
	d.CharsetReader = charsetReader
	var doc docOut
	var inTitle, inAuthor bool
	var first, last string
	for {
		t, err := d.Token(); if err == io.EOF { break }; if err != nil { return nil, err }
		switch se := t.(type) {
		case xml.StartElement:
			switch se.Name.Local {
			case "title-info": inTitle = true
			case "book-title": if inTitle { _ = d.DecodeElement(&doc.Title, &se) }
			case "author": if inTitle { inAuthor = true }
			case "first-name": if inAuthor { _ = d.DecodeElement(&first, &se) }
			case "last-name": if inAuthor { _ = d.DecodeElement(&last, &se) }
			}
		case xml.EndElement:
			if se.Name.Local == "author" && inAuthor {
				name := strings.TrimSpace(first + " " + last)
				if name != "" { doc.Authors = append(doc.Authors, name) }
				inAuthor = false; first = ""; last = ""
			}
			if se.Name.Local == "title-info" { inTitle = false }
		}
	}
	return &doc, nil
}

func charsetReader(charset string, input io.Reader) (io.Reader, error) {
	enc, err := htmlindex.Get(charset)
	if err != nil {
		return input, nil
	}
	return enc.NewDecoder().Reader(input), nil
}
