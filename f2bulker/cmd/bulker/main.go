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
	"fmt"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"github.com/schollz/progressbar/v3"
	"github.com/sirupsen/logrus"
	"golang.org/x/text/encoding/charmap"
	"golang.org/x/text/encoding/unicode"
	"gopkg.in/yaml.v3"
)

type Config struct {
	OpenSearch struct {
		IndexName string `yaml:"index_name"`
	} `yaml:"opensearch"`
	Paths struct {
		WarnDir   string `yaml:"warn_dir"`
		OutputDir string `yaml:"output_dir"`
		SourceDir string `yaml:"source_dir"`
	} `yaml:"paths"`
	Processing struct {
		Threads int `yaml:"threads"`
	} `yaml:"processing"`
}

type docOut struct {
	Title      string    `json:"title"`
	Authors    []string  `json:"authors,omitempty"`
	IngestedAt time.Time `json:"ingestedAt"`
	FileInfo   struct {
		Container string `json:"container"`
		Filename  string `json:"filename"`
		Sha1      string `json:"sha1"`
		Size      int64  `json:"size"`
	} `json:"fileInfo"`
}

var (
	cfg           Config
	log           = logrus.New()
	outFile       *os.File
	outMu         sync.Mutex
	bar           *progressbar.ProgressBar
	rescuedCount  int32
	flagRescan    *bool
	flagVerbose   *bool
	flagSuperFast *bool
)

func main() {
	configPath := flag.String("config", "./config.yaml", "Path to config file")
	container := flag.String("container", "", "Process specific ZIP")
	rescue := flag.Bool("rescue", false, "Rescue mode")
	flagRescan = flag.Bool("rescan", false, "Force rescan all")
	flagVerbose = flag.Bool("verbose", false, "Detailed check")
	flagSuperFast = flag.Bool("fast", false, "Ultra-fast skip if output exists")
	flag.Parse()

	cFile, err := os.ReadFile(*configPath)
	if err != nil {
		fmt.Printf("Error reading config: %v\n", err)
		os.Exit(1)
	}
	if err := yaml.Unmarshal(cFile, &cfg); err != nil {
		fmt.Printf("Error parsing YAML: %v\n", err)
		os.Exit(1)
	}

	log.SetFormatter(&logrus.TextFormatter{FullTimestamp: true, ForceColors: true})
	_ = os.MkdirAll(cfg.Paths.OutputDir, 0755)

	if *rescue {
		runRescueMode()
	} else if *container != "" {
		processSingleZip(filepath.Join(cfg.Paths.SourceDir, *container), filepath.Join(cfg.Paths.OutputDir, *container+".jsonl"))
	} else {
		archives, _ := filepath.Glob(filepath.Join(cfg.Paths.SourceDir, "*.zip"))
		for _, zipPath := range archives {
			processSingleZip(zipPath, filepath.Join(cfg.Paths.OutputDir, filepath.Base(zipPath)+".jsonl"))
		}
	}
}

func normalizeJSONL(path string) (int, error) {
	f, err := os.Open(path)
	if err != nil { return 0, err }
	defer f.Close()
	tmpPath := path + ".tmp"
	tmpFile, err := os.Create(tmpPath)
	if err != nil { return 0, err }
	defer tmpFile.Close()

	hashes := make(map[string]bool)
	scanner := bufio.NewScanner(f)
	re := regexp.MustCompile(`"_id":"([a-fA-F0-9]+)"`)
	count := 0
	for scanner.Scan() {
		line1 := scanner.Text()
		if strings.Contains(line1, `"_index"`) {
			match := re.FindStringSubmatch(line1)
			if len(match) > 1 {
				id := match[1]
				if scanner.Scan() {
					line2 := scanner.Text()
					if !hashes[id] {
						hashes[id] = true
						_, _ = tmpFile.WriteString(line1 + "\n")
						_, _ = tmpFile.WriteString(line2 + "\n")
						count++
					}
				}
			}
		}
	}
	_ = os.Rename(tmpPath, path)
	return count, nil
}

func countExistingDocs(path string) int {
	count := 0
	f, err := os.Open(path)
	if err != nil { return 0 }
	defer f.Close()
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		if strings.Contains(scanner.Text(), `"_index"`) { count++ }
	}
	return count
}

func loadExistingHashes(path string) map[string]bool {
	hashes := make(map[string]bool)
	f, err := os.Open(path)
	if err != nil { return hashes }
	defer f.Close()
	scanner := bufio.NewScanner(f)
	re := regexp.MustCompile(`"_id":"([a-fA-F0-9]+)"`)
	for scanner.Scan() {
		line := scanner.Text()
		if strings.Contains(line, `"_index"`) {
			match := re.FindStringSubmatch(line)
			if len(match) > 1 { hashes[match[1]] = true }
		}
	}
	return hashes
}

func processSingleZip(zipPath, dstPath string) {
	containerName := filepath.Base(zipPath)
	if *flagSuperFast && !*flagRescan {
		if info, err := os.Stat(dstPath); err == nil && info.Size() > 0 {
			log.Infof("[%s] Fast-skip: exists.", containerName)
			os.Exit(10)
		}
	}

	z, err := zip.OpenReader(zipPath)
	if err != nil { return }
	defer z.Close()

	fb2Count := 0
	for _, f := range z.File {
		if strings.HasSuffix(strings.ToLower(f.Name), ".fb2") { fb2Count++ }
	}

	if !*flagRescan && !*flagVerbose {
		if jsonlCount := countExistingDocs(dstPath); jsonlCount > 0 {
			if fb2Count == jsonlCount {
				os.Exit(10)
			} else {
				newCount, _ := normalizeJSONL(dstPath)
				if newCount == fb2Count { os.Exit(10) }
			}
		}
	}

	existingHashes := make(map[string]bool)
	if !*flagRescan { existingHashes = loadExistingHashes(dstPath) }

	type workItem struct {
		file *zip.File
		raw  []byte
		sha  string
	}
	var tasks []workItem

	for _, f := range z.File {
		if !strings.HasSuffix(strings.ToLower(f.Name), ".fb2") { continue }
		
		if len(existingHashes) == 0 && !*flagRescan && !*flagVerbose {
			tasks = append(tasks, workItem{file: f})
			continue
		}

		rc, err := f.Open()
		if err != nil { continue }
		data, _ := io.ReadAll(rc)
		rc.Close()
		sum := sha1.Sum(data)
		sha := hex.EncodeToString(sum[:])
		if !existingHashes[sha] {
			tasks = append(tasks, workItem{file: f, raw: data, sha: sha})
		}
	}

	if len(tasks) == 0 { os.Exit(10) }

	openOutputFile(dstPath)
	defer outFile.Close()
	bar = progressbar.Default(int64(len(tasks)), "🚢 "+containerName)
	jobs := make(chan workItem)
	var wg sync.WaitGroup
	for i := 0; i < cfg.Processing.Threads; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for item := range jobs {
				if item.raw == nil {
					rc, err := item.file.Open()
					if err == nil {
						item.raw, _ = io.ReadAll(rc)
						rc.Close()
						sum := sha1.Sum(item.raw)
						item.sha = hex.EncodeToString(sum[:])
					}
				}
				if item.raw != nil {
					if doc, err := parseResilient(item.raw); err == nil {
						saveToOutputWithSha(item.file.Name, containerName, item.raw, item.sha, doc)
					}
				}
				_ = bar.Add(1)
			}
		}()
	}
	for _, t := range tasks { jobs <- t }
	close(jobs)
	wg.Wait()
}

func runRescueMode() {
	files, _ := filepath.Glob(filepath.Join(cfg.Paths.WarnDir, "*fb2"))
	if len(files) == 0 { return }
	dstPath := filepath.Join(cfg.Paths.OutputDir, "rescued_items.jsonl")
	openOutputFile(dstPath)
	defer outFile.Close()
	jobs := make(chan string)
	var wg sync.WaitGroup
	for i := 0; i < cfg.Processing.Threads; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for path := range jobs {
				data, err := os.ReadFile(path)
				if err != nil { continue }
				if doc, err := parseResilient(data); err == nil {
					if saveToOutput(filepath.Base(path), "rescued", data, doc) {
						_ = os.Remove(path)
						atomic.AddInt32(&rescuedCount, 1)
					}
				}
			}
		}()
	}
	for _, f := range files { jobs <- f }
	close(jobs)
	wg.Wait()
}

func parseResilient(data []byte) (*docOut, error) {
	utf8Data := convertToUTF8(data)
	if doc, err := parseFB2(utf8Data); err == nil { return doc, nil }
	return parseWithRegex(utf8Data)
}

func convertToUTF8(data []byte) []byte {
	if len(data) < 2 { return data }
	if (data[0] == 0xFF && data[1] == 0xFE) || (data[0] == 0xFE && data[1] == 0xFF) {
		dec := unicode.UTF16(unicode.LittleEndian, unicode.UseBOM).NewDecoder()
		out, _ := dec.Bytes(data)
		return out
	}
	header := string(data[:min(len(data), 500)])
	if strings.Contains(strings.ToLower(header), "windows-1251") {
		out, _ := charmap.Windows1251.NewDecoder().Bytes(data)
		return out
	}
	return bytes.ToValidUTF8(data, []byte(" "))
}

func parseWithRegex(data []byte) (*docOut, error) {
	doc := &docOut{}
	reTitle := regexp.MustCompile(`(?is)<book-title[^>]*>(.*?)</book-title>`)
	if m := reTitle.FindSubmatch(data); len(m) > 1 { doc.Title = string(m[1]) }
	if doc.Title == "" { return nil, fmt.Errorf("regex failed") }
	return doc, nil
}

func openOutputFile(path string) {
	var err error
	outFile, err = os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		log.Fatalf("Critical: failed to open output file: %v", err)
	}
}

func saveToOutput(filename, container string, raw []byte, doc *docOut) bool {
	sum := sha1.Sum(raw)
	sha := hex.EncodeToString(sum[:])
	return saveToOutputWithSha(filename, container, raw, sha, doc)
}

func saveToOutputWithSha(filename, container string, raw []byte, sha string, doc *docOut) bool {
	doc.FileInfo.Container, doc.FileInfo.Filename, doc.FileInfo.Sha1, doc.FileInfo.Size = container, filename, sha, int64(len(raw))
	doc.IngestedAt = time.Now()
	action, _ := json.Marshal(map[string]map[string]any{"index": {"_index": cfg.OpenSearch.IndexName, "_id": sha}})
	data, _ := json.Marshal(doc)
	outMu.Lock()
	defer outMu.Unlock()
	_, _ = outFile.Write(append(action, '\n'))
	_, _ = outFile.Write(append(data, '\n'))
	return true
}

func parseFB2(data []byte) (*docOut, error) {
	var doc docOut
	d := xml.NewDecoder(bytes.NewReader(data))
	for {
		t, _ := d.Token()
		if t == nil { break }
		if se, ok := t.(xml.StartElement); ok && se.Name.Local == "book-title" {
			_ = d.DecodeElement(&doc.Title, &se)
		}
	}
	if doc.Title == "" { return nil, fmt.Errorf("no title") }
	return &doc, nil
}

func min(a, b int) int { if a < b { return a }; return b }
