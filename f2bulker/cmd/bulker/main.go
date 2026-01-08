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
	cfg          Config
	log          = logrus.New()
	outFile      *os.File
	outMu        sync.Mutex
	bar          *progressbar.ProgressBar
	rescuedCount int32
	// Флаг принудительного пересканирования
	flagRescan *bool
)

func main() {
	configPath := flag.String("config", "./config.yaml", "Path to config file")
	container := flag.String("container", "", "Process only this specific ZIP from source_dir")
	rescue := flag.Bool("rescue", false, "Rescue mode")
	flagRescan = flag.Bool("rescan", false, "Force rescan all files ignoring existing output")
	flag.Parse()

	cFile, err := os.ReadFile(*configPath)
	if err != nil {
		fmt.Printf("Error: Cannot read config file at %s\n", *configPath)
		os.Exit(1)
	}
	_ = yaml.Unmarshal(cFile, &cfg)

	log.SetFormatter(&logrus.TextFormatter{FullTimestamp: true, ForceColors: true})
	_ = os.MkdirAll(cfg.Paths.OutputDir, 0755)

	if *rescue {
		runRescueMode()
		fmt.Printf("\n🏁 Rescue Finished. Successfully processed: %d files.\n", atomic.LoadInt32(&rescuedCount))
	} else if *container != "" {
		fullPath := filepath.Join(cfg.Paths.SourceDir, *container)
		dstPath := filepath.Join(cfg.Paths.OutputDir, *container+".jsonl")
		processSingleZip(fullPath, dstPath)
	} else {
		// Режим прохода по всем, но он редко используется с вашим скриптом
		archives, _ := filepath.Glob(filepath.Join(cfg.Paths.SourceDir, "*.zip"))
		for _, zipPath := range archives {
			dstPath := filepath.Join(cfg.Paths.OutputDir, filepath.Base(zipPath)+".jsonl")
			processSingleZip(zipPath, dstPath)
		}
	}
}

// Загружаем хеши (SHA1) уже обработанных файлов из jsonl
func loadExistingHashes(path string) map[string]bool {
	hashes := make(map[string]bool)
	f, err := os.Open(path)
	if err != nil {
		return hashes // Файла нет, значит хешей нет
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	// Ищем строку {"index":{"_id":"<SHA1>",...}}
	// Простой Regex для извлечения _id
	re := regexp.MustCompile(`"_id":"([a-fA-F0-9]+)"`)

	for scanner.Scan() {
		line := scanner.Text()
		if strings.Contains(line, `"_index"`) { // Это строка метаданных
			match := re.FindStringSubmatch(line)
			if len(match) > 1 {
				hashes[match[1]] = true
			}
		}
	}
	return hashes
}

func processSingleZip(zipPath, dstPath string) {
	containerName := filepath.Base(zipPath)

	z, err := zip.OpenReader(zipPath)
	if err != nil {
		log.Errorf("Failed to open zip %s: %v", zipPath, err)
		return
	}
	defer z.Close()

	// 1. Загружаем существующие хеши, если не задан флаг -rescan
	existingHashes := make(map[string]bool)
	if !*flagRescan {
		existingHashes = loadExistingHashes(dstPath)
		if len(existingHashes) > 0 {
			log.Infof("[%s] Found %d already processed documents.", containerName, len(existingHashes))
		}
	}

	// 2. Предварительный проход: определяем, какие файлы реально нужно обрабатывать
	type workItem struct {
		file *zip.File
		raw  []byte
		sha  string
	}
	var tasks []workItem

	// Читаем файлы, считаем хеш и проверяем наличие
	// Да, это создает нагрузку на чтение, но это единственный надежный способ проверить дубликат по содержимому
	for _, f := range z.File {
		if !strings.HasSuffix(strings.ToLower(f.Name), ".fb2") {
			continue
		}

		rc, err := f.Open()
		if err != nil {
			log.Errorf("Read error %s: %v", f.Name, err)
			continue
		}
		raw, _ := io.ReadAll(rc)
		rc.Close()

		sum := sha1.Sum(raw)
		sha := hex.EncodeToString(sum[:])

		if existingHashes[sha] {
			// Пишем в лог, что файл пропущен
			log.Infof("Skipping %s (already exists in output)", f.Name)
			continue
		}

		tasks = append(tasks, workItem{
			file: f,
			raw:  raw,
			sha:  sha,
		})
	}

	// 3. Если задач нет - выходим с кодом 10
	if len(tasks) == 0 {
		log.Infof("Container %s is fully processed. Nothing new.", containerName)
		// Важно: закрываем зип перед выходом (хотя defer сработает, но os.Exit жесткий)
		z.Close()
		os.Exit(10)
	}

	// 4. Обрабатываем то, что осталось
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
				// Используем уже прочитанные байты (raw)
				doc, err := parseResilient(item.raw)
				
				if err != nil {
					log.Errorf("FAILED: %s | %v", item.file.Name, err)
					saveToWarn(item.file.Name, item.raw, err)
				} else {
					// Передаем SHA, который мы уже посчитали
					saveToOutputWithSha(item.file.Name, containerName, item.raw, item.sha, doc)
				}
				_ = bar.Add(1)
			}
		}()
	}

	for _, task := range tasks {
		jobs <- task
	}
	close(jobs)
	wg.Wait()
}

func runRescueMode() {
	files, _ := filepath.Glob(filepath.Join(cfg.Paths.WarnDir, "*fb2"))
	if len(files) == 0 { return }

	dstPath := filepath.Join(cfg.Paths.OutputDir, "rescued_items.jsonl")
	openOutputFile(dstPath)
	defer outFile.Close()

	bar = progressbar.Default(int64(len(files)), "🩹 Rescuing")
	jobs := make(chan string)
	var wg sync.WaitGroup

	for i := 0; i < cfg.Processing.Threads; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for path := range jobs {
				data, err := os.ReadFile(path)
				if err != nil || len(data) == 0 {
					_ = os.Remove(path)
					_ = os.Remove(path + ".log")
					_ = bar.Add(1)
					continue
				}
				doc, err := parseResilient(data)
				if err == nil {
					if saveToOutput(filepath.Base(path), "rescued", data, doc) {
						_ = os.Remove(path)
						_ = os.Remove(path + ".log")
						atomic.AddInt32(&rescuedCount, 1)
					}
				} else {
					log.Errorf("FAILED: %s | %v", filepath.Base(path), err)
				}
				_ = bar.Add(1)
			}
		}()
	}
	for _, f := range files { jobs <- f }
	close(jobs)
	wg.Wait()
}

func parseResilient(data []byte) (*docOut, error) {
	if len(data) == 0 { return nil, fmt.Errorf("empty file") }
	utf8Data := convertToUTF8(data)
	doc, err := parseFB2(utf8Data)
	if err == nil { return doc, nil }
	return parseWithRegex(utf8Data)
}

func convertToUTF8(data []byte) []byte {
	if len(data) < 2 { return data }
	if (data[0] == 0xFF && data[1] == 0xFE) || (data[0] == 0xFE && data[1] == 0xFF) {
		dec := unicode.UTF16(unicode.LittleEndian, unicode.UseBOM).NewDecoder()
		out, _ := dec.Bytes(data)
		return out
	}
	if len(data) > 10 && data[1] == 0 && data[3] == 0 {
		dec := unicode.UTF16(unicode.LittleEndian, unicode.IgnoreBOM).NewDecoder()
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
	if m := reTitle.FindSubmatch(data); len(m) > 1 {
		doc.Title = strings.TrimSpace(string(m[1]))
	}
	reAuthor := regexp.MustCompile(`(?is)<author[^>]*>(.*?)</author>`)
	reFirst := regexp.MustCompile(`(?is)<first-name[^>]*>(.*?)</first-name>`)
	reLast := regexp.MustCompile(`(?is)<last-name[^>]*>(.*?)</last-name>`)
	authors := reAuthor.FindAllSubmatch(data, -1)
	for _, a := range authors {
		fn, ln := reFirst.FindSubmatch(a[1]), reLast.FindSubmatch(a[1])
		name := ""
		if len(fn) > 1 { name += string(fn[1]) + " " }
		if len(ln) > 1 { name += string(ln[1]) }
		if name = strings.TrimSpace(name); name != "" { doc.Authors = append(doc.Authors, name) }
	}
	if doc.Title == "" { return nil, fmt.Errorf("regex failed") }
	return doc, nil
}

func openOutputFile(path string) {
	var err error
	// O_APPEND - чтобы дописывать в конец файла, если он существует
	outFile, err = os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil { log.Fatal(err) }
}

func saveToOutput(filename, container string, raw []byte, doc *docOut) bool {
	sum := sha1.Sum(raw)
	sha := hex.EncodeToString(sum[:])
	return saveToOutputWithSha(filename, container, raw, sha, doc)
}

// Новая функция для сохранения с уже известным SHA
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

func saveToWarn(filename string, data []byte, err error) {
	_ = os.WriteFile(filepath.Join(cfg.Paths.WarnDir, filename), data, 0644)
	_ = os.WriteFile(filepath.Join(cfg.Paths.WarnDir, filename+".log"), []byte(err.Error()), 0644)
}

func parseFB2(data []byte) (*docOut, error) {
	d := xml.NewDecoder(bytes.NewReader(data))
	d.CharsetReader = func(charset string, input io.Reader) (io.Reader, error) { return input, nil }
	d.Strict = false
	var doc docOut
	var inTitle bool
	for {
		t, err := d.Token()
		if err != nil || t == nil { break }
		switch se := t.(type) {
		case xml.StartElement:
			if se.Name.Local == "title-info" { inTitle = true }
			if se.Name.Local == "book-title" && inTitle { _ = d.DecodeElement(&doc.Title, &se) }
			if se.Name.Local == "author" && inTitle {
				var a struct { First string `xml:"first-name"`; Last string `xml:"last-name"` }
				_ = d.DecodeElement(&a, &se)
				if n := strings.TrimSpace(a.First + " " + a.Last); n != "" { doc.Authors = append(doc.Authors, n) }
			}
		case xml.EndElement:
			if se.Name.Local == "title-info" { inTitle = false }
		}
	}
	if doc.Title == "" { return nil, fmt.Errorf("xml: no title") }
	return &doc, nil
}

func min(a, b int) int { if a < b { return a }; return b }
