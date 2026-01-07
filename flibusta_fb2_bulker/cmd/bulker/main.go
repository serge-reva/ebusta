package main

import (
	"archive/zip"
	"bytes"
	"crypto/sha1"
	"encoding/hex"
	"encoding/json"
	"encoding/xml"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"golang.org/x/text/encoding/charmap"
	"golang.org/x/text/transform"
	"gopkg.in/yaml.v3"
)

type Config struct {
	OpenSearch struct {
		IndexName string `yaml:"index_name"`
	} `yaml:"opensearch"`
	Processing struct {
		Threads int `yaml:"threads"`
	} `yaml:"processing"`
}

type seq struct {
	Name   string   `json:"name,omitempty"`
	Number *float64 `json:"number,omitempty"`
}

type fileInfo struct {
	Container string `json:"container"`
	Filename  string `json:"filename"`
	Sha1      string `json:"sha1,omitempty"`
	Size      int64  `json:"size,omitempty"`
}

type docOut struct {
	DocId      string    `json:"docId,omitempty"`
	Title      string    `json:"title"`
	Authors    []string  `json:"authors,omitempty"`
	Genres     []string  `json:"genres,omitempty"`
	Languages  []string  `json:"languages,omitempty"`
	Sequences  []seq     `json:"sequences,omitempty"`
	IngestedAt time.Time `json:"ingestedAt"`
	FileInfo   fileInfo  `json:"fileInfo"`
}

var cfg Config

func main() {
	execPath, _ := os.Executable()
	defaultConfig := filepath.Join(filepath.Dir(execPath), "config.yaml")

	configPath := flag.String("config", defaultConfig, "Path to config file")
	srcPtr := flag.String("src", "", "Path to ZIP file")
	flag.Parse()

	cFile, err := os.ReadFile(*configPath)
	if err != nil {
		log.Fatalf("Error reading config [%s]: %v", *configPath, err)
	}
	yaml.Unmarshal(cFile, &cfg)

	if *srcPtr == "" {
		log.Fatal("Usage: bulker -src <path_to_zip>")
	}

	z, err := zip.OpenReader(*srcPtr)
	if err != nil {
		log.Fatal(err)
	}
	defer z.Close()

	jobs := make(chan *zip.File)
	var wg sync.WaitGroup

	for i := 0; i < cfg.Processing.Threads; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for f := range jobs {
				processFile(f, *srcPtr)
			}
		}()
	}

	for _, f := range z.File {
		if strings.HasSuffix(strings.ToLower(f.Name), ".fb2") {
			jobs <- f
		}
	}
	close(jobs)
	wg.Wait()
}

func processFile(f *zip.File, zipPath string) {
	rc, err := f.Open()
	if err != nil { return }
	defer rc.Close()

	raw, _ := io.ReadAll(rc)
	sum := sha1.Sum(raw)
	sha := hex.EncodeToString(sum[:])

	doc, err := parseFB2(raw)
	if err != nil { return }

	doc.IngestedAt = time.Now()
	doc.FileInfo = fileInfo{
		Container: filepath.Base(zipPath),
		Filename:  f.Name,
		Sha1:      sha,
		Size:      int64(len(raw)),
	}

	action := map[string]map[string]any{"index": {"_index": cfg.OpenSearch.IndexName, "_id": sha}}
	actBytes, _ := json.Marshal(action)
	docBytes, _ := json.Marshal(doc)

	fmt.Printf("%s\n%s\n", actBytes, docBytes)
}

func parseFB2(data []byte) (*docOut, error) {
	d := xml.NewDecoder(bytes.NewReader(data))
	d.CharsetReader = charsetReader
	var doc docOut
	var inTitle, inDoc, inAuthor bool
	var first, last string

	for {
		t, err := d.Token()
		if err == io.EOF { break }
		if err != nil { return nil, err }

		switch se := t.(type) {
		case xml.StartElement:
			switch se.Name.Local {
			case "title-info": inTitle = true
			case "document-info": inDoc = true
			case "book-title": 
				if inTitle { d.DecodeElement(&doc.Title, &se) }
			case "author": 
				if inTitle { inAuthor = true }
			case "first-name":
				if inAuthor { d.DecodeElement(&first, &se) }
			case "last-name":
				if inAuthor { d.DecodeElement(&last, &se) }
			case "genre":
				if inTitle {
					var g string
					d.DecodeElement(&g, &se)
					doc.Genres = append(doc.Genres, g)
				}
			case "lang":
				if inTitle {
					var l string
					d.DecodeElement(&l, &se)
					doc.Languages = append(doc.Languages, l)
				}
			case "sequence":
				if inTitle {
					var s seq
					for _, a := range se.Attr {
						if a.Name.Local == "name" { s.Name = a.Value }
						if a.Name.Local == "number" {
							var n float64
							fmt.Sscanf(a.Value, "%f", &n)
							s.Number = &n
						}
					}
					doc.Sequences = append(doc.Sequences, s)
				}
			case "id":
				if inDoc { d.DecodeElement(&doc.DocId, &se) }
			}
		case xml.EndElement:
			if se.Name.Local == "author" && inAuthor {
				name := strings.TrimSpace(first + " " + last)
				if name != "" { doc.Authors = append(doc.Authors, name) }
				inAuthor = false; first = ""; last = ""
			}
			if se.Name.Local == "title-info" { inTitle = false }
			if se.Name.Local == "document-info" { inDoc = false }
		}
	}
	return &doc, nil
}

func charsetReader(charset string, input io.Reader) (io.Reader, error) {
	switch strings.ToLower(charset) {
	case "windows-1251", "cp1251":
		return transform.NewReader(input, charmap.Windows1251.NewDecoder()), nil
	case "koi8-r":
		return transform.NewReader(input, charmap.KOI8R.NewDecoder()), nil
	}
	return input, nil
}
