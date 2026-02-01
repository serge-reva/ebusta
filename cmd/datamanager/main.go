package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net"
	"net/http"
	"regexp"
	"strings"

	libraryv1 "ebusta/api/proto/v1"
	"ebusta/internal/config"

	"google.golang.org/grpc"
)

var shaRegex = regexp.MustCompile(`^[a-f0-9]{40}$`)

func isSHA(s string) bool {
	return shaRegex.MatchString(strings.ToLower(s))
}

type storageServer struct {
	libraryv1.UnimplementedStorageServiceServer
	osBaseURL string
	indexName string
	debug     bool
}

func flattenPositiveTerms(q *libraryv1.SearchQuery, neg bool, out *[]string) {
	if q == nil {
		return
	}

	if f := q.GetFilter(); f != nil {
		if !neg {
			v := strings.TrimSpace(f.GetValue())
			if v != "" {
				*out = append(*out, v)
			}
		}
		return
	}

	if l := q.GetLogical(); l != nil {
		childNeg := neg
		if l.GetOp() == 3 { // NOT toggles negation for its children
			childNeg = !neg
		}
		for _, n := range l.GetNodes() {
			flattenPositiveTerms(n, childNeg, out)
		}
	}
}

func findFirstFieldValue(q *libraryv1.SearchQuery, field string) (string, bool) {
	var found string

	var walk func(*libraryv1.SearchQuery, bool)
	walk = func(n *libraryv1.SearchQuery, neg bool) {
		if n == nil || found != "" {
			return
		}

		if f := n.GetFilter(); f != nil {
			if !neg && strings.EqualFold(strings.TrimSpace(f.GetField()), field) {
				v := strings.TrimSpace(f.GetValue())
				if v != "" {
					found = v
				}
			}
			return
		}

		if l := n.GetLogical(); l != nil {
			childNeg := neg
			if l.GetOp() == 3 {
				childNeg = !neg
			}
			for _, c := range l.GetNodes() {
				walk(c, childNeg)
			}
		}
	}

	walk(q, false)
	if found == "" {
		return "", false
	}
	return found, true
}

func suggestTemplate(ast *libraryv1.SearchQuery) string {
	if ast == nil {
		return "fl_mixed_search"
	}
	if f := ast.GetFilter(); f != nil {
		if strings.EqualFold(strings.TrimSpace(f.GetField()), "author") {
			return "fl_author_exact"
		}
		return "fl_mixed_search"
	}
	// For complex logical AST: we currently degrade to mixed_search (terms flattening).
	return "fl_mixed_search"
}

func buildTemplateRequest(ast *libraryv1.SearchQuery, templateID string, limit, offset int32) (map[string]interface{}, string) {
	params := map[string]interface{}{
		"from": offset,
		"size": limit,
	}

	// Template param name mapping (matches scripts/sync_templates.sh)
	paramName := "query"
	if templateID == "fl_author_exact" {
		paramName = "author"
	}

	var value string

	if paramName == "author" {
		if v, ok := findFirstFieldValue(ast, "author"); ok {
			value = v
		} else {
			// degrade: no explicit author node found, use flattened terms
			terms := []string{}
			flattenPositiveTerms(ast, false, &terms)
			value = strings.Join(terms, " ")
		}
	} else {
		if f := ast.GetFilter(); f != nil {
			value = strings.TrimSpace(f.GetValue())
		} else {
			terms := []string{}
			flattenPositiveTerms(ast, false, &terms)
			value = strings.Join(terms, " ")
		}
	}

	value = strings.TrimSpace(value)
	params[paramName] = value
	return params, value
}

type osResponse struct {
	Hits struct {
		Total interface{} `json:"total"`
		Hits  []struct {
			Source struct {
				Title   string   `json:"title"`
				Authors []string `json:"authors"`
			} `json:"_source"`
			ID string `json:"_id"`
		} `json:"hits"`
	} `json:"hits"`
}

type osDocResponse struct {
	Found  bool   `json:"found"`
	ID     string `json:"_id"`
	Source struct {
		Title   string   `json:"title"`
		Authors []string `json:"authors"`
	} `json:"_source"`
}

func (s *storageServer) fetchByID(docID string) (*libraryv1.Book, error) {
	targetURL := fmt.Sprintf("%s/%s/_doc/%s", s.osBaseURL, s.indexName, docID)

	if s.debug {
		log.Printf("📤 [OS-REQ] GET %s", targetURL)
	}

	resp, err := http.Get(targetURL)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)

	var doc osDocResponse
	if err := json.Unmarshal(body, &doc); err != nil {
		return nil, err
	}

	if !doc.Found {
		if s.debug {
			log.Printf("📥 [OS-RESP] Document not found")
		}
		return nil, nil
	}

	if s.debug {
		log.Printf("📥 [OS-RESP] Found document: %s", doc.Source.Title)
	}

	return &libraryv1.Book{
		Id:      doc.ID,
		Title:   doc.Source.Title,
		Authors: doc.Source.Authors,
	}, nil
}

func (s *storageServer) executeSearch(templateID string, params map[string]interface{}) (*osResponse, error) {
	osReqBody := map[string]interface{}{
		"id":     templateID,
		"params": params,
	}

	jsonData, _ := json.Marshal(osReqBody)
	targetURL := fmt.Sprintf("%s/%s/_search/template", s.osBaseURL, s.indexName)

	if s.debug {
		log.Printf("📤 [OS-REQ] URL: %s | BODY: %s", targetURL, string(jsonData))
	}

	resp, err := http.Post(targetURL, "application/json", bytes.NewBuffer(jsonData))
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	body, _ := io.ReadAll(resp.Body)

	var osRaw osResponse
	if err := json.Unmarshal(body, &osRaw); err != nil {
		return nil, err
	}

	return &osRaw, nil
}

func extractTotal(osRaw *osResponse) int32 {
	var totalValue int32
	switch v := osRaw.Hits.Total.(type) {
	case float64:
		totalValue = int32(v)
	case map[string]interface{}:
		if val, ok := v["value"].(float64); ok {
			totalValue = int32(val)
		}
	}
	return totalValue
}

func buildResponse(osRaw *osResponse) *libraryv1.SearchResponse {
	totalValue := extractTotal(osRaw)

	res := &libraryv1.SearchResponse{Status: "ok"}
	for _, hit := range osRaw.Hits.Hits {
		res.Books = append(res.Books, &libraryv1.Book{
			Id:      hit.ID,
			Title:   hit.Source.Title,
			Authors: hit.Source.Authors,
		})
	}

	// FALLBACK: если хиты есть, а total 0 или не распарсился
	if totalValue == 0 && len(res.Books) > 0 {
		totalValue = int32(len(res.Books))
	}
	res.Total = totalValue

	return res
}

func (s *storageServer) SearchBooks(ctx context.Context, req *libraryv1.SearchRequest) (*libraryv1.SearchResponse, error) {
	ast := req.GetAst()
	if ast == nil {
		return nil, fmt.Errorf("missing ast; orchestrator must set SearchRequest.ast")
	}

	limit := req.GetLimit()
	if limit <= 0 {
		limit = 10
	}
	offset := req.GetOffset()
	if offset < 0 {
		offset = 0
	}

	// Check if this is an ID lookup (explicit id: field or SHA-like value in "any" field)
	if f := ast.GetFilter(); f != nil {
		field := strings.ToLower(strings.TrimSpace(f.GetField()))
		value := strings.TrimSpace(f.GetValue())

		isExplicitID := field == "id"
		isImplicitID := field == "any" && isSHA(value)

		if isExplicitID || isImplicitID {
			if s.debug {
				log.Printf("🔍 [ID-LOOKUP] field=%s value=%s explicit=%v implicit=%v", field, value, isExplicitID, isImplicitID)
			}

			book, err := s.fetchByID(value)
			if err != nil {
				log.Printf("❌ ID lookup error: %v", err)
				// Don't fail, try fallback if implicit
			}

			if book != nil {
				return &libraryv1.SearchResponse{
					Status: "ok",
					Total:  1,
					Books:  []*libraryv1.Book{book},
				}, nil
			}

			// If explicit id: and not found, return empty
			if isExplicitID {
				if s.debug {
					log.Printf("📥 [ID-LOOKUP] Not found, no fallback for explicit id:")
				}
				return &libraryv1.SearchResponse{Status: "ok", Total: 0}, nil
			}

			// If implicit (SHA in "any"), fallback to mixed search
			if s.debug {
				log.Printf("🔄 [FALLBACK] SHA not found as _id, trying fl_mixed_search")
			}
		}
	}

	// Normal template-based search flow
	templateID := strings.TrimSpace(req.GetTemplateId())
	if templateID == "" {
		templateID = suggestTemplate(ast)
	}

	params, queryValue := buildTemplateRequest(ast, templateID, limit, offset)
	if queryValue == "" {
		return nil, fmt.Errorf("empty query derived from AST")
	}

	osRaw, err := s.executeSearch(templateID, params)
	if err != nil {
		log.Printf("❌ Storage error: %v", err)
		return &libraryv1.SearchResponse{Status: "error"}, nil
	}

	totalValue := extractTotal(osRaw)

	if s.debug {
		log.Printf("📥 [OS-RESP] Found: %d books", totalValue)
	}

	// FALLBACK: если специфичный шаблон вернул 0 результатов - пробуем fl_mixed_search
	if totalValue == 0 && templateID != "fl_mixed_search" {
		if s.debug {
			log.Printf("🔄 [FALLBACK] %s returned 0, trying fl_mixed_search", templateID)
		}

		// Перестраиваем params для mixed_search
		fallbackParams := map[string]interface{}{
			"from":  offset,
			"size":  limit,
			"query": queryValue,
		}

		osRaw, err = s.executeSearch("fl_mixed_search", fallbackParams)
		if err != nil {
			log.Printf("❌ Storage fallback error: %v", err)
			return &libraryv1.SearchResponse{Status: "error"}, nil
		}

		if s.debug {
			log.Printf("📥 [OS-RESP FALLBACK] Found: %d books", extractTotal(osRaw))
		}
	}

	return buildResponse(osRaw), nil
}

func main() {
	cfg := config.Get()

	lis, err := net.Listen(cfg.Datamanager.Protocol, cfg.Datamanager.Address())
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	s := grpc.NewServer()
	libraryv1.RegisterStorageServiceServer(s, &storageServer{
		osBaseURL: cfg.OpenSearch.URL,
		indexName: cfg.OpenSearch.IndexName,
		debug:     cfg.OpenSearch.Debug,
	})

	log.Printf("💾 DataManager (Storage) started on %s (%s)", cfg.Datamanager.Address(), cfg.Datamanager.Protocol)
	if err := s.Serve(lis); err != nil {
		log.Fatalf("failed to serve: %v", err)
	}
}
