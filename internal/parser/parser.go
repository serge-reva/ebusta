package parser

import (
	"fmt"
	"ebusta/api/proto/v1"
)

// ==========================================
// PUBLIC API
// ==========================================

// Parse - точка входа. Создает лексер и парсер.
func Parse(input string) *libraryv1.SearchQuery {
	l := newLexer(input)
	p := newParser(l)
	return p.parseSearchQuery()
}

// ==========================================
// PARSER LOGIC
// ==========================================

type Parser struct {
	l       *Lexer
	curTok  Token
	peekTok Token
}

func newParser(l *Lexer) *Parser {
	p := &Parser{l: l}
	p.nextToken()
	p.nextToken()
	return p
}

func (p *Parser) nextToken() {
	p.curTok = p.peekTok
	p.peekTok = p.l.NextToken()
}

// Expression -> Term { OR Term }
func (p *Parser) parseSearchQuery() *libraryv1.SearchQuery {
	if p.curTok.Type == TOKEN_EOF {
		return nil
	}
	return p.parseExpression()
}

func (p *Parser) parseExpression() *libraryv1.SearchQuery {
	left := p.parseTerm()

	for p.curTok.Type == TOKEN_OR {
		p.nextToken() // eat OR
		right := p.parseTerm()
		left = &libraryv1.SearchQuery{
			Node: &libraryv1.SearchQuery_Logical{
				Logical: &libraryv1.LogicalNode{
					Op:    libraryv1.LogicalOp_OR,
					Nodes: []*libraryv1.SearchQuery{left, right},
				},
			},
		}
	}
	return left
}

// Term -> Factor { AND Factor }
func (p *Parser) parseTerm() *libraryv1.SearchQuery {
	left := p.parseFactor()

	for p.curTok.Type == TOKEN_AND {
		p.nextToken() // eat AND
		right := p.parseFactor()
		left = &libraryv1.SearchQuery{
			Node: &libraryv1.SearchQuery_Logical{
				Logical: &libraryv1.LogicalNode{
					Op:    libraryv1.LogicalOp_AND,
					Nodes: []*libraryv1.SearchQuery{left, right},
				},
			},
		}
	}
	return left
}

// Factor -> ( Expr ) | NOT Factor | Filter
func (p *Parser) parseFactor() *libraryv1.SearchQuery {
	switch p.curTok.Type {
	case TOKEN_LPAREN:
		p.nextToken() // eat (
		exp := p.parseExpression()
		if p.curTok.Type != TOKEN_RPAREN {
			fmt.Println("Error: expected )") 
		}
		p.nextToken() // eat )
		return exp

	case TOKEN_NOT:
		p.nextToken() // eat NOT
		right := p.parseFactor()
		return &libraryv1.SearchQuery{
			Node: &libraryv1.SearchQuery_Negation{
				Negation: &libraryv1.NotNode{
					Node: right,
				},
			},
		}
	
	default:
		return p.parseFilter()
	}
}

// Filter -> IDENT [OP] VALUE
func (p *Parser) parseFilter() *libraryv1.SearchQuery {
	if p.curTok.Type == TOKEN_IDENT && (p.peekTok.Type == TOKEN_COLON || p.peekTok.Type == TOKEN_EQUALS || p.peekTok.Type == TOKEN_CONTAINS) {
		field := p.curTok.Value
		p.nextToken() // eat field
		
		var op libraryv1.Operator
		switch p.curTok.Type {
		case TOKEN_COLON:    op = libraryv1.Operator_OP_CONTAINS
		case TOKEN_EQUALS:   op = libraryv1.Operator_OP_EQUALS
		case TOKEN_CONTAINS: op = libraryv1.Operator_OP_CONTAINS
		}
		
		p.nextToken() // eat op
		
		value := p.curTok.Value
		p.nextToken() // eat value

		return &libraryv1.SearchQuery{
			Node: &libraryv1.SearchQuery_Filter{
				Filter: &libraryv1.FilterNode{
					Field:    field,
					Value:    value,
					Operator: op,
				},
			},
		}
	}

	// Implicit "any" search
	val := p.curTok.Value
	p.nextToken()
	
	return &libraryv1.SearchQuery{
		Node: &libraryv1.SearchQuery_Filter{
			Filter: &libraryv1.FilterNode{
				Field:    "any",
				Value:    val,
				Operator: libraryv1.Operator_OP_CONTAINS,
			},
		},
	}
}
