package parser

import (
	"ebusta/api/proto/v1"
	"strings"
)

type Parser struct {
	lexer *Lexer
	curr  Token
}

func NewParser(input string) *Parser {
	p := &Parser{lexer: NewLexer(input)}
	p.curr = p.lexer.NextToken()
	return p
}

func (p *Parser) next() {
	p.curr = p.lexer.NextToken()
}

// Parse — главная точка входа
func (p *Parser) Parse() *libraryv1.SearchQuery {
	return p.parseExpression()
}

// parseExpression обрабатывает AND/OR (самый низкий приоритет, верх дерева)
func (p *Parser) parseExpression() *libraryv1.SearchQuery {
	left := p.parseUnary()

	for p.curr.Type == TokenAnd || p.curr.Type == TokenOr {
		opType := p.curr.Type
		p.next()
		right := p.parseUnary()

		logicalOp := libraryv1.LogicalOp_AND
		if opType == TokenOr {
			logicalOp = libraryv1.LogicalOp_OR
		}

		left = &libraryv1.SearchQuery{
			Node: &libraryv1.SearchQuery_Logical{
				Logical: &libraryv1.LogicalNode{
					Op:    logicalOp,
					Nodes: []*libraryv1.SearchQuery{left, right},
				},
			},
		}
	}
	return left
}

// parseUnary обрабатывает NOT
func (p *Parser) parseUnary() *libraryv1.SearchQuery {
	if p.curr.Type == TokenNot {
		p.next()
		return &libraryv1.SearchQuery{
			Node: &libraryv1.SearchQuery_Negation{
				Negation: &libraryv1.NotNode{
					Node: p.parsePrimary(),
				},
			},
		}
	}
	return p.parsePrimary()
}

// parsePrimary обрабатывает конкретные фильтры
func (p *Parser) parsePrimary() *libraryv1.SearchQuery {
	if p.curr.Type == TokenField {
		field := p.curr.Value
		p.next()
		val := p.curr.Value
		p.next()

		return &libraryv1.SearchQuery{
			Node: &libraryv1.SearchQuery_Filter{
				Filter: &libraryv1.FilterNode{
					Field:    field,
					Value:    val,
					Operator: p.detectOperator(val),
				},
			},
		}
	}

	// По умолчанию Any
	val := p.curr.Value
	p.next()
	return &libraryv1.SearchQuery{
		Node: &libraryv1.SearchQuery_Filter{
			Filter: &libraryv1.FilterNode{
				Field:    "any",
				Value:    val,
				Operator: p.detectOperator(val),
			},
		},
	}
}

func (p *Parser) detectOperator(val string) libraryv1.Operator {
	if strings.HasPrefix(val, "/") && strings.HasSuffix(val, "/") {
		return libraryv1.Operator_OP_REGEX
	}
	return libraryv1.Operator_OP_CONTAINS
}
