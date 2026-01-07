package parser

import (
	"strings"
)

// ==========================================
// LEXER DEFINITIONS
// ==========================================

type TokenType int

const (
	TOKEN_EOF TokenType = iota
	TOKEN_ERROR
	TOKEN_IDENT     // author, title, Кинг
	TOKEN_STRING    // "Стивен Кинг"
	TOKEN_COLON     // :
	TOKEN_AND       // AND
	TOKEN_OR        // OR
	TOKEN_NOT       // NOT, -
	TOKEN_LPAREN    // (
	TOKEN_RPAREN    // )
	TOKEN_EQUALS    // =
	TOKEN_CONTAINS  // ~
)

type Token struct {
	Type  TokenType
	Value string
	Pos   int
}

type Lexer struct {
	input  string
	pos    int
	start  int
	width  int
	tokens []Token
}

// newLexer создает лексер (используется в parser.go)
func newLexer(input string) *Lexer {
	return &Lexer{input: input}
}

// NextToken возвращает следующий токен (используется в parser.go)
func (l *Lexer) NextToken() Token {
	l.skipWhitespace()
	if l.pos >= len(l.input) {
		return Token{Type: TOKEN_EOF}
	}

	ch := l.input[l.pos]

	switch {
	case isLetter(ch):
		return l.scanIdentifier()
	case ch == '"':
		return l.scanString()
	case ch == ':':
		l.pos++
		return Token{Type: TOKEN_COLON, Value: ":"}
	case ch == '(':
		l.pos++
		return Token{Type: TOKEN_LPAREN, Value: "("}
	case ch == ')':
		l.pos++
		return Token{Type: TOKEN_RPAREN, Value: ")"}
	case ch == '-': // Минус как NOT
		l.pos++
		return Token{Type: TOKEN_NOT, Value: "-"}
	case ch == '=':
		l.pos++
		return Token{Type: TOKEN_EQUALS, Value: "="}
	case ch == '~':
		l.pos++
		return Token{Type: TOKEN_CONTAINS, Value: "~"}
	}

	return Token{Type: TOKEN_ERROR, Value: string(ch)}
}

func (l *Lexer) skipWhitespace() {
	for l.pos < len(l.input) && (l.input[l.pos] == ' ' || l.input[l.pos] == '\t') {
		l.pos++
	}
}

func (l *Lexer) scanIdentifier() Token {
	start := l.pos
	for l.pos < len(l.input) && isLetter(l.input[l.pos]) {
		l.pos++
	}
	lit := l.input[start:l.pos]
	
	switch strings.ToUpper(lit) {
	case "AND":
		return Token{Type: TOKEN_AND, Value: lit}
	case "OR":
		return Token{Type: TOKEN_OR, Value: lit}
	case "NOT":
		return Token{Type: TOKEN_NOT, Value: lit}
	}
	return Token{Type: TOKEN_IDENT, Value: lit}
}

func (l *Lexer) scanString() Token {
	l.pos++ // skip opening quote
	start := l.pos
	for l.pos < len(l.input) && l.input[l.pos] != '"' {
		l.pos++
	}
	lit := l.input[start:l.pos]
	if l.pos < len(l.input) {
		l.pos++ // skip closing quote
	}
	return Token{Type: TOKEN_STRING, Value: lit}
}

func isLetter(ch byte) bool {
	return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') || ch > 127 || ch == '_' || ch == '.'
}
