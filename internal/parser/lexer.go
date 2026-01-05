package parser

import (
	"strings"
	"unicode"
)

type TokenType int

const (
	TokenError TokenType = iota
	TokenEOF
	TokenString
	TokenField
	TokenAnd
	TokenOr
	TokenNot
	TokenRegex
)

type Token struct {
	Type  TokenType
	Value string
}

type Lexer struct {
	input []rune
	pos   int
}

func NewLexer(input string) *Lexer {
	return &Lexer{input: []rune(input)}
}

func (l *Lexer) NextToken() Token {
	l.skipWhitespace()

	if l.pos >= len(l.input) {
		return Token{Type: TokenEOF}
	}

	// Обработка регулярных выражений
	if l.input[l.pos] == '/' {
		return l.readRegex()
	}

	// Читаем токен до пробела ИЛИ до двоеточия (если это поле)
	start := l.pos
	for l.pos < len(l.input) && !unicode.IsSpace(l.input[l.pos]) {
		// Если встретили двоеточие — это конец имени поля
		if l.input[l.pos] == ':' {
			l.pos++ // Включаем двоеточие в токен поля
			word := string(l.input[start:l.pos])
			return Token{Type: TokenField, Value: strings.TrimSuffix(word, ":")}
		}
		l.pos++
	}

	word := string(l.input[start:l.pos])
	upperWord := strings.ToUpper(word)

	switch upperWord {
	case "AND":
		return Token{Type: TokenAnd, Value: "AND"}
	case "OR":
		return Token{Type: TokenOr, Value: "OR"}
	case "NOT":
		return Token{Type: TokenNot, Value: "NOT"}
	}

	return Token{Type: TokenString, Value: word}
}

func (l *Lexer) skipWhitespace() {
	for l.pos < len(l.input) && unicode.IsSpace(l.input[l.pos]) {
		l.pos++
	}
}

func (l *Lexer) readRegex() Token {
	start := l.pos
	l.pos++
	for l.pos < len(l.input) && l.input[l.pos] != '/' {
		l.pos++
	}
	if l.pos < len(l.input) {
		l.pos++
	}
	return Token{Type: TokenRegex, Value: string(l.input[start:l.pos])}
}
