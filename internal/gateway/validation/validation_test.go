package validation

import (
	"testing"
)

func TestValidateSearch(t *testing.T) {
	validator := NewValidator()

	tests := []struct {
		name    string
		body    string
		wantErr bool
	}{
		{
			name:    "valid search",
			body:    `{"query":"author:king","page":1,"limit":10}`,
			wantErr: false,
		},
		{
			name:    "valid quoted search",
			body:    `{"query":"author:\"михаил булгаков\"","page":1,"limit":10}`,
			wantErr: false,
		},
		{
			name:    "missing query",
			body:    `{"page":1}`,
			wantErr: true,
		},
		{
			name:    "invalid characters",
			body:    `{"query":"SELECT * FROM books;"}`,
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := validator.ValidateSearch([]byte(tt.body))
			if (err != nil) != tt.wantErr {
				t.Errorf("ValidateSearch() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestValidateToken(t *testing.T) {
	validator := NewValidator()

	tests := []struct {
		name  string
		token string
		want  bool
	}{
		{
			name:  "valid token (43 chars - standard)",
			token: "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-",
			want:  true,
		},
		{
			name:  "valid token (32 chars - minimum)",
			token: "abcdefghijklmnopqrstuvwxyzABCDEFGH",
			want:  true,
		},
		{
			name:  "invalid token (too short - 31 chars)",
			token: "abcdefghijklmnopqrstuvwxyzABCDE",
			want:  false,
		},
		{
			name:  "invalid token (invalid chars - space)",
			token: "token with spaces",
			want:  false,
		},
		{
			name:  "invalid token (invalid chars - plus)",
			token: "token+with+plus",
			want:  false,
		},
		{
			name:  "invalid token (invalid chars - slash)",
			token: "token/with/slash",
			want:  false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := validator.ValidateToken(tt.token); got != tt.want {
				t.Errorf("ValidateToken(%q) = %v, want %v", tt.token, got, tt.want)
			}
		})
	}
}
