package botcommand

import "testing"

func TestParseSearchCommand(t *testing.T) {
	cmd := Parse("/search tolstoy page 2")
	search, ok := cmd.(SearchCommand)
	if !ok {
		t.Fatalf("expected SearchCommand, got %T", cmd)
	}
	if search.Query != "tolstoy" || search.Page != 2 {
		t.Fatalf("unexpected search command: %+v", search)
	}
}

func TestParsePageCommand(t *testing.T) {
	cmd := Parse("/page 3")
	page, ok := cmd.(PageCommand)
	if !ok {
		t.Fatalf("expected PageCommand, got %T", cmd)
	}
	if page.Page != 3 {
		t.Fatalf("unexpected page command: %+v", page)
	}
}

func TestParseNavigationCommands(t *testing.T) {
	if _, ok := Parse("/next").(NextCommand); !ok {
		t.Fatalf("expected NextCommand")
	}
	if _, ok := Parse("/prev").(PrevCommand); !ok {
		t.Fatalf("expected PrevCommand")
	}
	if _, ok := Parse("/help").(HelpCommand); !ok {
		t.Fatalf("expected HelpCommand")
	}
}

func TestParseSelectBookCommand(t *testing.T) {
	cmd := Parse("/start book_7")
	selectCmd, ok := cmd.(SelectBookCommand)
	if !ok {
		t.Fatalf("expected SelectBookCommand, got %T", cmd)
	}
	if selectCmd.BookIndex != 7 {
		t.Fatalf("unexpected select command: %+v", selectCmd)
	}
}

func TestParseInvalidCommands(t *testing.T) {
	tests := []string{
		"",
		"/search",
		"/page",
		"/page x",
		"/unknown",
	}
	for _, tt := range tests {
		t.Run(tt, func(t *testing.T) {
			if _, ok := Parse(tt).(InvalidCommand); !ok {
				t.Fatalf("expected InvalidCommand for %q", tt)
			}
		})
	}
}
