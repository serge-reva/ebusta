package botcommand

type Kind string

const (
	KindInvalid Kind = "invalid"
	KindHelp    Kind = "help"
	KindSearch  Kind = "search"
	KindPage    Kind = "page"
	KindNext    Kind = "next"
	KindPrev    Kind = "prev"
	KindSelect  Kind = "select_book"
)

type Command interface {
	Kind() Kind
}

type InvalidCommand struct {
	Reason string
}

func (InvalidCommand) Kind() Kind { return KindInvalid }

type HelpCommand struct{}

func (HelpCommand) Kind() Kind { return KindHelp }

type SearchCommand struct {
	Query string
	Page  int
}

func (SearchCommand) Kind() Kind { return KindSearch }

type PageCommand struct {
	Page int
}

func (PageCommand) Kind() Kind { return KindPage }

type NextCommand struct{}

func (NextCommand) Kind() Kind { return KindNext }

type PrevCommand struct{}

func (PrevCommand) Kind() Kind { return KindPrev }

type SelectBookCommand struct {
	BookIndex int
}

func (SelectBookCommand) Kind() Kind { return KindSelect }
