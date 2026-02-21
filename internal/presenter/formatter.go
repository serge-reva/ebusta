package presenter

import "io"

// Formatter определяет интерфейс для форматирования результатов
type Formatter interface {
    Format(result *PresenterResult, w io.Writer) error
}

// FormatterFunc обёртка для функций-форматтеров
type FormatterFunc func(*PresenterResult, io.Writer) error

func (f FormatterFunc) Format(result *PresenterResult, w io.Writer) error {
    return f(result, w)
}
