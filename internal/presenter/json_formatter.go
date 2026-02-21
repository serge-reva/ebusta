package presenter

import (
    "encoding/json"
    "io"
)

// JSONFormatter форматирует результаты в JSON (для API)
type JSONFormatter struct {
    Pretty bool
}

func (f *JSONFormatter) Format(result *PresenterResult, w io.Writer) error {
    encoder := json.NewEncoder(w)
    if f.Pretty {
        encoder.SetIndent("", "  ")
    }
    return encoder.Encode(result)
}
