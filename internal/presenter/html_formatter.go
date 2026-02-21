package presenter

import (
    "html/template"
    "io"
    "strings"
)

// HTMLFormatter форматирует результаты в HTML (для web-frontend)
type HTMLFormatter struct {
    Template *template.Template
}

func (f *HTMLFormatter) Format(result *PresenterResult, w io.Writer) error {
    data := struct {
        Result     *PresenterResult
        HasResults bool
        URLFunc    func(string) string
    }{
        Result:     result,
        HasResults: len(result.Books) > 0,
        URLFunc:    urlEscape,
    }
    
    return f.Template.ExecuteTemplate(w, "base", data)
}

// urlEscape экранирует строку для использования в URL
func urlEscape(s string) string {
    return strings.ReplaceAll(s, " ", "%20")
}
