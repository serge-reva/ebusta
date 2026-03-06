package main

import (
	"html/template"
	"net/http"
)

// cssStyles — стили для визуальных компонентов
const cssStyles = `
/* === Форма поиска === */
#search-form { margin: 20px 0; text-align: center; }
#search-input { width: 400px; padding: 8px 12px; font-size: 16px; border: 1px solid #ccc; border-radius: 4px; }
#search-button { padding: 8px 20px; font-size: 16px; background-color: #4a90a4; color: white; border: none; border-radius: 4px; cursor: pointer; }
#search-button:hover { background-color: #357a8c; }

/* === Таблица результатов === */
#results-table { width: 100%; border-collapse: collapse; margin: 20px 0; }
.table-header { background-color: #f5f5f5; font-weight: bold; text-align: left; padding: 10px; border-bottom: 2px solid #ddd; }
.table-row { cursor: pointer; }
.table-row:hover { background-color: #f9f9f9; }
.table-cell-title, .table-cell-authors, .table-cell-id, .table-cell-file { padding: 8px 10px; border-bottom: 1px solid #eee; }

/* === Пагинация === */
#pagination { text-align: center; margin: 20px 0; }
.page-link { display: inline-block; padding: 5px 10px; margin: 0 2px; text-decoration: none; color: #4a90a4; border: 1px solid #ddd; border-radius: 3px; }
.page-link:hover { background-color: #f0f0f0; }
.page-current { font-weight: bold; color: #333; background-color: #e0e0e0; }
.page-disabled { color: #999; cursor: not-allowed; }
.page-nav { font-weight: bold; }

/* === Ошибки === */
#error-banner { background-color: #fff3f3; border: 1px solid #ffcccc; border-radius: 4px; padding: 20px; margin: 20px 0; text-align: center; }
#error-message { color: #cc0000; font-size: 18px; margin-bottom: 10px; }
#trace-info { color: #666; font-size: 14px; font-family: monospace; }
#back-link { display: inline-block; margin-top: 15px; color: #4a90a4; }
`

// templateFuncs — функции для использования в шаблонах
var templateFuncs = template.FuncMap{
	"add": func(a, b int) int { return a + b },
	"sub": func(a, b int) int { return a - b },
}

// tmpl — базовый шаблонизатор
var tmpl = template.Must(template.New("base").Funcs(templateFuncs).Parse(`<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html lang="ru">
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <title>ebusta — Электронная библиотека</title>
    <style>` + cssStyles + `</style>
</head>
<body>
    <h1>ebusta</h1>
    {{template "content" .}}
</body>
</html>`))

// indexTmpl — главная страница
var indexTmpl = template.Must(template.Must(tmpl.Clone()).Parse(`
{{define "content"}}
<form id="search-form" action="/search" method="get">
    <input id="search-input" type="text" name="q" value="" size="50" placeholder="Введите запрос...">
    <button id="search-button" type="submit">Выполнить</button>
</form>
{{end}}
`))

// resultsTmpl — страница с результатами поиска
var resultsTmpl = template.Must(template.Must(tmpl.Clone()).Parse(`
{{define "content"}}
<form id="search-form" action="/search" method="get">
    <input id="search-input" type="text" name="q" value="{{.Query}}" size="50">
    <button id="search-button" type="submit">Выполнить</button>
</form>

{{if .Result.Books}}
<table id="results-table">
    <thead>
        <tr class="table-header">
            <th>Title</th>
            <th>Authors</th>
            <th>Download</th>
        </tr>
    </thead>
    <tbody>
    {{range .Result.Books}}
        <tr class="table-row" onclick="window.location='{{.DownloadURL}}?q={{$.Query}}'">
            <td class="table-cell-title">{{.Title}}</td>
            <td class="table-cell-authors">{{.FullAuthors}}</td>
            <td class="table-cell-file">{{.DownloadURL}}</td>
        </tr>
    {{end}}
    </tbody>
</table>

<div id="pagination">
    {{if gt .Result.Page 1}}
        <a class="page-link page-nav" href="/search?q={{.Query}}&page=1">&laquo; Первая</a>
        <a class="page-link page-nav" href="/search?q={{.Query}}&page={{sub .Result.Page 1}}">&lt;</a>
    {{else}}
        <span class="page-link page-disabled">&laquo; Первая</span>
        <span class="page-link page-disabled">&lt;</span>
    {{end}}

    <span class="page-link page-current">{{.Result.Page}}</span>
    <span class="page-link">/ {{.Result.Pages}}</span>

    {{if lt .Result.Page .Result.Pages}}
        <a class="page-link page-nav" href="/search?q={{.Query}}&page={{add .Result.Page 1}}">&gt;</a>
        <a class="page-link page-nav" href="/search?q={{.Query}}&page={{.Result.Pages}}">Последняя &raquo;</a>
    {{else}}
        <span class="page-link page-disabled">&gt;</span>
        <span class="page-link page-disabled">Последняя &raquo;</span>
    {{end}}
</div>

{{else}}
<p>Ничего не найдено.</p>
{{end}}
{{end}}
`))

// errorTmpl — страница ошибки
var errorTmpl = template.Must(template.Must(tmpl.Clone()).Parse(`
{{define "content"}}
<div id="error-banner">
    <div id="error-message">{{.Message}}</div>
    {{if .TraceID}}
    <div id="trace-info">TraceID: {{.TraceID}}</div>
    {{end}}
    <a id="back-link" href="/">Вернуться к поиску</a>
</div>
{{end}}
`))

// renderIndex — рендеринг главной страницы
func renderIndex(w http.ResponseWriter) {
	if err := indexTmpl.ExecuteTemplate(w, "base", nil); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

// renderResults — рендеринг результатов поиска
func renderResults(w http.ResponseWriter, result *WebSearchResult, query string) {
	data := struct {
		Result *WebSearchResult
		Query  string
	}{
		Result: result,
		Query:  query,
	}

	if err := resultsTmpl.ExecuteTemplate(w, "base", data); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

// renderError — рендеринг страницы ошибки
func renderError(w http.ResponseWriter, message, traceID string) {
	data := struct {
		Message string
		TraceID string
	}{
		Message: message,
		TraceID: traceID,
	}
	if err := errorTmpl.ExecuteTemplate(w, "base", data); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}
