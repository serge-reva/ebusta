#!/usr/bin/env python3
import json
from http.server import BaseHTTPRequestHandler, HTTPServer

class Handler(BaseHTTPRequestHandler):
    def _json(self, code, payload):
        body = json.dumps(payload).encode('utf-8')
        self.send_response(code)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        if self.path == '/health':
            self._json(200, {'status': 'ok'})
            return
        self._json(404, {'error': 'not found'})

    def do_POST(self):
        if self.path != '/search':
            self._json(404, {'error': 'not found'})
            return
        length = int(self.headers.get('Content-Length', '0'))
        raw = self.rfile.read(length)
        try:
            payload = json.loads(raw or b'{}')
        except json.JSONDecodeError:
            self._json(400, {'error': 'bad json'})
            return
        query = payload.get('query', '')
        page = int(payload.get('page', 1) or 1)
        books = []
        total = 0
        pages = 1
        if query:
            books = [{
                'id': '1',
                'title': f'Mock result for {query}',
                'authors': ['Ebusta Test Suite'],
                'full_authors': 'Ebusta Test Suite',
                'download_url': '/download/mock-token',
                'container': 'mock.zip',
                'filename': 'mock.fb2'
            }]
            total = 1
        self._json(200, {
            'trace_id': self.headers.get('X-Trace-Id', 'gw-mock-trace'),
            'books': books,
            'total': total,
            'page': page,
            'pages': pages,
        })

    def log_message(self, format, *args):
        return

if __name__ == '__main__':
    HTTPServer(('0.0.0.0', 8443), Handler).serve_forever()
