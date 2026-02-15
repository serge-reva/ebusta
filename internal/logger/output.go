package logger

import (
	"io"
	"os"
	"sync"
)

// Output is an interface for log output destinations
type Output interface {
	io.Writer
	// Close closes the output (optional, may be no-op)
	Close() error
	// Name returns a descriptive name for the output
	Name() string
}

// StdoutOutput writes to standard output
type StdoutOutput struct{}

func (o *StdoutOutput) Write(p []byte) (n int, err error) {
	return os.Stdout.Write(p)
}

func (o *StdoutOutput) Close() error {
	return nil // no-op
}

func (o *StdoutOutput) Name() string {
	return "stdout"
}

// StderrOutput writes to standard error
type StderrOutput struct{}

func (o *StderrOutput) Write(p []byte) (n int, err error) {
	return os.Stderr.Write(p)
}

func (o *StderrOutput) Close() error {
	return nil // no-op
}

func (o *StderrOutput) Name() string {
	return "stderr"
}

// FileOutput writes to a file
type FileOutput struct {
	mu   sync.Mutex
	file *os.File
	path string
}

// NewFileOutput creates a new file output
// If append is true, the file will be opened for appending
func NewFileOutput(path string, append bool) (*FileOutput, error) {
	flag := os.O_CREATE | os.O_WRONLY
	if append {
		flag |= os.O_APPEND
	} else {
		flag |= os.O_TRUNC
	}

	file, err := os.OpenFile(path, flag, 0644)
	if err != nil {
		return nil, err
	}

	return &FileOutput{
		file: file,
		path: path,
	}, nil
}

func (o *FileOutput) Write(p []byte) (n int, err error) {
	o.mu.Lock()
	defer o.mu.Unlock()
	return o.file.Write(p)
}

func (o *FileOutput) Close() error {
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.file != nil {
		return o.file.Close()
	}
	return nil
}

func (o *FileOutput) Name() string {
	return "file:" + o.path
}

// WriterOutput wraps any io.Writer as an Output
type WriterOutput struct {
	w    io.Writer
	name string
}

// NewWriterOutput creates an Output from any io.Writer
func NewWriterOutput(w io.Writer, name string) *WriterOutput {
	return &WriterOutput{w: w, name: name}
}

func (o *WriterOutput) Write(p []byte) (n int, err error) {
	return o.w.Write(p)
}

func (o *WriterOutput) Close() error {
	// Try to close if it implements io.Closer
	if c, ok := o.w.(io.Closer); ok {
		return c.Close()
	}
	return nil
}

func (o *WriterOutput) Name() string {
	return o.name
}

// MultiOutput writes to multiple outputs simultaneously
type MultiOutput struct {
	mu      sync.RWMutex
	outputs []Output
}

// NewMultiOutput creates a new multi-output that writes to all provided outputs
func NewMultiOutput(outputs ...Output) *MultiOutput {
	return &MultiOutput{
		outputs: outputs,
	}
}

// Add adds a new output to the multi-output
func (m *MultiOutput) Add(output Output) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.outputs = append(m.outputs, output)
}

// Remove removes an output by name
func (m *MultiOutput) Remove(name string) {
	m.mu.Lock()
	defer m.mu.Unlock()
	for i, o := range m.outputs {
		if o.Name() == name {
			m.outputs = append(m.outputs[:i], m.outputs[i+1:]...)
			break
		}
	}
}

func (m *MultiOutput) Write(p []byte) (n int, err error) {
	m.mu.RLock()
	defer m.mu.RUnlock()

	// Write to all outputs; return the length if at least one succeeded
	for _, o := range m.outputs {
		n, err = o.Write(p)
		// Continue writing to other outputs even if one fails
	}
	return n, err
}

func (m *MultiOutput) Close() error {
	m.mu.Lock()
	defer m.mu.Unlock()

	var lastErr error
	for _, o := range m.outputs {
		if err := o.Close(); err != nil {
			lastErr = err
		}
	}
	return lastErr
}

func (m *MultiOutput) Name() string {
	return "multi"
}

// Outputs returns a copy of the outputs list
func (m *MultiOutput) Outputs() []Output {
	m.mu.RLock()
	defer m.mu.RUnlock()
	result := make([]Output, len(m.outputs))
	copy(result, m.outputs)
	return result
}
