package sqlite

import "database/sql"

const Schema = `
CREATE TABLE IF NOT EXISTS books (
  sha1 TEXT PRIMARY KEY,
  container TEXT NOT NULL,
  filename TEXT NOT NULL,
  size INTEGER NOT NULL,
  title TEXT NOT NULL,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
);
`

func EnsureSchema(db *sql.DB) error {
	_, err := db.Exec(Schema)
	return err
}
