# Downloads Import

Version: 1.0
Last Updated: 2026-03-07

## Purpose

`cmd/downloads-import` imports `f2bulker` JSONL output into the SQLite metadata store used by the downloads stack.

The tool expects bulk-style JSONL where each logical record is represented by two lines:

1. action line
2. document line

Only document lines are imported into SQLite.

## Build

`downloads-import` is built as part of:

```bash
make build-downloads-go
```

The resulting binary is written to:

```bash
./bin/downloads-import
```

## Flags

```text
-jsonl <path>              path to f2bulker JSONL bulk file (required)
-sqlite <path>             path to SQLite metadata database (required)
-log-level <level>         DEBUG, INFO, WARN, ERROR (default: INFO)
-log-file <path>           append logs to the given file
-continue-on-error         keep processing after per-record parse/insert errors
-quiet                     suppress stderr output, including progress bar
-commit-interval <N>       commit every N successfully inserted records; 0 means one transaction for the whole file
```

## Behavior

### Logging

- Logging uses `internal/logger`.
- By default, informational logs are written to `stderr`.
- If `-log-file` is set, logs are also appended to the specified file.
- If `-quiet` is enabled:
  - `stderr` output is suppressed;
  - progress bar is disabled;
  - logs still go to `-log-file` if configured.
- Fatal startup/runtime errors are still printed to `stderr` by the CLI wrapper.

### Progress Bar

The tool performs two passes over the JSONL file:

1. count document lines for exact progress reporting;
2. run the actual import.

The progress bar counts only document lines, not action lines.

### Error Handling

Without `-continue-on-error`:

- the first record-level parse or insert error stops the import;
- if `-commit-interval=0`, the current transaction is rolled back.

With `-continue-on-error`:

- invalid records are logged and skipped;
- the import continues with the next document line.

### Duplicate Handling

Duplicates are skipped using `INSERT OR IGNORE` on `books.sha1`.

They are included in the final summary as `duplicates`.

### Periodic Commit

If `-commit-interval N` is greater than zero, the importer commits every `N` successfully inserted records and starts a new transaction.

This reduces rollback scope on long imports.

## Examples

Single transaction, normal output:

```bash
./bin/downloads-import \
  -jsonl ./f2bulker/data/out/f.fb2-173909-177717.zip.jsonl \
  -sqlite /opt/ebusta/archive.meta.sqlite
```

Continue on bad rows and keep a file log:

```bash
./bin/downloads-import \
  -jsonl ./f2bulker/data/out/f.fb2-173909-177717.zip.jsonl \
  -sqlite /opt/ebusta/archive.meta.sqlite \
  -continue-on-error \
  -log-file ./logs/downloads-import.log
```

Periodic commits every 1000 inserted rows:

```bash
./bin/downloads-import \
  -jsonl ./f2bulker/data/out/f.fb2-173909-177717.zip.jsonl \
  -sqlite /opt/ebusta/archive.meta.sqlite \
  -commit-interval 1000
```

Quiet mode for scripted runs:

```bash
./bin/downloads-import \
  -jsonl ./f2bulker/data/out/f.fb2-173909-177717.zip.jsonl \
  -sqlite /opt/ebusta/archive.meta.sqlite \
  -quiet \
  -log-file ./logs/downloads-import.log
```

## Final Summary

On successful completion the tool logs final counters:

- `processed`
- `inserted`
- `duplicates`
- `errors`

Example:

```text
downloads-import finished processed=1000 inserted=997 duplicates=2 errors=1
```

## Deployment

Ansible playbook for copying the built binary to stage archive host:

```bash
cd infra/ansible
ansible-playbook -i inventory/stage/hosts.yml playbooks/deploy_downloads_import.yml --ask-become-pass
```

The binary is installed to:

```bash
/opt/ebusta/bin/downloads-import
```
