# AGENTS.md — Codex Working Agreement (STRICT MODE, Universal)

This file governs Codex behavior in this repository.

This repo is an architectural system. Codex operates as an execution agent inside strict boundaries.
ChatGPT acts as the Architect: reads docs, defines scope, writes prompts. Codex is the Executor: edits code and runs gates.

------------------------------------------------------------
0) ARCHITECTURAL DOCTRINE (NON-NEGOTIABLE)
------------------------------------------------------------

BOUNDARIES FIRST.

- Gateway is an **adapter**. It MUST NOT own persistence or business rules that belong to domain/storage.
- Storage owns **persistence** and its data correctness.
- Domain logic must not leak into adapters.

CONTRACTS ARE PRODUCT.

- Public/API contracts (proto, schemas, external HTTP) are guarded.
- No breaking changes without an explicit, versioned process.
- If a contract doc exists in docs/, it is authoritative.

TRACE / ERROR / LOG DISCIPLINE.

- Every request MUST have a trace_id:
  - accept from incoming request
  - generate if absent
  - propagate across boundaries (HTTP ↔ gRPC)
  - include in logs and error responses
- Domain errors are returned as domain diagnostics (structured), not transport failures.
- Transport failures use proper statuses (HTTP/gRPC) and never hide root causes.

DOCKER IS RUNTIME-ONLY VALIDATION.

- Runtime images MUST NOT build sources.
- All build artifacts are produced on host CI loop.
- Docker is used to validate runtime wiring and user testing only.

------------------------------------------------------------
1) OPERATING STANCE
------------------------------------------------------------

Codex must work step-by-step:

- Make minimal, focused changes.
- Verify after every change.
- Never assume green state.
- If gates fail, stop and fix before proceeding.

When uncertain, Codex must gather facts first (rg/tree/tests) and report them, then act.

------------------------------------------------------------
2) BRANCH / CHANGE MANAGEMENT (MANDATORY)
------------------------------------------------------------

NEW FEATURE / IMPORTANT CHANGE RULE:

- For any important change (new feature, new contract surface, refactor that moves packages, infra wiring changes):
  1) Create a NEW git branch.
  2) Re-read docs/ (see "docs-refresh" rule below).
  3) Only then implement.

COMMITS:

- Small commits with clear messages.
- Each commit must keep gates green (or explicitly be a temporary WIP only if Architect asked).

NO DIRECT WORK ON main:

- Work must happen on a feature/chore branch.
- Push branch to origin as soon as it’s created.

------------------------------------------------------------
3) HARD LIMITS (Forbidden unless Architect explicitly overrides)
------------------------------------------------------------

Forbidden:

- Changing proto field numbers / breaking API without version process
- Removing trace propagation or trace headers/metadata
- Silently swallowing errors / returning success on failure
- Moving persistence into gateway/adapters
- Building inside runtime Dockerfiles
- Marking MVP complete without Docker-based user validation

------------------------------------------------------------
4) OWNERSHIP / SCOPE
------------------------------------------------------------

Codex MUST obey ownership boundaries as defined by repo docs (if present).
If no explicit ownership file exists, default rule:

- Gateway area: adapters, HTTP handlers, wiring, UI static serving
- Storage area: persistence, storage handlers, migrations
- Shared/infra: logger/errutil/config/trace helpers used by all

If a task touches multiple layers, Codex must request Architect guidance before crossing boundaries.

------------------------------------------------------------
5) QUALITY GATES (MUST STAY GREEN)
------------------------------------------------------------

Codex must run the repo’s standard gates and keep them green.

Minimum expectation (adapt to this repo’s actual Makefile targets):

- unit tests
- build
- lint/static checks (if any)
- e2e host loop (if any)
- docker runtime validation (see MVP closure)

Codex must report exact commands run and their outcomes.

------------------------------------------------------------
6) MVP CLOSURE PROTOCOL (MANDATORY)
------------------------------------------------------------

Every MVP/Stage is considered CLOSED only after:

1) Runtime artifacts built (host build outputs ready)
2) Docker stack поднят (compose up)
3) Manual user testing instructions provided (docker-only)
4) Architect runs the manual test and confirms “OK”
5) Docker stack stopped (compose down)
6) Only then: tag/merge/close stage

IMPORTANT:

- Manual user tests MUST be performed only with docker-compose services running.
- Codex MUST provide docker-only test scenarios as part of “definition of done”.
- Codex MUST NOT claim completion without explicit Architect approval.

------------------------------------------------------------
7) USER TEST SCENARIOS (Codex responsibility)
------------------------------------------------------------

For each MVP/Stage, Codex must write:

- A concise docker-only manual test checklist (steps + expected results)
- One “happy path” and at least one “error path”
- How to observe trace_id in responses/logs (if applicable)

------------------------------------------------------------
8) DOCS REFRESH RULE (for long contexts)
------------------------------------------------------------

To prevent context loss, Codex must periodically refresh understanding of docs/:

- Provide (or maintain) a Makefile target named: `docs-refresh`
- The target prints the list of docs and key constitutions/standards (no edits).
- Architect may ask Codex to run docs-refresh at the start of major work.

------------------------------------------------------------
9) REPORTING FORMAT (what Codex must output)
------------------------------------------------------------

After implementation, Codex must output:

- Commit(s) made
- `git show --stat`
- Gates run + results
- Docker-only manual test steps
- Any known risks / follow-ups

