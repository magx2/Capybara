# TASK-6: Port module linking passes to Capybara

Issue: #184

## Goal

Move module identity, source descriptor classification, import resolution, and
declaration indexing into pure `.cfun` passes.

## Scope

- Port module/path normalization and canonical module identity.
- Port source descriptor and extension classification from already discovered
  immutable inputs.
- Port import resolution.
- Port declaration indexes and duplicate detection.
- Keep source discovery, directory walking, and file reads in `.coo` or
  `NativeProvider` shell code.

## FP Requirements

- Each pass is pure `.cfun`.
- Each pass returns `Result[Output]`.
- Symbol tables, caches, and contexts are explicit inputs/outputs.
- Diagnostics are deterministic values with stable ordering.

## Acceptance Criteria

- Dual-run parity tests pass for each migrated pass.
- Compile-error tests for touched diagnostics pass.
- Minimal cross-backend e2e canary passes when behavior can affect backends.
- `./gradlew :compiler:test :compiler:integrationTest` passes.

## Commit Save Point

Commit after this task with:

```text
feat(#184): port module linking passes to Capybara
```
