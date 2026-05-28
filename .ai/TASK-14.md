# TASK-14: Harden self-hosting native providers

Issue: #184

## Goal

Expose all host access introduced by self-hosting through deterministic,
ADR-compliant native provider contracts.

## Scope

- Define provider interfaces for:
  - parser;
  - filesystem;
  - environment;
  - console;
  - clock;
  - JSON/YAML;
  - ZIP/package writing;
  - Java compiler;
  - test execution.
- Choose stable qualifiers and lifetimes.
- Preserve source `@NativeProvider` as preferred provider declaration path.
- Keep `--native-wiring` JSON as compatibility input.

## Diagnostics To Preserve

- `NotWired`
- `DuplicateProvider`
- `TypeMismatch`
- `UnsupportedBackend`
- `InvocationFailure`

## Acceptance Criteria

- Java, JS, and Python native provider e2e suites pass.
- No mutable runtime provider registration path is introduced.
- No direct `.coo` host imports are introduced.
- Tests cover annotation wiring, manifest fallback, duplicate providers,
  missing backend bindings, unsupported factory/lifetime, malformed manifests,
  unused manifest entries, and provider call shadowing.

## Commit Save Point

Commit after this task with:

```text
feat(#184): harden self-hosting native providers
```
