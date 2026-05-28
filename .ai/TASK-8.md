# TASK-8: Port expression compiler passes

Issue: #184

## Goal

Move derive expansion, expression type checking, recursion checks, and
main/test detection into `.cfun`.

## Scope

- Port derive expansion.
- Port expression type checking.
- Port object construction and interop checks.
- Port `@Recursive` direct tail-recursion validation.
- Port main and test detection.

## Boundary Rules

- `.cfun` construction of `.coo` values remains effectful:
  `Person(...)` or generated bridge calls must type as `Effect[Person]`, never
  `Person`.
- Pure object construction must be rejected.
- OO exceptions remain user-program runtime failures, not implicit
  `Result.Error`.
- Compiler semantic and validation failures remain deterministic diagnostics.

## Acceptance Criteria

- Dual-run parity tests pass for each migrated expression pass.
- Compile-error tests for invalid programs pass.
- Cross-backend e2e canaries cover `.cfun` and mixed `.cfun`/`.coo` paths.
- `./gradlew :compiler:test :compiler:integrationTest` passes.

## Commit Save Point

Commit after this task with:

```text
feat(#184): port expression compiler passes
```
