# TASK-7: Port type and provider validation passes

Issue: #184

## Goal

Move type linking, annotation validation, and native provider catalog validation
into `.cfun`.

## Scope

- Port type linking.
- Port declaration annotation validation.
- Preserve `@Recursive` as the only v1 annotation with compiler behavior.
- Port native provider catalog validation.
- Preserve source `@NativeProvider` as preferred declaration path.
- Keep `--native-wiring` JSON as compatibility input.

## Native Provider Diagnostics

Preserve stable diagnostic terms:

- `NotWired`
- `DuplicateProvider`
- `TypeMismatch`
- `UnsupportedBackend`
- `InvocationFailure`

## Acceptance Criteria

- Duplicate and ambiguous cases fail deterministically.
- Provider symbol shadowing by locals/methods does not rewrite calls.
- Provider annotation and manifest compatibility tests pass.
- Compile-error tests for touched diagnostics pass.
- Cross-backend e2e canary passes when backend behavior can be affected.

## Commit Save Point

Commit after this task with:

```text
feat(#184): port type and provider validation passes
```
