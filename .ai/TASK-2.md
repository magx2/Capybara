# TASK-2: Add self-hosting source-set scaffolding

Issue: #184

## Goal

Add Capybara source roots for future compiler and CLI implementation without
changing production behavior.

## Scope

- Add Gradle wiring for `compiler/src/main/capybara`.
- Add Gradle wiring for `capy/src/main/capybara`.
- Generate Java from those sources into build directories before Java
  compilation.
- Keep generated classes internal during this phase.
- Add a trivial pure Capybara module and Java-side test proving generated code
  is callable.
- Ensure stdlib bootstrap does not recursively load the compiler being built.

## Deliverables

- Build wiring for compiler Capybara sources.
- Build wiring for capy Capybara sources.
- Minimal generated-code call test.
- No committed generated output.

## Acceptance Criteria

- No user-visible behavior changes.
- `./gradlew :compiler:test :capy:test` passes.
- Generated Java remains under `build/generated/...`.

## Commit Save Point

Commit after this task with:

```text
feat(#184): add self-hosting source-set scaffolding
```
