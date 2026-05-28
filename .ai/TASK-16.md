# TASK-16: Remove migrated Java compiler and CLI code

Issue: #184

## Goal

Remove obsolete production Java implementation code after the self-hosted path
is stable.

## Scope

- Remove migrated Java semantic logic.
- Remove migrated Java generator logic.
- Remove hand-written Java CLI launcher from the production path.
- Keep only permanent Java islands:
  - ANTLR parser facade;
  - host provider implementations;
  - required runtime helpers.
- Remove temporary Java parity harnesses unless the self-hosting ADR explicitly
  keeps them.
- Update `README.md`, `DEV_README.md`, `capy/CAPYBARA_E2E_TESTS.md`, and ADR
  status notes.
- Simplify Gradle wiring if old dual-run paths are no longer needed.

## Acceptance Criteria

- No dual implementation remains outside intentional compatibility shims.
- Dependency graph is simpler.
- Full test gate passes:
  - `./gradlew clean test`
  - `./gradlew :capy:e2eTests`
  - `:capy:jsTests`
  - `:capy:pythonTests`
  - stdlib compile/test gates

## Commit Save Point

Commit after this task with:

```text
chore(#184): remove migrated Java compiler and CLI code
```
