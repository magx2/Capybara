# TASK-1: Record self-hosting ADR and baseline gates

Issue: #184

## Goal

Create the architecture decision record and baseline evidence required before
any compiler or CLI implementation moves from Java to Capybara.

## Scope

- Add ADR `Self-hosted Compiler and CLI v1`.
- Define permanent Java islands:
  - ANTLR grammars and generated parser runtime.
  - Stable parser DTO/schema facade.
  - Native provider host implementations.
  - Minimal JVM runtime glue needed to load generated Capybara code.
- State that launching semantics move to Capybara:
  `fun main(args: List[String]): Effect[Program]`.
- State that production Gradle/package/user entrypoints must eventually invoke
  Java generated from Capybara source, not retained hand-written Java
  entrypoints.
- Update native-provider and OO ADR notes if needed.
- Freeze current behavior with parity/golden fixtures.

## Deliverables

- ADR for self-hosting architecture.
- Golden fixtures for parser diagnostics, compile diagnostics, linked JSON,
  generated Java/JS/Python output, native provider catalog/bootstrap output,
  and CLI exit/stderr behavior.
- Baseline timing notes for:
  - `./gradlew :compiler:test :compiler:integrationTest`
  - `./gradlew :capy:e2eTests`
  - `./gradlew clean test`

## Acceptance Criteria

- ADR is accepted.
- Current full suite is green before any cutover.
- Any known unstable output is documented.
- No implementation behavior changes are introduced.

## Commit Save Point

Commit after this task with:

```text
chore(#184): record self-hosting architecture and baseline gates
```
