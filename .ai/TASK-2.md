# TASK-2: Ground the native DI design in existing decisions

Goal: update `.ai/native.md` so the native dependency-injection design starts from existing Capybara decisions instead of inventing a parallel architecture.

Files to read:

- `adr/ADR-2026-04-14-capybara-oo-v1.md`
- `adr/ADR-2026-05-08-reflection-v1.md`
- `adr/2026-04-12-unsafe-constructor-bypass.md`
- `.ai/interoperability.md`
- `.ai/native.md`

Exact work:

1. In `.ai/native.md`, find `## Existing Decisions`.
2. Make sure it explicitly states these constraints:
   - `.coo` remains separate from `.cfun`.
   - OO exceptions are not automatically converted to functional `Result.Error`.
   - reflection metadata is descriptive only and must not become a dynamic invocation path for native objects.
   - unsafe constructor bypass is only for `.cfun data` values and must not be reused for OO native provider wiring.
   - `.cfun` access to wired OO/native objects must cross an `Effect` boundary.
3. Add a clear sentence that no existing ADR covers Java/JavaScript/Python native host provider wiring.
4. Add a recommendation that the eventual implementation should create one native interop ADR or update an existing matching ADR if one appears before implementation starts.
5. In `## Current State`, ensure the document says:
   - `.coo` already has interfaces.
   - Java generation already emits Java interfaces for `.coo` interfaces.
   - JavaScript and Python do not yet have generated native provider tables.
   - `.coo` imports are Capybara module imports only, not host-language imports.
   - existing native behavior is currently backend/std-lib specific, not a general OO DI model.

Do not:

- Change compiler, runtime, grammar, or generated files in this task.
- Propose direct Java package imports, CommonJS `require`, or Python `import` syntax inside `.coo`.

Acceptance checks:

- A developer can read the opening sections and understand which prior decisions constrain the design.
- The document clearly says this is a design note, not implemented behavior.
- The document does not duplicate an ADR decision without naming the relevant ADR.
