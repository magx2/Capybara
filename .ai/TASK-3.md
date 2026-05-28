# TASK-3: Define linked artifact and compiler contract schemas

Issue: #184

## Goal

Define stable, language-neutral contracts before moving compiler logic to
Capybara.

## Scope

- Define a versioned linked JSON schema or compatibility shim.
- Define Capybara codecs for:
  - linked modules;
  - compiled modules;
  - compiled expressions;
  - OO modules;
  - native provider catalogs;
  - diagnostics;
  - generated program metadata.
- Preserve old-to-new and new-to-old compatibility during migration.
- Define backend-neutral generated program IR.
- Define deterministic ordering for modules, imports, diagnostics, generated
  files, native providers, and duplicate errors.

## Deliverables

- Documented linked artifact schema.
- Compatibility shim or versioned reader/writer.
- Round-trip tests against Java-produced artifacts.
- Contract parity tests against current Java output.

## Acceptance Criteria

- Migrated code can read old Java-produced linked modules.
- Java compatibility paths can read migrated linked modules during transition.
- Unsupported schema versions fail with clear diagnostics.
- Current Jackson default typing is not the long-term self-hosted contract.

## Commit Save Point

Commit after this task with:

```text
feat(#184): define self-hosted compiler artifact contracts
```
