# TASK-10: Port Java generation to Capybara

Issue: #184

## Goal

Move backend-independent generation planning and Java backend emission into
Capybara while preserving generated output behavior.

## Scope

- Port generated file model and output ordering.
- Port backend-independent generation plan.
- Port Java backend emitter.
- Port Java native provider bootstrap emitter.
- Preserve stale-output manifest model where it affects Java generation.

## Requirements

- Preserve current package/class/file paths.
- Preserve reflection metadata shape.
- Preserve tail-recursive lowering behavior.
- Preserve runtime helper behavior until replaced by explicit provider-backed
  equivalents.
- Keep output deterministic.

## Acceptance Criteria

- Java generator unit tests pass.
- Normalized generated Java output parity tests pass.
- Java e2e suites pass.
- Native provider Java e2e passes.

## Commit Save Point

Commit after this task with:

```text
feat(#184): port Java generation to Capybara
```
