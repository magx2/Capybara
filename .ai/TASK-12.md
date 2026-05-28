# TASK-12: Port Python generation to Capybara

Issue: #184

## Goal

Move Python backend emission and Python native provider bootstrap generation
into Capybara.

## Scope

- Port Python backend emitter.
- Port Python native provider bootstrap emitter.
- Preserve generated package/module layout.
- Preserve native provider import/class lookup behavior.
- Preserve current test runner behavior.

## Requirements

- Output paths remain stable.
- Runtime helper semantics remain compatible.
- Native provider diagnostics remain compatible.
- Generated Python behavior stays equivalent to current Java implementation.

## Acceptance Criteria

- Python generator unit tests pass.
- Normalized generated Python output parity tests pass.
- `:capy:pythonTests` passes.
- `:capy:e2e-cfun-python`, `:capy:e2e-coo-python`, and
  `:capy:e2e-coo-python-native` pass.

## Commit Save Point

Commit after this task with:

```text
feat(#184): port Python generation to Capybara
```
