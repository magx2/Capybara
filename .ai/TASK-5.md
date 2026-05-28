# TASK-5: Model compiler core IR in Capybara

Issue: #184

## Goal

Model compiler-domain values in `.cfun` and make invalid states
unrepresentable where practical.

## Scope

- Inventory Java model classes before porting.
- Classify each Java model:
  - product-only records/value classes -> Capybara `data`;
  - sealed/interface alternatives -> Capybara `union` with one case per
    variant;
  - Java enums -> Capybara `enum` when they are closed symbolic sets;
  - behavior/host-resource classes -> `.coo` shell object or `NativeProvider`
    boundary, not `.cfun data`.
- Model module identity, imports, declarations, compiled types, expressions,
  diagnostics, and native provider declarations/catalog.
- Provide a no-op/pass-through compiler pipeline over stable DTO/schema
  boundaries.

## Deliverables

- Capybara IR definitions.
- Record-to-`data` and hierarchy-to-`union` mapping table in ADR or migration
  notes.
- Adapter round-trip tests.
- No production cutover.

## Acceptance Criteria

- Capybara IR round-trips through adapters for representative fixtures.
- Parity harness can compare Java legacy structures with Capybara structures.
- Generated Capybara class shapes are not used as public Java-island contracts.

## Commit Save Point

Commit after this task with:

```text
feat(#184): model compiler core IR in Capybara
```
