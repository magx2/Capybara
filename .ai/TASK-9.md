# TASK-9: Port OO validation to Capybara

Issue: #184

## Goal

Move OO validation and interop modeling without weakening `.coo` design or
leaking statementful OO semantics into `.cfun`.

## Scope

- Replace or formalize raw-string expression validation from
  `ObjectOrientedParser` and `ObjectOrientedValidator`.
- Model OO validation for:
  - field initialization;
  - visibility;
  - override/final/open/abstract rules;
  - mutable locals and assignment;
  - call-only statements;
  - entrypoint restrictions;
  - constructor/init restrictions;
  - trait limitations.
- Validate `.cfun`/`.coo` interop through explicit effect boundaries.

## OO Requirements

- Keep `.cfun` compiler core free of OO statement semantics.
- Keep user-program OO exceptions as runtime failures.
- Compiler validation failures must be compile diagnostics.
- Avoid anemic wrappers and god objects.

## Acceptance Criteria

- `.coo` compiler tests pass.
- OO compile-error tests pass.
- Java, JavaScript, and Python e2e behavior remains equivalent.
- Mixed `.cfun`/`.coo` compile-error tests cover invalid construction,
  unsupported targets, wrong arity/types, and effect-boundary violations.

## Commit Save Point

Commit after this task with:

```text
feat(#184): port OO validation to Capybara
```
