# TASK-4: Add Java ANTLR parser facade and Capybara DTO adapters

Issue: #184

## Goal

Keep ANTLR Java-owned while exposing stable parser DTOs that Capybara compiler
code can consume.

## Scope

- Create Java parser facade interfaces for:
  - parsing `.cfun` modules;
  - parsing `.coo` modules;
  - import extraction with stable line numbers;
  - parse diagnostics with source file and line/column.
- Wrap the parser facade behind `@NativeProvider` contracts for Capybara
  callers.
- Define parser DTO/schema values independent from Java records and generated
  Capybara class names.
- Add Capybara adapters from parser DTOs to compiler AST/IR.
- Preserve current parser diagnostics unless intentionally changed.

## Parser Golden Coverage

- `TYPE { ... }` data construction.
- `TYPE.field(...)`.
- `NAME(...)`.
- generics at line start and `LINE_START_LBRACK`.
- list/set/dict literals.
- `=>`, `|>`, reduce/lambda forms.
- match branches and guards.
- `if then else`.
- unary/postfix/infix precedence.
- empty index/slice cases.
- `.coo` arrays, `this`, `super`, calls, blocks, and exception statements.

## Acceptance Criteria

- ANTLR grammars regenerate without new warnings/conflicts, or every
  intentional conflict is documented with regression tests.
- Parser facade feeds Capybara IR without semantic changes.
- Parser error snapshots remain stable.

## Commit Save Point

Commit after this task with:

```text
feat(#184): add parser facade for self-hosted compiler
```
