# ADR-2026-04-29: `.cfun` `fun rec` Tail-Recursion Contract

- Status: accepted
- Deciders: Codex, repository maintainers
- Date: 2026-04-29

## Context

Issue #100 introduces `.cfun` `fun rec` as an explicit recursion form with compiler checks and backend consequences.

Without a dedicated marker, recursive intent and optimization expectations are implicit. That makes it hard to guarantee predictable lowering and diagnostics for recursion-heavy code.

## Decision

Capybara treats `fun rec` as a compiler-verified direct tail-recursion contract.

- `rec` is declaration-local and only meaningful with `fun`.
- The compiler validates direct tail-recursive shape for the declared function body.
- Violations are rejected at compile time instead of silently compiling as ordinary recursion.
- Java lowering for valid `fun rec` functions uses loop-style code generation rather than stack-growing self-calls.

## Consequences

Recursive performance intent is explicit in source, and invalid non-tail forms fail early.

Java backend behavior becomes more predictable for approved tail-recursive declarations, with stack usage aligned to loop lowering.

This ADR does not broaden recursion semantics beyond direct tail recursion and does not change non-`rec` function behavior.
