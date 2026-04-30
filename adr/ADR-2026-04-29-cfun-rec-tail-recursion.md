# ADR-2026-04-29: `.cfun` `fun rec` Tail-Recursion Contract

- Status: accepted
- Deciders: Codex, repository maintainers
- Date: 2026-04-29

## Context

Issue #100 introduces `.cfun` `fun rec` as an explicit recursion form with compiler checks and backend consequences.

Without a dedicated marker, recursive intent and optimization expectations are implicit. That makes it hard to guarantee predictable lowering and diagnostics for recursion-heavy code.

## Decision

Capybara detects direct self-recursion for every function. It treats `fun rec` as a compiler-verified direct tail-recursion contract.

- `rec` is declaration-local and only meaningful with `fun`.
- The compiler records whether each function is directly recursive.
- Functions whose direct self-recursive calls are all in tail position may use loop-style Java lowering, even without the `rec` marker.
- `fun rec` validates direct tail-recursive shape for the declared function body.
- `fun rec` violations are rejected at compile time instead of silently compiling as ordinary recursion.
- Non-tail recursive functions without `rec` remain valid recursive functions and keep ordinary call-style Java lowering.

## Consequences

Recursive performance intent is explicit in source, and invalid non-tail forms fail early.

Java backend behavior becomes more predictable for tail-recursive functions, with stack usage aligned to loop lowering when the compiler can prove the direct self-recursive shape.

This ADR does not broaden recursion semantics beyond direct self-recursion. The `rec` keyword remains the source-level assertion that recursion must be tail-recursive.
