# ADR-2026-04-29: `.cfun` `@Recursive` Tail-Recursion Contract

- Status: accepted
- Deciders: Codex, repository maintainers
- Date: 2026-04-29
- Amended: 2026-05-24, replacing the `fun rec` keyword form with the standard
  `@Recursive` annotation form.

## Context

Issue #100 introduced `.cfun` `fun rec` as an explicit recursion form with
compiler checks and backend consequences. Declaration annotations now provide a
single prefix syntax for declaration-level markers, so the recursion contract is
expressed as the standard `@Recursive` annotation instead of a dedicated
keyword.

Without a dedicated marker, recursive intent and optimization expectations are implicit. That makes it hard to guarantee predictable lowering and diagnostics for recursion-heavy code.

## Decision

Capybara detects direct self-recursion for every function. It treats the
standard `/capy/meta_prog/Recursive` annotation as a compiler-verified direct
tail-recursion contract.

- `Recursive` is declared in the standard library as
  `/capy/meta_prog/Recursive.Recursive` and must be visible through normal
  imports.
- User annotations named `Recursive` are ordinary metadata and do not trigger
  the tail-recursion contract.
- `@Recursive` is declaration-local and only meaningful on `.cfun` function
  declarations.
- `@Recursive` may be used on top-level functions and local functions.
- `@Recursive` is not supported on `.cfun` type methods or `.coo` methods.
- The compiler records whether each function is directly recursive.
- Functions whose direct self-recursive calls are all in tail position may use
  loop-style Java lowering, even without the `@Recursive` marker.
- `@Recursive` validates direct tail-recursive shape for the declared function
  body.
- `@Recursive` violations are rejected at compile time instead of silently
  compiling as ordinary recursion.
- Non-tail recursive functions without `@Recursive` remain valid recursive
  functions and keep ordinary call-style Java lowering.

## Consequences

Recursive performance intent is explicit in source, and invalid non-tail forms fail early.

Java backend behavior becomes more predictable for tail-recursive functions, with stack usage aligned to loop lowering when the compiler can prove the direct self-recursive shape.

This ADR does not broaden recursion semantics beyond direct self-recursion.
`@Recursive` is the source-level assertion that recursion must be
tail-recursive.
