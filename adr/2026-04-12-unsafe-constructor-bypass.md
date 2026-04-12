# Unsafe Constructor Bypass

- Status: accepted
- Deciders: Codex multi-agent workflow
- Date: 2026-04-12

## Status

Accepted.

## Context

Capybara supports smart constructors through `with constructor`, including constructors that return `Result[T]` to enforce invariants at creation time.

This works well for public call sites, but internal module code sometimes needs to construct values that are already known to satisfy those invariants. Existing code in `Date`, `Time`, and related modules carries unnecessary `Result[...]` plumbing for values that are provably valid.

The existing constructor-local `* { ... }` form already provides raw construction inside a constructor body, but there is no explicit same-module escape hatch for other trusted code in the defining file.

## Decision

Capybara adds `DataName! { ... }` as an explicit unsafe constructor bypass.

- `DataName { ... }` remains the default and unchanged.
- `DataName! { ... }` constructs the raw data value and skips the protected constructor pipeline.
- The bypass is allowed only from the defining module/file of that `data`.
- The bypass is allowed only when normal construction of that data would return `Result[...]`.
- The bypass reuses normal field checking, arity, spreads, positional arguments, and type inference rules.
- The feature is limited to `data` construction. It is not a general visibility change and it does not introduce a new imperative API.

## Consequences

Trusted module-internal code can avoid redundant `Result[...]` wrapping when it is constructing values that are already known valid.

Public callers still go through normal smart-constructor validation, so invariants remain enforced at module boundaries.

The language gains a small amount of extra syntax and one explicitly unsafe escape hatch, so the compiler must provide clear diagnostics and the syntax tooling must highlight it distinctly.
