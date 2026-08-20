# ADR-2026-05-21: `.cfun` Empty Data v1

- Status: accepted
- Deciders: Codex multi-agent workflow
- Date: 2026-05-21

## Context

Capybara Functional historically used two declaration forms for product values:
`data Name { fields }` for values with fields and `single Name` for fieldless
values. Runtime construction already used the same explicit value form for both:
`Name {}`.

Existing ADRs for unsafe constructor bypass, derive, and reflection depend on
normal data construction, field checking, and data metadata. None defines a
separate long-term role for `single`.

## Decision

Capybara Functional removes the source-level `single` declaration.

- Fieldless product values are declared as ordinary empty data:
  `data Name {}`.
- Empty data values are still constructed explicitly: `Name {}`.
- Bare `{}` remains the empty set literal; empty dict remains `{:}`.
- Bare enum values remain valid enum syntax. Fieldless data values are not bare
  expressions and must use `Name {}`.
- `case Name` remains the nullary constructor-pattern shorthand, and
  `case Name {}` remains valid explicit constructor-pattern syntax.
- Non-generic zero-field data without data extension, a custom constructor, or
  derive directives may keep the existing singleton runtime lowering. This
  preserves generated ABI and behavior for stdlib values such as `None`, `End`,
  `Success`, and `JsonNull`.
- Generic zero-field data and data extending parent fields are ordinary data,
  not singleton runtime values.
- Old `single Name` source should fail with a migration diagnostic that points
  to `data Name {}`.

## Consequences

The source language has one data declaration form for all product arities,
matching the ADT style of Haskell, OCaml, and F# more closely than a separate
nullary declaration keyword.

Reflection and derive continue to see empty data as data with an empty field
list. Backends can preserve old singleton-like output for non-generic empty
data, but that is an implementation strategy rather than a source construct.

The compiler must keep normal data-construction validation for empty data:
extra named, spread, or positional assignments are errors.
