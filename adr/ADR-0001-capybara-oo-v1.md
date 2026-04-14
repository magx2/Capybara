# ADR-0001: Capybara OO v1 Frontend Boundary

- Status: proposed
- Deciders: Codex, repository maintainers
- Date: 2026-04-14

## Context

Capybara currently ships only the functional frontend. The compiler, linked JSON, CLI source discovery, diagnostics, tests, and IntelliJ syntax support all assume `.cfun` inputs parsed by `Functional.g4`.

The repository already contains an empty `ObjectOriented.g4`, but there is no existing OO grammar, parser, AST, semantic validation pass, or backend lowering. The `adr/` directory also contains no earlier OO decision to reuse.

## Decision

Capybara OO v1 starts with a separate frontend boundary instead of extending `Functional.g4`.

- `.cfun` remains the functional language and keeps using `Functional.g4`.
- `.coo` is the object-oriented source extension and will use `ObjectOriented.g4`.
- Source handling must become extension-aware before OO semantics land.
- OO syntax uses `fun`, not `def`, to stay aligned with Capybara's existing declaration style.
- This first slice only adds `.coo` discovery, parsing, editor support, and extension-correct diagnostics.
- The compiler must fail explicitly for `.coo` modules until the OO semantic and backend pipeline exists.

## Consequences

- `Functional.g4` can remain stable while the OO frontend evolves independently.
- CLI and diagnostics stop silently ignoring `.coo` files.
- The repository gains a safe landing zone for parser tests, IntelliJ support, and later OO semantic work.
- Full OO support still requires follow-up decisions for:
  - OO AST and linked IR shape
  - inheritance and override validation
  - visibility semantics
  - trait state and initialization order
  - `.cfun`/`.coo` interoperability
  - Java-first lowering and explicit JS/Python behavior
