# ADR-2026-04-14: Capybara OO v1 Frontend Boundary

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
- OO syntax uses `def`, while functional `.cfun` keeps `fun`.
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

## Method Bodies v1

- `.coo` methods support either `= expression` bodies or brace-delimited statement blocks.
- `.coo` reuses the functional expression core from `Functional.g4` as closely as practical.
- `ObjectOriented.g4` should follow the same grammar architecture as `Functional.g4` where practical:
  - shared expression layering
  - shared qualified-type handling
  - shared function-type notation
- Expressions are not valid stand-alone statements in OO method blocks.
- v1 statement set is intentionally minimal:
  - `let`
  - mutable local `def`
  - assignment
  - `return`
  - `if` / `else`
  - `for` / `foreach`
  - `while`
  - `do` / `while`
  - `throw`
  - `try` / `catch`
  - nested statement blocks

## Exceptions v1

- Capybara OO adopts the smallest common denominator shared by Java, Python, and JavaScript:
  - `throw expression`
  - `try { ... } catch error { ... }`
- `try` / `catch` is statement-only in v1.
- `catch` binds one immutable local exception variable.
- `finally` is postponed.
- Checked exceptions and `throws` declarations are postponed.
- Java lowering is the reference implementation:
  - `throw` lowers through `dev.capylang.CapybaraException.wrap(...)`
  - `catch` lowers to `catch (RuntimeException error)`
- Functional `Result.Error` remains a value-level error representation, not an exception.
- There is no implicit compiler conversion between thrown OO exceptions and functional `Result.Error`.
- If interop is needed, it must stay explicit at library or user-code boundaries rather than becoming hidden control flow.

## Java Backend v1

- `.coo` now participates in the Java backend only.
- The compiler keeps parsed OO modules alongside the functional linked program instead of forcing OO declarations into the functional IR.
- Java generation is intentionally narrow in this slice:
  - classes
  - interfaces
  - behavior-only traits
  - fields on classes
  - expression-bodied and block-bodied instance methods
- Traits with state or init blocks remain unsupported in the Java backend.
- JavaScript and Python generation must still reject `.coo` modules explicitly.
