# ADR-2026-04-14: Capybara OO v1 Frontend Boundary

- Status: proposed
- Deciders: Codex, repository maintainers
- Date: 2026-04-14

## Implementation Update

As of 2026-05-21, this ADR records the initial frontend and Java-backend
boundary, but the implementation has moved beyond that first slice.

- `.coo` modules are parsed, validated, and generated for Java, JavaScript, and
  Python.
- The tested OO surface includes classes, interfaces, behavior-only traits,
  class fields, constructor parameters, expression-bodied and block-bodied
  methods, loops, exceptions, arrays, stdout calls, FP interop, and static
  `type()` metadata.
- Trait fields and trait `init` blocks remain unsupported.
- Java still has the strictest entrypoint checks: entrypoint classes cannot
  declare constructor state or `init` blocks. All backends reject entrypoint
  methods that use instance state or parent-qualified calls.
- JavaScript and Python no longer reject `.coo` modules by default; references
  below to Java-only OO generation describe the original v1 slice, not current
  backend coverage.

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
- Arbitrary value expressions are not valid stand-alone statements in OO method blocks.
- Call expressions are allowed as stand-alone statements in OO method blocks to support explicit side effects.
- v1 statement set is intentionally minimal:
  - `let`
  - mutable local `def`
  - assignment
  - call statements
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
  - `try { ... } catch "capy.error.kind" error { ... } catch error { ... }`
- `try` / `catch` is statement-only in v1.
- `catch` binds one immutable local `capy/lang/Result.Error` variable.
- A catch branch may specify an error `kind` string before the variable name. Branches are checked in source order.
- A catch branch without a kind is the fallback. If no branch matches and no fallback exists, the `Error` is rethrown.
- `finally` is postponed.
- Java-style checked exceptions and generated `throws` declarations are not used for value-level failures.
- Java lowering is the reference implementation:
  - `throw` type-checks its expression as `capy/lang/Result.Error`
  - `throw` lowers through an unchecked backend wrapper that preserves the structured `Error` value
  - `catch` catches that wrapper and binds the original structured `Error`
  - host `RuntimeException` values caught at this boundary are normalized into structured `Error` values
- Functional `Result.Error` remains the value-level recoverable failure representation.
- OO `throw` / `catch` carries the same structured `Error` value, but remains a separate control-flow mechanism.
- There is no implicit compiler conversion between thrown OO `Error` values and functional `Result` failures.
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

## Output v1

- Capybara OO output is library-shaped, not syntax-shaped.
- The standard stdout owner type is `/capy/io/Stdout`.
- Preferred usage imports its static methods:
  - `from /capy/io/Stdout import { * }`
  - `print(text: String): void`
  - `println(text: String): void`
- Output is an ordinary OO side effect and does not implicitly bridge to functional `/capy/lang/Program` or `Result.Error`.
- Java is the reference backend:
  - imported `print` lowers through `/capy/io/Stdout.print`, then `System.out.print`
  - imported `println` lowers through `/capy/io/Stdout.println`, then `System.out.println`
- Future Python and JavaScript backends should preserve the same OO surface while mapping to their native stdout mechanisms.
