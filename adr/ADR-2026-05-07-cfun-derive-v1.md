# ADR-2026-05-07: `.cfun` Compile-Time Derive v1

- Status: accepted
- Deciders: Codex multi-agent workflow
- Date: 2026-05-07

## Status

Accepted.

## Context

Capybara has parser AST declarations, linked type metadata, compiler intrinsics for reflection, and backend-independent functional IR, but it has no source-level way to ask the compiler to generate declarations from type metadata.

General procedural macros, compiler plugins, or arbitrary compile-time execution would require a larger evaluator, hygiene model, sandboxing story, incremental-compilation contract, and linked-artifact format. Existing ADRs cover explicit compile-time contracts such as `fun rec` and unsafe constructor bypass, but none covers metaprogramming or derive expansion.

## Decision

Capybara Functional v1 adds a constrained derive system:

- Top-level `data` and `type` declarations may end with `derive DeriverName`.
- A top-level `deriver DeriverName { ... }` declaration registers pure method templates in the same module.
- Deriver methods use normal expression syntax and are expanded by the compiler into ordinary generated type methods.
- Generated methods use the existing functional method encoding and pass through normal signature linking, type checking, diagnostics, linked JSON, and backend generation.
- Deriver methods do not receive compiler-injected metadata parameters. The generated method receiver is available as `receiver`.
- Derivers inspect metadata through the existing reflection API. `reflection(receiver)` returns `DataValueInfo`, whose `fields` expose linked field metadata and values.
- Metadata values come from linked field metadata, so inherited parent fields and validated field shapes are preserved.
- V1 does not evaluate arbitrary `.cfun` at compile time, does not run user IO, and does not expose mutable compiler state.
- V1 keeps `.coo` derive support out of scope. OO remains a separate frontend and is still Java-only for code generation.
- The early helper names `derive_type_name()` and `derive_fields_join(...)` are rejected with migration diagnostics instead of remaining as magic globals.

Example:

```cfun
from /capy/meta_prog/Reflection import { DataValueInfo, reflection }

deriver Show {
    fun show(): string =
        let info: DataValueInfo = reflection(receiver)
        let body: string = info.fields |> info.name + " { ", (acc, field) =>
            acc + (if acc == info.name + " { " then "" else ", ") + field.name
        body + " }"
}

data User { name: string, age: int } derive Show
```

This generates an ordinary `User.show(): string` method during compilation.

## Consequences

Derive expansion is deterministic and backend-independent because generated behavior becomes ordinary checked Capybara declarations instead of backend text.

The system is intentionally smaller than Scala macros, Template Haskell, Rust procedural macros, or OCaml PPX. It leaves room for a future typed quote/splice or plugin architecture without committing v1 to arbitrary compile-time execution.

Derivers are same-module only in v1 because compiled module artifacts do not yet preserve deriver declarations. Cross-module deriver publication requires a future linked-artifact and import/export decision.

The first metadata API is data-oriented but deliberately minimal. Richer type metadata, generated declaration values, typeclass/capability constraints, generic derivation strategies, and generated type declarations remain future work.
