# ADR-2026-05-08: Reflection v1

- Status: accepted
- Deciders: Codex workflow
- Date: 2026-05-08

## Context

Derive, JSON encoding, runtime type patterns, and OO Java generation now all need a small, explicit reflection model. The first branch exposed this through `/capy/meta_prog/Reflection`; that module works, but it mixes value reflection, `.cfun` type metadata, and `.coo` type metadata in one namespace.

## Decision

Reflection v1 is a language and runtime feature, not an arbitrary metaprogramming escape hatch.

- `.cfun` value reflection is supported for concrete `data` values and for dynamic values typed as `data`.
- The value-reflection intrinsic is exact: only the stdlib reflection function selected by overload resolution lowers to compiler IR. User functions named `reflection` are ordinary functions.
- `.cfun` metadata includes data, primitive, collection, tuple, function, package, field, and field-value shapes used by derive and user code.
- `.coo` supports static Java-backend `type()` metadata for classes, interfaces, and traits. Source-level `.coo` reflection APIs remain out of scope.
- Runtime type patterns are shallow in v1: primitives, `data`, bare `List`, bare `Set`, bare `Dict`, and nominal data/type names. Parameterized runtime patterns such as `List[String]`, `Dict[int]`, `Option[int]`, nested generics, tuple patterns, and function-type patterns are rejected.
- The public API should move toward split modules for value reflection, functional type metadata, and OO metadata. The existing `/capy/meta_prog/Reflection` module remains the Java-backed compatibility surface for this branch until the generated Java package migration is completed.
- `CapybaraDataValue.capybaraDataValueInfo()` keeps returning `Object` in the Java runtime interface because the runtime helper module is compiled before generated stdlib reflection classes. Generated implementations still return the concrete reflection record at runtime.
- Java is the implemented backend for reflection v1. Other backends must reject or clearly remain unsupported until they implement equivalent metadata emission.

## Consequences

Derive and JSON encoding can depend on reflection without treating every same-named function as magic. The shallow runtime pattern rule avoids pretending erased Java generics can be tested soundly.

The compatibility module keeps existing code working, but new public docs should describe value, `.cfun` type, and `.coo` type reflection as separate surfaces so the package split can happen without redesigning semantics.
