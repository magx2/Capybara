# ADR-2026-05-24: Declaration Annotations v1

- Status: accepted
- Deciders: Codex workflow, repository maintainers
- Date: 2026-05-24

## Context

Issue #177 introduces declaration annotations as public language syntax and as
metadata exposed through reflection. Because annotations affect source
compatibility, linked artifacts, and runtime-facing metadata, the v1 contract
must be fixed before grammar, linker, validator, reflection, or backend work
starts.

Existing decisions remain the baseline:

- `ADR-2026-05-07-cfun-derive-v1.md` defines explicit derive expansion and keeps
  arbitrary compile-time execution out of scope.
- `ADR-2026-05-08-reflection-v1.md` defines reflection as the official metadata
  surface.
- `ADR-2026-04-14-capybara-oo-v1.md` defines the OO frontend boundary and OO
  override validation as explicit language behavior.
- `ADR-2026-04-29-cfun-rec-tail-recursion.md` defines the built-in
  `@Recursive` annotation as an explicit recursion contract.

No earlier ADR defines declaration annotations.

## Decision

Capybara v1 declaration annotations are source-level declaration prefixes that
attach validated metadata to the declaration that immediately follows them.

Example usage:

```cfun
@Deprecated(message: "use parse_v2", since: "1.4")
fun parse(raw: String): Result[User] = ...

@Test(name: "parses user")
fun should_parse_user(): bool = true
```

```coo
@Entity(table: "users")
class User(name: String) {
    @JsonName(value: "user_name")
    field name: String = name
}
```

Annotation definitions are top-level `.cfun` declarations:

```cfun
annotation Deprecated on fun, method, const, data, union, enum, type, deriver, class, interface, trait, field, init {
    message: String
    since: String = ""
}

annotation JsonName on field {
    value: String
}

annotation Internal on fun, data, class {}
```

The standard library provides `Deprecated` from `/capy/meta_prog/Annotations`
with the same shape. It is ordinary metadata, not a compiler-special annotation.
Reflection exposes it on declaration kinds that have a reflection surface; uses
on other valid targets remain linked metadata for compiler and tooling
consumers.

V1 rules:

- Annotation usage is declaration-prefix only. An annotation attaches to the next
  declaration and is invalid on expressions, statements, parameters, local
  variables, or other non-declaration positions.
- Exactly one annotation is allowed per annotation prefix.
- Comma-separated forms such as `@A, @B` or `@A(), @B()` are invalid because each prefix
  holds one annotation.
- Multiple annotations are written as multiple prefixes and preserve source
  order in linked metadata and reflection:

  ```cfun
  @Internal
  @Deprecated(message: "use parse_v2")
  fun parse(raw: String): Result[User] = ...
  ```

- Annotation arguments are named only: `key: value`.
- Positional arguments, shorthand arguments, spread arguments, and mixed
  positional/named forms are invalid.
- Annotations with no explicit arguments may omit empty parentheses: `@Internal`.
  `@Internal()` remains valid for compatibility.
- Annotation definitions are declared only in top-level `.cfun` source. `.coo`
  declarations may use visible `.cfun` annotation definitions, but `.coo`
  annotation definitions are not part of v1.
- Annotation definitions participate in normal module visibility and import
  rules. Unknown or unimported annotation names are compile errors.
- Unknown keys, duplicate keys, missing required keys, wrong value types, and
  invalid targets are compile errors.
- Defaulted annotation fields are included in the effective metadata returned by
  reflection.
- V1 annotations are metadata and validation only.
- V1 annotations must not execute code, run macros, perform IO, mutate compiler
  state, or transform declarations.
- Ordinary user-defined annotations do not replace explicit language features:
  `derive Show`, OO `override`, visibility or mutability modifiers, and other
  source keywords keep their existing semantics.
- `@Recursive` is the one standard v1 annotation with compiler behavior. It is
  declared as `/capy/meta_prog/Recursive.Recursive`, must be visible through
  normal imports, and replaces the old `fun rec` keyword form.
- `@Recursive` may be used on top-level and local `.cfun` functions only. It
  is invalid on `.cfun` type methods and `.coo` methods.
- User-defined annotations remain invalid on local definitions; local functions
  accept only the standard `/capy/meta_prog/Recursive` marker.
- `@Override()` must not satisfy OO override semantics. If an OO declaration
  requires the real override marker or modifier, an annotation named `Override`
  is only metadata.
- Reflection is the first official runtime-facing consumer of annotation
  metadata.
- Annotation metadata must not affect value equality, object identity,
  stringification, generated `show` behavior, or derived JSON/string output
  unless a future explicit feature says otherwise.

## Targets

V1 annotation target identifiers are exactly:

- `fun`
- `method`
- `const`
- `data`
- `union`
- `enum`
- `type`
- `deriver`
- `class`
- `interface`
- `trait`
- `field`
- `init`

The target matrix is:

| Source | Target | V1 target identifier |
| --- | --- | --- |
| `.cfun` | top-level functions | `fun` |
| `.cfun` | functional methods written as `fun Type.method` | `method` |
| `.cfun` | constants | `const` |
| `.cfun` | data declarations | `data` |
| `.cfun` | data fields | `field` |
| `.cfun` | union declarations | `union` |
| `.cfun` | union case fields | `field` |
| `.cfun` | enum declarations | `enum` |
| `.cfun` | primitive-backed `type` declarations | `type` |
| `.cfun` | deriver declarations | `deriver` |
| `.cfun` | deriver methods | `method` |
| `.coo` | classes | `class` |
| `.coo` | interfaces | `interface` |
| `.coo` | traits | `trait` |
| `.coo` | fields | `field` |
| `.coo` | class and trait methods | `method` |
| `.coo` | interface methods | `method` |
| `.coo` | `init` blocks | `init` |

The following targets are postponed and invalid in v1:

- parameters
- local variables
- local `.cfun` definitions
- `.coo` local methods
- expressions
- statements

Future ADRs may split method subtargets or add finer-grained targets. V1 keeps a
small target set so the grammar, linker, diagnostics, reflection model, and all
backends can agree on one portable contract.

## Annotation Values

V1 annotation values are a closed metadata value set:

- string literals
- integer literals
- long literals
- float literals
- double literals
- boolean literals
- `???`
- type references as metadata-only type-name values

Type-name values are written as bare type references using the same type-token
forms accepted in type positions, for example `String`, `User`, `Result[User]`,
or a fully qualified type name when the language supports one. An identifier in
annotation value position is metadata only. It is not a local variable,
function, enum case, field, or computed expression.

Reflection exposes type-name values as `AnnotationTypeName` metadata whose
`value` field is the parsed type-reference text. Backends must not lower
type-name values to runtime class objects in v1.

`???` is allowed as an annotation value only as metadata. Reflection exposes it
as a distinct unknown annotation value, not as a thrown exception or executable
placeholder.

The following values are invalid in v1:

- full expressions
- lambdas
- lists
- sets
- dicts
- tuples
- function calls
- member access used as a value expression
- arithmetic, boolean, comparison, or pipeline expressions
- computed values of any kind

## Reflection Contract

Reflection v1 is extended with annotation metadata without changing the core
reflection decision in `ADR-2026-05-08-reflection-v1.md`.

Every reflected declaration kind that can be annotated must expose its
annotations in source order. A declaration without annotations exposes an empty
annotation list.

Each reflected annotation entry must include:

- the annotation's source name
- the annotation definition's owner package
- the effective named arguments, including defaults
- each argument value as one of the closed v1 annotation value variants

Reflection consumers must observe the same metadata whether annotations are
written in the current module or come from imported linked artifacts.

Annotation metadata is descriptive. It must not affect declaration identity,
runtime equality, hashing, stringification, or existing reflection fields other
than the added annotation metadata fields.

## Backend Parity

All supported backends for a reflected declaration kind must preserve the same
validated annotation metadata and source order. A backend may choose its native
metadata representation, but the public reflection API must expose equivalent
annotation names, owner packages, arguments, defaults, type-name values, and
unknown values.

Backends must not silently drop annotations for declarations that reflection can
observe. If a backend cannot provide equivalent annotation metadata for a
supported reflection path, that path must remain unsupported or fail explicitly
until parity is implemented.

Because annotations do not transform declarations in v1, ordinary generated code
for non-reflection behavior should be unchanged except for whatever metadata
storage is required by reflection.

## Non-goals

V1 does not introduce macros, compiler plugins, custom derive replacement,
conditional compilation, retention policies, runtime annotation classes, Java
annotation interop, dependency injection hooks, test discovery behavior, or
backend-specific code generation hooks.

Derive remains the explicit compile-time generation mechanism described by
`ADR-2026-05-07-cfun-derive-v1.md`. Except for the reserved built-in
`@Recursive` contract, OO override validation and other language features
remain explicit source constructs rather than annotations.

## Consequences

Annotations become a stable cross-frontend metadata feature without making the
compiler execute user code or making annotations a replacement for existing
language syntax.

The narrow value set keeps parsing, linked metadata, diagnostics, and backend
reflection portable. Future annotation features can add targets or value forms
through new ADRs without changing the v1 meaning of annotation prefixes or named
arguments.
