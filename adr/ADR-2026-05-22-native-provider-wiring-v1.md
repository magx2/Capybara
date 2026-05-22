# ADR-2026-05-22: Native Provider Wiring v1

- Status: proposed
- Deciders: Codex, repository maintainers
- Date: 2026-05-22

## Status

Proposed.

## Context

Capybara OO now has nominal interfaces that can be generated for Java,
JavaScript, and Python. Native interop needs a way to connect those Capybara
interfaces to host implementations without weakening the `.coo` frontend
boundary or turning backend-specific imports into language syntax.

Existing decisions constrain the design:

- `ADR-2026-04-14: Capybara OO v1 Frontend Boundary` keeps `.coo` separate from
  `.cfun` and keeps interop explicit at boundaries.
- `ADR-2026-05-08: Reflection v1` makes reflection metadata descriptive rather
  than an arbitrary metaprogramming or invocation mechanism.
- `2026-04-12: Unsafe Constructor Bypass` scopes unsafe construction to
  `.cfun data` values.

Before changing compiler behavior, Capybara needs one decision for how native
providers are named, wired, validated, and generated across Java, JavaScript
CommonJS, and Python.

## Decision

Capybara adopts compile-time native provider wiring for the first native
interop implementation slice.

Capybara OO owns the interface contract. A native provider targets a Capybara
OO interface by stable interface id and qualifier; the host implementation is
an implementation detail selected by compiler/build metadata.

Host implementations are wired through compile-time metadata, such as a
provider manifest consumed by the compiler or generator. v1 must not use mutable
runtime registration as the provider selection mechanism.

`.coo` code may declare a typed provider symbol, but it must not import Java
packages, CommonJS modules, npm packages, or Python modules directly:

```coo
interface Clock {
    def now_millis(): long
}

native provider system_clock: Clock key "system"
```

The corresponding provider manifest shape for v1 is:

```json
{
  "providers": [
    {
      "interface": "/dev/capylang/test/Clock",
      "qualifier": "system",
      "lifetime": "factory",
      "java": {
        "className": "dev.capylang.test.nativeinterop.SystemClock",
        "factory": "constructor"
      },
      "javascript": {
        "module": "./nativeinterop/system_clock.js",
        "export": "SystemClock",
        "factory": "new"
      },
      "python": {
        "module": "nativeinterop.system_clock",
        "className": "SystemClock",
        "factory": "call"
      }
    }
  ]
}
```

The v1 provider key is `(interfaceId, qualifier)`. The first implementation
slice supports exactly one provider for each key in the selected
compile/generate input set. Duplicate providers for the same key fail
deterministically; they are not merged, ordered by precedence, or accepted
silently.

Provider lifetimes supported in v1 are:

- `singleton`: the generated provider table creates or stores one host object
  and returns the same object for each lookup.
- `factory`: the generated provider table constructs or calls a provider
  factory for each lookup.

Reflection metadata remains descriptive. It may document OO shapes, but it must
not be used as the dispatch, invocation, or host-provider validation mechanism
for native provider wiring.

The unsafe constructor bypass remains scoped to `.cfun data` construction. It
must not be reused for native OO provider wiring or host object construction.

`.cfun` access to native-backed OO objects remains effectful. Functional code
that obtains or invokes native-backed OO values must cross the existing effect
boundary; this issue must not expose pure `.cfun` host calls.

The named first implementation slice is "native provider wiring v1". Its scope
is:

- parse and link typed `native provider` declarations for `.coo` interfaces;
- read a structured provider manifest for the selected backend;
- validate provider key uniqueness and target interface existence;
- generate immutable provider tables or typed provider methods;
- support Java, JavaScript CommonJS, and Python backend metadata;
- support only the `singleton` and `factory` lifetimes.

Non-goals for this slice are:

- direct Java, JavaScript, npm, CommonJS, or Python imports in `.coo`;
- mutable runtime provider registration or test overrides;
- reflection-based invocation, dispatch, or validation;
- reusing unsafe `.cfun data` constructor bypass behavior;
- multiple providers for the same `(interfaceId, qualifier)` key;
- provider disposal, scopes beyond singleton/factory, and async host APIs;
- pure `.cfun` host calls or automatic conversion of host effects into pure
  functional values.

## Consequences

Capybara source stays backend-neutral: domain code depends on Capybara
interfaces and provider symbols, while host class names and module names live in
compiler inputs or build metadata.

Compiler and generator diagnostics can be deterministic for missing providers,
duplicate provider keys, unknown target interfaces, unsupported backend
metadata, and host shape mismatches where those can be checked before runtime.

Java generation can wire providers with generated interface types and concrete
class names from the manifest. `factory: "constructor"` maps to direct
construction or a generated constructor reference. Singleton providers require a
thread-safe cached instance; factory providers create a new host object for each
lookup.

JavaScript CommonJS generation can lower manifest entries to deterministic
`require(...)` calls for the selected backend and export name. The generated
provider table remains immutable. Startup or lookup validation must report
missing modules, missing exports, wrong factory shape, or incompatible objects
against the provider key.

Python generation can lower manifest entries to deterministic imports for the
selected module and class name. The generated provider table remains immutable.
Startup or lookup validation must report missing modules, missing classes, wrong
factory shape, or incompatible objects against the provider key.

Because `.cfun` access remains effectful, functional callers cannot accidentally
treat host-backed object lookup or invocation as a pure expression. Pure native
function facades, if needed later, require a separate decision.
