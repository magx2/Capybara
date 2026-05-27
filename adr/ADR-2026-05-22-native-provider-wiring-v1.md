# ADR-2026-05-22: Native Provider Wiring v1

- Status: accepted
- Deciders: Codex, repository maintainers
- Date: 2026-05-22

## Status

Accepted. The v1 slice is implemented for compile-time provider annotations
with backend wiring metadata, and Java, JavaScript CommonJS, and Python provider
bootstrap generation.

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

Capybara OO owns the interface contract. A native provider targets the
annotated Capybara OO interface plus qualifier; the stable interface id comes
from the annotation target and is not repeated in sidecar configuration.

Host implementations are wired through compile-time annotation metadata. v1
must not use mutable runtime registration as the provider selection mechanism.

After `ADR-2026-05-24: Declaration Annotations v1`, the implementation slice
uses a standard `.cfun` annotation from `/capy/meta_prog/NativeProvider` to
declare a typed provider symbol on the Capybara OO interface. The annotation
names the qualifier and optional backend implementation metadata; the interface
id comes from the annotated interface and the generated provider symbol is
derived from the qualifier plus interface name. `.coo` code may mark an
interface as a native provider contract, but it must not import Java packages,
CommonJS modules, npm packages, or Python modules directly:

```coo
from /capy/meta_prog/NativeProvider import { NativeProvider }

@NativeProvider(
    qualifier: "system",
    lifetime: "factory",
    javaClassName: "dev.capylang.test.nativeinterop.SystemClock",
    javascriptModule: "../../nativeinterop/system_clock.js",
    javascriptExport: "SystemClock",
    pythonModule: "nativeinterop.system_clock",
    pythonClassName: "SystemClock"
)
interface Clock {
    def now_millis(): long
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

Backend factories default to Java `constructor`, JavaScript `new`, and Python
`call`. JavaScript may also use `call`.

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

- parse and link typed `@NativeProvider` annotations for `.coo` interfaces;
- read backend wiring metadata from the `@NativeProvider` annotation;
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

## Implemented v1

Supported `.coo` syntax:

```coo
from /capy/meta_prog/NativeProvider import { NativeProvider }

@NativeProvider(
    qualifier: "system",
    lifetime: "factory",
    javaClassName: "dev.capylang.test.nativeinterop.SystemClock",
    javascriptModule: "../../nativeinterop/system_clock.js",
    javascriptExport: "SystemClock",
    pythonModule: "nativeinterop.system_clock",
    pythonClassName: "SystemClock"
)
interface Clock {
    def now_millis(): long
}
```

The provider symbol is derived from the qualifier and interface name, so this
example is callable as `system_clock()` and returns the annotated Capybara
interface type. The compiler derives `/dev/capylang/test/Clock` from the
annotated interface and uses the backend metadata from the annotation. External
native wiring manifests remain accepted as compatibility input, but e2e native
provider wiring is represented in Capybara source annotations rather than JSON
sidecar files.

Supported lifetimes are `singleton` and `factory`.

Supported backend factories are:

- Java `className` with `factory: "constructor"`;
- JavaScript CommonJS `module` plus `export` with `factory: "new"` or
  `"call"`;
- Python `module` plus `className` with `factory: "call"`.

Implemented diagnostics use these stable terms: `NotWired`,
`DuplicateProvider`, `TypeMismatch`, `UnsupportedBackend`, and
`InvocationFailure`. Provider-related diagnostics include the provider symbol
when known, interface id, qualifier, backend for backend-specific failures, and
source or manifest file path when the reporting layer has it.

Recorded v1 non-goals are: no mutable runtime registration, no direct `.coo`
host imports, no automatic async handling, no scoped/request lifetime, no
resource disposal hooks, and no pure `.cfun` native lookup.

## Consequences

Domain code depends on Capybara interfaces and provider symbols. Provider
contract files may carry backend selector strings in `@NativeProvider`, while
ordinary consumers still call typed provider symbols and do not import host
runtime modules directly.

Compiler and generator diagnostics can be deterministic for missing providers,
duplicate provider keys, unknown target interfaces, unsupported backend
metadata, and host shape mismatches where those can be checked before runtime.

Java generation can wire providers with generated interface types and concrete
class names from annotation metadata. `factory: "constructor"` maps to direct
construction or a generated constructor reference. Singleton providers require a
thread-safe cached instance; factory providers create a new host object for each
lookup.

JavaScript CommonJS generation can lower annotation metadata to deterministic
`require(...)` calls for the selected backend and export name. The generated
provider table remains immutable. Startup or lookup validation must report
missing modules, missing exports, wrong factory shape, or incompatible objects
against the provider key.

Python generation can lower annotation metadata to deterministic imports for
the selected module and class name. The generated provider table remains
immutable. Startup or lookup validation must report missing modules, missing
classes, wrong factory shape, or incompatible objects against the provider key.

Because `.cfun` access remains effectful, functional callers cannot accidentally
treat host-backed object lookup or invocation as a pure expression. Pure native
function facades, if needed later, require a separate decision.
