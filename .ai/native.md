# Native Host Wiring For Capybara OO

Status: research and design note. This is not an implemented compiler feature.

This document records a compile-time wiring direction: Capybara owns the
interface contract, while Java, JavaScript, or Python code supplies a host
class/object/factory that implements that interface. The compiler consumes the
wiring metadata, validates what it can, and generates an immutable provider
table. Capybara code depends on Capybara interfaces, not on host-language class
names.

## Existing Decisions

- `adr/ADR-2026-04-14-capybara-oo-v1.md` keeps `.coo` separate from `.cfun`;
  native wiring must preserve that frontend boundary.
- `adr/ADR-2026-04-14-capybara-oo-v1.md` also records that OO exceptions are
  not automatically converted to functional `Result.Error`.
- `adr/ADR-2026-05-08-reflection-v1.md` treats reflection as metadata, not as a
  dynamic invocation mechanism. Reflection metadata is descriptive only and must
  not become a dynamic invocation path for native objects.
- `adr/2026-04-12-unsafe-constructor-bypass.md` limits unsafe construction to
  `.cfun data` values. It must not be reused for OO native provider wiring.
- `.ai/interoperability.md` covers `.cfun` to `.coo` construction and method
  invocation planning. `.cfun` access to wired OO/native objects must cross the
  same `Effect` boundary when functional code obtains or invokes them.

No existing ADR covers Java/JavaScript/Python native host provider wiring. The
eventual implementation should create one native interop ADR, or update an
existing matching ADR if one appears before implementation starts, with
cross-links from the OO v1 ADR and `.ai/interoperability.md`.

## Current State

- `.coo` already has interfaces in `compiler/src/main/antlr/ObjectOriented.g4`.
- Java generation already emits Java interfaces for `.coo` interfaces, so Java
  has a natural host-side implementation target.
- JavaScript and Python do not yet have generated native provider tables.
- `.coo` imports are Capybara module imports only. They do not import Java
  packages, CommonJS modules, npm packages, or Python modules.
- `.cfun` has `<native>` for native data bodies and function placeholders, but
  that is backend/std-lib specific and not a general OO dependency-injection
  model.
- Existing native behavior is currently backend/std-lib specific: hardcoded
  generator mappings for system time, random seed, parsing, console, and IO are
  not a general OO dependency-injection model.

## Core Model

Capybara source defines a normal OO interface:

```coo
interface Clock {
    def now_millis(): long
}

class ReportService(clock: Clock) {
    field clock: Clock = clock

    def started_at(): long =
        this.clock.now_millis()
}
```

The `Clock` interface is normal Capybara OO source. It is not marked native.

Host code is wired to that Capybara interface during compilation/generation
with a structured provider block:

```text
provider "/app/Clock" {
    qualifier "system"
    lifetime factory

    java {
        class "app.SystemClock"
        constructor "()"
    }

    javascript {
        module "./clock.js"
        export "SystemClock"
        factory "new"
    }

    python {
        module "app.clock"
        class "SystemClock"
        factory "call"
    }
}
```

Capybara code receives the implementation through constructor injection,
factory injection, or a typed generated provider symbol:

```coo
native provider system_clock: Clock key "system"

class Services {
    def clock(): Clock =
        system_clock()
}
```

`native provider` syntax is illustrative until the grammar is accepted, but the
contract is useful: it gives the compiler a typed Capybara symbol for a wired
implementation without adding Java/JS/Python imports to `.coo`. The important
shape is that lookup returns a `Clock` because the key is the Capybara
interface, not the host class.

Host-side sketches:

```java
final class SystemClock implements app.Clock {
    public long nowMillis() {
        return java.lang.System.currentTimeMillis();
    }
}
```

```javascript
module.exports.SystemClock = class SystemClock {
    now_millis() {
        return Date.now();
    }
};
```

```python
class SystemClock:
    def now_millis(self):
        return int(time.time() * 1000)
```

The exact host method names should come from generated metadata, because
backends may expose `now_millis`, `nowMillis`, or another escaped identifier.
Host code should not imperatively register itself at runtime in v1; the build
or compiler wiring metadata names the implementation.

## Recommended Direction

Use compile-time native provider wiring as the primary model.

- Capybara interface = contract.
- Host wiring metadata = implementation selection.
- Capybara domain classes depend on interfaces, not host facades.
- Host class names appear only in compiler inputs, manifests, or build metadata.
- Missing, duplicate, or incompatible wiring fails deterministically during
  compile/generate when possible, or during generated startup validation before
  the provider is used.

Direct external facades such as `external class HostMath` are still useful for
stdlib-level bindings or small pure host functions, but they should be a
secondary mechanism. They should not be the main object interop model.

## Compile-Time Wiring Semantics

Provider key is the stable manifest key used by the compiler and generator:

- Fully qualified Capybara interface id, for example `/app/Clock`.
- Optional qualifier for multiple implementations, for example `system`,
  `test`, or `utc`.
- Optional future member/capability id only if a later design needs method-level
  wiring. It is not part of the v1 key.

The v1 lookup key is exactly `(interface, qualifier)`. When a key is wired, v1
allows exactly one provider for that key in the selected compile/generate input
set. Duplicate providers for the same `(interface, qualifier)` key must fail;
they must not be merged, ordered by precedence, or accepted silently.

Valid provider values:

- Singleton instance.
- Factory that returns a fresh implementation.
- Host class reference plus constructor/factory metadata.

The provider manifest shape should be structured enough for a deterministic
parser. A provider block can carry backend-specific implementation metadata
under one Capybara provider key:

```text
provider "/app/Clock" {
    qualifier "system"
    lifetime factory

    java {
        class "app.SystemClock"
        constructor "()"
    }

    javascript {
        module "./clock.js"
        export "SystemClock"
        factory "new"
    }

    python {
        module "app.clock"
        class "SystemClock"
        factory "call"
    }
}
```

For the example above, the provider key is `("/app/Clock", "system")`. The
`java`, `javascript`, and `python` blocks are selected by backend. Missing
metadata for the selected backend is a deterministic failure, not a runtime
registration problem.

Compilation lifecycle:

1. Parse `.coo` interfaces and provider declarations.
2. Read provider metadata for the selected backend.
3. Validate provider uniqueness for each `(interface, qualifier)` key.
4. Validate target interface existence.
5. Validate host shape where statically possible.
6. Emit an immutable provider table or typed provider methods.
7. Run generated startup validation for backend facts that cannot be checked at
   compile time.

There should be no mutable runtime registration in v1. Lookup for an unwired
provider should fail with `NotWired`. Duplicate wiring should fail with
`DuplicateProvider`. A wired value that does not conform to the interface should
fail with `TypeMismatch`.

Compile/generate diagnostics and generated startup/invocation failures should
use these categories. Every diagnostic must include the provider key
`(interface, qualifier)` and the selected backend when backend-specific metadata
was involved:

- `NotWired`: a required provider declaration has no provider value for the
  selected backend and key.
- `DuplicateProvider`: more than one provider value exists for the same v1
  `(interface, qualifier)` key.
- `TypeMismatch`: the target is not a known Capybara interface, the provider
  host shape does not conform to that interface, or a representability/null
  policy check fails.
- `UnsupportedBackend`: the selected backend has no supported metadata for the
  key, or the interface shape cannot be represented by that backend.
- `InvocationFailure`: generated provider construction, factory invocation, or
  host method invocation fails after compilation.

Thread-safety should be defined per backend for generated provider tables. Java
should make provider resolution thread-safe. JavaScript and Python can be
single-runtime deterministic in v1 because the provider table is immutable.

## Interface Conformance

The compiler should validate that a host implementation conforms to the
Capybara interface contract when enough information is available. Generated
startup validation remains necessary for JavaScript/Python and for packaged
host artifacts that are not statically inspectable by the compiler.

Required checks:

- All required interface methods are present.
- Method arity matches.
- Parameter and return types are representable at the host boundary.
- Nullable returns follow an explicit policy, such as reject, map to
  `Option[T]`, or map to `Result`.
- Host exceptions follow the declared exception policy.
- Method naming aliases are explicit. Do not guess `snake_case` versus
  `camelCase` without generated metadata.

Java can validate strongly against the generated Java interface when the host
class implements it. JavaScript and Python need generated adapter wrappers or
generated startup structural validation.

Wired implementations should be opaque as host values. Capybara sees the
interface, not the concrete host type.

## `.coo` Usage

`.coo` should consume wired implementations the same way it consumes any
interface implementation.

Preferred:

- Constructor injection from an application composition root.
- Factory/provider class that returns the interface type.
- Test-specific wiring scoped to the test runtime.

Avoid:

- Direct host class names in domain modules.
- Direct `require`, Python `import`, or Java package names in `.coo`.
- Reflection-based invocation.
- A global service locator used everywhere without domain-level dependencies.

If a Capybara provider class is introduced, it should be cohesive and typed:

```coo
native provider system_clock: Clock key "system"

class NativeClockProvider {
    def required(qualifier: String): Clock =
        system_clock()
}
```

The final implementation may prefer compiler-generated provider methods per
interface instead of a generic runtime method, because current `.coo` syntax has
limited support for expressing type-indexed generic lookup ergonomically.

## Dependency Injection Flow

Native dependency injection should be a compile/generate-time composition
mechanism, not runtime mutation. The compiler knows which Capybara interface is
requested, which qualifier is selected, and which host implementation satisfies
that dependency for the selected backend.

### 1. Capybara Declares The Dependency Contract

The interface is ordinary `.coo` source. It is not marked native:

```coo
interface Clock {
    def now_millis(): long
}

class ReportService(clock: Clock) {
    field clock: Clock = clock

    def started_at(): long =
        this.clock.now_millis()
}
```

Capybara owns the `Clock` interface contract. `ReportService` is pure OO domain
code that depends on the interface type. It does not know whether `Clock` is
implemented by Capybara, Java, JavaScript, or Python.

### 2. A Provider Symbol Names The Injection Point

If Capybara source needs to request the dependency directly, declare a typed
provider symbol:

```coo
native provider system_clock: Clock key "system"
```

This illustrative declaration says:

- `system_clock` is a Capybara symbol.
- `Clock` is the required interface type.
- `system` is the qualifier used to select the wired implementation.
- The compiler must find exactly one provider for `("/app/Clock", "system")`
  for the selected backend.

This is different from a runtime service locator. The source-level symbol is
typed, importable, and checked by the compiler. The raw interface id and host
class name stay out of domain code. Calling `system_clock()` returns the
Capybara interface type `Clock`.

### 3. Build Metadata Wires The Host Implementation

The compiler or Gradle task receives backend-specific wiring metadata using the
structured provider block described in Compile-Time Wiring Semantics:

```text
provider "/app/Clock" {
    qualifier "system"
    lifetime factory

    java {
        class "app.SystemClock"
        constructor "()"
    }

    javascript {
        module "./clock.js"
        export "SystemClock"
        factory "new"
    }

    python {
        module "app.clock"
        class "SystemClock"
        factory "call"
    }
}
```

The compiler uses this metadata to generate backend-specific provider tables.
It should reject duplicate provider entries and missing selected-backend
entries during compile/generate.

### 4. Generated Code Creates An Immutable Provider Table

The generated backend output contains a fixed table equivalent to:

```text
("/app/Clock", "system") -> provider factory for selected backend
```

Java can lower this to class references and factories:

```java
NativeProviders.define("/app/Clock", "system", app.SystemClock::new);
```

For Java, generation should:

- Prefer the generated `.coo` Java interface as the host implementation target.
- Emit a provider-table entry using class references or factories.
- Validate with `instanceof` or `Class.cast` where possible.
- Support explicit `singleton` and `factory` lifetime.
- Wrap checked host exceptions into the OO runtime exception model unless an
  adapter maps them to a declared result.

JavaScript can lower this to deterministic CommonJS imports:

```javascript
const clockModule = require("./clock.js");
capy.defineNativeProviders({
    "/app/Clock#system": () => new clockModule.SystemClock()
});
```

For JavaScript, generation should:

- Keep generated output CommonJS-compatible.
- Emit deterministic static `require(...)` calls from wiring metadata.
- Add immutable provider-table helpers such as
  `defineNativeProviders(providerTable)` and
  `resolveNativeImplementation(interfaceId, qualifier)`.
- Validate method presence and arity structurally.
- Preserve receiver binding when invoking object methods.
- Avoid auto-awaiting promises in v1.

Python can lower this to deterministic imports:

```python
from app.clock import SystemClock

capy.define_native_providers({
    "/app/Clock#system": lambda: SystemClock()
})
```

For Python, generation should:

- Emit deterministic imports from wiring metadata.
- Add immutable provider-table helpers such as
  `define_native_providers(provider_table)` and
  `resolve_native_implementation(interface_id, qualifier)`.
- Validate method presence and arity structurally.
- Handle Python keyword conflicts and `None` with explicit adapter policy.
- Avoid automatically mapping iterable/context-manager protocols to Capybara
  collections or lifecycles.

These snippets are illustrative. The important rule is that the table is
generated and immutable; host code does not call `register(...)` after startup.

### 5. Composition Root Injects The Interface

An application-facing Capybara class wires the dependency into domain objects:

```coo
native provider system_clock: Clock key "system"

class App {
    def report_service(): ReportService =
        ReportService(system_clock())
}
```

After generation, `system_clock()` lowers to a provider-table lookup. The result
is typed as `Clock`, so the rest of the program uses normal interface dispatch.

Preferred style:

- Call provider symbols in composition roots or provider classes.
- Pass interfaces into domain classes through constructors or factories.
- Avoid calling provider symbols throughout domain logic.

### 6. Lifetime Controls Object Creation

Wiring metadata must define lifetime:

- `singleton`: provider table creates one host object and returns the same
  instance for every lookup.
- `factory`: provider table creates a fresh host object for every lookup.
- `scoped`: future option for request/test/application scopes.

The lifetime determines what `system_clock()` means. It should not be inferred
from the host class.

### 7. Failure Timing

Failures should happen as early as possible:

- compile/generate time: missing provider wiring for selected backend, duplicate
  provider key, target is not an interface, unsupported provider type;
- generated startup validation: JS/Python module missing, exported class
  missing, structural method mismatch that the compiler could not inspect;
- invocation time: host method throws or returns an invalid nullable value.

This keeps normal Capybara call sites simple while still giving actionable
diagnostics tied to the provider key.

### 8. Testing Overrides

Tests should be able to provide alternate wiring metadata:

```text
provider "/app/Clock" {
    qualifier "system"
    lifetime factory

    javascript {
        module "./fake_clock.js"
        export "FakeClock"
        factory "new"
    }
}
```

The override is selected at compile/generate time for the test task. It should
not mutate a global runtime registry shared with other tests.

## Capybara Runtime Surface

The Capybara-facing class should be small and typed. A conceptual standard
library surface could look like this:

```coo
class NativeClockProvider {
    def required(qualifier: String): Clock =
        Native.requiredClock(qualifier)

    def optional(qualifier: String): Result[Clock] =
        Native.optionalClock(qualifier)
}
```

The runtime-owned `Native` class should not expose arbitrary stringly typed host
lookups to domain code. Prefer compiler-generated methods per interface or
module-local provider classes that hide the raw interface id. Domain services
should depend on `Clock`, not on `Native`.

`native provider` declarations should be constrained:

- target type must be an imported or in-module `.coo interface`;
- target type cannot be a class, trait, primitive, array, `void`, `any`, or
  `.cfun data` type in v1;
- provider name and `(interface, qualifier)` must be unique in the module/program
  scope chosen by the ADR;
- provider lookup in `.coo` returns the interface type;
- provider lookup in `.cfun` returns `Effect[Result[Interface]]` or an
  equivalent typed failure shape, not a bare interface.

## `.cfun` Boundary

Provider lookup is effectful from `.cfun` even when the wiring was selected at
compile time.

- Looking up a wired implementation returns `Effect[Result[Clock]]` or an
  equivalent typed failure shape. It must not return a bare `Clock`.
- Invoking host-backed methods from `.cfun` goes through generated method
  bridges and returns `Effect[T]` by default.
- Pure `.cfun` wrappers over host implementations should be disallowed in v1
  unless the runtime can enforce a strict verified-pure contract.
- Host-thrown exceptions during invocation are runtime failures of the effect
  unless an explicit adapter maps expected failures to `Result`.

Effects must be lazy:

- Creating the effect does not perform provider lookup or host invocation.
- Running the effect performs provider lookup or host invocation.
- Re-running the effect repeats lookup/invocation according to the provider
  lifetime policy.

## Failure Policy

Use focused failure categories. Each failure must report the provider key
`(interface, qualifier)` and, when relevant, the selected backend:

- `NotWired`: no provider was wired for the key and selected backend.
- `DuplicateProvider`: more than one provider was wired for the same key where
  v1 expects exactly one.
- `TypeMismatch`: the provider for the key does not conform to the Capybara
  interface.
- `UnsupportedBackend`: wiring or interface shape is not supported by the
  selected backend for the key.
- `InvocationFailure`: host construction, factory call, or method invocation
  for the key failed after generation.

Failure timing is part of the contract:

- Compile/generate time: missing provider for the selected backend, duplicate
  provider key, unknown target interface, or unsupported provider type.
- Generated startup validation: missing JavaScript/Python module, missing
  export/class, or structural method mismatch that was not statically
  inspectable by the compiler.
- Invocation time: host method throws, or a host method returns a nullable value
  that violates the declared adapter/nullability policy.

For `.coo`, these failures can use the existing OO exception model unless the
provider API returns an explicit result type. For `.cfun`, lookup should prefer
`Effect[Result[T]]` so expected missing-provider failures can be handled as
values.

## Backend Notes

Java:

- Generated `.coo` interfaces should be the preferred host implementation
  target.
- Wiring metadata can name a host class/factory plus a stable interface id such
  as `/app/Clock`.
- Generated provider tables should validate `Class.cast` or `instanceof` the
  generated interface where possible.
- Factories should define `singleton` versus `factory` lifetime explicitly.
- Checked exceptions from host implementations should be wrapped into the OO
  runtime exception model unless an adapter maps them to a declared result.

JavaScript:

- Keep generated output CommonJS-compatible.
- The compiler/generator should emit deterministic static `require(...)` calls
  for wired provider modules.
- Add immutable provider-table helpers to `capybara.js`, for example:
  - `defineNativeProviders(providerTable)`
  - `resolveNativeImplementation(interfaceId, qualifier)`
- Provider modules are compiler inputs or generated dependencies, not direct
  `require(...)` calls in `.coo`.
- Validate method presence and arity structurally.
- Preserve receiver binding when invoking wired object methods.
- Promise-returning APIs should not auto-await in v1; require an explicit
  effect/async decision later.

Python:

- Add equivalent immutable provider-table helpers:
  - `define_native_providers(provider_table)`
  - `resolve_native_implementation(interface_id, qualifier)`
- Validate method presence and arity structurally.
- Handle Python keyword conflicts and `None` with explicit adapter policy.
- Iterable and context-manager protocols should not automatically become
  Capybara collections or resource lifecycles.

## Edge Cases To Specify

These are design questions to settle before expanding past the first slice.
Unless a case is named in Suggested First Slice, it is not a first-slice
implementation requirement.

- Singleton versus factory lifetime: define whether repeated lookups return a
  cached host object or construct a fresh implementation.
- Provider teardown/disposal: future work for files, sockets, processes,
  database handles, or other host resources.
- Multiple implementations of the same interface: require explicit qualifiers
  and reject duplicate `(interface, qualifier)` keys.
- Test-scoped overrides: future work for selecting test wiring without mutating
  a global runtime registry or leaking across tests.
- Host state mutation: document how lifetime and repeated `.cfun` effect runs
  expose mutations in host objects.
- Nullable host returns: define whether each boundary rejects null/`None`, maps
  it to `Option`, or maps it to `Result`.
- Host async APIs: future work; v1 should not auto-await JavaScript promises or
  implicitly run Python async functions.
- Interface versioning: define diagnostics when a Capybara interface changes
  after host code was compiled or packaged.
- Snake_case/camelCase mapping: require generated metadata for method aliases
  instead of backend guessing.
- Overloaded Java methods: define how a single Capybara method maps to a Java
  overload, or reject ambiguous overloads.
- Backend-specific provider availability: define deterministic failure when a
  provider is available for Java but not for the selected JavaScript or Python
  backend.

## Implementation Touchpoints

- Add a native wiring ADR defining interface ids, qualifiers, provider lifetime,
  validation phases, generated table shape, and failure categories.
- Add a `native provider` grammar form only if Capybara source needs typed
  provider symbols. Constructor injection can defer this syntax, but direct
  lookup should not be stringly typed.
- Extend linked metadata for `.coo` interfaces so runtimes can validate
  host-provider conformance.
- Add linked metadata for provider declarations: provider name, interface type,
  key/qualifier, visibility, source position, and required backend availability.
- Extend or replace `CompiledObjectType` with an OO catalog that records object
  kind, method signatures, parent interfaces, constructibility, and provider
  entries. This needs linked serialization so `capyc generate` behaves the same
  from linked artifacts.
- Add generated provider-table/runtime helpers for Java, JavaScript, and Python.
- Add generator support for required interface metadata and any generated typed
  provider methods.
- Keep direct host imports out of `ObjectOriented.g4`; wiring belongs in
  compiler/build metadata, not normal `.coo` imports.
- Add a provider manifest or build-plugin input so projects can package wired
  host modules/classes with generated output.
- Keep reflection metadata descriptive only. Reflection should not be used to
  bypass the registry.

## Test Plan

- Parser/compiler tests for every accepted `native provider` syntax shape,
  including `native provider name: Interface key "..."` if that form is
  accepted, and tests proving direct host import syntax was not added to `.coo`.
- Compile-error tests for invalid provider targets: class, primitive, unknown
  interface target, unsupported provider type, duplicate provider name, and
  duplicate `(interface, qualifier)` key.
- Compile-error tests for `.cfun` code expecting a bare interface where provider
  lookup returns `Effect[Result[Interface]]` or an equivalent typed failure
  shape.
- Java provider-table tests for successful lookup, missing provider
  (`NotWired`), duplicate provider (`DuplicateProvider`), and wrong
  implementation (`TypeMismatch` from failed `Class.cast`/`instanceof` or
  wrong method shape).
- JavaScript CommonJS provider tests for successful module/export wiring,
  missing export, wrong arity, and receiver binding when generated bridges call
  object methods.
- Python provider tests for successful module/class wiring, missing class, wrong
  arity, and `None` handling for non-nullable return types.
- `.coo` e2e test where a domain class receives an interface by constructor
  injection and never names the Java, JavaScript, or Python host implementation.
- `.cfun` tests for lazy `Effect` lookup, typed failure handling with `Result`
  or equivalent, generated bridge invocation returning `Effect[T]`, and
  invocation-time failures.
- CLI/package tests ensuring generated output can include or locate required
  provider wiring files without changing normal CommonJS/Python/Java
  generation behavior.

## Suggested First Slice

The minimum implementable feature should be deliberately small:

- one `.coo` interface;
- one method;
- one required provider key;
- one backend manifest entry each for Java, JavaScript, and Python;
- one explicit lifetime for the slice, such as `factory`;
- deterministic failure for missing, duplicate, and wrong-shape providers.

Do not include singleton support, scoped providers, disposal, async host APIs,
or richer type conversion in this slice.

Capybara:

```coo
interface Clock {
    def now_millis(): long
}

class ClockUser(clock: Clock) {
    field clock: Clock = clock

    def read(): long =
        this.clock.now_millis()
}
```

Compiler wiring input:

```text
provider "/app/Clock" {
    qualifier "system"
    lifetime factory

    java {
        class "app.SystemClock"
        constructor "()"
    }

    javascript {
        module "./clock.js"
        export "SystemClock"
        factory "new"
    }

    python {
        module "app.clock"
        class "SystemClock"
        factory "call"
    }
}
```

V1 should prove:

- host implementation can satisfy one Capybara interface with one method;
- Capybara code invokes it through the interface;
- missing, duplicate, and wrong-shape providers fail deterministically during
  compile/generate or generated startup validation;
- Java, JavaScript, and Python expose equivalent behavior;
- `.cfun` lookup remains effectful and lazy.

Future work after that slice includes singleton lifetime, additional qualifiers,
test-scoped overrides, resource disposal, host async APIs, richer type
conversion, and richer manifest features.
