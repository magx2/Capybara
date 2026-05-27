# ADR-2026-05-22: Native Provider Wiring v1

- Status: accepted
- Deciders: Codex, repository maintainers
- Date: 2026-05-22

## Status

Accepted. The v1 slice is implemented for compile-time provider annotations,
backend native implementation annotations, and Java, JavaScript CommonJS, and
Python provider bootstrap generation.

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

## Decision

Capybara adopts compile-time native provider wiring for the first native
interop implementation slice.

Capybara OO owns the interface contract. A native provider targets the return
interface of the annotated `.cfun` provider function plus qualifier. The stable
interface id is derived from that return type and is not repeated in sidecar
configuration.

Provider functions use `/capy/meta_prog/NativeProvider` in `.cfun`:

```cfun
from /capy/lang/Effect import { Effect }
from /capy/meta_prog/NativeProvider import { NativeProvider }
from /dev/capylang/test/Clock import { Clock }

@NativeProvider(qualifier: "system")
fun system_clock(): Effect[Clock] = <native>
```

Host implementations are wired through backend source annotations. The native
class implements or extends the generated Capybara interface and carries a
`NativeImplementation` annotation with the same qualifier:

```java
import dev.capylang.NativeImplementation;
import dev.capylang.test.Clock;

@NativeImplementation(qualifier = "system")
public final class SystemClock implements Clock {
}
```

```javascript
const { Clock } = require('../Clock.js');

@NativeImplementation("system")
class SystemClock extends Clock {
}
```

```python
from dev.capylang.capybara import NativeImplementation
from dev.capylang.test.Clock import Clock

@NativeImplementation(qualifier="system")
class SystemClock(Clock):
    pass
```

The v1 provider key is `(interfaceId, qualifier)`. The first implementation
slice supports exactly one provider for each key and backend in the selected
compile/generate input set. Duplicate providers for the same key and backend
fail deterministically.

Provider lookups always create a fresh host implementation object. There is no
provider caching mode in this slice.

Backend factories default to Java constructors, JavaScript `new`, and Python
class calls. The backend location is derived from the annotated implementation
class source:

- Java: the implementation package plus class name.
- JavaScript CommonJS: the implementation module path plus exported class name.
- Python: the implementation module path plus class name.

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

- parse and link typed `@NativeProvider` annotations for `.cfun` provider
  functions;
- require provider functions to return `Effect[X]` where `X` is the interface
  type;
- scan Java, JavaScript, and Python native source for `NativeImplementation`
  class annotations;
- derive provider backend names from annotated backend implementation classes;
- validate provider key uniqueness and target interface existence;
- generate immutable provider tables or typed provider methods;
- support Java, JavaScript CommonJS, and Python backend generation.

Non-goals for this slice are:

- direct Java, JavaScript, npm, CommonJS, or Python imports in `.coo`;
- mutable runtime provider registration or test overrides;
- reflection-based invocation, dispatch, or validation;
- reusing unsafe `.cfun data` constructor bypass behavior;
- multiple providers for the same `(interfaceId, qualifier, backend)` key;
- provider disposal, request scopes, and async host APIs;
- pure `.cfun` host calls or automatic conversion of host effects into pure
  functional values.

## Implemented v1

Supported `.cfun` syntax:

```cfun
from /capy/lang/Effect import { Effect }
from /capy/meta_prog/NativeProvider import { NativeProvider }
from /dev/capylang/test/Clock import { Clock }

@NativeProvider(qualifier: "system")
fun system_clock(): Effect[Clock] = <native>
```

Supported backend annotations:

```java
@NativeImplementation(qualifier = "system")
public final class SystemClock implements Clock {
}
```

```javascript
@NativeImplementation("system")
class SystemClock extends Clock {
}
```

```python
@NativeImplementation(qualifier="system")
class SystemClock(Clock):
    pass
```

The provider symbol is the annotated function name, so this example is callable
as `system_clock()` and returns an `Effect[Clock]`. Running the effect creates a
fresh host `SystemClock` instance. The compiler derives
`/dev/capylang/test/Clock` from the provider return type and derives backend
bindings from annotated backend classes.

Native implementation source is discovered from sibling `java`, `js`, `py`,
`native/java`, `native/js`, and `native/py` directories near the Capybara input
directory. External native wiring manifests remain accepted as compatibility
input, but e2e native provider wiring is represented in native source
annotations rather than JSON sidecar files.

Implemented diagnostics use these stable terms: `NotWired`,
`DuplicateProvider`, `TypeMismatch`, `UnsupportedBackend`, and
`InvocationFailure`. Provider-related diagnostics include the provider symbol
when known, interface id, qualifier, backend for backend-specific failures, and
source or manifest file path when the reporting layer has it.

Recorded v1 non-goals are: no mutable runtime registration, no direct `.coo`
host imports, no automatic async handling, no request scopes, no resource
disposal hooks, and no pure `.cfun` native lookup.

## Consequences

Domain code depends on Capybara interfaces and provider symbols. Provider
contract files carry only Capybara-level selectors in annotations, while host
implementation files carry backend implementation annotations.

Compiler and generator diagnostics can be deterministic for missing providers,
duplicate provider keys, unknown target interfaces, unsupported backend
metadata, and host shape mismatches where those can be checked before runtime.

Java generation can wire providers with generated interface types and concrete
class names derived from native Java source annotations. The generated provider
table constructs a new implementation object for each lookup.

JavaScript CommonJS generation can lower native JavaScript annotations to
deterministic `require(...)` calls for the selected backend and export name.
JavaScript native source uses decorator syntax for `@NativeImplementation`; the
generated bootstrap removes that compile-time decorator before CommonJS loads
the module. The generated provider table remains immutable. Startup or lookup
validation must report missing modules, missing exports, wrong factory shape, or
incompatible objects against the provider key.

Python generation can lower native Python annotations to deterministic imports
for the selected module and class name. The generated provider table remains
immutable. Startup or lookup validation must report missing modules, missing
classes, wrong factory shape, or incompatible objects against the provider key.

Because `.cfun` access remains effectful, functional callers cannot accidentally
treat host-backed object lookup or invocation as a pure expression. Pure native
function facades, if needed later, require a separate decision.
