# TASK-5: Specify backend dependency-injection generation

Goal: document how Java, JavaScript, and Python generated output receives native providers without requiring Capybara source to import host modules.

File to edit:

- `.ai/native.md`

Exact work:

1. In `## Dependency Injection Flow`, add or verify a subsection named `### 4. Generated Code Creates An Immutable Provider Table`.
2. State that generated output contains a fixed mapping equivalent to:

   ```text
   ("/app/Clock", "system") -> provider factory for selected backend
   ```

3. For Java, document that generation should:
   - prefer the generated `.coo` Java interface as the host implementation target;
   - emit a provider-table entry using class references or factories;
   - validate with `instanceof` or `Class.cast` where possible;
   - support explicit `singleton` and `factory` lifetime;
   - wrap checked host exceptions into the OO runtime exception model unless an adapter maps them to a declared result.
4. Include an illustrative Java lowering:

   ```java
   NativeProviders.define("/app/Clock", "system", app.SystemClock::new);
   ```

5. For JavaScript, document that generation should:
   - keep CommonJS output;
   - emit deterministic static `require(...)` calls from wiring metadata;
   - add immutable provider-table helpers such as `defineNativeProviders(providerTable)` and `resolveNativeImplementation(interfaceId, qualifier)`;
   - validate method presence and arity structurally;
   - preserve receiver binding when invoking object methods;
   - avoid auto-awaiting promises in v1.
6. Include an illustrative JavaScript lowering:

   ```javascript
   const clockModule = require("./clock.js");
   capy.defineNativeProviders({
       "/app/Clock#system": () => new clockModule.SystemClock()
   });
   ```

7. For Python, document that generation should:
   - emit deterministic imports from wiring metadata;
   - add immutable provider-table helpers such as `define_native_providers(provider_table)` and `resolve_native_implementation(interface_id, qualifier)`;
   - validate method presence and arity structurally;
   - handle Python keyword conflicts and `None` with explicit adapter policy;
   - avoid automatically mapping iterable/context-manager protocols to Capybara collections or lifecycles.
8. Include an illustrative Python lowering:

   ```python
   from app.clock import SystemClock

   capy.define_native_providers({
       "/app/Clock#system": lambda: SystemClock()
   })
   ```

9. Add or verify lifetime semantics:
   - `singleton` creates one host object and returns it for every lookup;
   - `factory` creates a fresh host object for every lookup;
   - `scoped` is future work for request/test/application scopes.

Do not:

- Let host code imperatively call `register(...)` after startup as the normal v1 path.
- Put backend import statements into `.coo`.
- Use backend-specific method names in Capybara source.

Acceptance checks:

- A Java backend developer can identify where to emit provider table entries.
- A JavaScript backend developer can keep the output CommonJS-compatible.
- A Python backend developer can identify import, adapter, and `None` handling responsibilities.
