# TASK-4: Specify compile-time provider wiring and validation

Goal: document the exact metadata and compiler/generator responsibilities for wiring host implementations to Capybara interfaces.

File to edit:

- `.ai/native.md`

Exact work:

1. In `## Compile-Time Wiring Semantics`, define the provider key as:
   - fully qualified Capybara interface id, for example `/app/Clock`;
   - optional qualifier, for example `system`;
   - optional future member/capability id only if a later design needs method-level wiring.
2. Define valid provider values:
   - singleton instance;
   - factory that returns a fresh implementation;
   - host class reference plus constructor/factory metadata.
3. State that v1 should allow exactly one provider for each `(interface, qualifier)` key.
4. Add or verify a structured metadata example shaped like:

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

5. Document the compilation lifecycle in order:
   - parse `.coo` interfaces and provider declarations;
   - read provider metadata for the selected backend;
   - validate provider uniqueness;
   - validate target interface existence;
   - validate host shape where statically possible;
   - emit immutable provider table or typed provider methods;
   - run generated startup validation for backend facts that cannot be checked at compile time.
6. Add compile/generate failure categories:
   - `NotWired`;
   - `DuplicateProvider`;
   - `TypeMismatch`;
   - `UnsupportedBackend`;
   - `InvocationFailure`.
7. In `## Interface Conformance`, require validation for:
   - all interface methods present;
   - method arity matches;
   - parameter and return types are representable at the host boundary;
   - nullable returns follow an explicit policy;
   - host exceptions follow the declared policy;
   - method name aliases are explicit.
8. State that Java can validate strongly against generated interfaces, while JavaScript and Python need generated adapter or startup structural validation.

Do not:

- Describe mutable runtime registration as the v1 mechanism.
- Use reflection as the primary validation or invocation strategy.
- Allow duplicate providers silently.

Acceptance checks:

- A developer can implement a provider manifest parser from the documented shape.
- A developer can implement deterministic validation phases from the lifecycle.
- Error categories are named and tied to provider keys.
