# TASK-3: Specify the Capybara-facing dependency contract

Goal: document exactly how Capybara source declares and consumes a dependency while host implementation details stay outside domain code.

File to edit:

- `.ai/native.md`

Exact work:

1. In `## Core Model`, add or verify an example with:
   - a `.coo interface Clock`;
   - a `.coo class ReportService(clock: Clock)`;
   - a method that calls `this.clock.now_millis()`.
2. State that the interface is normal Capybara OO source and is not marked native.
3. Add a subsection under `## Dependency Injection Flow` named `### 1. Capybara Declares The Dependency Contract` if it is missing.
4. In that subsection, explain:
   - Capybara owns the interface contract.
   - Domain classes depend on the interface type.
   - Domain classes do not know whether the implementation comes from Capybara, Java, JavaScript, or Python.
5. Add or verify a typed provider symbol example:

   ```coo
   native provider system_clock: Clock key "system"
   ```

6. Explain the provider symbol field-by-field:
   - `system_clock` is the Capybara symbol.
   - `Clock` is the required interface type.
   - `system` is the qualifier used to select the wired implementation.
   - the compiler must find exactly one provider for `(Clock, "system")` for the selected backend.
7. Add or verify a composition-root example:

   ```coo
   class App {
       def report_service(): ReportService =
           ReportService(system_clock())
   }
   ```

8. State the preferred style:
   - call provider symbols in composition roots or provider classes;
   - pass interfaces into domain classes through constructors or factories;
   - avoid calling provider symbols throughout domain logic.

Do not:

- Add direct host class names to `.coo` examples.
- Present a global stringly typed service locator as the primary API.
- Claim the `native provider` syntax is final; mark it as illustrative if the grammar has not been accepted.

Acceptance checks:

- A developer can implement the `.coo` surface without needing Java, JavaScript, or Python names in Capybara source.
- The document makes constructor injection the default domain pattern.
- The provider lookup result is always described as the Capybara interface type.
