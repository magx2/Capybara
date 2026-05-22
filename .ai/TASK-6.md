# TASK-6: Specify runtime boundary rules, edge cases, and tests

Goal: document the runtime semantics and test coverage needed before native provider wiring is implemented.

File to edit:

- `.ai/native.md`

Exact work:

1. In `## .cfun Boundary`, state that provider lookup from `.cfun` is effectful even when wiring was selected at compile time.
2. Specify that `.cfun` lookup should return `Effect[Result[Clock]]` or an equivalent typed failure shape, not a bare interface.
3. Specify that invoking host-backed methods from `.cfun` should go through generated bridges and return `Effect[T]` by default.
4. State that effects must be lazy:
   - creating the effect does not perform lookup or invocation;
   - running the effect performs lookup/invocation;
   - re-running the effect repeats lookup/invocation according to provider lifetime.
5. In `## Failure Policy`, define where each failure happens:
   - compile/generate time: missing provider, duplicate provider, unknown target interface, unsupported provider type;
   - generated startup validation: missing JS/Python module, missing export/class, structural method mismatch;
   - invocation time: host method throws or returns an invalid nullable value.
6. In `## Edge Cases To Specify`, include at least:
   - singleton versus factory lifetime;
   - provider teardown/disposal;
   - multiple implementations of the same interface;
   - test-scoped overrides;
   - host state mutation;
   - nullable host returns;
   - host async APIs;
   - interface versioning;
   - snake_case/camelCase mapping;
   - overloaded Java methods;
   - backend-specific provider availability.
7. In `## Test Plan`, add concrete test groups:
   - parser/compiler tests for any accepted `native provider` syntax;
   - compile-error tests for invalid provider targets and duplicate provider keys;
   - Java provider-table tests for success, missing provider, duplicate provider, and wrong implementation;
   - JavaScript CommonJS provider tests for success, missing export, wrong arity, and receiver binding;
   - Python provider tests for success, missing class, wrong arity, and `None` handling;
   - `.coo` e2e test where a domain class receives an interface by constructor injection and never names the host implementation;
   - `.cfun` tests for lazy `Effect` lookup and typed failure handling.
8. In `## Suggested First Slice`, define the minimum implementable feature:
   - one `.coo` interface;
   - one method;
   - one required provider;
   - one backend manifest entry per Java, JavaScript, and Python;
   - deterministic failure for missing, duplicate, and wrong-shape providers.

Do not:

- Leave test requirements as generic "add tests".
- Treat async host APIs, disposal, or scoped providers as v1 implementation requirements unless the section explicitly marks them as future work.
- Describe `.cfun` host invocation as pure.

Acceptance checks:

- A compiler developer can translate the failure timing into diagnostics and compile-error tests.
- Backend developers can derive focused runtime tests from the listed cases.
- The first implementation slice is small enough to build without solving every edge case.
