# ADR-2026-05-28: Self-Hosted Compiler and CLI v1

- Status: accepted
- Deciders: Codex, repository maintainers
- Date: 2026-05-28
- Issue: #184

## Context

Capybara currently implements most compiler semantics, backend generation, and
the CLI workflow in Java. The target architecture is self-hosting: those layers
move to Capybara source and are compiled to Java before they are used in normal
Gradle, package, test, and command-line workflows.

Existing decisions constrain the migration:

- `ADR-2026-04-14: Capybara OO v1 Frontend Boundary` keeps `.cfun` and `.coo`
  frontends separate and keeps source handling extension-aware.
- `ADR-2026-05-22: Native Provider Wiring v1` requires host access to cross
  `NativeProvider` contracts and remain effectful from `.cfun`.
- `ADR-2026-05-24: Declaration Annotations v1` keeps annotations as metadata
  except for explicit standard compiler contracts.
- `ADR-2026-05-08: Reflection v1` keeps reflection descriptive and disallows
  arbitrary dispatch or invocation.
- `ADR-2026-05-07: .cfun Compile-Time Derive v1` keeps derive deterministic and
  avoids arbitrary compile-time execution.
- `ADR-2026-04-29: .cfun @Recursive Tail-Recursion Contract` makes recursion
  behavior an explicit compiler-verified contract.
- `2026-04-12: Unsafe Constructor Bypass` scopes unsafe construction to `.cfun`
  data construction only.

This ADR records guardrails and baseline gates only. It does not change compiler
or CLI behavior.

## Decision

Capybara will move compiler semantics, generators, and CLI workflow into
Capybara source. The Java output generated from that Capybara source becomes the
production implementation used by Gradle tasks, package manifests, tests, and
user-facing commands.

CLI launching semantics are Capybara-owned. The source-level entrypoint contract
is:

```cfun
fun main(args: List[String]): Effect[Program]
```

Generated Java may expose a JVM `main(String...)` wrapper, but that wrapper is
launch glue. It must run the Capybara `Effect[Program]` entrypoint and apply the
`Program` exit-code contract.

The existing hand-written Java compiler and CLI entrypoints are transitional.
After cutover they must not remain production APIs for compiler semantics,
generation, or CLI workflow. Java that remains after cutover is limited to the
permanent Java islands defined below.

## Permanent Java Islands

The self-hosted architecture keeps these Java-owned islands:

- ANTLR grammar sources and generated ANTLR parser runtime for `.cfun` and
  `.coo`.
- A stable Java parser DTO/schema facade that translates ANTLR parse output into
  data consumed by the Capybara compiler.
- Host implementations for `NativeProvider` contracts.
- Required runtime helpers needed by generated Java, including JVM IO, process,
  exception, class-loading, and bootstrap helpers that cannot run before the
  generated Capybara code is loaded.
- Minimal JVM glue needed to locate generated classes, load them, and invoke the
  generated Capybara `main(args)` entrypoint.

ANTLR grammars and generated ANTLR parser runtime must not be migrated to
Capybara. The parser facade may evolve, but it is a compatibility boundary and
must remain small, stable, and data-shaped.

## Preserved Constraints

The migration must preserve these constraints:

- Do not add imperative constructs to `compiler/src/main/antlr/Functional.g4`
  to make migration easier.
- Do not introduce pure `.cfun` host calls.
- Host access must use `NativeProvider` contracts or existing runtime helpers
  until those helpers are replaced by an explicit later decision.
- Reflection remains descriptive metadata, not dispatch or invocation.
- Annotations remain metadata except existing standard compiler contracts such
  as `@Recursive` and `@NativeProvider`.
- Unsafe constructor bypass remains scoped to `.cfun data` construction.
- `.coo` stays a separate frontend boundary; `.cfun` is not expanded to absorb
  OO or host-language workflow syntax.

Any future migration step that weakens one of these constraints requires a new
ADR before implementation.

## Linked Artifact Compatibility

Linked artifacts are part of the migration contract. During transition:

- Old linked modules produced by the Java compiler can be read by the
  self-hosted compiler or by explicit transition adapters.
- New linked modules produced by the self-hosted compiler can be read by
  transition shims used by the remaining Java workflow.
- `program.json` remains the aggregate linked-program boundary.
- Any incompatible schema version must fail with a clear diagnostic that names
  the artifact path, schema version, reader version, and required migration or
  rebuild action.
- Compatibility shims may normalize field ordering and defaulted metadata, but
  must not silently reinterpret incompatible semantics.

The current linked format does not yet expose a formal external schema version.
The first self-hosting change that creates an incompatible linked artifact must
introduce versioned read/write tests before changing the format.

## Bootstrap Stages

The self-hosting bootstrap is staged:

- Stage 0: the current Java compiler builds the Capybara compiler sources.
- Stage 1: the generated compiler rebuilds the same Capybara compiler sources.
- Stage 2: the Stage 1 compiler rebuilds the same sources again.
- Stage 1 and Stage 2 outputs are compared.

The comparison must cover normalized linked artifacts, generated Java sources,
compiler diagnostics on the baseline invalid-program suite, native provider
catalog/bootstrap output, and CLI-observable behavior. Differences must be
explained by an explicit compatibility rule or treated as migration failures.

## Deletion Criteria

The old Java compiler and CLI implementations may be deleted only after all of
these gates pass:

- Stage 1 and Stage 2 compiler outputs compare equal under the approved
  normalization rules.
- Compiler unit tests, compiler integration tests, and compile-error tests pass.
- Functional and OO e2e tests pass for Java, JavaScript CommonJS, and Python.
- Native provider declaration, catalog, backend bootstrap, and e2e host-provider
  tests pass for supported backends.
- CLI contract tests pass for command parsing, stderr behavior, exit codes,
  linked output, generated output, package output, and test execution.
- Gradle tasks and package manifests invoke Java generated from Capybara source
  for production compiler and CLI behavior.

After deletion, remaining Java is limited to the permanent Java islands. New
hand-written Java in compiler semantics, generators, or CLI workflow requires a
new ADR.

## Baseline Gate Inventory

The current baseline is protected by existing coverage:

| Baseline area | Existing coverage |
| --- | --- |
| `.cfun` parser diagnostics and invalid syntax | `compiler/src/test/java/dev/capylang/parser/CapybaraParserTest.java` and parser-syntax cases in `compiler/src/test/java/dev/capylang/compiler/compilation_error/CompilationErrorTest.java`. |
| `.coo` parser diagnostics and invalid syntax | `compiler/src/test/java/dev/capylang/parser/ObjectOrientedParserTest.java` and `compiler/src/test/java/dev/capylang/compiler/compilation_error/ObjectOrientedCompilationErrorTest.java`. |
| Compile diagnostics and compile-error behavior | `compiler/src/test/java/dev/capylang/compiler/compilation_error/*`, plus focused compiler tests such as `AnnotationCompilerTest`, `MainDetectionCompilerTest`, `ObjectOrientedCompilerTest`, `PrimitiveBackedTypeCompilerTest`, and `RecursionCompilerTest`. |
| Linked `program.json` shape and round trip | `capy/src/test/java/dev/capylang/CapyTest.java` checks linked output and aggregate `program.json`; `ObjectOrientedCompilerTest.shouldRoundTripCompiledProgramWithNativeProviderCatalogThroughJackson` checks native-provider catalog round trip. |
| Generated Java output | `ObjectOrientedJavaGeneratorTest`, `ProgramMainSourceGeneratorTest`, `PrimitiveBackedTypeGeneratorTest`, and Java portions of `capy:e2eTests`. |
| Generated JavaScript CommonJS output | `JavaScriptGeneratorTest` and JavaScript portions of `capy:e2eTests`. |
| Generated Python output | `PythonGeneratorTest` and Python portions of `capy:e2eTests`. |
| Native provider catalog and bootstrap output | `NativeImplementationScannerTest`, `ObjectOrientedCompilerTest`, `ObjectOrientedJavaGeneratorTest`, `JavaScriptGeneratorTest`, `PythonGeneratorTest`, `CapyTest.shouldCompileGenerateWithNativeProviderAnnotationWiring`, and native-provider e2e source sets under `capy/src/e2e-coo-*-native`. |
| CLI exit codes and stderr behavior | `CapyTest` covers CLI usage/version/native-wiring/output failures; `JavaScriptGeneratorTest` and `PythonGeneratorTest` cover generated `Effect[Program]` process exit behavior; `ProgramMainSourceGeneratorTest` covers generated Java launcher source. |

No new tests are added by this ADR because these baseline areas are already
covered. Future migration PRs must add focused golden, parity, or compatibility
tests when they change any protected behavior.

## Baseline Timings

Measured on 2026-05-28 in the `feature/#184-self-hosted-compiler` branch.
Source baseline commit before this ADR: `bc44342e963f18e02dfce3461037920ea5685d36`.

| Command | Status | Elapsed time |
| --- | --- | --- |
| `./gradlew :compiler:test :compiler:integrationTest` | passed | 3:14.52 |
| `./gradlew :capy:e2eTests` | passed | 7:06.70 |
| `./gradlew clean test` | passed | 3:57.46 |

## Consequences

Self-hosting is a source ownership change, not permission to broaden the
language or weaken runtime boundaries. The migration can replace Java
implementations incrementally, but every step must keep the Java-owned islands
explicit and keep user-observable behavior under the baseline gates above.
