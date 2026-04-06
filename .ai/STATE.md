## 2026-04-06 build optimization pass

### Requested baseline
- Attempted `./gradlew :lib:capybara-lib:check --profile --rerun-tasks --console=plain`.
- The run could not complete in this sandbox:
  - default Gradle home under `/home/martin/.gradle` is read-only for wrapper lock creation;
  - redirecting `GRADLE_USER_HOME` to `/tmp` then failed because network access is disabled and Gradle 9.1.0 could not be downloaded.
- Checked for existing profile reports under `build/reports/profile`; none were present.

### Findings
- `lib/capybara-lib` was recompiling generated main Capybara Java sources during `compileTestJava` by adding `build/generated/sources/capybara/java/main` to the `test` source set.
- This is redundant because the test source set already compiles against main outputs on its classpath, so `:lib:capybara-lib:check` was paying to compile the generated main sources twice.
- `linkCapybara` also deleted part of `compileCapybara` output (`generated/sources/capybara/java/main/dev/capylang`) in a `doLast` block, which mutates another task's output and adds avoidable filesystem work.

### Changes made
- Removed `capybaraGeneratedMainDir` from the `test` source set in `lib/capybara-lib/build.gradle` so test compilation only compiles generated test sources.
- Removed the `linkCapybara` `doLast` cleanup that deleted files inside `compileCapybara` output.

### Verification status
- Source inspection confirms the change is build-graph-safe.
- Runtime verification is still blocked in this environment:
  - `./gradlew` cannot download the wrapper distribution without network;
  - `gradle -v` fails locally with `Failed to load native library 'libnative-platform.so' for Linux amd64`.

## 2026-04-06 build optimization pass follow-up

### Requested baseline
- Re-ran the requested command as `GRADLE_USER_HOME=$PWD/.gradle ./gradlew :lib:capybara-lib:check --profile --rerun-tasks --console=plain` to reuse the repository-local Gradle 9.1.0 distribution.
- Wrapper startup got past the missing-distribution problem but still failed in this sandbox before project execution:
  - Gradle could not create `FileLockContentionHandler`;
  - the root cause reported was `Could not determine a usable wildcard IP for this machine`.
- Because the new run never reached task execution, the only available profile data remains the existing HTML reports already present under `build/reports/profile`.

### Findings
- Existing profile report `build/reports/profile/profile-2026-04-04-19-07-42.html` shows `:lib:capybara-lib` spending about `28.996s` in task execution, with these largest tasks:
  - `:lib:capybara-lib:linkTestCapybara` `18.960s`
  - `:lib:capybara-lib:linkCapybara` `3.283s`
  - `:lib:capybara-lib:compileCapybara` `1.832s`
  - `:lib:capybara-lib:compileJava` `1.706s`
- `compileCapybara`, `compileTestCapybara`, and `integration-tests:generateJavaFromCapybara` were all invoking `capy generate java`, which copies bundled `java-lib-src/dev/capylang/**` sources by default.
- `lib/capybara-lib` immediately deleted those copied files in `doLast`, so the build was paying for redundant copy + delete filesystem work every profiled run.
- `integration-tests` was also copying the same bundled Java support sources even though it already compiles against `:lib:capybara-lib`, which provides those classes on the compile classpath.

### Changes made
- Added `capy generate ... --skip-java-lib` to suppress bundled Java runtime source copying when the caller already has the runtime on the classpath.
- Switched `lib/capybara-lib` main/test generation tasks to use `--skip-java-lib`.
- Removed the `doLast` cleanup blocks from `lib/capybara-lib` because they are no longer needed and they were mutating generated outputs after task completion.
- Switched `integration-tests:generateJavaFromCapybara` to use `--skip-java-lib` for the same reason.
- Added CLI tests covering both the default generate behavior and the new skip path.

### Verification status
- I could not run Gradle task verification in this sandbox because Gradle startup still fails before any task graph is created with `Could not determine a usable wildcard IP for this machine`.
- Verification here is limited to source inspection and targeted unit-test additions for the new CLI option.

## 2026-04-06 build optimization pass direct test generation

### Requested baseline
- Re-ran the requested baseline command exactly as `./gradlew :lib:capybara-lib:check --profile --rerun-tasks --console=plain`.
- It failed immediately in this sandbox because the wrapper tried to create `/home/martin/.gradle/.../gradle-9.1.0-bin.zip.lck` on a read-only filesystem.
- Re-ran with `GRADLE_USER_HOME=$PWD/.gradle` to use the repository-local wrapper cache; Gradle startup still failed before task execution with `Could not determine a usable wildcard IP for this machine`.
- Because neither run reached task execution, no new profile HTML was produced; analysis still relies on the latest existing report in `build/reports/profile`.

### Findings
- The existing profile still shows `:lib:capybara-lib:linkTestCapybara` as the dominant cost at about `18.960s`.
- That task was compiling test Capybara sources into linked JSON only so `compileTestCapybara` could immediately read the linked program back and generate Java.
- For the `:lib:capybara-lib:check` path, the linked test directory was an internal intermediate with no downstream consumer beyond Java generation, so the build was paying for:
  - writing the full linked test program to disk,
  - rereading that linked program in a second JVM invocation,
  - carrying library modules through the linked test output even though generation filters back to test source modules plus `CapyTestRuntime`.

### Changes made
- Added a new CLI command `capy compile-generate <output-type>` that compiles Capybara sources and immediately generates code without writing linked intermediates.
- Switched `lib/capybara-lib:compileTestCapybara` to use `compile-generate java --compile-tests --skip-java-lib`, removing the separate `linkTestCapybara` task entirely.
- Added CLI coverage for `compile-generate` with libraries, test discovery, injected `CapyTestRuntime`, and `--skip-java-lib`.

### Expected impact
- `:lib:capybara-lib:check --rerun-tasks` should no longer spend time on a standalone `linkTestCapybara` task.
- The test Capybara path should avoid one full linked-JSON write/read roundtrip and one extra JVM launch for test-source generation.

### Verification status
- I still could not execute Gradle tasks in this sandbox because Gradle startup fails before project evaluation.
- Verification for this pass is limited to source inspection and added CLI tests.

## 2026-04-06 build optimization pass fused main generation

### Requested baseline
- Re-ran the requested baseline command exactly as `./gradlew :lib:capybara-lib:check --profile --rerun-tasks --console=plain`.
- It still failed immediately in this sandbox because the wrapper tried to create `/home/martin/.gradle/.../gradle-9.1.0-bin.zip.lck` on a read-only filesystem.
- Re-ran a targeted verification as `GRADLE_USER_HOME=$PWD/.gradle ./gradlew :capy:test --console=plain -Djava.net.preferIPv4Stack=true`.
- Gradle startup still failed before project execution with `Could not determine a usable wildcard IP for this machine`.
- No new profile HTML was produced, so timing comparisons still rely on the latest existing report in `build/reports/profile/profile-2026-04-04-19-07-42.html`.

### Findings
- After the earlier test-path optimization, `lib/capybara-lib` main sources were still paying for two Capy CLI invocations on every clean run:
  - `linkCapybara` compiled sources and wrote linked JSON for downstream consumers;
  - `compileCapybara` then launched a second JVM to reread that linked output and generate Java.
- The linked main output still needs to exist for `compileTestCapybara`, `compiler:processResources`, and `integration-tests`, but the separate main `generate` invocation is avoidable because both outputs come from the same in-memory compilation result.

### Changes made
- Extended `capy compile-generate` with an optional `--linked-output <dir>` flag so a single invocation can both:
  - write linked JSON plus `build-info.json`, and
  - generate Java output in the same process.
- Restricted `--linked-output` parsing to `compile-generate` only and updated CLI help text.
- Switched `lib/capybara-lib:linkCapybara` to run `compile-generate java --skip-java-lib --linked-output ...`, so it now produces both linked main artifacts and generated Java in one pass.
- Converted `lib/capybara-lib:compileCapybara` into a lightweight compatibility task that depends on `linkCapybara`, preserving the existing task name used by repository docs and dependent tasks while removing the extra JVM launch.
- Added CLI test coverage for `compile-generate --linked-output` to verify that one command writes both generated Java and linked JSON outputs.

### Expected impact
- `:lib:capybara-lib:check --rerun-tasks` should no longer pay for a separate `:lib:capybara-lib:compileCapybara` Capy CLI execution after linking main sources.
- This should remove one JVM startup plus one full reread of the linked main program on each clean `lib/capybara-lib` build.

### Verification status
- I could not run Gradle task verification in this sandbox because Gradle still fails during startup with `Could not determine a usable wildcard IP for this machine`.
- Verification for this pass is limited to source inspection and the added CLI test coverage in `capy/src/test/java/dev/capylang/CapyTest.java`.

## 2026-04-06 build optimization pass fused integration generation

### Requested baseline
- Re-ran the requested baseline command exactly as `./gradlew :lib:capybara-lib:check --profile --rerun-tasks --console=plain`.
- In this sandbox it still cannot execute:
  - with default Gradle home, the wrapper tries to create `/home/martin/.gradle/.../gradle-9.1.0-bin.zip.lck` on a read-only filesystem;
  - with a writable `GRADLE_USER_HOME`, wrapper startup then requires downloading Gradle 9.1.0, but network access is blocked;
  - the system `gradle -v` binary is also unusable here because native services fail to initialize (`Failed to load native library 'libnative-platform.so' for Linux amd64`).
- Because I could not produce a fresh run, analysis continues from the existing checked-in profile reports under `build/reports/profile`.

### Findings
- The last recorded profile for `clean check` already shows `:lib:capybara-lib` dominating task time, and previous passes in this file removed its separate main and test generation JVMs.
- The remaining Capybara build logic in `integration-tests` still used a two-step `compile` then `generate java` pipeline even though the CLI now supports `compile-generate java --linked-output`.
- That path is outside the exact `:lib:capybara-lib:check` target, but it still adds unnecessary work to repository-wide clean builds and repeats the same linked-output reread pattern that was already removed from `lib:capybara-lib`.

### Changes made
- Switched `integration-tests:linkCapybaraSources` to `compile-generate java --skip-java-lib --linked-output ...`, so one Capy CLI invocation now writes both linked JSON and generated Java.
- Added `generatedJavaDir` as an output of `linkCapybaraSources` and prepared it alongside the linked output directory before execution.
- Converted `integration-tests:generateJavaFromCapybara` into a lightweight compatibility task that just depends on `linkCapybaraSources`, preserving the existing task name used by `compileJava`.

### Expected impact
- Clean full-build paths should avoid one extra Capy CLI JVM launch and one reread of the linked integration-test program.
- The integration-tests project now follows the same fused generation pattern already applied to `lib:capybara-lib`, reducing duplicated build mechanics across the repo.

### Verification status
- I could not run Gradle verification in this sandbox for the reasons listed above, so this pass is verified only by source inspection.

## 2026-04-06 build optimization pass reusable Gradle plugin

### Requested baseline
- Re-ran the requested baseline command exactly as `./gradlew :lib:capybara-lib:check --profile --rerun-tasks --console=plain`.
- It still failed immediately in this sandbox because the wrapper tried to create `/home/martin/.gradle/.../gradle-9.1.0-bin.zip.lck` on a read-only filesystem.
- Re-ran a narrower verification attempt as `GRADLE_USER_HOME=$PWD/.gradle ./gradlew :build-tool:gradle:test --console=plain --no-daemon -Djava.net.preferIPv4Stack=true`.
- Gradle startup still failed before project execution with `Could not determine a usable wildcard IP for this machine`.
- No new profile HTML was produced, so timing comparisons still rely on the existing report at `build/reports/profile/profile-2026-04-04-19-07-42.html`.

### Findings
- The handwritten build scripts for `lib/capybara-lib` and `integration-tests` were already moved to the fused `compile-generate` path, but the reusable Gradle plugin in `build-tool/gradle` still used the older two-step pipeline:
  - `compileCapybara` compiled to linked JSON;
  - `generateCapybaraJava` reread that output to generate Java;
  - `compileTestCapybara` then repeated the pattern for tests.
- The plugin also added main generated Java sources to the `test` source set, which makes test Java compilation recompile code already compiled by the main source set.
- That left plugin consumers paying the same redundant JVM/process and source-compilation costs that earlier passes had already removed from the repository’s handwritten module builds.

### Changes made
- Extended `CompileCapybaraTask` so one task invocation can:
  - compile sources once,
  - write linked JSON output, and
  - optionally generate Java from the in-memory compilation result in the same pass.
- Extended `Capy.generateCompiledProgram(...)` with an overload that can skip bundled Java runtime source copying when the caller already has those classes from main outputs.
- Rewired `CapybaraPlugin` so:
  - `compileCapybara` now writes linked main output and generated main Java in one task;
  - `generateCapybaraJava` is kept only as a lightweight compatibility task depending on `compileCapybara`;
  - `compileTestCapybara` now writes linked test output and generated test Java in one task while skipping bundled Java runtime copying;
  - `generateTestCapybaraJava` is kept only as a compatibility dependency task;
  - the `test` source set no longer includes main generated Java sources.
- Added plugin tests covering fused main generation, test generation without duplicate bundled runtime copying, and the corrected `test` source-set wiring.
- Added `gradleTestKit()` to the plugin module test dependencies for `ProjectBuilder`-based coverage.

### Expected impact
- Plugin consumers should avoid one extra generate pass for main Capybara sources and one extra generate pass for test Capybara sources.
- Test Java compilation for plugin consumers should stop recompiling main-generated Java sources through the `test` source set.
- This aligns the reusable plugin with the same build-graph simplifications already applied to `lib/capybara-lib` and `integration-tests`.

### Verification status
- `git diff --check` passed.
- I could not run Gradle task verification in this sandbox because Gradle still fails during startup with `Could not determine a usable wildcard IP for this machine`.
- Verification for this pass is limited to source inspection and the added plugin test coverage.

## 2026-04-06 build optimization pass reusable output directories

### Requested baseline
- Re-ran the requested baseline command exactly as `./gradlew :lib:capybara-lib:check --profile --rerun-tasks --console=plain`.
- It still failed immediately in this sandbox because the wrapper tried to create `/home/martin/.gradle/.../gradle-9.1.0-bin.zip.lck` on a read-only filesystem.
- Re-ran with `GRADLE_USER_HOME=$PWD/.gradle` to use the repository-local wrapper cache.
- Gradle startup still failed before project execution with `Could not determine a usable wildcard IP for this machine`.
- No fresh profile HTML was produced, so optimization for this pass is based on the existing profile data plus source inspection of the current rerun path.

### Findings
- The requested command explicitly uses `--rerun-tasks`, so up-to-date checks are bypassed and task execution cost is dominated by the work each task repeats on an already-populated `build/` tree.
- Even after earlier fusion work removed redundant Capy CLI launches, handwritten build scripts and the reusable plugin still recursively deleted entire linked/generated output trees before every Capybara compile or generate step.
- The CLI also still enforced an “output directory must be empty” contract, which forced callers to do that delete-first work up front.
- For `lib:capybara-lib`, those recursive deletions hit the same directories that dominate the recorded profile (`build/generated/sources/capybara/...`), so rerun builds were paying avoidable filesystem traversal and delete churn before rewriting nearly the same files.

### Changes made
- Changed the Capy CLI and in-process compiler helpers to accept reusable output directories instead of requiring empty ones.
- Added stale-file pruning inside Capy’s linked-output and generated-output writers so each run now:
  - rewrites the expected current files,
  - removes only files no longer produced,
  - removes newly empty directories afterward.
- Updated bundled Java runtime source copying to participate in the same expected-file tracking so `--skip-java-lib` also removes stale copied runtime sources from previous runs.
- Removed the recursive pre-run directory cleanup from:
  - `lib/capybara-lib/build.gradle`
  - `integration-tests/build.gradle`
  - `build-tool/gradle`’s `CompileCapybaraTask`
- Added CLI tests covering stale-file pruning for reused linked and generated output directories.
- Added a plugin test covering stale-file pruning when reusing plugin task outputs.

### Expected impact
- `:lib:capybara-lib:check --rerun-tasks` should avoid full tree deletion under Capybara linked/generated output directories before each compile/generate pass.
- Repeated local builds should do less filesystem work when outputs already exist, while still removing stale artifacts after source removals or option changes like `--skip-java-lib`.

### Verification status
- `git diff --check` passed.
- I could not run Gradle task verification in this sandbox because Gradle still fails during startup with `Could not determine a usable wildcard IP for this machine`.

## 2026-04-06 build optimization pass combined check preparation

### Requested baseline
- Re-ran the requested baseline command exactly as `./gradlew :lib:capybara-lib:check --profile --rerun-tasks --console=plain`.
- It still failed immediately in this sandbox because the wrapper tried to create `/home/martin/.gradle/.../gradle-9.1.0-bin.zip.lck` on a read-only filesystem.
- I inspected `build/reports/profile` again; the available reports are still the existing HTML files through `build/reports/profile/profile-2026-04-04-19-07-42.html`.
- No new profile HTML was produced by this pass, so timing guidance remains based on the existing reports plus source inspection of the current `check --rerun-tasks` path.

### Findings
- Earlier passes removed the old standalone `linkTestCapybara` task, but the current `:lib:capybara-lib:check` path still launches Capy twice:
  - once for main source compile+generate;
  - once for test source compile+generate.
- The second invocation recompiles test sources against the main program by loading the freshly written main linked output back from disk.
- That extra process boundary only matters for test-oriented builds; main-only builds such as `compileJava` should keep the narrower main-only path.

### Changes made
- Extended `capy compile-generate` with optional `--test-input` and `--test-output` arguments.
- The new path compiles main sources, writes linked main output when requested, generates main Java, then compiles test sources against the freshly compiled main modules in the same CLI invocation and generates test Java.
- Updated `lib/capybara-lib/build.gradle` to use a combined `prepareCapybaraForCheck` task for test-oriented requests (`check`, `test`, `compileTestJava`, `compileTestCapybara`, `testCapybara`, `testClasses`) while keeping the existing main-only and test-only task split for non-test builds.
- Kept `linkCapybara` and `compileTestCapybara` as compatibility task names so downstream task wiring and cross-project dependencies remain stable.
- Added CLI tests covering the new combined main+test generation mode and updated the help-text assertion.

### Expected impact
- `:lib:capybara-lib:check --rerun-tasks` should avoid one Capy CLI JVM launch on the library module path.
- The check/test path should also avoid rereading the just-produced main linked program from disk before compiling test Capybara sources.
- Main-only tasks should keep their narrower single-source generation path instead of always compiling test Capybara sources.

### Verification status
- `git diff --check` passed.
- I could not run Gradle task verification in this sandbox because the requested wrapper command still fails before project execution on the read-only `/home/martin/.gradle` lock path.
- Verification for this pass is limited to source inspection and the added CLI tests.

## 2026-04-06 build optimization pass stale test report pruning

### Requested baseline
- Re-ran the requested baseline command exactly as `./gradlew :lib:capybara-lib:check --profile --rerun-tasks --console=plain`.
- It still failed immediately in this sandbox because the wrapper tried to create `/home/martin/.gradle/.../gradle-9.1.0-bin.zip.lck` on a read-only filesystem.
- Checked `build/reports/profile` after the run; no new profile HTML was produced, and the latest available report is still `build/reports/profile/profile-2026-04-04-19-07-42.html`.

### Findings
- `lib:capybara-lib:testCapybara` still eagerly deleted the entire `build/test-results/capybara` tree before every run.
- The requested command uses `--rerun-tasks`, so that recursive delete executes on every profiled run even when most report filenames are unchanged.
- The `TestRunner` already rewrites current report files deterministically, so full directory cleanup is broader than necessary; only stale files from removed or renamed tests actually need to be deleted.

### Changes made
- Moved Capybara test-report cleanup into `TestRunner` as stale-file pruning after writing the current run's outputs.
- Updated `lib/capybara-lib: testCapybara` to stop recursively emptying `build/test-results/capybara` and only ensure the directory exists.
- Added focused `TestRunner` unit tests covering:
  - returned relative report paths,
  - stale file removal,
  - empty-directory cleanup after stale report deletion.

### Expected impact
- `:lib:capybara-lib:testCapybara` should avoid a full pre-run tree walk/delete under `build/test-results/capybara` on every `--rerun-tasks` build.
- Repeated local builds should do less filesystem churn while still removing obsolete JUnit XML files when tests disappear or report paths change.

### Verification status
- `git diff --check` passed.
- I could not run Gradle task verification in this sandbox because the wrapper still fails before task execution with a read-only lockfile path under `/home/martin/.gradle`.
- Verification for this pass is limited to source inspection and the added unit-test coverage.
- Verification for this pass is limited to source inspection and the added CLI/plugin test coverage.

## 2026-04-06 build optimization pass single-pass stale output pruning

### Requested baseline
- Re-ran the requested baseline command exactly as `./gradlew :lib:capybara-lib:check --profile --rerun-tasks --console=plain`.
- It still failed immediately in this sandbox because the wrapper tried to create `/home/martin/.gradle/.../gradle-9.1.0-bin.zip.lck` on a read-only filesystem.
- Checked `build/reports/profile` after the run; no new profile HTML was produced, and the latest available report is still `build/reports/profile/profile-2026-04-04-19-07-42.html`.
- Tried a narrower startup probe with `GRADLE_USER_HOME=$PWD/.gradle ./gradlew :lib:capybara-lib:help --console=plain --no-daemon -Djava.net.preferIPv4Stack=true -Djava.net.preferIPv6Addresses=false`; Gradle still failed before project execution with `Could not determine a usable wildcard IP for this machine`.

### Findings
- The earlier reusable-output work removed full pre-run directory deletion, but Capy’s post-write stale pruning still traversed each linked/generated output tree more than once on every rerun build:
  - one full `Files.walk(...)` to find stale files;
  - another full `Files.walk(...)` to visit directories in reverse order;
  - plus `Files.list(...)` calls for every directory to check emptiness.
- That repeated filesystem traversal sits directly on the hot path for `linkCapybara`, `compileTestCapybara`, and other Capy compile/generate tasks that write under `build/generated/...`.
- On the requested `--rerun-tasks` path, this overhead is paid every time even when output trees are already mostly up to date.

### Changes made
- Reworked `Capy.deleteStaleFiles(...)` to use a single reverse-order walk over the output tree.
- Split stale-file deletion and empty-directory deletion into small helpers so the reverse-order walk can:
  - delete stale files immediately;
  - opportunistically delete directories after their children have already been visited.
- Replaced per-directory `Files.list(...)` emptiness checks with `Files.deleteIfExists(...)` and ignored `DirectoryNotEmptyException` for directories that still contain current outputs.
- Extended `CapyTest.shouldPruneStaleFilesFromReusedGeneratedOutputDirectory()` to also verify that now-empty stale directories are removed.

### Expected impact
- Every Capy compile/generate pass that prunes stale outputs should perform less filesystem traversal on reused output directories.
- `:lib:capybara-lib:check --rerun-tasks` should spend less time in post-write cleanup under linked and generated source trees, especially once earlier passes have already eliminated the larger duplicate compile/generate work.

### Verification status
- `git diff --check` passed.
- I could not run Gradle task verification in this sandbox because the wrapper still fails before task execution with a read-only lockfile path under `/home/martin/.gradle`, and repo-local Gradle startup still fails with `Could not determine a usable wildcard IP for this machine`.
- Verification for this pass is limited to source inspection and the updated CLI unit-test coverage.

## 2026-04-06 build optimization pass aggregated linked program cache

### Requested baseline
- Re-ran the requested baseline command exactly as `./gradlew :lib:capybara-lib:check --profile --rerun-tasks --console=plain`.
- It still failed immediately in this sandbox because the wrapper tried to create `/home/martin/.gradle/.../gradle-9.1.0-bin.zip.lck` on a read-only filesystem.
- Checked `build/reports/profile` after the run; no new profile HTML was produced, and the latest available report is still `build/reports/profile/profile-2026-04-04-19-07-42.html`.

### Findings
- Even after earlier task-graph reductions, downstream Capy steps still reload linked outputs by recursively walking every module JSON file under the linked output directory.
- That affects the `:lib:capybara-lib:check --rerun-tasks` path because `compileTestCapybara` reloads the main linked library output, and other `generate` or compile steps can do the same.
- The linked-output directory already persists the full compiled program content in aggregate at task execution time, but the on-disk format only exposed per-module JSON files plus metadata, so each downstream read paid for:
  - a full directory walk,
  - per-file open/read/deserialize work,
  - repeated filtering of non-module metadata files.

### Changes made
- Added an aggregated `program.json` artifact to linked-output directories alongside the existing per-module JSON files and `build-info.json`.
- Updated `Capy.readLinkedProgram(...)` to prefer `program.json` when present and fall back to the legacy per-module walk when reading older outputs.
- Added CLI coverage that verifies:
  - linked outputs now contain `program.json`,
  - package archives include `program.json`,
  - library compilation can succeed using only the aggregated program file after removing individual module JSON files.
- Added plugin coverage asserting that plugin-produced linked outputs also write `program.json`.

### Expected impact
- Downstream library reads on reused linked outputs should avoid recursive file walking and repeated per-module JSON deserialization when `program.json` is available.
- `:lib:capybara-lib:compileTestCapybara` and other consumers of linked outputs should do less filesystem and JSON parsing work on `--rerun-tasks` builds.

### Verification status
- `git diff --check` passed.
- I could not run Gradle task verification in this sandbox because the wrapper still fails before task execution with a read-only lockfile path under `/home/martin/.gradle`, and repo-local Gradle startup still fails with `Could not determine a usable wildcard IP for this machine`.
- Verification for this pass is limited to source inspection and the added CLI/plugin test coverage.

## 2026-04-06 build optimization pass content-aware output writes

### Requested baseline
- Re-ran the requested baseline command exactly as `./gradlew :lib:capybara-lib:check --profile --rerun-tasks --console=plain`.
- It still failed immediately in this sandbox because the wrapper tried to create `/home/martin/.gradle/.../gradle-9.1.0-bin.zip.lck` on a read-only filesystem.
- Checked `build/reports/profile` after the run; no new profile HTML was produced, and the latest available report is still `build/reports/profile/profile-2026-04-04-19-07-42.html`.

### Findings
- The requested command uses `--rerun-tasks`, so Capy compile/generate steps execute even when their outputs are already populated and logically unchanged.
- After earlier work removed duplicate task launches and broad directory deletion, the remaining Capy output path still rewrote most files unconditionally on every rerun build:
  - linked `program.json` and per-module `.json` files,
  - generated Java source files,
  - copied bundled Java runtime sources,
  - Capybara JUnit XML test reports.
- That means the hottest remaining rerun tasks still paid avoidable truncate/write/copy work across large output trees even when file contents were identical to the previous run.

### Changes made
- Added content-aware write helpers in `capy` so linked JSON and generated source files are only rewritten when bytes actually change.
- Updated bundled Java runtime source copying to skip `REPLACE_EXISTING` copies when the target file already matches the source content.
- Updated `TestRunner` to avoid rewriting identical JUnit XML reports.
- Added focused tests asserting that repeated identical compile/generate/test-report writes preserve file modification times.

### Expected impact
- `:lib:capybara-lib:linkCapybara`, `:lib:capybara-lib:compileTestCapybara`, and `:lib:capybara-lib:testCapybara` should do less filesystem work on `--rerun-tasks` builds when outputs are unchanged.
- This should reduce rerun-path write churn without changing stale-file pruning or correctness when sources actually change.

### Verification status
- `git diff --check` passed.
- I could not run Gradle task verification in this sandbox because the wrapper still fails before task execution with a read-only lockfile path under `/home/martin/.gradle`, and repo-local Gradle startup still fails with `Could not determine a usable wildcard IP for this machine`.
- Verification for this pass is limited to source inspection and the added CLI/test-runner unit-test coverage.

## 2026-04-06 build optimization pass single-pass test report pruning

### Requested baseline
- Re-ran the requested baseline command exactly as `./gradlew :lib:capybara-lib:check --profile --rerun-tasks --console=plain`.
- In this sandbox the wrapper still cannot execute the build:
  - with default Gradle home, it fails trying to create `/home/martin/.gradle/.../gradle-9.1.0-bin.zip.lck` on a read-only filesystem;
  - with `GRADLE_USER_HOME=/tmp/gradle-user-home`, the wrapper then tries to download Gradle 9.1.0, but network access is blocked;
  - using the already-installed `/mnt/d/gradle/gradle-9.1.0/bin/gradle` binary gets past wrapper download, but Gradle startup still fails before project execution with `Could not determine a usable wildcard IP for this machine`.
- Checked `build/reports/profile` after these attempts; no new profile HTML was produced, and the latest available report is still `build/reports/profile/profile-2026-04-04-19-07-42.html`.

### Findings
- `lib:capybara-lib:testCapybara` is part of the requested `check --rerun-tasks` path, so its report cleanup work is paid on every profiled rerun.
- `TestRunner.deleteStaleOutputs(...)` still used the older multi-pass cleanup pattern:
  - one `Files.walk(...)` over the tree to find stale files;
  - a second `Files.walk(...)` over the tree to find directories;
  - plus `Files.list(...)` on each directory to check whether it had become empty.
- That duplicates the same avoidable filesystem traversal pattern that was already removed earlier from Capy’s linked/generated output pruning.

### Changes made
- Reworked `TestRunner.deleteStaleOutputs(...)` to use a single reverse-order walk over the report output tree.
- Renamed the stale-file helper to `deleteFileIfStale(...)` and kept stale-file deletion inline with the reverse-order walk.
- Replaced per-directory `Files.list(...)` emptiness checks with `Files.deleteIfExists(...)` and ignored `DirectoryNotEmptyException` for directories that still contain current reports.

### Expected impact
- `:lib:capybara-lib:testCapybara` should spend less time traversing `build/test-results/capybara` on `--rerun-tasks` builds.
- Repeated test-report cleanup should now do one tree walk instead of two tree walks plus one directory listing per directory.

### Verification status
- `git diff --check` passed.
- I could not run Gradle task verification in this sandbox because all available Gradle entry points still fail before project execution for the reasons listed above.
- Verification for this pass is limited to source inspection against the existing `TestRunnerTest` coverage.

## 2026-04-06 build optimization pass plugin library program cache reuse

### Requested baseline
- Re-ran the requested baseline command exactly as `./gradlew :lib:capybara-lib:check --profile --rerun-tasks --console=plain`.
- It still failed immediately in this sandbox because the wrapper tried to create `/home/martin/.gradle/.../gradle-9.1.0-bin.zip.lck` on a read-only filesystem.
- Checked `build/reports/profile` after the run; no new profile HTML was produced, and the latest available report is still `build/reports/profile/profile-2026-04-04-19-07-42.html`.

### Findings
- Earlier work added aggregated linked-program output as `program.json`, and the CLI already prefers that file when loading linked libraries.
- The reusable Gradle plugin’s `CompileCapybaraTask` had not been updated to match: it still recursively walked every `*.json` module file in each additional input directory and deserialized modules one by one.
- That leaves plugin consumers paying unnecessary filesystem traversal and per-file JSON reads on every rerun build, especially on test compilation where main linked outputs are loaded as libraries.

### Changes made
- Updated `build-tool/gradle`’s `CompileCapybaraTask` to load library inputs through `Capy.readLinkedProgram(...)` instead of manually walking module files.
- Added plugin coverage proving test compilation still succeeds when the main linked output keeps only aggregated `program.json` and an individual module JSON file has been removed.

### Expected impact
- Plugin-backed Capybara builds should reuse the aggregated linked-program cache instead of recursively reading every module file.
- Test-oriented plugin task paths should do less filesystem and JSON parsing work when loading previously linked main outputs as libraries.

### Verification status
- `git diff --check` passed.
- I could not run Gradle task verification in this sandbox because the requested wrapper command still fails before project execution on the read-only `/home/martin/.gradle` lock path, and repo-local Gradle startup still fails with `Could not determine a usable wildcard IP for this machine`.
- Verification for this pass is limited to source inspection and the added plugin test coverage.
