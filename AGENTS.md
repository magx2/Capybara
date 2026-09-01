# Repository Guidelines

## Project Structure & Module Organization
This repository is a Gradle multi-project build (`settings.gradle`) with these main modules:
- `capy/`: CLI entrypoint (`dev.capylang.cli.Capy`), compiler/parser/generator sources (`src/main/antlr`, `src/main/java`), compiler tests, and integration/e2e tests (`src/e2e-cfun` and `src/e2e-coo` source sets).
- `lib/java-lib/`: shared Java helpers.
- `lib/capybara-lib/`: standard library written in Capybara (`src/main/capybara`) plus generated Java tests.
- `Intellij/`: editor syntax bundle and related docs.

Do not edit generated outputs under `build/generated/...`; change source `.cfun`/`.java`/grammar files instead.

## Build, Test, and Development Commands
Use the wrapper from the repository root. Gradle parallelism and the configuration cache are already enabled in `gradle.properties`.

### Fast feedback first
- Do not run `clean` by default. It discards incremental outputs and makes the next build substantially slower. Use it only to diagnose stale/corrupt outputs or when explicitly requested.
- Start with the narrowest task that covers the changed code, then broaden verification only when the change crosses layers or before final handoff.
- For one JUnit test, use Gradle filtering: `./gradlew :capy:test --tests 'fully.qualified.TestClass.testMethod'`.
- For compiler integration tests, use `./gradlew :capy:integrationTest`, optionally with the same `--tests` filter.
- Reuse the Gradle daemon and the repository's normal Gradle user home. Do not add `--no-daemon`, `--rerun-tasks`, `--refresh-dependencies`, or a temporary `GRADLE_USER_HOME` unless troubleshooting requires it; these options defeat useful caches.
- Combine independent required tasks in one invocation when practical so Gradle configures the build once, for example: `./gradlew :capy:test :capy:e2e-javascript-cfun`.

### Task selection
- Java/compiler-only change: `./gradlew :capy:test`.
- Functional language behavior: `./gradlew :capy:e2e-javascript-cfun` and/or `./gradlew :capy:e2e-python-cfun`.
- Object-oriented language behavior: `./gradlew :capy:e2e-javascript-coo` and/or `./gradlew :capy:e2e-python-coo`.
- A single e2e case: use `--available-tests` on the specific backend/suite task, then append `--tests '<exact-selector>'`. E2e selectors require an exact file or test name; they do not support wildcard patterns.
- All JavaScript e2e tests: `./gradlew :capy:e2e-javascript`.
- All Python e2e tests: `./gradlew :capy:e2e-python`.
- Compile standard-library Capybara sources: `./gradlew :lib:capybara-lib:compileCapybara`.
- Test the standard library on one enabled backend: `./gradlew :lib:capybara-lib:testCapybaraJavaScript` or `./gradlew :lib:capybara-lib:testCapybaraPython`. The Java runtime test path is currently disabled and should not be invoked.
- Test the standard library on all enabled backends (JavaScript and Python): `./gradlew :lib:capybara-lib:testCapybara`.
- Broad project verification without deleting caches: `./gradlew test` or `./gradlew check`, choosing `check` when integration checks are relevant.
- Full clean verification, only when justified or requested: `./gradlew clean test`.

If Gradle has sandbox or cache permission issues, use a temporary Gradle home only as a fallback. On PowerShell: `$env:GRADLE_USER_HOME = Join-Path $env:TEMP 'capybara-gradle'; ./gradlew <task>`.

## Coding Style & Naming Conventions
- Java toolchain is 21 (configured in `buildSrc` conventions).
- Use 4-space indentation and standard Java naming: `UpperCamelCase` for types, `lowerCamelCase` for methods/fields.
- In `.cfun`, keep snake_case function names; Java generator maps exported names as needed.
- Private Capybara functions start with `_` and should remain distinct in generated code.
- Keep grammar changes in `capy/src/main/antlr/Functional.g4` with matching parser/linker/generator updates.

## Testing Guidelines
- Frameworks: JUnit 5 + AssertJ.
- Add/update tests with every behavior change:
  - compiler behavior: `capy/src/test/...`
  - language/integration behavior: `capy/src/e2e-cfun/...` and/or `capy/src/e2e-coo/...`
- Prefer focused test names (e.g., `reduceDict`, `should_dict_of_obj`).

## Commit & Pull Request Guidelines
- Follow concise conventional-style commits and include the issue number in the type prefix: `feat(#99): ...`, `fix(#99): ...`, `test(#99): ...`, `chore(#99): ...`.
- Take the issue number from the branch name when available. If the branch name does not contain an issue number, ask for it before creating the commit.
- Keep commits logically scoped (grammar, linker, generator, tests).
- When renaming or moving tracked files, use `git mv` instead of deleting and re-adding files.
- PRs should include:
  - what changed and why,
  - impacted modules,
  - commands run (for example `./gradlew clean test`),
  - sample `.cfun` snippet/output when behavior changes.

## Capybara Language Change Rules
- Any change to grammar, linking, type checking, name resolution, diagnostics, or code generation must be evaluated for:
  - compiler unit tests,
  - integration tests,
  - compilation-error tests for invalid programs.
- If behavior changes for `.cfun` programs, include at least one source example covering the new or changed behavior.
- Prefer changing the smallest layer possible:
  - syntax-only issue -> grammar/parser/tests
  - semantic issue -> linker/validator/tests
  - output issue -> generator/tests
- Do not fix source problems by editing generated Java under `build/generated/...`.
