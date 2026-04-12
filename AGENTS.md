# Repository Guidelines

## Project Structure & Module Organization
This repository is a Gradle multi-project build (`settings.gradle`) with these main modules:
- `compiler/`: Capybara parser, linker, and Java generator (`src/main/antlr/Functional.g4`, Java sources, unit tests).
- `app/`: CLI entrypoint (`dev.capylang.Capy`) used by generation tasks.
- `integration-tests/`: `.cfun` examples and Java integration tests. Capybara files in `src/main/capybara` are compiled into generated Java.
- `lib/java-lib/`: shared Java helpers.
- `lib/capybara-lib/`: standard library written in Capybara (`src/main/capybara`) plus generated Java tests.
- `Intellij/`: editor syntax bundle and related docs.

Do not edit generated outputs under `build/generated/...`; change source `.cfun`/`.java`/grammar files instead.

## Build, Test, and Development Commands
Use the wrapper from repository root:
- `./gradlew clean test`: full build and test for all modules.
- `./gradlew :compiler:test`: run compiler unit tests only.
- `./gradlew :integration-tests:test`: run integration suite.
- `./gradlew :lib:capybara-lib:compileCapybara`: compile library Capybara sources to Java.
- `./gradlew :lib:capybara-lib:testCapybara`: compile test Capybara sources and run generated `JsonTest` main.
- If Gradle has sandbox or cache permission issues, run it with a temporary Gradle home, for example: `env GRADLE_USER_HOME=/tmp/gradle-home ./gradlew clean check`.

## Coding Style & Naming Conventions
- Java toolchain is 21 (configured in `buildSrc` conventions).
- Use 4-space indentation and standard Java naming: `UpperCamelCase` for types, `lowerCamelCase` for methods/fields.
- In `.cfun`, keep snake_case function names; Java generator maps exported names as needed.
- Private Capybara functions start with `_` and should remain distinct in generated code.
- Keep grammar changes in `compiler/src/main/antlr/Functional.g4` with matching parser/linker/generator updates.

## Testing Guidelines
- Frameworks: JUnit 5 + AssertJ.
- Add/update tests with every behavior change:
  - compiler behavior: `compiler/src/test/...`
  - language/integration behavior: `integration-tests/src/main/capybara/...` + `integration-tests/src/test/java/...`
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


