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
