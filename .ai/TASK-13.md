# TASK-13: Move capy CLI entrypoint to Capybara

Issue: #184

## Goal

Move CLI command workflow and launch semantics to Capybara, with Gradle and
packaged CLIs invoking Java generated from Capybara source.

## Scope

- Implement `fun main(args: List[String]): Effect[Program]`.
- Model:
  - `Command`
  - `CommandArgs`
  - `CommandResult`
  - `CommandRouter`
  - `CompileRequest`
  - `GenerateRequest`
  - `CompileGenerateRequest`
  - `PackageRequest`
  - `BuildInfo`
  - `OutputManifest`
  - `CliDiagnostic`
- Move command parsing, option normalization, path planning, compile/generate
  orchestration, package orchestration, output pruning, build-info writing,
  Java compilation/test execution decisions, logging, and error formatting.
- Update Gradle in-process tasks to invoke generated Java compiled from
  Capybara, not old hand-written `dev.capylang.Capy`.

## Design Rules

- `CommandRouter` owns only command selection and dispatch.
- Request objects own command-specific normalization and invariants.
- Command handlers own exactly one workflow each.
- File deletion and writes stay in host/provider shell code.
- No production hand-written Java CLI facade remains after this task.

## Acceptance Criteria

- Exit codes remain:
  - `0` success
  - `1` usage error
  - `2` unexpected failure
  - `100` compilation error
- stdout/stderr and command help behavior remain compatible.
- `:capy:test`, `:capy:jsTests`, `:capy:pythonTests`, and
  `:capy:e2eTests` pass.
- Gradle in-process compile/test tasks invoke generated Capybara-compiled Java.

## Commit Save Point

Commit after this task with:

```text
feat(#184): move capy CLI entrypoint to Capybara
```
