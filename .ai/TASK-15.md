# TASK-15: Switch compiler and CLI to self-hosted path

Issue: #184

## Goal

Cut over production compiler and CLI ownership to the self-hosted Capybara
implementation after deterministic bootstrap parity is proven.

## Bootstrap Stages

1. Stage 0: existing Java compiler builds Capybara compiler sources.
2. Stage 1: generated Capybara compiler compiles the same compiler sources.
3. Stage 2: Stage 1 compiler compiles the same sources again.
4. Compare Stage 1 and Stage 2:
   - linked outputs;
   - generated Java;
   - generated JS/Python if applicable;
   - native provider catalogs, manifests, and backend bootstrap modules;
   - CLI command contract outputs for compile, generate, compile-generate, and
     package flows;
   - diagnostics for invalid fixtures.

## Cutover Rules

- Do not delete old Java semantic/generator code until Stage 1/2 parity is
  stable.
- Old Java paths may remain only behind a test-only parity harness.
- Production Gradle tasks, package manifests, and user-facing invocations must
  point at Capybara-compiled Java entrypoints.
- Removal happens in small commits by subsystem.

## Acceptance Criteria

- Self-hosted compiler passes full repository tests.
- Stage 1/2 output is stable.
- Native provider bootstrap and manifest compatibility are stable.
- CLI contract parity is stable.
- Gradle and packaged CLI entrypoints invoke generated Java from Capybara
  source.

## Commit Save Point

Commit after this task with:

```text
feat(#184): switch compiler and CLI to self-hosted path
```
