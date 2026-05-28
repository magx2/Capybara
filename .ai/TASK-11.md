# TASK-11: Port JavaScript generation to Capybara

Issue: #184

## Goal

Move JavaScript CommonJS backend emission and JavaScript native provider
bootstrap generation into Capybara.

## Scope

- Port JavaScript CommonJS backend emitter.
- Port JavaScript native provider bootstrap emitter.
- Preserve generated `dev/capylang/native_providers.js` behavior.
- Preserve JS fixture-copy/module-resolution behavior until a replacement is
  implemented.

## Requirements

- Keep generated output CommonJS-compatible.
- Use `'use strict'`, `require(...)`, and `module.exports`.
- Do not switch to ESM in this migration.
- Avoid syntax requiring newer Node than the pinned Gradle Node version.
- Preserve lazy OO construction semantics:
  effect creation must not allocate, and each effect run must allocate a fresh
  instance when construction is modeled as an effect.

## Acceptance Criteria

- JavaScript generator unit tests pass.
- Normalized generated JS output parity tests pass.
- `:capy:jsTests` passes.
- `:capy:e2e-cfun-js`, `:capy:e2e-coo-js`, and
  `:capy:e2e-coo-js-native` pass.

## Commit Save Point

Commit after this task with:

```text
feat(#184): port JavaScript generation to Capybara
```
