# ADR-2026-06-19: Result-Based Error Handling

- Status: accepted
- Deciders: Codex, repository maintainers
- Date: 2026-06-19

## Status

Accepted.

## Context

Capybara needs checked-like recoverable failures without Java-style checked
exceptions. Callers should see expected failure in the type they receive, but
the language should not require `catch`, add generated `throws` declarations, or
make Java exception mechanics part of the public Capybara contract.

The standard library previously mixed "failure" and "error" vocabulary and used
message-only `Result.Error` values. That made errors easy to print, but hard to
handle predictably across modules, backends, and object-oriented control flow.

The OO frontend already has `throw` and `try` / `catch`; that control-flow
mechanism needs to carry the same error value as `Result` without becoming an
implicit bridge between exceptions and value-level failures.

## Decision

Recoverable failures are represented as values.

- Pure fallible APIs return `Result[T]`.
- Side-effecting fallible APIs return `Effect[Result[T]]`.
- Absence that is not an error may still use `Option[T]`.
- Side effects that have no meaningful recoverable failure in the Capybara
  contract may remain plain `Effect[T]`.
- The compiler must not add Java `throws` declarations for value-level failures.
- Callers are never required by the compiler to catch recoverable failures.

`capy/lang/Result.Error` is the single structured error value used by both
functional and OO code:

- `kind`: stable machine-readable classifier.
- `message`: human-readable diagnostic text.
- `details`: optional structured domain payload.
- `location`: optional source location.
- `stack_trace`: structured backend-neutral frames when available.
- `raw_stack`: optional backend-rendered stack text.
- `cause`: optional wrapped error.
- `suppressed`: secondary errors.

Simple construction should go through standard helpers such as `error`,
`error_kind`, `error_with`, `error_at`, `error_full`, `fail`, `fail_kind`,
`fail_with`, and `fail_error`. Code that modifies errors should use helpers such
as `with_kind`, `with_message`, `with_details`, `with_location`,
`with_stack_trace`, `with_raw_stack`, `with_cause`, `with_suppressed`, and
`add_suppressed` instead of rebuilding the record by hand.

Error kinds are stable API. Standard-library kinds use the reserved `capy.*`
namespace with lowercase dot-separated segments:

```text
capy.io.path.not_found
capy.io.path.permission_denied
capy.io.write.failed
capy.lang.parse.invalid_int
capy.lang.index.out_of_bounds
capy.serialization.json.invalid_syntax
```

User and third-party code must use their own namespace, such as
`app.user.not_found` or `my_lib.payment.card_declined`.

OO `throw` / `catch` uses `Error` as its thrown and caught entity:

- `throw expression` type-checks `expression` as `capy/lang/Result.Error`.
- `catch` binds an immutable `Error`.
- `catch "kind" error` may branch by exact `Error.kind` before a fallback
  `catch error`.
- If no catch branch matches, the same `Error` is rethrown.
- OO throwing remains imperative control flow. It is not implicitly converted to
  `Result.Error`, and `Result.Error` is not implicitly thrown.

Host exceptions caught at Capybara runtime boundaries are normalized into
structured `Error` values. Backend-specific class names and raw stack strings
may appear in diagnostics, but public handling should depend on `kind` and
structured fields rather than host exception classes.

## Consequences

Fallibility is visible at API boundaries without forcing Java-style exception
handling into Capybara source. A caller cannot accidentally treat a
`Result[User]` as `User`; it must match, reduce, recover, propagate, or unwrap
the result explicitly.

The same `Error` value works across `.cfun`, `.coo`, Java, JavaScript, and
Python, which keeps backend interop and tests focused on Capybara semantics
rather than host exception mechanics.

Libraries need to treat `Error.kind` as compatibility surface. Renaming an
error kind is a breaking change even if the human-readable message is unchanged.

The language keeps two explicit failure paths:

- `Result` for recoverable value-level failures.
- OO `throw` / `catch` for imperative control flow over the same `Error` value.

Bridging between those paths is allowed only through explicit library or user
code.
