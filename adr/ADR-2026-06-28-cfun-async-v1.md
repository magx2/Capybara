# ADR-2026-06-28: `.cfun` Async Effects v1

- Status: accepted
- Deciders: Codex, repository maintainers
- Date: 2026-06-28

## Status

Accepted.

## Context

Capybara Functional already uses `Effect[T]` to make side effects explicit at
API boundaries. That model should remain intact: pure `.cfun` code stays pure,
and effectful work is visible in function signatures.

Issue #297 adds a need to start effectful work concurrently and combine the
result later. Reusing `Effect[T]` itself as the async handle would make the
type carry two different meanings: a description of effectful work and a
running computation. Making every `Effect` parallel would also hide scheduling
behind ordinary sequencing and would make simple effect composition harder to
reason about.

ADR-2026-06-19 established that recoverable failures are represented as
`Result` values, and side-effecting fallible APIs return `Effect[Result[T]]`.
Async observation needs to follow that same rule so host failures, mapper
failures, and user-level fallibility have one explicit value-level path.

The implementation must map cleanly to Java, JavaScript, and Python backends.
Java can use an executor-backed future, while JavaScript and Python use their
runtime async helpers. JavaScript output must remain CommonJS-compatible.

This ADR does not cover parallel `Seq` operations. That decision is deferred
because lazy and potentially unbounded sequences need a separate design that
preserves bounded demand, for example `Seq.natural().filter(...).take(10)`.

## Decision

Add `Async[T]` as a primitive native `.cfun` handle for work that has already
been started.

Starting async work is explicit and effectful:

```capybara
fun Effect[T].start(): Effect[Async[T]] = <native>
```

Observing async work is explicit and effectful:

```capybara
fun Async[T].join(): Effect[Result[T]] = <native>
fun Async[T].is_done(): Effect[bool] = <native>
```

`Async[T]` is an inert handle in pure code. Holding, passing, or composing an
`Async[T]` value must not observe its result. `join` is the boundary that may
block, await, or otherwise cooperate with the backend scheduler.

Add async composition helpers that transform the eventual result without
forcing a join:

```capybara
fun Async[T].map(map: T => Y): Async[Y] = <native>
fun Async[T].flat_map(flatmap: T => Result[Y]): Async[Y] = <native>
fun Async[T].`|`(map: T => Y): Async[Y] = this.map(map)
fun Async[T].`|*`(flatmap: T => Result[Y]): Async[Y] = this.flat_map(flatmap)
```

`map` runs the mapper after the source async succeeds and produces a new async
handle for the mapped value. `|` is only an alias that invokes `map`.
`flat_map` runs a mapper that returns `Result[Y]`; `Success` unwraps into the
returned async value and `Error` becomes the result observed by `join`. `|*` is
only an alias that invokes `flat_map`.

Failures are represented as values when the async result is joined:

- If the started effect fails at the runtime boundary, `join()` returns
  `Error` with kind `capy.lang.async.failed`.
- If a `map` mapper fails at the runtime boundary, `join()` returns `Error`
  with kind `capy.lang.async.failed`.
- If a `flat_map` mapper returns `Error`, `join()` returns that same `Error`.
- If native interop returns an invalid `flat_map` result, the runtime may
  normalize it to `capy.lang.argument.invalid`.

There is no new `async` or `await` syntax. There is no implicit parallelism for
all effects. There is no language-level concurrency-limit parameter; executor,
thread-pool, event-loop, or scheduler sizing is controlled by the backend
runtime and host configuration. There is no automatic cancellation or
structured concurrency in v1.

`Async` is not a replacement for `Effect`. Effects still mark side effects;
`Async` marks a started computation whose result can later be observed through
an effect.

## Consequences

The language can express concurrent effect starts while preserving explicit
effect boundaries:

```capybara
let task <- load_user(id).start()
let name_result <- (task | user => user.name).join()
```

Async pipelines can be built without blocking until `join`, and recoverable
mapper failures remain typed as `Result` values:

```capybara
let checked = task |* user => validate_user(user)
```

Backends and generators need special handling for the native `Effect.start` and
`Async` operations, including error normalization into `Result.Error`. Runtime
helpers must preserve backend-specific scheduling while presenting the same
Capybara semantics.

The design deliberately leaves cancellation, structured concurrency, timeout
APIs, and parallel sequence processing for later ADRs. Those features need
their own contracts so they do not weaken demand-driven sequence behavior or
the explicit `Effect` model.
