# Primitive-Backed Types

A primitive-backed type gives a domain-specific name to a primitive value. It
can also enforce an invariant whenever a value is constructed.

## Declaration Syntax

```text
[private | local] type name -> backing_type [with constructor {
    expression
}]
```

The backing type must be one of `byte`, `int`, `long`, `float`, `double`, or
`String`. Type names are commonly written in `snake_case`, such as `index`,
`user_id`, or `byte_count`.

Without a custom constructor, the declaration only introduces the new type:

```cfun
type second -> long

fun two_seconds(): second = second { 2L }
```

Use `.value` to access the wrapped value:

```cfun
fun seconds_as_long(duration: second): long = duration.value
```

Primitive-backed values proxy operations and methods from their backing type.
A method or operator declared for the new type takes precedence over the
backing implementation.

## Validated Construction

Add `with constructor` to validate or normalize the input:

```cfun
from /capy/lang/Result import { * }

/// Zero-based collection position.
type index -> int with constructor {
    if value >= 0
    then Success { value }
    else Error {
        kind: "capy.lang.index.out_of_bounds",
        message: "index must be greater than or equal to 0"
    }
}
```

The constructor has one implicit parameter named `value`. Its type is the
declared backing type, so `value` is an `int` in the example above.

Constructing the type without `!` runs the constructor expression:

```cfun
fun parse_index(value: int): Result[index] = index { value }
```

The result of `index { value }` is the result of the constructor body. In the
common `Result` pattern above, `Success { value }` is retagged as
`Success[index]`, while `Error` is returned unchanged. Consequently, valid
input produces a `Result[index]` and invalid input produces an `Error`.

The constructor may normalize a value before wrapping it. For example:

```cfun
type drive_letter -> String with constructor {
    if value.size() == 1
    then Success { value.to_upper_case() }
    else Error { message: "drive_letter must contain one character" }
}
```

## Using the Type

Declare extension functions on the primitive-backed type in the same way as
for other Capybara types. Access the backing value through `this.value`:

```cfun
fun index.next(): Result[index] =
    index { this.value + 1 }

fun index.`==`(other: index): bool =
    this.value == other.value
```

A validated result can be unwrapped with a result-binding `let`:

```cfun
fun next_index(value: int): Result[index] =
    let current <- index { value }
    current.next()
```

## Bypassing a Constructor

Append `!` to construct the raw value without running the custom constructor:

```cfun
const FIRST_ELEMENT: index = index! { 0 }
```

The bypass form returns `index` directly rather than `Result[index]`. It is
available anywhere the type itself is visible, including modules that import a
public primitive-backed type. Reserve it for values that are already known to
satisfy the type's invariant, because the compiler does not enforce the custom
constructor's validation when `!` is used.

## Visibility and Documentation

Primitive-backed types are public by default. Prefix a declaration with
`private` for module-only visibility or `local` for package and subpackage
visibility:

```cfun
private type internal_id -> long
local type package_id -> long
```

Documentation comments and annotations can be placed before the declaration,
just as with functions, data types, and unions.
