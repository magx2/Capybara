# Main Functions

A top-level `main` function can be the entry point of a Capybara program. For
the Java backend, the compiler generates a JVM launcher when the function has
this shape:

```cfun
fun main(args: List[String]): Effect[Program] = ...
```

## Entry-Point Contract

The function must meet all of these requirements:

- Its name is `main` and its visibility is public. Top-level functions are
  public by default, so the `public` keyword is normally unnecessary.
- It has exactly one parameter whose type is `List[String]` or `Seq[String]`.
- It returns `Effect[Program]`, using the standard `/capy/lang/Effect` and
  `/capy/lang/Program` types.

A function merely named `main` remains a normal callable function if it does
not match this contract. For example, `fun main(): Program`, a private `main`,
a `main` with multiple parameters, or one returning `Effect[int]` does not
produce a JVM launcher.

## Command-Line Arguments

The argument collection contains the command-line arguments passed after the
generated program's class name. With `Seq[String]`, the launcher converts the
JVM argument array to a sequence before calling `main`.

## Effects And Exit Codes

`main` returns an effect so that program I/O and other side effects remain
explicit. The generated launcher executes that effect once and interprets its
`Program` value as the process result:

- `Program.Success {}` completes with exit code `0`.
- `Program.Failed { exit_code }` terminates with that non-zero exit code. A
  `failed_exit_code` is restricted to the portable range `1..255`; use
  `DEFAULT_FAILED_EXIT_CODE` for a generic failure.

## Example

This program prints a greeting when given a name and otherwise returns the
default failure code:

```cfun
from /capy/io/Console import { println }
from /capy/lang/Effect import { Effect, pure }
from /capy/lang/Program import {
    Program, Success, Failed, DEFAULT_FAILED_EXIT_CODE
}

fun main(args: List[String]): Effect[Program] =
    match args[0] with
    case Some { name } ->
        println("Hello, " + name).map(_ => Success {})
    case None ->
        pure(Failed { exit_code: DEFAULT_FAILED_EXIT_CODE })
```
