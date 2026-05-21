# Capybara

Capybara is an experimental multi-backend language and compiler. The repository
currently contains two source languages:

- Capybara Functional (`.cfun`): expression-oriented, immutable by default, and
  built around algebraic data types, pattern matching, higher-order functions,
  explicit effects, and backend-independent code generation.
- Capybara Object-Oriented (`.coo`): class/trait/interface syntax for cohesive
  object models, with a shared expression core and interoperability with
  `.cfun` modules.

The compiler can generate Java, JavaScript, and Python. JavaScript output is
CommonJS-compatible and includes the Capybara runtime helper in the generated
tree.

## CLI

The CLI main class is `dev.capylang.Capy`. Its core commands are:

```bash
capy compile -i <source-dir> -o <linked-output-dir>
capy compile-generate <java|python|javascript|js> -i <source-dir> -o <generated-output-dir>
capy generate <java|python|javascript|js> [-i <linked-input-dir>] -o <generated-output-dir>
capy package (-ci <linked-input-dir> | -i <source-dir>) -m <capy.yml>
```

Useful options:

- `-l|--libs <dir1,dir2,...>` adds linked library modules for `compile` and
  `compile-generate`.
- `--compile-tests` compiles Capybara test producers into the test runtime.
- `--linked-output <dir>` writes linked JSON during `compile-generate`; without
  it, generation happens without linked intermediates.
- `--test-input <dir> --test-output <dir>` compiles test sources against the
  freshly compiled main program in one invocation. These options must be
  provided together.
- `--skip-java-lib` omits bundled Java runtime sources when the caller already
  has them on the compile classpath.
- `--log <DEBUG|INFO|WARN|ERROR>` controls CLI logging.

Generated output directories are reusable. The CLI records generated files in
`.capy-output-manifest` and prunes stale generated files automatically.

## Source Files, Modules, And Imports

Only `.cfun` and `.coo` files are Capybara sources; source discovery ignores
other files.

Source file names become module names. Subdirectories become module paths.

- `src/User.cfun` defines functional module `User`.
- `src/domain/User.coo` defines object-oriented module `domain/User`.

Both source languages support import lines before declarations:

```cfun
from /capy/collection/List import { * }
from Pck2 import { * } except { add }
from /dev/example/Math import { clamp, average }
import /dev/example/Geometry
```

Unqualified imports use sibling modules or modules on the library path. A
leading `/` names a fully qualified module path. `import ModuleName` imports a
module for qualified access without bringing its exported symbols into the
current scope, for example `Geometry.Point`.

Top-level `.cfun` declarations are exported by default. Use `private` for
module-private declarations and `local` for package/subpackage-visible
declarations. Leading underscores are only part of the identifier name; they are
not the visibility mechanism.

## Capybara Functional (`.cfun`)

`.cfun` is expression-oriented. Functions return expressions, `let` introduces
immutable local names, and side effects are represented explicitly with library
types such as `Effect` and `Program`.

### Basic Functions

```cfun
fun greet(name: String): String = "Hello, " + name

fun classify(x: int): String =
    if x > 0 then "positive" else "non-positive"

fun add_then_classify(left: int, right: int): String =
    let total = left + right
    classify(total)
```

Function types use `A => B` for one argument and `(A, B) => C` for multiple
arguments:

```cfun
fun apply_twice(f: int => int, value: int): int =
    f(f(value))

fun combine(f: (int, int) => int, left: int, right: int): int =
    f(left, right)
```

Use `:name` to pass a named function as a value:

```cfun
fun is_positive(value: int): bool = value > 0
fun any_positive(values: List[int]): bool = values.any(:is_positive)
```

### Local Definitions

Top-level functions may contain local functions, local types, local data,
and constants before a `---` separator:

```cfun
fun adjusted(value: int): int =
    fun double(x: int): int = x * 2
    const OFFSET = 1
    ---
    double(value) + OFFSET
```

Use this form for local declarations. Braces are expression grouping, not
imperative statement blocks.

### Data, Unions, And Enums

Capybara models domain values with product types (`data`) and sum types
(`union`):

```cfun
union Pet = Dog | Cat
data Dog { name: String }
data Cat { age: int }

fun pet_text(pet: Pet): String =
    match pet with
    case Dog { name } -> "dog:" + name
    case Cat { age } -> "cat:" + age
```

Other declaration forms:

```cfun
enum Status { READY, DONE }

data Empty {}
data User {
    name: String,
    age: int,
}
```

Value construction uses braces, including empty data values:

```cfun
fun make_dog(name: String): Pet = Dog { name: name }
fun empty_value(): Empty = Empty {}
```

### Constructors And Invariants

`with constructor` lets a type validate or normalize input. Inside a constructor,
`* { ... }` creates the raw value.

```cfun
from /capy/lang/Result import { * }
from /capy/lang/String import { * }

data User { age: int } with constructor {
    if age <= 0 then
        Error { "Age has to be greater than 0. Was " + age + "." }
    else
        Success { * { age: age } }
}

fun validate_age(age: int): String =
    match User { age: age } with
    case Success { value } -> "ok:" + value.age
    case Error { message } -> "err:" + message
```

`DataName! { ... }` bypasses a `Result`-returning constructor. It is allowed
only inside the module that defines the data type and should be reserved for
values already known to satisfy the invariant.

```cfun
fun default_user(): User = User! { age: 1 }
```

Primitive-backed types wrap primitive values while preserving a domain name:

```cfun
from /capy/lang/Result import { * }

type user_id -> int with constructor {
    if value > 0
    then Success { value }
    else Error { message: "bad user id" }
}

fun new_user_id(value: int): Result[user_id] = user_id { value }
fun unwrap_user_id(id: user_id): int = id.value
```

### Collections, Tuples, Indexing, And Slicing

```cfun
fun sample_list(): List[int] = [1, 2, 3]
fun sample_set(): Set[int] = {1, 2, 3}
fun one_value_set(): Set[int] = {1,}
fun sample_dict(): Dict[int] = {"one": 1, "two": 2}
fun empty_dict(): Dict[int] = {:}
fun pair(): Tuple[String, int] = ("age", 42)
```

Notes:

- `{1}` is expression grouping. Use `{1,}` for a one-value set.
- `{:}` is an empty dictionary.
- `values[index]` returns an `Option` for collection access.
- List slices use `values[from:to]`, `values[from:]`, and `values[:to]`.
- String slices use `text[from, to]`.

### Pattern Matching

`match` supports constructor, literal, type, wildcard, and guarded cases:

```cfun
fun describe(value: any): String =
    match value with
    case int number when number > 0 -> "positive int"
    case String text -> "string:" + text
    case _ -> "other"
```

Runtime type patterns are intentionally shallow in v1. Match concrete nominal
types or bare collection/data shapes, but do not rely on erased parameterized
runtime checks such as `List[String]`.

### Pipelines And Higher-Order Operations

The pipe family supports common collection transformations:

```cfun
fun doubled(values: List[int]): Seq[int] =
    values | value => value * 2

fun positives(values: List[int]): Seq[int] =
    values |- value => value > 0

fun duplicate(values: List[int]): Seq[int] =
    values |* value => [value, value]

fun sum(values: List[int]): int =
    values |> 0, (acc, value) => acc + value

fun dict_sum(values: Dict[int]): int =
    values |> 0, (acc, _, value) => acc + value
```

Named methods such as `.map(...)`, `.filter(...)`, `.flat_map(...)`,
`.reduce(...)`, `.any(...)`, and `.all(...)` are available through the standard
library.

### Methods And Operators

Methods are functions whose first receiver is implicit as `this`:

```cfun
data Total { value: int }

fun Total.plus(amount: int): Total =
    Total { value: this.value + amount }

fun Total.`+`(amount: int): Total =
    this.plus(amount)

fun add_two(total: Total): Total =
    total + 2
```

Backticks declare symbolic methods. Overload resolution uses argument and
receiver types.

### Recursion

Recursive functions are allowed. `fun rec` is a compiler-checked direct
tail-recursion contract:

```cfun
fun rec sum_to(n: int, acc: int): int =
    if n <= 0 then acc else sum_to(n - 1, acc + n)
```

If a `fun rec` self-call is not in tail position, compilation fails. Unmarked
recursive functions remain valid, but `fun rec` documents and verifies the
tail-recursive shape.

### Derive And Reflection

Derivers are deterministic compile-time templates that generate ordinary methods
from type metadata. Generated methods go through normal linking, validation, and
backend generation.

```cfun
from /capy/meta_prog/Reflection import { DataValueInfo, reflection }

deriver Show {
    fun show(): String =
        let info: DataValueInfo = reflection(receiver)
        info.name
}

data Account { name: String } derive Show

fun show_account(): String =
    Account { name: "main" }.show()
```

The generated receiver is named `receiver` inside a deriver method. Reflection
v1 is intentionally small: it supports `.cfun` data value metadata, derive use
cases, shallow runtime type patterns, and static `.coo` type metadata.

### Effects And Programs

Pure functions are the default. Effects are explicit values that can be composed
and returned from `main`.

```cfun
from /capy/lang/Effect import { * }
from /capy/collection/List import { * }

fun main(args: List[String]): Effect[/capy/lang/Program] =
    pure(/capy/lang/Program.Success {})
```

Use `Result[T]` for value-level errors and `Effect[T]` for delayed side effects.
There is no implicit conversion between thrown OO exceptions and functional
`Result.Error`.

### Object Construction From CFun

`.cfun` can construct imported `.coo` classes. Object construction is effectful:
calling a class constructor returns `Effect[Class]`, so constructor side effects
and `init` blocks run only when the effect is executed.

```coo
class Person(name: String) {
    field name: String = name
}
```

```cfun
from /capy/lang/Effect import { * }
import Person

fun make_person(name: String): Effect[Person.Person] =
    Person.Person(name)

fun second_person(left: String, right: String): Effect[Person.Person] =
    let ignored <- Person.Person(left)
    Person.Person(right)
```

Use `from Person import { Person }` when you want the unqualified
`Person(name)` constructor form instead.

## Capybara Object-Oriented (`.coo`)

`.coo` is for object models with state, behavior, inheritance, and explicit
statement blocks. It shares imports, many expression forms, lambdas, collection
syntax, matching, and type references with `.cfun`, but declarations use
`class`, `trait`, `interface`, `field`, and `def`.

### Classes, Interfaces, And Traits

```coo
from /capy/io/Stdout import { * }

open class GreeterBase {
    field prefix: String = "hello"

    open def describe(name: String): String = prefix + " " + name
}

interface Printable {
    def print(): String
}

trait BracketNaming {
    def bracket(name: String): String = "[" + name + "]"
}

class Person(name: String): GreeterBase, Printable, BracketNaming {
    field name: String = name

    def greet(): String = this.describe(this.name)

    override def print(): String {
        let label: String = GreeterBase.describe(this.name)
        return label + "!"
    }

    def emit_greeting(): void = println(this.greet())
}
```

Class methods may use expression bodies (`= expression`) or statement blocks.
Interfaces declare method contracts. Traits provide behavior-only reusable
methods in the current backends.

### OO Statements

Statement blocks support immutable locals, mutable locals, assignment, returns,
conditionals, loops, exceptions, local methods, and nested blocks:

```coo
class Counter(seed: int) {
    field seed: int = seed

    def first_positive(values: List[int]): int {
        for value in values {
            if value > 0 {
                return value
            }
        }
        return 0
    }

    def mutable_label(): String {
        def label = "start"
        label = "done"
        return label
    }

    def recover(flag: bool): String {
        try {
            if flag {
                throw "boom"
            }
            return "ok"
        } catch error {
            return error.getMessage()
        }
    }
}
```

Use `let` for immutable local values. Use mutable `def` locals only when the OO
workflow needs assignment. Prefer behavior on cohesive objects over utility-style
procedural blocks.

### Arrays And Entrypoints

`.coo` supports array types and literals:

```coo
class Names {
    def second(values: String[]): String = values[1]
    def defaults(): String[] = String[]{"zero", "one"}
    def slots(size: int): int[] = int[size]
}
```

An OO `main(args: List[String])` entrypoint can be generated when the method is
static-compatible. Java also requires the entrypoint class to avoid constructor
state and `init` blocks.

### Functional Interop

`.coo` can import `.cfun` data and call exported functional module functions.
Functional `snake_case` exports are called by their generated lower-camel names
from OO source and generated backends.

```coo
from ObjectOrientedFpInterop import { InteropPet, InteropDog, InteropCat, user_id }

class PetInteractor {
    def create_fp_data(name: String): InteropPet =
        InteropDog { name: name }

    def invoke_fp_function(name: String): String =
        ObjectOrientedFpInterop.petText(ObjectOrientedFpInterop.makeDog(name))

    def match_fp_type(pet_name: String): String {
        let pet: InteropPet = InteropDog { name: pet_name }
        return match pet with
        case InteropDog { name } -> ("dog:" + name)
        case InteropCat { age } -> ("cat:" + age)
    }

    def echo_user_id(id: user_id): user_id = id
}
```

Primitive-backed `.cfun` types without custom constructors can be constructed
directly from `.coo`. For types with custom constructors, expose and call a
functional factory.

### OO Reflection

Classes, interfaces, and traits expose static `type()` metadata in generated
code. The metadata includes names, fields, methods, parent types, and modifier
information used by the reflection APIs and tests.

## Language Guardrails

Keep these boundaries in mind when changing code or examples:

- `.cfun` is expression-oriented. Avoid legacy draft keywords and old
  pipe-prefixed match branch examples.
- Match branches use `case Pattern -> expression`.
- `.cfun` methods are declared on the receiver type with dotted method syntax.
- `fun rec` is a tail-recursion assertion, not a general recursion keyword.
- `* { ... }` is constructor-local raw construction.
- `DataName! { ... }` is an unsafe same-module constructor bypass for
  `Result`-returning constructors.
- `.coo` methods use `def`; `.cfun` functions use `fun`.
- Trait state and trait initialization are not supported by the current OO
  backends.
