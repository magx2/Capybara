# Capybara

## Development

Enable the repository Git hooks after cloning:

```bash
git config core.hooksPath .githooks
```

The hooks validate branch names and commit subjects before commits and pushes. Branch names must be the repository default branch, `{feature|bug|chore}/#{issue-number}{description}`, or `release/{major}.{minor}.x`. Commit subjects must start with a conventional type, optionally followed by an issue number, for example `feat: add parser support` or `feat(#111): add parser support`.

## Language

### Functional part

Extension: `.cfun`

#### Example

```cfun
fun main(params: List[String]): ProgramResult =
    let name = params | head | default("world")
    yield Success { result = Some { value = "Hello, " + name + "!" }}

// local aliases with let
fun greeting(firstName: String, lastName: String): String =
    let fullName = firstName + " " + lastName
    yield "Hello, " + fullName

fun score_label(score: int): String =
    let normalized = score / 10
    if normalized > 8 then "great"
    else "ok"

fun rectangle_area(width: float, height: float): float =
    let area = width * height
    yield area
 
type ProgramResult = Success { result: Option[String] } | Failure { errorCode: int }
 
fun test_if(x: int): String =
    if x > 0 then "positive"
    else if x < 0 then "negative"
    else "zero"
    
fun fibbonacci(n: int): int =
    if n <= 1 then n
    else fibonacci(n - 1) + fibonacci(n - 2)
    
fun test_pipe(n: int): String = n | fibbonacci | test_if

fun test_val(x: int): String {
    val y = x * 2
    if y > 10 then "big" else "small"
}

fun test_list(list: List[int]): List[int] =
    list 
        // filter
        |- x => x > 0
        // map
        | x => x * 2

fun test_list_reduce(list: List[int]): List[int] =
    list 
        // filter
        |- x => x > 0
        // map
        | x => x * 2
        // reduce right (default)
        |> 0 | a,b => a + b
        // reduce left 
        // |l> 0 | a,b => a + b

fun test_new_static_list(): List[int] = [1, 2, 3]

fun build_list(n: int): List[int] =
    if n <= 0 then None
    else Cons { "head": n, "tail": build_list(n - 1) }

// check: `fun add on List[T]`
fun add_to_list(n: int, list: List[int]): List[int] =
    list + n
    // or `list add n`
    // or `add(list, n)` 
    // or `+ (list, n)`
    // or `list.add(n)`
    
fun test_new_static_set(): Set[int] = {1, 2, 3}

fun test_new_static_dictonary(): Dict[int] =
    {
        "one": 1,
        "two": 2,
        "three": 3
    }

// algebraic type
type Shape = Circle | Rectangle
data Circle { radius: float }
data Rectangle { width: float, height: float }

fun area(shape: Shape): float =
    match shape with
    | Circle { radius } => 3.14 * radius * radius
    | Rectangle { width, height } => width * height

fun circle(radius: float): Circle = Circle { "radius": radius }
fun rectangle(width: float, height: float): Rectangle = Rectangle { "width": width, "height": height }

type Option[T] = Some(T) | None
data Some[T] { value: T }
single None

fun positive_or_none(x: int): Option[int] =
    if x > 0 then Some { "value": x }
    else None

// type with common value
type Person { name: String, age: int } = Student | Teacher
data Student { grade: int }
data Teacher { subject: String }
fun ppl_in_school(persons: Person): List[String] = persons | p => p.name + " is age of " + p.age

// type extension
data Point { x: float, y: float }
data Point3D =  { z: float, ...Point }

fun new_point3d(x: float, y: float, z: float): Point3D = Point3D { "x": x, "y": y, "z": z }
fun new_point3d(p: Point, z: float): Point3D = Point3D { ...p, "z": z }
fun length2d(p: Point): float = sqrt(p.x * p.x + p.y * p.y)
/// whenever Point is required I can put a Point3D and it will work because of the extension
fun test_substitution(p3d: Point3D): float = length2d(p3d)
// TODO: think about diamond problem and multiple extension (inheritance)

// higher order functions
fun apply_twice(f: (int) => int, x: int): int = x | f | f

fun compose(f: (int) => int, g: (int) => int): (int) => int = x => x | f | g

type BuildIn = Primitive | Collection | Tuple
type Primitive = Number | String | bool
type Number = int | long | float | double
type Collection[T] = List[T] | Set[T] | Dict[T]
type List[T] = Cons[T] | None
data Cons[T] { head: T, tail: List[T] }

// method on types
fun add on List[T](list: List[T], element: T): List[T] =
    match list with
    | None => Cons { "head": element, "tail": None }
    | Cons { head, tail } => Cons { "head": head, "tail": add(tail, element) }
fun `+` on List[T](list: List[T], element: T): List[T] = add(list, element)

fun add on List[T](list1: List[T], list2: List[T]): List[T] =
    match list1 with
    | None => list2
    | Cons { head, tail } => Cons { "head": head, "tail": add(tail, list2) }
// there is an operation overload so `+` have 2 different implementations based on the type of the second argument
fun `+` on List[T](list1: List[T], list2: List[T]): List[T] = add(list1, list2)

fun pop on List[T](list: List[T]): Option[Tuple[T, List[T]]] =
    match list with
    | None => None
    | Cons { head, tail } =>  Option { value: (head, tail) }
```

### Object-oriented part

Extension: `.coo`

Status: frontend scaffold only in this patch. `.coo` files are discovered, parsed, and highlighted, but the compiler still rejects them with an explicit unsupported-pipeline diagnostic until OO semantic analysis and backend lowering land.
