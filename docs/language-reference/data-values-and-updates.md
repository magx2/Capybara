# Data Values and Updates

A `data` declaration defines an immutable value with named fields. After a
value has been created, its fields cannot be assigned new values directly.
Use the `with` expression to create a copy containing the required changes.

## Declaring and Constructing Data

Declare the fields and their types inside braces:

```cfun
data User {
    name: String,
    age: int
}
```

Construct a value by providing every required field:

```cfun
fun new_user(): User = User { name: "Ada", age: 36 }
```

Read a field with dot notation:

```cfun
fun user_name(user: User): String = user.name
```

## Updating a Field

Call `.with(...)` on an existing value and name the field to replace:

```cfun
fun birthday(user: User): User =
    user.with(age: user.age + 1)
```

The expression returns a new `User`. Fields that are not named keep their
existing values. The original value is unchanged:

```cfun
fun example(): String =
    let original = User { name: "Ada", age: 36 }
    let updated = original.with(age: 37)
    original.name + ": " + original.age + " -> " + updated.age
```

Direct field assignment is not supported:

```cfun
// Invalid: data fields are immutable.
user.age = 37
```

## Updating Several Fields

Pass multiple named fields to one `with` expression:

```cfun
fun rename_and_increment(user: User): User =
    user.with(
        name: "Grace",
        age: user.age + 1
    )
```

The replacement value must have the declared type of its field, and every
named field must exist on the data type.

Updates can also be chained. Each call operates on the value returned by the
previous call:

```cfun
fun update_in_steps(user: User): User =
    user
        .with(name: "Grace")
        .with(age: 38)
```

Use one call when the changes belong to one logical update. Chaining is useful
when updates are produced in separate steps.

## Updating Nested Data

To update a nested value, first create the updated inner value and place it in
a copy of the outer value:

```cfun
data Address { city: String, country: String }
data Account { user: User, address: Address }

fun move_to(account: Account, city: String): Account =
    account.with(
        address: account.address.with(city: city)
    )
```

Both the original `Address` and the original `Account` remain unchanged.

## Constructors and Invariants

If a data type has a custom constructor, `with` invokes that constructor again.
This preserves the same validation or normalization rules used during initial
construction.

For a constructor that returns the data value directly, `.with(...)` also
returns the data value:

```cfun
data Counter { value: int } with constructor {
    * { value: if value < 0 then 0 else value }
}

fun decrement(counter: Counter): Counter =
    counter.with(value: counter.value - 1)
```

If the constructor returns a `Result`, `.with(...)` returns a `Result` too:

```cfun
from /capy/lang/Result import { * }

data PositiveCounter { value: int } with constructor {
    if value >= 0 then
        Success { * { value: value } }
    else
        Error { "counter cannot be negative" }
}

fun decrement(counter: PositiveCounter): Result[PositiveCounter] =
    counter.with(value: counter.value - 1)
```

Handle or bind that result in the same way as the result of the original
constructor.

## Updating a Union Field

When data types belong to a union with shared fields, `with` can update a
shared field through the union type. The concrete data type and its other
fields are preserved:

```cfun
union Pet { name: String } = Dog | Cat

data Dog { favorite_toy: String }
data Cat { lives: int }

fun rename(pet: Pet, name: String): Pet = pet.with(name: name)
```

Calling `rename` with a `Dog` returns an updated `Dog` as a `Pet`; it does not
change the value into a `Cat`.
