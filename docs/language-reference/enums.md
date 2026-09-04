# Enums

An enum defines a closed set of named values. Use one when every value has the
same type and no value needs additional fields.

## Declaration Syntax

```text
enum TypeName { VALUE, ... }
```

An enum must contain at least one value. The declaration may span multiple
lines and may have a trailing comma:

```cfun
enum DeliveryStatus {
    PENDING,
    IN_TRANSIT,
    DELIVERED,
}
```

Enum type names and values use type identifiers. By convention, type names use
`UpperCamelCase` and values use `UPPER_SNAKE_CASE`.

## Using Enum Values

Values declared in the current module can be referenced by their bare name or
qualified with the enum type:

```cfun
fun initial_status(): DeliveryStatus = PENDING

fun completed_status(): DeliveryStatus = DeliveryStatus.DELIVERED
```

Enum values are constants. They do not use data-construction braces, so write
`PENDING`, not `PENDING {}`.

They can be stored in data fields and collections like other values:

```cfun
data Delivery {
    status: DeliveryStatus,
    history: List[DeliveryStatus],
}

fun new_delivery(): Delivery =
    Delivery {
        status: PENDING,
        history: [PENDING],
    }
```

## Names, Order, and All Values

Each enum value has two fields:

- `name: String` is the value's declared name.
- `order: int` is its zero-based position in the declaration.

The enum type also exposes `values`, a `Set` containing every declared value:

```cfun
fun status_name(status: DeliveryStatus): String = status.name

fun status_order(status: DeliveryStatus): int = status.order

fun all_statuses(): Set[DeliveryStatus] = DeliveryStatus.values
```

For the declaration above, `PENDING.order` is `0` and
`DELIVERED.order` is `2`. Reordering values therefore changes their `order`.

## Parsing

`TypeName.parse` accepts either a value name or its integer order and returns a
`Result[TypeName]`:

```cfun
from /capy/lang/Result import { * }

fun parse_status(name: String): Result[DeliveryStatus] =
    DeliveryStatus.parse(name)

fun status_at(order: int): Result[DeliveryStatus] =
    DeliveryStatus.parse(order)
```

Parsing is exact: the string must match the declared name, including its case.
An unknown name or order produces an `Error` rather than an enum value.

## Pattern Matching

Match enum values directly by name. List every value when each case has
different behavior, or add a wildcard for a shared fallback:

```cfun
fun status_label(status: DeliveryStatus): String =
    match status with
    case PENDING -> "pending"
    case IN_TRANSIT -> "in transit"
    case DELIVERED -> "delivered"

fun is_finished(status: DeliveryStatus): bool =
    match status with
    case DELIVERED -> true
    case _ -> false
```

The built-in `enum` type can be used when code should accept a value from any
enum. Its `name` is available after a type pattern:

```cfun
fun enum_name(value: any): String =
    match value with
    case enum item -> item.name
    case _ -> "not an enum"
```

Prefer a concrete enum type when the accepted set is known, because it
preserves the specific type and its values.

## Imports

Import the enum type when it appears in a signature or when values are
qualified. Import individual enum values to use their bare names:

```cfun
from /shipping/Delivery import { DeliveryStatus, PENDING, DELIVERED }

fun queued(): DeliveryStatus = PENDING

fun done(): DeliveryStatus = DeliveryStatus.DELIVERED
```

A wildcard import makes the module's enum types and values available along
with its other public declarations.

## Visibility, Documentation, and Annotations

Enums and their values are public. Unlike functions, data declarations, and
unions, enum declarations do not currently accept `private` or `local`.

Documentation comments and enum-targeted annotations can be placed before the
enum declaration:

```cfun
from /capy/meta_prog/Annotations import { Deprecated }

/// State of a delivery in the shipping workflow.
@Deprecated(message: "use ShipmentStatus", since: "2.0")
enum DeliveryStatus { PENDING, IN_TRANSIT, DELIVERED }
```

Documentation and annotations apply to the enum declaration as a whole.
That annotation metadata is also available when an enum value is reflected.
Individual values cannot currently declare their own documentation comments or
annotations.
