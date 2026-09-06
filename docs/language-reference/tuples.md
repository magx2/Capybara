# Tuples

A tuple is a fixed-size, ordered group of values. Unlike a `List`, a tuple can
hold values of different types while preserving the type of every position.
Tuples are useful for small, temporary groups of related values, especially
when naming the positions with a data type would add little clarity.

## Literals and Types

Write a tuple literal as two or more comma-separated expressions in
parentheses:

```cfun
fun user_summary(): Tuple[String, int, bool] = ("Ada", 42, true)
```

The type arguments of `Tuple` describe the elements in order. In the example,
index `0` is a `String`, index `1` is an `int`, and index `2` is a `bool`.
The compiler can infer this type when no annotation is needed:

```cfun
fun user_name(): String =
    let summary = ("Ada", 42, true)
    summary[0]
```

A tuple literal must contain at least two values. Parentheses around one
expression only group that expression; `(42)` is an `int`, not a tuple.

Tuple size and element order are part of the type. For example,
`Tuple[String, int]`, `Tuple[int, String]`, and
`Tuple[String, int, bool]` are different types.

## Indexing

Index a tuple with an integer literal. Indexes start at `0`, and a negative
index counts backward from the end:

```cfun
fun first(pair: Tuple[String, int]): String = pair[0]

fun last(pair: Tuple[String, int]): int = pair[-1]
```

Tuple indexing returns the value directly, with the type declared at that
position. This differs from indexing a `List`, which returns an `Option`
because a list index can be outside the list at runtime.

The index must be a literal because the compiler uses it to determine the
result type. Use a `List` when the position is selected dynamically or when
all elements have the same type.

## Slicing

A slice creates another tuple. The start is inclusive and the end is
exclusive. Either bound can be omitted, and negative bounds count from the
end:

```cfun
fun tail(values: Tuple[int, String, double]): Tuple[String, double] =
    values[1:]

fun without_last(values: Tuple[int, String, double]): Tuple[int, String] =
    values[:-1]

fun middle(values: Tuple[int, String, double, bool]): Tuple[String, double] =
    values[1:3]
```

As with tuple indexes, slice bounds must be integer literals so the compiler
can determine the exact result type.

## Destructuring in Lambdas

When a collection contains tuples, a lambda can bind each tuple position to a
separate parameter. The number of parameters must match the tuple size:

```cfun
from /capy/collection/List import { * }
from /capy/collection/Seq import { * }

fun labels(entries: List[Tuple[String, int]]): Seq[String] =
    entries | (name, count) => name + ": " + count
```

Use `_` for a position that is not needed:

```cfun
fun positive_counts(entries: List[Tuple[String, int]]): Seq[Tuple[String, int]] =
    entries |- (_, count) => count > 0
```

This form is available to collection operations that receive tuple elements,
including `map`, `filter`, `flat_map`, `any`, and `all`. The `map`, `filter`,
and `flat_map` operations also support the `|`, `|-`, and `|*` pipe operators,
respectively.

## Tuples from Collections

Several collection operations use tuples for naturally paired results:

- `Dict.entries()` returns key-value tuples.
- Adding a `(key, value)` tuple to a `Dict` adds or replaces an entry.
- `Seq.zip()` pairs corresponding elements from two sequences.
- `Set.cartesian_product()` and the `×` operator return pairs from two sets.

For example:

```cfun
from /capy/collection/Dict import { * }
from /capy/collection/List import { * }
from /capy/collection/Seq import { * }

fun display_scores(scores: Dict[int]): List[String] =
    (scores.entries() | (name, score) => name + ": " + score).as_list()
```

Prefer a `data` declaration instead when the values form a domain concept,
callers would benefit from named fields, or the shape is likely to evolve.
