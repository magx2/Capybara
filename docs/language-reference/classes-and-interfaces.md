# Classes, Interfaces, and Native Interoperability

Capybara object-oriented source files use the `.coo` extension. They define
classes for state and behavior, interfaces for nominal contracts, and traits
for reusable behavior. The same source can be generated for Java, JavaScript,
or Python.

## Classes

A class can declare constructor parameters, fields, initialization blocks, and
methods:

```coo
from /capy/lang/Result import { * }

class Counter(seed: int) {
    field value: int = seed

    init {
        if seed < 0 {
            throw error_kind("example.counter.negative", "seed must not be negative")
        }
    }

    def current(): int = this.value
}
```

Constructor parameters are available while fields are initialized and while
`init` blocks run. A field may have an initializer. Methods use `def` and may
have either an expression body (`= expression`) or a statement block.

Classes are public. Fields and methods are public by default; use `local` for
package and subpackage visibility or `private` for module-only visibility.
Classes are closed to inheritance by default; prefix a base class with `open`
when another class should be allowed to extend it.

Define a method with the same signature to replace an inherited implementation:

```coo
open class Named {
    def name(): String = "unknown"
}

class User(name: String): Named {
    field name: String = name

    def name(): String = this.name
}
```

The grammar currently accepts the method modifiers `open`, `abstract`,
`override`, and `final`, but the compiler does not retain or enforce them. Do
not rely on them to require an override, prevent an override, or emit an
abstract target-language method.

## Interfaces

An interface declares methods without bodies. A class lists its base class and
implemented interfaces after `:`:

```coo
interface Clock {
    def now_millis(): long
}

interface NamedClock: Clock {
    def name(): String
}

class FixedClock(value: long): NamedClock {
    field value: long = value

    def now_millis(): long = this.value
    def name(): String = "fixed"
}
```

Interfaces may extend other interfaces. A class may have at most one class
parent and may implement multiple interfaces. The generators use these nominal
contracts to produce the corresponding target-language inheritance shape.

Traits use the same parent list but provide implemented, behavior-only
methods. Trait fields and initialization are not supported by the current
backends.

## Using OO Types from Functional Code

A `.cfun` module can import `.coo` classes and interfaces. Construction and
method invocation cross an effect boundary: a class call returns
`Effect[Class]`, and invoking one of its methods from functional code returns
an effect containing the result.

Given `Counter.coo`:

```coo
class Counter(seed: int) {
    field value: int = seed

    def current(): int = this.value
}
```

functional code can use it as follows:

```cfun
from /capy/lang/Effect import { * }
from Counter import { Counter }

fun read_counter(seed: int): Effect[int] =
    let counter <- Counter(seed)
    counter.current()
```

Conversely, `.coo` code can import `.cfun` data types and call exported
functional functions. Generated `snake_case` function names are written in
`lowerCamelCase` when called from OO source.

## Implementing an Interface in Java, JavaScript, or Python

Use a native provider when a Capybara interface needs a host-language
implementation. This keeps the domain contract in Capybara and selects the
matching host implementation at compile time. Direct Java, JavaScript, or
Python imports are not part of `.coo` syntax.

First define the interface in a `.coo` file:

```coo
interface Clock {
    def now_millis(): long
}
```

Then expose an effectful provider from a `.cfun` file. The provider's interface
type and qualifier form its identity:

```cfun
from /capy/lang/Effect import { Effect }
from /capy/meta_prog/NativeProvider import { NativeProvider }
from Clock import { Clock }

@NativeProvider(qualifier: "system")
fun system_clock(): Effect[Clock] = <native>
```

The native class uses the same qualifier and implements the generated `Clock`
contract. For a module at `/dev/capylang/example/Clock`, implementations look
like these.

### Java

```java
package dev.capylang.example.nativeinterop;

import dev.capylang.NativeImplementation;
import dev.capylang.example.Clock;

@NativeImplementation(qualifier = "system")
public final class SystemClock implements Clock {
    @Override
    public long now_millis() {
        return System.currentTimeMillis();
    }
}
```

Java interfaces are emitted as Java interfaces. Implement the exact method name
in the generated interface: the Java generator rewrites selected
standard-library-style names, such as `is_empty` to `isEmpty` and `starts_with`
to `startsWith`, while names such as `now_millis` remain unchanged.

### JavaScript

Generated JavaScript uses CommonJS modules. Its interface contract is an
exported base class, so the implementation extends it:

```javascript
'use strict';

const { Clock } = require('../Clock.js');

@NativeImplementation("system")
class SystemClock extends Clock {
    now_millis() {
        return Date.now();
    }
}

module.exports = { SystemClock };
```

`@NativeImplementation` is compile-time syntax scanned by Capybara; generated
bootstrap code removes the decorator before Node loads the CommonJS module.
The annotated class must be exported.

### Python

The generated Python interface is a base class:

```python
import time

from dev.capylang.capybara import NativeImplementation
from dev.capylang.example.Clock import Clock


@NativeImplementation(qualifier="system")
class SystemClock(Clock):
    def now_millis(self):
        return time.time_ns() // 1_000_000
```

## Source Layout and Provider Behavior

Native implementation source is discovered in `native/java`, `native/js`, and
`native/py` directories near the Capybara input directory. When the input is a
directory named `capybara`, put `native` beside that directory. For example:

```text
src/main/
|-- capybara/dev/capylang/example/Clock.coo
|-- capybara/dev/capylang/example/ClockProvider.cfun
`-- native/
    |-- java/dev/capylang/example/nativeinterop/SystemClock.java
    |-- js/dev/capylang/example/nativeinterop/SystemClock.js
    `-- py/dev/capylang/example/nativeinterop/SystemClock.py
```

Generate only the backend whose native source should be used:

```text
capy compile-generate java -i src/main/capybara -o build/generated/java
capy compile-generate javascript -i src/main/capybara -o build/generated/js
capy compile-generate python -i src/main/capybara -o build/generated/py
```

Calling `system_clock()` returns an `Effect[Clock]`. Each execution of that
effect constructs a fresh native implementation; providers are not cached.
Exactly one implementation may exist for each interface, qualifier, and
backend combination. A missing or duplicate implementation produces a native
provider diagnostic.

Keep parameter and return types compatible with the generated contract. Native
provider wiring does not automatically translate exceptions into
`Result.Error`, adapt asynchronous host APIs, manage resource disposal, or
make host calls pure.
