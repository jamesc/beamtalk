# Gang of Four Patterns in Beamtalk

Beamtalk implementations of the classic Design Patterns from
_Design Patterns: Elements of Reusable Object-Oriented Software_
(Gamma, Helm, Johnson, Vlissides — the "Gang of Four").

Each pattern is self-contained in `src/<pattern>/` with source, tests, and a
walkthrough README showing how Beamtalk's features express the pattern.

## Running the tests

```bash
beamtalk test
```

---

## Creational

### [Factory Method](src/factory_method/README.md)
Define an interface for creating an object, but let subclasses decide which
class to instantiate. Here `ShapeFactory` declares `createShape`; concrete
subclasses (`CircleFactory`, `RectangleFactory`) supply the product.

### [Builder](src/builder/README.md)
Separate the construction of a complex object from its representation.
`HtmlBuilder` assembles an `HtmlElement` step-by-step through a fluent
`withAttr:value:` / `withText:` / `build` chain.

### [Singleton](src/singleton/README.md)
Ensure a class has only one instance. In Beamtalk a spawned `Actor` is a
BEAM process with a unique PID — `AppLogger` demonstrates the pattern using
process identity rather than a class-level variable.

---

## Structural

### [Adapter](src/adapter/README.md)
Convert the interface of a class into another interface clients expect.
`CelsiusToFahrenheitAdapter` wraps a `CelsiusThermometer` and exposes the
`readFahrenheit` method that `FahrenheitThermometer` clients require.

### [Composite](src/composite/README.md)
Compose objects into tree structures to represent part-whole hierarchies.
`FsFile` (leaf) and `FsDir` (composite) both implement `FsEntry`, so callers
recurse through a directory tree without knowing which node type they hold.

### [Decorator](src/decorator/README.md)
Attach additional responsibilities to an object dynamically. Beverage
condiments (`Milk`, `Sugar`, `WhippedCream`) each wrap a `Beverage` and
delegate `description` and `cost`, adding their own contribution.

---

## Behavioural

### [Observer](src/observer/README.md)
Define a one-to-many dependency so that when one object changes state, all
its dependents are notified. `EventBus` maintains a subscriber list of
`EventCollector` actors; `notify:` delivers messages concurrently over BEAM.

### [Strategy](src/strategy/README.md)
Define a family of algorithms, encapsulate each one, and make them
interchangeable. `Sorter` holds a pluggable comparison block; `ascending`,
`descending`, and custom blocks are all valid strategies.

### [Command](src/command/README.md)
Encapsulate a request as an object, allowing parameterisation and undo.
`CommandHistory` snapshots the `TextBuffer` before each `InsertCommand` or
`DeleteCommand`, enabling unlimited undo without commands knowing how to
reverse themselves.

### [Template Method](src/template_method/README.md)
Define the skeleton of an algorithm in a base class, deferring some steps to
subclasses. `DataExporter#export:` is the fixed skeleton; `CsvExporter` and
`HtmlExporter` override `header`, `footer`, and `formatRow:`.
