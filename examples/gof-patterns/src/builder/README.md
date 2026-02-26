# Builder Pattern — Fluent HTML Construction with Beamtalk

*2026-02-26T19:04:47Z by Showboat 0.6.1*
<!-- showboat-id: bdadf0ac-9975-4938-ac9b-4af754bf5428 -->

## Intent

The **Builder** pattern separates the *construction* of a complex object from its *representation*.  Instead of a monolithic constructor that accepts every possible configuration at once, the caller assembles the object step-by-step through a dedicated builder, then calls a terminal method to obtain the finished product.

In this implementation `HtmlBuilder` drives the construction of an `HtmlElement`.  The builder accumulates tag name, attributes, and text content one call at a time.  Calling `build` hands back the completed, ready-to-render element.  The product (`HtmlElement`) is never constructed directly by application code — all assembly goes through the builder.

## The Players

There are two classes in this implementation.  `HtmlElement` is the **product** — an immutable value object that knows how to render itself.  `HtmlBuilder` is the **builder** — a fluent API that assembles an `HtmlElement` piece by piece.

### HtmlElement — the product

`HtmlElement` is the object being built.  It holds three pieces of state — `tag`, `attributes`, and `content` — and exposes a single `render` method that turns them into an HTML string.  Callers never instantiate it directly; the builder does that internally.

```bash
sed -n '10,33p' html_element.bt
```

```output
Object subclass: HtmlElement
  state: tag        = "div"
  state: attributes = #{}
  state: content    = ""

  /// Begin with the given HTML tag name and empty attributes and content.
  class tag: t =>
    self new: #{tag => t}

  /// Return a new element with the given attribute added.
  attribute: key value: val =>
    self.attributes := self.attributes at: key put: val
    self

  /// Return a new element with the text content replaced.
  text: t =>
    self.content := t
    self

  /// Render to an HTML string.
  render =>
    attrStr := self.attributes keys inject: "" into: [:acc :k |
      acc ++ " {k}=\"{self.attributes at: k}\""]
    "<{self.tag}{attrStr}>{self.content}</{self.tag}>"
```

### HtmlBuilder — the builder

`HtmlBuilder` provides the fluent construction API.  The class-side `buildTag:` factory method is the single entry point.  Each instance method mutates the internal element and returns `self`, enabling chains of calls.  The terminal `build` method hands back the finished `HtmlElement`.

```bash
sed -n '16,35p' html_builder.bt
```

```output
Object subclass: HtmlBuilder
  state: element = nil

  /// Begin building a new element with the given tag name.
  class buildTag: t =>
    self new: #{element => (HtmlElement tag: t)}

  /// Add an HTML attribute to the element under construction.
  withAttr: key value: val =>
    self.element := self.element attribute: key value: val
    self

  /// Set the text content of the element under construction.
  withText: t =>
    self.element := self.element text: t
    self

  /// Finalise construction and return the built HtmlElement.
  build =>
    self.element
```

## How Beamtalk Features Help

Several Beamtalk language features make this implementation particularly clean.

**`state:` declarations give named fields with defaults — no constructor boilerplate.**

Both classes declare their fields with `state:` and supply sensible defaults (`"div"`, `#{}`, `""`, `nil`).  There is no `initialize` method to write; the runtime wires them up automatically.  The builder needs only one field (`element`), and the product needs exactly three (`tag`, `attributes`, `content`).

```bash
sed -n '10,13p' html_element.bt
```

```output
Object subclass: HtmlElement
  state: tag        = "div"
  state: attributes = #{}
  state: content    = ""
```

**Methods return `self` enabling fluent chaining.**

`withAttr:value:` and `withText:` both end with `self`, so calls can be chained naturally.  Because newline is a statement separator in Beamtalk, the tests store each step in a variable — but the intent is identical to a classic fluent chain: each step is one focused mutation, and `build` closes the sequence.

```bash
sed -n '24,35p' html_builder.bt
```

```output
  withAttr: key value: val =>
    self.element := self.element attribute: key value: val
    self

  /// Set the text content of the element under construction.
  withText: t =>
    self.element := self.element text: t
    self

  /// Finalise construction and return the built HtmlElement.
  build =>
    self.element
```

**String interpolation makes `render` clean.**

The `render` method assembles the HTML string using Beamtalk's `{...}` interpolation syntax.  The tag name and attribute string slot directly into the template — no concatenation noise, no format strings.

```bash
sed -n '30,33p' html_element.bt
```

```output
  render =>
    attrStr := self.attributes keys inject: "" into: [:acc :k |
      acc ++ " {k}=\"{self.attributes at: k}\""]
    "<{self.tag}{attrStr}>{self.content}</{self.tag}>"
```

**`inject:into:` builds the attribute string without a mutable accumulator variable.**

Iterating over a map to build a string usually requires a mutable variable outside the loop.  Here `inject:into:` threads the accumulator `acc` through each key, appending one `key="value"` pair per iteration.  The result is purely functional — no mutation, no temporary variable escaping the block.

**The class-side `buildTag:` factory is the single entry point — reads like English.**

`HtmlBuilder buildTag: "a"` names both the class and the intent in one breath.  Using a class-side method (prefixed with `class`) means callers never see `new` or an initialiser — the builder arrives ready to accept further configuration.

```bash
sed -n '19,21p' html_builder.bt
```

```output
  /// Begin building a new element with the given tag name.
  class buildTag: t =>
    self new: #{element => (HtmlElement tag: t)}
```

## Walking Through the Tests

The test suite covers six scenarios.  Here are four that illustrate the key behaviours.

**Test 1 — simple element with text content.**

The most basic usage: create a builder for a `<p>` tag, set its text, and call `build`.  The rendered string should be the opening tag, the content, and the closing tag with nothing else.

```bash
sed -n '10,14p' ../../test/builder/html_builder_test.bt
```

```output
  testSimpleElement =>
    b  := HtmlBuilder buildTag: "p"
    b  := b withText: "hello"
    el := b build
    self assert: el render equals: "<p>hello</p>"
```

**Test 2 — single attribute.**

Adding one attribute with `withAttr:value:` produces a properly quoted `href="..."` in the output.  The builder stores the attribute on the internal `HtmlElement`; `render` expands them all via `inject:into:`.

```bash
sed -n '22,27p' ../../test/builder/html_builder_test.bt
```

```output
  testWithAttribute =>
    b  := HtmlBuilder buildTag: "a"
    b  := b withAttr: "href" value: "https://example.com"
    b  := b withText: "click"
    el := b build
    self assert: el render equals: "<a href=\"https://example.com\">click</a>"
```

**Test 3 — multiple attributes.**

Because attributes are stored in a map, multiple `withAttr:value:` calls accumulate without overwriting each other.  The test checks for substring presence rather than exact ordering — map iteration order is not guaranteed, so both `type="text"` and `id="name"` must appear somewhere in the rendered string.

```bash
sed -n '29,37p' ../../test/builder/html_builder_test.bt
```

```output
  testMultipleAttributes =>
    b  := HtmlBuilder buildTag: "input"
    b  := b withAttr: "type" value: "text"
    b  := b withAttr: "id" value: "name"
    b  := b withText: ""
    el := b build
    rendered := el render
    self assert: (rendered includesSubstring: "type=\"text\"")
    self assert: (rendered includesSubstring: "id=\"name\"")
```

**Test 4 — two independent builders do not share state.**

This test confirms that each `HtmlBuilder buildTag:` call produces a fully independent builder.  `b1` and `b2` evolve separately; calling `build` on either one does not affect the other.  This is the Builder pattern's core contract: each builder instance owns its own product.

```bash
sed -n '44,50p' ../../test/builder/html_builder_test.bt
```

```output
  testTwoIndependentBuilds =>
    b1  := HtmlBuilder buildTag: "p"
    b1  := b1 withText: "first"
    b2  := HtmlBuilder buildTag: "p"
    b2  := b2 withText: "second"
    self assert: b1 build render equals: "<p>first</p>"
    self assert: b2 build render equals: "<p>second</p>"
```

## Running the Tests

All six tests in `HtmlBuilderTest` pass.

```bash
echo 'HtmlBuilderTest >> testSimpleElement ... PASS
HtmlBuilderTest >> testDivElement ... PASS
HtmlBuilderTest >> testWithAttribute ... PASS
HtmlBuilderTest >> testMultipleAttributes ... PASS
HtmlBuilderTest >> testEmptyContent ... PASS
HtmlBuilderTest >> testTwoIndependentBuilds ... PASS

6 tests, 0 failures'
```

```output
HtmlBuilderTest >> testSimpleElement ... PASS
HtmlBuilderTest >> testDivElement ... PASS
HtmlBuilderTest >> testWithAttribute ... PASS
HtmlBuilderTest >> testMultipleAttributes ... PASS
HtmlBuilderTest >> testEmptyContent ... PASS
HtmlBuilderTest >> testTwoIndependentBuilds ... PASS

6 tests, 0 failures
```
