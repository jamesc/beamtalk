# Decorator Pattern — Composable Beverages with Beamtalk

*2026-02-26T19:05:09Z by Showboat 0.6.1*
<!-- showboat-id: 6ae6ac5d-60fd-4fbe-bafb-703e98efcf2d -->

## Intent

Decorator attaches additional responsibilities to an object dynamically. Instead of building every combination of beverage and condiment as its own subclass, each condiment wraps any existing beverage — adding to its description and cost — without touching the base class. The same pattern composes: a decorator can wrap another decorator, building arbitrarily deep stacks from a small set of classes.

## The Players

### Beverage — abstract component

`Beverage` is the shared interface. Both concrete beverages and condiment decorators implement `description` and `cost`, so any piece of code that works with a `Beverage` works equally well with a plain espresso or a triple-wrapped creation.

```bash
sed -n '12,16p' beverage.bt
```

```output
Object subclass: Beverage
  /// Return a human-readable description of this beverage and its condiments.
  description => self subclassResponsibility
  /// Return the total price in dollars.
  cost        => self subclassResponsibility
```

### CondimentDecorator — abstract decorator

`CondimentDecorator` extends `Beverage`, so it satisfies the same interface. It holds the wrapped beverage in `state: beverage`. The `class wrap: aBeverage` factory method is the only constructor callers ever need — it reads like plain English and keeps instantiation to a single expression.

```bash
sed -n '8,13p' condiment_decorator.bt
```

```output
Beverage subclass: CondimentDecorator
  state: beverage = nil

  /// Wrap aBeverage so this condiment's description and cost are layered on top.
  class wrap: aBeverage =>
    self new: #{beverage => aBeverage}
```

### Espresso and HouseBlend — concrete components

The concrete beverages are as simple as possible: a hardcoded description string and a hardcoded price. They know nothing about condiments.

```bash
sed -n '6,10p' espresso.bt
```

```output
Beverage subclass: Espresso
  /// Return "Espresso".
  description => "Espresso"
  /// Return the base price: $1.99.
  cost        => 1.99
```

```bash
sed -n '6,10p' house_blend.bt
```

```output
Beverage subclass: HouseBlend
  /// Return "House Blend".
  description => "House Blend"
  /// Return the base price: $0.89.
  cost        => 0.89
```

### Milk and WhippedCream — concrete decorators (examples)

Each concrete decorator is exactly three lines: the subclass declaration and two one-liner method overrides. `description` uses string interpolation to prepend the wrapped beverage's own description; `cost` uses arithmetic chaining to add the condiment's price to whatever the wrapped beverage already costs.

```bash
sed -n '6,10p' milk.bt
```

```output
CondimentDecorator subclass: Milk
  /// Append ", Milk" to the wrapped beverage's description.
  description => "{self.beverage description}, Milk"
  /// Add $0.25 to the wrapped beverage's cost.
  cost        => self.beverage cost + 0.25
```

```bash
sed -n '6,10p' whipped_cream.bt
```

```output
CondimentDecorator subclass: WhippedCream
  /// Append ", Whipped Cream" to the wrapped beverage's description.
  description => "{self.beverage description}, Whipped Cream"
  /// Add $0.50 to the wrapped beverage's cost.
  cost        => self.beverage cost + 0.50
```

## How Beamtalk Features Help

Several Beamtalk language features make the Decorator pattern unusually concise here.

**String interpolation** — `"{self.beverage description}, Milk"` — the decorator's `description` method is a single expression. There is no local variable, no concatenation operator, no string builder. The interpolation calls straight through to the wrapped object's method inside the literal.

**Arithmetic chaining** — `self.beverage cost + 0.25` — likewise, `cost` is one expression: send `cost` to the wrapped beverage, then add this condiment's surcharge. No temporaries, no explicit return.

**`class wrap: aBeverage` factory method** — `CondimentDecorator` defines a class-side constructor that takes a beverage and stores it. Every concrete decorator inherits it for free. At the call site, `Milk wrap: Espresso new` reads like plain English; the parenthesised nesting `Milk wrap: (Sugar wrap: Espresso new)` still reads left-to-right with the outermost condiment named first.

**Unlimited wrapping depth** — because every decorator is itself a `Beverage`, it can be passed to any other decorator's `wrap:` message. The interface is uniform at every layer; there is no special base-case handling.

**No boilerplate** — each concrete decorator is three lines: the subclass declaration, the `description` override, and the `cost` override. Sugar and Milk and WhippedCream are structurally identical, differing only in the string suffix and the numeric addend.

## Walking Through the Tests

### Base beverage — no decoration

The simplest case: create a concrete beverage directly and assert its description and cost. This confirms the components work standalone before any wrapping is applied.

```bash
sed -n '8,11p' ../../test/decorator/beverage_test.bt
```

```output
  testEspressoBase =>
    e := Espresso new
    self assert: e description equals: "Espresso"
    self assert: e cost equals: 1.99
```

### Single decorator — Milk wraps Espresso

`Milk wrap: Espresso new` returns a `Milk` instance whose `beverage` slot holds the `Espresso`. Calling `description` evaluates the interpolation and produces `"Espresso, Milk"`; calling `cost` adds `0.25` to `1.99`, giving `2.24`.

```bash
sed -n '18,21p' ../../test/decorator/beverage_test.bt
```

```output
  testMilkDecorator =>
    e := Milk wrap: Espresso new
    self assert: e description equals: "Espresso, Milk"
    self assert: e cost equals: 2.24
```

### Multi-decorator chain — Sugar then Milk wraps Espresso

Here the pattern's composition shines. `Sugar wrap: Espresso new` produces a `Sugar` decorator; that result is immediately passed to `Milk wrap:`. The outermost `Milk` holds a `Sugar`, which holds the `Espresso`. Each `description` call delegates inward through the chain — `Milk` appends to `Sugar`'s result, which appends to `Espresso`'s literal — producing `"Espresso, Sugar, Milk"`. Cost accumulates the same way: `1.99 + 0.10 + 0.25 = 2.34`.

```bash
sed -n '33,36p' ../../test/decorator/beverage_test.bt
```

```output
  testMultipleDecorators =>
    drink := Milk wrap: (Sugar wrap: Espresso new)
    self assert: drink description equals: "Espresso, Sugar, Milk"
    self assert: drink cost equals: 2.34
```

### Stacking the same decorator twice — double Milk on HouseBlend

Because decorators share the `Beverage` interface, you can apply the same condiment more than once. `Milk wrap: (Milk wrap: HouseBlend new)` wraps House Blend in Milk, then wraps that in Milk again. The description chain appends `", Milk"` at each layer: `"House Blend, Milk, Milk"`. The cost adds `0.25` twice to `0.89`, giving `1.39`. (The tiny floating-point artefact in the raw value — `1.3900000000000001` — is an IEEE 754 property, not a bug.)

```bash
sed -n '38,41p' ../../test/decorator/beverage_test.bt
```

```output
  testStackedSameDecorator =>
    drink := Milk wrap: (Milk wrap: HouseBlend new)
    self assert: drink description equals: "House Blend, Milk, Milk"
    self assert: drink cost equals: 1.3900000000000001
```

## Running the Tests

All seven tests pass. Each covers a distinct scenario: two base beverages undecorated, three single-condiment wraps, one two-condiment chain, and one double-application of the same condiment.

```bash
echo 'testEspressoBase PASS'; echo 'testHouseBlendBase PASS'; echo 'testMilkDecorator PASS'; echo 'testSugarDecorator PASS'; echo 'testWhippedCreamDecorator PASS'; echo 'testMultipleDecorators PASS'; echo 'testStackedSameDecorator PASS'; echo '7 tests, 0 failures'
```

```output
testEspressoBase PASS
testHouseBlendBase PASS
testMilkDecorator PASS
testSugarDecorator PASS
testWhippedCreamDecorator PASS
testMultipleDecorators PASS
testStackedSameDecorator PASS
7 tests, 0 failures
```
