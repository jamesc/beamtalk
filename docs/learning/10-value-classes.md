<!-- btfixture: fixtures/point.bt -->
<!-- btfixture: fixtures/money.bt -->
<!-- btfixture: fixtures/ch10value_classes.bt -->
<!-- btfixture: fixtures/ch10money_example.bt -->

## Value Classes

A value class is an immutable object — like a record or struct. Once created,
its slots cannot change. "Mutations" return a new object with the change applied.

Use `Value subclass:` for data that travels between methods without needing
concurrency or identity. `Value subclass:` enforces immutability at compile
time — direct slot assignment (`self.x := ...`) is a compile error. Use
`Object subclass:` when you want the same immutable style with snapshot
semantics but are comfortable with runtime rather than compile-time enforcement.

Examples: coordinates, money amounts, colours, dates, config records.

## Defining a value class

```beamtalk
Value subclass: Point
  state: x = 0
  state: y = 0

  // Read-only access (auto-generated for each slot too, but you can override):
  x => self.x
  y => self.y

  // A method that computes from slots:
  distanceSquared => (self.x * self.x) + (self.y * self.y)

  // Methods return new instances for "mutation":
  translateBy: dx and: dy =>
    Point x: (self.x + dx) y: (self.y + dy)

  // String representation:
  printString => "Point({self.x}, {self.y})"
```

## Construction forms

Three ways to create a value object — all equivalent:

```beamtalk
TestCase subclass: Ch10ValueClasses

  testDefaultConstruction =>
    // new — all slots get declared defaults
    p := Point new
    self assert: p x equals: 0
    self assert: p y equals: 0

  testMapConstruction =>
    // new: with a map — missing keys keep defaults
    p := Point new: #{#x => 3, #y => 4}
    self assert: p x equals: 3
    self assert: p y equals: 4

  testKeywordConstruction =>
    // Keyword constructor — preferred, auto-generated from slot names
    p := Point x: 3 y: 4
    self assert: p x equals: 3
    self assert: p y equals: 4

  testPartialConstruction =>
    // Only provide some slots — rest use defaults
    p := Point new: #{#x => 7}
    self assert: p x equals: 7
    self assert: p y equals: 0
```

## Functional setters (with*:)

Each slot gets an auto-generated `withSlotName:` method.
It returns a NEW object with that one slot changed; the original is untouched.

```beamtalk
  testWithSetter =>
    p := Point x: 1 y: 2
    p2 := p withX: 10
    self assert: p2 x equals: 10
    self assert: p2 y equals: 2     // y preserved
    self assert: p x equals: 1      // original unchanged

  testChainedWithSetters =>
    p := Point new
    p2 := (p withX: 5) withY: 7
    self assert: p2 x equals: 5
    self assert: p2 y equals: 7
```

## Immutability

```beamtalk
  testImmutableByDefault =>
    // Direct slot mutation is a compile-time error inside Value subclass: methods.
    // At runtime, fieldAt:put: raises immutable_value:
    p := Point x: 1 y: 2
    self
      should: [p fieldAt: #x put: 99]
      raise: #immutable_value
```

## Value equality

Two value objects with the same class and slot values are equal (`=:=`).

```beamtalk
  testValueEquality =>
    p1 := Point x: 3 y: 4
    p2 := Point x: 3 y: 4
    self assert: p1 =:= p2

  testValueInequality =>
    p1 := Point x: 3 y: 4
    p2 := Point x: 9 y: 9
    self deny: p1 =:= p2

  testWithSetterEquality =>
    p := Point x: 1 y: 2
    self assert: (p withX: 10) =:= (Point x: 10 y: 2)
```

## Using methods

```beamtalk
  testDistanceSquared =>
    p := Point x: 3 y: 4
    self assert: p distanceSquared equals: 25

  testTranslate =>
    p := Point x: 1 y: 2
    p2 := p translateBy: 3 and: -1
    self assert: p2 x equals: 4
    self assert: p2 y equals: 1
    self assert: p x equals: 1      // original unchanged

  testPrintString =>
    p := Point x: 3 y: 4
    self assert: p printString equals: "Point(3, 4)"
```

## Value objects in collections

```beamtalk
  testValueObjectsInArray =>
    points := #[Point x: 1 y: 1, Point x: 2 y: 2, Point x: 3 y: 3]
    xs := points collect: [:p | p x]
    self assert: xs equals: #[1, 2, 3]

  testSelectOnValueObjects =>
    points := #[Point x: 1 y: 1, Point x: 2 y: 2, Point x: 3 y: 3]
    big := points select: [:p | p x > 1]
    self assert: big size equals: 2

  testInjectOnValueObjects =>
    points := #[Point x: 1 y: 0, Point x: 2 y: 0, Point x: 3 y: 0]
    sumX := points inject: 0 into: [:acc :p | acc + p x]
    self assert: sumX equals: 6
```

## Reflection

```beamtalk
  testFieldAccess =>
    p := Point x: 3 y: 4
    self assert: (p fieldAt: #x) equals: 3
    self assert: (p fieldAt: #y) equals: 4

  testClass =>
    p := Point x: 3 y: 4
    self assert: p class equals: Point

  testSuperclass =>
    self assert: Point superclass equals: Value
```

## A more real-world example: Money

```beamtalk
Value subclass: Money
  state: amount = 0
  state: currency = #USD

  add: other =>
    other currency =:= self.currency
      ifFalse: [^self error: "Currency mismatch"]
    Money amount: (self.amount + other amount) currency: self.currency

  printString => "{self.amount} {self.currency}"

TestCase subclass: Ch10MoneyExample

  testMoneyAddition =>
    a := Money amount: 10 currency: #USD
    b := Money amount: 5 currency: #USD
    c := a add: b
    self assert: c amount equals: 15
    self assert: c currency equals: #USD

  testMoneyImmutable =>
    a := Money amount: 10 currency: #USD
    b := Money amount: 5 currency: #USD
    a add: b
    self assert: a amount equals: 10   // unchanged

  testMoneyPrintString =>
    m := Money amount: 42 currency: #EUR
    self assert: m printString equals: "42 EUR"
```
