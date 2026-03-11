<!-- btfixture: fixtures/point.bt -->
<!-- btfixture: fixtures/ch24reflection.bt -->

## Reflection & Metaprogramming

Beamtalk provides rich reflection capabilities: you can inspect any object's
class, query its methods, read and write fields, and navigate the class
hierarchy — all at runtime.

## Querying an object's class

Every value in Beamtalk knows its class:

```beamtalk
42 class          // => Integer
"hello" class     // => String
true class        // => True
#(1, 2) class     // => List
#foo class        // => Symbol
nil class         // => UndefinedObject
```

## Testing for message support

`respondsTo:` checks whether an object understands a particular message:

```beamtalk
"hello" respondsTo: #size        // => true
"hello" respondsTo: #factorial   // => false
42 respondsTo: #factorial        // => true
42 respondsTo: #size             // => false
```

Works with variables holding symbols too:

```beamtalk
msg := #size                    // => _
"hello" respondsTo: msg         // => true
```

## Reading fields

### fieldNames

`fieldNames` returns the list of an object's fields:

```beamtalk
TestCase subclass: Ch24FieldNames
  testValueFields =>
    p := Point x: 3 y: 4
    names := p fieldNames
    self assert: (names includes: #x)
    self assert: (names includes: #y)
```

### fieldAt:

Read a field by name:

```beamtalk
TestCase subclass: Ch24FieldAccess
  testReadField =>
    p := Point x: 10 y: 20
    self assert: (p fieldAt: #x) equals: 10
    self assert: (p fieldAt: #y) equals: 20
```

Primitives are immutable — attempting field access raises an error:

```beamtalk
TestCase subclass: Ch24FieldErrors
  testPrimitiveFieldAccess =>
    self should: [42 fieldAt: #x] raise: #immutable_value
    self should: ["hello" fieldAt: #x] raise: #immutable_value
```

### fieldAt:put: (actors only)

Actors support mutable field updates:

```beamtalk
// In the REPL:
// c := Counter spawn
// c fieldAt: #count put: 42
// c fieldAt: #count          → 42
```

Value objects return a new copy with the updated field instead.

## Dynamic dispatch with perform:

`perform:` sends a message by symbol name:

```beamtalk
TestCase subclass: Ch24Perform
  testPerform =>
    p := Point x: 5 y: 10
    result := p perform: #x
    self assert: result equals: 5
```

## Class introspection

### Listing methods

```beamtalk
TestCase subclass: Ch24ClassMethods
  testMethods =>
    methods := Point methods
    self assert: (methods includes: #x)
    self assert: (methods includes: #y)

  testAllMethods =>
    // allMethods includes inherited methods too
    all := Point allMethods
    self assert: (all includes: #x)
    self assert: (all includes: #class)
```

### Superclass chain

```beamtalk
TestCase subclass: Ch24Superclass
  testSuperclass =>
    self assert: Point superclass equals: Value
```

### Class-side respondsTo:

Classes themselves respond to messages:

```beamtalk
TestCase subclass: Ch24ClassRespondsTo
  testClassRespondsTo =>
    self assert: (Point respondsTo: #new)
    self assert: (Point respondsTo: #methods)
```

## The metaclass hierarchy

Every class has a **metaclass** — the class of the class. This parallel
hierarchy mirrors the instance side:

```beamtalk
TestCase subclass: Ch24Metaclass
  testMetaclass =>
    meta := Point class
    self assert: meta isMeta
    self deny: meta isClass

  testMetaclassName =>
    name := Point class name
    self assert: (name includesSubstring: "class")

  testSelfGrounding =>
    // The metaclass of a metaclass is itself — the hierarchy is self-grounding
    m1 := Point class class
    m2 := Point class class class
    self assert: m1 equals: m2
```

## Practical example: generic inspector

Reflection enables generic tools that work with any object:

```beamtalk
TestCase subclass: Ch24Inspector
  testInspectValue =>
    p := Point x: 5 y: 10

    // Build a description using reflection
    self assert: p class equals: Point
    self assert: (p fieldNames includes: #x)
    self assert: (p fieldAt: #x) equals: 5
```

## Summary

**Object introspection:**

```text
obj class                    → the object's class
obj respondsTo: #selector    → Boolean
obj fieldNames               → List of field name Symbols
obj fieldAt: #name           → field value
obj fieldAt: #name put: val  → update field (actors) or return new copy (values)
obj perform: #selector       → dynamic message send
```

**Class reflection:**

```text
MyClass methods              → List of local method selectors
MyClass allMethods           → List including inherited methods
MyClass superclass           → parent class
MyClass respondsTo: #sel     → Boolean (class-side method)
```

**Metaclass:**

```text
MyClass class         → metaclass
metaclass isMeta      → true
metaclass isClass     → false
metaclass name        → "MyClass class"
metaclass thisClass   → MyClass
```

Next: Chapter 25 — HTTP Client
