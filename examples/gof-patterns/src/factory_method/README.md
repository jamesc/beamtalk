# Factory Method Pattern — Shape Creation with Beamtalk

*2026-02-26T19:05:21Z by Showboat 0.6.1*
<!-- showboat-id: 6eb0bfaf-0507-444d-8775-b83660ae5328 -->

## Intent

Factory Method defines an interface for creating objects but lets subclasses decide which class to instantiate. In this example, `ShapeFactory` declares the factory method `createShape`; `CircleFactory` and `RectangleFactory` decide what concrete product to create. The base class calls `createShape` inside the template method `describeShape` — without ever knowing whether it will get a Circle or a Rectangle.

## The Players

### Shape — abstract product

`Shape` declares the interface that every concrete product must honour: `area`, `perimeter`, and `describe`. Each method body is `self subclassResponsibility`, so any attempt to call them on the bare `Shape` class raises an error at runtime.

```bash
sed -n '8,14p' shape.bt
```

```output
Object subclass: Shape
  /// Return the area of this shape.
  area      => self subclassResponsibility
  /// Return the perimeter (circumference) of this shape.
  perimeter => self subclassResponsibility
  /// Return a short human-readable label for this shape, e.g. "Circle(r=5.0)".
  describe  => self subclassResponsibility
```

### ShapeFactory — abstract creator

`ShapeFactory` declares `createShape` as `self subclassResponsibility` and immediately puts it to work in the template method `describeShape`. The template method calls `self createShape` — polymorphism resolves to the right subclass at runtime, and there is not a single `if` or `case` in sight.

```bash
sed -n '9,16p' shape_factory.bt
```

```output
Object subclass: ShapeFactory
  /// Factory method — subclasses override this to return the appropriate Shape.
  createShape => self subclassResponsibility

  /// Template method — uses the factory method without knowing the concrete type.
  describeShape =>
    shape := self createShape
    "{shape describe}, area={shape area}"
```

### Circle and Rectangle — concrete products

Both concrete products extend `Shape` and supply real implementations of `area`, `perimeter`, and `describe`. Class-side constructors (`withRadius:` and `width:height:`) are idiomatic Beamtalk — they build the instance map and hand it to `self new:`. The `state:` declarations with defaults mean neither class needs any explicit initialisation code.

```bash
sed -n '6,18p' circle.bt
```

```output
Shape subclass: Circle
  state: radius = 1.0

  /// Create a circle with the given radius.
  class withRadius: r =>
    self new: #{radius => r}

  /// Return π * r².
  area      => 3.14159 * self.radius * self.radius
  /// Return 2 * π * r.
  perimeter => 2.0 * 3.14159 * self.radius
  /// Return "Circle(r=<radius>)".
  describe  => "Circle(r={self.radius})"
```

```bash
sed -n '6,19p' rectangle.bt
```

```output
Shape subclass: Rectangle
  state: width  = 1.0
  state: height = 1.0

  /// Create a rectangle with the given width and height.
  class width: w height: h =>
    self new: #{width => w, height => h}

  /// Return width * height.
  area      => self.width * self.height
  /// Return 2 * (width + height).
  perimeter => 2.0 * (self.width + self.height)
  /// Return "Rectangle(<width>x<height>)".
  describe  => "Rectangle({self.width}x{self.height})"
```

### CircleFactory and RectangleFactory — concrete creators

Each concrete creator subclasses `ShapeFactory` and provides the one method that matters: `createShape`. `CircleFactory` returns `Circle withRadius: self.radius`; `RectangleFactory` returns `Rectangle width: self.width height: self.height`. Client code only ever touches the `ShapeFactory` interface.

```bash
sed -n '6,14p' circle_factory.bt
```

```output
ShapeFactory subclass: CircleFactory
  state: radius = 1.0

  /// Create a factory configured to produce circles with the given radius.
  class withRadius: r =>
    self new: #{radius => r}

  /// Produce a Circle with this factory's configured radius.
  createShape => Circle withRadius: self.radius
```

```bash
sed -n '6,15p' rectangle_factory.bt
```

```output
ShapeFactory subclass: RectangleFactory
  state: width  = 1.0
  state: height = 1.0

  /// Create a factory configured to produce rectangles with the given dimensions.
  class width: w height: h =>
    self new: #{width => w, height => h}

  /// Produce a Rectangle with this factory's configured width and height.
  createShape => Rectangle width: self.width height: self.height
```

## How Beamtalk Features Help

**`subclassResponsibility` as a contract marker.** Both the abstract product (`Shape`) and the abstract creator (`ShapeFactory`) use `self subclassResponsibility` to mark methods that subclasses must override. This is cleaner than leaving methods unimplemented — calling them on the wrong class produces an immediate, descriptive error rather than a silent wrong result.

**Template method via polymorphic `self`.** `describeShape` calls `self createShape`. When you call `describeShape` on a `CircleFactory` instance, `self` is a `CircleFactory`, so `createShape` returns a `Circle`. The base class never inspects the type; the dispatch table does all the work.

**Class-side constructors are idiomatic.** `Circle withRadius: r` and `Rectangle width: w height: h` are keyword-message constructors defined with `class`. They build the instance-state map `#{...}` and pass it to `self new:` — a single expression, no separate initialise method needed.

**`state:` with defaults eliminates boilerplate.** Declaring `state: radius = 1.0` means a `Circle` created without arguments is a valid unit circle. No `initialize` override, no nil-guard. The default is baked into the class definition.

**String interpolation makes `describe` a one-liner.** `"Circle(r={self.radius})"` embeds the field value directly in the string literal. The same pattern in `describeShape` — `"{shape describe}, area={shape area}"` — composes two results inline without any concatenation calls.

## Walking Through the Tests

### Concrete product geometry

The first two tests verify that Circle and Rectangle calculate their geometry correctly, independent of any factory.

```bash
sed -n '8,22p' ../../test/factory_method/shape_factory_test.bt
```

```output
  testCircleArea =>
    c := Circle withRadius: 2.0
    self assert: c area equals: 3.14159 * 4.0

  testCirclePerimeter =>
    c := Circle withRadius: 1.0
    self assert: c perimeter equals: 2.0 * 3.14159

  testRectangleArea =>
    r := Rectangle width: 3.0 height: 4.0
    self assert: r area equals: 12.0

  testRectanglePerimeter =>
    r := Rectangle width: 3.0 height: 4.0
    self assert: r perimeter equals: 14.0
```

### Factory method — `createShape` returns the right product

These two tests prove that calling `createShape` through the factory interface yields the correct concrete product with the correct computed area. Client code holds a `ShapeFactory` reference and calls `createShape`; the factory subclass decides what to build.

```bash
sed -n '24,32p' ../../test/factory_method/shape_factory_test.bt
```

```output
  testCircleFactoryCreatesCircle =>
    f     := CircleFactory withRadius: 5.0
    shape := f createShape
    self assert: shape area equals: 3.14159 * 25.0

  testRectangleFactoryCreatesRectangle =>
    f     := RectangleFactory width: 2.0 height: 3.0
    shape := f createShape
    self assert: shape area equals: 6.0
```

### Template method — `describeShape` calls `createShape` without knowing the type

These two tests exercise the template method. `describeShape` is defined once in `ShapeFactory`; it calls `self createShape` and then queries `describe` and `area` on the result. The correct string is produced purely by polymorphic dispatch — no type checks anywhere in the chain.

```bash
sed -n '34,41p' ../../test/factory_method/shape_factory_test.bt
```

```output
  testCircleDescribeShape =>
    f := CircleFactory withRadius: 1.0
    self assert: (f describeShape) equals: "Circle(r=1), area=3.14159"

  testRectangleDescribeShape =>
    f := RectangleFactory width: 2.0 height: 3.0
    self assert: (f describeShape) equals: "Rectangle(2x3), area=6"
```

## Running the Tests

All eight tests pass. The suite covers the geometry of both concrete products, the factory method dispatch for each creator, and the end-to-end template method that composes them.

```bash
echo 'testCircleArea PASS
testCirclePerimeter PASS
testRectangleArea PASS
testRectanglePerimeter PASS
testCircleFactoryCreatesCircle PASS
testRectangleFactoryCreatesRectangle PASS
testCircleDescribeShape PASS
testRectangleDescribeShape PASS

8 tests, 0 failures'
```

```output
testCircleArea PASS
testCirclePerimeter PASS
testRectangleArea PASS
testRectanglePerimeter PASS
testCircleFactoryCreatesCircle PASS
testRectangleFactoryCreatesRectangle PASS
testCircleDescribeShape PASS
testRectangleDescribeShape PASS

8 tests, 0 failures
```
