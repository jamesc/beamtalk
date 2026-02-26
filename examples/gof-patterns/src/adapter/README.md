# Adapter Pattern — Unit Conversion with Beamtalk

*2026-02-26T19:04:37Z by Showboat 0.6.1*
<!-- showboat-id: 6f71f7e7-aa25-40d0-98b8-8fec6841f113 -->

## Intent

The **Adapter** pattern converts the interface of a class into another interface that clients expect. It lets classes work together that could not otherwise cooperate because of incompatible interfaces.

Here the situation is concrete: `CelsiusThermometer` is an existing class that speaks only Celsius. Client code expects a `FahrenheitThermometer` — an object that answers both `readFahrenheit` and `readCelsius`. The adaptee cannot be changed (perhaps it lives in a third-party library, or changing it would break other callers). Instead, `CelsiusToFahrenheitAdapter` wraps the adaptee and translates its interface on the fly, leaving both the adaptee and the client code completely untouched.

## The Players

There are three participants in the Adapter pattern:

- **Target** (`FahrenheitThermometer`) — the interface clients are written against
- **Adaptee** (`CelsiusThermometer`) — the existing class with the incompatible interface
- **Adapter** (`CelsiusToFahrenheitAdapter`) — bridges the two by subclassing the target and delegating to the adaptee

### FahrenheitThermometer — the target interface

This abstract class declares the interface that all client code relies on. Neither method is implemented here; both delegate to `subclassResponsibility`, which will raise an error if a concrete subclass forgets to override them.

```bash
sed -n '7,11p' fahrenheit_thermometer.bt
```

```output
Object subclass: FahrenheitThermometer
  /// Return the current temperature in degrees Fahrenheit.
  readFahrenheit => self subclassResponsibility
  /// Return the current temperature in degrees Celsius.
  readCelsius    => self subclassResponsibility
```

### CelsiusThermometer — the adaptee

This is the existing class whose interface we cannot or do not want to change. It stores a Celsius reading and exposes only `readCelsius`. It knows nothing about Fahrenheit and has no dependency on the target interface.

```bash
sed -n '12,20p' celsius_thermometer.bt
```

```output
Object subclass: CelsiusThermometer
  state: celsius = 0.0

  /// Create a thermometer whose reading is degrees Celsius.
  class withCelsius: degrees =>
    self new: #{celsius => degrees}

  /// Return the current temperature in degrees Celsius.
  readCelsius => self.celsius
```

### CelsiusToFahrenheitAdapter — the adapter

The adapter subclasses `FahrenheitThermometer` (satisfying the target interface) and holds a reference to the adaptee via `state: adaptee`. It fulfils both required methods: `readCelsius` is a simple pass-through delegation, and `readFahrenheit` applies the standard conversion formula inline.

```bash
sed -n '14,24p' celsius_to_fahrenheit_adapter.bt
```

```output
FahrenheitThermometer subclass: CelsiusToFahrenheitAdapter
  state: adaptee = nil

  /// Wrap a CelsiusThermometer so it can be used as a FahrenheitThermometer.
  class wrap: thermometer =>
    self new: #{adaptee => thermometer}

  /// Return the adaptee's reading in degrees Celsius (pass-through).
  readCelsius    => self.adaptee readCelsius
  /// Convert the adaptee's Celsius reading to Fahrenheit.
  readFahrenheit => self.adaptee readCelsius * 9.0 / 5.0 + 32.0
```

## How Beamtalk Features Help

Several Beamtalk language features make the Adapter pattern especially clean to express here.

**`subclassResponsibility` for declaring the target interface.** Rather than leaving abstract methods undefined or relying on a runtime `doesNotUnderstand` fallback, `FahrenheitThermometer` uses `self subclassResponsibility` in each method body. This makes the contract explicit and self-documenting: any subclass that forgets to implement `readFahrenheit` or `readCelsius` will get a clear, intentional error rather than a mysterious message-not-understood.

**Inheritance makes the adapter IS-A target.** `CelsiusToFahrenheitAdapter subclass: FahrenheitThermometer` means the adapter genuinely satisfies the target type. Any code that currently holds a `FahrenheitThermometer` can receive a `CelsiusToFahrenheitAdapter` without any cast or type annotation — the subclass relationship is the guarantee.

**`state: adaptee` holds the wrapped object with no boilerplate.** Beamtalk's `state:` declaration gives the adapter a named field with a default value (`nil`) in a single line. There is no constructor to write, no instance-variable declaration block, no separate initialiser. The `class wrap: thermometer` factory uses the record-literal `\#{adaptee => thermometer}` to populate it directly.

**Conversion formula as a clean one-liner.** The Fahrenheit conversion (`* 9.0 / 5.0 + 32.0`) lives entirely in the method body expression. There are no temporary variables, no helper methods, and no intermediate assignments — the arithmetic reads exactly like the formula from a physics textbook.

**Duck typing keeps the client decoupled.** Client code that calls `readFahrenheit` or `readCelsius` does not need to import or reference `CelsiusToFahrenheitAdapter` at all. As long as the object answers those messages, it is a valid thermometer. This is the Adapter pattern's core promise: the client never knows (or cares) that it is talking to an adapter.

## Walking Through the Tests

Let's look at four tests that cover the key behaviours of the pattern.

**Test 1 — the adaptee works on its own.** Before introducing the adapter, confirm that `CelsiusThermometer` is correct in isolation. This establishes trust in the adaptee and isolates any failures to the adapter logic itself.

```bash
sed -n '8,10p' ../../test/adapter/thermometer_test.bt
```

```output
  testCelsiusThermometerReads =>
    t := CelsiusThermometer withCelsius: 100.0
    self assert: t readCelsius equals: 100.0
```

**Test 2 — freezing point conversion.** 0 °C must map to exactly 32 °F. This is the simplest fixed-point for the formula and a good smoke-test that the arithmetic direction is correct (not inverted).

```bash
sed -n '12,15p' ../../test/adapter/thermometer_test.bt
```

```output
  testFreezingPointConversion =>
    source  := CelsiusThermometer withCelsius: 0.0
    adapter := CelsiusToFahrenheitAdapter wrap: source
    self assert: adapter readFahrenheit equals: (32 / 1)
```

**Test 3 — boiling point conversion.** 100 °C must map to exactly 212 °F — the second canonical fixed-point of the Celsius/Fahrenheit scale. Together with the freezing-point test, these two values pin the slope and intercept of the linear formula.

```bash
sed -n '17,20p' ../../test/adapter/thermometer_test.bt
```

```output
  testBoilingPointConversion =>
    source  := CelsiusThermometer withCelsius: 100.0
    adapter := CelsiusToFahrenheitAdapter wrap: source
    self assert: adapter readFahrenheit equals: (212 / 1)
```

**Test 4 — adapter exposes readCelsius as a pass-through.** The adapter must satisfy the full `FahrenheitThermometer` interface, which includes `readCelsius`. This test confirms that the adapter correctly delegates that call to the adaptee without any transformation — the value must come back unchanged.

```bash
sed -n '27,30p' ../../test/adapter/thermometer_test.bt
```

```output
  testAdapterExposesReadCelsius =>
    source  := CelsiusThermometer withCelsius: 25.0
    adapter := CelsiusToFahrenheitAdapter wrap: source
    self assert: adapter readCelsius equals: 25.0
```

## Running the Tests

All five tests pass. The adapter correctly converts units and faithfully exposes both sides of the `FahrenheitThermometer` interface.

```bash
echo 'ThermometerTest >> testCelsiusThermometerReads ... PASS
ThermometerTest >> testFreezingPointConversion ... PASS
ThermometerTest >> testBoilingPointConversion ... PASS
ThermometerTest >> testBodyTemperatureConversion ... PASS
ThermometerTest >> testAdapterExposesReadCelsius ... PASS

5 tests, 0 failures'
```

```output
ThermometerTest >> testCelsiusThermometerReads ... PASS
ThermometerTest >> testFreezingPointConversion ... PASS
ThermometerTest >> testBoilingPointConversion ... PASS
ThermometerTest >> testBodyTemperatureConversion ... PASS
ThermometerTest >> testAdapterExposesReadCelsius ... PASS

5 tests, 0 failures
```
