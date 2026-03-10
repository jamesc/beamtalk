## Arithmetic & Comparison

Beamtalk uses standard math precedence (not Smalltalk's left-to-right).
This chapter covers all the numeric operators, integer vs float arithmetic,
and the equality/comparison operators — including some Beamtalk-specific ones.

## Integer arithmetic

```beamtalk
3 + 4   // => 7
10 - 6  // => 4
3 * 7   // => 21
```

Integer division always yields a Float:

```beamtalk
10 / 4  // => 2.5
10 / 2  // => 5.0
```

Truncating integer division:

```beamtalk
10 div: 3   // => 3
-7 div: 2   // => -3
```

Modulo with `%` (remainder, same sign as dividend):

```beamtalk
10 % 3   // => 1
-10 % 3  // => -1
```

Exponentiation (right-associative):

```beamtalk
2 ** 10        // => 1024
// Right-associative: 2 ** 3 ** 2 is the same as 2 ** (3 ** 2)
2 ** 3 ** 2    // => 512
```

## Float arithmetic

```beamtalk
3.14 + 2.0   // => 5.140000000000001
1.5 * 4.0    // => 6.0
7.5 / 2.5    // => 3.0
```

Mixed integer/float:

```beamtalk
3 + 1.5   // => 4.5
2 * 3.0   // => 6.0
```

Float-specific operations:

```beamtalk
3.7 floor      // => 3
3.2 ceiling    // => 4
3.7 truncated  // => 3
3.5 rounded    // => 4
3.14 isNaN     // => false
```

## Operator precedence (standard math)

Highest to lowest within binary messages:

```
**         exponentiation
* / %      multiplicative
+ - ++     additive
< > <= >=  comparison
= == =:=   equality (lowest binary)
```

```beamtalk
2 + 3 * 4    // => 14
10 - 2 * 3   // => 4
2 ** 3 + 1   // => 9
```

Parentheses override:

```beamtalk
(2 + 3) * 4  // => 20
```

## Comparison operators

```beamtalk
3 < 5   // => true
5 > 3   // => true
3 <= 3  // => true
4 >= 5  // => false
```

## Equality

Beamtalk has several equality operators:

- `=:=` value equality (works across Integer/Float)
- `==` value equality (same as `=:=`)
- `=/=` value inequality
- `/=` value inequality (same as `=/=`)
- `=` alias for `=:=` (legacy; lint warns on `x = true` / `x = false`)

```beamtalk
5 =:= 5      // => true
5 =:= 5.0    // => true
5 == 5.0     // => true
5 =/= 6      // => true
5 /= 5.0     // => false
```

Strings and symbols use `=:=`:

```beamtalk
"hello" =:= "hello"  // => true
#foo =:= #foo        // => true
#foo =:= #bar        // => false
```

Object identity (same object in memory):

```beamtalk
x := "hello"  // => _
x == x        // => true
```

## Numeric utility messages

```beamtalk
-5 abs    // => 5
5 negated  // => -5
3 max: 7   // => 7
3 min: 7   // => 3
```

Between (inclusive):

```beamtalk
5 between: 1 and: 10   // => true
15 between: 1 and: 10  // => false
```

GCD and LCM:

```beamtalk
12 gcd: 8  // => 4
4 lcm: 6   // => 12
```

Square root (returns Float):

```beamtalk
9 sqrt  // => 3.0
2 sqrt  // => 1.4142135623730951
```

## Converting between numeric types

```beamtalk
42 asFloat      // => 42.0
3.9 asInteger   // => 3
3.9 floor       // => 3
3.9 ceiling     // => 4
3.9 rounded     // => 4
3.1 rounded     // => 3
```

## Summary

- Arithmetic: `+  -  *  /  div:  %  **`
- Comparison: `<  >  <=  >=`
- Equality: `=:=  ==  =/=  /=`
- Utilities: `abs`, `negated`, `max:`, `min:`, `between:and:`, `gcd:`, `lcm:`, `sqrt`
- Precedence: `** > */% > +- > comparison > equality`

Key difference from classic Smalltalk: standard math precedence, not left-to-right.

Next: Chapter 6 — Strings
