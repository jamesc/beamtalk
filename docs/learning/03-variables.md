## Variables

Variables in Beamtalk are simple: assign with `:=`, use the name, and they
go out of scope at the end of the method or script. No type declarations,
no `var`/`let`/`const` keywords.

## Assignment with `:=`

`:=` is the assignment operator. The name on the left, the value on the right.

```beamtalk
x := 42            // => _
x                  // => 42

greeting := "Hello, World!"  // => _
greeting                     // => Hello, World!
```

Variables hold any type — there are no type declarations:

```beamtalk
result := 3 + 4  // => _
result           // => 7
```

Reassignment is allowed:

```beamtalk
count := 1            // => _
count := count + 1    // => _
count                 // => 2
```

## No declarations needed

Unlike Smalltalk (which requires `| var1 var2 |` declarations at the top of
a method), Beamtalk introduces variables at the point of first assignment.

This just works — no declaration block needed:

```beamtalk
a := 10   // => _
b := 20   // => _
a + b     // => 30
```

## Multiple assignments

Assign multiple variables separately, then combine them:

```beamtalk
first := 1           // => _
second := 2          // => _
third := 3           // => _
first + second + third  // => 6
```

## Destructuring assignment

Arrays can be destructured into multiple variables in one step:

```beamtalk
#[x, y] := #[10, 20]  // => _
x                       // => 10
y                       // => 20
```

Works with any expression that evaluates to an array:

```beamtalk
#[a, b, c] := #[1, 2, 3]  // => _
a + b + c                   // => 6
```

## Variables in expressions

Variables are just names for values. Use them anywhere a value is expected:

```beamtalk
width := 5          // => _
height := 3         // => _
area := width * height  // => _
area                // => 15
```

String interpolation uses variables directly:

```beamtalk
name := "Beamtalk"       // => _
"Welcome to {name}!"     // => Welcome to Beamtalk!
```

## Instance variables (slots) — a preview

Inside a class definition, instance variables are accessed via `self.name`.
This chapter covers only local (temporary) variables.
Chapter 10 (Value Classes) and Chapter 11 (Actors) cover instance variables.

The key distinction:

- `x := 42` — local variable, lives in the current method/script
- `self.x := 42` — slot assignment on an object; valid for `Actor subclass:` and `Object subclass:`, but rejected at compile time for `Value subclass:` (values are immutable)

For now, all variables we've seen are local.

## Naming conventions

- Variable names: `lowerCamelCase` — `count`, `firstName`, `totalPrice`
- Class names: `UpperCamelCase` — `Integer`, `String`, `Counter`, `ShoppingCart`
- Symbols used as constants: `#lowerCamelCase` or `#UPPER_SNAKE` (both seen in stdlib)

Beamtalk is case-sensitive:

```beamtalk
myVar := "hello"  // => _
myVar             // => hello
```

## Summary

```
x := expr        assign (introduce or rebind)
x                read
#[a, b] := arr   destructure
```

Next: Chapter 4 — Messages (the fundamental Beamtalk concept)
