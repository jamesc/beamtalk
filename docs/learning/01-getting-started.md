## Getting Started

Welcome to Beamtalk — a Smalltalk-inspired language that compiles to the
BEAM (the Erlang virtual machine). You get Smalltalk's elegant message-passing
object model on top of BEAM's battle-tested concurrency and fault-tolerance.

This chapter shows you the basics: the REPL, your first expressions, and how
to print output.

## Starting the REPL

Run the REPL with:

```
beamtalk repl
```

You'll see a prompt:

```
bt>
```

Type an expression and press Enter. The REPL evaluates it and prints the result.

## Your first expressions

Arithmetic works as you'd expect, with standard math precedence:

```beamtalk
3 + 4   // => 7
10 - 3  // => 7
2 * 6   // => 12
```

Division always returns a float:

```beamtalk
10 / 4  // => 2.5
```

## Printing output

`Transcript show:` prints a string to the console.
`Transcript show: "foo"; cr` prints and adds a newline.

In the REPL you'll mostly see return values directly, but in scripts
you'll use Transcript:

```text
Transcript show: "Hello, World!"; cr
Transcript show: "Hello, " ++ "Beamtalk!"; cr
```

## Everything is an object

In Beamtalk, *everything* is an object — integers, strings, booleans, even
classes themselves. You interact with objects by sending them messages.

`class` is a message that asks an object what class it belongs to:

```beamtalk
42 class     // => Integer
"hello" class  // => String
true class   // => True
3.14 class   // => Float
```

`printString` is a message that asks an object for its string representation:

```beamtalk
42 printString    // => 42
true printString  // => true
```

## Comments

Comments start with `//` and run to the end of the line.
There are no multi-line block comments.

```beamtalk
// This is a comment — the line below is code:
1 + 1  // => 2
```

## Exercises

**1. Explore object classes.** Send the `class` message to five different values
(an integer, a float, a string, a boolean, and `nil`). What class does each report?

<details>
<summary>Hint</summary>

```text
42 class       // => Integer
3.14 class     // => Float
"hello" class  // => String
true class     // => True
nil class      // => UndefinedObject
```
</details>

**2. Build a greeting.** Using string concatenation (`++`) and the `printString`
message, build the string `"I am 25 years old"` from the integer `25`.

<details>
<summary>Hint</summary>

```text
"I am " ++ 25 printString ++ " years old"    // => "I am 25 years old"
```

`printString` converts any value to its string representation.
</details>

**3. Chain unary messages.** What does `-42 abs class` evaluate to? Predict the
answer before running it, then verify in the REPL.

<details>
<summary>Hint</summary>

```text
-42 abs class    // => Integer
```

Unary messages chain left-to-right: `(-42 abs)` gives `42`, then `42 class`
gives `Integer`.
</details>

## What's next

Chapter 2 covers the built-in types: integers, floats, strings, booleans,
nil, symbols, and characters.
