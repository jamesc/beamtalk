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
`Transcript showLine:` prints and adds a newline.

In the REPL you'll mostly see return values directly, but in scripts
you'll use Transcript:

```text
Transcript showLine: "Hello, World!"
Transcript showLine: "Hello, " ++ "Beamtalk!"
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

## What's next

Chapter 2 covers the built-in types: integers, floats, strings, booleans,
nil, symbols, and characters.
