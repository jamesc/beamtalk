## Glossary

A reference of key Beamtalk terms and concepts. Each entry links back to the
chapter where the concept is introduced.

---

**Actor** — A concurrent object that runs as an independent BEAM process with its
own mutable state and mailbox. Defined with `Actor subclass:`. Actors communicate
via message passing (synchronous calls or asynchronous casts). See
[Chapter 11](11-actors.md).

**BEAM** — The Erlang virtual machine (Bogdan/Björn's Erlang Abstract Machine).
Beamtalk compiles to Core Erlang, which is then compiled to BEAM bytecode.
The BEAM provides lightweight processes, fault tolerance, and hot code loading.
See [Chapter 16](16-beam-interop.md).

**Binary message** — A message with exactly one argument and a symbolic selector
like `+`, `-`, `*`, `/`, `++`, `<`, or `=:=`. Binary messages have higher
precedence than keyword messages but lower than unary. Beamtalk uses standard
math precedence within binary messages (unlike classic Smalltalk). See
[Chapter 4](04-messages.md).

**Block** — An anonymous function (closure) written as `[body]` or `[:arg | body]`.
Blocks are first-class objects: they can be stored in variables, passed as
arguments, and evaluated with `value`, `value:`, or `valueWithArguments:`.
Blocks close over their enclosing scope. See [Chapter 7](07-blocks.md).

**Cascade** — A syntactic form that sends multiple messages to the same receiver
using semicolons: `obj msg1; msg2; msg3`. The result of a cascade is the result
of the last message. See [Chapter 4](04-messages.md).

**DNU (doesNotUnderstand)** — The error raised when an object receives a message
it doesn't understand. DNU is Beamtalk's equivalent of "method not found" in
other languages. Guard against it with `respondsTo:`. See
[Chapter 4](04-messages.md) and [Chapter 12](12-error-handling.md).

**Keyword message** — A message with one or more arguments, where each argument
is preceded by a keyword ending in a colon: `receiver key: arg1 key2: arg2`.
Keyword messages have the lowest precedence of the three message types. The
full selector is the concatenation of all keywords (e.g., `inject:into:`). See
[Chapter 4](04-messages.md).

**Message** — The fundamental operation in Beamtalk. All computation is expressed
as sending messages to objects. A message has a receiver, a selector (name), and
optionally arguments. There are three kinds: unary, binary, and keyword. See
[Chapter 4](04-messages.md).

**Non-local return** — A return triggered by `^` inside a block. Unlike a normal
return (which exits only the block), `^` exits the *enclosing method* immediately.
This is standard Smalltalk semantics, implemented via throw/catch in codegen.
See [Chapter 7](07-blocks.md) and [Chapter 8](08-control-flow.md).

**OTP** — The Open Telecom Platform, a set of Erlang libraries and design
principles for building fault-tolerant systems. Beamtalk actors map directly to
OTP gen_servers, and supervisors use OTP supervision trees. See
[Chapter 17](17-otp-supervisors.md).

**Slot** — An instance variable of a class, declared with `state: name = default`.
In value classes, slots are immutable (updates return a new object via `with*:`
setters). In actors, slots are mutable with `self.name := value`. See
[Chapter 10](10-value-classes.md) and [Chapter 11](11-actors.md).

**Unary message** — A message with no arguments, written as `receiver selector`.
Examples: `42 class`, `"hello" size`, `-5 abs`. Unary messages have the highest
precedence and evaluate left-to-right when chained. See
[Chapter 4](04-messages.md).

**Value class** — An immutable object defined with `Value subclass:`. Once
created, its slots cannot change — "mutations" return a new object. Value
objects support structural equality (`=:=`) and are ideal for data records like
coordinates, money amounts, and configuration. See
[Chapter 10](10-value-classes.md).
