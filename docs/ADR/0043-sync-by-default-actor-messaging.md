# ADR 0043: Sync-by-Default Actor Messaging

## Status
Proposed (2026-02-26)

## Context

### The Async Tax

Every actor message send in BeamTalk currently follows a mandatory async path:

```smalltalk
c := Counter spawn.
c increment await.        "async_send → Future process → await → value"
c getValue await.         "same: Future process spawned, then immediately awaited"
```

Under the hood, `beamtalk_message_dispatch:send/3` unconditionally calls `beamtalk_actor:async_send/4`, which spawns a `beamtalk_future` process, sends the message via `gen_server:cast`, and returns a `{beamtalk_future, Pid}` tuple. The caller must then `await` the future to extract the result.

This design was motivated by Principle 7 ("Async Actors, Sync Primitives") in `beamtalk-principles.md`:

> Inter-actor message sends are **asynchronous**, returning futures. Primitive operations are synchronous. Sends to actors return immediately with a **future/promise**, not a value.

In practice, this principle has been steadily eroded:

1. **The REPL auto-awaits.** `beamtalk_repl_eval:maybe_await_future/1` transparently awaits every future before displaying results. Interactive use feels synchronous — the async nature is invisible.

2. **Chained message sends auto-await.** BT-840 added auto-await inside `beamtalk_message_dispatch:send/3` — if a receiver is a Future, it's awaited before re-dispatching. `counter increment increment` works because the intermediate Future is consumed implicitly.

3. **Nearly every actor send is immediately awaited.** Examining the test suite and examples, the pattern `actor message await` appears hundreds of times. The Future is created and consumed in the same expression — the async machinery provides no value.

4. **Value type sends are already synchronous.** `beamtalk_primitive:send/3` returns values directly. The async/sync split already exists at the type level; it's just that actors are forced onto the async path even when sync would be correct.

### The Cost of Mandatory Async

Every synchronous actor interaction (the 95%+ common case) pays for async infrastructure it doesn't use:

| Step | gen_server:call (sync) | Current async path |
|------|----------------------|-------------------|
| 1 | Send message to actor mailbox | Spawn Future process |
| 2 | Actor processes, replies to caller | Send message to actor mailbox (via cast) |
| 3 | Caller receives reply | Actor processes, sends result to Future process |
| 4 | — | Future process receives result |
| 5 | — | Caller sends await request to Future |
| 6 | — | Future sends result back to caller |
| 7 | — | Future process auto-terminates (5 min timer) |

A synchronous `gen_server:call` is 1 message send + 1 receive. The current async path is 1 process spawn + ~4 message sends + process cleanup. For the vast majority of actor interactions — where the caller needs the result before continuing — the async machinery is pure overhead.

### What This ADR Is Not

This ADR is specifically about the **message send protocol** — how callers interact with actors. It is independent of ADR 0042 (Immutable Value Objects), which addresses the object model. The two can be implemented in either order.

This ADR does **not** remove the `Future` class or `beamtalk_future` infrastructure. Futures remain available for explicit async patterns (forking blocks, parallel computation, etc.). What changes is that actor message sends no longer force the caller through a Future for every interaction.

## Decision

**Make actor message sends synchronous by default.** The statement terminator determines the messaging protocol:

- **`.` (period)** — Synchronous call (`gen_server:call`). The caller blocks until the actor processes the message and returns a result. This is the default for all actor message sends.

- **`!` (bang)** — Asynchronous fire-and-forget cast (`gen_server:cast`). The message is sent to the actor's mailbox; the caller continues immediately. No return value.

### Syntax

```smalltalk
counter := Counter spawn.

"Synchronous call — blocks, returns result:"
counter increment.                "=> 1"
x := counter getValue.            "=> x is 1"

"Asynchronous cast — fire-and-forget, no return value:"
counter increment!                "=> no value, counter will increment eventually"
logger log: 'something happened'! "=> fire-and-forget to logger"

"Cast has no return value — using it in expression context is a compiler error:"
x := counter increment!           "COMPILER ERROR — cast has no return value"
```

### Message Types

| Syntax | Message type | Compiled to | Returns | Use when |
|--------|-------------|-------------|---------|----------|
| `actor msg.` | Unary call | `gen_server:call(Pid, {msg, []})` | Result value | You need the result |
| `actor key: val.` | Keyword call | `gen_server:call(Pid, {key:, [val]})` | Result value | You need the result |
| `actor msg!` | Unary cast | `gen_server:cast(Pid, {msg, []})` | None | Fire-and-forget |
| `actor key: val!` | Keyword cast | `gen_server:cast(Pid, {key:, [val]})` | None | Fire-and-forget |

### Value Types Are Unaffected

`.` on a value type is a direct function call, exactly as today. `!` on a value type is a **compiler error** — value types are not processes and cannot receive async messages:

```smalltalk
point := Point x: 3 y: 4.
point printString.            "Direct function call — sync, no gen_server"
point printString!            "COMPILER ERROR — Point is not an actor"
```

For statically-known receiver types (assigned from a constructor, `self` in value methods), the compiler rejects `!` on value types at compile time via the existing `is_actor_class()` classification. For dynamically-typed receivers (parameters, collection elements), the dispatch layer checks `is_pid(Receiver)` at runtime and raises `#beamtalk_error{type => not_an_actor}` if `!` is used on a non-actor.

### Self-Sends

Self-sends within actor methods already bypass gen_server (calling `Module:dispatch` directly to avoid deadlock). This is unchanged — `.` on a self-send remains a direct dispatch call, not a gen_server round-trip.

```smalltalk
Actor subclass: Counter
  state: value = 0

  increment =>
    self.value := self.value + 1

  incrementTwice =>
    self increment.       "Direct dispatch — no gen_server, no deadlock risk"
    self increment
```

### Deadlock Considerations

Synchronous actor-to-actor calls can deadlock if actor A calls actor B while B is calling A — both block waiting for a reply that can never arrive. This is the standard gen_server deadlock pattern, well-understood in the BEAM ecosystem.

**Mitigation guidance:**

- **Use `!` for notifications.** If the sender doesn't need a response, use fire-and-forget: `logger log: event!`
- **Avoid cyclic call patterns.** If A needs to call B and B needs to call A, restructure so one direction uses `!`.
- **Self-sends are always safe.** They bypass gen_server entirely (direct dispatch).
- **Client→service is always safe.** A stateless caller invoking a service actor cannot deadlock.

**Detection:** gen_server:call has a configurable timeout (default 5000ms). A deadlocked call will crash with `{timeout, {gen_server, call, ...}}` after 5 seconds, producing a clear crash dump. This is the same behavior Erlang/Elixir developers encounter daily — it's a known pattern, not a novel risk.

### Timeouts

The gen_server:call default timeout is 5000ms. For the initial implementation, BeamTalk uses this default. A future enhancement could add timeout configuration:

```smalltalk
"Future enhancement — not part of this ADR:"
counter getValue timeout: 30000.    "30 second timeout"
```

Timeout configuration is deferred because the default 5000ms is appropriate for the vast majority of actor interactions, and the timeout is configurable at the Erlang level (`gen_server:call(Pid, Msg, Timeout)`) for advanced use cases via BEAM interop.

### REPL Behavior

The REPL currently auto-awaits futures via `maybe_await_future/1`. With sync-by-default:

- **Sync calls (`.`):** Return values directly. No auto-await needed — the REPL displays the result immediately.
- **Casts (`!`):** Produce no result. The REPL displays nothing (or a confirmation like `ok`).
- **`maybe_await_future/1` remains** for backward compatibility during the migration period, handling any remaining Future-returning code paths.

```smalltalk
"> counter := Counter spawn"
#Actor<Counter,<0.123.0>>

"> counter increment"
1

"> counter increment!"
(no output — fire-and-forget)

"> counter getValue"
2
```

## Prior Art

### Erlang/OTP — The Platform We Compile To

Erlang provides both patterns as explicit function calls:

```erlang
gen_server:call(Pid, Request)        %% Sync — blocks until reply
gen_server:cast(Pid, Request)        %% Async — fire-and-forget
Pid ! Message                        %% Raw async send
```

`gen_server:call` is the standard OTP pattern for request-response. It's used far more frequently than `cast` in production Erlang code. Erlang makes the programmer choose explicitly; BeamTalk makes sync the default and async the opt-in, which matches the actual usage distribution.

**Adopted:** The gen_server:call/cast split as the underlying mechanism. BeamTalk's `.`/`!` maps directly to this well-proven OTP pattern.

### Elixir — Same Platform, Same Patterns

```elixir
GenServer.call(pid, :pop)           # Sync — blocks
GenServer.cast(pid, {:push, val})   # Async — fire-and-forget
send(pid, message)                  # Raw async
```

Elixir follows Erlang's explicit function-call approach. Like Erlang, `GenServer.call` is the dominant pattern. Elixir doesn't provide syntactic sugar for the call/cast distinction — it's always a function call. BeamTalk improves on this by making the distinction syntactic (`.` vs `!`), which is lighter-weight and more visible at the call site.

**Adopted:** The validation that sync request-response is the dominant pattern on BEAM.

### Smalltalk — The Syntax We Inherit

All Smalltalk message sends are synchronous. Period terminates a statement. There is no async primitive — concurrency is achieved via explicit forking (`[expr] fork`).

```smalltalk
counter increment.        "Synchronous — always"
[counter increment] fork. "Async — explicit, via block"
```

BeamTalk's `.` for sync calls is directly faithful to Smalltalk semantics. Adding `!` for async casts extends the syntax without breaking the Smalltalk mental model — `.` still means what it always meant.

**Adopted:** Sync as the default, matching 50 years of Smalltalk convention.

### Akka/Scala — Tell vs Ask

```scala
actor ! message           // "tell" — async fire-and-forget
actor ? message           // "ask" — returns Future[T]
```

Akka defaults to async (`!` is "tell", the preferred pattern). Sync requires `?` ("ask") which returns a Future. Akka's philosophy is explicitly async-first.

**Insight:** Akka uses `!` for async (fire-and-forget), which aligns with BeamTalk's proposed `!` for cast. The symbol choice is consistent across both languages, even though the defaults are inverted (Akka defaults async; BeamTalk defaults sync).

### Pony — Async-Only Cross-Actor

```pony
actor Counter
  be increment() => count = count + 1  // "be" = async behavior
  fun get_count(): U32 => count        // "fun" = sync (local only)
```

Pony enforces async for all cross-actor communication. `fun` is local-only; `be` (behavior) is the only way to communicate across actors. This prevents deadlock by design but at the cost of flexibility — you can never get a synchronous response from another actor.

**Not adopted:** Pony's async-only model prevents deadlock but eliminates request-response, the most common actor interaction pattern. BeamTalk preserves request-response as the default and relies on the standard gen_server timeout mechanism for deadlock detection.

### Gleam — Typed Actor Messaging

```gleam
actor.send(subject, message)                   // Async — fire-and-forget
actor.call(subject, message, timeout_ms)       // Sync — waits for reply
```

Gleam provides both patterns with type safety — the `Subject` type ensures messages match the actor's expected type. Async (`send`) is the simpler function; sync (`call`) requires an explicit timeout.

**Adopted:** The validation that both sync and async patterns are needed on BEAM. Gleam's timeout parameter on `call` is worth considering as a future enhancement for BeamTalk.

### Summary

| Language | Default | Sync syntax | Async syntax | BeamTalk alignment |
|----------|---------|-------------|--------------|-------------------|
| Erlang | Explicit choice | `gen_server:call` | `gen_server:cast` / `!` | Direct mapping |
| Elixir | Explicit choice | `GenServer.call` | `GenServer.cast` | Direct mapping |
| Smalltalk | Sync | `.` | `[...] fork` | `.` = sync preserved |
| Akka | Async | `?` (ask) | `!` (tell) | `!` = async shared |
| Pony | Async only | N/A (local only) | `be` | Different model |
| Gleam | Explicit choice | `call()` | `send()` | Both patterns available |

BeamTalk's proposal sits at the intersection of Smalltalk (sync default, `.` terminator) and Erlang (gen_server:call/cast as the mechanism, `!` for async). No other language occupies this exact point.

## User Impact

### Newcomer (from Python/JS/Ruby)

**Positive:** Actor sends look like regular method calls — `counter increment.` returns a value, just like calling a method in Python. No Future/Promise/async/await ceremony to learn. The mental model is "send a message, get a response" which matches synchronous function calls in every mainstream language.

**Positive:** `!` for fire-and-forget is intuitive — the bang conveys "do this and don't wait." It's reminiscent of Ruby's `save!` convention (though the semantics differ).

**Concern:** The deadlock risk is invisible. A newcomer who has two actors call each other will get a timeout crash with no obvious explanation. Error messages must be clear: "Actor A timed out calling Actor B. This may indicate a deadlock — consider using `!` (cast) for one direction."

### Smalltalk Developer

**Positive:** `.` means exactly what it always meant — a synchronous message send. This is the most Smalltalk-faithful messaging model possible on BEAM. Current code like `counter increment await` becomes `counter increment.` — the Smalltalk developer never wanted `await` in the first place.

**Positive:** `!` is a natural extension. Smalltalk doesn't have async primitives, but if it did, bang would be a reasonable syntax choice. It doesn't conflict with any existing Smalltalk syntax.

**Neutral:** The call/cast distinction is new — Smalltalk doesn't have fire-and-forget messaging. But the concept maps to "send this message and don't wait for a response," which is straightforward.

### Erlang/BEAM Developer

**Positive:** `.` = `gen_server:call`, `!` = `gen_server:cast`. This is exactly how they already think about actor messaging. The syntax is a thin, predictable layer over OTP patterns they know intimately. Deadlock patterns, timeout behavior, supervision — all transfer directly.

**Positive:** `!` echoes Erlang's `!` send operator. While the semantics differ slightly (Erlang's `!` is raw send; BeamTalk's `!` is `gen_server:cast`), the intent is the same: "send this message asynchronously."

**Concern:** Erlang's `gen_server:call` has explicit timeout parameters. BeamTalk's `.` uses the default 5000ms with no override syntax. For production systems that need longer timeouts (e.g., calling a slow external service), the escape hatch is BEAM interop. This is adequate for now but may need a first-class solution later.

### Operator / Production User

**Positive — Simpler runtime model.** No Future processes spawned for routine actor interactions. Fewer processes means less memory, less GC pressure, and simpler `observer` output. Each actor call is a direct gen_server interaction — observable with standard BEAM tooling (`sys:get_state/1`, `dbg`, `recon`).

**Positive — Standard timeout behavior.** gen_server:call timeouts produce clear crash dumps with the stuck call visible in the stack trace. This is the same failure mode Erlang/Elixir operators debug daily — no new failure patterns to learn.

**Positive — Deadlock detection is built in.** Unlike the current async model (where deadlocks manifest as silently accumulated Futures or mysterious hangs), sync calls fail loudly with a timeout. The 5000ms default means deadlocks surface within 5 seconds, not silently.

**Concern — Timeout tuning.** The 5000ms default may be too short for some production patterns (e.g., an actor that queries a database). Until BeamTalk adds timeout configuration syntax, operators must use BEAM interop for custom timeouts.

## Steelman Analysis

### Best Argument for Alternative 1: Keep Async-by-Default (Status Quo)

| Cohort | Their strongest argument |
|--------|------------------------|
| **Newcomer** | "At least with Futures I can fire off three requests and await them all at the end. With sync-by-default, I have to do them sequentially. You're taking away parallelism that I get for free today." |
| **Smalltalk purist** | "Smalltalk messages ARE synchronous, so the async model was wrong from the start. But now you've built an ecosystem around Futures — stdlib tests, examples, the REPL. Changing the default is churn. The REPL already auto-awaits, so the ergonomic problem is solved. Why break everything?" |
| **BEAM veteran** | "Async-by-default is the honest design. BEAM processes communicate asynchronously — that's the physical reality. gen_server:call is syntactic sugar over async send + receive. By making sync the default, you're hiding the true cost of blocking. Erlang developers know that every call is a potential timeout; making it look like a method call hides that." |
| **Language designer** | "You're sacrificing a genuine differentiator. 'Everything is async with Futures' is a unique position in the language design space. Sync-by-default is what every OOP language does. You had something interesting and you're giving it up for ergonomics. The right answer is better Future ergonomics (auto-await, pipeline operators), not removing Futures." |
| **Operator** | "I can reason about async systems. If every send returns a Future, I know the failure modes: Future timeout, Future rejection, unresolved Future. With sync-by-default, I have gen_server:call timeouts AND potential deadlocks AND the old Future code paths during migration. The mixed model during transition is worse than either pure model." |

### Best Argument for Alternative 2: Implicit-Await Futures

| Cohort | Their strongest argument |
|--------|------------------------|
| **Language designer** | "Keep the Future machinery, make it invisible. `.` auto-awaits, `!` discards. The user gets sync ergonomics AND you preserve the Future infrastructure for advanced patterns (parallel sends, timeouts, combinators). You don't have to choose between sync and async — you can have both." |
| **BEAM veteran** | "Auto-await is a proven pattern — your REPL already does it. Extending it to compiled code is a smaller, safer change than switching the underlying gen_server protocol. You keep async_send + Future as the mechanism but hide the ceremony. If Future performance is the concern, optimize the Future implementation (pool processes, use ETS), don't remove it." |
| **Operator** | "I like that Futures give me explicit timeout control (`await: 10000`). With gen_server:call, I'm stuck with the default timeout unless I drop to Erlang interop. Implicit-await preserves the timeout escape hatch." |

### Why Sync-by-Default Wins

**Against the Status Quo:** The BEAM veteran's "honest design" argument is the strongest. But honesty cuts both ways — the current implementation is dishonest about its own cost. Every "async" send immediately synchronizes via `await`. The Future process is spawned and consumed in the same expression 95%+ of the time. The system is paying async overhead to deliver sync behavior. Making sync the default is more honest about what the code actually does.

The language designer's "unique differentiator" argument is appealing but misidentifies the differentiator. BeamTalk's uniqueness is Smalltalk syntax on BEAM with live development — not "everything is async." Erlang and Elixir already own the "async-first BEAM language" space. BeamTalk's value proposition is bringing Smalltalk's interactive, message-passing model to BEAM, and Smalltalk messages are synchronous.

**Against Implicit-Await Futures:** The BEAM veteran's "smaller change" argument is technically true. But implicit-await preserves the worst of both worlds: the overhead of Future process creation with none of the benefits (since Futures are immediately consumed). It's a compatibility shim, not a design improvement. If Futures are invisible, they shouldn't exist — they're just overhead. If they're visible, the current explicit `await` is more honest than auto-await.

The operator's timeout concern is valid. The timeout escape hatch will need a solution eventually — but gen_server:call's timeout is configurable at the Erlang level today, and a first-class BeamTalk syntax can be added in a future ADR without changing the messaging model.

### Tension Points

- **Differentiator loss vs. ergonomic gain.** Principle 7 ("Async Actors, Sync Primitives") was a stated design choice, not an accident. Reversing it is a conscious repositioning of the language. The ADR must be explicit about this: BeamTalk's differentiators are interactive development, Smalltalk syntax, and the value/actor model — not async-by-default messaging.

- **Parallel sends.** Today you can fire off 3 async sends without awaiting, then await all 3. With sync-by-default, the caller blocks on each send sequentially. For the rare case where parallel actor requests are needed, an explicit mechanism (Future-returning send, or `[block] fork`) fills the gap. This is deferred to a future ADR.

- **Migration churn.** Every `foo await` in tests and examples must be rewritten to `foo.` (or just remove `await`). The migration is mechanical but touches many files.

## Alternatives Considered

### Alternative 1: Keep Async-by-Default (Status Quo)

Maintain the current model. All actor sends return Futures. Callers use `await` for sync behavior. Improve ergonomics with better Future combinators and auto-await in more contexts.

```smalltalk
c increment await.         "explicit sync — status quo"
c increment.               "auto-await in REPL (already works)"
f := c increment.          "Future object in compiled code"
```

**Rejected because:** The async model imposes ~4x overhead for the dominant use case (sync request-response). The REPL already hides the async nature via auto-await, creating an inconsistency between REPL and compiled code. Nearly every actor send in the test suite is immediately awaited, demonstrating that the async model doesn't match actual usage patterns. The Future infrastructure remains available for explicit async patterns — removing it from the default send path doesn't eliminate it.

### Alternative 2: Implicit-Await Futures (`.` auto-awaits, `!` discards)

Keep the Future machinery under the hood. `.` sends async but auto-awaits at the statement boundary. `!` sends async and discards the Future.

```smalltalk
c increment.              "async_send + auto-await → value"
c increment!              "async_send + discard → fire-and-forget"
```

**Rejected because:** Preserves Future process overhead for every actor send while delivering sync behavior — the worst of both worlds. Every `.` still spawns a process, sends ~4 messages, and terminates the process. If the behavior is synchronous, the mechanism should be synchronous. The BT-840 auto-await in `send/3` proved the concept; this ADR replaces the mechanism rather than adding another layer of auto-await.

### Alternative 3: Explicit Syntax for Both (No Default)

Require the programmer to choose call or cast for every send:

```smalltalk
c increment.              "AMBIGUOUS — compiler error"
c call increment.         "explicit sync"
c cast increment.         "explicit async"
```

**Rejected because:** Violates Smalltalk syntax — messages don't have `call`/`cast` prefixes. The verbosity defeats the purpose of a message-passing language. The 95%+ case is sync; forcing explicit annotation for the common path is hostile to ergonomics. Erlang requires explicit `gen_server:call` vs `gen_server:cast`, which is appropriate for a systems language but too verbose for a Smalltalk-inspired language where messages should be lightweight.

### Alternative 4: Method-Level Declaration (Pony-Style)

Declare whether a method is sync or async at the definition site:

```smalltalk
Actor subclass: Counter
  state: value = 0

  increment => self.value := self.value + 1         "sync by default"
  async logEvent: event => logger log: event!        "declared async"
```

**Rejected because:** This conflates the method's implementation with its call protocol. In OTP, the same gen_server method can be called via `call` or `cast` — the caller decides, not the callee. A `Counter` might normally be called synchronously, but a monitoring system might fire-and-forget `increment!` for performance. The caller should choose the protocol, not the method author. Additionally, this requires new syntax at the definition site and splits the method namespace, adding complexity for a problem that `.`/`!` solves more simply at the call site.

## Consequences

### Positive

- **Eliminates Future overhead for sync calls.** The dominant use case (sync request-response) becomes a direct gen_server:call — 1 message send + 1 receive, no intermediate process.
- **REPL and compiled code are consistent.** Both use `.` for sync sends. No more "REPL auto-awaits but compiled code doesn't" inconsistency.
- **Removes `await` boilerplate.** `counter increment await` becomes `counter increment.` — every actor interaction site is cleaner.
- **Matches Smalltalk mental model.** `.` means what Smalltalk developers expect — a synchronous message send that returns a value.
- **Maps directly to OTP idioms.** `.` = `gen_server:call`, `!` = `gen_server:cast`. BEAM developers can predict the generated code.
- **Explicit concurrency choice.** The `.`/`!` distinction makes the sync/async decision visible at every call site. No hidden async machinery.
- **Deadlocks fail loudly.** gen_server:call timeouts produce clear crash dumps, unlike silently accumulated Futures.

### Negative

- **Deadlock risk for cyclic actor calls.** Two actors that synchronously call each other will deadlock. This is the standard gen_server pattern — well-understood but still a risk for newcomers who don't know the pattern. Error messages at the timeout boundary must be clear and actionable.
- **No parallel sends by default.** Today, you can fire off 3 async sends and await them later. With sync-by-default, sends are sequential. An explicit parallel-send mechanism (deferred) fills this gap.
- **Breaking change.** All `foo await` patterns must be rewritten. Tests, examples, and user code are affected. The migration is mechanical but touches many files.
- **Principle 7 reversal.** This explicitly reverses a stated design principle. The principle must be updated, and the reasons for the reversal documented (this ADR).
- **Fixed timeout.** gen_server:call's 5000ms default is not configurable from BeamTalk syntax (initially). Production systems may need BEAM interop for custom timeouts until a first-class syntax is added.

### Neutral

- **Future class remains.** `beamtalk_future.erl` and `Future.bt` continue to exist for explicit async patterns (forking blocks, parallel computation). They are no longer on the actor message-send hot path.
- **`beamtalk_message_dispatch:send/3` routing changes.** The dispatch layer must route actor sends through `sync_send/3` instead of `async_send/4` for `.` sends, and through a new cast path for `!` sends.
- **Self-send behavior unchanged.** Self-sends already bypass gen_server via direct dispatch. The `.`/`!` distinction doesn't affect self-sends — they're always direct calls.

## Implementation

### Phase 0: Wire Check (S)

- Add `!` (bang) as `TokenKind::Bang` in `token.rs` and `lexer.rs`
- Parse `!` as a statement terminator (distinct from `.`)
- Compile a single `actor msg.` to `gen_server:call` and verify it returns a value
- Compile a single `actor msg!` to `gen_server:cast` and verify fire-and-forget
- **Goal:** Prove the lexer/parser/codegen changes work end-to-end on one example
- **Effort:** S

### Phase 1: Language Model Changes (M)

**Lexer (`token.rs`, `lexer.rs`):**
- Add `TokenKind::Bang` token
- Handle `!` in the lexer's main dispatch (currently falls to error branch)

**Parser:**
- Recognize `!` as a statement terminator alongside `.`
- Mark AST nodes with call/cast annotation based on terminator
- Detect cast-in-expression-context errors (`x := foo bar!`) and produce clear error message: "Cast (`!`) has no return value and cannot be used in an expression. Use `.` for a synchronous call that returns a value."
- Detect `!` on statically-known value types and produce: "Cannot use `!` (cast) on value type Point. Value types are not actors — use `.` for a synchronous call."

**Semantic Analysis:**
- Verify `!` is only used on actor-typed receivers where statically determinable
- Propagate call/cast annotation through to codegen

### Phase 2: Codegen and Runtime Changes (L)

**Codegen (`dispatch_codegen.rs`, `expressions.rs`):**
- For `.` on actor receivers: generate `beamtalk_actor:sync_send(Pid, Selector, Args)` (or direct `gen_server:call`)
- For `!` on actor receivers: generate `beamtalk_actor:cast_send(Pid, Selector, Args)` (or direct `gen_server:cast`)
- For `.` on value types: unchanged (direct function call)
- For `!` on value types: compiler error (caught in Phase 1)

**Runtime (`beamtalk_message_dispatch.erl`):**
- Update `send/3` to route actor sends through `sync_send/3` instead of `async_send/4`
- Add `cast_send/3` (or extend `async_send` with no-future path) for `!` sends
- Keep auto-await for futures in `send/3` for backward compatibility during migration

**Runtime (`beamtalk_actor.erl`):**
- `sync_send/3` already exists and uses `gen_server:call` — verify it handles all edge cases (lifecycle methods, dead actors, timeouts)
- Add `cast_send/3` or extend existing infrastructure for structured casts

**REPL (`beamtalk_repl_eval.erl`):**
- `maybe_await_future/1` remains during migration
- Once all send paths use sync, `maybe_await_future/1` becomes a no-op for actor sends (Futures only appear from explicit async code)

### Phase 3: Migration and Testing (L)

- Remove `await` from all stdlib tests that use `actor method await` → `actor method.`
- Remove `await` from all e2e tests
- Update examples
- Update `docs/beamtalk-language-features.md` — replace "Async Message Passing" section with sync-by-default model
- Update `docs/beamtalk-principles.md` — revise Principle 7 from "Async Actors, Sync Primitives" to "Sync-by-Default Actors with Explicit Cast"
- Verify REPL behavior with the new model
- Test deadlock detection: two actors in a cycle, verify 5000ms timeout and clear error message
- Test `!` on value types: verify compiler error
- Test `!` in expression context: verify compiler error
- Performance benchmarks: compare sync path vs old async+await path

## Migration Path

### Code Changes

| Current Pattern | New Pattern | Notes |
|---|---|---|
| `counter increment await` | `counter increment.` | Remove `await`, keep `.` |
| `counter increment await: 10000` | `counter increment.` | Custom timeout deferred — use BEAM interop if needed |
| `c getValue await` | `c getValue.` | Remove `await` |
| `f := counter increment. f await` | `x := counter increment.` | Direct value, no Future |
| `counter increment` (REPL) | `counter increment` (REPL) | Unchanged — REPL already auto-awaited |

### Fire-and-Forget (New)

| Current Pattern | New Pattern | Notes |
|---|---|---|
| `logger log: event` (no await, Future leaked) | `logger log: event!` | Explicit cast — no Future created |

### Principle Update

Principle 7 in `beamtalk-principles.md` changes from:

> **Async Actors, Sync Primitives** — Inter-actor message sends are asynchronous, returning futures.

To:

> **Sync-by-Default Actors** — Actor message sends are synchronous by default (`.` = `gen_server:call`). Fire-and-forget casts use `!` (`gen_server:cast`). Self-sends and value type sends are direct calls. The caller chooses the protocol at the call site.

## References

### Related ADRs
- ADR 0005 (BEAM Object Model) — actors as gen_server processes
- ADR 0006 (Unified Method Dispatch) — dispatch routing for actors vs value types
- ADR 0039 (Syntax Pragmatism vs Smalltalk) — `!` as a pragmatic syntax extension
- ADR 0042 (Immutable Value Objects) — independent but complementary; `.`/`!` syntax split out from that ADR

### External References
- Erlang gen_server: `gen_server:call/2,3` and `gen_server:cast/2` — the underlying OTP mechanism
- Akka tell (`!`) vs ask (`?`) — `!` for async fire-and-forget is an established convention
- Elixir GenServer — validation that sync (call) is the dominant OTP pattern
- Pony behaviors — async-only cross-actor sends, alternative design point

### Related Issues
- BT-840 — Auto-await futures in chained message sends (the BT-840 workaround that motivated this change)
- BT-507 — Future combinators (deferred; Futures remain for explicit async patterns)
