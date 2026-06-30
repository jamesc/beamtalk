# ADR 0101: Unified Erlang Interop — `native:` for Stateless Objects, Wrap-by-Default FFI, and a Clean `@primitive`/`@intrinsic`/`native:` Split

## Status
Accepted (2026-06-28)

## Implementation Tracking

**Epic:** BT-2719
**Issues:** BT-2720 (native: codegen) · BT-2721 (bulk migrate) · BT-2722 (unify FFI proxy) · BT-2723 (migrate exit/throw catches) · BT-2724 (optional @primitive string) · BT-2725 (substrate→@intrinsic + ReactiveSubprocess) · BT-2726 (internal seams) · BT-2727 (E2E + docs)
**Start:** BT-2722 (foundation — adds the `native_call` helper + unified wrapping; no deps), then BT-2720 (consumes `native_call`)
**Status:** Planned

## Context

### Problem statement

Beamtalk's standard library binds to the BEAM through **three overlapping mechanisms that have drifted into incoherence**:

1. **`@primitive "selector"`** — lowers to `beamtalk_<class>:dispatch(Selector, Args, Self)`, a runtime dispatch that adds type guards, structured `#beamtalk_error{}` wrapping, and the open-world extension registry (ADR 0007).
2. **`(Erlang module) call: arg`** — a cross-module Erlang call. Wrapping today is **inconsistent across two runtime paths**: `beamtalk_erlang_proxy:direct_call/3` wraps only `badarg`/`undef` and lets every other `error:*`, `exit`, and `throw` propagate raw (`beamtalk_erlang_proxy.erl:161-186`); `validate_and_apply/4` (the `ErlangModule` proxy-object path) wraps *all* `error:*` **and** `exit:*` (as `erlang_exit`) **and** `throw:*` (as `erlang_throw`) (`beamtalk_erlang_proxy.erl:395-438`). So the same surface syntax has two different error contracts depending on lowering, and embedded FFI in logic frequently hits the leakier `direct_call` path.
3. **`native:` + `self delegate`** — a class-level declaration that an `Actor`'s gen_server is provided by a named Erlang module; `self delegate` lowers to `beamtalk_actor:sync_send/3`, which auto-wraps errors at the actor boundary (ADR 0056).

Measuring the current stdlib (`stdlib/src/*.bt`):

- **362** `@primitive "selector"` bindings (comment-excluded; 365 raw includes 3 doc-comment examples).
- **350** methods containing `(Erlang …)` FFI calls (≈368 call sites).
- **39** `@intrinsic` bindings.
- Only **2** classes use `native:` (`Subprocess`, `TranscriptStream`).

Two smells fall out of this:

**Smell 1 — `@primitive` is overloaded.** Of the 362, ~306 are value-type primitives: `self` is a native BEAM value (Integer, Float, String, a `#beamtalk_error{}` record, a fun) needing guarded dispatch + an extension table. But ~56 others use `@primitive` for unrelated reasons — ~36 are reflection over class objects (`Behaviour`, `Metaclass`, `Class`, `Protocol`), ~15 are dispatch/actor substrate (`ProtoObject`, base `Actor` lifecycle), ~5 are an Erlang-backed actor (`ReactiveSubprocess`). The keyword conflates "native value type" with "anything bound to Erlang." (Part 3 shows the right split is subtler than "value type vs not" — some reflection *correctly* stays `@primitive` because it needs the dispatch+extension+wrapping layer, while substrate moves to `@intrinsic` and the actor to `native:`.)

**Smell 2 — `native:` is the missing twin of FFI.** Of the 350 FFI methods, **305 are pure pass-through** — the entire body is one `(Erlang module) selector: self with: arg` call. These are *exactly* what `self delegate` expresses for actors, but because the class is a stateless `Object` (not an `Actor`), the author must hand-thread `self` into every call. The poster child is `Stream`, which sits **next to** its actor twin `TranscriptStream` in the tree:

```beamtalk
// TranscriptStream (Actor) — clean
show: value :: Printable -> Nil => self delegate

// Stream (Object) — must hand-thread self
select: predicate :: Block -> Stream(E) => (Erlang beamtalk_stream) select: self with: predicate
take: count :: Integer -> List(E)       => (Erlang beamtalk_stream) take: self count: count
```

The asymmetry is purely an accident of class kind. **15 of Stream's 16 methods are pure delegation.**

The remaining **45 FFI methods are "embedded"** — FFI buried inside real Beamtalk logic (`collect:`/`inject:into:`/`isNil ifTrue:`). These are concentrated: **`SystemNavigation` alone holds 29 (64%)**, the rest a thin tail. These genuinely leak raw Erlang errors today, mitigated only by each backing module hand-rolling `try…catch → beamtalk_error:new`.

### Current state — the three mechanisms compared

| | `@primitive "sel"` | `native:` + `self delegate` | `(Erlang module)` FFI |
|---|---|---|---|
| Lowers to | `beamtalk_X:dispatch(Sel, Args, Self)` | `beamtalk_actor:sync_send(Pid, Sel, Args)` | `module:fn(Args)` |
| Restricted to | "primitive type" classes | `Actor` subclasses only | any class, any method body |
| Error wrapping | automatic (`#beamtalk_error{}`) | automatic (`ensure_wrapped`) | **inconsistent** — `direct_call` leaks most; `validate_and_apply` wraps everything incl. `exit`/`throw` |
| Type guards / extension registry | yes | n/a | no |

### Constraints

- `native:` is **already accepted by the parser on any class** (`parser/declarations.rs`) — only codegen ignores it for non-actors (`mod.rs` routes by `is_actor_class`, and the value-type path has no `native:` handling). So extending it is "make codegen honor existing syntax," not new surface.
- `self delegate` always emits `sync_send/3` today (`native_facade.rs`) — a stateless variant must emit a direct module call instead.
- `beamtalk_erlang_proxy:direct_call/3` **deliberately** does not wrap `exit`/`throw` (so `Erlang erlang exit: 1` propagates). Any wrap-by-default change must preserve that.
- The `#beamtalk_error{}` contract is mandated for all user-facing/public errors (CLAUDE.md, ADR 0028).

## Decision

Adopt a **single coherent model** with four parts, delivered in phases.

### Part 1 — `native:` extends to stateless `Object`s

`native:` becomes the universal "this class is backed by a named Erlang module" declaration. The **class kind selects the lowering** (exactly as the compiler already branches everywhere else):

| Class kind | `self delegate` lowers to | Receiver convention | Wrapping boundary |
|---|---|---|---|
| `Actor` | `beamtalk_actor:sync_send(Pid, Sel, Args)` | `self` = pid; cross-process; can block/timeout/cross-node | `sync_send` (`ensure_wrapped`) |
| `Object` | `beamtalk_erlang_proxy:native_call(Mod, Fn, [Self\|Args], {Class, Sel})` | `self` = first positional arg; in-process; synchronous | shared `native_call` helper — same `error:*`→`#beamtalk_error{}` wrapping **and** 0076 `Result` coercion as inline FFI |

**`native:` must NOT lower to a bare `mod:Fn(Self, Args)` call.** A bare call bypasses the FFI proxy and would therefore get *neither* wrap-by-default (Part 2) *nor* the ok/error→`Result` coercion (ADR 0076) — re-introducing the exact "two FFI behaviors" inconsistency this ADR removes (e.g. `File readAll:` would return a raw `{ok, Bin}` instead of `Result(String, Error)`). Instead it lowers through a shared boundary that applies the **same** coercion + wrapping as inline `(Erlang …)` FFI, carrying the Beamtalk `{Class, Sel}` as context so `native:` errors read `Stream>>take:` (better than the bare-MFA context raw inline FFI gets — see Part 2 limitation).

Two implementation notes from review:
- **Don't add a redundant concept.** `direct_call/3` already factors its work through `apply_with_coercion/4` (charlist retry + `Result` coercion), which Part 2 upgrades to wrap all `error:*`. The `native:` path is best a thin entry that threads `{Class, Sel}` through that same helper (e.g. `apply_with_coercion/5`), *not* a parallel reimplementation. The only genuinely new element is the context argument; "`native_call`" is a name for that entry, not a second wrapping path.
- **Skip the *proactive* export pre-check, but keep `undef` handling.** It may skip `direct_call`'s up-front `get_exports`/arity *pre-check* (codegen guarantees the call shape at compile time — a real perf/simplicity win), **but it must still catch `error:undef` in the try** and surface a structured `does_not_understand` with `{Class, Sel}` context. The compile-time guarantee does **not** survive **hot code reload**: a backing module can be swapped between compile and call (`validate_and_apply/4` already flags this TOCTOU race at `beamtalk_erlang_proxy.erl:301`), so a `native:` call to a since-removed function must wrap, not leak a raw `undef`.

Self-threading is inferred from declaration side: **instance methods prepend `self`; class methods omit it** — the compiler already knows which is which. `Stream` becomes:

```beamtalk
sealed typed Object subclass: Stream(E) native: beamtalk_stream
  class new: _ :: Object -> Nil => self error: "Use 'Stream on:' or 'Stream from:'"  // real body stays
  class from: start :: Integer -> Stream(Integer) => self delegate   // ⇒ beamtalk_stream:from(Start)
  select: predicate :: Block -> Stream(E)         => self delegate   // ⇒ beamtalk_stream:select(Self, Predicate)
  take: count :: Integer -> List(E)               => self delegate   // ⇒ beamtalk_stream:take(Self, Count)
  asList -> List(E)                               => self delegate   // ⇒ beamtalk_stream:asList(Self)
```

Delegated methods use an explicit `=> self delegate` body — **identical** to the actor twin `TranscriptStream` (`show: value => self delegate`); there is no bare-signature method form in Beamtalk. This requires a **`delegate` sentinel on the `Object`/`Value` base**, mirroring exactly what ADR 0056 added to `Actor` (`sealed delegate => @primitive "actorDelegate"`, raising "delegate called on a non-native Actor"): it makes `self delegate` typecheck on a stateless Object and is the runtime safety net for misuse on a non-`native:` class. **And — because Stream's constructors (`class from:`, `class on:`) are class-side delegations where `self` is the class object — a matching sentinel is needed on `Object class` (the metaclass) too**, so `self delegate` typechecks in a class-method body. (Actor needs only the instance-side sentinel because no stdlib Actor has class-side `self delegate` methods; `native:` Objects do.) Codegen rewrites `self delegate` on a `native:` class to the real call (direct for Objects, `sync_send` for Actors) before the sentinel is ever reached. A class may mix `self delegate` methods with real-body methods (TranscriptStream already does).

### Part 2 — FFI wraps by default (no opt-out in v1)

`(Erlang module) call:` becomes **safe by default and unified across both proxy paths**: `error`-class exceptions are converted to `#beamtalk_error{}` (reusing `beamtalk_exception_handler:ensure_wrapped`, which is idempotent).

**Primary motivation — a user must never see a raw Erlang error tuple.** In an interactive, Smalltalk-style environment the REPL surfaces errors constantly; a raw `** exception error: badarg` or `{badarg, [...]}` tuple leaks the host VM, breaks the illusion of being *in* Beamtalk, and stops a newcomer cold. Wrap-by-default guarantees every error a user sees is a `#beamtalk_error{}` with `kind`/`hint`/`selector` — the *same* shape whether it originated in a value-type `@primitive`, a `native:` delegate, or raw FFI. That uniformity matters most at the REPL, which is the surface where the abstraction is most exposed.

This is **not purely additive** — it changes both existing paths in opposite directions, which the migration path must account for:

| Exception class | `direct_call/3` today | `validate_and_apply/4` today | **Proposed unified default** | Net change |
|---|---|---|---|---|
| `error:*` | only `badarg`/`undef` wrapped; rest leak | all wrapped | **wrap** → `#beamtalk_error{}` | `direct_call` wraps *more* |
| `exit:*` | propagate raw | wrapped as `erlang_exit` | **propagate** | `validate_and_apply` **rolls back** to propagate |
| `throw:*` | propagate raw | wrapped as `erlang_throw` | **pass through** | `validate_and_apply` **rolls back** to pass through |

The `exit`/`throw` rows are a deliberate choice: process exit and throws are semantically meaningful (intentional `erlang:exit/1`, `{noproc,_}` from a dead actor, control-flow throws) and should not be flattened into `#beamtalk_error{}`. `Erlang erlang exit: 1` keeps working with no opt-out. **Risk:** any code currently relying on `ErlangModule`-dispatch wrapping `exit`/`throw` as a catchable `#beamtalk_error{}` changes behavior — Phase 2 must grep for and migrate such call sites (expected to be rare; `exit`/`throw` from FFI is unusual).

**No opt-out receiver in v1 (decided).** An earlier draft proposed a `(RawErlang …)` receiver as the opt-out. Review showed its use case collapsed once `exit`/`throw` propagate by default — the *only* thing it would still buy is returning a raw `error:*` tuple instead of `#beamtalk_error{}`, a rare low-level need. So v1 ships **no opt-out**:

```beamtalk
(Erlang erlang) exit: 1                       // propagates unchanged (exit not wrapped)
(Erlang beamtalk_xref) senders_of_bt: sel     // error:* → #beamtalk_error{}
// raw error:* tuple → not supported in v1
```

A future raw-`error:*` escape (receiver or `@raw` pragma) is left as a documented option to add only if a concrete need appears; the existing `beamtalk_erlang_proxy:dispatch/3` `call:args:` form is *not* it (it routes through `validate_and_apply`, which wraps). The existing per-module hand-wrapping (`beamtalk_file:readAll:`'s `file_not_found` + hint, etc.) is **demoted to an enrichment layer** — kept where it adds domain-specific hints, deleted where it only duplicated the generic `try…catch → beamtalk_error:new` boilerplate the default now provides.

**Limitation (documented, accepted):** for **raw inline `(Erlang …)` FFI**, the proxy knows the Erlang MFA, not the Beamtalk class/selector, so an auto-wrapped error carries `beamtalk_xref:senders_of_bt` context rather than `SystemNavigation>>sendersOf:`. Still structured, still no raw crash. **`native:` methods do not have this limitation** — their `native_call` lowering passes `{Class, Sel}`, so the error carries `Stream>>take:`.

**Performance:** wrapping adds a `try/catch` frame per FFI/`native:` call (`ensure_wrapped` runs only on the error path, never on success). For inline FFI there is **no new frame** — the proxy is already wrapped today (`beamtalk_erlang_proxy.erl:190`); Part 2 only broadens the existing catch from named clauses (`badarg`/`undef`) to a generic `error:*` catch-all, which the Erlang compiler may optimize slightly differently but does not add a frame. The frame is a constant amortized against coarse-grained FFI work (one call = an I/O op, ETS access, or an in-Erlang fold), and the hot fine-grained path (`@primitive`/`@intrinsic`) bypasses the proxy entirely. The only behavioral change is that `try/catch` defeats tail-call optimization on the wrapped call — a non-issue for `native:` methods, which bottom out in Erlang that loops internally. No fast-path carve-out in v1; the `native_call` helper is the seam to add one if profiling ever justifies it.

#### Interaction with `Result` (ADR 0076) and typing

Wrap-by-default touches the **exception channel**; ADR 0076's ok/error→`Result` coercion touches the **return-value channel**. They are orthogonal and **mutually exclusive per call** — a function either returns a value or raises — so they never collide:

| Outcome of an FFI call | Mechanism | Beamtalk result | Type-visible? | Handled with |
|---|---|---|---|---|
| *returns* `{ok,V}` / `{error,R}` | ADR 0076 | `Result(V, R)` — a **value** | **yes** (`-> Result(T,E)` from the FFI spec, ADR 0075) | `andThen:` / `unwrap` / `isOk` |
| *raises* `error:Reason` | ADR 0101 | `#beamtalk_error{}` — **raised** | **no** (out of band, like any exception) | `on:do:` / `ensure:`, or bubbles to REPL |
| *raises* `exit:Reason` | ADR 0101 | propagates past the proxy unwrapped; an enclosing `on:do:` still catches it and wraps it `erlang_exit`, else it reaches process death | no | `on:do:` (→ `#beamtalk_error{}`) or supervisor / let-it-crash if uncaught |
| *raises* `throw:Term` | ADR 0101 | passes through unchanged | no | required — `^` and Beamtalk exceptions are throw/catch in codegen |

The pipeline in `direct_call/3` makes the exclusivity explicit: `try V = apply(M,F,A), coerce_result(coerce_charlist(V)) catch error:E -> raise(ensure_wrapped(E)); exit/throw -> propagate end`. The coercion runs only if the call **returned**; the wrapping only if it **raised**.

**The disambiguator is returned-vs-raised, not tuple shape.** A function that *returns* `{error, enoent}` → `Result error: …`; a function that *raises* `erlang:error({error, enoent})` → a raised `#beamtalk_error{}`. Same tuple, different channel.

**Consequence to document (it surprises otherwise):** the *same* function may use both channels. `File readAll: path -> Result(String, Error)` *returns* `Result error: file_not_found` for the modeled domain failure, but *raises* `#beamtalk_error{}` if called with a non-binary path (`badarg`). The split is principled — **`Result` for expected/recoverable outcomes the API models, exceptions for misuse/faults** — but the user guide must state it so "File returns `Result`, why did it throw?" doesn't bite.

**Typing bottom line:** wrap-by-default changes **no return type**. `select: pred -> Stream(E)` stays `Stream(E)`; a raised `#beamtalk_error{}` is invisible to the type system. `Result` remains the *only* type-visible error channel, driven by the FFI spec, not by the wrapping.

**Foreign `throw` — the real hazard is over-catching, not a raw leak (pre-existing).** `throw` passes through *the proxy* unwrapped, but it does **not** surface raw to the user: `on:do:`'s generated catch preamble (`control_flow/exception_handling.rs:84-149`) catches every exception, re-raises only Beamtalk's own non-local-return throws (`{'throw', {'$bt_nlr', …}}`), and routes everything else — including a *foreign* Erlang `throw(Term)` — through `ensure_wrapped`, producing `#beamtalk_error{kind = erlang_throw}` (`beamtalk_exception_handler.erl:500`). So the genuine concern is the **opposite** of a raw leak: an Erlang library that uses `throw` as ordinary control flow (parsers, validators, parts of `cowboy`/`ecto`) can be **silently intercepted** by a user's broad `[ (Erlang …) … ] on: Error do: […]`. This is **pre-existing** `on:do:` behaviour, *not* introduced or worsened by wrap-by-default (the proxy change only touches `error:*`). Noted here because it's the real edge of the "never see raw Erlang" goal; a future refinement could have `on:do:` pass through non-`$bt_nlr` throws, but that's an `exception_handling` change out of scope for this ADR.

### Part 3 — Reclassify the 362 `@primitive`s

The deciding question is **not** "what is `self`" (an earlier draft used that and mis-routed the reflection classes). It is **what mechanism does the method need**:

- Needs **guarded dispatch + extension-registry fallback + error wrapping** (a native BEAM value, OR reflection that messages class processes and is runtime-extensible) → **`@primitive`**
- Is the **dispatch act itself** (`==`, `class`, `perform:`, DNU, actor lifecycle) → **`@intrinsic`**
- Is **pure stateless delegation** to one module, needing none of the dispatch layer → **`native:`**

This refinement is the key correction from review: `Behaviour`/`Metaclass`/`Class`/`Protocol` are reflection over class objects, but they **stay `@primitive`**, for three concrete reasons:
1. **Name mismatch** — their methods (`name`, `superclass`, `reload`) bind to Erlang functions named `className`, `classSuperclass`, `classReload` (`primitives/behaviour.rs`, `beamtalk_behaviour_intrinsics.erl`). The `native:` self-threading rule infers `mod:name(Self)`, which is the *wrong* function. `native:` would need a per-method name override (new surface we reject).
2. **Process-crossing** — `classReload`/`superclass` call into class gen_servers and the workspace via `gen_server:call` (`beamtalk_behaviour_intrinsics.erl:121,671`). They are not the in-process synchronous calls `native:`-Object lowering assumes; their `exit:*` (dead class process, timeout) would propagate raw under Part 2, where `@primitive` dispatch wraps it.
3. **Extension registry** — `@primitive` dispatch consults the open-world extension table (ADR 0007/0066/0070) as a fallback; `native:` direct calls bypass it, silently killing cross-package extensions targeting these selectors.

So `@primitive` is **not** "value types only" — it is "anything needing the guarded-dispatch + extension + wrapping layer." Only one class genuinely migrates from `@primitive` to `native:`: `ReactiveSubprocess` (an `Actor`, like `Subprocess`).

Two ergonomic cleanups land with this:

- **The `@primitive` selector string becomes optional.** ≥86% of the 362 strings literally restate the method's own selector (`size => @primitive "size"`). Bare `@primitive` infers the selector; `@primitive "other"` stays as the override for the ~14% genuine renames (`signal:` → `classSignal:`).
- **"Performance primitives" stay `@primitive`.** A handful (`Collection>>sum`, `inject:into:`) could be pure Beamtalk but are kept native for speed (the source says so). These remain `@primitive`; the ADR documents that *"native BEAM value type"* OR *"perf-critical fold over a native collection"* are both valid `@primitive` rationales. No new keyword.

Resulting `@primitive`-side split (362), **revised after review**:

| Disposition | Count | Classes |
|---|---|---|
| Stay `@primitive` (value types **+ runtime-extensible reflection**) | **342** | String 58, List 41, Float 37, Integer 34, **Behaviour 24**, Character 19, Set 15, Dictionary 13, Binary 11, Array 11, Exception 11, Tuple 8, StackFrame 8, Pid 8, **Metaclass 7**, Symbol 7, CompiledMethod 6, Reference 6, Collection 5, Port 5, **Protocol 4**, Block 2, **Class 1**, Value 1 |
| → `@intrinsic` (substrate) | **15** | ProtoObject 7 (`==`/`/=`/`class`/`doesNotUnderstand:`/`perform:*`), Actor 7 (`pid`/`monitor`/`stop`/`kill`/`delegate`/`isAlive`/`onExit:`), Object 1 (`class`) |
| → `native:` (clean Actor delegation) | **5** | ReactiveSubprocess 5 |

(The earlier draft moved 41 to `native:`; review showed 36 of those — the reflection facades — must stay `@primitive`. Only `ReactiveSubprocess` migrates.)

### Part 4 — `internal` seams dissolve the embedded-FFI category

The 45 embedded-FFI methods are already safe under Part 2 (wrapped in place). As an **optional clarity refactor**, extract each buried `(Erlang …)` call into a thin `internal` helper (the `internal` modifier already exists — `docs/beamtalk-language-features.md:3798`, package-scoped, compile-time only, still reflection-visible). The public method becomes pure Beamtalk:

```beamtalk
internal xrefImplementorsOf: sel :: Symbol -> List => (Erlang beamtalk_xref) implementors_of_bt: sel

implementorsOf: aSelector -> List(Behaviour) =>
  partition := self xrefImplementorsOf: aSelector   // pure BT — no FFI in the logic
  indexImpls := indexedRows collect: [:row | ... ]
```

This does **not** make `SystemNavigation` a `native:` class — it hits four backing modules (`beamtalk_interface`/`beamtalk_xref`/`beamtalk_extensions`/`beamtalk_class_registry`), and `native:` names one. But it drives the "embedded FFI in logic" category to **0**: every Erlang call site in the stdlib becomes a one-line seam.

### End state — the whole interop surface

| Mechanism | Methods | Meaning |
|---|---|---|
| **`@primitive`** | **342** | native BEAM value types, perf folds, runtime-extensible reflection (keeps dispatch + extension + wrapping) |
| **`native:`** | **310** (5 + 305) | pure stateless delegation to one backing module — grows from 2 classes to ~45 |
| **`internal` FFI seam** | ~45 | one-line `(Erlang …)`, wrapped-by-default (post-Part 4; embedded → 0) |
| **`@intrinsic`** | 15 (+39 existing) | dispatch / actor / block substrate |

`native:` does not become the single largest mechanism (`@primitive` stays ahead at 342) — but it goes from **2 classes to ~45**, absorbing every pure-delegate FFI method. Every Erlang call is then one of: a `@primitive`, a `native:` delegate, an `@intrinsic`, or a one-line `internal` seam. Nothing buried in logic; nothing leaking raw errors.

## Prior Art

- **Smalltalk (Pharo/Squeak)** — named VM primitives `<primitive: 'name' module: 'mod'>` with a Smalltalk fallback body. We keep the *named-primitive* idea for value types but deliberately reject the raw `module:` MFA escape hatch (ADR 0007) — `native:`/`@primitive`/`@intrinsic` all route through safety layers. Smalltalk has no equivalent of `native:`-class delegation; its primitives are per-method only.
- **Gleam** — `@external(erlang, "mod", "fn")` per-function FFI with mandatory type signatures and no automatic error wrapping. Our `native:` class-level form is more concise for the common "whole class wraps one module" case; our wrap-by-default differs from Gleam's "errors are values" stance because Beamtalk has exceptions and the `#beamtalk_error{}` contract.
- **Elixir** — `defdelegate` forwards a function to another module, the closest analogue to `native:` for stateless Objects. We adopt the spirit (declare delegation, don't hand-write each forward) and add self-threading inference + error wrapping.
- **Erlang/OTP** — `gen_server` is exactly what `native:` Actors wrap; the `sync_send` boundary mirrors `gen_server:call` error semantics.
- **Ruby / Python (mainstream native extensions)** — Ruby C extensions and Python C modules expose native code as ordinary methods/functions with no language-level distinction at the call site; the boundary is invisible and errors surface as normal exceptions. We adopt the *invisible-boundary* goal (a `native:` method looks like any method) but reject the *invisible-error-translation* gap — wrap-by-default guarantees a `#beamtalk_error{}` rather than leaking a native fault, which Ruby/Python extensions notoriously do (segfaults, untranslated C errors).

## User Impact

- **Newcomer** — sees one keyword (`native:`) for "backed by Erlang" instead of guessing between `@primitive` and `(Erlang …)`. Crucially, **a faulting call surfaces a structured `#beamtalk_error{}` at the REPL, not a raw tuple** — `error:*` and foreign `throw:*` are both wrapped (the latter by `on:do:`/top-level), so the host VM doesn't leak through the interactive surface. The one thing that still bypasses this is an *uncaught* `exit:` — but that's a process crash (a supervisor/crash-report event), not an error value printed at the prompt.
- **Smalltalk developer** — `native:` reads as message delegation; value-type `@primitive` preserves the "primitive method" tradition. Reflection over classes (`Behaviour`) stops masquerading as primitives.
- **Erlang/BEAM developer** — `exit`/`throw` still propagate (raw OTP semantics for ports, links, intentional exits preserved without ceremony); wrap-by-default means casual FFI no longer leaks `badarg`. Per-module hand-wrapping shrinks to domain hints only.
- **Operator** — no behavior change to `exit`/links (still propagate); fewer raw crashes surfacing from stdlib reflection paths.
- **Tooling developer** — `self delegate` `native:` methods are clean AST for completion/docs (signature + sentinel body); the `@primitive`/`@intrinsic`/`native:` split is now semantically meaningful for analysis.

## Steelman Analysis

### Alternative A — a distinct keyword `backedBy:` instead of reusing `native:`
- ⚙️ **BEAM veteran** (strongest case): "Debugging a production hang, `self delegate` on `Stream` and `TranscriptStream` read *identically* — but one is an in-process call that cannot block and the other crosses a process boundary that can block indefinitely, time out, raise `noproc`, and cross nodes. The single fact I need to reason about the hang — *does this call leave the process?* — is exactly what the shared keyword hides. `backedBy:` vs `native:` would put it in the source, one token, no jumping to the class header."
- 🏭 **Operator**: "Which methods can time out is a deploy-time risk question. A distinct keyword is a greppable signal of where the process boundaries are."
- 🎨 **Language designer**: "Two lowerings (`sync_send` vs direct call) with fundamentally different failure modes deserve two names; overloading `delegate` is a leaky abstraction by construction."
- 🧑‍💻 **Newcomer**: "Two names would tell me upfront whether a call is cheap-and-local or might hang."
- 🎩 **Smalltalk purist** (cuts the *other* way): "No — in Smalltalk everything is a message send; whether the receiver is a process or a module is precisely what I should *not* have to know at the call site. One keyword is the *more* Smalltalk answer."

**Rejected, on merits:** the process-boundary distinction is real, but it is *already* encoded by class kind — the reader sees `Actor subclass:` vs `Object subclass:` one line above. Duplicating it into the delegation keyword is redundant *and* re-creates the "two mechanisms for one concept" smell this ADR exists to delete. The leaky-abstraction cost is genuine and kept as an explicit Negative; the mitigation is docgen/hover/`:help` labelling which kind a `native:` class is — metadata, not a second keyword. The veteran and the purist pull in opposite directions; "class kind already says it" + symmetry + "parser already allows it" breaks the tie toward reuse.

### Alternative B — raw FFI by default, opt-in `SafeErlang` wrapping
- ⚙️ **BEAM veteran** (strongest case): "BEAM is let-it-crash. Supervisors are *designed* to see `error:*` and restart. Intercepting `error:*` at the boundary risks turning a clean crash-and-restart into a swallowed fault that limps on with a wrong answer — you're fighting the platform's core reliability model, and that's worse than an ugly tuple."
- 🏭 **Operator**: "`SafeErlang` is auditable — I can grep exactly which calls are protected. A blanket default hides where wrapping happens and adds a `try/catch` to every FFI call I never asked for."
- 🎨 **Performance / language designer**: "Every FFI call now pays `try/catch` + `ensure_wrapped` for a safety net most calls never trip. On a tight FFI loop that's measurable."
- 🎩 **Smalltalk purist** (for wrap-by-default): "A raw `badarg` tuple at the REPL is a layering violation — Smalltalk never shows you the VM. Wrapping is the *more* faithful choice."
- 🧑‍💻 **Newcomer** (for wrap-by-default): "`badarg` means nothing to me; a structured error with a hint is the only one I can act on."

**Rejected, on merits:** the let-it-crash worry conflates two layers. Wrap-by-default converts `error:*` into a *raised* `#beamtalk_error{}` — still an exception that propagates and kills the process if uncaught, so **supervision still fires**; and `exit`/links are untouched, propagating raw. The only thing that changes is the *shape* a caller sees *if it chooses to catch* — structured instead of raw. Auditability is real but inverts the cost onto the ~99% of sites that want safety (precisely the ADR 0007 anti-pattern); a future `@raw` marker covers the rare audited-raw case. Performance: the `try/catch` is cheap next to the cross-module call itself, and the genuinely hot path (value-type ops) is `@primitive`/`@intrinsic`, not FFI. The decisive cohorts are the newcomer + purist at the REPL — never leaking the VM is the ADR's primary motivation.

### Alternative C — status quo: keep three mechanisms, hand-fix only the 45 leak sites
- 🏭 **Operator/maintainer**: "Zero migration risk, no behavior change, no new surface — just add `try/catch` to the 45 leaky sites and move on."
- ⚙️ **BEAM veteran**: "Every wrap is explicit and visible; nothing is magic."

**Rejected, on merits:** cheapest *today*, most expensive *cumulatively* — it entrenches the `@primitive`/FFI duplication and the 305-method self-threading ceremony, and leaves safety as per-author discipline forever (forget once → a leak). It is genuinely available as a fallback (Parts 1–3 stand without Part 4), but it fixes the *smallest* smell while leaving the structural one.

### Tension points
- **Smalltalk purist vs BEAM veteran on wrapping** — purist wants the VM hidden (wrap); veteran wants let-it-crash honored (don't intercept). Resolved by wrapping `error:*` *as a raised exception* (supervision still works) while `exit`/links propagate untouched — each gets most of what they want.
- **Language designer vs Smalltalk purist on `native:` vs `backedBy:`** — designer wants the process boundary surfaced; purist wants it hidden ("all messages"). Resolved toward reuse: class kind already encodes it, and "all messages" is the more Smalltalk stance.
- **Operator auditability** — the one cost wrap-by-default does *not* fully answer: you can't grep which FFI is protected, because all of it is. Accepted, with a future `@raw` marker as the deliberate-unprotected escape if the need is shown.

## Alternatives Considered

See Steelman Analysis for `backedBy:`-vs-`native:` and raw-vs-wrapped defaults. Additionally:

### Status quo + targeted hand-wrapping
Leave the three mechanisms as-is; fix only the 45 embedded-FFI leak sites by hand-adding `try…catch → beamtalk_error:new` (matching existing curated wrappers in `beamtalk_file`/`beamtalk_atomic_counter`). Zero new surface, zero migration risk. **Rejected** because it entrenches the `@primitive`/FFI duplication and the 305-method self-threading ceremony, and leaves "safe FFI" as per-author discipline rather than a default. But note: since the ADR states the 45 are "already safe under Part 2," **Parts 1–3 are viable without Part 4** — Part 4 is explicitly optional cleanup, a valid stopping point.

### Codegen-only: optional `@primitive` selector, no `native:` for Objects
Deliver only the Part 3 de-ceremony (bare `@primitive` infers the selector) without introducing `native:` for non-Actor Objects. Smaller surface. **Rejected** because it leaves the 305 pure-delegate FFI methods stuck with hand-threaded `self` and no error-wrapping unification — it fixes the smaller smell and ignores the larger one.

### Per-method `@delegate mod` annotation instead of class-level `native:`
Allow `select: predicate => @delegate beamtalk_stream` per method rather than a class-level declaration. Directly analogous to Elixir's `defdelegate` (cited in Prior Art) and handles the multi-module case (e.g. SystemNavigation's four backing modules) without forcing one module per class. **Rejected for v1** — class-level `native:` is far less repetitive when a class delegates wholesale to one module (Stream, File), and first-keyword+`self` inference covers all 305 pure-delegate methods. Deferred (not dismissed): if selector/function-name mismatches prove common, `@delegate … as: fn` is the natural way to add per-method name-overrides without the reflection classes needing it.

### Consolidate SystemNavigation to one backing module
Make `SystemNavigation` `native: beamtalk_system_navigation` by introducing a facade module that fans out to interface/xref/extensions. **Rejected** — pushes Beamtalk-level fold/fan-out logic into Erlang (wrong layer) or is pure indirection. The `internal`-seam approach (Part 4) keeps orchestration in Beamtalk.

## Consequences

### Positive
- One concept (`native:`) for delegation; `@primitive` reserved for value types and runtime-extensible reflection (the dispatch + extension + wrapping layer); `@intrinsic` for substrate. The mechanism you see tells you what safety layer wraps the call.
- Wrap-by-default closes the raw-error leak for all FFI, not just curated dispatch — safety by construction, matching ADR 0007's guarantee for value types.
- ~305 pure-delegate FFI methods and ~41 reflection methods lose their per-line ceremony / self-threading.
- Embedded-FFI category → 0 (post-Part 4); every Erlang call is a one-line seam.
- Per-module hand-wrapping boilerplate can be deleted (kept only for domain hints).

### Negative
- Large mechanical migration: ~310 methods change mechanism, plus runtime modules adjusting to the `Fn(Self, Args)` convention where dispatch indirection is removed.
- **`self delegate` is a leaky abstraction, not just a subtlety.** Object delegation is an in-process synchronous call (cannot timeout, cannot raise `noproc`); Actor delegation crosses a process boundary (`sync_send`, can block indefinitely, timeout, raise `noproc`, cross nodes). The same keyword in `Stream.bt` and `TranscriptStream.bt` hides fundamentally different failure/performance/supervision profiles. **The sharper edge:** changing a pure-delegation class between `Actor subclass:` and `Object subclass:` (both with `self delegate` bodies) is a **silent semantic change** — no type error, no warning, but the lowering flips between `sync_send` (cross-process, can timeout/`noproc`) and `native_call` (in-process, cannot). Mitigation: docgen/hover/`:help` must label which kind a `native:` class is; a future lint could flag a kind-change on a `native:` class. The ADR keeps the single keyword only because the symmetry value outweighs the cost (see Steelman).
- **Wrap-by-default `exit`/`throw` rollback** changes the `validate_and_apply` path (see Part 2 table) — a behavior change, not pure addition.
- Auto-wrapped FFI errors carry Erlang MFA context, not the Beamtalk selector (less pretty than hand-rolled hints).
- **Part 4 inflates the xref index.** Extracting SystemNavigation's ~29 embedded calls into `internal` helpers nearly doubles its indexed method count (ADR 0087); `unusedSelectors`/dead-code queries will surface `internal` seam helpers that are never sent cross-class. Acceptable, but Part 4 must update any xref-based tooling expectations.

### Neutral
- Total method count is unchanged (Parts 1–3); this is reclassification + de-ceremony, not feature addition. Part 4 adds ~45 `internal` helper methods.
- The 39 existing `@intrinsic` bindings are unaffected.
- **Extension registry is preserved** where it matters: by keeping the reflection facades (`Behaviour`/`Metaclass`/`Class`/`Protocol`) on `@primitive` dispatch, no class loses its open-world extension hook (ADR 0007/0066/0070). The `native:`-migrated classes (`Stream`, `File`, …, `ReactiveSubprocess`) were pure-delegate FFI with no extension hook to lose.

## Implementation

Phased, bottom-up; each phase leaves CI green.

**Phase ordering note:** Part 2 must land first (or together with Phase 1) — `native:` Object dispatch lowers through the `native_call` helper that Part 2 adds, so Phase 1's generated code does not run end-to-end until Part 2 is merged. This is a hard dependency (BT-2720 blocked by BT-2722), not just the soft "failure-prone delegations want wrapping" ordering.

- **Phase 1 — `native:` for stateless Objects.** Add a `delegate` sentinel to the `Object`/`Value` base (parallel to `Actor`'s `sealed delegate => @primitive "actorDelegate"`, ADR 0056/BT-1208) so `self delegate` typechecks on a stateless Object. **Note the wider blast radius vs ADR 0056:** `Actor`'s `delegate` is visible only to Actor subclasses, but `Object`'s is visible to *every* class — verify no existing stdlib Object subclass already defines a `delegate` selector (confirmed: only `Actor.bt` does today) and treat `delegate` as a now-reserved selector on the Object protocol. **Also add a class-side `delegate` sentinel on `Object class` (the metaclass)** so class-method bodies using `self delegate` typecheck — Stream's constructors (`class from:`, `class on:`) need it. Codegen: add a stateless-`Object` branch so a `self delegate` method routes through the shared coercion+wrapping boundary carrying `{Class, Sel}` (see Part 1 — thread the context through `apply_with_coercion`, do not build a parallel path); it may skip the proactive export *pre-check* but **must still catch `error:undef`** (hot-reload TOCTOU) and surface a structured `does_not_understand`. Lift the actor-only validators. **Naming rule (normative — matches the existing FFI convention, `docs/beamtalk-native-erlang.md:74`):** the Erlang function name is the **first keyword with its colon removed**; `self` is prepended for instance methods; the remaining keyword *values* follow as positional args. So `select: pred` → `mod:select(Self, Pred)`, `inject: i into: b` → `mod:inject(Self, I, B)` (cf. the `inject/3` alias at `beamtalk_stream.erl:306`), unary `asList` → `mod:asList(Self)`. **Two non-applicable cases, by construction:** (a) selectors that collide under the first-keyword rule (getter `at:` and setter `at:put:` both → `at`) — but those are value-type `@primitive`s using full-selector dispatch, never `native:`-eligible; (b) the reflection classes whose Erlang names differ from the selector (`name`→`className`), which is why they stay `@primitive`. Verified: the rule matches all 305 pure-delegate FFI methods (they already write that mapping inline). **Inheritance:** `native:` delegation follows the class declaration, not inheritance — a subclass that does not redeclare `native:` inherits *no* backing module (it cannot add `self delegate` methods of its own). A subclass that redeclares `native: other_mod` uses `other_mod` only for the `self delegate` methods *it* declares. **Reserved words:** if a first keyword is an Erlang reserved word, codegen must emit a compile error rather than invalid output like `mod:receive(Self, X)` that would fail `erlc`. The check must use the **complete** Erlang reserved-word set (`after and andalso band begin bnot bor bsl bsr bxor case catch cond div end fun if let not of or orelse receive rem try when xor`), not an illustrative subset — and codegen already has the canonical list it uses when quoting atoms; reuse it. Migrate `Stream` as the worked example. *Files:* `codegen/core_erlang/mod.rs`, `codegen/core_erlang/gen_server/native_facade.rs`, `semantic_analysis/validators/native_validators.rs`, `stdlib/src/Stream.bt`.
- **Phase 2 — wrap-by-default FFI.** `beamtalk_erlang_proxy`: unify both apply paths to wrap `error:*` via `ensure_wrapped` and propagate `exit`/`throw` (rolling back `validate_and_apply`'s exit/throw wrapping). No new opt-out receiver. Grep for and migrate any call site relying on `exit`/`throw` being caught as `#beamtalk_error{}`. *Files:* `beamtalk_erlang_proxy.erl`, `codegen/core_erlang/`.
- **Phase 3 — reclassify the 362.** Optional selector string; relabel the 15 `@intrinsic` methods (ProtoObject/Actor/Object substrate); migrate only `ReactiveSubprocess` (5) to `native:`. Reflection facades stay `@primitive` (see Part 3). *Files:* `codegen/core_erlang/primitives/`, `primitive_bindings.rs`, the affected `stdlib/src/*.bt`.
- **Phase 4 — `internal` seams for embedded FFI.** Extract SystemNavigation's 29 (+ thin tail) into `internal` helpers. *Files:* `stdlib/src/SystemNavigation.bt` et al.
- **E2E** — btscript exercising `native:` Object delegation, a wrapped FFI error surfacing as `#beamtalk_error{}`, and an `(Erlang erlang) exit:` still propagating.

## Migration Path

- **`exit`/`throw`-dependent FFI** (e.g. `Erlang erlang exit:`) — unchanged; only `error:*` is wrapped. Audit for any code pattern-matching on raw `error` tuples returned from FFI (ok/error→`Result` value coercion per ADR 0076 is unaffected — that's return values, not exceptions).
- **Pure-delegate FFI classes** — mechanical: replace each `(Erlang mod) sel: self …` body with `=> self delegate`, add `native: mod`. ~40 classes convert wholesale (File, Tracing, Ets, …).
- **Reclassified primitives** — relabel keyword; behavior identical for value types staying `@primitive`.
- **Per-module wrappers** — delete generic `try…catch → beamtalk_error:new`; keep domain-hint wrapping.
- **`gen_server:call` timeout catches (named pattern).** The most common non-obvious break: Beamtalk code doing `(Erlang gen_server) call: Pid timeout: 5000` and catching the resulting `erlang_exit` (`{timeout,_}`) as a `#beamtalk_error{}` — safe under `validate_and_apply` today, but under Part 2 the `exit` propagates raw and terminates the calling Actor unless a supervisor catches it. This is let-it-fail-correct by design, but Phase 2 must grep for it specifically and either remove the catch (let the supervisor restart) or move to a timeout idiom that *returns* rather than exits. **BT-2723 specifies the replacement idiom** (see Resolved Decision 4 below) — Beamtalk has no documented bare monitor-timeout, and `Actor withTimeout:` spawns a proxy actor rather than a plain monitor; the issue owns deciding/sketching the canonical Beamtalk form before mechanically migrating the grep hits. Treat this as the headline case of the "grep for `exit`/`throw` catches" migration, not a rare one.
  - **Phase 2 grep outcome (BT-2723).** Grepping `stdlib/src`, `stdlib/test`, `examples` (`.bt`) and `runtime/apps/*/src` (`.erl`) for the pattern found **zero** sites that issue a raw `(Erlang gen_server) call: … timeout:` and catch its `exit` as a structured error. The reason is structural: no Beamtalk code calls `gen_server:call` through the FFI proxy — *all* synchronous actor messaging is lowered to `beamtalk_actor:sync_send/3,4`, which **already** converts `exit:{timeout,_}` into a *raised* `#beamtalk_error{kind = timeout}` (`beamtalk_actor.erl` `raise_timeout/1`) and never depended on the FFI proxy's exit-wrapping. BT-2722's rollback (proxy propagates `exit`) therefore leaves actor-call timeout behaviour unchanged, so the headline case is forward-looking guidance, not a live migration.
- **ADR 0100 sequencing** — no hard ordering. ADR 0100 is still *Proposed* (not implemented), so shipping ADR 0101 first introduces no false diagnostics (there's no classifier running yet). Whenever ADR 0100 is built, it must satisfy the coordination requirement (count `self delegate` `native:` methods as declared methods; treat `native:` Objects as `ClosedComplete`, `@primitive` as Open). Doing 0100 first is *not* required and would not de-risk 0101 — the coupling is a one-line rule in 0100's surface scan, cheapest to honor when 0100 is implemented.

## Resolved Decisions
These forks were raised in review and resolved:
1. **No `RawErlang` opt-out in v1.** `exit`/`throw` propagate by default, so the only remaining use was raw `error:*` tuples — too narrow to justify new receiver syntax now. A `@raw`/receiver escape is a documented future option. (See Part 2.)
2. **Class-level `native:` only; no per-method `@delegate`.** First-keyword + `self` inference covers all 305 pure-delegate methods (written with that same convention); the few selector/function-name mismatches stay inline `(Erlang …)` (wrapped-by-default). `@delegate` is deferred — revisit only if mismatches prove common.
3. **ADR 0100: coordination, not conflict.** `native:` Objects are legitimately `ClosedComplete` (no extension hook → a DNU hint on an unknown selector is *correct*). `@primitive` classes (incl. the reflection facades) keep their Open/extensible treatment. **Requirement on ADR 0100's implementation:** its method-surface scan must count `self delegate` `native:` methods as declared methods (so they don't read as "missing" and trigger false hints). Neither ADR blocks the other (see Migration Path).
4. **Canonical timeout idiom (BT-2723).** The Migration Path flagged the `gen_server:call` timeout replacement as unspecified. Resolved: **Beamtalk already has a structured-timeout path; no raw `exit`-catch idiom is introduced.**
   - A bounded synchronous send uses `anActor withTimeout: ms` (→ `TimeoutProxy`). A timeout surfaces as a **raised** `#beamtalk_error{kind = timeout}` (catchable as `#timeout`, under `Error`/`Exception`) — *not* a propagating raw `exit`. This is produced by `beamtalk_actor:sync_send/4` → `raise_timeout/1`, independent of the FFI proxy, so it is unaffected by Part 2.
   - **Result-returning form (canonical):** to convert a timeout *specifically* into a `Result error:` *value* (while letting every other failure propagate to a supervisor), guard on the `#timeout` kind and re-raise anything else. Wrap the success path in `Result ok:` so both branches share the `Result(Rows, Error)` type:
     ```beamtalk
     result := [ Result ok: ((db withTimeout: 30000) query: sql) ]
       on: Error
       do: [:e |
         e kind = #timeout
           ifTrue: [ Result error: e ]   // a timeout becomes a Result error: value
           ifFalse: [ e signal ] ]       // re-raise non-timeout errors (let-it-crash)
     // result : Result(Rows, Error) — `Result ok:` on reply, `Result error:` on timeout
     ```
     The `#timeout` guard matters: a bare `on: Error do: [Result error: …]` would also swallow `actor_dead`, validation faults, and any other crash surfacing through `sync_send`, defeating supervision. There is no dedicated `TimeoutError` class today — `kind = timeout` falls through to the `Error` class — so the kind check is how the idiom stays timeout-specific; a future `TimeoutError` subclass could replace the guard with `on: TimeoutError do:`.
   - A dedicated `callWithTimeout:` convenience helper that returns `Result` directly is **deferred** (recorded as an optional future addition): the `on:do:` form already composes the two existing, type-visible channels, and no current call site needs the sugar. Adding it later is non-breaking.
   - **Migration consequence:** because the only structured-timeout source (the actor path) already *raises* rather than propagating a raw `exit`, the Phase 2 grep for FFI exit/throw catches finds nothing to migrate (see Migration Path bullet). The idiom above is the documented canonical form for any future code that needs a value-returning timeout.

## References
- Related issues: BT-2719 (Epic) · BT-2720 · BT-2721 · BT-2722 · BT-2723 · BT-2724 · BT-2725 · BT-2726 · BT-2727
- Related ADRs: ADR 0007 (primitive injection / extension registry), ADR 0028 (BEAM interop strategy), ADR 0055 (Erlang-backed class authoring), ADR 0056 (native Erlang-backed actors), ADR 0066 (extension registry), ADR 0070 (cross-package extensions), ADR 0075 (FFI type definitions), ADR 0076 (ok/error→Result at FFI boundary), ADR 0087 (xref index — Part 4 interaction), ADR 0100 (open-world diagnostics — see Open Questions)
- Documentation: `docs/beamtalk-native-erlang.md`, `docs/beamtalk-language-features.md` (§ `internal` visibility, FFI)
