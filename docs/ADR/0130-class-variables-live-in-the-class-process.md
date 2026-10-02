# ADR 0130: Class Variables Live in the Class Process During an Invocation

## Status
Proposed (2026-10-02)

Supersedes ADR 0110 (Class-Variable Shadow Write-Through for Foreign NLR Relay) when implemented. Amends ADR 0013 §1 (class-variable storage), ADR 0084 (ClassBuilder class-method fun arity), ADR 0111 and ADR 0122 (removes the `ClassVars` threading family and slot family).

## Context

### Problem statement

Making class-side self-sends late-bound (BT-3666, 2026-09-30) was correct: a subclass override of a class method must be reached from an inherited method's `self foo`, which is the template-method pattern. It also made the class-variable implementation unsound in a way that has not converged. In the three days after BT-3666 merged, the follow-ups were:

| Issue | Shape | Outcome |
|---|---|---|
| BT-3667 | Override's write dropped inside blocks, loops, arms | Fixed by reading the ADR 0110 shadow mid-chain |
| BT-3675 | That fix resurrected writes of callees that raised and were caught | Replaced with per-scope tokens (`make_ref`, commit, export, take, read) |
| BT-3681 | Stored closure invoked by a later statement drops the write | Not fixed; compiler warning added |
| BT-3682 | Block passed to a class-side higher-order method drops the write | Not fixed; needs a three-way merge, not implemented |
| BT-3683 | Write lost when a later iteration's arm is skipped | Fixed by a scope-read fallback chain |
| BT-3688 | False positives and gaps in the BT-3681 warning | Fixed |
| BT-3691 | Sealed class: writing arm plus provably pure arm in a fold loses the write | Open; four tests pin the wrong answer |
| BT-3693 | `verify()` fails on about 20% of generated class methods | Open |
| BT-3694 | `self foo` in a `whileTrue:` condition emits unbound `State` | Open |
| BT-3696 | Advisory misses sends inside a `class sealed` method of an open class | Open |
| BT-3669, 3676, 3690, 3692 | Open class-side self-send went from about 100-120 ns to 350-470 ns | Partly recovered to about 215 ns; target not met |

At least 21 BUnit tests in `stdlib/test/self_send_override_blocks_test.bt` and `stdlib/test/self_send_plain_reply_test.bt` pin answers that the language documentation says are wrong (3 `PIN-BUG BT-3682`, 4 `PIN-BUG BT-3691`, and 14 stored-closure and section-literal limits). ADR 0110 now carries two "known limit" amendments that are dropped writes.

Each fix was correct for the shape in its test and the next nesting broke. This ADR is about why, and about changing the representation so that the bug class cannot be expressed.

### How class variables work today

A class's class variables are one flat map, keyed by variable name, in the `class_state` field of the class's `gen_server` (`beamtalk_object_class`, `#class_state{}`). Between invocations that map is the source of truth, mirrored to the ETS table `beamtalk_class_state_snapshot` after every change.

During an invocation the map is threaded as a value:

- Every compiled class method is `class_<sel>(ClassSelf, ClassVars, Args...)`. It returns the bare result, or `{class_var_result, Result, ClassVarsN}` when it may have written.
- `self.n` lowers to `maps:get(n, ClassVarsN)` and `self.n := v` to `ClassVarsN+1 = maps:put(n, v, ClassVarsN)`, using the versioned `ClassVars` family in `ThreadedIr` (ADR 0111).
- A class-side self-send passes the caller's current `ClassVarsN` and rebinds from the reply. All class-side self-sends are in-process function calls; none goes through the `gen_server`.
- `invoke_class_method/7` (`beamtalk_class_dispatch.erl`) stores the returned map as the new `gen_server` state on normal return and replies with the pre-call map on a genuine error.

Since BT-3666 the callee's write set is unknown at the call site, so every send must hand its returned map back to the enclosing scope. A scope that can return a value lexically (a straight-line statement, a `Letrec` loop with a `ClassVars` parameter, a fold with a `{Acc, ClassVars}` accumulator) threads it. A scope that cannot (a block literal compiled as a closure, an arm nested in a fold, an `on:do:` handler, a stored closure, a block given to a user higher-order method) needs an out-of-band channel. Today that channel is:

1. **The ADR 0110 shadow**, `{'$bt_class_vars_shadow', ClassTag}` in the process dictionary, written at every top-frame write and read by `invoke_class_method/7` on a foreign non-local-return relay.
2. **The BT-3675 commit map**, `'$bt_class_vars_commit'`, a map from a per-scope `make_ref()` token to a `ClassVars` value, with `class_var_scope_commit/3`, `read/3`, `take/3` and `export/3`.
3. **A pre-call sync** before each confined send, `class_var_scope_read(ClassSelf, [innermost..outermost tokens], ClassVarsN)`, minted as a real `Bind`.
4. **A post-scope refresh** after each confined scope, `class_var_scope_take` with a `class_var_scope_read` fallback (BT-3683).
5. **A conditional commit** that only commits a `class_var_result` reply (BT-3690), except when an argument contains a block literal.
6. **A purity analysis** (`compute_class_var_mutating_selectors`) that lets a sealed class skip tokens for provably pure sends.

An open class's `self foo` inside a `do:` arm now emits, per iteration: a `make_ref`, a scope read over two tokens, the `class_self_direct_ok` guard (three `persistent_term` reads), the call, two `case`s on the reply, a conditional commit, a `take` with a nested `read` fallback, and a commit to the enclosing token. The surveys for this ADR counted about 2,500 to 3,500 lines of codegen source (roughly half comments) and about 170 runtime lines that exist to serve this, with 69 scope call sites across 11 codegen files.

### Why it does not converge

The class variables have four representations during one invocation: the lexical `ClassVarsN`, the shadow, the commit map, and the `gen_server` state. Correctness requires that, at every read, the newest of them wins. The commit map has no recency order between a fold's returned accumulator, a scope commit, and the lexical copy (BT-3691 is exactly that: the fold's stale accumulator is committed over a newer export). Every new nesting of scope kinds is a new pairwise merge case, and the number of cases grows with the product of the scope kinds.

The ThreadedIr verifier cannot help. It checks that versions are bound and linear (`UnboundVersion`, `NonLinearVersion`), not that the value threaded is the newest one. A stale-but-well-formed thread is valid Core Erlang that computes a wrong answer, which is what most of the bugs above are.

ADR 0110 considered the alternative that removes the problem: keep class variables in one place during the call (its Option B). It rejected B because "the real cost of B is the codegen migration ... an L-sized refactor of working, tested code for the same observable fix". That reasoning held when self-sends were statically bound and the only gap was a foreign non-local return. It no longer holds: the threading is no longer working, more than an L has been spent patching it since, and ADR 0110's own steelman conceded that "revert-on-error would be nearly free under Option B too".

### Facts that make a single home cheap

From the runtime and codegen surveys done for this ADR:

- **Every class-side self-send already runs in the class's own process**, as a direct call or a hierarchy walk (`class_self_send/4`). Only sends to *other* classes hop to another `gen_server`.
- **The class variables are already one map per class, already in the class process.** Between invocations they live in the `gen_server` state.
- **The pattern already exists twice.** ADR 0110 keys a process-dictionary entry by class tag, and actors keep `'$bt_actor_state'` in the process dictionary during a call for re-entrant self-dispatch (`beamtalk_actor:self_dispatch/2`, `restore_dispatch_pdict/1`).
- **Classes with class state are never direct-called** (ADR 0129 Phase 0b, `compute_direct_call_eligible` gate 2). A class method that touches class variables runs inside its class's process, with three exceptions, all runtime-owned: a block carried into another process (ADR 0109); `performLocally:withArguments:` (`beamtalk_object_class:local_call/3`), which today passes an empty map so a read fails with a raw `badkey`; and supervisor definition, where `beamtalk_supervisor:static_init/2`, `dynamic_init/2` and the `withClassMethod:` child factory call `class children`, `class strategy`, `class maxRestarts`, `class restartWindow` and the factory selector in the **supervisor process** against a copy read from the ETS snapshot, discarding any writes, so that a value set by an earlier `configure:` call is visible to `class children`. Those sites need a defined path under this ADR (§5).
- **The class process already keeps per-invocation facts in its dictionary.** Besides the ADR 0110 shadow, `beamtalk_class_name`, `beamtalk_class_module` and `beamtalk_class_is_abstract` live there for in-process `new`/`spawn` (`handle_self_instantiation`).
- **Class pids are not stable.** Class processes are `temporary` children; crash recovery is a fresh process via `beamtalk_class_registry:restart_class/1`, and `class_send_with_recovery` already rewrites the pid in a `ClassSelf` on recovery. A closure that captured a `ClassSelf` before a restart carries a stale pid, which rules out pid equality as the "am I at home" test.

### Constraints

- An error that escapes a class-method invocation must leave the class variables as they were before that invocation (ADR 0110's constraint, and how the `gen_server` reply has always worked).
- A foreign non-local return that passes through a class method keeps the writes made before it (ADR 0110's fix).
- Classes without class variables must not pay for this in compiled code. A per-invocation cost in `invoke_class_method/7` is acceptable if it is small next to the `gen_server` round trip every such invocation already pays.
- The block-runs-where-invoked semantics of ADR 0109 stay as they are; this ADR decides what class-variable access means for a block that runs in another process.
- Hot code upgrade (ADR 0125) must not mix old and new calling conventions silently.
- The language should keep one model of mutable state per kind of thing (ADR 0013 chose the `gen_server` for class variables partly for its "identical model to actor state (less to learn)").

## Decision

### 1. One home

During a class-method invocation, a class's class variables live in exactly one place: the dictionary of the class's own process, under the key `{'$bt_class_vars', ClassTag}`. There is no lexical copy, no shadow, and no commit map.

`invoke_class_method/7`, `invoke_class_extension/7` and the metaclass path install the stored map on entry and read it back on exit:

```erlang
invoke_class_method(Selector, Args, ClassName, _Module, DefiningClass, DefiningModule, ClassVars) ->
    Key = beamtalk_class_vars:key(ClassName),
    beamtalk_class_vars:assert_absent(Key),      %% check first: a present key is a nested invocation
    put(Key, ClassVars),
    try apply_class_method_in_context(Selector, Args, ClassName, DefiningClass, DefiningModule) of
        test_spawn -> test_spawn;
        {ok, Result} -> {reply, {ok, Result}, get(Key)};
        {nlr_relay, Nlr, _ST} -> {reply, {error, Nlr}, get(Key)};   %% writes before a foreign ^ are kept
        {error, Error} -> {reply, {error, Error}, ClassVars}       %% escaping error: pre-call map
    after
        erase(Key)
    end.
```

**Invariant: the key is absent on entry**, checked before anything is installed. Today nothing nests `invoke_class_method/7` for one class (it is reached only from the class's `handle_call`, own-class sends from inside raise `dispatch_error`, and `new`/`spawn` short-circuit through `handle_self_instantiation`). `assert_absent/1` raises a structured internal error when the key is already present, before `put/2` runs, so a future refactor that makes nesting reachable fails loudly with the outer map intact instead of overwriting it; with the invariant checked first, `after` can simply erase. (`beamtalk_actor:restore_dispatch_pdict/1` saves and restores instead because actor self-dispatch does nest; class invocations must not.)

Between invocations nothing changes: the `gen_server` state holds the map and the ETS snapshot mirrors it.

### 2. Access is a call into one runtime module

A new runtime module, `beamtalk_class_vars`, owns the key and every access. Codegen emits calls to it and never builds the key itself, so no rule crosses the Rust and Erlang boundary.

| Beamtalk | Core Erlang emitted |
|---|---|
| `self.n` | `call 'beamtalk_class_vars':'get'(ClassSelf, 'n')` |
| `self.n` where `n` is `late classState:` (ADR 0124) | `call 'beamtalk_class_vars':'get_late'(ClassSelf, 'n')`, raising `class_var_uninitialized` as the guarded `maps:find` does today |
| `self.n := v` | `call 'beamtalk_class_vars':'put'(ClassSelf, 'n', V)` |
| `self clearField: #n` | `call 'beamtalk_class_vars':'clear'(ClassSelf, 'n')` |
| `self hasField: #n` | `call 'beamtalk_class_vars':'has'(ClassSelf, 'n')` |

Each helper derives the key from `ClassSelf`'s class tag and looks it up in the current process. **Key presence is the "at home" test, not pid equality.** A foreign process never has the home class's key (the home process is blocked in the `gen_server:call` that carried the block away), the class process outside an invocation has none either, and a nested invocation at home has it. So `get/1` answering `undefined` raises the structured error in §5, and nothing else is compared. This keeps a closure that captured a `ClassSelf` before a class-process restart working at home, and makes the direct-called path's `ClassSelf = nil` trivially safe (it has no class variables to access). Phase 0 measures whether the helpers should be inlined as `erlang:get/1` plus `maps:get/2` instead.

### 3. Class methods neither take nor return class variables

The compiled calling convention becomes `class_<sel>(ClassSelf, Args...)`, returning the bare result. `{class_var_result, _, _}` is deleted from codegen and runtime. A class-side self-send of any kind (sealed or open, direct or walked, `super`, own-class `Base foo`) passes nothing and rebinds nothing. Sealing a class changes how a send is dispatched and nothing about class variables.

Codegen no longer has a `ClassVars` threading family. Loops, folds, arms, handler arms, blocks and stored closures need nothing for class variables, because a write is a `put` wherever it happens.

### 4. Error semantics: writes take effect when they are made

A class-variable write takes effect immediately, as in Smalltalk.

- **An error that escapes the invocation** discards everything the invocation wrote: `invoke_class_method/7` replies with the pre-call map. Unchanged.
- **A caught error does not undo writes.** `[self bumpThenFail] on: Error do: [:e | nil]` keeps the write that `bumpThenFail` made before it raised. This reverses the behaviour BT-3675 introduced and the language documentation now describes.
- **A foreign non-local return** keeps the writes made before it. Unchanged, and now needs no special path.
- **An own non-local return** returns from the method; the writes are already in place. Unchanged.

This is a deliberate semantic change, discussed under Alternatives (per-send rollback) and Migration Path. It has one asymmetry that must be named: **the external send is the transaction boundary; the method body is not.** `[X failingBump] on: Error do: [:e | nil]` evaluated outside `X`'s process (the REPL, an actor) rolls the write back, because `X failingBump` is a `gen_server` call whose error reply carries the pre-call map; the same expression inside one of `X`'s class methods keeps it, because `self failingBump` is an in-process call. Today both revert. An `ensure:` cleanup write made while an error escapes the invocation is discarded with everything else, as today.

### 5. A block reads and writes its home class's variables only at home

A block literal written in a class method reads and writes the live class variables whenever it runs in its home class's process. That covers loops, conditionals, `do:`/`collect:` and every other instance-side collection method, `ensure:`/`on:do:`, `Result tryDo:`, stored closures, a block passed to a user-defined class-side higher-order method of the same class, and a block passed to a direct-called (`class sealed`, stateless) class method of another class.

A block that runs in another process cannot reach its home class's variables: the home process is blocked in the `gen_server:call` that carried the block away. Today the compiler rejects a direct class-variable write in any block that is not inlined (`FieldAssignmentInUnsupportedBlock`), and a read sees the value captured when the block was created. Under this ADR those compile-time rejections go away, because a block that runs at home can now write. In exchange, any class-variable read or write from another process raises at run time:

```erlang
#beamtalk_error{kind = class_state_unreachable, class = 'Counter', selector = n,
                hint = <<"A block that reads or writes Counter's class variables ran in another "
                         "process (it was passed to another class's class method, an actor, or "
                         "performLocally:). Read the value into a local before passing the block, "
                         "or return the value and assign it in Counter's own method.">>}
```

**Runtime-owned out-of-process invocations get a read-only snapshot.** Two runtime paths deliberately run a class method outside the class process: supervisor definition (`beamtalk_supervisor:static_init/2`, `dynamic_init/2`, the `withClassMethod:` factory) and `performLocally:withArguments:` (`beamtalk_object_class:local_call/3`). Both wrap the call in `beamtalk_class_vars:with_snapshot(ClassSelf, Fun)`: if the key is already present (the caller is the class process mid-invocation) `Fun` runs against the live map; otherwise the ETS snapshot (`beamtalk_class_state_snapshot`, the map as of the last completed invocation) is installed under the key, marked read-only, for the duration of `Fun`, and removed after. Reads see what `class children` needs to see today. A write through a read-only snapshot raises `class_state_read_only` with a hint naming the entry point, instead of being silently discarded as the supervisor does today or failing with `badkey` as `performLocally:` does. This is Alternative E restricted to runtime-owned call sites that already have snapshot semantics; user code never gets a snapshot.

`local_call/3`'s doc contract changes from "class-variable mutations are discarded" to: reads see the snapshot, writes raise, and a `performLocally:` reached from inside the class's own method mid-invocation reads and writes the live map. The helpers never accept `nil`: a `nil` receiver is an internal error, not a reachable-state question.

Where the compiler can see the case, it warns instead of waiting for run time: a block literal that reads or writes class variables, passed directly as an argument to a class-side send whose receiver is statically another class that has class state or whose method is not `class sealed`, gets a `class-state-abroad` warning at the block. This is a lint, not a guarantee; a block passed through a variable or an instance method is only caught at run time.

### 6. What is deleted

- Runtime: the ADR 0110 shadow key and its `nlr_relay` read, the commit map and `class_var_scope_commit/read/take/export`, `erase_class_var_scratch/1`, the `class_var_result` arms in dispatch and in `unwrap_self_dispatch_outcome/3`, and the macros in `beamtalk.hrl`.
- Codegen: `VersionPrefix::ClassVars` and its counter, scope tokens, marks and regions (`generator/version.rs`), `class_var_for_send`, `emit_class_var_result_unwrap` and the pure-send variant, `confined_class_var_refresh_stmt`, `commit_live_class_var_doc`, the `{Acc, ClassVars}` fold accumulator, the `Letrec` `ClassVars` parameter, the shadow-write flag on `Bind`, and the `has_class_vars` field of `NlrBoundary::ClassMethod`.
- Verifier: `ShadowWriteMissing` and the `shadow_write_eligible` stack.
- Diagnostics: `ClassVarAssignmentInThreadedBody`, `ClassMethodSelfSendInThreadedLoopBody`, `ClassMethodSelfSendInUnthreadedBlock`, `ClassVarMutationLostAcrossNestedLoop`, and the class-variable case of `FieldAssignmentInUnsupportedBlock`.
- Analysis: the BT-3681 stored-closure advisory, and `compute_class_var_mutating_selectors` wherever it only gates class-variable threading.
- Runtime, other: the supervisor's `class_var_result` unwrapping and snapshot plumbing (`beamtalk_supervisor.erl`, replaced by `with_snapshot/2`), `put_class_method/4`'s fixed `n+2` fun arity for ClassBuilder funs (ADR 0084; becomes `n+1`), and about 30 EUnit pattern matches on `class_var_result` across `beamtalk_class_dispatch_tests.erl`, `beamtalk_supervisor_tests.erl`, `beamtalk_class_dispatch_test_helper.erl` and `beamtalk_object_class_tests.erl`.
- Docs: the ADR 0110 amendments and known limits, the class-variable caveats in `docs/beamtalk-language-features.md` § Passing Blocks Through Class Methods, the `performLocally:` paragraph, the Erlang FFI rule in `docs/development/erlang-guidelines.md` that tells a hand-written class method to return `class_var_result` and write the shadow (replaced by "use `beamtalk_class_vars`"), and the fun-shape documentation in `stdlib/src/class_builder.bt`.

Actor state, value-type `Self` threading, local-variable threading and non-local return are unchanged. The `ThreadedIr` keeps every family except `ClassVars`.

### REPL session

```beamtalk
Object subclass: Counter
  classState: n = 0
  class bump -> Integer => self.n := self.n + 1
  class hook => nil
  class run =>
    b := [self hook]
    #(1, 2, 3) do: [:i | i > 1 ifTrue: [self bump]]
    b value
    self.n

Counter subclass: LoudCounter
  classState: n = 0          // class variables are per class, not inherited (ADR 0013)
  class hook => self bump

Counter run        // => 2
LoudCounter run    // => 3
```

On today's compiler (`main` at `14799bd`), this class does not compile: the loop body is rejected with "Cannot send 'bump' to self inside this block ... this block has no way to thread such a mutation back", and `b := [self hook]` gets the `stored-closure` warning. With the documented workaround (an unused local mutated in the loop body), it compiles and `LoudCounter run` answers 2, because the stored closure's write is dropped (run on `main` at `14799bd`; the same shape is pinned by `testStoredClosureInvokedLaterLosesWrite`).

### Error examples

```beamtalk
Object subclass: Tally
  classState: total = 0
  class add: x => self.total := self.total + x
  class addAll: items =>
    Batch each: items do: [:x | self add: x]
    self.total

Object subclass: Batch
  classState: runs = 0
  class each: items do: aBlock =>
    self.runs := self.runs + 1
    items do: aBlock
    nil

Tally addAll: #(1, 2)
// => ERROR: class_state_unreachable: a block that reads or writes Tally's class variables ran in another process ...
```

Today this compiles without a warning and answers 0: the block runs in `Batch`'s process, `self add: x` runs `Tally`'s method there against a copy of the map, and the writes are silently lost (run on `main` at `14799bd`; a direct `self.total := ...` in the block is instead rejected at compile time). Under this ADR the `class-state-abroad` lint warns at the block, and the send raises when the block runs in `Batch`'s process.

```beamtalk
Object subclass: Ids
  classState: next = 0
  class take =>
    self.next := self.next + 1
    self error: "boom"
  class tryTake =>
    [self take] on: Error do: [:e | nil]
    self.next

Ids tryTake        // => 1
```

Today this answers 0 (run on `main` at `14799bd`): the write made before the caught raise is discarded. Under this ADR it is kept.

## Prior Art

**Pharo / Squeak / GNU Smalltalk.** A class variable or class-instance variable is a slot of a shared object. Assignment takes effect immediately and nothing is rolled back by an exception, caught or not. This ADR adopts that for writes inside an invocation. It keeps Beamtalk's one difference: an error that escapes the whole class-method call discards its writes, because the class is a `gen_server` and the reply is a transaction boundary.

**Erlang/OTP.** A `gen_server` callback's new state takes effect only when the callback returns, so an escaping error leaves the state unchanged; that is the boundary this ADR keeps. Inside a callback, the process dictionary is the idiomatic process-local mutable store for data a call chain needs without threading it (`$ancestors`, `logger` metadata, `rand` seeds). This ADR uses it the same way, scoped to one callback and erased in `after`.

**Beamtalk actors.** Actors already hold `'$bt_actor_state'` in the process dictionary during a call for re-entrant self-dispatch. Actor state is still threaded functionally, and an actor self-send that raises returns the pre-call state (`safe_dispatch/3`'s error arm), so actors roll back a raising send's writes. Class variables will not. See Consequences.

**Ruby and Kotlin.** Ruby class instance variables and Kotlin companion-object fields are plain mutable fields. Writes are immediate and survive caught exceptions.

**Clojure refs and Haskell STM.** Transactional memory gives each transaction a consistent snapshot and discards a failed transaction's writes. That is the model the current token design approximates per scope. It is rejected as a model for class variables because class methods run serially in one process, so there is no concurrency for transactions to resolve, only error rollback, and the escaping-error case already has a boundary.

## User Impact

**Newcomer.** Class variables behave like variables: a write is visible to the next line, inside a loop, inside a block and after a helper method returns. The stored-closure warning and the "give the loop body a local to thread alongside the class variable" advice disappear. The new error appears only when a block is carried to another process, and its hint says what to do.

**Smalltalk developer.** This is the Pharo model. The template-method pattern works with class-side state in every position, and refactoring a write into a helper method no longer changes whether it survives. The one difference from Pharo is that an error escaping a class method discards that call's writes, which is a strict improvement they will rarely notice.

**Erlang/BEAM developer.** The generated code is simpler: a class method is a plain function of `ClassSelf` and its arguments, and `self foo` is a plain call. The process dictionary is used inside one `gen_server` callback, checked absent on entry and erased in `after`; the actor's `'$bt_actor_state'` saves and restores instead because actor self-dispatch nests. An Erlang-implemented class method can read and write class variables through `beamtalk_class_vars` instead of producing `class_var_result` tuples.

**Production operator.** The class variables are inspectable mid-call with `erlang:process_info(Pid, dictionary)` and between calls with `sys:get_state/1`, as today. Open class-side self-sends should return to about the pre-BT-3666 cost, to be measured in Phase 0. A module compiled with the older calling convention is refused at load with `abi_mismatch`, and upgrading across this release needs a node restart (see Implementation).

**Tooling developer.** The type checker is unaffected: class-variable reads and writes keep the same AST and types. The LSP's diagnostic set changes: a whole class of codegen diagnostics disappears and one lint appears, on every surface equally (`docs/development/surface-parity.md`). The new runtime errors are `class_state_unreachable` and `class_state_read_only`.

## Steelman Analysis

### Alternative A: Keep the current design and fix the open bugs

| Cohort | Strongest argument |
|---|---|
| Newcomer | "The open bugs are corner cases: stored closures, sealed folds with pure arms. I will never hit them." |
| Smalltalk purist | "The current semantics roll back a send that raised. That is more careful than Pharo." |
| BEAM veteran | "Functional threading is the Erlang way. A process dictionary hides data flow from the reader and from tools." |
| Operator | "The current code is in production and tested by hundreds of BUnit cases. A rewrite resets that." |
| Language designer | "The verifier and token design are general machinery; finishing them is cheaper than replacing them." |

**Why not.** The chain has produced a new bug per nesting for three days, and the remaining open shapes (BT-3691, BT-3693, BT-3694, BT-3682) need new merge rules, not fixes. The verifier cannot see stale-but-well-formed values, so the tests are the only defence, and generated programs find failures in about 20% of class methods.

### Alternative B: Make class variables a first-class `State` family on the ADR 0041 block protocol

Treat a class method's class variables exactly like an actor's `State`: every stateful block, including stored closures and blocks passed to user higher-order methods, takes and returns them (`fun(Args..., StateAcc) -> {Result, NewStateAcc}`), and every late-bound class-side self-send returns `{Result, NewClassVars}` the way `safe_dispatch/3` returns `NewState`.

| Cohort | Strongest argument |
|---|---|
| Newcomer | "Class variables would behave exactly like actor state. One model to learn." |
| Smalltalk purist | "Message sends stay pure functions of their inputs; no hidden global." |
| BEAM veteran | "No process dictionary. Everything is visible in the generated code and checked by the verifier. This is the general fix in `ThreadedIr` that the project's own guidelines ask for." |
| Operator | "Behaviour stays transactional per send, so no existing test changes its answer." |
| Language designer | "It unifies two threading pipelines into one instead of adding a third representation." |

**Why not.** It keeps the property that causes the bug class: every scope kind must return the newest map, so correctness still depends on every scope kind being threaded, and the actor pipeline has needed its own fixes for the same shapes (BT-3580, BT-912). It is the most expensive option, because every class method body moves onto the actor `StateAcc` protocol, and the per-send reply tuple keeps the self-send cost near the open actor's (~120 ns), not the baseline. It is the right question to ask about actor state too, which is why this ADR records it as a follow-up rather than a reason to keep class variables threaded.

### Alternative C: One home, plus per-send rollback

Adopt §1 to §3 and §5, but keep BT-3675's semantics: each class-side send snapshots the map before the call and restores it if the callee raises (non-local returns pass through without restoring), and `on:do:` in a class method snapshots at entry and restores before running the handler.

| Cohort | Strongest argument |
|---|---|
| Newcomer | "No existing answer changes. My tests stay green." |
| Smalltalk purist | "A failed message leaves no trace. That is cleaner than Pharo." |
| BEAM veteran | "A `try` around a call is a few nanoseconds. The snapshot is one `get`." |
| Operator | "Class variables and actor state agree on what a raising send does." |
| Language designer | "It is still local and compositional: the rule lives at two constructs, not at every scope." |

**Why not.** Three costs. (a) Per-send rollback has to be emitted by codegen at every class-side self-send, direct call and hierarchy walk alike, which is exactly the layer this ADR deletes; the runtime cannot do it, since self-sends never pass through `invoke_class_method/7`. (b) The snapshot is a second representation of the map again, with the non-local-return pass-through recency question reintroduced: a `^` through a rolled-back frame must decide which copy wins. (c) The rule only holds where Beamtalk code catches: a write made directly inside a block and followed by a raise that `Result tryDo:` (a native-backed sealed value whose catch is outside any Beamtalk lowering), an Erlang `catch`, or any other catcher swallows is kept, while the same write moved into a helper method is rolled back, so the outcome depends on whether a write sits in a method or a block.

### Alternative C′: one home, plus rollback at `on:do:` handler entry only

Snapshot the map when an `on:do:` inside a class method enters its protected block, and restore it before running the handler. No per-send rollback.

| Cohort | Strongest argument |
|---|---|
| Newcomer | "`on:do:` is where I expect a failed attempt to be undone. That is the one place that matters." |
| Smalltalk purist | "It is explicit and local: the construct that catches is the construct that restores." |
| BEAM veteran | "Two codegen sites, one `get` and one `put`. No second representation outside the handler construct." |
| Operator | "Actor and class state agree for the common `on:do:` case, and the inside/outside-process asymmetry in §4 disappears for it." |
| Language designer | "It removes the method-versus-block inconsistency of C, because the rule is attached to the catcher, not the write." |

**Why not, by default.** It keeps writes swallowed by `Result tryDo:`, `ensure:` cleanup paths, Erlang catchers and handlers written in another class, so "a caught error undoes writes" is still only true for one construct, and the §4 asymmetry remains for every other catcher. It is small (two codegen sites) and compositional, which is why Open Question 1 is between this ADR's §4 and C′, not between §4 and C.

### Alternative D: ETS-backed class variables (ADR 0013's deferred option)

Store class variables in a per-class ETS table read and written directly from any process.

| Cohort | Strongest argument |
|---|---|
| Newcomer | "A block passed anywhere can read and write them. No new error." |
| BEAM veteran | "Concurrent reads at ~100 ns and no `gen_server` bottleneck; ADR 0013 already listed this as the scaling path." |
| Operator | "Visible in `observer` without attaching to a process." |
| Language designer | "Location-independent state is simpler than process-scoped state." |

**Why not.** It gives up the escaping-error rollback (an ETS write is visible immediately to everyone), so a partially failed class method leaves torn state visible to other processes. It turns class methods from serialized to concurrent, which is a language change of its own. It is a scaling decision for ADR 0013 to revisit, not a fix for this bug class.

### Alternative E: Snapshot reads from other processes

Adopt this ADR, but let a block running in another process *read* the class variables as of the last committed invocation (the ETS snapshot) or as captured at block creation, and raise only on writes.

| Cohort | Strongest argument |
|---|---|
| Newcomer | "Reading a setting from a block should just work." |
| Operator | "Existing code that only reads will not start failing." |

**Why not, by default.** A read from the ETS snapshot misses writes the home invocation has already made, and a creation-time capture brings back a second representation. Both are silently stale, which is the bug class. Phase 0 counts how often existing code reads class variables from another process; if it is common, E is the fallback for reads.

### Tension points

- BEAM veterans prefer B (no process dictionary, everything in the verifier); Smalltalk developers and newcomers prefer this ADR (Pharo semantics, no limits). The deciding fact is that B keeps the per-scope "newest value" obligation that has failed repeatedly.
- Operators and anyone with existing tests prefer C or C′ (fewer answer changes). C's cost is a second representation and a rule that depends on whether a write is in a method or a block; C′'s cost is that only one catcher restores.

## Alternatives Considered

The five options are described with their steelmen above. In short:

- **A, status quo:** rejected; it does not converge, and the verifier cannot detect the failure mode.
- **B, thread class variables on the ADR 0041 protocol:** rejected for class variables; recorded as a question for actor state.
- **C, one home plus per-send rollback:** rejected; it re-creates a second representation in the layer being deleted.
- **C′, rollback at `on:do:` handler entry only:** not chosen by default; the main open question.
- **D, ETS-backed:** out of scope; changes concurrency semantics.
- **E, snapshot reads from other processes:** fallback if Phase 0 shows cross-process reads are common.

## Consequences

### Positive

- The bug class goes away by construction: there is one value, so there is nothing to merge and no newest copy to choose. Every scope kind, including stored closures and blocks passed to higher-order methods, sees the same variable.
- 19 of the 21 known-wrong pinned tests flip to the answer the documentation calls correct. The other two involve a caught raise and take this ADR's answer instead (see Migration Path). The BT-3681 warning, the stored-closure limits and four compile-time errors are removed.
- Several thousand lines of codegen and verifier code and about 170 runtime lines are deleted. The `ThreadedIr` loses its most complex family.
- An open class-side self-send loses its per-send token, scope read, export, commits and reply unwrapping. It should return to about the pre-BT-3666 cost; Phase 0 measures it.
- Silent data loss for blocks run in other processes becomes a structured error with a hint.

### Negative

- **Two semantic changes are visible to users.** A caught error no longer undoes the writes made before it, and a class-variable access from a block running in another process now raises. Both change answers that tests pin today.
- **Class variables and actor state now disagree** about a raising send that is caught: an actor rolls back the callee's writes, a class does not. Until actor state is revisited, the language has two rules.
- **Inside and outside the class process disagree** about a caught error (§4): the external send is the transaction boundary, the method body is not. Today both revert.
- **Every class-variable write costs a process-dictionary round trip, and straight-line reads pay a remote call where today they pay nothing.** In an isolated microbenchmark (OTP on the CI container, 2M iterations, three runs) a read through `get/1` plus `maps:get/2` costs the same as `maps:get/2` on a threaded variable (10-13 ns either way, with or without the pid check), while a write through `get/1`, `maps:put/3` and `put/2` costs 52-76 ns against 10-12 ns for a threaded rebind. In isolation reads are free and a write costs about 50 ns, which is what one scope token costs today per *send*; through generated code a helper call adds a remote call per access, which is why Phase 0 gates the read/write loop separately and pre-authorises inlining.
- **The process dictionary is a hidden channel.** Reading generated code no longer shows class-variable data flow, and a hand-written Erlang class method must use `beamtalk_class_vars` to see the current values.
- **The calling convention changes, and the change is not hot-upgradable.** Every compiled class method is recompiled, a module compiled by the previous compiler is refused at load with `abi_mismatch`, and the release that ships this needs a node restart (see Implementation).
- **One class of mistake moves from compile time to run time.** Today a class-variable write in a block passed to another class's method is rejected by the compiler, as a side effect of rejecting every non-inlined block write. Under this ADR only the statically visible cases get the `class-state-abroad` lint; a block that reaches another process indirectly fails when it runs, with a structured error.

### Neutral

- The `gen_server` state shape, the ETS snapshot, `sys:get_state/1` output, and external `get_class_var`/`set_class_var` calls are unchanged.
- ADR 0129 Phase 0b direct calls are unaffected; they only apply to classes without class state.
- ADR 0126 rewrites a class object crossing nodes into a by-name reference, so a remote `ClassSelf` resolves to the receiving node's class, where the key is absent and the §5 error is raised. Class variables stay node-local.
- The LSP's diagnostic set changes: four codegen errors and the BT-3681 warning disappear, and the `class-state-abroad` lint appears on every surface, since it lives in `beamtalk-core` semantic analysis with the validators it replaces.
- The ThreadedIr guideline in `docs/agents/expanded.md` still applies to every remaining family.

## Implementation

Effort: L overall. Phase 0 gates the rest.

**Phase 0: prove it (S).**
- Add `beamtalk_class_vars.erl` and hand-lower one open class's `bump`/`foo` pair to the new convention.
- Measure with `runtime/perf/self_send_bench` (method in `docs/development/benchmarks.md`): open self-send at top level and in an arm, sealed self-send, and a class-variable read/write loop, against the pre-BT-3666 baseline. Two gates: open self-send within 15% of baseline; the read/write loop within 2x of today's threaded access. If the second gate fails, the pre-authorised response is to inline the helpers as `erlang:get/1` plus `maps:get/2`/`maps:put/3` in codegen (the key shape stays owned by `beamtalk_class_vars`, exported as a macro or generated constant), not to reopen the decision.
- Count class-variable accesses from a process other than the home class over the BUnit corpus and fixtures (no `stdlib/src` class declares `classState:`, so the stdlib itself cannot be the gate), the REPL-protocol cases and `test-package-compiler/cases`, by instrumenting the current runtime. Gate: every hit must be a supervisor-definition or `performLocally:` site covered by `with_snapshot/2`, or a test that pins today's silent loss; anything else is a user-visible breakage to document in Migration Path.

**Phase 1: tests that pin the semantics (M, in parallel with Phase 2).**
- A BUnit matrix, one fixture per (sealed, open, subclass override) × (top level, arm, letrec loop, fold loop, `on:do:` body, `on:do:` handler, `ensure:`, bare block, stored closure, block to a same-class higher-order method, block to another class's method, `performLocally:`), asserting the §4/§5 answers. Written during Phase 1 and red until Phase 3 lands; together with the generator corpus below it is the gate for Phase 3.
- Extend `arb_program` (`crates/beamtalk-core/src/test_helpers.rs`) to class state, loops, exception handling, stored closures and writing, plain and late-bound self-sends, with the agreement oracle: the sealed, open and subclass-override spellings of one program must answer the same, and must match a reference interpretation in which every write is immediate. Run it against `main` and record the failure rate. This is the gate for Phase 3, as the guideline in `docs/agents/expanded.md` § State-Threading Codegen requires of any change to these lowerings; removing a family by construction is not exempt, because the same deletion touches the `State` and `Self` families' lowerings in the same files, and the generator is what shows those still hold.

**Phase 2: runtime (M).**
- Install, read back and erase the map in `invoke_class_method/7`, `invoke_class_extension/7` and the metaclass path, with the absent-on-entry assertion first; ADR 0084 builder funs run inside those and need only the arity change in `put_class_method/4`.
- Implement `get/get_late/put/clear/has`, `with_snapshot/2`, `class_state_unreachable` and `class_state_read_only`; wrap the three supervisor-definition sites and `local_call/3` in `with_snapshot/2`.
- Record the calling convention as a `class_var_abi` entry in `__beamtalk_meta/0`, and make the loader (`beamtalk_object_class` registration, the hot-reload path, and ADR 0125 §2.3's compatibility preflight, which already reads `__beamtalk_meta/0`) refuse a module whose ABI differs from the running runtime's or from its loaded superclass chain's with a structured `abi_mismatch` error that names the module and says to recompile. There is no dual-ABI window: an old-ABI caller direct-calls `class_<sel>(ClassSelf, ClassVars, ...)` on whatever module defines the method, so an old module and a new module in one hierarchy cannot be bridged at the call site, and bridging only at the runtime dispatch boundary would reintroduce the two-representations reconciliation this ADR removes. The upgrade across this release is a whole-node restart, which is the only upgrade `beamtalk release` v1 supports anyway (ADR 0125 §2.1); when the relup phase lands, `class_var_abi` is part of the `__beamtalk_meta/0` record its appup derivation diffs (ADR 0125 §2.2), so a change to it marks the release as restart-only. A package compiled by an older compiler is refused at load until it is recompiled.
- Change `local_call/3`'s doc contract from "class-variable mutations are discarded" to the §5 snapshot rule, and update the `performLocally:` paragraph of `docs/beamtalk-language-features.md` to match.

**Phase 3: codegen (L).**
- Emit the §2 calls and the §3 convention, and add the `class-state-abroad` lint (update `docs/development/surface-parity.md` for the new diagnostic).
- Delete everything in §6 and the tests that pin the deleted machinery (`class_var_scope_tokens.rs`, `class_var_shadow_contract.rs`, `class_var_shadow_writes.rs`, the `ClassVars` parts of `class_var_bind.rs`). Regenerate the affected snapshots.
- Flip every `PIN-BUG BT-3682` and `PIN-BUG BT-3691` test and every stored-closure and section-literal limit to the correct answer. Close BT-3682, BT-3691, BT-3693, BT-3694 and BT-3696 against those tests.
- `just verify-threaded-ir`, `just test` and the Phase 1 matrix must pass.

**Phase 4: docs and cleanup (S).**
- Mark ADR 0110 Superseded; amend ADR 0013 §1, ADR 0084, ADR 0111 and ADR 0122; rewrite the class-variable parts of `docs/beamtalk-language-features.md`, `docs/development/debugging.md`, `docs/development/benchmarks.md`, `docs/development/erlang-guidelines.md` (FFI rule) and `docs/development/surface-parity.md` (the removed diagnostics and the new lint).

Affected components: runtime (`beamtalk_class_dispatch`, `beamtalk_object_class`, new `beamtalk_class_vars`), codegen (`core_erlang` class-method, dispatch, control-flow, block and exception lowering, `threaded_ir`), semantic analysis (stored-closure validator), docs. Parser, AST, type checker, LSP and REPL are unaffected.

## Migration Path

- **Programs that relied on a caught error undoing a write** will see the write kept. About 30 BUnit tests in `self_send_override_blocks_test.bt`, `self_send_plain_reply_test.bt` and `class_var_nlr_shadow_test.bt` assert how writes interact with a caught raise, and most of them change their expected answer: for example `testCaughtThenSend`, `testOwnTryBodyRaiseThenSend`, `testOpenArmsRaisedIterationDiscarded` and the sealed twins. Two pinned known-wrong tests move to this ADR's answer rather than the one their comments call correct: `testSealedPinBugRaisedIterationBetweenKept` (`raisedIterationBetweenKept`) answers 3, because the raised iterations' writes are kept, where its comment says 1; and `testStoredClosureReadsRevertedValueAfterCaughtRaise` keeps its current answer of 2, which becomes the correct one. The language documentation's statement that "a write made by a callee that raised is never kept, even when the raise was caught" is replaced. To keep the old behaviour in user code, assign after the protected block succeeds.
- **Programs that passed a block touching class variables to another class's class method or an actor** will get `class_state_unreachable` instead of a stale read or a lost write. Fix: read into a local before passing the block, or return the value and assign it in the home class's method.
- **Supervisor definitions and `performLocally:` keep reading** what they read today (the snapshot as of the last completed invocation). A class-variable write from `class children` or through `performLocally:` outside the class process, silently discarded today, now raises `class_state_read_only`.
- **Programs that worked around the old limits** (an unused local added to thread a loop body, `@expect stored_closure`) keep working; the workaround becomes unnecessary and `@expect stored_closure` becomes an unused-expectation warning to remove.
- **Hot upgrades** across the release that changes the calling convention are not supported: the node restarts, and every package is recompiled. A module from an older compiler is refused at load with an `abi_mismatch` error that names it.

## Open Questions

1. **Caught-error semantics.** This ADR proposes Smalltalk semantics (§4), with the inside/outside-process asymmetry named there. Alternative C′ restores at `on:do:` handler entry only, which removes that asymmetry for the common case at the cost of restoring for one catcher only. Which does the project want?
2. **Cross-process reads.** This ADR proposes an error. Alternative E keeps snapshot reads. Phase 0's count decides unless the project has a preference.
3. **Actor state.** Should actor state get the same treatment, so that both kinds of mutable state agree on caught errors and on stored closures? That needs its own ADR.
4. **Verifier visibility in release builds.** `report_threaded_ir_verify_errors` records an `internal:` error diagnostic in release builds, but BT-3693 reports that the release CLI printed none. Phase 1's property test must fail on that diagnostic, and the CLI path should be checked.

## References

- Related issues: BT-3666, BT-3667, BT-3669, BT-3675, BT-3676, BT-3681, BT-3682, BT-3683, BT-3688, BT-3690, BT-3691, BT-3692, BT-3693, BT-3694, BT-3696, BT-3580, BT-912
- Related ADRs: ADR 0013 (class variables), ADR 0041 (universal state-threading block protocol), ADR 0109 (blocks run where invoked), ADR 0110 (class-variable shadow, superseded by this ADR), ADR 0111 (ThreadedIr verifier), ADR 0125 (releases and upgrades), ADR 0127 (traits), ADR 0129 (class-side facades, Phase 0b direct calls)
- Code: `runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl` (`invoke_class_method/7`, scope helpers), `beamtalk_object_class.erl` (`#class_state{}`, `local_call/3`), `beamtalk_actor.erl` (`self_dispatch/2`), `crates/beamtalk-codegen/src/core_erlang/generator/version.rs`, `dispatch_codegen.rs`, `gen_server/methods.rs`, `threaded_ir/verify.rs`
- Tests: `stdlib/test/self_send_override_blocks_test.bt`, `stdlib/test/self_send_plain_reply_test.bt`
- Docs: `docs/development/benchmarks.md` § Class-side self-send cost, `docs/beamtalk-language-features.md` § Passing Blocks Through Class Methods, `docs/agents/expanded.md` § State-Threading Codegen
- External: Erlang `erlang:put/2` and `get/1` (process dictionary), `gen_server` callback semantics; Pharo class variables and class-instance variables
