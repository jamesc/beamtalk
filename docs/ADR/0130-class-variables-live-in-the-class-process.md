# ADR 0130: Class Variables Live in the Class Process During an Invocation

## Status
Accepted (2026-10-02)

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
- **Classes with class state are never direct-called** (ADR 0129 Phase 0b, `compute_direct_call_eligible` gate 2). A class method that touches class variables runs inside its class's process, with three exceptions, all runtime-owned: a block carried into another process (ADR 0109); `performLocally:withArguments:` (`beamtalk_object_class:local_call/3`), which today passes an empty map so a read fails with a raw `badkey`; and supervisor definition, where `beamtalk_supervisor:static_init/2`, `dynamic_init/2` and the `withClassMethod:` child factory call `class children`, `class strategy`, `class maxRestarts`, `class restartWindow` and the factory selector in the **supervisor process**, and `run_initialize/1` calls the user's `class initialize:` hook in whatever process called `supervise`, each against a copy read from the ETS snapshot, so that a value set by an earlier `configure:` call is visible to `class children`. The factory discards a write; `static_init/2` and `dynamic_init/2` do not unwrap a `class_var_result` reply at all, so a `class children` that writes a class variable breaks supervisor startup today. Those four sites need a defined path under this ADR (§5).
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

`invoke_class_method/7` and `invoke_class_extension/7`, the two entry points every class-method call reaches (the `class_method_call` and `metaclass_method_call` messages, and `class new:`, all funnel through `handle_class_method_call/6` to one of them), install the stored map on entry and read it back on exit:

```erlang
invoke_class_method(Selector, Args, ClassName, _Module, DefiningClass, DefiningModule, ClassVars) ->
    Key = beamtalk_class_vars:key(ClassName),
    beamtalk_class_vars:assert_absent(Key),      %% check first: a present key, or any live home entry, is a nested invocation
    beamtalk_class_vars:install(Key, ClassVars), %% put(Key, ClassVars), put('$bt_class_vars_home', Key)
    try apply_class_method_in_context(Selector, Args, ClassName, DefiningClass, DefiningModule) of
        test_spawn -> test_spawn;
        {ok, Result} -> {reply, {ok, Result}, get(Key)};
        {nlr_relay, Nlr, _ST} -> {reply, {error, Nlr}, get(Key)};   %% writes before a foreign ^ are kept
        {error, Error} -> {reply, {error, Error}, ClassVars}       %% escaping error: pre-call map
    after
        beamtalk_class_vars:uninstall(Key)       %% erase(Key), erase('$bt_class_vars_home')
    end.
```

`install/2` and `uninstall/1` are the only writers of the key and the home entry together, and `invoke_class_extension/7` uses the same pair. **Invariant: the key is absent on entry**, checked before anything is installed. Today nothing nests `invoke_class_method/7` for one class (it is reached only from the class's `handle_call`, own-class sends from inside raise `dispatch_error`, and `new`/`spawn` short-circuit through `handle_self_instantiation`). `assert_absent/1` raises a structured internal error when the class's key is already present *or* when any `'$bt_class_vars_home'` entry is present (a live invocation of any class in this process), before `put/2` runs, so a future refactor that makes nesting reachable, same class or not, fails loudly with the outer map and home entry intact instead of overwriting them; with the invariant checked first, `after` can simply erase. (`beamtalk_actor:restore_dispatch_pdict/1` saves and restores instead because actor self-dispatch does nest; class invocations must not.)

Between invocations nothing changes: the `gen_server` state holds the map and the ETS snapshot mirrors it.

### 2. Access is a call into one runtime module

A new runtime module, `beamtalk_class_vars`, owns the key and every access. Codegen emits calls to it and never builds the key itself, so no rule crosses the Rust and Erlang boundary.

| Beamtalk | Core Erlang emitted |
|---|---|
| `self.n` | `call 'beamtalk_class_vars':'get'(ClassSelf, 'n')` |
| `self.n` where `n` is `late classState:` (ADR 0124) | `call 'beamtalk_class_vars':'get_late'(ClassSelf, 'n')`, raising `uninitialized_state_error` as the guarded `maps:find` does today |
| `self.n := v` | `call 'beamtalk_class_vars':'put'(ClassSelf, 'n', V)` |
| `self clearField: #n` | `call 'beamtalk_class_vars':'clear'(ClassSelf, 'n')` |
| `self hasField: #n` | `call 'beamtalk_class_vars':'has'(ClassSelf, 'n')` |

Each helper derives the key from `ClassSelf`'s class tag and looks it up in the current process. **Key presence is the "at home" test, not pid equality.** A foreign process never has the home class's key (the home process is blocked in the `gen_server:call` that carried the block away), the class process outside an invocation has none either, and a self-send within the invocation has it. So `get/1` answering `undefined` raises the structured error in §5, and nothing else is compared. This keeps a closure that captured a `ClassSelf` before a class-process restart working at home, and makes the direct-called path's `ClassSelf = nil` trivially safe (it has no class variables to access). Phase 0 measures whether the helpers should be inlined as `erlang:get/1` plus `maps:get/2` instead.

### 3. Class methods neither take nor return class variables

The compiled calling convention becomes `class_<sel>(ClassSelf, Args...)`, returning the bare result. `{class_var_result, _, _}` is deleted from codegen and runtime. A class-side self-send of any kind (sealed or open, direct or walked, `super`, own-class `Base foo`) passes nothing and rebinds nothing. Sealing a class changes how a send is dispatched and nothing about class variables.

Codegen no longer has a `ClassVars` threading family. Loops, folds, arms, handler arms, blocks and stored closures need nothing for class variables, because a write is a `put` wherever it happens.

Class-side extension methods (ADR 0066) follow the same convention. Today a class-side extension on an Actor subclass is compiled in actor context as `fun(Args, Self, State) -> {Result, NewState}`, `invoke_class_extension/7` passes the class-variable map as that `State` and stores the returned map, and `unwrap_self_dispatch_extension_outcome/3` turns the tuple into a `class_var_result` for in-process self-sends: a second copy of the map, which is exactly the two-representations shape this ADR removes. Under this ADR every class-side extension fun, whatever the target class's kind, is compiled in class-method context as `fun(Args, ClassSelf) -> Result` and reads and writes through `beamtalk_class_vars`; the 3-arity class-side path and the `class_var_result` arm of `unwrap_self_dispatch_extension_outcome/3` are deleted (§6). ClassBuilder class-method funs (ADR 0084) likewise become `fun(ClassSelf, Args...)`.

### 4. Error semantics: a boundary discards what happened inside it

A class-variable write takes effect when it is made, and an error that crosses a boundary discards every write made inside that boundary. There are two kinds of boundary, and the rule is the same at both. This is Erlang's `try` semantics applied to class variables: a `try` yields only its body's value, never the body's bindings, so state threaded through a body that raises is gone, and an error that escapes the callback leaves the `gen_server`'s state as it was.

- **The invocation boundary.** An error that escapes the invocation discards everything the invocation wrote: `invoke_class_method/7` replies with the pre-call map. Unchanged.
- **The catch boundary.** A protected region (`on:do:`'s protected block, and every runtime catcher that runs Beamtalk blocks, such as `Result tryDo:` and the exception handler) snapshots the map on entry and restores it when an error crosses its catch, before the handler runs. So `[self bumpThenFail] on: Error do: [:e | nil]` discards the write `bumpThenFail` made before it raised, and a write made *before* entering the protected block is kept. This is the behaviour BT-3675 established and the language documentation describes, now as one rule at one kind of place instead of a property of each send.
- **A non-local return** is not an error. A `^` passing through a protected region keeps the writes made before it, foreign or own, and needs no special path.
- **`ensure:`** is not a boundary. It does not catch, so an error passes through to the next boundary and the cleanup block's own writes go with it.

The mechanism is a snapshot and a restore around the protected region, keyed by the invocation, not by whoever wrote the catch. The two entry points record the key they install under `'$bt_class_vars_home'` and erase both together; nothing else ever writes that entry. `beamtalk_class_vars:snapshot()` reads it: with no home entry in this process (a foreign process, a direct-called stateless method outside any invocation, a supervisor or `performLocally:` snapshot region with no live invocation) it answers `none`; otherwise it answers `{Key, get(Key)}`, a pointer to the immutable map. `restore(none)` does nothing and plants nothing; `restore({Key, Map})` is one `put`. **Every compiled `on:do:` is a catch boundary**: codegen emits `let Snap = snapshot() in` before the existing `try`, and `restore(Snap)` as the first statement of the catch arm's non-NLR branch, that is, after **both** existing `$bt_nlr` pass-through arms that `on_do_catch_preamble` (`exception_handling.rs`) emits, the 3-tuple `{'$bt_nlr', Tok, Val}` and the actor-shaped 4-tuple `{'$bt_nlr', Tok, Val, State}`, and before the handler's class filter runs, in every method context, class-side or instance-side, direct-called or not. A `^` therefore crosses the arm without touching the map, and every other exception, matched by the filter or not, restores before anything else happens. The `try` body itself is untouched, so the actor `State` and value-type `Self` lowering of `on:do:` (`exception_handling.rs`) does not change and no closure is allocated. For Erlang catchers that take a block, `protect(Fun)` is the same pair around a fun: `Result tryDo:`'s native implementation (`beamtalk_result:'tryDo:'/1`) and any other `catch` in the runtime or stdlib that invokes a block argument use it, and a hand-written Erlang catcher that runs Beamtalk blocks must use it (the FFI rule in `docs/development/erlang-guidelines.md`); `beamtalk_exception_handler` only classifies exceptions and runs no block, so it is not a catcher in this sense. Because the home entry is the invocation's, every catcher inside one invocation protects the same map whatever class wrote the catch: a block from `X` with its own `on:do:` invoked inside `Y`'s invocation protects `Y`'s map, the only one present (`X`'s accesses raise there anyway), and `Result tryDo:` in the same position does the same, so "one rule at one kind of place" holds across classes. The restore happens on *any* error crossing the protected block, including one the `on:do:` class filter then declines and re-raises; an `ensure:` cleanup between an inner non-matching `on:do:` and the outer catch therefore sees the restored map, and the final state is the same either way. A `$bt_nlr` throw is not an error and restores nothing: the compiled arm orders both tuple shapes before the restore, and `protect/1` tests for both shapes before restoring, so the two agree. `with_snapshot/2` installs a class's key and a read-only marker (`{'$bt_class_vars_ro', Tag}`, checked by `put` and `clear`, never by reads) only when that key is absent, erases exactly what it installed in an `after` (a raising `Fun` is the designed path, since a write through the snapshot raises, and the hosting process is long-lived) and nothing when the key was already present (a live map, or an outer snapshot of the same class), and does **not** touch the home entry, so a `performLocally:` on `Y` from inside `X`'s live invocation leaves `X`'s home in place and `X`'s restore still works afterwards; a snapshot never captures a read-only map, because the home entry is only ever an invocation's live key. Outside an invocation `snapshot/0` is one `get` answering `none`; Phase 0 measures that cost on an instance-side `on:do:` loop rather than exempting it. The obligation is checked, not trusted: the `ThreadedIr` verifier gains a `CatchWithoutClassVarRestore` error for an `on:do:` node in any method context whose catch arm's non-NLR branch does not begin with the restore, or whose two NLR pass-through arms are not both ordered before it (guideline point 3 in `docs/agents/expanded.md`). Because the snapshot is taken from the live map and restored into it, there is still one home; the snapshot is scoped to the region and never read by anything else.

What this means for the two places that catch today. Outside the class process, each class-method call is its own transaction: in `[X bump. X failingBump] on: Error do: [:e | nil]` evaluated from the REPL or an actor, `bump`'s write is committed when its call returns and only `failingBump`'s is discarded. Inside one of `X`'s class methods, the protected region is the transaction: the same expression with `self` discards `bump`'s write too, because it was made inside the region. That is an asymmetry, in the direction Erlang's `try` has (a region is one unit), and it is named under Consequences. The failing send itself reverts in both places, as today, and actor state (whose `safe_dispatch/3` returns the pre-call state on error, and whose `on:do:` continues with the state from before the protected block) behaves the same way. The alternative of letting writes survive a caught error, as Pharo does, is discussed under Alternatives.

### 5. A block writes its home class's variables only at home, and reads what it captured elsewhere

A block literal written in a class method reads and writes the live class variables whenever it runs in its home class's process. That covers loops, conditionals, `do:`/`collect:` and every other instance-side collection method, `ensure:`/`on:do:`, `Result tryDo:`, stored closures, a block passed to a user-defined class-side higher-order method of the same class, and a block passed to a direct-called (`class sealed`, stateless) class method of another class.

A block that runs outside its home invocation cannot reach the live class variables. That is a block carried to another process (the home process is blocked in the `gen_server:call` that carried it away), and it is also an *escaping* closure: a block returned or stored by a class method (a sort block, a formatter, a callback, a `Future` or `Timer` body) that runs after the invocation that created it has ended and outside any later invocation of its class. A stored closure invoked from a *later* invocation of the same class is at home again: the key is present, so it reads live and may write, deliberately, since the class process is the one place its variables can be consistent. The rule for such a block is Erlang's rule for a fun: **it reads the values the class variables had when it was created, and it cannot write them.** At home (the key is present) a read is live; anywhere else it is the captured value; a write anywhere but home raises. When a block is carried synchronously to another class's method or an actor, the home invocation is blocked for the duration, so the captured value *is* the live value and nothing changes for that code. When it is carried asynchronously (a cast, a `Future`, a `Timer`) or stored and run later outside any invocation of its class, the captured value is the one from creation time, which is what an Erlang closure gives and what today's compiler gives too. The fun analogy is deliberately partial: the same stored block run from a later invocation of its own class is at home, reads live and may write, so where a block runs decides which it gets. Today the compiler rejects a direct class-variable write in any block that is not inlined (`FieldAssignmentInUnsupportedBlock`); under this ADR that rejection goes away, because a block that runs at home can now write, and a write outside a live invocation of the home class raises at run time:

```erlang
#beamtalk_error{kind = class_state_unreachable, class = 'Counter', selector = bump,
                message = <<"Counter's class variable n cannot be written from this process">>,
                details = #{class_variable => n},
                hint = <<"A block that writes Counter's class variables ran outside any Counter class "
                         "method (it was passed to another class's class method or an actor, or "
                         "stored or returned and run later outside Counter's own methods). A block "
                         "can read Counter's class "
                         "variables anywhere, as the values they had when the block was made, but "
                         "can only write them from Counter's own method: return the value and "
                         "assign it there.">>}
```

**Runtime-owned out-of-process invocations get a read-only snapshot.** Four runtime paths deliberately run a class method outside the class process: supervisor definition (`beamtalk_supervisor:static_init/2`, `dynamic_init/2`, the `withClassMethod:` factory) in the supervisor process; the user's `class initialize:` hook (`beamtalk_supervisor:run_initialize/1`) in whatever process called `supervise`, which may be another class mid-invocation; and `performLocally:withArguments:` (`beamtalk_object_class:local_call/3`), which today calls the method with `ClassSelf = nil` and must instead pass the receiver class object it already holds. All wrap the call in `beamtalk_class_vars:with_snapshot(ClassSelf, Fun)`: if the key is already present (the caller is the class process mid-invocation) `Fun` runs against the live map; otherwise the ETS snapshot (`beamtalk_class_state_snapshot`, the map as of the last completed invocation) is installed under the key, marked read-only, for the duration of `Fun`, and removed after. The snapshot is resolved by class *name* through the registry, never through the pid in `ClassSelf`, which may predate a restart (the ETS table is keyed by pid today; `with_snapshot/2` goes through `whereis_class` first); a name with no live class raises `class_state_unreachable`. Reads see what `class children` needs to see today. A write through a read-only snapshot raises `class_state_read_only` with a hint naming the entry point, instead of breaking supervisor startup (`static_init`/`dynamic_init`), being silently discarded (the factory, `initialize:`) or failing with `badkey` (`performLocally:`) as today. This is Alternative E′'s mechanism (the ETS mirror) kept for runtime-owned call sites only, which run with no invocation live and need the committed state; it is the one deliberate exception to "key presence means at home": inside one of these regions a read is a mirror read by design, including for a block from `X` carried into a process that then calls `X performLocally:`. User blocks never read the mirror; abroad they read their creation-time capture.

`local_call/3`'s doc contract changes from "class-variable mutations are discarded" to: reads see the snapshot, writes raise, and a `performLocally:` reached from inside the class's own method mid-invocation reads and writes the live map. The access helpers (`get`, `get_late`, `put`, `clear`, `has`) never accept `nil`: a `nil` receiver there is an internal error, not a reachable-state question; `get` and `has` on a name that is not a declared class variable of the class raise a structured error rather than a raw `badkey`. `snapshot/0`, `restore/1` and `protect/1` take no receiver and are pass-throughs when no invocation is live; `with_snapshot/2` is only reached from runtime sites that always have a real `ClassSelf`.

The mechanism is a capture at block creation. Codegen binds `let CVSnap = beamtalk_class_vars:capture(ClassSelf, Outer) in` where any block literal that reads a class variable is created (`Outer` is the enclosing block's capture, or `none` at method level), which answers the live map when the key is present and `Outer` otherwise, so a block created abroad inherits its parent's capture; reads inside the block lower to `get(ClassSelf, 'n', CVSnap)` (live when the key is present, otherwise `maps:get` on the capture), `has` and `get_late` likewise; writes lower to the ordinary `put`, which raises when the key is absent. The capture is a pointer to an immutable map, costs one `get` per block creation, is never written, and is consulted only when the key is absent, so there is no second home and nothing to merge: it is a read-only fallback with no recency question, the same way a captured variable in an Erlang fun has none.

Where the compiler can see the case, it says what the capture means instead of leaving it to run time: a block literal that reads class variables and is passed to an asynchronous send, stored in a variable or class variable, or returned, and a block literal that writes class variables and is passed to a class-side send whose receiver is statically another class that has class state or whose method is not `class sealed`, each get a `class-state-abroad` warning at the block (reads: "outside an invocation of this class, reads the values captured at creation"; writes: "outside an invocation of this class, raises"), suppressible with `@expect class_state_abroad`. This is a lint, not a guarantee; a block passed through a variable or an instance method is only caught at run time, and only for writes.

### 6. What is deleted

- Runtime: the ADR 0110 shadow key and its `nlr_relay` read, the commit map and `class_var_scope_commit/read/take/export`, `erase_class_var_scratch/1`, the `class_var_result` arms in dispatch, in `unwrap_self_dispatch_outcome/3` and in `unwrap_self_dispatch_extension_outcome/3`, the 3-arity class-side extension path in `apply_class_extension_fun` / `apply_extension_by_arity`, and the macros in `beamtalk.hrl`.
- Codegen: `VersionPrefix::ClassVars` and its counter, scope tokens, marks and regions (`generator/version.rs`), `class_var_for_send`, `emit_class_var_result_unwrap` and the pure-send variant, `confined_class_var_refresh_stmt`, `commit_live_class_var_doc`, the `{Acc, ClassVars}` fold accumulator, the `Letrec` `ClassVars` parameter, the shadow-write flag on `Bind`, and the `has_class_vars` field of `NlrBoundary::ClassMethod`.
- Verifier: `ShadowWriteMissing` and the `shadow_write_eligible` stack.
- Diagnostics: `ClassVarAssignmentInThreadedBody`, `ClassMethodSelfSendInThreadedLoopBody`, `ClassMethodSelfSendInUnthreadedBlock`, `ClassVarMutationLostAcrossNestedLoop`, and the class-variable case of `FieldAssignmentInUnsupportedBlock`.
- Analysis: the BT-3681 stored-closure advisory, and `compute_class_var_mutating_selectors` wherever it only gates class-variable threading.
- Runtime, other: the supervisor's `class_var_result` unwrapping and snapshot plumbing (`beamtalk_supervisor.erl`, the four sites, replaced by `with_snapshot/2`), `put_class_method/4`'s fixed `n+2` fun arity for ClassBuilder funs (ADR 0084; becomes `n+1`), and about 30 EUnit pattern matches on `class_var_result` across `beamtalk_class_dispatch_tests.erl`, `beamtalk_supervisor_tests.erl`, `beamtalk_class_dispatch_test_helper.erl` and `beamtalk_object_class_tests.erl`.
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

Today this compiles without a warning and answers 0: the block runs in `Batch`'s process, `self add: x` runs `Tally`'s method there against a copy of the map, and the writes are silently lost (run on `main` at `14799bd`; a direct `self.total := ...` in the block is instead rejected at compile time). Under this ADR the `class-state-abroad` lint warns at the block, and the write raises when the block runs in `Batch`'s process. A block that only *read* `self.total` there would answer the captured value, which is the live one since `Tally` is blocked in the call, as today.

```beamtalk
Object subclass: Ids
  classState: next = 0
  class take =>
    self.next := self.next + 1
    self error: "boom"
  class tryTake =>
    [self take] on: Error do: [:e | nil]
    self.next

Ids tryTake        // => 0
```

The write made inside the protected block is discarded when the error crosses the catch, so `tryTake` answers 0 today (run on `main` at `14799bd`) and under this ADR. Had `tryTake` written a class variable before entering the protected block, that write would be kept: boundaries discard what happened inside them and nothing else.

## Prior Art

**Pharo / Squeak / GNU Smalltalk.** A class variable or class-instance variable is a slot of a shared object. Assignment takes effect immediately and nothing is rolled back by an exception, caught or not. This ADR adopts the single mutable home and the immediacy of writes, and rejects the "nothing is rolled back" half (see Alternatives): Beamtalk's class is a `gen_server`, and a `gen_server` has boundaries that Smalltalk's image does not.

**Erlang/OTP.** A `gen_server` has no rollback mechanism; state is a value threaded through the callback, and "rollback" is continuing with the value you had. That gives two boundaries, and both discard what happened inside them: `try` yields only its body's value, so the idiom `try Body catch _ -> {reply, {error, R}, State}` discards the body's updates for free, and an escaping exception discards the whole callback's (by killing the process). §4 is those two boundaries applied to class variables, with the softer "reply with the pre-call map" standing in for the crash. Inside a callback, the process dictionary is the idiomatic process-local store for data a call chain needs without threading it (`$ancestors`, `logger` metadata, `rand` seeds); this ADR uses it the same way, scoped to one callback and erased in `after`. Mnesia is the nearest precedent for "process-local transaction context, discarded at a boundary": it keeps the active transaction's context in the process dictionary (`mnesia_activity_state`) and throws the transaction's pending writes away when it aborts.

**Beamtalk actors.** Actors already hold `'$bt_actor_state'` in the process dictionary during a call for re-entrant self-dispatch, and an actor self-send that raises returns the pre-call state (`safe_dispatch/3`'s error arm), so an actor's `on:do:` continues with the state from before the protected block. §4 gives class variables the same behaviour, so the two kinds of mutable state agree on what a caught error does.

**Ruby and Kotlin.** Ruby class instance variables and Kotlin companion-object fields are plain mutable fields. Writes are immediate and survive caught exceptions, the Smalltalk half this ADR does not adopt.

**Clojure refs and Haskell STM.** Transactional memory gives each transaction a consistent snapshot and discards a failed transaction's writes. That is the model the current token design approximates per scope. It is rejected as a model for class variables because class methods run serially in one process, so there is no concurrency for transactions to resolve, only error rollback, and the escaping-error case already has a boundary.

## User Impact

**Newcomer.** Class variables behave like variables: a write is visible to the next line, inside a loop, inside a block and after a helper method returns. The stored-closure warning and the "give the loop body a local to thread alongside the class variable" advice disappear. A block can read class variables anywhere, and the one new error appears only when a block carried to another process, or run after its method returned, tries to write one; its hint says what to do.

**Smalltalk developer.** Reads and writes are the Pharo model: the template-method pattern works with class-side state in every position, and refactoring a write into a helper method no longer changes whether it survives. The one difference from Pharo is that an error crossing a catch or escaping the call discards the writes made inside it, which is the behaviour the language has documented since BT-3675 and the one they will expect from a language whose classes are processes.

**Erlang/BEAM developer.** The generated code is simpler: a class method is a plain function of `ClassSelf` and its arguments, and `self foo` is a plain call. The process dictionary is used inside one `gen_server` callback, checked absent on entry and erased in `after`; the actor's `'$bt_actor_state'` saves and restores instead because actor self-dispatch nests. An Erlang-implemented class method can read and write class variables through `beamtalk_class_vars` instead of producing `class_var_result` tuples.

**Production operator.** The class variables are inspectable mid-call with `erlang:process_info(Pid, dictionary)` and between calls with `sys:get_state/1`, as today. Open class-side self-sends lose the class-variable half of their BT-3666 regression here, with the late-binding guard tracked separately; Phase 0 measures both. A module compiled with the older calling convention is refused at load with `abi_mismatch`, and upgrading across this release needs a node restart (see Implementation).

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

**Why not.** It keeps the property that causes the bug class: every scope kind must return the newest map, so correctness still depends on every scope kind being threaded, and the actor pipeline has needed its own fixes for the same shapes (BT-3580, BT-912). It is the most expensive option, because every class method body moves onto the actor `StateAcc` protocol. It is the right question to ask about actor state too, which is why this ADR records it as a follow-up rather than a reason to keep class variables threaded.

### Alternative C: One home, plus per-send rollback

Adopt §1 to §3 and §5, but make the *send* the boundary: each class-side send snapshots the map before the call and restores it if the callee raises.

| Cohort | Strongest argument |
|---|---|
| Newcomer | "A failed message leaves no trace, wherever I catch it." |
| Smalltalk purist | "Message sends are the unit of meaning; a send that fails should be as if it never happened." |
| BEAM veteran | "A `try` around a call is a few nanoseconds. The snapshot is one `get`." |
| Operator | "It is the most conservative: no answer changes." |
| Language designer | "It is local to the send, not to the construct that catches." |

**Why not.** Three costs. (a) Per-send rollback has to be emitted by codegen at every class-side self-send, direct call and hierarchy walk alike, which is exactly the layer this ADR deletes; the runtime cannot do it, since self-sends never pass through `invoke_class_method/7`. (b) The snapshot is a second representation of the map at every send, with the non-local-return pass-through recency question reintroduced at each. (c) Erlang has no send boundary; its boundaries are `try` and the callback, and a rule that restores at sends but not at catches makes a write in a block followed by a caught raise behave differently from the same write in a helper method.

### Alternative S: Smalltalk semantics, writes survive a caught error

Adopt §1 to §3 and §5, with no catch boundary: a write takes effect when made and only an error that escapes the invocation discards it.

| Cohort | Strongest argument |
|---|---|
| Newcomer | "A variable is a variable. If I wrote it, it is written." |
| Smalltalk purist | "This is exactly Pharo. Rollback is a database idea, not a Smalltalk one." |
| BEAM veteran | "No snapshot, no restore, no helper. The cheapest possible implementation." |
| Operator | "One fewer mechanism to reason about in an incident." |
| Language designer | "Fewest rules: one home, one invocation boundary, nothing else." |

**Why not.** It is the one option that does not fit the platform. Erlang's `try` discards the protected body's state by construction, and actor state already behaves that way through `safe_dispatch/3`, so S would make class variables the only state in the language that survives a caught error. Its inside/outside difference is also the larger one: under S the failing send itself reverts outside the class process (the `gen_server` reply carries the pre-call map) and keeps its write inside, so the same single send answers differently by process; under §4 the failing send reverts in both places and only the treatment of *earlier completed* sends in the region differs. And S changes the answer of about 30 existing tests that pin discard-on-caught-raise. The steelman's cost argument is real but small: the catch boundary costs one `get` on entry and one `put` on the error path.

### Alternative D: ETS-backed class variables (ADR 0013's deferred option)

Store class variables in a per-class ETS table read and written directly from any process.

| Cohort | Strongest argument |
|---|---|
| Newcomer | "A block passed anywhere can read and write them. No new error." |
| BEAM veteran | "Concurrent reads at ~100 ns and no `gen_server` bottleneck; ADR 0013 already listed this as the scaling path." |
| Operator | "Visible in `observer` without attaching to a process." |
| Language designer | "Location-independent state is simpler than process-scoped state." |

**Why not.** It gives up the escaping-error rollback (an ETS write is visible immediately to everyone), so a partially failed class method leaves torn state visible to other processes. It turns class methods from serialized to concurrent, which is a language change of its own. It is a scaling decision for ADR 0013 to revisit, not a fix for this bug class.

### Alternative R: Raise on any access outside a live invocation

Adopt this ADR, but make a block that runs outside its home invocation raise on a class-variable *read* as well as a write, so the only way to use a class variable abroad is to copy it into a local first.

| Cohort | Strongest argument |
|---|---|
| Newcomer | "One rule: class variables only exist inside the class's own methods. No hidden capture to learn." |
| Smalltalk purist | "A read that silently answers an old value is the worst of both worlds; an error is honest." |
| BEAM veteran | "It finds the async-staleness bug at the first run instead of in production." |
| Operator | "No capture at block creation, so nothing to measure." |
| Language designer | "Fewest mechanisms: the key is present or it is not." |

**Why not.** It breaks correct code. A block carried synchronously to another class's method or an actor reads a value that cannot have changed, because the home is blocked for the duration, and that is the common case (`Batch each: items do: [:x | x * self.factor]`); raising there protects nothing. For the asynchronous and stored cases the captured value is the one an Erlang fun would carry, a defined and explainable semantic, not a bug, and the lint names those shapes. The capture is not a second representation in the sense that failed before: it is never written and never merged, only read when the key is absent. R's real merit, surfacing async staleness early, is kept as the lint.

### Alternative E′: Snapshot reads from the ETS mirror

Let a block abroad read the class's ETS snapshot (the map as of the last completed invocation) instead of its creation-time capture.

| Cohort | Strongest argument |
|---|---|
| BEAM veteran | "ETS is where observers already look; one source for every out-of-process read." |
| Operator | "No per-block capture; a stored callback sees the newest committed values." |

**Why not.** The ETS mirror lags the live map by exactly the home invocation's own uncommitted writes, so a block carried synchronously would read *older* values than the capture does, and a stored block would read values from a different time than the one its author could reason about. The runtime-owned snapshot sites (§5) use the mirror because they run with no invocation live and need the committed state; user blocks get the creation-time capture, which is the Erlang rule.

### Tension points

- BEAM veterans prefer B (no process dictionary, everything in the verifier); Smalltalk developers and newcomers prefer this ADR (Pharo semantics, no limits). The deciding fact is that B keeps the per-scope "newest value" obligation that has failed repeatedly.
- Smalltalk purists and language designers who weigh rule count prefer S (Pharo semantics, no boundary inside the call); BEAM veterans and operators prefer the chosen catch boundary, which is Erlang's `try` and what actors already do. The deciding facts are platform fit and consistency with actor state.
- Language designers who weigh mechanism count prefer R (no capture); BEAM veterans prefer the chosen capture, which is what a fun does. The deciding fact is that R raises in the synchronous case, where the captured value is provably current.

## Alternatives Considered

The seven options are described with their steelmen above. In short:

- **A, status quo:** rejected; it does not converge, and the verifier cannot detect the failure mode.
- **B, thread class variables on the ADR 0041 protocol:** rejected for class variables; recorded as a question for actor state.
- **C, one home plus per-send rollback:** rejected; it re-creates a second representation in the layer being deleted, at a boundary Erlang does not have.
- **S, Smalltalk semantics (writes survive a caught error):** rejected; it does not fit Erlang's `try`, disagrees with actor state, and makes a single failing send answer differently by process.
- **D, ETS-backed:** out of scope; changes concurrency semantics.
- **R, raise on reads outside a live invocation:** rejected; it breaks the synchronous case for no gain, and its early-warning merit survives as the lint.
- **E′, reads from the ETS mirror:** rejected for user blocks; it reads older values than the creation-time capture in the synchronous case.

## Consequences

### Positive

- The bug class goes away by construction: there is one value, so there is nothing to merge and no newest copy to choose. Every scope kind, including stored closures and blocks passed to higher-order methods, sees the same variable.
- Every one of the 21 known-wrong pinned tests flips to the answer the documentation calls correct, including the two that involve a caught raise. The BT-3681 warning, the stored-closure limits and four compile-time errors are removed.
- Class variables and actor state agree on what a caught error does: a protected region's writes are discarded when an error crosses its catch, at both levels of state.
- Several thousand lines of codegen and verifier code and about 170 runtime lines are deleted. The `ThreadedIr` loses its most complex family.
- An open class-side self-send loses its per-send token, scope read, export, commits and reply unwrapping, which is about half of what it pays over the pre-BT-3666 baseline today and all of what made arms and loop bodies expensive. The other half, the late-binding guard, is a separate follow-up (Phase 0). Phase 0 measures both.
- Silent data loss for blocks run in other processes becomes a structured error with a hint, and a block's reads work everywhere, live at home and captured elsewhere, which is what today's code relies on.

### Negative

- **Two changes are visible to users.** A class-variable *write* from a block running outside its home invocation now raises (reads keep today's captured-value behaviour, now with a lint naming the shapes where the capture can be older than the live value). And caught-error behaviour, unchanged in substance, moves from every send to the catch boundary, so a write made inside a protected block through a path the token design could not cover (a stored closure, a block given to a higher-order method) is now discarded like every other write in the region, where today it is resurrected or lost.
- **Inside and outside the class process treat earlier completed sends in a protected region differently** (§4): from outside, each call is its own transaction and a completed call's writes stay when a later call in the same `on:do:` fails; inside, the region is the transaction and they are discarded together. The failing send itself reverts in both. Actor state has the same shape today (each external message is its own transaction; inside a method, an `on:do:` continues with the state from before the protected block), so this is the language's existing rule extended to class variables, not a new one.
- **Every protected region costs a snapshot and, on the error path, a restore**, and every runtime or FFI catcher that runs Beamtalk blocks must use `protect/1` or it keeps writes it should discard. Compiled `on:do:` is checked by the verifier; the in-tree Erlang catchers are enumerated in Phase 2; a hand-written one is an FFI rule, not something the compiler can check.
- **Every class-variable write costs a process-dictionary round trip, and straight-line reads pay a remote call where today they pay nothing.** In an isolated microbenchmark (OTP on the CI container, 2M iterations, three runs) a read through `get/1` plus `maps:get/2` costs the same as `maps:get/2` on a threaded variable (10-13 ns either way), while a write through `get/1`, `maps:put/3` and `put/2` costs 52-76 ns against 10-12 ns for a threaded rebind, a 5-7x ratio per write. In isolation reads are free and a write costs about 50 ns, about what one scope commit costs today per confined *send* (BT-3690's profile: commit ~46 ns, token 12-26 ns); through generated code a helper call adds a remote call per access, which is why Phase 0 gates the read/write loop separately and names its fallbacks.
- **The process dictionary is a hidden channel.** Reading generated code no longer shows class-variable data flow, and a hand-written Erlang class method must use `beamtalk_class_vars` to see the current values.
- **A read inside a block does not say whether it is live or captured.** At home it is live; anywhere else it is the value from the block's creation; and a stored block can be either, depending on where it is later run, so the Erlang-fun analogy holds abroad and not at home. The lint names the shapes (async send, stored, returned) where the difference can matter.
- **The calling convention changes, and the change is not hot-upgradable.** Every compiled class method is recompiled, a module compiled by the previous compiler is refused at load with `abi_mismatch`, and the release that ships this needs a node restart (see Implementation).
- **One class of mistake moves from compile time to run time.** Today a class-variable write in a block passed to another class's method is rejected by the compiler, as a side effect of rejecting every non-inlined block write. Under this ADR only the statically visible cases get the `class-state-abroad` lint; a block that reaches another process indirectly fails when it writes, with a structured error.

### Neutral

- The `gen_server` state shape, the ETS snapshot, `sys:get_state/1` output, and external `get_class_var`/`set_class_var` calls are unchanged.
- ADR 0129 Phase 0b direct calls are unaffected; they only apply to classes without class state.
- ADR 0126 rewrites a class object crossing nodes into a by-name reference, so a remote `ClassSelf` resolves to the receiving node's class, where the key is absent: a read answers the block's capture and a write raises the §5 error. Class variables stay node-local.
- The LSP's diagnostic set changes: four codegen errors and the BT-3681 warning disappear, and the `class-state-abroad` lint appears on every surface, since it lives in `beamtalk-core` semantic analysis with the validators it replaces.
- The ThreadedIr guideline in `docs/agents/expanded.md` still applies to every remaining family.

## Implementation

Effort: L to XL overall (Phase 3 alone deletes several thousand lines across 11 codegen files and regenerates snapshots). Phase 0 gates the rest.

**Phase 0: prove it (S).**
- Add `beamtalk_class_vars.erl` and hand-lower one open class's `bump`/`foo` pair to the new convention.
- Measure with `runtime/perf/self_send_bench` (method in `docs/development/benchmarks.md`): open self-send at top level and in an arm, sealed self-send, and a class-variable read/write loop, against the pre-BT-3666 baseline. Method: at least 7 interleaved rounds per side, medians with min-max, on an idle machine, since the documented run-to-run spread is up to 40% and a 15% margin is near the noise floor. Three gates. (1) Open self-send, top level and in an arm alike: `median_after <= 1.15 * baseline + guard`, where `guard` is the measured cost of the `class_self_direct_ok` guard (about 55 ns in BT-3690's profile; measure it again), so roughly `1.15 * 117 + 55 ≈ 190 ns`; BT-3700 owns the rest down to BT-3700's own "within 15% of baseline" target. (2) Sealed self-send: no regression beyond noise. (3) A class-variable loop of ten reads and three writes per iteration, straight-line, no sends: expected ratio against today's threaded access is between 1x (reads) and 5-7x (writes), so the gate is `median_after <= 2 * median_today` for the whole body, with the per-write number recorded; if it fails, the fallback is inlining the access helpers as `erlang:get/1` plus `maps:get/2`/`maps:put/3`, measured again; if that fails too, the number is recorded and accepted with the ADR's reasoning, not used to reopen the decision. Coalescing consecutive writes into one `put` is not a fallback: any expression in the run that can raise (an inlined `1 / 0`, say) would let an `ensure:` cleanup see a write that has not happened yet. The guard is late binding's cost, not class-variable threading's, and this ADR does not touch it: of the roughly 100 ns per send that BT-3690 left over the baseline, the token, scope read, export, commit and reply unwrapping are removed here and the guard's three `persistent_term` reads remain. Collapsing the guard to one read (a per-class or per-selector "overridden or shadowed anywhere" flag maintained where extensions and runtime funs are installed, which would also let the subclass-receiver hierarchy walk short-circuit) is a separate runtime change, BT-3700, and the actor open self-send (BT-3692) is unrelated to class variables. The key shapes used by any inlined form (the class key and the home entry) have one source of truth on the Rust side, a `class_var_keys` leaf in `beamtalk-codegen` that both the emitter and the `build-stdlib` step read; that step regenerates `runtime/apps/beamtalk_runtime/include/beamtalk_class_vars_keys.hrl`, which `beamtalk_class_vars` includes. The header is **checked in**, because the runtime compiles before `build-stdlib` runs (`build-stdlib: build-rust build-erlang` in the `Justfile`), and its freshness is guarded by extending the existing `just check-generated-builtins` recipe, which already does exactly this for `beamtalk_generated_builtins.hrl`, so a stale header fails CI through the mechanism the repo has rather than a second one (CLAUDE.md's shared-fixture rule). This is needed only if the inlining fallback is adopted; it is listed under Phase 3 as conditional. The Phase 0 spike that measures the inlined form hand-lowers one class and hardcodes the key shape in that throwaway code, since it exists to produce a number, not to ship; the leaf, header and check land with Phase 3 only if either Phase 0 measurement, the class-variable loop or the `snapshot/0` overhead, makes inlining the adopted lowering; both escape hatches use the same leaf.
- Measure `snapshot/0` plus the restore arm on an instance-side `on:do:` loop outside any class invocation, where `snapshot/0` is one `get` answering `none`, against today's uninstrumented `on:do:`. Gate: within 10%. If it fails, the escape hatch is to inline the `get` through the generated home-key constant described above (Rust-owned, header generated by the build step), so the pass-through costs one BIF call and no remote call; the semantics are unchanged. Omitting the snapshot by any static criterion (no sends in the block, no `classState:` in the module) is not an option: any send can reach a class method that writes, and a block carried in from another module can write its home's variables, so a static exemption is unsound.
- Count class-variable accesses outside a live home invocation over the BUnit corpus and fixtures (no `stdlib/src` class declares `classState:`, so the stdlib itself cannot be the gate), the REPL-protocol cases and `test-package-compiler/cases`. The runtime cannot see most of them: a non-inlined block's `self.n` reads a lexically captured map with no runtime call, so the probe goes in codegen (a build flag that makes every class-variable read or write inside a non-inlined block report its class, process and whether the home invocation is still live), with a static count of escaping closures (blocks returned or stored by a class method that read a class variable) alongside. Gate: every *write* hit must be one of the four `with_snapshot/2` sites, or a test that pins today's silent loss; anything else is a user-visible breakage to document in Migration Path. Read hits are not breakages (they keep their captured value) but are counted by shape to tune the `class-state-abroad` lint so it fires on the shapes that occur.

**Phase 1: tests that pin the semantics (M, in parallel with Phase 2).**
- A BUnit matrix, one fixture per (sealed, open, subclass override) × (top level, arm, letrec loop, loop condition, fold loop, `on:do:` body, `on:do:` handler, a non-matching `on:do:` with an `ensure:` between it and the outer catch, `ensure:`, bare block, stored closure run within the invocation, stored or returned closure run from a later invocation of the same class (live read, write succeeds), the same closure run from a foreign process or after the class process is idle (captured read, write raises), block carried to another class's method synchronously, block carried to an actor asynchronously, `performLocally:`, `class initialize:`), asserting the §4/§5 answers: live reads at home, captured reads elsewhere (equal to the live value in the synchronous row, the creation-time value in the asynchronous and escaping rows), and `class_state_unreachable` on every write abroad. It lands green in Phase 1: each fixture whose current answer differs is committed with today's answer and a `PIN-BUG` marker citing this ADR's implementation issue, the convention in `docs/agents/expanded.md` § State-Threading Codegen point 5, and Phase 3 flips every pin. Together with the generator corpus below it is the gate for Phase 3.
- Extend `arb_program` (`crates/beamtalk-core/src/test_helpers.rs`) to class state, loops, exception handling, stored closures and writing, plain and late-bound self-sends, with the agreement oracle: the sealed, open and subclass-override spellings of one program must answer the same, and must match a reference interpretation of §4: every write is immediate, every write inside a protected region is undone when an error crosses that region's catch, every write of an invocation is undone when an error escapes it, and a `^` undoes nothing. The generator must emit `on:do:`, `ensure:` and `Result tryDo:` with raises inside the protected block, and self-sends in loop conditions. Run it against `main` and record the failure rate. This is the gate for Phase 3, as the guideline in `docs/agents/expanded.md` § State-Threading Codegen requires of any change to these lowerings; removing a family by construction is not exempt, because the same deletion touches the `State` and `Self` families' lowerings in the same files, and the generator is what shows those still hold.

**Phase 2: runtime (M).** Lands as one change with Phase 3, on one branch merged together, not merely in the same release: its helpers are inert until codegen emits the §2 calls, but its removals (`class_var_result` handling, the `n+2` ClassBuilder arity, the 3-arity class-side extension path) break the convention today's codegen still produces, so neither phase can be on `main` without the other. The ABI gate is the last item of Phase 3.
- Install, read back and erase the map in `invoke_class_method/7` and `invoke_class_extension/7`, with the absent-on-entry assertion first. ADR 0084 builder funs run inside those and need the arity change in `put_class_method/4`, which rejects an old `n+2` fun with a structured error naming the selector; `beamtalk_extensions:register` likewise refuses a 3-arity class-side extension fun with a structured error, so an extension module or ClassBuilder script built by an older compiler fails at install, not silently.
- Implement `get/get_late/put/clear/has`, their 3-arity captured-fallback forms and `capture/2`, `with_snapshot/2` (key plus read-only marker, resolved by class name, never the home entry), `snapshot/0`, `restore/1` and `protect/1` (via the `'$bt_class_vars_home'` entry that the two entry points install and erase with the key), `class_state_unreachable` and `class_state_read_only`; wrap the three supervisor-definition sites, `run_initialize/1` and `local_call/3` in `with_snapshot/2`; wrap every runtime catcher that runs Beamtalk blocks in `protect/1` (`beamtalk_result:'tryDo:'/1` and any other `catch` in `beamtalk_runtime`/`beamtalk_stdlib` that invokes a block argument; the 44 runtime modules with a `catch` that BT-3675 counted are the audit list), with an EUnit test per site that a write inside the protected block is discarded and a write before it is kept, plus tests that `protect/1` passes both `$bt_nlr` tuple shapes through without restoring, that `with_snapshot/2` erases its key and marker when `Fun` raises, that `snapshot/0` answers `none` and `restore/1` and `protect/1` are pass-throughs with no home entry (a foreign process, no invocation, a `with_snapshot/2` region with no live invocation) and plant nothing; that `X` mid-invocation calling `performLocally:` on `Y` leaves `X`'s home entry in place and `X`'s restore still works afterwards; that a write through a read-only snapshot raises; that `with_snapshot/2` resolves the snapshot after a class-process restart; and a BUnit test that a block writing the caller's class variables is discarded by a caught error inside a direct-called method's `on:do:`, inside an instance-side method's `on:do:`, and inside another class's invocation alike.
- Make `local_call/3` pass its receiver class object as `ClassSelf` instead of `nil`, change its doc contract from "class-variable mutations are discarded" to the §5 snapshot rule, and update the `performLocally:` paragraph of `docs/beamtalk-language-features.md` to match.

**Phase 3: codegen (L).**
- Emit the §2 calls and the §3 convention for class methods, class-side extension funs and ClassBuilder funs, the `capture/2` binding at the creation of every block literal that reads a class variable with the captured-fallback reads inside it, emit `snapshot/0` before and `restore/1` as the first statement of the catch arm's non-NLR branch of every compiled `on:do:` in every method context, with the `CatchWithoutClassVarRestore` verifier check, and add the `class-state-abroad` lint (update `docs/development/surface-parity.md` for the new diagnostic).
- Delete everything in §6 and the tests that pin the deleted machinery (`class_var_scope_tokens.rs`, `class_var_shadow_contract.rs`, `class_var_shadow_writes.rs`, the `ClassVars` parts of `class_var_bind.rs`). Regenerate the affected snapshots.
- Flip every `PIN-BUG` test to the correct answer: the existing `BT-3682` and `BT-3691` pins, the stored-closure and section-literal limits, and the Phase 1 matrix pins that cite this ADR's implementation issue. Close BT-3682, BT-3691, BT-3693 and BT-3696 against those tests; BT-3694 (an unbound actor `State`, not `ClassVars`, in a `whileTrue:` condition) closes only if the matrix's loop-condition row passes, otherwise it stays open as an actor-context bug.
- `just verify-threaded-ir`, `just test` and the Phase 1 matrix must pass.
- Conditional, only if either Phase 0 gate (the class-variable loop or the `snapshot/0` overhead) makes inlining the adopted lowering: the `class_var_keys` leaf in `beamtalk-codegen`, the `build-stdlib` regeneration of the checked-in `beamtalk_class_vars_keys.hrl`, its inclusion by `beamtalk_class_vars`, and the `check-generated-builtins` extension that guards it.
- Last: record the calling convention as a `class_var_abi` entry in `__beamtalk_meta/0`, and make the loader (`beamtalk_object_class` registration, the hot-reload path, and ADR 0125 §2.3's compatibility preflight, which already reads `__beamtalk_meta/0`) refuse a module whose ABI differs from the running runtime's or from its loaded superclass chain's with a structured `abi_mismatch` error that names the module and says to recompile. The gate covers compiled Beamtalk class modules, identified as today's loader does by an exported `__beamtalk_meta/0`. Among those, a module whose metadata has no `class_var_abi` entry, which is every module compiled before this release, counts as the old ABI and is refused; the check is "equal to the current value", never "present and unequal", and it applies whether or not the module defines class methods, since the entry costs nothing and a per-module exemption is one more rule. Modules that do not export `__beamtalk_meta/0` (hand-written Erlang class modules and EUnit fixtures, which `beamtalk_object_class` already registers through its `function_exported` fallbacks) are outside the gate; their obligation is the rewritten FFI rule in `docs/development/erlang-guidelines.md` (use `beamtalk_class_vars`, never return `class_var_result`), and the in-tree ones are updated in Phase 2. A Phase 3 EUnit test loads a pre-change compiled `.beam` fixture and expects `abi_mismatch`, and another loads a metadata-less Erlang class module and expects it to register. There is no dual-ABI window: an old-ABI caller direct-calls `class_<sel>(ClassSelf, ClassVars, ...)` on whatever module defines the method, so an old module and a new module in one hierarchy cannot be bridged at the call site, and bridging only at the runtime dispatch boundary would reintroduce the two-representations reconciliation this ADR removes. The upgrade across this release is a whole-node restart, which is the only upgrade `beamtalk release` v1 supports anyway (ADR 0125 §2.1); when the relup phase lands, `class_var_abi` is part of the `__beamtalk_meta/0` record its appup derivation diffs (ADR 0125 §2.2), so a change to it marks the release as restart-only. A package compiled by an older compiler is refused at load until it is recompiled.

**Phase 4: docs and cleanup (S).**
- Mark ADR 0110 Superseded; amend ADR 0013 §1, ADR 0066 (class-side extension fun shape), ADR 0084, ADR 0109 (what a block running elsewhere may do), ADR 0111 and ADR 0122; rewrite the class-variable parts of `docs/beamtalk-language-features.md`, `docs/development/debugging.md`, `docs/development/benchmarks.md`, `docs/development/erlang-guidelines.md` (FFI rule) and `docs/development/surface-parity.md` (the removed diagnostics and the new lint); update the state-threading rule in `CLAUDE.md` and `docs/agents/expanded.md` § State-Threading Codegen, which still list `ClassVars` and class-var shadow-writes among the families, and the "Blocks into class methods" rule in `CLAUDE.md`.

Affected components: runtime (`beamtalk_class_dispatch`, `beamtalk_object_class`, `beamtalk_supervisor`, `beamtalk_extensions`, `beamtalk_result`, new `beamtalk_class_vars`), codegen (`core_erlang` class-method, dispatch, control-flow, block and exception lowering, `threaded_ir`), semantic analysis (the stored-closure validator removed, the `class-state-abroad` lint added), docs. Parser, AST and type checker are unaffected; the LSP, REPL, CLI and MCP surface the lint and the new runtime errors through their existing diagnostic and error paths.

## Migration Path

- **Caught-error behaviour keeps its documented meaning.** The language documentation's statement that "a write made by a callee that raised is never kept, even when the raise was caught" stays true, now as a property of the protected region rather than of each send. The roughly 30 BUnit tests in `self_send_override_blocks_test.bt`, `self_send_plain_reply_test.bt` and `class_var_nlr_shadow_test.bt` that pin discard-on-caught-raise keep their answers, and the two known-wrong pins that involve a caught raise (`testStoredClosureReadsRevertedValueAfterCaughtRaise`, `testSealedPinBugRaisedIterationBetweenKept`) flip to the answers their comments call correct. The only observable difference is in shapes the token design could not cover: a write inside a protected block made through a stored closure or a block given to a higher-order method is now discarded with the rest of the region instead of being resurrected or lost.
- **Programs that passed a block that writes class variables to another class's class method or an actor** will get `class_state_unreachable` instead of a lost write; a block that only reads keeps working, with the value it captured, exactly as today. A class-side extension on an Actor subclass, or a ClassBuilder script, compiled by an older compiler is refused at install with a structured error. Fix: read into a local before passing the block, or return the value and assign it in the home class's method.
- **Supervisor definitions, `class initialize:` and `performLocally:` keep reading** what they read today (the snapshot as of the last completed invocation). A class-variable write there now raises `class_state_read_only`: for `class children` and the other `static_init`/`dynamic_init` selectors that replaces a supervisor-startup crash, for the `withClassMethod:` factory and `initialize:` it replaces a silent discard, and for `performLocally:` it replaces a raw `badkey`.
- **Escaping closures** (a block returned or stored by a class method and run after that invocation ended) that read a class variable keep seeing the value captured at creation when they run outside any invocation of their class, as today, and now get a `class-state-abroad` warning saying so; one that writes there raises `class_state_unreachable` where today it is a compile error or a lost write. Run from a later invocation of the same class, such a block is at home: it reads live and its writes take effect, which today's compiler does not allow.
- **Programs that worked around the old limits** (an unused local added to thread a loop body, `@expect stored_closure`) keep working; the workaround becomes unnecessary and `@expect stored_closure` becomes an unused-expectation warning to remove.
- **Hot upgrades** across the release that changes the calling convention are not supported: the node restarts, and every package is recompiled. A module from an older compiler is refused at load with an `abi_mismatch` error that names it.

## Open Questions

1. **Actor state.** Caught errors now agree across both kinds of state. Should actor state also move to a single home, so that stored closures and blocks given to higher-order methods behave the same for actors as §5 makes them for classes (BT-3580 is the actor twin of this bug class)? That needs its own ADR.
2. **Verifier visibility in release builds.** `report_threaded_ir_verify_errors` records an `internal:` error diagnostic in release builds, but BT-3693 reports that the release CLI printed none. Phase 1's property test must fail on that diagnostic, and the CLI path should be checked.

## Implementation Tracking

**Epic:** BT-3701
**Issues:** BT-3702, BT-3703 (Phase 0); BT-3704, BT-3705 (Phase 1); BT-3706, BT-3707, BT-3708 (Phase 2); BT-3709, BT-3710, BT-3711, BT-3712, BT-3713 (Phase 3); BT-3714, BT-3715 (Phase 4)
**Status:** Planned

## References

- Related issues: BT-3666, BT-3667, BT-3669, BT-3675, BT-3676, BT-3681, BT-3682, BT-3683, BT-3688, BT-3690, BT-3691, BT-3692, BT-3693, BT-3694, BT-3696, BT-3700 (late-binding guard, follow-up), BT-3580, BT-912
- Related ADRs: ADR 0013 (class variables), ADR 0041 (universal state-threading block protocol), ADR 0084 (ClassBuilder), ADR 0109 (blocks run where invoked), ADR 0110 (class-variable shadow, superseded by this ADR), ADR 0111 (ThreadedIr verifier), ADR 0122 (slot families), ADR 0124 (late slots), ADR 0125 (releases and upgrades), ADR 0126 (distribution), ADR 0127 (traits), ADR 0129 (class-side facades, Phase 0b direct calls)
- Code: `runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl` (`invoke_class_method/7`, scope helpers), `beamtalk_object_class.erl` (`#class_state{}`, `local_call/3`), `beamtalk_actor.erl` (`self_dispatch/2`), `crates/beamtalk-codegen/src/core_erlang/generator/version.rs`, `dispatch_codegen.rs`, `gen_server/methods.rs`, `threaded_ir/verify.rs`
- Tests: `stdlib/test/self_send_override_blocks_test.bt`, `stdlib/test/self_send_plain_reply_test.bt`
- Docs: `docs/development/benchmarks.md` § Class-side self-send cost, `docs/beamtalk-language-features.md` § Passing Blocks Through Class Methods, `docs/agents/expanded.md` § State-Threading Codegen
- External: Erlang `erlang:put/2` and `get/1` (process dictionary), `gen_server` callback semantics; Pharo class variables and class-instance variables
