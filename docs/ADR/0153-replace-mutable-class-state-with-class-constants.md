# ADR 0153: Replace Mutable `classState:` with Immutable Class Constants

## Status
Proposed (2026-11-24)

The owner has taken the decision in principle: remove mutable class variables, keep a class-level declaration for immutable constants, and remove without a deprecation period. This ADR designs the constants feature and lists what is deleted. The points still open are in [Open Questions](#open-questions).

If accepted, this **supersedes [ADR 0130](0130-class-variables-live-in-the-class-process.md)** and **[ADR 0019](0019-singleton-class-variables.md)**, supersedes **§1 of [ADR 0013](0013-class-variables-class-methods-instantiation.md)** (class variables; its class-method and instantiation sections stay), and **amends [ADR 0142](0142-class-process-owns-only-class-state.md)** (rules 1 and 2 of §1 have nothing left to test), **[ADR 0084](0084-class-side-runtime-method-fun-dispatch.md)** (`classVars:`), **[ADR 0123](0123-versioned-state-migration.md)** (the no-class-variable rule for migrations) and **[ADR 0124](0124-slots-late-assignment-definite-assignment.md)** (`late classState:`).

**How to read the evidence.** A claim marked *(read)* was checked by reading the cited code or document on `main` at `f1fcd5e`. *(survey)* means it comes from the owner's usage and cost survey and was not re-checked here. *(inferred)* follows from what was read but was not checked directly. Nothing in this ADR was verified by running the system.

## Context

### Problem statement

`classState:` declares a mutable class variable, the Beamtalk form of a Smalltalk class-side instance variable.

```beamtalk
sealed Object subclass: MyRegistry
  classState: current = nil

  class current => self.current
  class current: instance => self.current := instance
```

A Smalltalk class variable is shared memory. The BEAM has no shared mutable memory, so ADR 0130 emulates it: the variables live in the class's gen_server (the class process), and a class method of a class that declares any runs inside that process. That makes a class with `classState:` an actor already, but one with its own rules, its own machinery and its own bugs. Three facts say it is not worth keeping.

**1. Nothing uses it.**

| Where | `classState:` declarations |
|---|---|
| `stdlib/src` | 0. Four files mention it, all in doc comments (`behaviour.bt:238, 263`, `class_builder.bt:126, 300`, `system_navigation.bt:1116`, `process_dictionary.bt:26`) *(read)* |
| `examples/` | 0 *(read)* |
| `src/` of beamtalk-agents, announcements, buccaneer, catalina, hpack, http, linear, mint, oauth, yaml, symphony, exdura, liquid | 0; every hit is a doc comment *(survey; those repositories are outside this session's access)* |
| `stdlib/test`, `tests/repl-protocol`, `test-package-compiler/cases` | 118 files declare one (90 in `stdlib/test/fixtures`, 18 in `tests/repl-protocol`, 10 in `test-package-compiler/cases`); 140 mention it *(read; the survey counted 106, by a different method)* |
| `benchmarks/`, `runtime/perf` | 5 `.bt` files declare one; 38 files mention class variables, most of them recorded result tables *(read; the survey counted 18)* |

Every declaration is a test or a benchmark of the feature. Downstream code that needs shared state uses named actors instead: `named:` / `spawnAs:` in 21 `src` files *(survey)*.

**2. It is expensive to keep.**

| Piece | Size *(read: `wc -l`)* |
|---|---|
| `runtime/apps/beamtalk_runtime/src/beamtalk_class_vars.erl` | 758 |
| `beamtalk_class_var_abi.erl`, `beamtalk_class_var_errors.erl` | 206, 189 |
| `crates/beamtalk-codegen/src/core_erlang/class_var_access.rs`, `class_var_keys.rs` | 513, 172 |
| `crates/beamtalk-core/src/semantic_analysis/validators/class_state_abroad.rs` | 563 (and 969 lines of tests) |
| `validators/class_process_ffi_validators.rs` | 1,040, about half for the class-process case *(survey)* |
| Harness: `crates/beamtalk-core/src/test_helpers/class_var_program.rs`, `crates/beamtalk-cli/tests/cli/cli_class_var_agreement.rs`, `crates/beamtalk-codegen/tests/class_var_agreement.rs` | 2,503, 1,370, 307 |
| EUnit: `beamtalk_class_vars_tests.erl`, `beamtalk_class_var_abi_tests.erl` | 852, 538 |
| CI: the nightly `class-var-corpus` job (`.github/workflows/fuzz.yml:304`) and its triage path (`fuzz-triage.yml`), `just test-class-var-corpus`, `just lint-class-var-catchers` | |

Beyond the dedicated files, 145 Rust files and 60 Erlang files (31 source, 29 test) mention class variables *(read: `grep -l`)*.

A Linear search for "class variable" over the last 90 days returns 86 issues *(read)*; the survey puts about 65 of them on class variables and the class process. Many are lost writes: BT-3691 *(read)*, BT-3683, BT-3667, BT-3611, BT-3584 *(survey)*. Two epics were spent on it: BT-3701 (ADR 0130) and BT-4297 (ADR 0142) *(read)*.

**3. Code that never declares a class variable pays for it.**

- **Every compiled `on:do:`** takes a class-variable snapshot before its `try` and restores it in the catch arm: `class_var_snapshot_let_doc` (`class_var_access.rs:133-143`), emitted for `CatchEntry::ClassVarSnapshot` at `threaded_ir/emit.rs:663` *(read)*. BT-4983 counted 35 such snapshots on one HTTP request *(read: issue title)*.
- **`Result tryDo:`** runs its block under `beamtalk_class_vars:protect/1` (`beamtalk_result.erl:228, 285`) *(read)*, and every runtime catcher that runs a block must do the same, enforced by `just lint-class-var-catchers`.
- **Every class send** reads a guard, `direct_call_ok/2` (`beamtalk_class_shadow_flags.erl:655`), one of whose three inputs is "the class or an ancestor declares class variables" *(read)*. BT-4987 measured that read at 15 ns of a 22-36 ns class send *(read)*. Removing class variables removes an input, not the read: see Open Question 5.

### The conceptual argument

Because the class process is where class variables live, the language carries rules that exist only to explain it:

- **Two execution modes for class methods** (ADR 0142). A class method runs in the caller unless the class or any ancestor declares class variables. The rule is computed in Rust (`ClassHierarchy::runs_class_method_in_caller`, `class_hierarchy/mod.rs:480`), mirrored at run time (`beamtalk_class_metadata:chain_has_class_variables/1`) and re-checked on every send *(read)*.
- **Two lints split on that rule**: `class-process-ffi` and `class-method-process-resource` *(read: language features, lines 1356-1357)*.
- **Writes discarded at a catch boundary** (ADR 0130 §4), which differs from Smalltalk and needs the snapshot above.
- **Blocks that read captured values abroad and raise on a write** (`class_state_unreachable`, ADR 0130 §5), with the `class-state-abroad` lint.
- **`performLocally:withArguments:`** and its read-only snapshot (`class_state_read_only`).
- **Deadlocks and `dispatch_error`** between class processes (language features, line 4134).
- **Class variables are per class and not inherited**, so an inherited method fails at run time with `undeclared_class_variable` unless the subclass redeclares the variable (language features, lines 1377-1389).

A newcomer who writes one `classState:` line meets all of this. An Actor has one rule: it owns its state, and you send it messages.

### What is actually wanted

Smalltalk class variables are used for three things. Only the third needs a class-level declaration.

| Use | BEAM-native home |
|---|---|
| A singleton or default instance | A named Actor (`spawnAs:`, `named:`) |
| Counters and caches | `AtomicCounter`, `Ets`, or an Actor |
| Constants and lookup tables | Nothing declarative today. A class method whose body is a literal works (`class protocolError => 1`, the BT-4363 "named-constant idiom"), but a computed table is rebuilt on every call |

### Constraints

- **No image mechanics** (`docs/beamtalk-principles.md` §4). A constant must mean the same in the REPL, `beamtalk run`, `beamtalk test` and a release.
- **Hot reload.** A class is reloaded by loading a new module whose `-on_load` hook registers it; the old code stays current until the hook returns (ADR 0142 §2) *(read)*.
- **`persistent_term`.** Reads are constant time and copy nothing. Replacing or erasing a key whose value is not an immediate term makes the VM scan every process heap for the old term (a global GC).
- **ADR 0152 is in flight.** BT-5141 (started) removes cross-file `>>` and edits `beamtalk_class_dispatch.erl` and `beamtalk_class_shadow_flags.erl`, which this ADR also edits.

## Decision

### 1. The rule

**Shared mutable state lives in an Actor** (or in `AtomicCounter` / `Ets`, which are owned by a process). A class has no mutable state. `classState:` is removed. A class may declare **constants**: named values that never change after they are first computed.

### 2. Syntax: `constant:`

```beamtalk
Object subclass: HttpStatus
  constant: notFound = 404
  constant: reasons :: Dictionary(Integer, String) =
    #{200 => "OK", 404 => "Not Found", 500 => "Internal Server Error"}
  constant: tokenPattern :: Regex = (Regex from: "[A-Za-z0-9_]+") unwrap

  class reasonFor: code :: Integer -> String =>
    self reasons at: code ifAbsent: ["Unknown"]
```

- The form is `constant: name [:: Type] = expression`. The initialiser is required; `constant: x` is a compile error.
- **A constant is read with a message.** The declaration defines a class-side method of the same name. `HttpStatus notFound` reads it from anywhere, `self notFound` from a class method, `self class notFound` from an instance method.
- **There is no assignment.** No syntax writes a constant.
- **`self.name` on the class side is removed.** A class has no slots. `self.reasons` in a class method is a compile error whose hint says to send `self reasons`.

Why a new keyword and not `classState:` with a restriction: `classState:` says "state", and code that keeps the keyword would silently change meaning. A new keyword lets the compiler give old code one precise error. Why `constant:` and not `classConstant:`: there is no per-instance constant to tell it apart from (`field:` is the immutable per-instance slot), and it sits beside `state:` and `field:` as a third one-word declaration. See Open Question 1.

Why a method and not a slot: it keeps message passing as the only way to reach a class's data, needs no new access syntax, makes inheritance and overriding the ordinary method rules, and lets a scalar constant reuse the constant-method fast path that already exists (§4).

```beamtalk
>> HttpStatus notFound
=> 404
>> HttpStatus reasonFor: 404
=> "Not Found"
>> HttpStatus constantNames
=> #(#notFound, #reasons, #tokenPattern)
```

Errors:

```text
error: `classState:` was removed (ADR 0153). A class has no mutable state.
  hint: for a value that never changes, declare `constant: current = ...`.
        For state that changes, use an Actor (`spawnAs:` / `named:`), an
        AtomicCounter or an Ets table.

error: `self.reasons` in a class method: a class has no slots.
  hint: `reasons` is a constant of HttpStatus; send `self reasons`.

error: constant `notFound` and class method `notFound` are both defined in HttpStatus.

error: constant `registry` is initialised with an Actor (`Registry spawn`).
  hint: a constant is computed once and never refreshed, so it would outlive
        the process. Start the actor with `spawnAs:` and look it up with `named:`.
```

### 3. When the value is computed

A constant is computed **at most once per load of its class, on first read**. Nothing is computed when the class loads.

- **Literal initialisers are not computed at all.** When the initialiser is a literal (a number, string, symbol, character, `true`, `false`, `nil`, or a list, tuple or dictionary literal whose elements are literals), the generated method answers the literal. It lives in the module's constant pool.
- **Any other initialiser is evaluated on first read** and the value is kept. Later reads answer the kept value.

The two cases behave the same; the split is an optimisation, not a rule to learn.

**Why not at load time.**
- During a reload the class's `-on_load` hook runs while the *old* code is still current, so an initialiser that calls one of the class's own methods would run the old method *(inferred from ADR 0142 §2)*.
- An initialiser that names another class would depend on load order, which differs between the REPL, `beamtalk run` and a release.
- An initialiser that raises would fail the module load, and the user would see a load error and not their own exception.

**If the initialiser raises**, the exception reaches the reader unchanged, nothing is kept, and the next read evaluates it again. A constant is never left "poisoned".

**The initialiser must be pure.** Two processes that read a constant for the first time at the same moment both evaluate it, and one value is kept. Three checks support this:
- An initialiser whose static type is an Actor, `Pid`, `Port`, `Reference`, `Ets` or `AtomicCounter` is a compile error (the fourth error above). These are identities owned by a process, not values.
- An initialiser that reads its own constant is a compile error. A cycle through other constants or methods is caught at run time: the evaluation records the constant in the process dictionary while it runs and raises `constant_cycle` on re-entry.
- `self` in an initialiser is the declaring class, whatever class received the message. The value therefore cannot differ by receiver.

### 4. Storage

| Initialiser | Generated class method | Cost of a read |
|---|---|---|
| Scalar literal | Body is the literal. It is certified in `constant_class_methods` and a class-literal send answers it in place (the existing BT-4363 / BT-4504 path, `ast/method.rs:literal_constant_body`) *(read)* | 1 reduction, as today |
| Literal collection | Body is the literal | A guarded class send; no allocation |
| Computed | Body reads `persistent_term` and falls to the runtime on a miss | A guarded class send plus one `persistent_term:get/2`; no copy |

For a computed constant the method is, in effect:

```erlang
class_tokenPattern(_ClassSelf) ->
    Key = {beamtalk_constant, 'bt@app@http_status', tokenPattern, <<"3f9a…">>},
    case persistent_term:get(Key, '$beamtalk_constant_unset') of
        '$beamtalk_constant_unset' ->
            beamtalk_constants:compute(Key, fun() -> <initialiser> end);
        Value ->
            Value
    end.
```

- `beamtalk_constants` is a new leaf runtime module of a few dozen lines: `compute/2` (cycle check, evaluate, `persistent_term:put/2`, answer) and `erase_module/2`. It is the only writer of these keys, so no rule crosses the Rust and Erlang boundary beyond the key shape, which codegen emits and the runtime never rebuilds.
- The last key element is a hash of the class's source, baked in at compile time. Old and new code of a reloaded class therefore use different keys (§5).
- Codegen builds this with the `Document` / `form` API like any other method body (ADR 0089).

**Why `persistent_term` and not the alternatives.**
- *ETS* copies the value into the reader's heap on every read. Lookup tables are the main use, and they are the worst case for that.
- *The class process* is what this ADR removes.
- *A compile-time value* would need the Rust compiler to run Beamtalk. It cannot.
- *A module generated at run time to hold the value* is what `persistent_term` replaced in OTP.

### 5. Reload

When a class is reloaded or removed, the runtime erases the computed-constant keys of the code it replaces. It knows them from the old module's metadata, which lists the computed constant names and the source hash.

- A literal constant changes with the module. Nothing else happens.
- A computed constant is evaluated again, by the new code, on its next read. That is also how a constant picks up a change in a method its initialiser calls: reload the class that declares the constant.
- A constant that was never read has no key, so it costs nothing to erase.
- **Cost.** Erasing a key whose value is not an immediate term triggers a global GC. So reloading a class costs one global GC when it has computed constants that were read. This is the cost the static-call keys already have on every reload, since their values are class references *(inferred: `static_call_target/4` stores a class reference, ADR 0142 §2)*. The runtime erases all of a module's keys in one pass. It is a development-time and upgrade-time cost, not a steady-state one.
- **Old code that is still running** when the reload happens may evaluate the old initialiser once more and store it under the old key. New code never reads that key. One stale entry can remain until the class is next reloaded. This is accepted; the alternative (one key for both versions) would let old code overwrite the new value.

`Workspace` state, the REPL and `beamtalk test` need no special case: a constant has the same value wherever it is read.

### 6. Subclasses

A constant is **inherited and can be overridden**, because it is a class method.

```beamtalk
Object subclass: Shape
  constant: sides = 0
  class describe => "A shape with " ++ self sides printString ++ " sides"

Shape subclass: Triangle
  constant: sides = 3

Shape describe      // => "A shape with 0 sides"
Triangle describe   // => "A shape with 3 sides"
```

- A subclass that does not redeclare a constant answers its ancestor's value. One value exists per declaring class, not per receiver.
- A subclass may redeclare it, or define a class method of that name. `self sides` late-binds like any self-send.
- This reverses ADR 0013's "per class, not inherited" rule, which existed because a mutable variable shared with subclasses would be shared state. An immutable value has no such problem.

### 7. Types, reflection and tooling

- **Types.** `constant: n :: T = e` gives the class method the signature `class n -> T`, and `e` is checked against `T`. Without an annotation the return type is the inferred type of `e`. In a `typed` class the annotation is required for a computed constant, as it is for a method's return type *(inferred: follows the existing rule for class methods)*.
- **`late`** does not apply. `late constant:` is a compile error: a constant always has an initialiser, and first-read evaluation already covers "cannot be computed at load". `late classState:` goes with `classState:`. `late state:` is unchanged.
- **Reflection.** `Behaviour>>classVarNames` and `allClassVarNames` (`stdlib/src/behaviour.bt:271, 279`) become `constantNames` and `allConstantNames`. A constant is read reflectively with `perform:`. The class-side forms of `fieldAt:`, `fieldAt:put:`, `hasField:` and `clearField:` (BT-4148) are removed, with `ClassInfo::is_class_var_reflection_selector` and its conformance fixture.
- **Browse, LSP, `beamtalk doc`.** A constant is listed with the class's class-side methods, marked as a constant, with its declared type and doc comment. Completion offers it wherever it offers class methods. Hover shows the initialiser.
- **ClassBuilder.** `classVars:` and `addClassState:default:` (`stdlib/src/class_builder.bt:134, 307`) are removed. A class built at run time gets a constant from `addClassMethod:body:` with a block that answers the value. No new builder API is added (Open Question 6).
- **Migrations (ADR 0123).** The rule that a `migrateFromVN:` method may not read class variables is deleted with its validator. A migration may read a constant, since reading one does not need the class process.

### 8. What replaces each use of a class variable

**A singleton or default instance** is a named Actor. `current` asks the runtime each time (principles §4).

```beamtalk
Actor subclass: Registry
  state: entries = #{}

  class current -> Registry => (self named: #registry) unwrap

  register: key as: value => self.entries := self.entries at: key put: value
  lookup: key => self.entries at: key ifAbsent: [nil]
```

The actor is started by the application's supervisor under that name (`SupervisionSpec` with a `name`), so it is restarted if it dies. For scripts and the REPL, a lazily started form is useful, and it needs a small helper because "look up, and spawn if absent" races:

```beamtalk
  class current -> Registry => (self namedOrSpawn: #registry) unwrap
```

`Actor class >> namedOrSpawn:` is a proposed stdlib addition: answer the actor registered under the name, spawn it with `spawnAs:` when there is none, and on `name_registered` look it up again (Open Question 3). This replaces the `class current` example in `docs/beamtalk-language-features.md` § Class-Side Methods.

**A default instance that is a value** is a constant: `constant: default :: Config = Config host: "localhost" port: 80`.

**A counter** is an `AtomicCounter`, created by the process that should own it: `(AtomicCounter named: #orders) increment`.

**A cache** is an `Ets` table owned by an Actor or a supervisor, or an Actor's own `state:`.

**A constant or lookup table** is `constant:`.

### 9. What is deleted

The file list is in Implementation. In summary:

- The `classState:` declaration, `late classState:`, and class-side `self.x` reads and writes.
- **The class-variable execution mode for class methods.** ADR 0142 §1 rules 1 and 2 have nothing to test, so every class method runs in the caller unless its selector is process-bound or an ADR 0084 runtime fun shadows the class. `ClassHierarchy::chain_has_class_variables`, `beamtalk_class_metadata:chain_has_class_variables/1`, the `class_vars` kind of shadow flag, and the install / read-back / uninstall of the class-variable home in `invoke_class_method/7`.
- The class-variable ABI, error module and access module.
- The snapshot emitted before every `on:do:`, the restore in its catch arm, `VerifyError::CatchWithoutClassVarRestore`, block capture of class variables, and `protect/1` in `Result tryDo:` and every other runtime catcher.
- The `class-state-abroad` and `class-process-ffi` lints. `class-method-process-resource` stays and loses its "or declare `classState:`" advice.
- The error kinds `class_state_unreachable`, `class_state_read_only` and `undeclared_class_variable`.
- `performLocally:withArguments:` (Open Question 4).
- The class-variable fuzz harness, agreement corpus and nightly job.
- `ClassBuilder` `classVars:` and `addClassState:default:`; `beamtalk_runtime_api:set_class_var/3`.

**What stays, and why.** The class process stays as the owner of the class object: registration, metadata, reload, the method tables. The process-bound selectors keep their routing, for reasons that have nothing to do with class variables *(read: the doc comment on `PROCESS_BOUND_CLASS_SELECTORS`, `class_info.rs:507-539`)*:

| Selectors | Why they are process-bound |
|---|---|
| `new`, `new:`, `startLink`, `startLink:` | `basicNew` / `basicNewWith` read the class name and module from the class process's dictionary. The in-caller constructor shapes of BT-4099, BT-4464 and BT-4475 are unchanged, and their "no class variables in the chain" condition becomes always true |
| `spawn`, `spawnWith:` | Dedicated clauses in `beamtalk_class_dispatch:class_send_plain/3` |
| `supervise`, `spawnAs:`, `spawnWith:as:` | Supervisor routing, already started in the caller (ADR 0135 §1) |
| `runAll`, `runAll:`, `run:`, `run:method:` | The `TestCase` run selectors answer `test_spawn` and need the gen_server's `noreply` |

Whether any of these can leave the class process is a separate question and is not decided here.

### Single source of truth

- "Is this declaration a constant, and is its initialiser a literal?" is one function in `beamtalk-core`, next to `literal_constant_body` (`ast/method.rs`). Codegen, the type checker, the LSP and `beamtalk doc` call it.
- The `persistent_term` key shape is emitted by codegen and written only by `beamtalk_constants`. The runtime learns a module's keys from `__beamtalk_meta/0`. No hand-kept copy exists on the Erlang side, so no conformance fixture is needed. The generated header `beamtalk_class_vars_keys.hrl` and `class_var_keys.rs` are deleted.
- "Where does a class method run?" stays one rule, `runs_class_method_in_caller`, now with one fewer input.

## Prior Art

Summarised from the languages' documentation; not re-checked for this ADR.

| Language | What it does | What we take |
|---|---|---|
| **Pharo / Squeak** | Class variables (shared with subclasses) and class-side instance variables (per class), both mutable shared memory. Constants are usually class-side methods that answer a literal, or lazily initialised class variables (`^ Default ifNil: [Default := ...]`) | The class-side accessor as the way to read a constant. We drop the mutable variable behind it |
| **Newspeak** | No global namespace and no static state. A class has no mutable class-side state; shared state belongs to a module instance. Slots can be immutable (`x = expr`) | The closest precedent for removing class variables from a Smalltalk-family language |
| **Erlang** | No module-level mutable state. Constants are macros or functions that answer a literal. `persistent_term` (OTP 21.2) is the store for values read often and written rarely, with a global GC on update | The storage choice and its cost model |
| **Elixir** | Module attributes (`@name value`) are evaluated at compile time and inlined. State is a process (`Agent`, `GenServer`), ETS or `:persistent_term` | "Constants at the module, state in a process" as the idiom BEAM developers expect. We cannot evaluate at compile time, so computed constants are lazy |
| **Gleam** | `const` holds compile-time literals only; no function calls | The literal tier. We go further with computed constants because lookup tables are the main use |
| **Scala, Kotlin, Swift** | `lazy val`, `by lazy`, `static let`: computed once on first use. Scala and Kotlin evaluate again after an exception | First-read evaluation and the retry rule |
| **Java** | `static final` is computed when the class initialises. An exception there leaves the class unusable (`NoClassDefFoundError` afterwards) | The failure we avoid by not computing at load |
| **Pony, Dart** | Pony has no global mutable state; constants are functions on `primitive` types. Dart statics are lazy and per isolate, never shared | Actor languages do not share mutable statics |

## User Impact

- **Newcomer.** One fewer concept, and the one that is left is the familiar one: a named constant. The error for `classState:` says what to use. They no longer need to know what a class process is before writing a class method.
- **Smalltalk developer.** Loses class variables and class-side instance variables, which is a real departure. Keeps the idiom Smalltalk code already uses for constants (a class-side accessor), and gets inheritance and overriding by the ordinary method rules. The singleton pattern becomes a named Actor, which is where a Smalltalk developer on the BEAM ends up anyway.
- **Erlang / Elixir developer.** Gets what they expect: no module-level mutable state, constants that compile to literals or `persistent_term`, and class methods that are always plain functions in the caller (apart from the constructors). A constant is `Mod:class_name(ClassRef)` from Erlang.
- **Operator.** No class process serialising calls for any user class. Computed constants are visible with `persistent_term:get()` under `{beamtalk_constant, Module, Name, _}`. Reloading a class with computed constants costs a global GC; a release upgrade that reloads many such classes should expect one pass.
- **Tooling developer.** A constant is a class method with a marker, so completion, hover, go-to-definition and references reuse the class-method paths. The class-variable capture analysis and two lints go away.

## Steelman Analysis

### Keep `classState:` as it is

| Cohort | Strongest argument |
|---|---|
| Newcomer | "It is one line. An Actor is a class, a spawn and a name." |
| Smalltalk purist | "Class-side variables are part of the language I know. Every Smalltalk has them." |
| BEAM veteran | "ADR 0130 and 0142 already paid for it, and it now has a single home and a clear rule." |
| Operator | "It works and the bugs are fixed. Removing it is churn." |
| Language designer | "A class is an object; objects have state. A class without state is a module." |

### Keep the syntax, back it with a hidden Actor

| Cohort | Strongest argument |
|---|---|
| Newcomer | "I keep the one-line declaration and get the Actor semantics for free." |
| Smalltalk purist | "The syntax survives." |
| BEAM veteran | "It is honest about being a process." |
| Language designer | "One state model underneath, two spellings on top." |

### Constants only through class methods (no new syntax)

| Cohort | Strongest argument |
|---|---|
| Newcomer | "Nothing new to learn: `class notFound => 404`." |
| Smalltalk purist | "This is exactly what Pharo does." |
| BEAM veteran | "A function that answers a literal is the Erlang idiom, and it is already 1 reduction." |
| Language designer | "The smallest language. Add memoisation later if someone needs it." |

### ETS-backed `classState:`

| Cohort | Strongest argument |
|---|---|
| Newcomer | "Read and write from anywhere, no process rules, no new error." |
| BEAM veteran | "Concurrent reads, no gen_server bottleneck. ADR 0013 listed it as the scaling path." |
| Operator | "Visible in observer." |

### Tension points

- The newcomer and the language designer both have a case for "no new syntax". It is the strongest alternative, and the answer to it is the computed lookup table, which a plain class method rebuilds on every call.
- The Smalltalk purist loses something in every option except the first. The decision accepts that, as ADR 0152 did for open classes.
- The BEAM veteran's argument for the status quo ("it is paid for") is true of the build cost and false of the running cost: the snapshot, the guard input and the nightly job are paid on every `on:do:`, every send and every night.

## Alternatives Considered

### Keep `classState:` as it is
Rejected. Nothing uses it (Context §1), and code that does not use it pays for it on `on:do:`, on `Result tryDo:` and on every class send (Context §3). The "classes are objects with state" argument is sound in an image; on the BEAM the state is in a process either way, and an Actor says so.

### Keep mutable `classState:`, backed by an Actor transparently
The compiler would generate a hidden actor per class and turn `self.x := v` into a message to it. Rejected.
- It is what ADR 0130 already is, with the class process as the hidden actor, and it brought all the rules listed under "The conceptual argument".
- Each read becomes a call to another process (microseconds), or else the method must run inside the actor, which is the execution-mode split this ADR removes.
- A read-modify-write across two messages is a race the syntax hides. A user who writes an Actor sees the message boundary.

### Constants only through class methods
`class notFound => 404` already works and is 1 reduction. Rejected as the whole answer, kept as the mechanism.
- A computed table (`class reasons => self buildReasons`) is rebuilt on every call. Users would memoise by hand, and the only tools for that are the ones this ADR sends them to for state.
- A declaration carries intent: the tools can list a class's constants, the compiler can reject an Actor in one, and the reader knows the value never changes.
- The cost of the syntax is small because it lowers to a class method.

### ETS-backed `classState:`
A per-class table readable and writable from any process. Rejected, for ADR 0130's reasons and one more.
- A write is visible at once to every process, so a class method that fails half way leaves torn state, and two callers race on read-modify-write.
- It makes class methods concurrent over shared mutable state with no owner, which is the opposite of the rule in Decision §1.
- Every read copies the value. `Ets` already exists for users who want exactly this, with the table visible in their code.

### Reuse the `classState:` keyword for constants
Rejected: see Decision §2. The word is wrong, and existing code would change meaning without an error at the declaration.

### Compute constants at load time
Rejected: see Decision §3. Old code is current during a reload's `-on_load`, the result depends on load order, and a raising initialiser becomes a load failure.

### Deprecate for a release, then remove
Rejected by the owner, as for ADR 0152: with no user of the feature there is nobody for a warning to reach.

## Consequences

### Positive
- One rule for shared mutable state: an Actor (or a process-owned counter or table).
- One execution mode for ordinary class methods. A class method is a function in the caller.
- `on:do:` and `Result tryDo:` lose the snapshot and restore. The catcher audit and its lint go away.
- About 3,400 lines of dedicated source, about 6,500 lines of dedicated tests and harness *(read: the `wc -l` figures in Context)*, a nightly job and three error kinds are deleted.
- Lookup tables get a declarative, copy-free home.
- Constants are inherited, which class variables were not.
- A migration method can read a constant.

### Negative
- A breaking change with no deprecation period. Any private code that declares `classState:` stops compiling.
- A departure from Smalltalk that every Smalltalk developer will notice.
- The singleton pattern takes more lines: an Actor, a name, and a supervisor entry or `namedOrSpawn:`.
- Reloading a class with computed constants costs a global GC.
- A computed constant's initialiser can run more than once (concurrent first reads, a retry after an exception, each reload), so it must be pure. The compiler catches the common mistake (an Actor) and not every side effect.
- A constant does not notice a change in a method its initialiser calls until its own class is reloaded.
- About 120 test fixtures and their snapshots are rewritten or deleted.

### Neutral
- The class process stays, for the class object and the process-bound selectors.
- The per-send guard read stays until Open Question 5 is settled.
- `AtomicCounter` and `Ets` tables are owned by the process that creates them, so replacing a class-variable counter means deciding who owns it. That decision was hidden before; it is not new work.

## Implementation

Sizes are rough (S, M, L). Phases 0 and 1 do not depend on BT-5141. **Phases 2 and 3 land after BT-5141 (ADR 0152) merges**: both edit `beamtalk_class_dispatch.erl` and `beamtalk_class_shadow_flags.erl`, and after ADR 0152 the shadow flags have two kinds, `runtime_fun` and `class_vars`, of which this ADR removes the second.

### Phase 0: inventory (S)
Produce the list Phases 2 and 3 work from.

- **Fixtures and tests that declare `classState:`**: 90 in `stdlib/test/fixtures`, 18 in `tests/repl-protocol` (15 fixtures, 3 cases), 10 in `test-package-compiler/cases` *(read)*. Sort each into: tests class-variable semantics (delete, with a note of what it pinned), or uses a class variable to reach another scenario (rewrite with an Actor, an `AtomicCounter` or a constant).
- **Benchmarks**: `benchmarks/vs-elixir/beamtalk/src/vb_stateful_math.bt`, `vb_late_state_math.bt`, `vb_bench.bt`, `benchmarks/dispatch-overhead/beamtalk/src/vd_stateful_math.bt`, `runtime/perf/self_send_bench/src/ssb_class_var_class.bt`, `ssb_class_var_threaded_loop_class.bt`, `ssb_on_do_raise_class.bt`, and the harness rows that drive them (`vs_bench.erl`, `vd_bench.erl`). Recorded result files under `benchmarks/vs-elixir/results/` are history and are not edited.
- **Rust and Erlang files that mention class variables**: 145 and 60 *(read)*. Each gets a disposition.
- **Downstream.** Re-run the `classState:` and `performLocally:` searches over the 13 packages and beamtalk-uat immediately before Phase 2.
- **Checks to make.**
  - Whether `__beamtalk_meta/0` already carries a source hash usable in the constant key.
  - Whether a class built with `ClassBuilder` runs every class method in the class process because its methods are runtime funs (Open Question 5).
  - `beamtalk_object_class:local_call/3` has one caller besides `performLocally:`, the migration runner (`beamtalk_shape_migration.erl:306`) *(read)*. Confirm nothing else reaches it through the intrinsic.
  - That the build stamp forces a recompile of dependency BEAMs and workspace-persisted BEAMs (see Phase 3).

### Phase 1: add `constant:` (M)
- **Parser, AST, unparse, formatter.** A `constant:` declaration in `source_analysis/parser/declarations.rs`, beside `classState:`. A `ConstantDeclaration` node (name, optional type, initialiser, doc comment, `@expect`), not a `StateDeclaration` with a new `SlotKind`: a constant is not a slot.
- **Semantic analysis.** The declaration contributes a class-side method to `ClassInfo`. New checks: missing initialiser, duplicate of a class method, self-reference, process-identity type, `late constant:`. The type checker gives the method its return type.
- **Codegen.** The generated class method (three shapes, Decision §4), the `computed_constants` and source-hash entries in `__beamtalk_meta/0` (`class_meta.rs`), and the literal case feeding `constant_class_methods`.
- **Runtime.** `beamtalk_constants.erl`. Erasure of the old module's keys on reload, removal and `renameTo:`, called from the same place that invalidates the shadow flags (`apply_class_info`, `beamtalk_object_class.erl:2190`).
- **Reflection and tooling.** `constantNames` / `allConstantNames`; browse, LSP completion and hover, `beamtalk doc`; `SystemNavigation` references to a constant.
- **Stdlib.** `Actor class >> namedOrSpawn:` if Open Question 3 says yes.
- **Docs.** A "Class constants" section in `docs/beamtalk-language-features.md`; `docs/development/surface-parity.md` for the new reflection sends.
- **Tests.** BUnit: the three initialiser shapes, inheritance and override, a raising initialiser retried, a cycle, a read from an instance method and from another process. REPL protocol: reload changes a literal constant and a computed one. EUnit for `beamtalk_constants`.

### Phase 2: remove `classState:` (L)
One change, with no warning period.

- **The compiler rejects `classState:`, `late classState:` and class-side `self.x`** with the errors in Decision §2, on every surface (build, check, lint, test, REPL compiler port, MCP, LSP).
- **Migrate** the fixtures, tests and benchmarks from the Phase 0 list.
- **Codegen stops emitting** `beamtalk_class_vars` calls: delete `class_var_access.rs`, `class_var_keys.rs`, the `CatchEntry::ClassVarSnapshot` entry and restore in `threaded_ir/` (`emit.rs`, `ir.rs`, `verify.rs`), `VerifyError::CatchWithoutClassVarRestore`, and block capture of class variables in `block_facts`. The PR carries `ci:corpus`. Every module's `on:do:` changes, so `ci:core-diff` shows a wide diff; review a sample.
- **Analysis.** Delete `validators/class_state_abroad.rs`, the class-process half of `class_process_ffi_validators.rs` (the `class-process-ffi` lint), `chain_has_class_variables`, `is_class_var_reflection_selector`, the `late` rules for `classState:` in `late_slot_validators.rs`, and the ADR 0123 migration rule. `runs_class_method_in_caller` loses two of its three conditions.
- **Stdlib.** `ClassBuilder classVars:` / `addClassState:default:`; `classVarNames` / `allClassVarNames`; the `performLocally:` method (`proto_object.bt:115`) if Open Question 4 says delete; the doc comments in Context §1.
- **Harness.** Delete `class_var_program.rs`, `cli_class_var_agreement.rs`, `class_var_agreement.rs`, `just test-class-var-corpus`, the `class-var-corpus` job in `fuzz.yml` and its branch in `fuzz-triage.yml`.
- **Docs.** `docs/beamtalk-language-features.md`: § Class-Side Methods (the run-in-caller rule and the two lints), the `classState:` paragraphs and the singleton example, `late`, class-side `fieldAt:`, § Class variables and blocks, `performLocally:`, the error table. Also `docs/beamtalk-native-erlang.md`, `docs/beamtalk-tooling.md`, `docs/development/erlang-guidelines.md` (the `protect/1` rule), `docs/development/debugging.md`, `docs/agents/expanded.md`, and the "State-threading codegen" and "Blocks into class methods" rules in `AGENTS.md`.
- **ADRs to annotate.** 0013, 0019, 0084, 0109, 0111, 0123, 0124, 0129, 0130, 0133, 0137, 0142.
- **`CHANGELOG.md`**, under Unreleased, a breaking-change entry: `classState:` and class-side `self.x` are removed; use `constant:` for values that never change and an Actor, `AtomicCounter` or `Ets` for state; `performLocally:withArguments:`, `classVarNames`, `ClassBuilder classVars:` and the `class-process-ffi` / `class-state-abroad` lints are removed.

### Phase 3: delete the class-process machinery (L)
After Phase 2 nothing the compiler emits calls `beamtalk_class_vars`.

**Compiled code from before this change.** Every module built by an older compiler calls `beamtalk_class_vars:snapshot/0` before each `on:do:`. Loaded into a runtime without that module, it fails with `undef`. As for ADR 0152, the build stamp is what prevents this (a build is invalidated when `BEAMTALK_VERSION` changes), and Phase 0 confirms it covers dependency and persisted BEAMs. `beamtalk_class_var_abi` already refuses modules built for an older calling convention; its last act is to refuse every module built before this change, and then it is deleted *(inferred: the mechanism exists; this use of it is proposed)*.

| Piece | Where |
|---|---|
| Access, ABI, errors | `beamtalk_class_vars.erl`, `beamtalk_class_var_abi.erl`, `beamtalk_class_var_errors.erl`, `include/beamtalk_class_vars_keys.hrl`, `beamtalk_class_vars_test_support.erl`, and their EUnit suites |
| The class-variable home | The install / read-back / uninstall in `invoke_class_method/7` (`beamtalk_class_dispatch.erl:1635`) and the `class_state` field of `#class_state{}`; `get_class_var` / `set_class_var` (`beamtalk_object_class.erl:316, 1213-1215`) and `beamtalk_runtime_api:set_class_var/3`; the class-side reflection selectors and `class_var_reflection_selectors_conformance.json` |
| The execution-mode input | `set_class_vars/2` and the `class_vars` value of the derived flag in `beamtalk_class_shadow_flags.erl`; `chain_has_class_variables/1` and `put_class_variables/2` in `beamtalk_class_metadata.erl`; the descendant-flag erasure that a class-variable change triggers |
| Catch boundaries | `protect/1` in `beamtalk_result.erl:228, 285` and every other converted catcher; `scripts/ci/lint-class-var-catchers.escript`, its fixtures, the Justfile recipe and `docs/development/class-var-catcher-audit.md` |
| Snapshots | `with_snapshot/2` in `beamtalk_supervisor_class_call.erl:63`, `beamtalk_supervisor_init_config.erl:135` and `local_call/3` (`beamtalk_object_class.erl:474`) *(read)*; the class-state snapshot rows in `beamtalk_class_registry.erl` |
| `performLocally:` | The intrinsic and its tests (`stdlib/test/perform_locally_test.bt`, `tests/repl-protocol/cases/perform_locally.btscript`). `local_call/3` stays as an internal function: the migration runner calls it (`beamtalk_shape_migration.erl:306`) |

**Must pass after each phase:** `just ci`, `just test-corpus`, `just verify-threaded-ir`, `just test-repl-protocol`, `just dialyzer`. A benchmark run before Phase 2 and after Phase 3 (`docs/development/benchmarks.md`) records what the snapshot and the guard input cost: the `on:do:` rows, `Result tryDo:`, and vs-elixir rows 3a-3j.

## Migration Path

There is no deprecation period.

| Code today | After |
|---|---|
| `classState: limit = 100`, read only | `constant: limit = 100`; read with `self limit` |
| `classState: table = ...` built once | `constant: table = ...` |
| `classState: current = nil` holding an instance | A named Actor: `class current => (self named: #name) unwrap` |
| `classState: count = 0` incremented | `AtomicCounter`, or a field of an Actor |
| `classState: cache = #{}` | An `Ets` table owned by an Actor, or an Actor's `state:` |
| `late classState: x :: T` | An Actor with `late state: x :: T` |
| `self.x` in a class method | `self x` |
| `Cls fieldAt: #x` | `Cls x`, or `Cls perform: #x` |
| `Cls performLocally: #sel withArguments: args` | `Cls perform: #sel withArguments: args` |
| `builder classVars: #{#x => 1}` | `builder addClassMethod: #x body: [:cls | 1]` |

Packages must be rebuilt with the new compiler. No downstream package needs a source change *(survey)*.

## Open Questions

1. **The keyword.** `constant:` (proposed) or `classConstant:`. The longer name says "class" and matches `classState:`; the shorter one matches `state:` and `field:`.
2. **Visibility.** Every constant is a public class method. Should `internal constant:` be allowed, as `internal` is on methods? Proposed: yes, in Phase 1, if it is cheap in the parser; otherwise later.
3. **`namedOrSpawn:`.** Add it to `Actor`, under that name? A lazily spawned actor is not supervised, so the docs would present it as the script and REPL form and the supervised name as the production form.
4. **`performLocally:withArguments:`.** Proposed: delete it. With no class variables, `perform:withArguments:` already runs in the caller for every selector that is not process-bound. The risk is a downstream caller the survey did not look for.
5. **How far the class-process execution mode can go.** After this ADR and ADR 0152, a class method still runs in the class process in two cases: a process-bound selector, and a class whose own methods are shadowed by an ADR 0084 runtime fun. If runtime funs can be applied in the caller (ADR 0142 already does this for an *ancestor's* fun), the `runtime_fun` flag is no longer needed for routing, and the per-send guard is left with only "the method is still exported and the class is alive". Whether the 15 ns read that BT-4987 measured can then be dropped or hoisted is a follow-up decision. Proposed: out of scope here; open an issue when Phase 3 lands.
6. **ClassBuilder.** Is `addClassMethod:body:` enough for a run-time class's constants, or is `addConstant:value:` wanted for symmetry?
7. **The process-identity check.** Should an Actor-typed initialiser be an error (proposed) or a warning that `@expect` can silence? An error is safer; a warning allows a constant that holds, say, a `Reference` used as a unique token.
8. **Stale constants after a dependency changes.** A computed constant keeps its value until its own class is reloaded. Is that enough, or should the workspace offer a way to recompute one (`Cls recomputeConstants`)? Proposed: not in this ADR; reloading the class is the way.
9. **Global GC on reload.** Accepted in this design. If a release upgrade that reloads many classes with large computed constants turns out to be a problem, the fallback is to leave old keys in place and erase them in one batch after the upgrade.

## References
- Related issues: BT-5141 (ADR 0152 removal, must land first), BT-4983 (dispatch overhead research), BT-4987 (per-send guard read), BT-4363 / BT-4504 (constant class methods), BT-3701 (ADR 0130 epic), BT-4297 (ADR 0142 epic), BT-3691 (lost write), BT-4148 (class-side `fieldAt:`)
- Related ADRs: [0013](0013-class-variables-class-methods-instantiation.md), [0019](0019-singleton-class-variables.md), [0048](0048-class-side-method-syntax.md) (the `class` modifier, unchanged), [0084](0084-class-side-runtime-method-fun-dispatch.md), [0089](0089-typed-document-leaves.md), [0123](0123-versioned-state-migration.md), [0124](0124-slots-late-assignment-definite-assignment.md), [0129](0129-class-side-system-facades.md), [0130](0130-class-variables-live-in-the-class-process.md), [0135](0135-supervisor-instances-and-start-arguments.md), [0142](0142-class-process-owns-only-class-state.md), [0152](0152-remove-cross-file-extension-methods.md)
- Documentation: `docs/beamtalk-language-features.md` § Class-Side Methods, § Class variables and blocks; `docs/beamtalk-principles.md` §4
