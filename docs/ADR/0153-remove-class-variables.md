# ADR 0153: Remove Class Variables (`classState:`)

## Status
Proposed (2026-11-24)

The owner has decided the following; this ADR records and details it.
- Mutable class variables are removed, with no deprecation period.
- **No constant syntax is added.** A literal constant is a class method that answers a literal. An expensive computed constant uses a small stdlib helper over `persistent_term`.
- `ClassBuilder` stays. Only its class-variable setters go.
- ADR 0123 (versioned state migration) is frozen, not removed. Only its class-variable rule is trimmed.

The points still open are in [Open Questions](#open-questions).

If accepted, this **supersedes [ADR 0130](0130-class-variables-live-in-the-class-process.md)** and **[ADR 0019](0019-singleton-class-variables.md)**, supersedes **§1 of [ADR 0013](0013-class-variables-class-methods-instantiation.md)** (class variables; its class-method and instantiation sections stay), and **amends [ADR 0142](0142-class-process-owns-only-class-state.md)** (rules 1 and 2 of §1 have nothing left to test), **[ADR 0084](0084-class-side-runtime-method-fun-dispatch.md)** (`classVars:`, and where runtime funs run), **[ADR 0123](0123-versioned-state-migration.md)** (the no-class-variable rule for migrations) and **[ADR 0124](0124-slots-late-assignment-definite-assignment.md)** (`late classState:`).

**How to read the evidence.** A claim marked *(read)* was checked by reading the cited code or document on `main` at `f1fcd5e`. *(survey)* means it comes from the owner's usage and cost survey, relayed by the session that commissioned this ADR, and was not re-checked here; the downstream repositories are outside this session's access. *(inferred)* follows from what was read but was not checked directly. Nothing in this ADR was verified by running the system.

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
| `src/` of beamtalk-agents, announcements, buccaneer, catalina, hpack, http, linear, mint, oauth, yaml, symphony, exdura, liquid | 0; every hit is a doc comment *(survey)* |
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
- **Every class send** reads a guard derived from `direct_call_ok/2` (`beamtalk_class_shadow_flags.erl:655`), one of whose inputs is "the class or an ancestor declares class variables" *(read)*. BT-4987 measured that read at 15 ns of a 22-36 ns class send *(read)*. Removing class variables removes an input, not the read (Open Question 4).

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

### What class variables are for, and where each use already lives

| Use in Smalltalk | Home on the BEAM | Evidence |
|---|---|---|
| A singleton or default instance | A named Actor (`spawnAs:`, `named:`) | 21 downstream `src` files *(survey)* |
| Counters and caches | `AtomicCounter`, `Ets`, or an Actor | `stdlib/src/atomic_counter.bt`, `ets.bt` *(read)* |
| A literal constant | A class method that answers the literal | 93 one-line unary class methods in `stdlib/src`, 10 of them a bare literal such as `class supervisionPolicy -> RestartStrategy => #permanent` *(read: `grep`)*; about 260 one-line `class foo =>` methods across stdlib and the packages *(survey)* |
| An expensive computed constant | `persistent_term`, written by hand | beamtalk-hpack does it twice in `src/hpackhuffman.bt`: `class tables` and `class fastTables` call `persistent_term get:`, build on a miss, then `put:` *(survey)* |

Nothing used `classState:` for a constant. The first three rows need nothing new. The fourth is a pattern that has been written by hand twice and deserves to be written once.

### Constraints

- **No image mechanics** (`docs/beamtalk-principles.md` §4). Whatever replaces a class variable must mean the same in the REPL, `beamtalk run`, `beamtalk test` and a release.
- **`ClassBuilder` stays** (owner decision). `fields:`, `methods:`, `classMethods:`, ADR 0084 runtime class-method funs, `class_builder_source.rs` and `register/1` are not touched.
- **ADR 0123 is frozen** (owner decision). Instance-state migration keeps working as it does.
- **`persistent_term`.** Reads are constant time and copy nothing. Replacing or erasing a key whose value is not an immediate term makes the VM scan every process heap for the old term (a global GC).
- **ADR 0152 is in flight.** BT-5141 (started) removes cross-file `>>` and edits `beamtalk_class_dispatch.erl` and `beamtalk_class_shadow_flags.erl`, which this ADR also edits.

## Decision

### 1. The rule

**Shared mutable state lives in an Actor** (or in `AtomicCounter` / `Ets`, which a process owns). A class has no state of its own. `classState:` is removed, and nothing replaces it as syntax.

```text
error: `classState:` was removed (ADR 0153). A class has no state of its own.
  hint: for state that changes, use an Actor (`spawnAs:` / `named:`), an
        AtomicCounter or an Ets table.
        For a constant, write a class method: `class limit => 100`.
        For a value that is expensive to build, see `PersistentTerm`.

error: `self.count` in a class method: a class has no slots.
  hint: `self.x` reads an instance's state. Class methods have none.
```

The second error also covers `self.x := v` in a class method. Both are compile errors on every surface.

### 2. What replaces each use

| Use | Replacement |
|---|---|
| Mutable shared state, singletons | A named Actor |
| Counters | `AtomicCounter` |
| Caches | `Ets`, or an Actor's `state:` |
| Literal constants | A class method |
| Computed constants | `PersistentTerm at:ifAbsentPut:` (new, §3) |

**A singleton is a named Actor**, and `current` asks the runtime each time it is called (principles §4). This replaces the `class current` example in `docs/beamtalk-language-features.md` § Class-Side Methods:

```beamtalk
Actor subclass: Registry
  state: entries = #{}

  class current -> Registry => (self named: #registry) unwrap

  register: key as: value => self.entries := self.entries at: key put: value
  lookup: key => self.entries at: key ifAbsent: [nil]
```

The application's supervisor starts the actor under that name (a `SupervisionSpec` with a `name`), so it is restarted if it dies. In a script or at the REPL, start it once: `Registry spawnAs: #registry`.

No "look up, or spawn if absent" helper is added. The downstream packages already use `named:` and `spawnAs:` directly in 21 files *(survey)*, and a lazily spawned actor is unsupervised, which is the wrong default to make easy. If the need appears, the helper is a few lines on `Actor` (Open Question 2).

**A counter** is an `AtomicCounter`. Create it once with `new:`, at application start, and look it up by name from any process (`stdlib/src/atomic_counter.bt:45, 56`) *(read)*. `new:` is process-bound, so the table behind the counter is created in, and owned by, `AtomicCounter`'s class process (language features, line 1352) *(reviewer, read)*. It lives until `delete` or until the node stops, whichever process asked for it.

```beamtalk
AtomicCounter new: #ordersCreated                 // once, at application start
(AtomicCounter named: #ordersCreated) increment   // anywhere
```

**A literal constant** is a class method. It compiles to a literal in the module's constant pool, and a send that names the defining class answers a scalar one in place, at 1 reduction (BT-4363, BT-4504; `ast/method.rs:literal_constant_body`, which covers scalars only) *(read)*. Any other send (an inherited method, `self class notFound` from an instance, a collection literal) is an ordinary in-caller class send. A subclass inherits the method and may override it.

```beamtalk
Object subclass: HttpStatus
  class notFound -> Integer => 404
  class reasons -> Dictionary(Integer, String) =>
    #{200 => "OK", 404 => "Not Found", 500 => "Internal Server Error"}
```

```beamtalk
>> HttpStatus notFound
=> 404
>> HttpStatus reasons at: 404
=> "Not Found"
```

A dictionary literal `#{...}` or list literal `#(...)` whose elements are literals is emitted as a Core Erlang map or list literal (`crates/beamtalk-codegen/src/core_erlang/expressions.rs:507-570`), which `erlc` folds into one constant-pool term. An array literal `#[...]` is a call to `beamtalk_array:from_list` (`expressions.rs:577-596`) and is rebuilt on every call, so a constant table should be a list or a dictionary *(reviewer, read)*.

### 3. `PersistentTerm`: the helper for computed constants

A new sealed stdlib class, a class-side facade over `persistent_term`, in the same family as `ProcessDictionary` (`stdlib/src/process_dictionary.bt`).

```beamtalk
Object subclass: HpackHuffman
  class tables -> Tuple =>
    PersistentTerm at: #hpack_huffman_tables ifAbsentPut: [self buildTables]
```

```beamtalk
>> PersistentTerm at: #demo_squares ifAbsentPut: [(1 to: 5) collect: [:n | n * n]]
=> #(1, 4, 9, 16, 25)
>> PersistentTerm at: #demo_squares ifAbsentPut: [Error signal: "not run"]
=> #(1, 4, 9, 16, 25)
>> PersistentTerm erase: #demo_squares
=> true
```

| Method | Answers |
|---|---|
| `class at: key ifAbsentPut: block` | The stored value. On a miss it runs `block`, stores the result and answers it |
| `class at: key ifAbsent: block` | The stored value, or the block's value. Stores nothing |
| `class includesKey: key` | Whether a value is stored |
| `class erase: key` | `true` if a value was removed (a Boolean, as `persistent_term:erase/1` answers; `ProcessDictionary erase:` answers the previous value). **Forces a global GC** |

**Why this name and this API.**
- It is named for the BEAM mechanism, like `ProcessDictionary`, because the mechanism's cost model is the thing a user must know. A name such as `Constant` would promise a language feature and hide that an update is expensive.
- `at:ifAbsentPut:` is the Smalltalk dictionary idiom for "build once", and it is the exact shape of the hpack code.
- **There is no `at:put:`.** Replacing a value is the expensive operation, and nothing asked for it. A caller who must replace a value erases it first and sees the cost in the code.

**Rules, stated in the class's doc comment.**
- **Build-once values only.** A read copies nothing. Erasing or replacing a value makes the VM scan every process's heap, and OTP has no batch erase: each `erase:` waits on every scheduler, 0.1-0.3 ms by the runtime's own measurement (`beamtalk_class_shadow_flags.erl:249-251`) *(reviewer, read)*. Use it for a table that is built once and read many times. For anything that changes, use an Actor or `Ets`.
- **The block must be pure.** Two processes that miss at the same moment both run it. The helper reads again before storing and keeps the first value, so the second process normally answers the stored one and does not replace it. A small window remains between that read and the store; inside it, a second store of an equal value is free (OTP skips it), and a second store of a different value replaces the first and costs a global GC. So the block must also be deterministic. A block that spawns an actor or opens a resource would do so twice.
- **If the block raises**, nothing is stored and the exception reaches the caller. The next call runs the block again.
- **Do not store a pid, a port, an actor, a class, an `Ets` handle, or a block.** A process identity outlives its process, and a class reference carries the class process's pid (`beamtalk_class_dispatch.erl:265, 301`) *(reviewer)*. The helper cannot check this statically (a `Dynamic` value passes any type check), so it is a documented rule; whether the native module should refuse such values at run time is Open Question 5. A stored block refers to its module's code and fails once that code has been reloaded twice and purged.
- **Keys.** The helper stores under `{beamtalk_persistent_term, Key}`, so a user key cannot collide with the runtime's own `persistent_term` keys (class flags, static call keys). The wrapper makes every key a tuple; BT-4442 measured a tuple-key read at about 36 ns against 21 ns for an atom key (`beamtalk_class_shadow_flags.erl:410-416`) *(reviewer)*. That is accepted for a helper whose callers are building tables. Among users, keys are global to the node: use a package-prefixed Symbol, as `ProcessDictionary` advises.
- **Each node has its own.** Nothing is replicated.
- **Tests.** The store is node-global, and `TestCase` already says a class that touches `persistent_term` must run serially (`stdlib/src/test_case.bt:80-89`) *(reviewer, read)*. A test that stores under a fixed key overrides `class serial -> Boolean => true` or uses a key unique to the test. An `erase:` in `tearDown` is a global GC per test.

**Reload: nothing is cleared.** A value stays until `erase:` or until the node stops. The helper is not tied to a class, so reloading the class whose method built the value does not rebuild it.
- Tying keys to a class would need the runtime to track which class owns which key and erase on its reload. That is class-level state with reload rules, the thing this ADR removes.
- Erasing on every reload would cost a global GC per reloaded class, including for classes whose tables did not change.
- When the built value's shape changes, change the key (`#hpack_huffman_tables_v2`), which is the usual Erlang practice. During development, `PersistentTerm erase:` from the REPL rebuilds it on the next read.
- **This is a deliberate exception to principles §4.** After the builder method is edited at the REPL the stored value is stale, while `beamtalk run` builds it fresh, so the value can differ between surfaces until the key is changed or erased. It is listed under Consequences.

**Implementation.** `stdlib/src/persistent_term.bt` plus a native module `beamtalk_persistent_term.erl` of a few dozen lines. The class is `sealed typed`, like `ProcessDictionary`. It declares no state and its selectors are not process-bound, so its methods, and the block passed to them, run in the caller. Type signatures follow `ProcessDictionary`'s (`key :: Object`, `block :: Block(Object) -> Object`).

**Follow-up:** move beamtalk-hpack's `class tables` and `class fastTables` to the helper (a change in that repository).

### 4. What is deleted

The file list is in Implementation. In summary:

- The `classState:` declaration, `late classState:`, and class-side `self.x` reads and writes.
- **The class-variable execution mode for class methods.** ADR 0142 §1 rules 1 and 2 have nothing to test. `ClassHierarchy::chain_has_class_variables`, `beamtalk_class_metadata:chain_has_class_variables/1`, the `class_vars` kind of shadow flag, and the install / read-back / uninstall of the class-variable home in `invoke_class_method/7`.
- The class-variable ABI, error module and access module.
- The snapshot emitted before every `on:do:`, the restore in its catch arm, `VerifyError::CatchWithoutClassVarRestore`, block capture of class variables, and `protect/1` in `Result tryDo:` and every other runtime catcher.
- The `class-state-abroad` and `class-process-ffi` lints. `class-method-process-resource` stays and loses its "or declare `classState:`" advice.
- The error kinds `class_state_unreachable`, `class_state_read_only` and `undeclared_class_variable`.
- Class-side reflection over class variables: `classVarNames`, `allClassVarNames` (`stdlib/src/behaviour.bt:271, 279`), and the class-side forms of `fieldAt:`, `fieldAt:put:`, `hasField:` and `clearField:` (BT-4148), with `ClassInfo::is_class_var_reflection_selector` and its conformance fixture.
- `performLocally:withArguments:` (Open Question 3).
- The class-variable fuzz harness, agreement corpus and nightly job.
- `beamtalk_runtime_api:set_class_var/3`.
- **From `ClassBuilder`, only** `classVars:` and `addClassState:default:` (`stdlib/src/class_builder.bt:134, 307`), the `classState` field behind them, and their docs and tests.

### 5. Where a class method runs afterwards

A class method runs in the caller unless its selector is process-bound. That is the whole rule.

**ClassBuilder and ADR 0084 runtime funs keep working, and run in the caller.** Today a class whose own class methods are runtime funs (every class built with `ClassBuilder classMethods:`) runs them in the class process, because a runtime fun shadows the class's compiled methods (language features, line 4120) *(read)*. ADR 0142 already applies an *ancestor's* runtime fun in the caller (`beamtalk_class_dispatch:apply_class_method_body/7`) *(read: the amendment note at the top of ADR 0084)*. Phase 3 extends that to the receiving class's own funs.
- **Nothing in the funs assumes the class process** *(reviewer, read)*. They are stored in the public ETS table `beamtalk_class_method_funs`, which any process reads without calling the class process (`beamtalk_class_metadata.erl:361-371, 1823-1835`), and they are applied as `apply(Fun, [ClassSelf | Args])` with no wrapping (`beamtalk_class_dispatch.erl:1803`).
- **The guard must be split, not relaxed.** Today one verdict, `direct_call_ok/2`, declines whenever `runtime_fun` is set (`beamtalk_class_shadow_flags.erl:642-660`), and the same verdict feeds both the in-caller owner memo and `static_call_target/4` (`beamtalk_class_dispatch.erl:356-360`, `beamtalk_class_shadow_flags.erl:771-790`). `run_in_caller/7` also skips the fun store when the defining class is the receiver (`beamtalk_class_dispatch.erl:528-540`). Phase 3 therefore makes "runs in the caller" require only that the class is ready, keeps "no `runtime_fun`" as an extra condition for installing a static call key, and has `run_in_caller/7` always go through `apply_class_method_body/7`. Relaxing the one gate without this split would bind a static call key to the compiled function while a fun shadows it.
- Installing a fun still goes through the class process, which owns the method table. Only execution moves.
- A `classMethods:` block that used class variables (`[:self | self.total := self.total + 1]`, the example at `class_builder.bt:103`) has no class variable to use. Its doc example is rewritten.
- The `runtime_fun` shadow flag stays. A static call site still needs it, to know that a fun and not the compiled function answers the selector.
- Must keep passing: the ClassBuilder suites (`stdlib/test/class_builder_*`), including `class_builder_class_method_source_test.bt`, and the ADR 0084 runtime-fun tests.

**The process-bound selectors keep their routing**, for reasons that have nothing to do with class variables *(read: the doc comment on `PROCESS_BOUND_CLASS_SELECTORS`, `class_info.rs:507-539`)*:

| Selectors | Why they are process-bound |
|---|---|
| `new`, `new:`, `startLink`, `startLink:` | `basicNew` / `basicNewWith` read the class name and module from the class process's dictionary. The in-caller constructor shapes of BT-4099, BT-4464 and BT-4475 are unchanged, and their "no class variables in the chain" condition becomes always true |
| `spawn`, `spawnWith:` | Dedicated clauses in `beamtalk_class_dispatch:class_send_plain/3` |
| `supervise`, `spawnAs:`, `spawnWith:as:` | Supervisor routing, already started in the caller (ADR 0135 §1) |
| `runAll`, `runAll:`, `run:`, `run:method:` | The `TestCase` run selectors answer `test_spawn` and need the gen_server's `noreply` |

The class process itself stays, as the owner of the class object: registration, metadata, reload, the method tables. Whether the process-bound selectors can leave it is a separate question, not decided here.

### 6. Other ADRs

| ADR | Effect |
|---|---|
| 0013 | §1 (class variables) superseded. Class methods and instantiation unchanged |
| 0019 | Superseded. Singleton access is a named Actor. The stdlib stopped using class-variable singletons with ADR 0129 *(inferred: `stdlib/src` declares none)* |
| 0048 | None. The `class` modifier stays |
| 0084 | `classVars:` removed. Runtime class-method funs stay and run in the caller (§5) |
| 0123 | **Frozen; the class-variable parts trimmed.** Also withdrawn, because they only existed for the class process: §2's `Base performLocally: #migrateFromV1: withArguments:` idiom and its caveat that an ancestor step is serialised through `Base`'s process where class variables are visible (`0123-versioned-state-migration.md:304-318`), the recorded refinement of lowering `migrateFromVN:` sends to `local_call/3`, and the rationale about `local_call/3` passing empty class variables (`:405-417`) *(reviewer, read)*. An ancestor step becomes an ordinary in-caller class send. The compile error "a `migrateFromVN:` method may not access class variables" and its validator are removed, because class-side `self.x` is now an error everywhere and the special case has nothing to add. The `with_snapshot/2` wrapper inside `beamtalk_object_class:local_call/3` is removed; `local_call/3` itself stays, because the migration runner calls it (`beamtalk_shape_migration.erl:306`) *(read)*. `shapeVersion:`, `migrateFromVN:` and instance-state migration are unchanged |
| 0124 | `late classState:` removed. `late state:` unchanged. The five compile-time errors in the language docs lose their `classState:` halves |
| 0130 | Superseded in full |
| 0142 | §1 rules 1 and 2, the chain-wide amendment and the descendant-flag erasure for class-variable changes have nothing to act on. Rule 3 (process-bound selectors), the guarded static call and the static call keys stay |
| 0152 | Sequenced first (Implementation). After both, the shadow flag has one kind, `runtime_fun` |

### Single source of truth

- "Where does a class method run?" stays one rule, `runs_class_method_in_caller`, with one input left (the process-bound list, already generated into metadata from Rust).
- The `{beamtalk_persistent_term, Key}` wrapper is built in one place, `beamtalk_persistent_term.erl`. Nothing in Rust knows it.
- The generated header `beamtalk_class_vars_keys.hrl`, `class_var_keys.rs` and `class_var_reflection_selectors_conformance.json` are deleted; the rules they kept in step no longer exist.

## Prior Art

Summarised from the languages' documentation; not re-checked for this ADR.

| Language | What it does | What we take |
|---|---|---|
| **Pharo / Squeak** | Class variables (shared with subclasses) and class-side instance variables (per class), both mutable shared memory. Constants are usually class-side methods that answer a literal | The class-side method as the constant. We drop the variables |
| **Newspeak** | No global namespace and no static state. A class has no mutable class-side state; shared state belongs to a module instance | The closest precedent for removing class variables from a Smalltalk-family language |
| **Erlang** | No module-level mutable state. Constants are macros or functions that answer a literal. `persistent_term` (OTP 21.2) holds values read often and written rarely, with a global GC on update | Both replacements for constants, unchanged |
| **Elixir** | Module attributes are compile-time constants. State is a process (`Agent`, `GenServer`), ETS or `:persistent_term` | "Constants in the module, state in a process" is what BEAM developers expect |
| **Gleam** | `const` holds compile-time literals only | Literal constants need no run-time support |
| **Scala, Kotlin, Swift** | `lazy val`, `by lazy`, `static let`: computed once on first use; Scala and Kotlin evaluate again after an exception | `at:ifAbsentPut:` has the same first-use and retry behaviour |
| **Pony, Dart** | Pony has no global mutable state; constants are functions on `primitive` types. Dart statics are per isolate, never shared | Actor languages do not share mutable statics |

## User Impact

- **Newcomer.** One fewer concept. They no longer need to know what a class process is before writing a class method. The cost: a newcomer who wants "one shared counter" must meet `AtomicCounter` or an Actor first, so the tutorial needs that example early. The error for `classState:` names the replacements.
- **Smalltalk developer.** Loses class variables and class-side instance variables, a departure every Smalltalk developer will notice. Keeps the Pharo idiom for constants (a class-side method) and `ClassBuilder`. `at:ifAbsentPut:` reads as Smalltalk.
- **Erlang / Elixir developer.** Gets what they expect: no module-level mutable state, constants as functions that answer literals, `persistent_term` under its own name with its own caveat, and class methods that are plain functions in the caller.
- **Operator.** No class process serialising calls for any user class. `PersistentTerm` values are visible with `persistent_term:get()` under `{beamtalk_persistent_term, Key}`. `erase:` is a global GC and should not be called on a hot path.
- **Tooling developer.** The class-variable capture analysis, two lints and the class-side slot paths in completion, references and `SystemNavigation` go away. Nothing new to model: the replacements are ordinary class methods and an ordinary class.

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

### Immutable class-constant syntax (`constant: name = expr`)

| Cohort | Strongest argument |
|---|---|
| Newcomer | "`constant: limit = 100` says what it is. A method that happens to answer a literal does not." |
| Smalltalk purist | "A declaration the browser can list is better than a naming convention." |
| BEAM veteran | "The compiler could pick the storage for me and clear it on reload, which the helper does not." |
| Operator | "Constants tied to a class are rebuilt when the class is reloaded. Hand-managed keys go stale." |
| Language designer | "Intent in the declaration lets the compiler reject an Actor in a constant." |

### ETS-backed `classState:`

| Cohort | Strongest argument |
|---|---|
| Newcomer | "Read and write from anywhere, no process rules, no new error." |
| BEAM veteran | "Concurrent reads, no gen_server bottleneck. ADR 0013 listed it as the scaling path." |
| Operator | "Visible in observer." |

### Tension points

- The constant-syntax steelman is the strongest. Its real advantage is reload: a declared constant can be rebuilt when its class reloads, and a `PersistentTerm` key cannot. The decision accepts stale keys on reload in exchange for adding no syntax, no reload machinery and no global GC per reload.
- The Smalltalk purist loses something in every option except the first. The decision accepts that, as ADR 0152 did for open classes.
- The BEAM veteran's argument for the status quo ("it is paid for") is true of the build cost and false of the running cost: the snapshot, the guard input and the nightly job are paid on every `on:do:`, every send and every night.

## Alternatives Considered

### Keep `classState:` as it is
Rejected. Nothing uses it (Context §1), and code that does not use it pays for it on `on:do:`, on `Result tryDo:` and on every class send (Context §3). "Classes are objects with state" is sound in an image; on the BEAM the state is in a process either way, and an Actor says so.

### Keep mutable `classState:`, backed by an Actor transparently
The compiler would generate a hidden actor per class and turn `self.x := v` into a message to it.

```beamtalk
Object subclass: Tally
  classState: n = 0
  class bump => self.n := self.n + 1   // would compile to: read from the hidden actor, add, write back
```

Rejected.
- It is what ADR 0130 already is, with the class process as the hidden actor, and it brought all the rules listed under "The conceptual argument".
- Each read becomes a call to another process (microseconds), or else the method must run inside the actor, which is the execution-mode split this ADR removes.
- A read-modify-write across two messages is a race the syntax hides. A user who writes an Actor sees the message boundary.

### Immutable class-constant syntax
The first draft of this ADR designed it: `constant: name [:: Type] = expr` lowering to a class-side method of the same name, a literal initialiser compiled in, a computed one evaluated on first read and kept in `persistent_term` under a key that carries a hash of the class source, erased when the class reloads, inherited and overridable like any class method.

```beamtalk
Object subclass: HttpStatus
  constant: notFound = 404
  constant: tokenPattern :: Regex = (Regex from: "[A-Za-z0-9_]+") unwrap
```

Rejected by the owner as not needed yet (YAGNI).
- A class method already covers the literal case, at the same cost, and is the established idiom.
- `PersistentTerm at:ifAbsentPut:` covers the computed case in one line.
- Nothing used `classState:` for a constant, so there is no demand to measure the syntax against.
- It would add a parser form, a declaration node, type and reflection rules, and a reload path with a global GC, to save one line per constant.

If stale `PersistentTerm` keys on reload become a real problem, this is the design to return to.

### ETS-backed `classState:`
A per-class table readable and writable from any process: `self.n := self.n + 1` becomes `ets:lookup` then `ets:insert`. Rejected, for ADR 0130's reasons and one more.
- A write is visible at once to every process, so a class method that fails half way leaves torn state, and two callers race on read-modify-write.
- It makes class methods concurrent over shared mutable state with no owner, the opposite of the rule in Decision §1.
- Every read copies the value. `Ets` already exists for users who want exactly this, with the table visible in their code.

### A helper tied to the class (`self once: #tables do: [...]`)
The helper could key on the calling class and have the runtime erase a class's keys when it reloads. Rejected: see Decision §3, "Reload". It rebuilds the per-class bookkeeping this ADR deletes.

### No helper; call `persistent_term` through FFI
`(Erlang persistent_term) get: key with: default` works today, and hpack uses it. Rejected as the documented answer: the miss-build-store sequence has a race and a caveat that each author would have to rediscover, and raw keys can collide with the runtime's own.

### Deprecate for a release, then remove
Rejected by the owner, as for ADR 0152: with no user of the feature there is nobody for a warning to reach.

## Consequences

### Positive
- One rule for shared mutable state: an Actor (or a process-owned counter or table).
- One execution mode for ordinary class methods, ClassBuilder classes included. A class method is a function in the caller.
- `on:do:` and `Result tryDo:` lose the snapshot and restore. The catcher audit and its lint go away.
- About 3,000 lines of dedicated source, about 7,500 lines of dedicated tests and harness *(read: the `wc -l` figures in Context)*, a nightly job and three error kinds are deleted.
- No new syntax. The language gets smaller.
- The hand-rolled `persistent_term` pattern is written once, with its race handled and its caveat documented.

### Negative
- A breaking change with no deprecation period. Any private code that declares `classState:` stops compiling.
- A departure from Smalltalk that every Smalltalk developer will notice.
- The singleton pattern takes more lines: an Actor, a name and a supervisor entry.
- **A `PersistentTerm` value is not rebuilt when the code that built it is reloaded.** The author must change the key or erase it. A stale table after a hot upgrade is possible, and nothing warns about it.
- `PersistentTerm` is a node-global namespace with no owner. Two packages that pick the same key share a value. The package-prefix convention is advice, not a check.
- `PersistentTerm erase:` is a global GC that any code can trigger.
- A literal constant written as a class method is not marked as a constant. Tools list it with the other class methods.
- About 120 test fixtures and their snapshots are rewritten or deleted.

### Neutral
- The class process stays, for the class object and the process-bound selectors.
- The per-send guard read stays (Open Question 4).
- An `Ets` table is owned by the process that creates it, so replacing a class-variable cache means deciding who owns it. That decision was hidden before; it is not new work. An `AtomicCounter` is owned by its class process and lives until it is deleted.
- `ClassBuilder` keeps its API apart from two setters.

## Implementation

Sizes are rough (S, M, L). Phases 0 and 1 do not depend on BT-5141. **Phases 2 and 3 land after BT-5141 (ADR 0152) merges**: both edit `beamtalk_class_dispatch.erl` and `beamtalk_class_shadow_flags.erl`, and after ADR 0152 the shadow flags have two kinds, `runtime_fun` and `class_vars`, of which this ADR removes the second.

### Phase 0: inventory (S)
Produce the list Phases 2 and 3 work from.

- **Fixtures and tests that declare `classState:`**: 90 in `stdlib/test/fixtures`, 18 in `tests/repl-protocol` (15 fixtures, 3 cases), 10 in `test-package-compiler/cases` *(read)*. Sort each into: tests class-variable semantics (delete, with a note of what it pinned), or uses a class variable to reach another scenario (rewrite with an Actor, an `AtomicCounter` or a class method).
- **Benchmarks**: `benchmarks/vs-elixir/beamtalk/src/vb_stateful_math.bt`, `vb_late_state_math.bt`, `vb_bench.bt`, `benchmarks/dispatch-overhead/beamtalk/src/vd_stateful_math.bt`, `runtime/perf/self_send_bench/src/ssb_class_var_class.bt`, `ssb_class_var_threaded_loop_class.bt`, `ssb_on_do_raise_class.bt`, and the harness rows that drive them (`vs_bench.erl`, `vd_bench.erl`). Recorded result files under `benchmarks/vs-elixir/results/` are history and are not edited.
- **Rust and Erlang files that mention class variables**: 145 and 60 *(read)*. Each gets a disposition.
- **ClassBuilder.** List the tests and docs that use `classVars:`, `addClassState:default:` or a class variable inside a `classMethods:` block.
- **Rewrite guidance.** `AtomicCounter` and named `Ets` tables are node-global names, so a fixture rewritten to use one needs a name unique to its test, or parallel test runs collide. An `Ets` table created inside a class method belongs to the calling process and dies with it; create it in the test's `setUp` or in an Actor. An `AtomicCounter` belongs to its class process and must be deleted in `tearDown` *(reviewer)*.
- **Downstream.** Re-run the `classState:`, `performLocally:`, `classVarNames` and `classVars:` searches over the 13 packages and beamtalk-uat immediately before Phase 2.
- **Checks to make.**
  - That a literal dictionary, list or tuple in a method body compiles to one constant-pool term (Decision §2).
  - That nothing in the runtime or the workspace still sets a class variable through `set_class_var/3` *(read: no caller in `runtime/apps/*/src` outside its own definition and `beamtalk_runtime_api`)*.
  - That the build stamp forces a recompile of dependency BEAMs and workspace-persisted BEAMs (see Phase 3).

### Phase 1: add `PersistentTerm` (S)
- `stdlib/src/persistent_term.bt` and `runtime/apps/beamtalk_stdlib/src/beamtalk_persistent_term.erl`, with the doc comment of Decision §3.
- BUnit: build once; the block is not run on a hit; a raising block stores nothing and is retried; `erase:`; `includesKey:`; a stored `nil` counts as present; two processes missing together agree on one value. EUnit for the native module.
- Docs: a short section in `docs/beamtalk-language-features.md`, and the stdlib reference.
- Follow-up issue: move beamtalk-hpack's two tables to the helper.

### Phase 2: remove `classState:` (L)
One change, with no warning period.

- **The compiler rejects `classState:`, `late classState:` and class-side `self.x`** with the errors in Decision §1, on every surface (build, check, lint, test, REPL compiler port, MCP, LSP). The parity projects must report them identically.
- **Migrate** the fixtures, tests and benchmarks from the Phase 0 list.
- **Codegen stops emitting** `beamtalk_class_vars` calls: delete `class_var_access.rs`, `class_var_keys.rs`, the `CatchEntry::ClassVarSnapshot` entry and restore in `threaded_ir/` (`emit.rs`, `ir.rs`, `verify.rs`), `VerifyError::CatchWithoutClassVarRestore`, and block capture of class variables in `block_facts`. The PR carries `ci:corpus`. Every module's `on:do:` changes, so `ci:core-diff` shows a wide diff; review a sample.
- **Analysis.** Delete `validators/class_state_abroad.rs`, the class-process half of `class_process_ffi_validators.rs` (the `class-process-ffi` lint), `chain_has_class_variables`, `is_class_var_reflection_selector`, the `classState:` rules in `late_slot_validators.rs`, the class-variable reflection check in `reserved_name_validators.rs`, and the ADR 0123 migration rule. `runs_class_method_in_caller` loses two of its three conditions. The `class_variables` field of `ClassInfo` and its carriers (`class_meta.rs`, `build_stdlib/metadata.rs`, `beamtalk-compiler-port/src/decode.rs`, the language service's `identifier_lookup.rs` and `references_provider.rs`) go with it.
- **Parser, AST, unparse.** The `classState:` arm in `source_analysis/parser/declarations.rs` stays only to produce the error; the class-variable list on `ClassDefinition` is removed.
- **Stdlib.** `ClassBuilder classVars:` / `addClassState:default:` and the `classState` field; `classVarNames` / `allClassVarNames`; the `performLocally:` method (`proto_object.bt:115`) if Open Question 3 says delete; the class-side slot branches in `system_navigation.bt` (around lines 1116-1219); the doc comments listed in Context §1.
- **Harness.** Delete `class_var_program.rs`, `cli_class_var_agreement.rs`, `class_var_agreement.rs`, the `class_var_*` codegen test modules, `just test-class-var-corpus`, the `class-var-corpus` job in `fuzz.yml` and its branch in `fuzz-triage.yml`.
- **Docs.** `docs/beamtalk-language-features.md`: § Class-Side Methods (the run-in-caller rule and the two lints), the `classState:` paragraphs and the singleton example, `late`, class-side `fieldAt:`, § Class variables and blocks, `performLocally:`, the error table. Also `docs/beamtalk-native-erlang.md`, `docs/beamtalk-tooling.md`, `docs/development/erlang-guidelines.md` (the `protect/1` rule), `docs/development/debugging.md`, `docs/development/surface-parity.md`, `docs/agents/expanded.md`, and the "State-threading codegen", "Opt-in CI labels" and "Blocks into class methods" rules in `AGENTS.md`.
- **ADRs to annotate.** 0013, 0019, 0084, 0109, 0111, 0123, 0124, 0127 (its rule that a trait body may not declare `classState:` becomes moot), 0129, 0130, 0133, 0137, 0142.
- **`CHANGELOG.md`**, under Unreleased, a breaking-change entry: `classState:` and class-side `self.x` are removed; use an Actor, `AtomicCounter` or `Ets` for state, a class method for a constant, and `PersistentTerm` for a value that is expensive to build; `performLocally:withArguments:`, `classVarNames`, `ClassBuilder classVars:` and the `class-process-ffi` / `class-state-abroad` lints are removed.

### Phase 3: delete the class-process machinery (L)
After Phase 2 nothing the compiler emits calls `beamtalk_class_vars`.

**Compiled code from before this change.** Every module built by an older compiler calls `beamtalk_class_vars:snapshot/0` before each `on:do:`. Loaded into a runtime without that module, it fails with `undef`. As for ADR 0152, the build stamp is what prevents this (a build is invalidated when `BEAMTALK_VERSION` changes), and Phase 0 confirms it covers dependency and persisted BEAMs. `beamtalk_class_var_abi` already refuses modules built for an older calling convention; its check moves to whatever module-load check remains, so that a module built before this change is refused with a clear message and not an `undef` *(inferred: the mechanism exists; this use of it is proposed)*.

| Piece | Where |
|---|---|
| Access, ABI, errors | `beamtalk_class_vars.erl`, `beamtalk_class_var_abi.erl`, `beamtalk_class_var_errors.erl`, `include/beamtalk_class_vars_keys.hrl`, `beamtalk_class_vars_test_support.erl`, and their EUnit suites |
| The class-variable home | The install / read-back / uninstall in `invoke_class_method/7` (`beamtalk_class_dispatch.erl:1635`) and the `class_state` field of `#class_state{}`; `get_class_var` / `set_class_var` (`beamtalk_object_class.erl:316, 1213-1215`) and `beamtalk_runtime_api:set_class_var/3`; the class-side reflection selectors and `class_var_reflection_selectors_conformance.json`; `register/1`'s read of the builder's `classState` key (`beamtalk_class_builder.erl:429-430`; the rest of `register/1` is untouched) |
| The execution-mode input | `set_class_vars/2` and the `class_vars` value of the derived flag in `beamtalk_class_shadow_flags.erl`; `chain_has_class_variables/1` and `put_class_variables/2` in `beamtalk_class_metadata.erl`; the descendant-flag erasure that a class-variable change triggers |
| Runtime funs in the caller | Split the guard and route `run_in_caller/7` through `apply_class_method_body/7` (Decision §5). Tests: a ClassBuilder class whose fun shadows nothing, one whose fun shadows a compiled method reached by a static send, and one installed while a caller loops |
| Catch boundaries | `protect/1` in `beamtalk_result.erl:228, 285`, `beamtalk_supervisor.erl:391`, `beamtalk_telemetry.erl:317`, `beamtalk_test_case.erl:76`, `beamtalk_test_assert.erl:48` and `beamtalk_repl_eval.erl:842`, and the snapshot / restore pair in `beamtalk_erlang_proxy.erl:436, 511-520` *(reviewer; Phase 0 confirms the list with the catcher lint before it is deleted)*; `scripts/ci/lint-class-var-catchers.escript`, its fixtures, the Justfile recipe and `docs/development/class-var-catcher-audit.md` |
| Snapshots | `with_snapshot/2` in `beamtalk_supervisor_class_call.erl:63`, `beamtalk_supervisor_init_config.erl:135` and `local_call/3` (`beamtalk_object_class.erl:474`) *(read)*; the class-state snapshot rows in `beamtalk_class_registry.erl` |
| `performLocally:` | The intrinsic and its tests (`stdlib/test/perform_locally_test.bt`, `tests/repl-protocol/cases/perform_locally.btscript`). `local_call/3` stays as an internal function for the migration runner |

**Must pass after each phase:** `just ci`, `just test-corpus`, `just verify-threaded-ir`, `just test-repl-protocol`, `just dialyzer`. A benchmark run before Phase 2 and after Phase 3 (`docs/development/benchmarks.md`) records what the snapshot and the guard input cost: the `on:do:` rows, `Result tryDo:`, and vs-elixir rows 3a-3j.

## Migration Path

There is no deprecation period.

| Code today | After |
|---|---|
| `classState: limit = 100`, read only | `class limit => 100`; read with `self limit` |
| `classState: table = ...`, built once | `class table => PersistentTerm at: #pkg_table ifAbsentPut: [...]` |
| `classState: current = nil` holding an instance | A named Actor: `class current => (self named: #name) unwrap` |
| `classState: count = 0`, incremented | `AtomicCounter`, or a field of an Actor |
| `classState: cache = #{}` | An `Ets` table owned by an Actor, or an Actor's `state:` |
| `late classState: x :: T` | An Actor with `late state: x :: T` |
| `Cls fieldAt: #x` | `Cls x` |
| `Cls performLocally: #sel withArguments: args` | `Cls perform: #sel withArguments: args` |
| `builder classVars: #{#x => 1}` | `builder addClassMethod: #x body: [:self | 1]` |

Packages must be rebuilt with the new compiler. No downstream package needs a source change *(survey)*.

## Open Questions

1. **The helper's name and surface.** `PersistentTerm` with `at:ifAbsentPut:`, `at:ifAbsent:`, `includesKey:` and `erase:` is proposed. The owner's sketch was `Constant named: #key ifAbsent: [...]`. Is the mechanism name right, and is leaving out `at:put:` right?
2. **A "named, or spawn" helper on `Actor`.** Proposed: not now. Add `namedOrSpawn:` only when a package needs it.
3. **`performLocally:withArguments:`.** Proposed: delete it. With no class variables, `perform:withArguments:` runs in the caller for every selector that is not process-bound. The risk is a downstream caller; Phase 0 searches for one.
4. **The per-send guard.** After this ADR and ADR 0152, the guard's remaining reasons are a runtime fun shadowing a compiled method, a method removed by a reload or `removeSelector:`, and the class process being alive. Whether the 15 ns read BT-4987 measured can then be hoisted or dropped is a follow-up. Proposed: out of scope here; open an issue when Phase 3 lands.
5. **Checks in `PersistentTerm`.** Is the package-prefix key convention enough, or should the helper require a `{Package, Name}` key? Should the native module refuse a top-level pid, port, actor or class as a value? Proposed: convention only for keys, as for `ProcessDictionary`; refuse the top-level process identities, since the check is one guard and runs only on a miss.
6. **A lint for the hand-rolled pattern.** Should a direct `(Erlang persistent_term) put:` in package code get a hint pointing at the helper? Proposed: no; a doc note is enough.
7. **Class-side `fieldAt:` after removal.** `SomeClass fieldAt: #x` should answer what any object with no such field answers. Phase 2 must pin which error that is.

## References
- Related issues: BT-5141 (ADR 0152 removal, must land first), BT-4983 (dispatch overhead research), BT-4987 (per-send guard read), BT-4363 / BT-4504 (constant class methods), BT-3701 (ADR 0130 epic), BT-4297 (ADR 0142 epic), BT-3691 (lost write), BT-4148 (class-side `fieldAt:`)
- Related ADRs: [0013](0013-class-variables-class-methods-instantiation.md), [0019](0019-singleton-class-variables.md), [0048](0048-class-side-method-syntax.md), [0084](0084-class-side-runtime-method-fun-dispatch.md), [0123](0123-versioned-state-migration.md), [0124](0124-slots-late-assignment-definite-assignment.md), [0129](0129-class-side-system-facades.md), [0130](0130-class-variables-live-in-the-class-process.md), [0135](0135-supervisor-instances-and-start-arguments.md), [0142](0142-class-process-owns-only-class-state.md), [0152](0152-remove-cross-file-extension-methods.md)
- Documentation: `docs/beamtalk-language-features.md` § Class-Side Methods, § Class variables and blocks; `docs/beamtalk-principles.md` §4; `stdlib/src/process_dictionary.bt` (the model for `PersistentTerm`)
- External: [`persistent_term` (Erlang/OTP)](https://www.erlang.org/doc/apps/erts/persistent_term.html)
