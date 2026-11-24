# ADR 0152: Remove Cross-File Extension Methods

## Status
Accepted (2026-11-24)

This **supersedes the cross-file part of [ADR 0066](0066-open-class-extension-methods.md)**: a `Class >> selector => ...` definition in a file that does not define `Class`. Superseded: §2 (the `Class+Feature.bt` file convention), §3 (ETS dispatch with interleaved lookup), §4 (compile-time conflict detection and type metadata), §6 (load order), §7 (ETS table lifecycle), and the BT-5002, BT-5060 and BT-5061 lookup-order amendments. Kept: the `>>` syntax (§1) for same-file methods and for live method definition, and the sealed-class policy (§5) as far as those two forms reach.

The first draft of this ADR proposed replacing cross-file `>>` with retroactive protocol implementations (`Proto implementFor: Class`). The owner decided to remove first and defer the replacement until a concrete need appears, and to remove without a deprecation period, because no package uses the feature. That design is kept under [Future direction](#future-direction-retroactive-protocol-implementations).

**How to read the evidence.** A claim marked *(read)* was checked by reading the cited code on `main` at `9dcf825`. A claim marked *(inferred)* follows from that reading but was not checked directly. A claim marked *(reviewer)* was reported by an adversarial review pass and spot-checked, not read in full. Nothing in this ADR was verified by running the system.

## Context

### Problem statement

Beamtalk has one way to add a method to a class from outside the class's file: the cross-file extension method of ADR 0066.

```beamtalk
// In helpers.bt. String is defined in stdlib, not in this file.
String >> shout => self uppercase ++ "!"
```

The compiler registers the method in a runtime table when the declaring module loads, and every lookup consults that table at each level of the superclass walk. The mechanism has three problems.

**1. It is almost unused.** A count of top-level `Class >> selector =>` definitions whose target class is not defined in the same file:

| Where | Cross-file extensions |
|---|---|
| `stdlib/src` | 0 *(read)* |
| `examples/` | 0 *(read)* |
| buccaneer, catalina, http, oauth, mint, hpack, yaml, linear, agents, announcements (about 400 `.bt` files) | 0 (reported by the owner's survey; not re-checked here) |
| beamtalk-symphony (48 `.bt` files), beamtalk-exdura (92) | 0. Every `>>` is a `Class>>selector` reference in a comment (checked by the parent session, and by `gh api search/code` here) |
| beamtalk-watcher, beamtalk-collections | 0. Same comment-only pattern in the search fragments returned (code search shows a few fragments per file, so this is strong but not exhaustive) |
| beamtalk-liquid (32 `.bt` files), beamtalk-registry | 0 hits for `>>` in `.bt` files (liquid also checked by the parent session). With liquid, every dependency of symphony and exdura is covered |
| beamtalk-uat | 1 file: a UAT fixture with `SomeActor >> bumpBy:` and `SomeActor >> tallyPlusTen`, written to exercise the feature (BT-2452) |
| `stdlib/test` | 11 files, 16 target classes *(read)* |
| `tests/parity` | 3 files *(read)* |
| `tests/repl-protocol/cases` | 5 `.btscript` files that target a class defined elsewhere *(read)* |
| `fuzz/corpus` | 0 definitions. The one `>>` file uses the `Counter >> #sel` reflection form *(read)* |

Every use is a test of the feature itself. No package uses it to do work.

**2. It is expensive to keep.**

- `runtime/apps/beamtalk_runtime/src/beamtalk_extensions.erl` is 913 lines and owns three ETS tables *(read)*.
- 59 of 303 non-test Erlang source files mention "extension". About 52 of them mean open-class extensions, and 22 name `beamtalk_extensions` *(read)*.
- 250 of 1,130 Rust files mention "extension" (166 outside tests). 111 touch the standalone-method or extension-index types *(read)*.
- Every lookup pays for it. Compiled `has_method/1` checks `beamtalk_extensions:has/2` at each class. The actor and value default dispatch arms call `beamtalk_extensions:lookup/2` (`crates/beamtalk-codegen/src/core_erlang/gen_server/dispatch.rs:769-786`, `core_erlang/dispatch_spec.rs:305-360`). The inherited-send path carries an `{ext, Owner}` verdict (`inherited_target/2`, `beamtalk_dispatch.erl:279`). BT-4260 and BT-4280 measured this on hot paths; BT-4800 removed double work on the class-side path.

**3. Its lookup order is a steady source of bugs.** The order is: the receiver's own compiled method, then at each ancestor that ancestor's extension before its compiled method. The class side differs: an extension beats the class's own compiled method (BT-5061). A Linear search for "extension" over the last 90 days returns 55 issues. A handful are about the VS Code extension; most of the rest are lookup-order or precedence bugs:

| Issue | What went wrong |
|---|---|
| BT-5002 | An extension on `Actor` was ignored more than one level below `Actor` |
| BT-5060 | An extension on a Value class is ignored by a direct subclass for a selector the class compiles (the "one-hop gap"; assessed and left as a warning) |
| BT-5061 | Class-side extension beats the class's own compiled class method, the opposite of the instance side |
| BT-5062 | `removeSelector:` on an owner that also has an extension |
| BT-4307 | Class-side extensions on an ancestor not reached through a subclass |
| BT-4588 | `new` / `new:` ignored an ancestor's class-side extension |
| BT-4250 | Class-side `super` consulted the wrong class's extension |
| BT-4133 | Class-side extensions bypassed by direct-called methods |
| BT-3951 | A runtime extension overriding a known collection selector defeated a codegen guard |

Tooling carried the same cost: the WS3 epic (BT-4610, with BT-4608 and BT-4611) made dependency extensions visible to consumers, and BT-4658 and BT-4923 found validators that skipped extension bodies.

Not every extension bug is a precedence bug. BT-4503 (a value-type extension with `^` in a block answered `{Result, Self}`) and BT-3920 were bugs in how extension *bodies* are compiled, and this ADR keeps that body codegen (see Consequences).

The precedence bugs share one root. **An extension can take precedence over a compiled method**, so every dispatch path, static call, cache and tool must ask "is there an extension that wins here?" The BT-5060 work that merged as #5718 is the latest instance: it adds warning plumbing through eval, load and sync replies to tell the user about a case the dispatch order cannot honour.

### What `>>` is actually used for

`>>` has three uses, and only one of them needs the extension table.

- **Same-file methods.** `Pt >> extra: n => ...` next to `Value subclass: Pt` is folded into the class before analysis (BT-4721). `crates/beamtalk-core/src/compilation/same_file_fold.rs:15-17` says: "nothing registers a same-file extension through `beamtalk_extensions`" *(read)*.
- **Live method definition.** `Foo >> sel => body` at the REPL goes `beamtalk_repl_eval:handle_method_definition` → `beamtalk_repl_loader:reload_method_definition/4` → `reload_class_method_definition` (`beamtalk_repl_loader.erl:238-281`). That reads the class's stored source, merges the method in (`crates/beamtalk-compiler-port/src/handlers/compile_method.rs:243-252`), recompiles the whole class and hot-loads it *(read)*. `Foo class >> sel` takes the same path with `is_class_method = true`. The IDE and MCP `save_method` evaluate `Class compile: #sel source: "..."`, which reaches the same `install_method` recompile path (`beamtalk_repl_loader_methods.erl:85-171`) *(read)*. No workspace source file calls `beamtalk_extensions:register` *(read)*.
- **Cross-file extensions.** Only these use the table. Codegen emits a `beamtalk_extensions:register/5` call in the declaring module's `register_class/0` (`crates/beamtalk-codegen/src/core_erlang/gen_server/extensions.rs:116-143`) *(read)*.

So the first two uses already work without the extension table.

### What is lost

Two things, and the decision has to own both.

- **Adding a method to a class you cannot edit.** After removal there is no way to make `String` or another package's `Money` answer a new selector. The one need seen so far is `asJson`: `Json generate:` calls `asJson` on any value that responds to it (`beamtalk_json.erl:344-350`), and BT-2819's `JsonMappable` (#5717, #5719) must be declared with `uses:` in the class's own body. No package has used a cross-file extension to meet that need, or any other.
- **Patching a class method of a class the workspace has no source for.** BT-5061 kept the class-side "extension first" order for this. It is discussed under Decision §4 and is Open Question 1.

### Constraints

- Same-file `>>` and live method definition must keep working exactly as they do.
- A compiled method must be the only thing that can answer a send, apart from the ADR 0084 runtime class-method funs, which are a separate, deliberate mechanism.
- The meaning must be the same in the REPL, `beamtalk run`, `beamtalk test` and a release (`docs/beamtalk-principles.md` §4).
- Live patching is a message send (`docs/beamtalk-principles.md` §11). Anything this ADR removes from that surface is a regression against a stated principle and has to be named.
- Changing a REPL error message or reply needs the owner's sign-off (AGENTS.md, "REPL output"). The owner's decision to make cross-file `>>` an error at the REPL is that sign-off for the two changes named in Decision §1.

## Decision

**Cross-file extension methods are removed.** A `Class >> selector => ...` or `Class class >> selector => ...` definition is valid only where the class's source is at hand: in the file that defines the class, or as a live edit of a class whose source the workspace holds.

### 1. What stays and what goes

| Form | After this ADR |
|---|---|
| `Foo >> sel => ...` in the file that defines `Foo` | Unchanged. Folded into the class (BT-4721). |
| `Foo >> sel => ...` and `Foo class >> sel => ...` at the REPL | Unchanged. Recompiles the class from its stored source and hot-loads it. |
| `Class compile: #sel source: "..."`, IDE and MCP `save_method` | Unchanged. Same recompile path. |
| `Proto >> sel => ...` at the REPL (ADR 0127 §11) | Unchanged. Patches the provision and re-expands users. |
| **The `>>` reflection operator**, `Foo >> #sel`, which answers a `CompiledMethod` | **Unchanged.** Only `>>` as a cross-file method *definition* is removed. Code built on the operator, such as exdura's `self runActivity: OrderActivities >> #reserveInventory:`, is not affected |
| `Foo >> sel => ...` in a file that does not define `Foo`, **including another file of the same package** | Removed. A compile error. |
| `pkg@Foo >> sel => ...` (ADR 0070) | Removed with it. |
| `beamtalk_extensions:register/4,5` called through FFI | Removed. The module is deleted. |

```beamtalk
// money.bt — still valid: Money is defined in this file
Value subclass: Money
  field: cents = 0

Money >> isZero => self.cents = 0
```

```beamtalk
// helpers.bt — a compile error after this ADR
String >> shout => self uppercase ++ "!"
```

```text
error: `String >> shout` adds a method to String, which is not defined in this file.
  = cross-file extension methods were removed (ADR 0152).
  hint: define the method in the file that defines the class (in the class
        body, or as `String >> shout` next to it), or, for a class loaded in
        a workspace, define it live from the REPL or the IDE.
        For a class you cannot edit, write a method on a class of your own
        that takes the String.
```

At the REPL, a live definition on a class with source works as before:

```beamtalk
>> Counter >> incrementBy: n => self.value := self.value + n
>> c incrementBy: 5
=> 6
```

A live definition on a class without stored source already fails, with "Class source not available for Integer" (`beamtalk_repl_loader.erl:241-246`), and `Class compile:source:` refuses a stdlib class outright (`beamtalk_repl_eval.erl:1062-1073`) *(read; not run)*. So `Integer >> double => self * 2`, the REPL example in ADR 0066 §1, fails today as far as the code shows. This ADR does not change that behaviour.

There is no deprecation period. `Workspace load:` of a file with a cross-file `>>` fails with the compile error above, and an inline REPL eval that mixes a class definition with a foreign `X >> m`, which today drops the method with a warning (`inline_definitions.rs:90-97`) *(read)*, fails with the same error. These are changes to REPL output, made by this decision.

### 2. Lookup order after removal

1. The receiver's class: its compiled method.
2. Each ancestor in turn: its compiled method.
3. `doesNotUnderstand:args:`.

The class side follows the same order over class methods, with one existing addition that this ADR keeps: an ADR 0084 runtime class-method fun of a class is checked before that class's compiled class methods (`put_class_method`, `beamtalk_object_class.erl:691`) *(read)*.

There is no extension step at any level, on either side. The BT-5060 one-hop gap disappears because there is nothing to skip. `respondsTo:`, `canUnderstand:`, `perform:` and `super` follow the same order because they have nothing else to consult.

**The guarantee, once the runtime pieces are deleted (Phase 2).** Loading a package cannot change which method answers a send to an existing class. Between Phase 1 and Phase 2 the table still exists, but nothing the compiler accepts can fill it.

### 3. What is deleted

The full list, with files, is in Implementation. In summary:

- The runtime extension registry, `beamtalk_extensions.erl`, with its three ETS tables, last-writer-wins policy and conflict history.
- The extension lookups interleaved in dispatch, in generated code and in `beamtalk_dispatch.erl`.
- The one-hop paths and the BT-5060 warnings that merged as #5718.
- The class-side extension-first order and the `extension` kind of shadow flag. The `runtime_fun` and `class_vars` kinds stay.
- The compiler's extension index, conflict check and foreign-extension codegen.
- The WS3 plumbing that carries dependency extensions to a consumer's type checker (BT-4608, BT-4610, BT-4611).
- Reflection and tooling that exist only for extensions: `SystemNavigation extendersOf:` and `extensionsBy:` (`system_navigation.bt:223`), the browse "extensions" group, extension rows in xref and the ChangeLog.

What stays: `same_file_fold.rs`; the REPL and IDE recompile path; `StandaloneMethodDefinition` in the AST (the same-file and live forms parse to it); the class-derived cache and its generation read, which hot reload still needs (BT-4984 kept that read partly for extensions; this removes one reason for it, not the read); ADR 0084 runtime funs.

### 4. Patching a running class

What the code supports today, and so what remains:

| Target | Instance method | Class method |
|---|---|---|
| Class with source in the workspace | `Foo >> sel => ...` at the REPL; `Class compile:source:`; MCP and IDE `save_method`. All recompile the class through `install_method/9` (`beamtalk_repl_loader_methods.erl:85-171`) *(read)* | `Foo class >> sel => ...` at the REPL, the same path with `is_class_method = true` (`beamtalk_repl_loader.erl:268-279`) *(read)*. **Not** reachable from MCP `save_method` or `Class compile:source:`, which are instance side only (`beamtalk_class_refactor.erl:1208-1210`; `SaveMethodParams` has no side field) *(read)* |
| Stdlib class | Refused: "built-in methods are read-only in the workspace" (`stdlib_method_read_only_error`, `beamtalk_repl_eval.erl:1238`) *(read)* | Refused the same way |
| Dependency class | Fails: no class source is stored for a dependency (`activate_dependency_modules` registers the module and source path only, `beamtalk_workspace_bootstrap.erl:121-128`) *(read)* | Fails the same way. An ADR 0084 runtime fun can still be installed |

Cross-file extensions were the only way to change the last two rows from a file. For the instance side nobody relied on it. For the class side, BT-5061 kept "extension first" on purpose, because a class-side extension was "the documented way to patch a class method of a running class" (`docs/beamtalk-language-features.md`, "Hot-patching and direct calls"), and BT-4133, BT-3700, BT-4270, BT-4301, BT-4316 and BT-4317 built the shadow flags and static call keys to honour it.

After this ADR, a class method of a class without stored source can be replaced only by an ADR 0084 runtime fun or by redeploying the class. That is a regression against principle 11. It is the one open question (Open Question 1). The implementation proceeds on that question's stated default, and Phase 2 is ordered so the class-side pieces go last and can be held back if the owner chooses otherwise.

### 5. What to do instead, until there is a replacement

- **You own the class.** Put the method in the class's file, as a body method or a same-file `>>`.
- **You do not own the class.** Write the operation on a class of your own that takes the value: `Shout of: aString` in place of `aString shout`.
- **JSON for a foreign class.** Convert at the boundary in your own class's mapping. `JsonMapping mapField:to:using:` takes a converter (`stdlib/src/json_mapping.bt:41-78`) *(reviewer: read by a research pass, not re-read)*, so a class that holds a foreign `Money` can say how to encode it without `Money` answering `asJson`.
- **You need the foreign class itself to answer.** That is the trigger for the Future direction below. Open an issue with the concrete case.

### Single source of truth

- The rule "a `>>` target must be defined in this file" is enforced in one place, after `fold_same_file_extensions` (`crates/beamtalk-core/src/compilation/same_file_fold.rs:46-87`): any definition the fold leaves in `Module.method_definitions` is the error. Build, `check`, `lint`, the REPL compiler port, MCP and the LSP all call the fold already *(read)*, so they report the same diagnostic.
- The lookup order is stated once in `stdlib/test/lookup_order_matrix_test.bt`, which already compares literal sends, `perform:`, `respondsTo:` and `super` against one reference order. It keeps that role with the extension dimension removed.

## Prior Art

| Language | Can outside code add a method to a class or type? | Notes |
|---|---|---|
| Pharo, Squeak | Yes, `*Package` extension protocols | Last loaded wins. Overrides are a known packaging problem |
| Ruby | Yes, open classes | Last definition wins. Refinements (lexical) are not seen by `respond_to?` |
| Objective-C | Yes, categories | Undefined which implementation runs on a clash (Apple QA1908) |
| Swift | Yes, extensions; retroactive conformance warns since SE-0364 | Resolved statically for non-protocol members |
| Kotlin, C# | Yes, but statically resolved and lexically scoped | The member always wins, silently |
| Elixir | Only through a protocol: `defimpl P, for: T` | Reached as `P.fun(x)`; nothing enters the type's own functions |
| Rust | Only through a trait, under the orphan rule (RFC 2451) | No inherent methods on a foreign type |
| Go | No. Methods may be declared only on types defined in the same package | The idiom is a function or a local wrapper type *(from memory, not searched)* |
| Newspeak | No | No global namespace to extend |
| Gleam, Erlang | No | Functions in your own module |
| Python | Not for built-in types | User classes can be patched; `str` and `int` cannot *(from memory, not searched)* |

After this ADR Beamtalk sits with Go, Newspeak, Gleam and Erlang: a class's methods are defined with the class. That is a departure from Pharo, taken for the reason ADR 0066 itself gave for rejecting Pharo's last-writer-wins: a package ecosystem is not one developer's image.

The languages that allow outside additions and keep them sound (Rust, Elixir) do it through a protocol or trait with rules about who may implement it. That is the shape of the Future direction.

## User Impact

**Newcomer.** One fewer feature to learn, and one fewer way for a method to appear from nowhere. Someone coming from Ruby or Kotlin will try `String >> shout` in a helper file and get an error that says what to write instead.

**Smalltalk developer.** This is a real departure: Pharo developers extend `Object` and `String` freely. What stays is the part of the Smalltalk experience Beamtalk actually exercises: redefine a method on a live class from the REPL or the browser and see it take effect.

**Erlang/BEAM developer.** A class is its module again. To find what answers a send, read the class and its superclasses; there is no table to consult. Generated dispatch loses its extension probes.

**Production operator.** Loading a package cannot change the behaviour of an existing class. The cost: a class method of a deployed class without source can no longer be replaced from a file (Decision §4).

**Tooling developer.** No secondary index for extension locations, no "extensions" group, no dependency-extension channel, and no `dependency_extensions_unknown` guard in the type checker.

## Steelman Analysis

### Keep ADR 0066 open classes
- 🧑‍💻 **Newcomer**: "`String >> shout => ...` is one line. Now I have to invent a helper class."
- 🎩 **Smalltalk purist**: "Open classes are Smalltalk. Every Pharo package extends `Object` and `String`. Removing them makes sealed primitives truly closed."
- ⚙️ **BEAM veteran**: "It's built and tested, and the matrix test pins the order. Deleting working code has its own risk."
- 🏭 **Operator**: "I can hot-patch a class method on a running node from a file, without its source. You are taking away an incident tool."
- 🎨 **Language designer**: "It covers both axes of the expression problem with one construct. Removing it with nothing in its place leaves a hole you will have to fill later, under pressure."

### Additive-only `>>` (keep the syntax, forbid overriding)
- 🧑‍💻 **Newcomer**: "Same one-liner as before, and I get an error instead of a surprise when it clashes."
- 🎩 **Smalltalk purist**: "The syntax is unchanged; only overriding goes."
- ⚙️ **BEAM veteran**: "A miss-only table is one lookup on a path that was going to fail anyway."
- 🏭 **Operator**: "Compiled methods always win, so loading code still can't change what works."
- 🎨 **Language designer**: "It fixes the precedence bugs, which are the actual cost, and keeps the capability. Removal throws away the capability to fix the same bugs."

### Replace now with protocol implementations (`implementFor:`)
- 🧑‍💻 **Newcomer**: "I'd rather learn the replacement once than lose the feature and get a different one later."
- 🎩 **Smalltalk purist**: "At least the method is still a message send on the object."
- ⚙️ **BEAM veteran**: "It's `defimpl` with ordinary sends."
- 🏭 **Operator**: "One migration, not two."
- 🎨 **Language designer**: "The design is already worked out. Shipping removal alone means a release where `asJson` on a foreign class is impossible."

### Pure protocol dispatch (Elixir): `Json encode: x`
- 🧑‍💻 **Newcomer**: "It's obvious where the code lives: in the protocol."
- 🎩 **Smalltalk purist**: "`Json encode: x` is still a message send, to the service that owns the operation."
- ⚙️ **BEAM veteran**: "Nothing enters the class's lookup, so nothing can go wrong in it."
- 🏭 **Operator**: "No table on the dispatch path at all."
- 🎨 **Language designer**: "Implementations are keyed by `{protocol, class}`, so an ownership rule gives real coherence."

### Tension points
- The language designer's case for additive-only `>>` is the strongest argument against plain removal: it buys the same runtime guarantee and keeps the capability. The answer is that it also keeps a table on the miss path, a registration protocol, load-order questions and tooling for a feature with no users, and it commits to a design (selector-keyed, no ownership) before any real case has shown what the design should be.
- Operators lose a real tool (Decision §4). Nobody is known to have used it, but its absence will be felt exactly when it is needed.
- Removing first and replacing later means two changes for anyone who did depend on the feature. The survey found nobody.

## Alternatives Considered

### Keep ADR 0066 open classes as they are
Rejected. The feature has no users outside its own tests, about 52 Erlang and 111 Rust files carry it, and its lookup order has produced about nine precedence bugs in 90 days, the last of which (BT-5060) was closed by adding warnings instead of fixing the order. The cost recurs with every new dispatch optimisation, because each one must prove that no extension outranks the method it resolved.

### Additive-only `>>` without protocols
Keep the cross-file syntax, make a clash with a compiled method an error, and consult the table only on a miss.

```beamtalk
// app/src/Money+Json.bt
Money >> asJson => self class jsonMapping encode: self
Money class >> jsonMapping => JsonMapping new |> mapField: #cents to: "amount"
```

This removes the precedence bugs and keeps the capability. Compared with removal and with the deferred design:

| | Remove (this ADR) | Additive-only `>>` | `implementFor:` (deferred) |
|---|---|---|---|
| Precedence bugs removed | Yes | Yes | Yes |
| Extension table on the dispatch path | None | On a miss only | On a miss only |
| Registration, load order, clash handling at run time | None | Needed | Needed |
| Add a method to a foreign class | No | Yes | Yes |
| Ceremony for one method | n/a | None | A protocol |
| Provided methods come with the protocol | n/a | No; each is written out again | Yes |
| Something to attach an ownership rule to | n/a | No | Yes |
| WS3 dependency plumbing | Deleted | Kept | Kept, as declarations |

Rejected for now. It keeps a runtime table, a registration protocol and the dependency plumbing alive for a feature with no users, and it fixes the shape of the replacement (keyed by selector, no ownership) before a real case exists. It stays on the list: if a need appears and the owner concludes that ownership rules are not worth their ceremony, this is the smaller of the two replacements, and nothing in the miss-only runtime design depends on which syntax fills the table.

### Replace now with protocol implementations
The first draft of this ADR. Add `Proto implementFor: Class`, then deprecate and remove cross-file `>>`. Rejected by the owner in favour of removing first, for three reasons: nothing uses the feature being replaced; the ownership rule at the centre of the design cannot be settled without a real case (see Future direction, F3); and the replacement is an XL piece of work whose only demonstrated use is `asJson`. The design is kept below.

### Pure protocol dispatch: `Json encode: x`, nothing enters the class's lookup
Implementations are keyed by `{protocol, class}` and reached only through the protocol: `JsonRepresentable asJson: x`, or a facade such as `Json encode: x`.

This is the cleanest design at run time and the only one where an ownership rule is fully sound. It is not needed for removal, so the question is whether it should be the future direction instead of `implementFor:`. It was weighed seriously and is not preferred, because:

- **It loses `x asJson`.** A class that writes `asJson` in its body and a class that has it implemented from outside would be called differently, or every caller would have to use the protocol form. Either the two populations are not interchangeable, or the language grows a second call syntax for every protocol-shaped operation.
- **It breaks the existing contract.** `Json generate:` finds the hook with `respondsTo: #asJson` and a send. `JsonMappable`'s provision is `asJson => self class jsonMapping encode: self`. Collections of mixed objects are sent `asJson` uniformly. All of that would have to become protocol dispatch.
- **It is a second dispatch mechanism.** Beamtalk's object model has one question, "does the receiver understand this message?". Pure protocol dispatch adds "does this protocol have an implementation for the receiver's class?", with its own inheritance walk, reflection and tooling.
- **Structural conformance would split.** A class conforming by having the method and a class conforming by external implementation would not both pass `conformsTo:` by the same test.

What `implementFor:` gives up in return is real: coherence is by `{class, selector}`, so its ownership rule is weaker than Rust's (F3). Because this ADR only removes, the choice between the two stays open until a real case arrives, and a library is free to offer `Json encode: x`-style entry points today.

### Lexically scoped static extension functions (Kotlin, C#)
`x shout` resolves at compile time, from the static type of `x`, to a function imported into the file.

Rejected because Beamtalk sends are dynamic and the static type is often `Dynamic`. ADR 0127 rejected "extension methods on a protocol" for the same reason. Concretely:

- `respondsTo: #asJson` would answer `false`, so the `Json generate:` hook would not find the method.
- `perform:`, a send through a `Dynamic` receiver, and a send from another file would not find it either.
- The REPL has no lexical import scope to resolve against, so the same expression would mean different things in a file and at the prompt (principles §4).
- Ruby refinements are the dynamic-language attempt at this, and they have exactly these gaps with `respond_to?`.

### A library-level registry: `Json register: aMapping for: Money`
Solve JSON in the JSON library and add nothing to the language. Rejected as the general answer: it is one registry per library, each with its own lookup, reload and ownership behaviour, and `x asJson` still would not work on the class. It remains a reasonable thing for a single library to do.

### Fold cross-file `>>` within one package
Allow `Foo >> sel` in another file of the package that defines `Foo`, and fold it into the class at build time as the same-file fold does. Rejected: "one file is one compilation unit for its class" (`docs/beamtalk-language-features.md`) is what makes reload replace a class whole, and a cross-file fold would make a class's compiled form depend on which other files the build saw. No package does this today.

### Deprecate in documentation only
Leave the runtime and tell people not to use it. Rejected: the cost is in the dispatch path and the tools, not in user code.

## Consequences

### Positive
- A send is answered by a compiled method or by nothing (plus ADR 0084 runtime funs on the class side). The lookup order has no extension step.
- The BT-5002, BT-5060, BT-5061 class of bug cannot recur, and neither can the extension-body codegen bugs (BT-4503, BT-3920), because that codegen is deleted too.
- Loading a package cannot change the behaviour of an existing class.
- The per-level extension probe leaves `has_method/1`, the default dispatch arms and the inherited-send path.
- `beamtalk_extensions.erl` (913 lines), `gen_server/extensions.rs` (479 lines), `extension_index.rs` (583), `extension_conflicts.rs` (447) and `dependency_extensions.rs` (272) are deleted outright *(line counts from a research pass)*.
- ADR 0070's recorded limitation, that extension conflicts between two dependencies are unresolvable (line 699), no longer applies.
- The #5718 warning plumbing for the one-hop gap is no longer needed.

### Negative
- There is no way to add a method to a class you cannot edit, sealed primitives included. ADR 0066 called extensions "the only way to add methods to sealed primitives"; after this ADR there is none.
- `asJson` on a foreign class is not possible until a replacement exists. The workaround is a converter in the holder's mapping (Decision §5).
- A class method of a class without stored source can no longer be replaced from a file, against principle 11 (Decision §4, Open Question 1).
- This is a breaking language change with no deprecation period. The survey found no user code affected, but it could not see private packages; such a package breaks at its next build.
- 19 test files that use cross-file `>>`, and 23 `stdlib/test` files that call `beamtalk_extensions` through FFI, need rewriting or deleting. Some of them use an extension as a convenient way to set up a different scenario, and need another way to do that.
- If a replacement is added later, the miss-path, registration and tooling work is done again from a clean base, not adapted from the code deleted here.
- Deleting about 2,700 lines across dispatch, codegen and the type checker carries regression risk in exactly the code paths that had the bugs.

### Neutral
- The class-derived cache and its generation read stay.
- ADR 0084 runtime class-method funs are unchanged, and become the only non-compiled thing that can answer a send.
- `StandaloneMethodDefinition` and `Module.method_definitions` stay in the AST. After the fold, a non-empty `method_definitions` is an error, so the validators that walk it (the BT-4658 and BT-4923 fixes) have nothing to check and can be removed in follow-up cleanups.
- Generated Core Erlang changes for every class (the dispatch default arms and `has_method/1` lose a probe), so `just core-diff` will show a diff on every module.

## Implementation

Sizes are rough (S, M, L).

### Phase 0: inventory (S)
Produce the list that Phases 1 and 2 work from. Known so far:

- **Tests that define cross-file extensions (19 files).** `stdlib/test`: 11 files, 16 target classes (`guarded_dispatch_differential_test`, `class_dispatch_differential_test`, `set_theoretic_operators_e2e_test`, `class_side_super_test`, `actor_extension_error_test`, `sealed_direct_dispatch_differential_test`, `value_super_extension_test`, `adr0133enumerable_user_test`, `own_class_literal_send_test`, `value_type_nlr_answer_test`, `foreign_extension_codegen_test`). `tests/parity`: `projects/mixed/src/array_extensions_host.bt`, `string_extensions_host.bt`, `diagnostics/lint_verdict_parity/src/shouting.bt`. `tests/repl-protocol/cases`: `hot_reload_actors`, `load_method_patch`, `protocol_live_patch`, `release_console_default`, `same_file_extension_redefine` *(read by a research pass)*. For each, record whether it tests extensions (delete) or uses one to reach another scenario (rewrite).
- **Tests that call `beamtalk_extensions` through FFI.** 23 `stdlib/test` files, `lookup_order_matrix_test.bt` among them.
- **Rust tests.** 37 Rust test files embed `X >> sel =>` in source strings; sort them the same way.
- **Erlang tests.** `beamtalk_dispatch_tests.erl` and the extension fixture from BT-5038 (`beamtalk_test_extensions.erl`).
- **The `beamtalk-uat` fixture** (`SomeActor >> bumpBy:`, BT-2452), in another repository.
- **Docs that teach cross-file `>>`.** `docs/beamtalk-language-features.md` (§ Extension Methods, § Cross-file extensions, "Hot-patching and direct calls", § Live Patching), `docs/beamtalk-agent-native-development.md`, `docs/beamtalk-tooling.md`, `docs/beamtalk-architecture.md`, `docs/development/surface-parity.md`, `docs/development/benchmarks.md`, and the `stdlib/src/behaviour.bt:496-500` doc comment, which says a patch to a stdlib or dependency class installs in memory when the code refuses it *(read)*. `examples/` has none.
- **ADRs to annotate.** 0066 (superseded in part), 0070 (`pkg@Target >>`, line 699), 0084 §5 (cross-class class-side extensions as future work), 0127 (§3a "foreign extensions count as inherited", §12 extension-conflict row), 0130 (the class-side extension fun shape).

Exit: the list is complete and each item has a disposition.

### Phase 1: make cross-file `>>` an error, and migrate (M)
There is no deprecation warning. One change does all of this:

- **The compiler rejects cross-file `>>`** from the single check after the fold, in `build`, `check`, `lint`, `test`, the REPL compiler port (`Workspace load:` and inline evals), MCP lint and the LSP, with the message in Decision §1.
- **Migrate the tests and the fixture** from the Phase 0 list: the 19 test files, the Rust tests that embed cross-file `>>` source, and the `beamtalk-uat` fixture (a change in that repository, landed together). Tests that pin extension precedence are deleted with a note of what they pinned. Tests that register through `beamtalk_extensions` FFI keep working until Phase 2 deletes the module, and are migrated here if they are in the way.
- **Stop emitting registrations.** With no cross-file `>>` accepted, `register_class/0` no longer contains `beamtalk_extensions:register` calls; delete `gen_server/extensions.rs`.
- **Docs and release notes.** Update the docs from the Phase 0 list and `docs/development/surface-parity.md`. Add a **breaking change** entry to `CHANGELOG.md` and the release notes: cross-file `>>` method definitions are removed; the `>>` reflection operator, same-file `>>` and live method definition are unchanged. Annotate the ADRs listed in Phase 0.
- **Must pass:** `just ci`, `just test-corpus` (the PR carries `ci:corpus`, because codegen changes), and the parity projects reporting the new error identically on CLI, MCP and LSP.

### Phase 2: delete the machinery (L)
Three steps, in this order, each its own PR. Phases 1 and 2a may be one PR if that is cleaner.

**Compiled code from before Phase 1.** A package built by an older compiler may still call `beamtalk_extensions:register/5` from its `register_class/0` when it loads. Until step 2c deletes the module that call succeeds, and the row is never consulted once 2b lands. After 2c the call fails with `undef`. Packages are rebuilt against the compiler they ship with, and the survey found no package with a cross-file extension, so this is accepted; 2c must make the failure legible (a clear load error naming the package and this ADR, not a bare `undef`) *(inferred: how module activation reports an `on_load` failure was not checked)*.

**2a. Compiler.** Delete `extension_index.rs`, `extension_conflicts.rs`, `ClassHierarchy::register_extensions` (`class_hierarchy/mod.rs:850`) and its class-side "extension replaces signature" rule (BT-5061, BT-5111), the `pkg@Target >>` qualifier, and the WS3 plumbing: `crates/beamtalk-project/src/dependency_extensions.rs`, the `dependency_extensions_unknown` guard (`class_hierarchy/mod.rs`, `receiver_knowledge.rs`, `consulted.rs`), and its carriers in `beamtalk-package` (`dependency_classes.rs`, `lint_pipeline.rs`, `manifest.rs`), `beamtalk-language-service`, `beamtalk-mcp` (`server/lint.rs`) and the CLI build commands. LSP and language-service code that indexes extension locations goes with it.

**2b. Instance-side runtime.**

| Piece | Where |
|---|---|
| Extension probes in generated code | `beamtalk_extensions:lookup` in the actor and value default arms and the `beamtalk_extensions:has` step of `has_method/1` (`gen_server/dispatch.rs:769-786`, `core_erlang/dispatch_spec.rs:355, 478`, `supervisor_codegen.rs:239`) |
| Interleaved lookups | `beamtalk_dispatch.erl`: the extension checks in `lookup/5`, `super/5` and `lookup_in_class_chain/5`; the `{ext, Owner}` verdict of `inherited_target/2`; `inherited_value_ext/5`; `check_extension/2` |
| The one-hop gap and its warnings (BT-5060, #5718) | `one_hop_children_missing_extension/2`, `one_hop_parent_extensions_missing/1` (`beamtalk_dispatch.erl:339, 368`); `warn_one_hop_children/2`, `warn_one_hop_for_class/1`, `emit_child_warning/3`, `emit_parent_warning/3` (`beamtalk_extensions.erl:316-410`); `warn_one_hop_gaps/1` (`beamtalk_repl_loader_activate.erl:156`); `with_extension_warning_scope/2` and its `'$extension_scope'` rows (`beamtalk_class_registry.erl:432`); the `bt5060_live_*` fixtures. The `slow` routing of `one_hop_selectors` (`value_type_codegen/dispatch.rs:308`) can then go straight to the superclass's `dispatch/3` |
| Guards against an extension overriding a known selector | The `String`-extension gate on the Binary fast path (`binary_or_string_module/2`, `beamtalk_primitive.erl:1005-1010`); the per-level extension probe behind the `equals:` shortcut (`beamtalk_equality.erl:120-140`); `beamtalk_collection_selectors.erl:90-127` (BT-3951); `beamtalk_default_method:note_override/2` on registration *(inferred: each needs checking that the extension case is its only reason)* |
| Reflection and tooling | `respondsTo:` and `canUnderstand:` extension checks (`beamtalk_behaviour_intrinsics.erl`); the extension-first step of `removeSelector:` and `purge_extensions` (`beamtalk_class_refactor.erl`, `beamtalk_class_lifecycle.erl:127-132`); `SystemNavigation extendersOf:` / `extensionsBy:`; the browse `extension` provenance (`beamtalk_repl_ops_browse_origin.erl`); extension xref rows and ChangeLog entries |

**2c. Class-side runtime.** Proceeds on the default of Open Question 1; held back if the owner chooses option (b) there.

| Piece | Where |
|---|---|
| Class-side extension-first order | `class_side_extension/2` (`beamtalk_class_dispatch.erl:945`) and its callers in `handle_class_method_call/6` and `walk_class_side`; the `extension` verdict of the owner memo (BT-4800); `invoke_class_extension/7` (ADR 0130) *(reviewer)* |
| Constructor extensions | The class-side-extension half of the `new` / `new:` check in `beamtalk_object_class.erl` (BT-4409, BT-4588). The ADR 0084 runtime-fun half stays |
| The `extension` shadow-flag kind | `beamtalk_class_shadow_flags.erl`, and the direct-call-key invalidation that an extension registration triggers. `runtime_fun` and `class_vars` stay |
| The registry itself | `beamtalk_extensions.erl`, deleted last, with `beamtalk_test_extensions.erl` |

**Tests that must still pass after each step:**
- `just test`, `just test-corpus` and `just verify-threaded-ir`. Phase 1 and step 2b change codegen, so their PRs carry `ci:corpus`. `ci:core-diff` is expected to show a diff on every module in 2b; review a sample.
- `stdlib/test/lookup_order_matrix_test.bt`, reduced to the order in Decision §2, for actors, value types, primitives and class-side sends through a literal send, `perform:`, `respondsTo:` and `super`.
- The same-file fold tests (BT-4721, BT-4489, BT-4799) and `tests/repl-protocol/cases/same_file_extension_redefine.btscript`.
- Live patching: `load_method_patch.btscript`, `hot_reload_actors.btscript`, `protocol_live_patch.btscript`, and the `Class compile:source:` and `removeSelector:` tests.
- ADR 0084 runtime class-method fun tests, and the BT-4133 / BT-4316 / BT-4317 direct-call tests with the extension cases removed.
- `just test-repl-protocol`, `just dialyzer`, and the `tests/parity` projects, which must report the new error identically on CLI, MCP and LSP.

A benchmark run (`docs/development/benchmarks.md`) before and after 2b records what the removed probes cost.

## Migration Path

A cross-file extension has no direct replacement, and there is no deprecation period.

| You have | Do this |
|---|---|
| `Foo >> sel` in another file of the package that defines `Foo` | Move the method into `Foo`'s file |
| `String >> shout` or another addition to a class you do not own | A method on your own class that takes the value: `Shout of: aString` |
| An extension that overrides a method of a class you do not own | Subclass, if the class is not sealed, or wrap |
| A class-side extension used to patch a deployed class | An ADR 0084 runtime fun, or redeploy (Open Question 1) |
| `Foo asJson` on a foreign `Foo` | A `using:` converter in the holder's `JsonMapping` (Decision §5) |

Known affected code outside this repository: the `beamtalk-uat` fixture only, which is migrated with Phase 1. There is no warning period: a package the survey could not see gets the compile error on its next build, with the hint above. The change is listed as breaking in `CHANGELOG.md` and the release notes.

## Future direction: retroactive protocol implementations

This section is not part of the decision. It records the design worked out in the first draft so that it is the starting point if the need appears.

**Trigger.** A concrete package that needs to attach a protocol to a class it does not own, for example a package that must make another package's value class `JsonRepresentable`. One real case also supplies what the design could not settle on paper: who owns what, and so which ownership rule is right.

**Outline.** A top-level declaration makes a class answer a protocol's selectors, allowed only where it cannot change what a compiled method does:

#### F1. Syntax

```beamtalk
// app/src/Money+JsonRepresentable.bt
JsonRepresentable implementFor: Money
  asJson => #{"amount" => self cents, "currency" => self currency}
```

The header is `ProtocolName implementFor: ClassName`. The indented body holds method definitions, written as in a class body. A class-side method is prefixed `class`, as in a class or protocol body.

The owner's sketch ended the header with `=>`. This draft drops it. `Protocol define: X` and `Object subclass: X` are the existing declaration headers and neither takes `=>`; `=>` introduces one method body everywhere else. This is a recommendation, not a decision.

Either name may be package-qualified as ADR 0070 allows: `json@JsonRepresentable implementFor: money@Money`.

The file convention from ADR 0066 stays, with the protocol as the feature: `Money+JsonRepresentable.bt`. One file may hold several implementations.

A provided method comes with the protocol. This is the BT-2819 gap closed:

```beamtalk
JsonMappable implementFor: Money
  class jsonMapping -> JsonMapping =>
    JsonMapping new
      |> mapField: #cents to: "amount"
      |> mapField: #currency
// Money now answers `asJson`, which JsonMappable provides.
```

#### F2. Rule: additive only

An implementation may add selectors. It can never change what a compiled method does.

**At compile time:**

- The body may define only selectors the protocol declares (required or provided), on the side the protocol declares them.
- A body selector that the class already defines or inherits as a compiled method, on that side, is a **compile error**. Methods flattened in from a trait (ADR 0127 §3), synthesized accessors and constructors, and (on the class side) the methods of the class object's own chain (`Class`, `Behaviour`, `Object`) all count as compiled.
- After the implementation, the class must conform to the protocol: every required selector resolves, from the body, the class's own methods or the protocol's provisions. Otherwise it is a compile error.
- An implementation that installs nothing, because the class already conforms, is allowed and gets a lint warning that it is redundant.
- **Clash between implementations.** Two implementations that install the same selector on the same side are a compile error, naming both, when they target the same class, or when they come from different packages and target classes related by inheritance. One package may implement a selector for both `Collection` and `List`; the nearer one answers. Two packages may not, because loading one would change what the other's sends do.
- **Known subclasses.** If a subclass visible in the build already compiles an implemented selector with a signature incompatible with the protocol's, the build warns: the subclass is now declared to conform and does not.

```text
error: JsonRepresentable implementFor: Money defines `asJson`, but Money already
       defines `asJson` (money/src/money.bt:14).
  = an implementation can add selectors to a class; it cannot replace one.
  hint: delete this method; Money's own `asJson` already answers.
```

**Provided methods.** Every provision of the protocol is registered for the class, whatever the class already understands. Miss-only lookup makes that safe: a provision the class already answers with a compiled method never runs. For typing and reflection the compiler computes which provisions are shadowed.

- A provision shadowed by a method in the class's own body is silent, as class-wins is in ADR 0127 §3.
- A provision shadowed by an *inherited* method gets a warning naming both.
- This differs from `uses:` on purpose. Under ADR 0127 §3a a provision may replace an inherited method when the class writes `overriding:`, and a `printString` provision replaces `Object`'s without it. Under `implementFor:` a provision never replaces anything. `Describable implementFor: Money` does not change `Money printString`; `uses: Describable` in `Money`'s body does.
- Registering every provision also covers version skew: if the owner later removes the method that shadowed a provision, the provision answers instead of the send failing.

**At run time:**

- Implemented methods live in a **protocol-implementation table**. It is consulted only where the root `doesNotUnderstand:args:` would otherwise raise: the compiled lookup has missed on the receiver's class and every ancestor, **and** no class on the chain below `ProtoObject` compiles its own `doesNotUnderstand:args:`. A compiled `doesNotUnderstand:args:` is a compiled answer. So `Erlang`, `ErlangModule` and forwarding proxies keep receiving every miss, and an implementation for `Actor` or `Object` does not reach them.
- The table is searched from the receiver's class upward, so an implementation for `Collection` answers for `List`.
- A compiled method anywhere in the chain is found first. There is no interleaving, and the class side follows the same order as the instance side. On the class side the table comes after the class object's own chain.
- Rows are registered by class *name*. The class need not be loaded yet, because module activation orders by superclass only *(reviewer)*. When a class activates, the runtime re-runs the additive check for its rows, as `warn_one_hop_for_class/1` did for extensions.
- If a class gains a compiled method for an implemented selector (the owner ships a new version, or the class is hot-reloaded), the compiled method answers. The next build of the implementing package fails with the error above. At run time the dead row is reported once as a warning.
- **A run-time clash does not depend on load order.** If two packages that were never built together register clashing rows (possible in a REPL or after `:sync`), neither answers: the send raises a structured `ambiguous_implementation` error naming both, until one is removed. The registration is also reported in the reply warnings.

**The guarantee, stated exactly.** With cross-file `>>` already gone, loading a package cannot change the result of a send that a compiled method answers. It can make a send answer that used to raise `does_not_understand`, which is the purpose, so code that branches on `respondsTo:` or catches DNU can observe it. It cannot change which implementation another package's send reaches, because of the clash rule.

This keeps the property the removal buys: no dispatch path, static call or cache has to ask whether something outranks a compiled method.

The cost is real and deliberate: an implementation cannot customise a selector that `Object` or another ancestor already defines. `Printable implementFor: Money` with a `printString` body is an error, because `Money` inherits `printString`.

#### F3. Rule: ownership

An implementation is allowed only if the declaring package owns the protocol or the class.

**Definition of "owns".** Each class and protocol resolves to exactly one owner, and the check compares that owner with the declaring package:

| Entity | Owner |
|---|---|
| A class or protocol in a file compiled under a `beamtalk.toml` | That manifest's `[package] name` |
| A class or protocol carried in from a dependency | The dependency's package name (`ClassInfo.package`, `ProtocolInfo.package`) |
| A stdlib class or protocol, **including bootstrap builtins** whose `ClassInfo.package` is `None` (such as `Future`) | `stdlib` |
| A class or protocol defined at the REPL, or in a script outside any project | The workspace's package |
| Anything whose owner cannot be resolved | Unknown. An unknown owner never matches, so the implementation is refused |

The declaring package is the `[package] name` the file is compiled under, or the workspace's package at the REPL.

This needs one resolver, written once, because `None` in `ClassInfo.package` means "REPL classes and bootstrap builtins" (`class_info.rs:330-333`) while `None` in `ProtocolInfo.package` means the package being compiled (`protocol_registry.rs:141-145`) *(read)*. It also needs every protocol's signatures and owner to reach consumers. Today only protocols with provided methods, and protocols named like a stdlib trait, are carried into other compiles (`is_carried_protocol`, `trait_expansion.rs:567`) *(read)*, so a requirements-only protocol such as `JsonRepresentable` may reach a consumer without its requirement list or owner *(reviewer)*.

So a user package may implement its own protocol for `String`. It may not implement `stdlib`'s `JsonRepresentable` for another package's `Money`.

```text
error: package `app` cannot implement JsonRepresentable (owned by `stdlib`)
       for Money (owned by `money`): it owns neither.
  hint: ask `money` to implement it, or declare a protocol in `app`.
```

In an acyclic dependency graph, at most one package can satisfy the rule for a given protocol and class from each side, and both sides at once would need the two packages to depend on each other. So for any protocol and class in different packages there is exactly one package that may write the implementation without a circular dependency, plus the degenerate case where one package owns both.

**What the rule guarantees, and what it does not.** In Rust the orphan rule gives coherence because a method belongs to its trait. Beamtalk selectors are global and conformance is structural, so the rule is much weaker here. The owner should decide with this in view:

- It guarantees that every added selector has a named, owned contract.
- **It does not stop the thing it was proposed to stop.** An app that owns neither side can write `Protocol define: AppJson` with an `asJson` requirement and implement that for `Money`. Afterwards `Money asJson` works, `Json generate:` finds it, and `Money conformsTo: #JsonRepresentable` answers `true`, because conformance is structural. Two unrelated packages can each do this for `String`.
- For `JsonMappable` the workaround is worse: the local protocol has to copy the provided `asJson` body, which is the duplication this project's rules forbid.
- What actually prevents two packages from both adding `String asJson` is the clash rule in F2, at the consumer's build. The consumer still cannot repair a clash between two dependencies (the limitation ADR 0070 already records at line 699).

As written, then, the ownership rule costs ceremony and protects little. The three coherent positions are listed under "Questions to settle then" below. Choosing between them needs a real case, which is one reason this design is deferred.

#### F4. Rule: declared conformance

An implementation is a declaration that the class conforms.

- **Type checker.** The implementation's selectors join the class's method surface, and the class conforms to the protocol, so a `Money` is accepted where `:: JsonRepresentable` is expected. Conformance in Beamtalk is structural (ADR 0068), so this mostly follows from the selectors being visible. The new part is the completeness check in F2. A compiled signature always beats an implementation's signature, on both sides. Implemented methods are tagged as such in the class's method table, so static calls, guarded direct calls and sealed self-sends never target them (BT-4133 is the precedent for what goes wrong otherwise).
- **Runtime.** `Money conformsTo: #JsonRepresentable` answers `true`. `conforms_to/2` (`beamtalk_protocol_registry.erl:434`) uses `classCanUnderstandFromName`, which will consult the implementation table after the compiled walk.
- **Reflection.** Added beside the ADR 0127 §12 surface: `Protocol implementationsOf: #JsonRepresentable` (the classes with a declared implementation) and `Behaviour implementedProtocols`. `CompiledMethod origin` answers the protocol, as it does for a flattened trait method. `usersOf:` stays reserved for `uses:`.
- **Browse.** Implemented methods are grouped under the protocol's name with a new `provenance := implementation` row, where the `extension → "extensions"` branch used to be (`protocol_from_source/4`, `beamtalk_repl_ops_browse_origin.erl:275`).

#### F5. Rule: call sites unchanged

`x asJson` is an ordinary send. `respondsTo:`, `canUnderstand:`, `perform:` and `super` see an implemented method, after compiled methods. The `respondsTo: #asJson` hook in `beamtalk_json.erl:344-350` needs no change.

```beamtalk
>> (Money cents: 250 currency: "EUR") asJson
=> #{"amount" => 250, "currency" => "EUR"}
>> Money conformsTo: #JsonRepresentable
=> true
>> (Money >> #asJson) origin
=> #JsonRepresentable
```

#### F6. Class-side requirements

`implementFor:` supports the protocol's class-side requirements (BT-1611, ADR 0144), written `class selector => body`. The same rules apply on the class side: additive only, miss-only lookup after the class-method chain. After the implementation the class conforms on both sides, so it is accepted where `P class` is expected.

A class-side implemented method is compiled as the class-side extension fun was, `fun(Args, ClassSelf) -> Result` (ADR 0130 §3). It runs where a class method of the target class runs under ADR 0142: in the caller, unless the class or an ancestor declares class variables or the selector is process-bound, in which case it runs in the class process. That is what a class-side extension did (ADR 0142, BT-4307).

Class-side *provisions* are still post-v1 in ADR 0127 §1; this ADR does not change that.

#### F7. What an implementation body may do

- **Public interface only.** A body may send messages to `self`. It may not read or write `self.field`, and a class-side body may not touch the target's class variables. This is the sketch's default and is not decided. The reasons: the implementer usually does not own the class, a field rename in the owner's minor release would otherwise break the implementation at run time with no build error, and ADR 0127 §7 already makes provided methods stateless.
- **Actors.** An implemented method on an `Actor` class runs inside the actor's process, as a foreign extension did, and its `self` sends are same-process sends. It would use the old extension fun shape, `fun(Args, Self, State) -> {Result, NewState}` (`gen_server/extensions.rs:27-34`), because a self-send can change state even when the body cannot touch slots. `JsonMappable`'s provision relies on same-process self-sends.
- **`^` and blocks.** Bodies need the body codegen this ADR deletes with `gen_server/extensions.rs`, or a successor. That code had its own bug history (BT-4503, BT-3920). A future implementation must run the `^`-in-block and error-propagation cases against value, actor and class-side implementations.

#### F8. Hot reload and removal

- **Reloading the implementing file** replaces the rows that module owns. A method deleted from the file stops answering. A deleted cross-file extension used to keep answering until `removeSelector:` or a restart.
- **Deleting the implementing file** and running `:sync` purges the rows of a module that is gone.
- **Re-evaluating an implementation** from the same package replaces it. A clashing implementation from a different package makes the selector ambiguous (F2). There is no last-writer-wins and no first-writer-wins.
- **Reloading the class** changes nothing unless the class now compiles an implemented selector. Then the compiled method answers, and a warning names the dead implementation row.
- **Editing the protocol** (a provision or a requirement) re-expands implementations with the users, under ADR 0127 §11's two-stage rule: compile everything, then load.
- **`Money removeSelector: #asJson`** on an implemented selector is an error, as it is for a trait-provided selector (`refuse_if_protocol_provided/4`, `beamtalk_class_refactor.erl:267-276`): "`asJson` is implemented for Money by `JsonRepresentable implementFor: Money` (package `app`); remove the implementation instead." Removing one selector would leave a declared conformance that is false.
- **Removing an implementation** is one operation on the whole unit: `Protocol removeImplementationOf: #JsonRepresentable for: #Money` (spelling not decided). It records a ChangeLog entry as extension removal did.
- **`removeFromSystem`** on the class or the protocol purges its implementation rows, as `purge_extensions` did.


#### F9. Questions to settle then

1. **The ownership rule.** As stated in F3 it can be sidestepped with a local protocol, and it blocks the plain form of the motivating case. Three coherent positions:
   - **(a) Selector-based, with a root-package exemption.** A package may not make a foreign class answer a selector that a dependency's protocol declares, unless it owns that protocol or the class. The root package of a build is exempt, because nothing depends on it; an orphan implementation in a dependency is an error. This is the only option that does what the rule was proposed to do. "Root package" means the package being built, not a package with an `[application]` section; a library can have one.
   - **(b) As stated in F3.** Simple, and documents intent, with the loophole accepted.
   - **(c) A lint.** No hard rule; warn on an orphan implementation in a library, as Clojure's guidance does. The clash rule in F2 remains the real protection. This strengthens the case for additive-only `>>` instead.
2. **Header syntax.** With or without a trailing `=>`; whether there is also a reflective message form for live use (principles 6 and 11); the removal selector.
3. **Run-time clash behaviour.** Ambiguous (neither answers, F2) or first-registered-wins with a warning.
4. **Slot and class-variable access** from implementation bodies (F7), and whether the class's own package gets more.
5. **REPL strictness.** Whether the ownership rule applies at the REPL.
6. **`methods` and `includesSelector:`.** Whether they list implemented selectors.
7. **Whether this is the right replacement at all.** Additive-only `>>` and pure protocol dispatch (Alternatives) should be re-weighed against the real case.

A first implementation step would be a wire check: register a hand-written fun in a miss-only table through FFI, hook the root DNU path, and run the lookup-order matrix for a value class, an actor, a primitive, a class-side send, a class with its own `doesNotUnderstand:args:`, `respondsTo:`, `perform:` and `super`. DNU handling is generated per class today (`gen_server/dispatch.rs:986-1021`) *(read)*, so whether the hook has one choke point is a real question.

## Open Questions

1. **Patching a class method of a class with no stored source.** After removal, such a method can be replaced only by an ADR 0084 runtime fun or a redeploy (Decision §4). Options:
   - **(a) Accept the loss, and name ADR 0084 as the route.** Remove the class-side extension path with the rest (step 2c). Document the ADR 0084 runtime class-method fun as the supported way to hot-patch a class method of a class without source, and file a follow-up to give MCP `save_method` and `Class compile:source:` a class-side form, since `install_method/9` already supports it.
   - **(b) Keep the class-side extension path** until the workspace can store dependency class source, so `Foo class >> sel => ...` recompiles a dependency class as it does a project class.

   **Default taken by the implementation: (a).** The reasons: no package uses a class-side extension; keeping (b) keeps the `extension` shadow flag, the extension-first order and the registry alive, which is most of the class-side cost this ADR exists to remove; and the stdlib is read-only in the workspace either way. Whether a user can reach the ADR 0084 runtime fun without FFI was not checked; if not, the follow-up includes an entry point. The owner can switch to (b) at any time before step 2c lands.

**Decided, and recorded here:**

- **Sequencing.** Remove cross-file `>>` now; defer `implementFor:` until a concrete need appears (Future direction).
- **Deprecation.** None. Cross-file `>>` becomes an error in one step, because no package in the survey uses it.
- **REPL output.** A file load or inline eval containing a cross-file `>>` fails with the compile error of Decision §1.

## References
- Related issues: BT-5060 (#5718), BT-5002, BT-5061, BT-5062, BT-4307, BT-4588, BT-4250, BT-4133, BT-3951, BT-4503, BT-3920, BT-4260, BT-4280, BT-4800, BT-4984, BT-4721, BT-4610, BT-4608, BT-4611, BT-4658, BT-4923, BT-2819 (`JsonMapping`, `JsonMappable`; #5717, #5719), BT-2452
- Related ADRs: [ADR 0066](0066-open-class-extension-methods.md) (cross-file part superseded), [ADR 0068](0068-parametric-types-and-protocols.md) (structural protocols), [ADR 0070](0070-package-namespaces-and-dependencies.md) (packages; `pkg@Target >>`), [ADR 0084](0084-class-side-runtime-method-fun-dispatch.md) (runtime class-method funs), [ADR 0127](0127-traits-for-behavioural-reuse.md) (traits), [ADR 0130](0130-class-variables-live-in-the-class-process.md) (class-side extension fun shape), [ADR 0142](0142-class-process-owns-only-class-state.md) (where class methods run), [ADR 0144](0144-class-side-protocol-types.md) (class-side protocol types)
- Code: `runtime/apps/beamtalk_runtime/src/beamtalk_extensions.erl`, `beamtalk_dispatch.erl`, `beamtalk_class_dispatch.erl`, `beamtalk_class_shadow_flags.erl`; `runtime/apps/beamtalk_workspace/src/beamtalk_repl_loader.erl`, `beamtalk_repl_loader_methods.erl`; `crates/beamtalk-core/src/compilation/same_file_fold.rs`, `extension_index.rs`, `extension_conflicts.rs`; `crates/beamtalk-codegen/src/core_erlang/gen_server/extensions.rs`; `crates/beamtalk-project/src/dependency_extensions.rs`; `stdlib/test/lookup_order_matrix_test.bt`
- Prior art: [Rust RFC 2451](https://rust-lang.github.io/rfcs/2451-re-rebalancing-coherence.html), [Swift SE-0364](https://github.com/apple/swift-evolution/blob/main/proposals/0364-retroactive-conformance-warning.md), [Elixir `Protocol`](https://hexdocs.pm/elixir/Protocol.html), [Clojure protocols](https://clojure.org/reference/protocols), [Haskell orphan instances](https://wiki.haskell.org/Orphan_instance), [C# extension methods](https://learn.microsoft.com/dotnet/csharp/programming-guide/classes-and-structs/extension-methods), [Ruby refinements](https://docs.ruby-lang.org/en/2.5.0/syntax/refinements_rdoc.html), [Apple QA1908](https://developer.apple.com/library/content/qa/qa1908/_index.html)
