# ADR 0152: Protocol Implementations for Unowned Classes

## Status
Proposed (2026-11-24)

If accepted, this supersedes [ADR 0066](0066-open-class-extension-methods.md) in part: its cross-file extension mechanism (§3 dispatch order, §4 conflict detection, §6 load order, §7 ETS tables) and the BT-5002, BT-5060 and BT-5061 lookup-order amendments. ADR 0066's `>>` syntax (§1) stays for same-file methods and for live method definition.

**How to read the evidence.** A claim marked *(read)* was checked by reading the cited code on `main` at `9dcf825`. A claim marked *(inferred)* follows from that reading but was not checked directly. Nothing in this ADR was verified by running the system.

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
| beamtalk-liquid, beamtalk-registry | 0 hits for `>>` in `.bt` files |
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
- Every lookup pays for it. Compiled `has_method/1` checks `beamtalk_extensions:has/2` at each class. The actor and value default dispatch arms call `beamtalk_extensions:lookup/2` (`crates/beamtalk-codegen/src/core_erlang/gen_server/dispatch.rs:769-786`, `dispatch_spec.rs:305-360`). The inherited-send path carries an `{ext, Owner}` verdict (`beamtalk_dispatch.erl:230-298`). BT-4260 and BT-4280 measured this on hot paths; BT-4800 removed double work on the class-side path.

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
| BT-4503 | A value-type extension with `^` in a block answered `{Result, Self}` |
| BT-3951 | A runtime extension overriding a known collection selector defeated a codegen guard |

Tooling carried the same cost: the WS3 epic (BT-4610, with BT-4608 and BT-4611) made dependency extensions visible to consumers, and BT-4658 and BT-4923 found validators that skipped extension bodies.

All of these share one root. **An extension can take precedence over a compiled method**, so every dispatch path, static call, cache and tool must ask "is there an extension that wins here?" The BT-5060 work that merged as #5718 is the latest instance: it adds warning plumbing through eval, load and sync replies to tell the user about a case the dispatch order cannot honour.

### What `>>` is actually used for

`>>` has three uses, and only one of them needs the extension table.

- **Same-file methods.** `Pt >> extra: n => ...` next to `Value subclass: Pt` is folded into the class before analysis (BT-4721). `crates/beamtalk-core/src/compilation/same_file_fold.rs:15-17` says: "nothing registers a same-file extension through `beamtalk_extensions`" *(read)*.
- **Live method definition.** `Foo >> sel => body` at the REPL goes `beamtalk_repl_eval:handle_method_definition` → `beamtalk_repl_loader:reload_method_definition/4` → `reload_class_method_definition` (`beamtalk_repl_loader.erl:238-281`). That reads the class's stored source, merges the method in (`crates/beamtalk-compiler-port/src/handlers/compile_method.rs:243-252`), recompiles the whole class and hot-loads it *(read)*. `Foo class >> sel` takes the same path with `is_class_method = true`. The IDE and MCP `save_method` evaluate `Class compile: #sel source: "..."`, which reaches the same `install_method` recompile path (`beamtalk_repl_loader_methods.erl:85-171`) *(read)*. No workspace source file calls `beamtalk_extensions:register` *(read)*.
- **Cross-file extensions.** Only these use the table. Codegen emits a `beamtalk_extensions:register/5` call in the declaring module's `register_class/0` (`crates/beamtalk-codegen/src/core_erlang/gen_server/extensions.rs:116-143`) *(read)*.

So the first two uses already work without the extension table.

### The use case that remains

One real need is left: making a class you do not own answer a selector. The clearest example is JSON. `Json generate:` calls `asJson` on any value that responds to it (`beamtalk_json.erl:344-350`). BT-2819 shipped `JsonMapping` and the `JsonMappable` protocol (#5717, #5719), but a class opts in with `uses: JsonMappable` in its own body. A class from another package cannot be opted in.

ADR 0066 anticipated this. It rejected protocols as a *replacement* for open classes, and added: "many practical uses of `>>` (JSON serialization, formatting, logging) are protocol-shaped and would be better expressed as protocols once they exist." Protocols exist now (ADR 0068), with provided methods (ADR 0127) and class-side types (ADR 0144).

### Constraints

- Call sites must not change. `x asJson` stays a message send, and `respondsTo: #asJson` must answer `true`.
- Compiled methods must always win, on both sides, with no exception.
- The meaning must be the same in the REPL, `beamtalk run`, `beamtalk test` and a release (`docs/beamtalk-principles.md` §4).
- The package model is ADR 0070: a package is a `beamtalk.toml` `[package] name`. Compiled classes carry their package (`ClassInfo.package`, `class_hierarchy/class_info.rs:333`; `'package'` in `__beamtalk_meta/0`). The stdlib is the package `stdlib` (`class_hierarchy/builtins.rs:175-185`) *(read)*.

## Decision

Replace cross-file extension methods with **protocol implementations**: a top-level declaration that makes a class answer a protocol's selectors, allowed only where it cannot change what an existing method does.

### 1. Syntax

```beamtalk
// app/src/Money+JsonRepresentable.bt
JsonRepresentable implementFor: Money
  asJson => #{"amount" => self cents, "currency" => self currency}
```

The header is `ProtocolName implementFor: ClassName`. The indented body holds method definitions, written as in a class body. A class-side method is prefixed `class`, as in a class or protocol body.

The owner's sketch ended the header with `=>`. This draft drops it. `Protocol define: X` and `Object subclass: X` are the existing declaration headers and neither takes `=>`; `=>` introduces one method body everywhere else. This is a recommendation, and it is listed under Open Questions.

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

### 2. Rule: additive only

An implementation may add selectors. It can never change what an existing method does.

**At compile time:**

- The body may define only selectors the protocol declares (required or provided), on the side the protocol declares them.
- A body selector that the class already defines or inherits as a compiled method, on that side, is a **compile error**. Methods flattened in from a trait (ADR 0127 §3) count as compiled.
- A *provided* selector the class already understands is not installed. The class's method stays. No `excluding:` or `overriding:` is needed, because the provision cannot win.
- After the implementation, the class must conform to the protocol: every required selector resolves, from the body, the class's own methods or the protocol's provisions. Otherwise it is a compile error.
- Two implementations in one build that install the same `{class, side, selector}` are a compile error naming both, as ADR 0066 §4 does today (`crates/beamtalk-core/src/compilation/extension_conflicts.rs`).

```text
error: JsonRepresentable implementFor: Money defines `asJson`, but Money already
       defines `asJson` (money/src/money.bt:14).
  = an implementation can add selectors to a class; it cannot replace one.
  hint: delete this method. Money already conforms to JsonRepresentable.
```

**At run time:**

- Implemented methods live in a **protocol-implementation table**, consulted only when the compiled lookup has missed on the receiver's class and every ancestor, immediately before `doesNotUnderstand:`. The table is then searched from the receiver's class upward, so an implementation for `Collection` answers for `List`.
- A compiled method anywhere in the chain is found first. There is no interleaving, and the class side follows the same order as the instance side.
- If a class later gains a compiled method for an implemented selector (the owner ships a new version, or the class is hot-reloaded), the compiled method answers. The next build of the implementing package fails with the error above. At run time the dead row is reported once as a warning.

This removes the precedence-bug family by construction. No dispatch path, static call or cache has to ask whether something outranks a compiled method, because nothing can.

The cost is real and deliberate: an implementation cannot customise a selector that `Object` or another ancestor already defines. `Printable implementFor: Money` with a `printString` body is an error, because `Money` inherits `printString`.

### 3. Rule: ownership

An implementation is allowed only if the declaring package owns the protocol or the class.

**Definition of "owns".** Package P owns a class or protocol when P is the package that defines it:

- The declaring package is the `[package] name` of the `beamtalk.toml` the file is compiled under.
- A class's owner is its `ClassInfo.package`. A protocol's owner is its `ProtocolInfo.package`. In both, `None` means "the package being compiled" (`protocol_registry.rs:141-145`) *(read)*.
- The stdlib is the package `stdlib`. A user package may implement its own protocol for `String`. It may not implement `stdlib`'s `JsonRepresentable` for another package's `Money`.
- A REPL session and a script outside any project belong to the workspace's package, and the same rule applies.

```text
error: package `app` cannot implement JsonRepresentable (owned by `stdlib`)
       for Money (owned by `money`): it owns neither.
  hint: ask `money` to implement it, or declare a protocol in `app`.
```

**What the rule guarantees, and what it does not.** In Rust the orphan rule gives coherence because a method belongs to its trait. Beamtalk selectors are global, so the rule is weaker here, and the ADR should say so plainly:

- It guarantees that every added selector has a named, owned contract, and that for a given protocol and class there are at most two packages that may implement it.
- It does **not** stop two packages from each declaring their own protocol with `asJson` and implementing it for `String`. The duplicate check in §2 catches that at the consumer's build, but the consumer still cannot fix a clash between two dependencies (the limitation ADR 0070 already records at line 699).
- It blocks the application case: an app that depends on `money` and wants `Money asJson` owns neither side. It can get there by declaring its own protocol with an `asJson` requirement, which the rule permits. That is the same workaround as Rust's extension trait.

Two refinements would make the rule match its intent. Both are left to the owner (Open Questions 1 and 2): an exemption for application packages, and a check that stops a local protocol from re-declaring a selector that a dependency's protocol already declares.

### 4. Rule: declared conformance

An implementation is a declaration that the class conforms.

- **Type checker.** The implementation's selectors join the class's method surface, and the class conforms to the protocol, so a `Money` is accepted where `:: JsonRepresentable` is expected. Conformance in Beamtalk is structural (ADR 0068), so this mostly follows from the selectors being visible. The new part is the completeness check in §2.
- **Runtime.** `Money conformsTo: #JsonRepresentable` answers `true`. `conforms_to/2` (`beamtalk_protocol_registry.erl:433`) uses `classCanUnderstandFromName`, which will consult the implementation table after the compiled walk.
- **Reflection.** Added beside the ADR 0127 §12 surface: `Protocol implementationsOf: #JsonRepresentable` (the classes with a declared implementation) and `Behaviour implementedProtocols`. `CompiledMethod origin` answers the protocol, as it does for a flattened trait method. `usersOf:` stays reserved for `uses:`.
- **Browse.** Implemented methods are grouped under the protocol's name with a new `provenance := implementation` row, replacing the `extension → "extensions"` branch (`protocol_from_source/4`, `beamtalk_repl_ops_browse_origin.erl:275`).
- **Xref.** `SystemNavigation extendersOf:` and `extensionsBy:` are replaced by queries over implementations *(inferred: the exact selectors are an implementation choice)*.

### 5. Rule: call sites unchanged

`x asJson` is an ordinary send. `respondsTo:`, `canUnderstand:`, `perform:` and `super` see an implemented method, after compiled methods. The `respondsTo: #asJson` hook in `beamtalk_json.erl:344-350` needs no change.

```beamtalk
>> (Money cents: 250 currency: "EUR") asJson
=> #{"amount" => 250, "currency" => "EUR"}
>> Money conformsTo: #JsonRepresentable
=> true
>> (Money >> #asJson) origin
=> #JsonRepresentable
```

### 6. Class-side requirements

`implementFor:` supports the protocol's class-side requirements (BT-1611, ADR 0144), written `class selector => body`. The same rules apply on the class side: additive only, miss-only lookup after the class-method chain. After the implementation the class conforms on both sides, so it is accepted where `P class` is expected.

A class-side implemented method is compiled as today's class-side extension fun, `fun(Args, ClassSelf) -> Result` (ADR 0130 §3). It is stateless from the class's point of view, so under ADR 0142 it runs in the caller.

Class-side *provisions* are still post-v1 in ADR 0127 §1; this ADR does not change that.

### 7. What happens to `>>`

| Form | After this ADR |
|---|---|
| `Foo >> sel => ...` in the file that defines `Foo` | Unchanged. Folded into the class (BT-4721). |
| `Foo >> sel => ...` at the REPL, `Class compile:source:`, IDE and MCP `save_method` | Unchanged. Recompiles the class from its stored source and hot-loads it. |
| `Proto >> sel => ...` at the REPL (ADR 0127 §11) | Unchanged. Patches the provision and re-expands users. |
| `Foo >> #sel` reflection | Unchanged. |
| `Foo >> sel => ...` in a file that does not define `Foo` | Removed. A compile error with a hint to use `implementFor:`. |
| `pkg@Foo >> sel => ...` | Removed with it. |

The live forms work only for a class whose source the workspace holds. For a stdlib or dependency class, `reload_class_method_definition` answers "Class source not available" (`beamtalk_repl_loader.erl:241-246`), and `compile_method/7` refuses a stdlib class outright (`beamtalk_repl_eval.erl:1062-1073`) *(read; not run)*. So `Integer >> double => self * 2`, the REPL example in ADR 0066 §1, already fails today as far as the code shows. After this ADR the answer for that case is an implementation, which can be typed at the REPL like any declaration.

### 8. Hot reload and removal

- **Reloading the implementing file** replaces the rows that module owns. A method deleted from the file stops answering. Today a deleted cross-file extension "keeps answering until `removeSelector:` or a restart" (`docs/beamtalk-language-features.md` § Cross-file extensions).
- **Re-evaluating an implementation** from the same package replaces it. An implementation of the same `{class, side, selector}` from a different package is refused with a structured error, and the first one stays. There is no last-writer-wins.
- **Reloading the class** changes nothing unless the class now compiles an implemented selector. Then the compiled method answers, and a warning names the dead implementation row.
- **Editing the protocol** (a provision or a requirement) re-expands implementations with the users, under ADR 0127 §11's two-stage rule: compile everything, then load.
- **`Money removeSelector: #asJson`** on an implemented selector is an error, as it is for a trait-provided selector (`beamtalk_class_refactor.erl:267-276`): "`asJson` is implemented for Money by `JsonRepresentable implementFor: Money` (package `app`); remove the implementation instead." Removing one selector would leave a declared conformance that is false.
- **Removing an implementation** is one operation on the whole unit: `Protocol removeImplementationOf: #JsonRepresentable for: #Money` (spelling under Open Questions). It records a ChangeLog entry as extension removal does today.
- **`removeFromSystem`** on the class or the protocol purges its implementation rows, as `purge_extensions` does today (`beamtalk_class_lifecycle.erl:127-132`).

### 9. Patching a running class method

BT-5061 kept the class-side "extension first" order because a class-side extension was "the documented way to patch a class method of a running class". The code shows what that covers today:

- For a class with source in the workspace, `Foo class >> greet => ...` at the REPL does not create an extension. It recompiles `Foo` with the new class method (`beamtalk_repl_loader.erl:268-279`) *(read)*. This is unaffected.
- The extension-first order therefore matters only for a class-side extension loaded from a file (or registered through FFI) onto a class the workspace has no source for.
- `install_method/9` supports the class side internally, but MCP `save_method` and `Class compile:source:` reach it on the instance side only (`beamtalk_class_refactor.erl:1208-1210`; `SaveMethodParams` has no side field) *(read)*.

After this ADR, replacing a compiled class method of a class without stored source is no longer possible through an extension. What remains is the ADR 0084 runtime class-method fun, which sits before compiled class methods and is a separate store (`beamtalk_object_class.erl:691-710`), and redeploying the class. Whether that is enough is Open Question 4.

### Single source of truth

- **Runtime.** The implementation table belongs in `beamtalk_protocol_registry.erl`, which already owns the protocol ETS tables, the `uses:` indexes and the conformance cache. It is not a new module beside `beamtalk_extensions.erl`.
- **Compiler.** The implementation index replaces `ExtensionIndex` (`compilation/extension_index.rs`) and feeds `ClassHierarchy` through the path `register_extensions` uses today (`class_hierarchy/mod.rs:835-900`). The ownership check reads `ClassInfo.package` and `ProtocolInfo.package`; it adds no second notion of package.
- **Rust and Erlang agreement.** The "compiled first, then the implementation table, then DNU" order is stated once in `stdlib/test/lookup_order_matrix_test.bt`, which already compares literal sends, `perform:`, `respondsTo:` and `super` against one reference order. The matrix keeps that role with far fewer rows.

## Prior Art

| Language | Mechanism | Who may add | On a clash |
|---|---|---|---|
| Rust | `impl Trait for Type` | The crate that owns the trait or the type (orphan rule, RFC 2451) | Compile error. Cost: the newtype and extension-trait workarounds |
| Swift | `extension T: P` | Anyone. Since SE-0364 (Swift 6) a warning unless marked `@retroactive` | Duplicate conformances are not reliably diagnosed across modules (from memory, not checked against the proposal) |
| Elixir | `defimpl P, for: T` | Anyone | Both impls compile to module `P.T`; the last loaded wins with a "redefining module" warning (forum reports) |
| Clojure | `extend-type`, `extend-protocol` | Anyone. Official guidance: a library should extend only if it owns the protocol or the type; an application may do more | Last loaded wins |
| Haskell | Class instances | Anyone. An orphan instance warns under `-Worphans` | Ambiguity when both are in scope. PureScript forbids orphans |
| Kotlin, C# | Extension functions and methods | Anyone. Resolved statically from the declared type, in lexical scope | The member always wins, silently |
| Ruby | Open classes; refinements | Anyone | Last definition wins. Refinements are lexical and are not seen by `respond_to?` |
| Pharo | `*Package` extension protocols | Anyone. The package owns the method | Last loaded wins. Overrides are a known packaging problem |
| Objective-C | Categories | Anyone | Undefined which implementation runs (Apple QA1908) |
| Newspeak | None | — | No global namespace to extend |
| Gleam, Erlang | None | — | — |

What this ADR takes:

- **From Rust:** the ownership rule. Its known cost, the application that owns neither side, is the subject of Open Question 1.
- **From Clojure:** the same rule as written guidance, and the observation that applications are a different case from libraries.
- **From Kotlin and C#:** "the member always wins". Those languages apply it silently, so a library that later adds a member changes which code runs without a diagnostic. This ADR makes the same event a compile error at the next build.
- **From Swift:** conformance declared outside the type, with the type then usable wherever the protocol is expected.
- **From Pharo:** the method is globally visible and package-owned, and reflection sees it.

What it rejects: last-writer-wins (Pharo, Ruby, Clojure, Elixir) and lexical scoping (Kotlin, C#, Scala, Ruby refinements), which cannot be seen by a dynamic send.

## User Impact

**Newcomer.** One more declaration form to learn, and it reads as a sentence. The errors say which rule was broken and what to write instead. The surprising part will be that `printString` cannot be implemented for someone else's class.

**Smalltalk developer.** This is a departure. Pharo lets any package add or override any method on any class. Here an addition needs a protocol and cannot override. In exchange, loading a package cannot change the behaviour of code that worked before it loaded. Live patching through `Foo >> sel => ...` is unchanged for classes with source.

**Erlang/BEAM developer.** The model is close to Elixir's `defimpl`, with the method reachable as an ordinary send. Implemented methods are plain functions in the declaring module, and dispatch reaches them only on a miss, which is easy to trace.

**Production operator.** Loading a package can no longer change the result of an existing send. The per-level extension lookup leaves the send path. Hot reload of an implementing module removes deleted methods.

**Tooling developer.** An implementation is a block with a protocol, a class and method signatures, so it fits the indexes that already hold classes and protocols. The `dependency_extensions_unknown` guard and the separate extension index go away or shrink.

## Steelman Analysis

### Keep ADR 0066 open classes
- 🧑‍💻 **Newcomer**: "`String >> shout => ...` is one line. I don't want to name a protocol to add one method."
- 🎩 **Smalltalk purist**: "Open classes are Smalltalk. Every Pharo package extends `Object` and `String`."
- ⚙️ **BEAM veteran**: "It's built and tested. The matrix test pins the order."
- 🏭 **Operator**: "I can hot-patch a class method on a running node from a file, without its source."
- 🎨 **Language designer**: "It covers both axes of the expression problem with one construct."

### Additive-only `>>` without protocols
- 🧑‍💻 **Newcomer**: "Same one-liner as before, and I get an error instead of a surprise when it clashes."
- 🎩 **Smalltalk purist**: "The syntax is unchanged; only overriding goes."
- ⚙️ **BEAM veteran**: "The runtime change is identical: a miss-only table."
- 🏭 **Operator**: "Same safety at run time."
- 🎨 **Language designer**: "It fixes the precedence bugs, which are the actual cost, with the smallest change. The ownership rule is weak anyway (§3), so why pay for protocols?"

### Lexically scoped static extension functions (Kotlin, C#)
- 🧑‍💻 **Newcomer**: "I know this from Kotlin."
- ⚙️ **BEAM veteran**: "It compiles to a direct call. No table, no dispatch change."
- 🏭 **Operator**: "Nothing global changes when a package loads."
- 🎨 **Language designer**: "No clash is possible: two packages' extensions never meet."

### Pure protocol dispatch (Elixir): `Json encode: x`
- 🧑‍💻 **Newcomer**: "It's obvious where the code lives: in the protocol."
- 🎩 **Smalltalk purist**: "`Json encode: x` is still a message send, to the service that owns the operation."
- ⚙️ **BEAM veteran**: "This is `defimpl`. Nothing enters the class's lookup, so nothing can go wrong in it."
- 🏭 **Operator**: "The dispatch path has no extension table at all, not even on a miss."
- 🎨 **Language designer**: "Implementations are keyed by `{protocol, class}`, so the ownership rule gives real coherence. Two packages' `asJson` cannot collide because there is no shared selector."

### Tension points
- The designer's argument for pure protocol dispatch is the strongest one against this ADR. Keying by `{protocol, class}` is what makes the orphan rule sound, and this ADR keys by `{class, selector}` to keep `x asJson`.
- Newcomers and Smalltalk developers want the one-line form; the usage numbers say nobody has needed it.
- Operators lose file-based patching of a class without source, and gain a guarantee that loading code cannot change existing sends.

## Alternatives Considered

### Keep ADR 0066 open classes as they are
Rejected. The feature has no users outside its own tests, about 52 Erlang and 111 Rust files carry it, and its lookup order has produced roughly ten precedence bugs in 90 days, the last of which (BT-5060) was closed by adding warnings instead of fixing the order. The cost recurs with every new dispatch optimisation, because each one must prove that no extension outranks the method it resolved.

### Additive-only `>>` without protocols
Keep `String >> shout => ...` across files, but make a clash with a compiled method an error and consult the table only on a miss.

This gets the whole runtime simplification, and it deserves to be taken seriously for that reason. Rejected because:

- It has nothing to attach ownership to. "You may add a selector to a class you don't own if you own …" needs a second thing to own, and the protocol is that thing.
- It gives no declared conformance. The type checker could see the selector, but there would be no unit that says "Money is JsonRepresentable, completely", and no completeness check.
- It has no unit of removal, browsing or documentation, so tools would keep an "extensions" bucket per class.
- It cannot carry provided methods, so it does not close the `JsonMappable` gap.

The cost of choosing protocols is ceremony for a one-off method on one class. ADR 0066 named that case (`Integer >> factorial`) as the reason to keep open classes. The usage table shows no such code in practice.

### Lexically scoped static extension functions (Kotlin, C#)
`x shout` resolves at compile time, from the static type of `x`, to a function imported into the file.

Rejected because Beamtalk sends are dynamic and the static type is often `Dynamic`. ADR 0127 rejected "extension methods on a protocol" for the same reason. Concretely:

- `respondsTo: #asJson` would answer `false`, so the `Json generate:` hook would not find the method.
- `perform:`, a send through a `Dynamic` receiver, and a send from another file would not find it either.
- The REPL has no lexical import scope to resolve against, so the same expression would mean different things in a file and at the prompt (principles §4).
- Ruby refinements are the dynamic-language attempt at this, and they have exactly these gaps with `respond_to?`.

### Pure protocol dispatch: `Json encode: x`, nothing enters the class's lookup
Implementations are keyed by `{protocol, class}` and reached only through the protocol: `JsonRepresentable asJson: x`, or a facade such as `Json encode: x`.

This is the cleanest design at run time and the only one where the ownership rule is fully sound. It was weighed seriously. Rejected because:

- **It loses `x asJson`.** A class that writes `asJson` in its body and a class that has it implemented from outside would be called differently, or every caller would have to use the protocol form. Either the two populations are not interchangeable, or the language grows a second call syntax for every protocol-shaped operation.
- **It breaks the existing contract.** `Json generate:` finds the hook with `respondsTo: #asJson` and a send. `JsonMappable`'s provision is `asJson => self class jsonMapping encode: self`. Collections of mixed objects are sent `asJson` uniformly. All of that would have to become protocol dispatch.
- **It is a second dispatch mechanism.** Beamtalk's object model has one question, "does the receiver understand this message?". Pure protocol dispatch adds "does this protocol have an implementation for the receiver's class?", with its own inheritance walk, reflection and tooling.
- **Structural conformance would split.** A class conforming by having the method and a class conforming by external implementation would not both pass `conformsTo:` by the same test.

What this ADR gives up in return is real: coherence is by `{class, selector}`, so the ownership rule is weaker than Rust's (§3). The duplicate check and the additive rule are the backstop. If selector clashes between unrelated protocols turn out to be common, this alternative is the one to revisit, and nothing in this ADR prevents adding `Protocol send:to:`-style qualified dispatch later.

### Do it by convention only
Deprecate cross-file `>>` in documentation and leave the runtime. Rejected: the cost is in the dispatch path and the tools, not in user code.

## Consequences

### Positive
- A compiled method always wins, on both sides, at every depth. The lookup order becomes "compiled chain, then implementations, then DNU".
- The BT-5002, BT-5060, BT-5061 class of bug cannot recur.
- Loading a package cannot change the behaviour of an existing send.
- The per-level extension lookup leaves `has_method/1` and the default dispatch arms.
- A class from another package can be made `JsonMappable` or `JsonRepresentable`.
- A class that gains a method its users had implemented produces a build error at the implementer, where it can be fixed.
- Deleting a method from an implementing file takes effect on reload.

### Negative
- Adding one method to one foreign class now needs a protocol declaration.
- An implementation cannot customise an inherited selector such as `printString`, `=` or `hash`.
- The ownership rule does not give Rust-level coherence, because selectors are global (§3).
- An application that owns neither the protocol nor the class must declare its own protocol, unless Open Question 1 is accepted.
- File-based replacement of a compiled class method on a class without stored source is lost (§9).
- A send of an implemented selector takes the whole compiled walk before it reaches the table. It is slower than a compiled method unless the verdict is memoised in `beamtalk_class_derived_cache`, which the inherited-send path already does for its own verdicts.
- The tests that use cross-file extensions (19 files by this ADR's count; the owner's survey counted 33 uses) and the `beamtalk-uat` fixture need rewriting or deleting. A further 23 `stdlib/test` files call `beamtalk_extensions` through FFI.
- This is a breaking language change, with a deprecation period.

### Neutral
- The class-derived cache and its generation read stay. BT-4984 kept that read partly so extension changes show on the next send; hot reload still needs it. This ADR removes one reason for it, not the read.
- ADR 0084 runtime class-method funs are unchanged.
- `StandaloneMethodDefinition` stays in the AST for the same-file and live forms.
- Implementations cannot add state, as extensions could not.

## Implementation

### Phase 1: add `implementFor:`
- **Parser and AST.** A `ProtocolImplementation { protocol, class, package qualifiers, methods, class_methods, span }` node in `crates/beamtalk-core/src/ast/class.rs`, parsed beside `Protocol define:` *(inferred: the parser must tell the declaration from a keyword send by the indented body, as it does for class and protocol headers)*.
- **Semantic analysis.** An implementation index; the additive, completeness, duplicate and ownership checks; provisions expanded through `trait_expansion.rs`; signatures registered with `ClassHierarchy`. Body validators run on implementation bodies from the start (the BT-4658 and BT-4923 lesson).
- **Codegen.** Method funs reuse the foreign-extension shapes in `gen_server/extensions.rs:27-34`. Registration is emitted in `register_class/0` with the `Document` API.
- **Runtime.** The implementation table and its API in `beamtalk_protocol_registry.erl`; the miss-path lookup before DNU on the instance and class sides; `respondsTo:` and `canUnderstand:` after the compiled walk; the refusal of a different-owner duplicate.
- **Reflection, browse, xref, LSP.** As in Decision §4. Document symbols, go-to-definition, hover and completion for the new block.
- **Dependencies.** Implementations travel in the dependency interface with classes and protocols, replacing `DependencyExtensions` (`crates/beamtalk-project/src/dependency_extensions.rs`) *(inferred)*. This is what WS3 becomes: the same visibility, carried as declarations in the package interface instead of a separate extension channel.
- **Docs.** `docs/beamtalk-language-features.md`, `docs/development/surface-parity.md`.

During this phase both mechanisms exist. An implemented selector and an extension on the same `{class, selector}` is a compile error.

### Phase 2: deprecate cross-file `>>`
- A warning on every cross-file `>>`, with the `implementFor:` rewrite in the hint.
- Migrate the 11 `stdlib/test` files, 3 `tests/parity` files and 5 `.btscript` cases. Tests that pin extension precedence are deleted with the behaviour they pin.
- Decide Open Questions 4 and 5 before Phase 3, because they bound what is lost.

### Phase 3: remove
Cross-file `>>` becomes a compile error. Then delete:

| Piece | Where |
|---|---|
| The extension registry: three ETS tables, last-writer-wins, the conflicts bag | `beamtalk_extensions.erl` (913 lines) |
| Interleaved lookups | `beamtalk_dispatch.erl`: the extension checks in `lookup/5`, `super/5` and `lookup_in_class_chain/5`; the `{ext, Owner}` verdict; `inherited_value_ext/5`; `check_extension/2` |
| Extension probes in generated code | `beamtalk_extensions:lookup` in the actor and value default arms and the `beamtalk_extensions:has` step of `has_method/1` (`gen_server/dispatch.rs`, `dispatch_spec.rs`, `supervisor_codegen.rs:239`) |
| The one-hop gap and its warnings (BT-5060, #5718) | `one_hop_children_missing_extension/2`, `one_hop_parent_extensions_missing/1` (`beamtalk_dispatch.erl:325-398`); `warn_one_hop_children/2`, `warn_one_hop_for_class/1`, `emit_child_warning/3`, `emit_parent_warning/3` (`beamtalk_extensions.erl:311-410`); `warn_one_hop_gaps/1` (`beamtalk_repl_loader_activate.erl:152-165`); the `bt5060_live_*` fixtures |
| Class-side extension-first order | `class_side_extension/2` and its callers in `beamtalk_class_dispatch.erl`; the constructor-extension path in `beamtalk_object_class.erl:1881-1911` |
| The `extension` shadow-flag kind | `beamtalk_class_shadow_flags.erl`. The `runtime_fun` and `class_vars` kinds stay |
| Guards against an extension overriding a known selector | `beamtalk_primitive.erl:1000-1010`, `beamtalk_equality.erl:127-138`, `beamtalk_collection_selectors.erl:90-127`, `beamtalk_default_method:note_override/2` *(inferred: each needs checking that the extension case is its only reason)* |
| Compiler pieces | `extension_conflicts.rs`, `extension_index.rs`, `register_extensions`, the `dependency_extensions_unknown` guard, the `pkg@Target >>` qualifier |

The reply-warning scope added for BT-5060 (`with_extension_warning_scope/2` in `beamtalk_class_registry.erl`) is deleted unless the "dead implementation row" warning of §8 reuses it *(an implementation decision)*.

`one_hop_selectors` in `value_type_codegen/dispatch.rs:298-330` routes selectors the direct superclass compiles to that superclass's `dispatch/3`. It no longer has a gap, because no implementation can exist for a selector the superclass compiles.

## Migration Path

A cross-file extension that adds a new selector becomes a protocol and an implementation:

```beamtalk
// Before
String >> shout => self uppercase ++ "!"

// After
Protocol define: Shoutable
  shout -> String

Shoutable implementFor: String
  shout => self uppercase ++ "!"
```

A cross-file extension that overrides a compiled or inherited method has no equivalent. If you own the class, move the method into it. If you do not, subclass or wrap.

Outside this repository the only known use is the `beamtalk-uat` fixture, which tests the feature being removed. The Phase 2 warning is the real check for packages this survey could not see.

## Open Questions

1. **Application exemption.** Should the root package of a build be allowed to implement a protocol for a class when it owns neither? Nothing depends on an application, so it cannot cause a clash someone else has to resolve, and Clojure's guidance draws this line. `beamtalk.toml` already separates applications (`[application]`, `beamtalk new --app`) from libraries. *Recommendation: yes, with an error when an orphan implementation is found in a dependency.* Without it the motivating case (an app adding `asJson` to a third-party class) needs a throwaway protocol.
2. **Selector laundering.** Should a package be stopped from declaring a local protocol that re-declares a selector a dependency's protocol already declares (`asJson`), and implementing it for a foreign class? Without this, the ownership rule can be sidestepped; with it and without question 1, applications are stuck. *Recommendation: decide 1 and 2 together.*
3. **Header syntax.** `P implementFor: C` with an indented body (this draft), or with a trailing `=>` (the original sketch)? Also the removal selector in §8.
4. **Patching a class method without source.** Is the ADR 0084 runtime fun enough to replace file-based class-side extensions for patching a running class (§9)? Should MCP `save_method` and `Class compile:source:` gain a class-side form, since `install_method/9` already supports it?
5. **Dependency classes at the REPL.** `Foo >> sel => ...` fails for a dependency class because no source is stored (`beamtalk_workspace_bootstrap.erl:121-128`). Should the workspace store dependency sources, or is "use an implementation" the answer?
6. **Slot access.** May an implementation body read or write `self.field` on a class its package does not own? Today's foreign extensions can. *Default in this draft: unchanged.* Restricting it to the class's own package would make an implementation a client of the public interface only.
7. **REPL strictness.** Does the ownership rule apply at the REPL, or is an interactive session exempt like an application (question 1)?
8. **`methods` and `includesSelector:`.** Do they list implemented selectors? Extensions are listed today; flattened trait methods are local. *Default: listed, with `origin`.*
9. **Deprecation length.** How many releases between the Phase 2 warning and the Phase 3 error, given no known external users?

## References
- Related issues: BT-2819 (`JsonMapping`, `JsonMappable`; #5717, #5719), BT-5060 (#5718), BT-5002, BT-5061, BT-5062, BT-4307, BT-4588, BT-4250, BT-4133, BT-4503, BT-3951, BT-4260, BT-4280, BT-4800, BT-4984, BT-4721, BT-4610, BT-4608, BT-4611, BT-4658, BT-4923
- Related ADRs: [ADR 0066](0066-open-class-extension-methods.md) (superseded in part), [ADR 0068](0068-parametric-types-and-protocols.md) (structural protocols), [ADR 0070](0070-package-namespaces-and-dependencies.md) (packages), [ADR 0084](0084-class-side-runtime-method-fun-dispatch.md) (runtime class-method funs), [ADR 0127](0127-traits-for-behavioural-reuse.md) (traits), [ADR 0130](0130-class-variables-live-in-the-class-process.md) (class-side fun shape), [ADR 0142](0142-class-process-owns-only-class-state.md) (where class methods run), [ADR 0144](0144-class-side-protocol-types.md) (class-side protocol types)
- Code: `runtime/apps/beamtalk_runtime/src/beamtalk_extensions.erl`, `beamtalk_dispatch.erl`, `beamtalk_protocol_registry.erl`; `runtime/apps/beamtalk_workspace/src/beamtalk_repl_loader.erl`; `crates/beamtalk-core/src/compilation/same_file_fold.rs`, `extension_index.rs`; `crates/beamtalk-codegen/src/core_erlang/gen_server/extensions.rs`; `stdlib/test/lookup_order_matrix_test.bt`
- Prior art: [Rust RFC 2451](https://rust-lang.github.io/rfcs/2451-re-rebalancing-coherence.html), [Swift SE-0364](https://github.com/apple/swift-evolution/blob/main/proposals/0364-retroactive-conformance-warning.md), [Elixir `Protocol`](https://hexdocs.pm/elixir/Protocol.html), [Clojure protocols](https://clojure.org/reference/protocols), [Haskell orphan instances](https://wiki.haskell.org/Orphan_instance), [C# extension methods](https://learn.microsoft.com/dotnet/csharp/programming-guide/classes-and-structs/extension-methods), [Ruby refinements](https://docs.ruby-lang.org/en/2.5.0/syntax/refinements_rdoc.html), [Apple QA1908](https://developer.apple.com/library/content/qa/qa1908/_index.html)
