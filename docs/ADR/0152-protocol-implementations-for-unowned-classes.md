# ADR 0152: Protocol Implementations for Unowned Classes

## Status
Proposed (2026-11-24)

If accepted, this supersedes [ADR 0066](0066-open-class-extension-methods.md) in part: its cross-file extension mechanism (§3 dispatch order, §4 conflict detection, §6 load order, §7 ETS tables) and the BT-5002, BT-5060 and BT-5061 lookup-order amendments. ADR 0066's `>>` syntax (§1) stays for same-file methods and for live method definition.

**How to read the evidence.** A claim marked *(read)* was checked by reading the cited code on `main` at `9dcf825`. A claim marked *(inferred)* follows from that reading but was not checked directly. A claim marked *(reviewer)* was reported by the adversarial review pass and spot-checked, not read in full. Nothing in this ADR was verified by running the system.

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

### The use case that remains

One real need is left: making a class you do not own answer a selector. The clearest example is JSON. `Json generate:` calls `asJson` on any value that responds to it (`beamtalk_json.erl:344-350`). BT-2819 shipped `JsonMapping` and the `JsonMappable` protocol (#5717, #5719), but a class opts in with `uses: JsonMappable` in its own body. A class from another package cannot be opted in.

ADR 0066 anticipated this. It rejected protocols as a *replacement* for open classes, and added: "many practical uses of `>>` (JSON serialization, formatting, logging) are protocol-shaped and would be better expressed as protocols once they exist." Protocols exist now (ADR 0068), with provided methods (ADR 0127) and class-side types (ADR 0144).

### Constraints

- Call sites must not change. `x asJson` stays a message send, and `respondsTo: #asJson` must answer `true`.
- Compiled methods must always win, on both sides, with no exception.
- The meaning must be the same in the REPL, `beamtalk run`, `beamtalk test` and a release (`docs/beamtalk-principles.md` §4).
- The package model is ADR 0070: a package is a `beamtalk.toml` `[package] name`. Compiled classes carry their package (`ClassInfo.package`, `class_hierarchy/class_info.rs:333`; `'package'` in `__beamtalk_meta/0`). Generated stdlib classes are stamped `stdlib` *(read)*.
- `ClassInfo.package` is `None` for "REPL classes and bootstrap builtins" (`class_info.rs:330-333`), for example `Future`. `ProtocolInfo.package` is `None` for the package being compiled (`protocol_registry.rs:141-145`). The two `None`s mean different things *(read)*.
- Only protocols with provided methods, and protocols named like a stdlib trait, are carried into other files' and packages' compiles today (`is_carried_protocol`, `trait_expansion.rs:567`) *(read)*. A requirements-only protocol such as `JsonRepresentable` may therefore reach a consumer without its requirement list or its owner *(reviewer)*.
- Live patching is a message send (`docs/beamtalk-principles.md` §11). Anything this ADR removes from that surface is a regression against a stated principle and has to be named.

## Decision

Replace cross-file extension methods with **protocol implementations**: a top-level declaration that makes a class answer a protocol's selectors, allowed only where it cannot change what an existing method does.

### 1. Syntax

```beamtalk
// app/src/Money+JsonRepresentable.bt
JsonRepresentable implementFor: Money
  asJson => #{"amount" => self cents, "currency" => self currency}
```

The header is `ProtocolName implementFor: ClassName`. The indented body holds method definitions, written as in a class body. A class-side method is prefixed `class`, as in a class or protocol body.

The owner's sketch ended the header with `=>`. This draft drops it. `Protocol define: X` and `Object subclass: X` are the existing declaration headers and neither takes `=>`; `=>` introduces one method body everywhere else. This is a recommendation, and it is Open Question 2.

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
- Rows are registered by class *name*. The class need not be loaded yet, because module activation orders by superclass only *(reviewer)*. When a class activates, the runtime re-runs the additive check for its rows, as `warn_one_hop_for_class/1` does for extensions today.
- If a class gains a compiled method for an implemented selector (the owner ships a new version, or the class is hot-reloaded), the compiled method answers. The next build of the implementing package fails with the error above. At run time the dead row is reported once as a warning.
- **A run-time clash does not depend on load order.** If two packages that were never built together register clashing rows (possible in a REPL or after `:sync`), neither answers: the send raises a structured `ambiguous_implementation` error naming both, until one is removed. The registration is also reported in the reply warnings.

**The guarantee, stated exactly.** Loading a package cannot change the result of a send that a compiled method answers. It can make a send answer that used to raise `does_not_understand`, which is the purpose, so code that branches on `respondsTo:` or catches DNU can observe it. It cannot change which implementation another package's send reaches, because of the clash rule.

This removes the precedence-bug family by construction. No dispatch path, static call or cache has to ask whether something outranks a compiled method, because nothing can.

The cost is real and deliberate: an implementation cannot customise a selector that `Object` or another ancestor already defines. `Printable implementFor: Money` with a `printString` body is an error, because `Money` inherits `printString`.

### 3. Rule: ownership

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

This needs one resolver, written once, because `None` in `ClassInfo.package` does not mean "the current package" (Constraints). It also needs every protocol's signatures and owner to reach consumers, not only protocols with provided methods; that is a Phase 1 prerequisite.

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
- What actually prevents two packages from both adding `String asJson` is the clash rule in §2, at the consumer's build. The consumer still cannot repair a clash between two dependencies (the limitation ADR 0070 already records at line 699).

As written, then, the ownership rule costs ceremony and protects little. Open Question 1 asks the owner to pick one of three coherent positions: make the rule selector-based with a root-package exemption, keep it as stated, or reduce it to a lint.

### 4. Rule: declared conformance

An implementation is a declaration that the class conforms.

- **Type checker.** The implementation's selectors join the class's method surface, and the class conforms to the protocol, so a `Money` is accepted where `:: JsonRepresentable` is expected. Conformance in Beamtalk is structural (ADR 0068), so this mostly follows from the selectors being visible. The new part is the completeness check in §2. A compiled signature always beats an implementation's signature, on both sides. Implemented methods are tagged as such in the class's method table, so static calls, guarded direct calls and sealed self-sends never target them (BT-4133 is the precedent for what goes wrong otherwise).
- **Runtime.** `Money conformsTo: #JsonRepresentable` answers `true`. `conforms_to/2` (`beamtalk_protocol_registry.erl:434`) uses `classCanUnderstandFromName`, which will consult the implementation table after the compiled walk.
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

A class-side implemented method is compiled as today's class-side extension fun, `fun(Args, ClassSelf) -> Result` (ADR 0130 §3). It runs where a class method of the target class runs under ADR 0142: in the caller, unless the class or an ancestor declares class variables or the selector is process-bound, in which case it runs in the class process. That is what a class-side extension does today (ADR 0142, BT-4307).

Class-side *provisions* are still post-v1 in ADR 0127 §1; this ADR does not change that.

### 6a. What an implementation body may do

- **Public interface only.** A body may send messages to `self`. It may not read or write `self.field`, and a class-side body may not touch the target's class variables. This is the draft's default and is Open Question 5. The reasons: the implementer usually does not own the class, a field rename in the owner's minor release would otherwise break the implementation at run time with no build error, and ADR 0127 §7 already makes provided methods stateless.
- **Actors.** An implemented method on an `Actor` class runs inside the actor's process, as a foreign extension does today, and its `self` sends are same-process sends. It keeps today's fun shape, `fun(Args, Self, State) -> {Result, NewState}` (`gen_server/extensions.rs:27-34`), because a self-send can change state even when the body cannot touch slots. `JsonMappable`'s provision relies on same-process self-sends.
- **`^` and blocks.** Bodies are compiled by the existing extension-body codegen. That code has its own bug history (BT-4503, BT-3920), which this ADR does not remove. Phase 1 must run the `^`-in-block and error-propagation cases against value, actor and class-side implementations.

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
- **Deleting the implementing file** and running `:sync` purges the rows of a module that is gone.
- **Re-evaluating an implementation** from the same package replaces it. A clashing implementation from a different package makes the selector ambiguous (§2). There is no last-writer-wins and no first-writer-wins.
- **Reloading the class** changes nothing unless the class now compiles an implemented selector. Then the compiled method answers, and a warning names the dead implementation row.
- **Editing the protocol** (a provision or a requirement) re-expands implementations with the users, under ADR 0127 §11's two-stage rule: compile everything, then load.
- **`Money removeSelector: #asJson`** on an implemented selector is an error, as it is for a trait-provided selector (`refuse_if_protocol_provided/4`, `beamtalk_class_refactor.erl:267-276`): "`asJson` is implemented for Money by `JsonRepresentable implementFor: Money` (package `app`); remove the implementation instead." Removing one selector would leave a declared conformance that is false.
- **Removing an implementation** is one operation on the whole unit: `Protocol removeImplementationOf: #JsonRepresentable for: #Money` (spelling: Open Question 2). It records a ChangeLog entry as extension removal does today.
- **`removeFromSystem`** on the class or the protocol purges its implementation rows, as `purge_extensions` does today (`beamtalk_class_lifecycle.erl:127-132`).

### 9. Patching a running class method

BT-5061 kept the class-side "extension first" order because a class-side extension was "the documented way to patch a class method of a running class". The code shows what that covers today:

- For a class with source in the workspace, `Foo class >> greet => ...` at the REPL does not create an extension. It recompiles `Foo` with the new class method (`beamtalk_repl_loader.erl:268-279`) *(read)*. This is unaffected.
- The extension-first order therefore matters only for a class-side extension loaded from a file (or registered through FFI) onto a class the workspace has no source for.
- `install_method/9` supports the class side internally, but MCP `save_method` and `Class compile:source:` reach it on the instance side only (`beamtalk_class_refactor.erl:1208-1210`; `SaveMethodParams` has no side field) *(read)*.

After this ADR, replacing a compiled class method of a class without stored source is no longer possible through an extension. What remains is the ADR 0084 runtime class-method fun, which sits before compiled class methods and is a separate store (`put_class_method`, `beamtalk_object_class.erl:691`), and redeploying the class.

This is a regression against principle 11 ("Live Patching is a Message Send"), not only a tooling gap. Phase 3 is therefore gated on Open Question 3: either the workspace can patch a dependency class (stored dependency source, or an equivalent), or the owner accepts the loss explicitly.

### Single source of truth

- **Runtime.** The implementation table belongs in `beamtalk_protocol_registry.erl`, which already owns the protocol ETS tables, the `uses:` indexes and the conformance cache. It is not a new module beside `beamtalk_extensions.erl`.
- **Compiler.** The implementation index replaces `ExtensionIndex` (`compilation/extension_index.rs`) and feeds `ClassHierarchy` through the path `register_extensions` uses today (`class_hierarchy/mod.rs:850`), with its class-side rule inverted: today a class-side extension replaces the compiled signature (BT-5061, BT-5111); an implementation never does. The ownership check goes through one owner resolver (§3) over `ClassInfo.package` and `ProtocolInfo.package`; it adds no second notion of package.
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

- **From Rust:** the ownership rule. Its known cost, the application that owns neither side, and its weakness here, are the subject of Open Question 1.
- **From Clojure:** the same rule as written guidance, and the observation that applications are a different case from libraries.
- **From Kotlin and C#:** "the member always wins". Those languages apply it silently, so a library that later adds a member changes which code runs without a diagnostic. This ADR makes the same event a compile error at the next build.
- **From Swift:** conformance declared outside the type, with the type then usable wherever the protocol is expected.
- **From Pharo:** the method is globally visible and package-owned, and reflection sees it.

What it rejects: last-writer-wins (Pharo, Ruby, Clojure, Elixir) and lexical scoping (Kotlin, C#, Scala, Ruby refinements), which cannot be seen by a dynamic send.

## User Impact

**Newcomer.** One more declaration form to learn, and it reads as a sentence. The errors say which rule was broken and what to write instead. The surprising part will be that `printString` cannot be implemented for someone else's class.

**Smalltalk developer.** This is a departure. Pharo lets any package add or override any method on any class. Here an addition needs a protocol and cannot override. In exchange, loading a package cannot change the behaviour of code that worked before it loaded. Live patching through `Foo >> sel => ...` is unchanged for classes with source.

**Erlang/BEAM developer.** The model is close to Elixir's `defimpl`, with the method reachable as an ordinary send. Implemented methods are plain functions in the declaring module, and dispatch reaches them only on a miss, which is easy to trace.

**Production operator.** Loading a package can no longer change the result of a send that a compiled method answers. A file-loaded patch to a class method of a class without source is no longer available (§9). The per-level extension lookup leaves the send path. Hot reload of an implementing module removes deleted methods.

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
- 🎩 **Smalltalk purist**: "At least it leaves the class's method dictionary alone."
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
- The designer's argument for additive-only `>>` is hard to answer while the ownership rule stays as weak as §3 admits. If the owner reduces ownership to a lint (Open Question 1, option c), most of the case for requiring a protocol rests on provided methods and the completeness check.
- Zero users of cross-file `>>` is evidence that it can be removed. It is not evidence that anyone needs `implementFor:`. The one demonstrated need is `asJson` on a foreign class.
- The designer's argument for pure protocol dispatch is the strongest one against this ADR. Keying by `{protocol, class}` is what makes the orphan rule sound, and this ADR keys by `{class, selector}` to keep `x asJson`.
- Newcomers and Smalltalk developers want the one-line form; the usage numbers say nobody has needed it.
- Operators lose file-based patching of a class without source, and gain a guarantee that loading code cannot change a send that a compiled method answers.

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

This gets the whole runtime simplification, and about 80% of this ADR's value is that runtime change. It is the closest alternative, and the review pass judged the case against it the weakest part of the first draft. The honest comparison:

| | Additive-only `>>` | `implementFor:` |
|---|---|---|
| Precedence bugs removed | Yes | Yes |
| Runtime machinery deleted | The same | The same |
| New syntax | None | One declaration form |
| Ceremony for one method | None | A protocol |
| Completeness check ("Money really is JsonRepresentable") | No | Yes |
| Provided methods come with the protocol | No; each is written out again, as above | Yes |
| Something to attach an ownership rule to | No | Yes, though the rule is weak (§3) |
| Unit for browse, removal and docs | The file | The implementation |

Rejected in this draft because the owner's proposal makes the protocol the unit of the feature, and because provisions and the completeness check are real. It is the right fallback if the owner concludes from §3 that ownership is not worth its ceremony. The two are also compatible in sequence: nothing in the runtime design depends on which syntax fills the table.

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

### A library-level registry: `Json register: aMapping for: Money`
Solve JSON in the JSON library and add nothing to the language. Rejected as the general answer: it is one registry per library, each with its own lookup, reload and ownership behaviour, and `x asJson` still would not work on the class. It remains a reasonable thing for a single library to do.

### Retroactive `uses:` written outside the class
`Money uses: JsonMappable` as a top-level line, reusing ADR 0127 §3a unchanged. Rejected because §3a lets a provision replace an inherited method (`overriding:`), which is exactly the precedence this ADR removes, and flattening needs the class's source, which a consumer does not recompile.

### Fold own-package implementations into the class at build time
When the declaring package owns the class, compile the implementation into the class as `same_file_fold` does, so the table holds only foreign targets. Not rejected: it is a possible later optimisation. It is left out of v1 because it needs the build graph to recompile a class when another file changes, and the miss-only table is correct without it.

### Remove cross-file `>>` now and add nothing
With no users, delete the feature and wait for a second real need before designing `implementFor:`. This is a serious option and is Open Question 7. It is not the draft's choice because the `asJson` need already exists (BT-2819 left it open), and removing the only way to meet it first would leave a gap.

### Do it by convention only
Deprecate cross-file `>>` in documentation and leave the runtime. Rejected: the cost is in the dispatch path and the tools, not in user code.

## Consequences

### Positive
- A compiled method always wins, on both sides, at every depth. The lookup order becomes "compiled chain, then implementations, then DNU".
- The BT-5002, BT-5060, BT-5061 class of bug cannot recur.
- Loading a package cannot change the result of a send that a compiled method answers.
- The per-level extension lookup leaves `has_method/1` and the default dispatch arms.
- A class from another package can be made `JsonMappable` or `JsonRepresentable`.
- A class that gains a method its users had implemented produces a build error at the implementer, where it can be fixed.
- Deleting a method from an implementing file takes effect on reload.

### Negative
- Adding one method to one foreign class now needs a protocol declaration.
- An implementation cannot customise an inherited selector such as `printString`, `=` or `hash`.
- The ownership rule does not give Rust-level coherence, because selectors are global and conformance is structural. As stated it can be sidestepped (§3).
- **A new semver hazard.** A method that a dependency adds in a minor release breaks the build of every package that had implemented that selector for the class. The fix is to delete the implementation, but the break is real, and at run time the same event is only a warning.
- A send that misses, including a `respondsTo:` that answers `false` and every genuine `does_not_understand`, pays a table probe per class on the chain before it fails.
- Implementation bodies reuse the extension-body codegen, so that code's bug class (BT-4503, BT-3920) stays.
- Much of the compiler-side code is renamed, not deleted: the index, the dependency channel and the body scoping survive under new names. The deletion is concentrated in the runtime dispatch path.
- There is no evidence yet of demand for `implementFor:` beyond `asJson`.
- An application that owns neither the protocol nor the class must declare its own protocol, unless Open Question 1(a) is accepted.
- File-based replacement of a compiled class method on a class without stored source is lost, against principle 11 (§9).
- A send of an implemented selector takes the whole compiled walk before it reaches the table. It is slower than a compiled method unless the verdict is memoised in `beamtalk_class_derived_cache`, which the inherited-send path already does for its own verdicts.
- The tests that use cross-file extensions (19 files by this ADR's count; the owner's survey counted 33 uses) and the `beamtalk-uat` fixture need rewriting or deleting. A further 23 `stdlib/test` files call `beamtalk_extensions` through FFI.
- This is a breaking language change, with a deprecation period.

### Neutral
- The class-derived cache and its generation read stay. BT-4984 kept that read partly so extension changes show on the next send; hot reload still needs it. This ADR removes one reason for it, not the read.
- ADR 0084 runtime class-method funs are unchanged.
- `StandaloneMethodDefinition` stays in the AST for the same-file and live forms.
- Implementations cannot add state, as extensions could not.

## Implementation

Sizes are rough (S, M, L, XL) and assume Open Question 1 is settled first.

### Phase 0: wire check (S)
Prove the one runtime assumption before building syntax. Register a hand-written fun in a miss-only table through FFI, hook the root DNU path, and run `stdlib/test/lookup_order_matrix_test.bt` extended with the new order for: a value class, an actor, a primitive (`Integer`, `String`), a class-side send, a class with its own `doesNotUnderstand:args:`, `respondsTo:`, `perform:` and `super`. If the miss hook cannot be placed at one choke point for all of these, the design needs to change before Phase 1. DNU handling is generated per class today (`gen_server/dispatch.rs:986-1021`) *(read)*, so this is a real question.

### Phase 1: add `implementFor:` (XL)
- **Prerequisite.** Carry every protocol's signatures and owning package in the stdlib table and the dependency interface, and write the owner resolver of §3.
- **Parser and AST (M).** A `ProtocolImplementation { protocol, class, package qualifiers, methods, class_methods, span }` node in `crates/beamtalk-core/src/ast/class.rs`, parsed beside `Protocol define:` *(inferred: the parser must tell the declaration from a keyword send by the indented body, as it does for class and protocol headers)*. Formatter and unparse support.
- **Semantic analysis (L).** An implementation index; the additive, completeness, clash, known-subclass and ownership checks; provisions expanded through `trait_expansion.rs`; signatures registered with `ClassHierarchy`, compiled signature first and tagged as implemented. Body validators run on implementation bodies from the start (the BT-4658 and BT-4923 lesson).
- **Codegen (M).** Method funs reuse the foreign-extension shapes in `gen_server/extensions.rs:27-34`. Registration is emitted in `register_class/0` with the `Document` API. This touches extension-body codegen, so the PR carries `ci:corpus`.
- **Runtime (L).** The implementation table and its API in `beamtalk_protocol_registry.erl`; the miss hook from Phase 0; `respondsTo:` and `canUnderstand:` after the compiled walk; by-name registration; the re-check at class activation; the `ambiguous_implementation` error; purge on `:sync` and `removeFromSystem`.
- **Reflection, browse, xref, LSP (M).** As in Decision §4. Document symbols, go-to-definition, hover and completion for the new block.
- **Dependencies (M).** Implementations travel in the dependency interface with classes and protocols, replacing `DependencyExtensions` (`crates/beamtalk-project/src/dependency_extensions.rs`) *(inferred)*. This is what WS3 becomes: the same visibility, carried as declarations in the package interface instead of a separate extension channel.
- **Tests.** BUnit tests in `stdlib/test` for each rule and each error; the matrix test for lookup order; `tests/repl-protocol/cases` for reload, removal, the dead-row warning and the ambiguity error; `tests/parity` for CLI, MCP and LSP diagnostics agreeing.
- **Docs.** `docs/beamtalk-language-features.md`, `docs/development/surface-parity.md`.

During this phase both mechanisms exist. An implemented selector and an extension on the same `{class, selector}` is a compile error.

### Phase 2: deprecate cross-file `>>` (M)
- A warning on every cross-file `>>`, with the `implementFor:` rewrite in the hint.
- Migrate the 11 `stdlib/test` files, 3 `tests/parity` files and 5 `.btscript` cases, and the 23 `stdlib/test` files that call `beamtalk_extensions` through FFI. Tests that pin extension precedence are deleted with the behaviour they pin.
- Settle Open Question 3. Phase 3 does not start until it is.

### Phase 3: remove (L)
Cross-file `>>` becomes a compile error. Then delete:

| Piece | Where |
|---|---|
| The extension registry: three ETS tables, last-writer-wins, the conflicts bag | `beamtalk_extensions.erl` (913 lines) |
| Interleaved lookups | `beamtalk_dispatch.erl`: the extension checks in `lookup/5`, `super/5` and `lookup_in_class_chain/5`; the `{ext, Owner}` verdict; `inherited_value_ext/5`; `check_extension/2` |
| Extension probes in generated code | `beamtalk_extensions:lookup` in the actor and value default arms and the `beamtalk_extensions:has` step of `has_method/1` (`gen_server/dispatch.rs`, `core_erlang/dispatch_spec.rs:355, 478`, `supervisor_codegen.rs:239`) |
| The one-hop gap and its warnings (BT-5060, #5718) | `one_hop_children_missing_extension/2`, `one_hop_parent_extensions_missing/1` (`beamtalk_dispatch.erl:339, 368`); `warn_one_hop_children/2`, `warn_one_hop_for_class/1`, `emit_child_warning/3`, `emit_parent_warning/3` (`beamtalk_extensions.erl:316-410`); `warn_one_hop_gaps/1` (`beamtalk_repl_loader_activate.erl:156`); the `bt5060_live_*` fixtures |
| Class-side extension-first order | `class_side_extension/2` (`beamtalk_class_dispatch.erl:945`) and its callers; the constructor-extension path in `beamtalk_object_class.erl` (BT-4409, BT-4588) |
| The `extension` shadow-flag kind | `beamtalk_class_shadow_flags.erl`. The `runtime_fun` and `class_vars` kinds stay |
| Guards against an extension overriding a known selector | The `String`-extension gate on the Binary fast path (`binary_or_string_module/2`, `beamtalk_primitive.erl:1005-1010`); the per-level extension probe behind the `equals:` shortcut (`beamtalk_equality.erl:120-140`); `beamtalk_collection_selectors.erl:90-127`; `beamtalk_default_method:note_override/2` on registration *(inferred: each needs checking that the extension case is its only reason)* |
| Compiler pieces | `extension_conflicts.rs`, the `dependency_extensions_unknown` guard, the `pkg@Target >>` qualifier, the class-side "extension replaces signature" rule |

What is renamed or reworked, not deleted: `extension_index.rs` (becomes the implementation index), `gen_server/extensions.rs` (registration target changes; body codegen stays), `dependency_extensions.rs` (folds into the dependency interface). `StandaloneMethodDefinition` stays for the same-file and live forms.

The reply-warning scope added for BT-5060 (`with_extension_warning_scope/2`, `beamtalk_class_registry.erl:432`) is deleted unless the dead-row and ambiguity warnings of §2 reuse it *(an implementation decision)*.

`one_hop_selectors` (`value_type_codegen/dispatch.rs:308`) routes selectors the direct superclass compiles to that superclass's `dispatch/3`. It no longer has a gap, because no implementation can exist for a selector the superclass compiles.

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

1. **What should the ownership rule be?** As stated it can be sidestepped with a local protocol, and it blocks the plain form of the motivating case (§3). Three coherent positions:
   - **(a) Selector-based, with a root-package exemption.** A package may not make a foreign class answer a selector that a dependency's protocol declares, unless it owns that protocol or the class. The root package of a build is exempt, because nothing depends on it; an orphan implementation found in a dependency is an error. This is the only option that does what the rule was proposed to do. *Recommended.*
   - **(b) As stated.** Simple, and documents intent, with the loophole accepted.
   - **(c) A lint.** No hard rule; warn on an orphan implementation in a library, as Clojure's guidance does. The clash rule in §2 remains the real protection. This weakens the case against additive-only `>>`.
   "Root package" means the package being built, not a package with an `[application]` section; a library can have one.
2. **Header syntax.** `P implementFor: C` with an indented body (this draft), or with a trailing `=>` (the original sketch)? Should there also be a reflective message form for live use, in the spirit of principles 6 and 11? And the spelling of the removal selector in §8.
3. **Patching a class without source (gates Phase 3).** Is the ADR 0084 runtime fun enough to replace file-based class-side extensions for patching a running class (§9)? Should the workspace store dependency sources so `Foo >> sel => ...` works for a dependency class (today it fails: `activate_dependency_modules` registers a module and source path but stores no class source, `beamtalk_workspace_bootstrap.erl:121-128`)? Should MCP `save_method` and `Class compile:source:` gain a class-side form, since `install_method/9` already supports it?
4. **Run-time clash behaviour.** This draft makes a clashing selector ambiguous (neither answers) so the result does not depend on load order. The alternative is first-registered-wins with a warning, which is friendlier at the REPL and order-dependent.
5. **Slot and class-variable access.** This draft allows an implementation body the public interface only (§6a). Today's foreign extensions can read and write `self.field`. Should the class's own package be allowed slot access in its implementations?
6. **REPL strictness.** Does the ownership rule apply at the REPL, or is an interactive session treated like the root package (question 1a)?
7. **Sequencing.** Add `implementFor:` first and then remove cross-file `>>` (this draft), or remove cross-file `>>` now, since nothing uses it, and add `implementFor:` when a second need appears beside `asJson`?
8. **`methods` and `includesSelector:`.** Do they list implemented selectors? Extensions are listed today; flattened trait methods are local. *Default: listed, with `origin`.*
9. **Deprecation length.** How many releases between the Phase 2 warning and the Phase 3 error, given no known external users?

## References
- Related issues: BT-2819 (`JsonMapping`, `JsonMappable`; #5717, #5719), BT-5060 (#5718), BT-5002, BT-5061, BT-5062, BT-4307, BT-4588, BT-4250, BT-4133, BT-4503, BT-3951, BT-4260, BT-4280, BT-4800, BT-4984, BT-4721, BT-4610, BT-4608, BT-4611, BT-4658, BT-4923
- Related ADRs: [ADR 0066](0066-open-class-extension-methods.md) (superseded in part), [ADR 0068](0068-parametric-types-and-protocols.md) (structural protocols), [ADR 0070](0070-package-namespaces-and-dependencies.md) (packages), [ADR 0084](0084-class-side-runtime-method-fun-dispatch.md) (runtime class-method funs), [ADR 0127](0127-traits-for-behavioural-reuse.md) (traits), [ADR 0130](0130-class-variables-live-in-the-class-process.md) (class-side fun shape), [ADR 0142](0142-class-process-owns-only-class-state.md) (where class methods run), [ADR 0144](0144-class-side-protocol-types.md) (class-side protocol types)
- Code: `runtime/apps/beamtalk_runtime/src/beamtalk_extensions.erl`, `beamtalk_dispatch.erl`, `beamtalk_protocol_registry.erl`; `runtime/apps/beamtalk_workspace/src/beamtalk_repl_loader.erl`; `crates/beamtalk-core/src/compilation/same_file_fold.rs`, `extension_index.rs`; `crates/beamtalk-codegen/src/core_erlang/gen_server/extensions.rs`; `stdlib/test/lookup_order_matrix_test.bt`
- Prior art: [Rust RFC 2451](https://rust-lang.github.io/rfcs/2451-re-rebalancing-coherence.html), [Swift SE-0364](https://github.com/apple/swift-evolution/blob/main/proposals/0364-retroactive-conformance-warning.md), [Elixir `Protocol`](https://hexdocs.pm/elixir/Protocol.html), [Clojure protocols](https://clojure.org/reference/protocols), [Haskell orphan instances](https://wiki.haskell.org/Orphan_instance), [C# extension methods](https://learn.microsoft.com/dotnet/csharp/programming-guide/classes-and-structs/extension-methods), [Ruby refinements](https://docs.ruby-lang.org/en/2.5.0/syntax/refinements_rdoc.html), [Apple QA1908](https://developer.apple.com/library/content/qa/qa1908/_index.html)
