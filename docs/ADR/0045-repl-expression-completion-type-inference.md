# ADR 0045: REPL Expression-Level Completion via Gradual Type Inference

## Status
Proposed 2026-03-01

## Context

The REPL completion system handles single-token receivers: class names, workspace bindings, and simple literals (integers, strings). Concretely, `"hello" si` offers String methods, `counter m` offers Counter methods (via runtime `class_of/1`), and `Integer cl` offers Integer class methods. These work because the receiver is a single, classifiable token.

Completions fail for expressions with chained sends. Typing `"hello" size cl` should offer Integer methods (because `String#size` is annotated `-> Integer`), but the current engine cannot determine that. Similarly, `#(1, 2, 3) collect: [:x | x * 2] si` and `counter getValue to` produce no completions because the receiver is not a single token.

Evaluating subexpressions to infer their type is not viable: `counter increment cl` would mutate state as a side effect of requesting completions. The solution must be **inference without evaluation**.

Three type-inference assets are relevant:

1. **Runtime method signature maps** (BT-988, BT-990): each class's `class_state` stores `method_signatures` and `class_method_signatures` as `#{selector() => binary()}` display strings (e.g., `size => <<"size -> Integer">>`). These are present for all registered classes, including user-defined classes created in the REPL.

2. **Rust `TypeChecker`** (ADR 0025, `crates/beamtalk-core/src/semantic_analysis/type_checker.rs`): walks the AST, infers `InferredType::Known(ClassName)` or `InferredType::Dynamic` for each expression span. This runs in the compiler context against a `ClassHierarchy` built from the full module graph. Currently stores results in a `TypeMap` for LSP queries only — does not write back to the AST.

3. **Compile-time body inference** (new in this ADR): the TypeChecker's inference results for method return types can be written back to `MethodDefinition.return_type` before codegen, so that `method_return_types` in the emitted BEAM module contains inferred types alongside explicit annotations. This bridges assets 1 and 2: the TypeChecker's inference powers the runtime return-type maps without requiring explicit annotation on every method.

The key constraint is: **the Rust compiler port process has no access to the runtime class registry**. The port is stateless — it handles one request at a time and constructs only `ClassHierarchy::with_builtins()`, which contains stdlib classes but not classes defined by the user in their REPL session. Routing completion through the port would make user-defined classes (`Counter`, `MyTree`, custom actors) invisible to inference. This defeats the primary REPL use case.

The question is: **how does expression-level type inference for completion run, and how are return types propagated to the completion engine?**

## Decision

Expression-level completion is driven by the **Erlang runtime**, using a new structured `method_return_types` map stored on each class alongside the existing `method_signatures` display strings. The completion engine parses the expression into a chain of message sends, resolves the receiver type, then walks the chain one hop at a time — looking up each send's return type directly from the class's return-type map — until it arrives at the type of the final receiver. The final method lookup against that class produces the completions.

### Return-Type Metadata

A new field is added to `class_state` in `beamtalk_object_class.erl`, storing machine-readable return types separately from the human-readable display signatures:

```erlang
-record(class_state, {
    %% ... existing fields ...
    method_signatures         = #{} :: #{selector() => binary()},       %% BT-988: display strings
    class_method_signatures   = #{} :: #{selector() => binary()},       %% BT-990: display strings
    method_return_types       = #{} :: #{selector() => atom()},         %% BT-989: machine-typed
    class_method_return_types = #{} :: #{selector() => atom()}           %% BT-989: machine-typed
}).
```

The codegen emits both maps in the same pass. `method_return_types` is populated from `MethodDefinition.return_type` — the same source as the display signature `->` suffix — so they are always consistent. Only `TypeAnnotation::Simple` values produce return-type entries (e.g., `-> Integer` maps to `size => 'Integer'`). These may come from explicit programmer annotations or from the compile-time body inference writeback pass (Phase 1b), which synthesises `Simple` annotations from `InferredType::Known` results. Union types (`Integer | False`), generic types (`List<Integer>`), and singleton types (`#north`) are omitted — the chain resolution treats them as dynamic. This is an acceptable initial limitation because the vast majority of method return types are simple class names; complex types can be supported in a future iteration if warranted.

When `return_type` is `None` or non-Simple, the selector is absent from the map (absence = dynamic):

```erlang
%% Generated for String class (conceptual):
method_return_types = #{size => 'Integer', reversed => 'String', ...},
%% Note: detect:ifNone: (union return type) and format: (generic) are absent
```

This keeps display strings purely human-readable and gives completion a stable, typed data source with no parsing required.

The `method_return_types` map must be threaded through all mutation paths: `apply_class_info/2` (class redefinition in REPL) and `put_method/3` (dynamic method addition/removal). `put_method/3` already removes stale entries from `method_signatures`; it must do the same for `method_return_types`.

### Chain Resolution

The completion engine is extended with a chain-resolution path triggered when the receiver is not a single classifiable token:

```erlang
%% Resolve the type at the end of a send chain.
%% e.g., "hello" size  →  Integer
%%        counter getValue reversed  →  String (if getValue -> String)
-spec resolve_chain_type(binary(), map()) -> {ok, atom()} | undefined.
resolve_chain_type(Expr, Bindings) ->
    case tokenise_send_chain(Expr) of
        {ok, ReceiverToken, Sends} ->
            case classify_receiver(ReceiverToken, Bindings) of
                {instance, ClassName} -> walk_chain(ClassName, Sends);
                {class, ClassName}    -> walk_chain_class(ClassName, Sends);
                undefined             -> undefined
            end;
        error ->
            undefined
    end.

walk_chain(ClassName, []) ->
    {ok, ClassName};
walk_chain(ClassName, [Selector | Rest]) ->
    %% Walks superclass chain to find the method, same as dispatch
    case beamtalk_class_registry:get_method_return_type(ClassName, Selector) of
        {ok, NextClass} -> walk_chain(NextClass, Rest);
        undefined       -> undefined   %% chain breaks — annotation absent or non-Simple
    end.

%% Class-side chain: first hop uses class_method_return_types,
%% then transitions to instance-side walk_chain for subsequent sends.
walk_chain_class(ClassName, []) ->
    {ok, ClassName};
walk_chain_class(ClassName, [Selector | Rest]) ->
    case beamtalk_class_registry:get_class_method_return_type(ClassName, Selector) of
        {ok, NextClass} -> walk_chain(NextClass, Rest);  %% transition to instance-side
        undefined       -> undefined
    end.
```

The return-type lookup (`get_method_return_type/2`) must walk the superclass chain, analogous to how `beamtalk_method_resolver` resolves method dispatch. A method defined on `Integer` must be findable when the receiver type is `SmallInteger`. This can reuse the existing hierarchy-walking infrastructure.

### Completion Engine Changes

`beamtalk_repl_ops_dev:get_context_completions/2` is extended with the chain path:

```erlang
get_context_completions(Code, Bindings) ->
    case parse_receiver_and_prefix(Code) of
        {single_token, Receiver, Prefix} ->
            %% existing path — classify_receiver/2
            classify_and_complete(Receiver, Prefix, Bindings);
        {expression, ReceiverExpr, Prefix} ->
            %% new path — Erlang chain resolution
            case resolve_chain_type(ReceiverExpr, Bindings) of
                {ok, ClassName} -> complete_instance_methods(ClassName, Prefix);
                undefined       -> []
            end;
        _ ->
            get_completions(Code)
    end.
```

### Prefix Stripping

The incomplete token at the cursor is stripped before chain resolution, leaving a complete send chain:

```
User types:  "hello" size cl
             ─────────────── ──
             ReceiverExpr   Prefix
Chain:       "hello"  →  size  →  [prefix cl stripped]
Resolved:    String   →  Integer
Offered:     clock, collect:, collect:separatedBy:, isZero, max:, min:, ...
```

### Send Chain Scope

Chain resolution initially supports **unary send chains** — sequences of unary (zero-argument) messages separated by whitespace. This covers the common patterns: `"hello" size cl<TAB>`, `counter getValue to<TAB>`, `#(1, 2, 3) size is<TAB>`.

**Not in initial scope:**
- **Keyword sends mid-chain:** `myList inject: 0 into: [...] si<TAB>` — reconstructing `inject:into:` as a single selector from space-separated tokens requires bracket matching and keyword-arg parsing. This is a significant parsing problem deferred to a follow-up.
- **Parenthesised subexpressions:** `(myList size) cl<TAB>` — requires paren matching.
- **Method references:** `(Counter >> #increment) s<TAB>` — requires understanding `>>` return type.

These are genuine limitations. The `tokenise_send_chain/1` function returns `error` for any expression it cannot parse as a simple unary chain, and the completion engine falls back to the existing single-token path (which also produces no completions for these cases, so there is no regression).

Keyword sends as the *final* message (the completion target) work naturally because the prefix is stripped before chain resolution. Keyword sends as *intermediate* messages in the chain are the hard case and a natural follow-up issue.

### Return-Type Coverage

Chain resolution breaks silently when a method in the chain has neither an explicit `-> ClassName` annotation nor a body-inference result that yields a Simple type. Coverage therefore comes from two sources:

1. **Explicit annotations** — the programmer writes `-> Integer` on the method. Required for builtins (defined in Rust, not compiled through the TypeChecker) and for methods whose return type is too complex for body inference (union types, dynamic dispatch).
2. **Compile-time body inference** (Phase 1b) — the TypeChecker infers the return type from the method body and the writeback pass populates `MethodDefinition.return_type` before codegen. This covers the common case of user-defined methods with straightforward return expressions (e.g., `getValue => ^balance` where `balance` is Integer).

For **builtins**, body inference does not apply — they are defined in `generated_builtins.rs` as Rust data structures, not compiled Beamtalk source. The ~337 built-in method entries must be manually audited; approximately half currently have `return_type: None`. The audit targets high-frequency chains first (Integer arithmetic, String manipulation, Collection operations).

For **user-defined methods**, body inference handles many cases automatically. Explicit annotation is only needed when the body returns a union type, uses dynamic dispatch, or delegates through cross-method calls that the TypeChecker cannot resolve to a single class.

### REPL Session Example

```
bt> "hello" size <TAB>
clock  collect:  collect:separatedBy:  isZero  max:  min:  ...

bt> #(1, 2, 3) size <TAB>
clock  collect:  collect:separatedBy:  isZero  max:  min:  ...

bt> counter getValue <TAB>       % user-defined class, return type known (annotation or inferred)
toUpperCase  reversed  size  ...

bt> counter getValue unknownChain <TAB>
(no completions — return type unknown, chain breaks)

bt> counter <TAB>                % single-token binding — existing path unchanged
deposit:  withdraw:  balance  ...
```

## Prior Art

**Pharo Smalltalk**: The Pharo completion system uses AST-level type hints without evaluation. The default algorithm extracts type information from method argument naming conventions (`aString`, `anInteger`) for ~36% coverage; heuristics improve this to ~50%. Pharo workspaces also use results of prior evaluations: if you have evaluated `"hello" size` and the result `5` is visible in the workspace, the completion engine can use that runtime result. Beamtalk adopts the same "no evaluation" principle; the chain-inference approach is closer to Pharo's static AST analysis path than to the result-caching path.

**Elixir IEx / ElixirSense**: Completion uses `@spec` type annotations on compiled module metadata. Return types must be explicitly annotated; IEx does not infer intermediate types for chained calls. Elixir stores type information separately from documentation strings — `@spec` is a typed contract, `@doc` is a display string. Beamtalk's decision to store `method_return_types` separately from `method_signatures` follows this same principle: display and machine-readable data have different consumers and different stability requirements.

**Gleam**: No traditional REPL; completion provided at language-server level with full static types. Not applicable.

**Ruby (Sorbet / RBS / Solargraph)**: Solargraph infers return types through a combination of explicit `@return` yard annotations and lightweight type propagation without evaluation. Coverage is annotation-driven; chained method completions work when each step is annotated. The Beamtalk approach is directly analogous.

**Existing Beamtalk LSP** (ADR 0024, ADR 0025): The `TypeChecker` powers LSP hover and diagnostic completions using the full `ClassHierarchy` built from a compiled module. REPL completion uses a different data source (the live runtime class registry) to serve a different context (interactive single expressions). These are complementary, not competing: the LSP path covers file-level analysis, the runtime path covers live session state including dynamically-defined classes.

## User Impact

**Newcomer** (Python/JS/Ruby background): Completions work for common patterns (`"hello" size <TAB>`, `myList size <TAB>`) without any configuration. For user-defined classes, body inference handles many cases automatically — the user doesn't need to know about type annotations for simple methods. Chains through methods where inference fails silently produce no completions rather than an error — the REPL remains fully functional.

**Smalltalk developer**: Matches workspace ergonomics — completions after message sends feel natural. The annotation requirement is familiar: Pharo completion quality also depends on type hints. The dev is incentivised to annotate method return types, which also improves `:h` documentation output (BT-988 display signatures) and LSP hover types.

**Erlang/BEAM developer**: Pure in-process Erlang, no IPC dependency. Works with the live runtime class registry, including any classes loaded from Erlang/OTP interop modules. The annotation format (`-> ClassName`) is visible in `:h` output, making the contract inspectable at the REPL.

**Production operator**: Completion is a development-time feature; no production impact. The completion path is in-process with no external dependencies, no latency budget concerns, and no new failure modes.

**Tooling developer** (LSP, debugger): The `walk_chain/2` function is a small, pure function over a direct map lookup. Independently testable without standing up the full compiler pipeline. The `method_return_types` map is a clean typed data source — also reusable for future features like inline type hints in the REPL or debugger variable inspection.

## Steelman Analysis

### Option B: Rust TypeChecker via OTP Port

Send the expression to the Rust `TypeChecker` via the existing OTP Port. The TypeChecker infers the type at the cursor position and returns it.

| Cohort | Strongest argument |
|--------|--------------------|
| **Newcomer** | "The REPL knows exactly the same types as the compiler — fewer surprises when LSP hover and completion disagree" |
| **Smalltalk purist** | "One inference model. The compiler is the source of truth for types; querying it for completions is architecturally correct" |
| **BEAM veteran** | "The TypeChecker handles unannotated method return types via body analysis — you get completions even for methods that lack explicit annotations" |
| **Operator** | "Single source of truth reduces the chance of a subtle divergence between what the LSP says a type is and what the REPL completes on" |
| **Language designer** | "Most coherent long-term: as the TypeChecker improves (inference for generics, union types, protocol conformance), REPL completions improve automatically" |

**Why rejected**: The compiler port process constructs only `ClassHierarchy::with_builtins()`. It has no access to the live runtime class registry. Classes defined by the user in the REPL — the primary use case — are invisible to the TypeChecker in the port context. Fixing this would require either (a) serialising the full class hierarchy into each request (expensive and complex), (b) making the port stateful (contradicts current design), or (c) accepting that REPL-defined classes never get expression-level completions. The BEAM veteran's "body analysis for unannotated methods" point is genuinely strong — the TypeChecker can infer return types from method bodies for *any* code it can see, not just builtins. But the port cannot see REPL-defined classes at all, so body analysis is unavailable precisely where the REPL user needs it most. The Erlang-side approach has better coverage for REPL-defined classes and equivalent coverage for builtins (both depend on annotations), with dramatically less infrastructure.

### Option C: Hybrid — Erlang Parses Chain, Compiler Resolves Ambiguity

Fast path via Erlang runtime registry; fallback to Rust port for unannotated methods.

| Cohort | Strongest argument |
|--------|--------------------|
| **Newcomer** | "Best of both worlds — common annotated cases are instant, and the compiler fills gaps for unannotated builtins" |
| **Smalltalk purist** | "This mirrors how Pharo actually works — a fast heuristic covers most completions, the compiler can be consulted for harder cases. Pragmatic Smalltalk tradition" |
| **BEAM veteran** | "Fault-tolerant: fast path has no external dependency, slow path degrades gracefully" |
| **Operator** | "Two paths with a clear fallback boundary are easier to reason about operationally than a single path with silent degradation — you know *which* path failed" |
| **Language designer** | "Preserves optionality — can migrate fully to the compiler path once the ClassHierarchy-in-port problem is solved" |

**Why deferred, not rejected**: Option C's fast path is Option A — the `method_return_types` chain walk — verbatim. The fallback adds port-based inference for unannotated methods on top. This means Option A is the correct first step: it builds exactly the infrastructure Option C needs, without committing to the port fallback before the ClassHierarchy-in-port problem is solved. Option C is not rejected — it is the natural upgrade path from Option A.

The ClassHierarchy-in-port problem has a clean solution that does not require source code or whole-world recompilation: **BEAM metadata streaming via a background actor**. When a Beamtalk class is compiled and loaded in the REPL, the generated BEAM module already contains the full class metadata as callable functions (`instance_methods/0`, `class_methods/0`, and with this ADR's work, `method_return_types/0`). A background actor can stream this metadata to the compiler session gen-server as classes are registered, populating an incremental `ClassHierarchy` without source. The compiler session can reconstruct its state after a crash by reading all loaded Beamtalk BEAM modules directly — the same recovery model as Dialyzer's PLT. A follow-up issue (BT-993) tracks this design.

Note: BT-993 is specifically about enabling the Rust TypeChecker *port* to see user-defined classes — relevant for cross-class diagnostics and the TypeChecker fallback path (Option C). For chain-based completion, BT-993 is not required: a compile-time body inference writeback pass (Phase 1b of this ADR) populates `method_return_types/0` with inferred Simple return types for user-defined classes before the BEAM module is emitted (see Consequences > Neutral). The completion feature is complete at Phase 2 of this ADR; BT-993 is the path to richer diagnostic coverage beyond completion.

**Tension points**: The BEAM veteran and language designer cohorts make the strongest case for Option B/C. The core tension is "REPL visibility vs. architectural coherence." Option A has structurally better coverage for user-defined classes — the runtime class registry sees every loaded class, and the compile-time body inference writeback (Phase 1b) means even unannotated user-defined methods get inferred return types in the chain walk. Option B has better architectural coherence for future diagnostic features (cross-class type checking, protocol conformance). The decision takes REPL coverage as the priority because the REPL is the primary editing surface. Option A is not a permanent commitment — it is the starting point of a progression toward Option C.

## Alternatives Considered

### Speculative evaluation for pure expressions

Evaluate the receiver expression only when it can be statically determined to be "pure" (no sends to actor objects, no I/O primitives). This would catch `#(1, 2, 3) size` and similar literal-heavy chains.

Rejected because: the purity analysis is itself a non-trivial static analysis problem. Actor status may not be known statically (a binding could hold either a value object or an actor). The boundary between "safe to evaluate" and "has side effects" is the same problem as full type inference, but with worse failure modes. The Erlang chain resolution is strictly safer.

### Cache prior evaluation results

After the user evaluates `"hello" size` and sees `5`, cache `{"hello" size, Integer}` and use that for completion the next time the same prefix is typed. This is not speculative evaluation — it reuses results the user already caused.

Rejected for this ADR as a primary mechanism because: the cache is stale after bindings change, requires matching against arbitrary expression text (a fuzzy equality problem), and only works for expressions the user has already fully evaluated. It could be a useful complement to chain resolution in a future iteration but does not cover the primary case of typing a new expression for the first time.

## Consequences

### Positive
- Expression-level completions work for chained sends through annotated methods, including user-defined classes defined in the current REPL session.
- Pure in-process Erlang: zero IPC latency, zero new failure modes, no port dependency.
- Completion quality improves incrementally as return-type annotations are added to stdlib methods — both existing sessions and new sessions benefit immediately.
- Display strings (`method_signatures`) remain purely human-readable; the machine-typed `method_return_types` map has a stable, typed contract.
- `walk_chain/2` is a small, independently testable pure function over a direct map lookup.

### Negative
- Chain resolution breaks silently when any method in the chain lacks a return-type annotation and body inference cannot determine a Simple type. The user sees empty completions with no indication of why.
- Builtin method return type coverage must be improved as part of this work; that audit is not trivial for ~337 built-in methods.
- Adds two new fields to `class_state` and corresponding codegen output.

### Neutral
- Return-type coverage (explicit annotations + body inference) directly determines completion quality. For builtins, explicit annotations are the only source; for user-defined methods, body inference covers many cases automatically. This creates organic incentive to annotate methods where inference is insufficient — a side effect that also improves `:h` documentation and LSP hover types.
- The existing single-token completion path is unchanged; chain resolution is additive.
- The TypeChecker (`InferredType`) and the runtime return-type maps both derive from `MethodDefinition.return_type` — the same source — so they cannot diverge as long as the codegen emits both correctly.
- **Compile-time body inference can cover unannotated user-defined methods without BT-993.** The TypeChecker already infers return types from method bodies during semantic analysis — but currently stores results only in a `TypeMap` for LSP queries, not back into the AST. A new **writeback pass** (Phase 1b) takes the TypeChecker's inferred return type for each method, and where the result is `InferredType::Known(ClassName)` and the method has no explicit annotation, synthesises a `TypeAnnotation::Simple` and writes it to `MethodDefinition.return_type` before codegen. This requires the AST to be mutable between semantic analysis and codegen — a pipeline change, but a contained one. Once implemented, the BEAM module contains inferred return types in `method_return_types/0`, and the Erlang chain walk finds them when the class registers. Explicit annotation is only required for cases body inference cannot resolve (complex unions, cross-method inference, dynamic returns). BT-993 (BEAM metadata streaming) is needed for the Rust TypeChecker *port* to see user-defined classes for cross-class diagnostics and Option C fallback — it is not a prerequisite for chain-based completion coverage of user-defined classes.

## Implementation

### Phase 0: Prove the core mechanic

Before the annotation audit, validate the data pipeline with a minimal spike:

- Add `method_return_types` and `class_method_return_types` fields to `class_state` in `beamtalk_object_class.erl`
- Emit the maps from codegen for one class (e.g., `String`) with a handful of annotated methods
- Verify `beamtalk_class_registry:get_method_return_type('String', size)` returns `{ok, 'Integer'}`

This confirms the codegen → registry → lookup pipeline before investing in the annotation audit.

### Phase 1a: Annotation audit and codegen

- **`runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl`**: add `method_return_types` and `class_method_return_types` fields to `class_state`
- **`crates/beamtalk-core/src/codegen/`**: emit both new maps from `MethodDefinition.return_type` in the same codegen pass that emits `method_signatures`; omit the selector key entirely when `return_type` is `None` or non-Simple (absence = dynamic, consistent with the record type and Decision section)
- **`crates/beamtalk-core/src/semantic_analysis/class_hierarchy/generated_builtins.rs`**: audit ~337 built-in method entries; add `return_type` for unambiguous cases (Integer arithmetic, String operations, Collection operations, Boolean predicates). This audit is primarily about builtins — user-defined methods are covered by Phase 1b.
- **`stdlib/src/*.bt`**: add `-> ClassName` return-type annotations on key stdlib methods where body inference alone would be insufficient (e.g., methods with union return types, dynamic dispatch)
- Target: high-frequency chains covered before Phase 2

### Phase 1b: Compile-time body inference writeback

The TypeChecker currently infers method return types during semantic analysis but stores results only in a `TypeMap` (span → `InferredType`) for LSP queries. A new writeback pass bridges inference and codegen:

- **`crates/beamtalk-core/src/semantic_analysis/mod.rs`** (or new `return_type_writeback.rs`): after `infer_types` runs, iterate over methods where `return_type` is `None`. For each, look up the method body's inferred type in the `TypeMap`. If the result is `InferredType::Known(class_name)`, synthesise a `TypeAnnotation::Simple(Identifier::new(class_name, synthetic_span))` and write it to `method.return_type`.
- **Pipeline change**: the `Module` AST must be mutable between semantic analysis and codegen. Currently `infer_types` takes `&Module`; the writeback runs as a separate pass on `&mut Module` after inference completes.
- **Conservative scope**: only write back `Known` → `Simple` mappings. `Dynamic`, `Unknown`, and complex inferred types are left as `None` (absence = dynamic). This maintains the same guarantee as explicit annotations: codegen only sees `Simple` types in the return-type maps.
- **Validation**: for a user-defined class `Counter` with method `getValue => ^balance` (where `balance` is typed as `Integer`), the writeback should populate `method_return_types` with `getValue => 'Integer'` even without an explicit `-> Integer` annotation. Verify via REPL: `counter getValue <TAB>` offers Integer methods.

### Phase 2: Runtime chain resolution

- **`runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_dev.erl`**: implement `tokenise_send_chain/1`, `resolve_chain_type/2`, `walk_chain/2`, `walk_chain_class/2`
- Extend `parse_receiver_and_prefix/1` to return `{expression, ReceiverExpr, Prefix}` for multi-token receivers
- Wire chain path into `get_context_completions/2`

### Phase 3: Tests

- Unit tests for `walk_chain/2` (single hop, multi-hop, broken chain, absent annotation)
- E2e REPL completion tests: `"hello" size <TAB>` → Integer methods; `counter getValue <TAB>` → correct methods for return type; `counter unknownChain <TAB>` → empty

### Affected components

| Component | Change |
|-----------|--------|
| `beamtalk_object_class.erl` | Two new fields on `class_state`; thread through `apply_class_info/2` and `put_method/3` |
| `beamtalk_class_registry.erl` | New `get_method_return_type/2` and `get_class_method_return_type/2` with superclass chain walking |
| Codegen (Rust, `crates/beamtalk-core/src/codegen/`) | Emit new return-type maps from `MethodDefinition.return_type` (Simple annotations only) |
| Semantic analysis (`type_checker.rs`, new writeback pass) | New pass: write `InferredType::Known` results back to `MethodDefinition.return_type` for unannotated methods before codegen |
| Rust builtins (`generated_builtins.rs`) | Return-type annotation audit; add `return_type` for ~half of built-in methods |
| Stdlib (`.bt` files) | Add/verify `-> ClassName` return-type annotations on key methods |
| Completion engine (`beamtalk_repl_ops_dev.erl`) | Chain resolution path; prefix stripping for multi-token receivers |
| Tests | Unit tests for chain resolution; e2e REPL completion tests |

## Migration Path

Not applicable — this is additive. The existing single-token completion path is unchanged.

## References

- Related issues: BT-989, BT-993 (incremental compiler ClassHierarchy — upgrade path to Option C)
- Related ADRs: ADR 0022 (embedded compiler via OTP Port), ADR 0024 (static-first IDE tooling), ADR 0025 (gradual typing and protocols), ADR 0033 (runtime-embedded documentation)
- Current completion implementation: `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_dev.erl`
- Method signature storage: `runtime/apps/beamtalk_runtime/src/beamtalk_object_class.erl`
- Builtin method definitions: `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/generated_builtins.rs`
