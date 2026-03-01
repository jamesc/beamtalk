# ADR 0045: REPL Expression-Level Completion via Gradual Type Inference

## Status
Proposed 2026-03-01

## Context

The REPL completion system handles single-token receivers: class names, workspace bindings, and simple literals (integers, strings). Concretely, `'hello' si` offers String methods, `counter m` offers Counter methods (via runtime `class_of/1`), and `Integer cl` offers Integer class methods. These work because the receiver is a single, classifiable token.

Completions fail for expressions with chained sends. Typing `'hello' size cl` should offer Integer methods (because `String >> size` returns an Integer), but the current engine cannot determine that. Similarly, `#(1 2 3) collect: [:x | x * 2] si` and `(Counter >> #increment) s` produce no completions because the receiver is not a single token.

Evaluating subexpressions to infer their type is not viable: `counter increment cl` would mutate state as a side effect of requesting completions. The solution must be **inference without evaluation**.

Two type-inference assets already exist in the codebase:

1. **Rust `TypeChecker`** (ADR 0025, `crates/beamtalk-core/src/semantic_analysis/type_checker.rs`): walks the AST, infers `InferredType::Known(ClassName)` or `InferredType::Dynamic` for each expression span. Already handles `'hello' size → Known("Integer")` when `String#size` is in the class hierarchy.

2. **Runtime method signature maps** (BT-988, BT-990): each class's `class_state` stores `method_signatures` and `class_method_signatures` as `#{selector() => binary()}` display strings (e.g., `size => <<"size -> Integer">>`). These are human-readable but not machine-typed.

The OTP Port (ADR 0022) provides a live channel from the Erlang runtime to the embedded Rust compiler. The question is: **where does expression-level type inference for completion run, and how does the inferred type flow back to the completion engine?**

## Decision

Expression-level completion is driven by the **Rust TypeChecker via the OTP Port**, using a new lightweight `type_at_cursor` operation. The Erlang completion engine strips the incomplete final token (the prefix), sends the remaining expression plus the current workspace bindings to the Rust compiler, receives an inferred class name, and performs the final method lookup against the runtime class registry.

### Protocol

A new port operation `type_at_cursor` is added alongside the existing `compile` operation:

```erlang
%% Request (Erlang → Rust compiler port)
Request = #{
    op       => <<"type_at_cursor">>,
    source   => <<"'hello' size ">>,   %% expression with prefix stripped
    bindings => #{                      %% current workspace binding types
        <<"counter">> => <<"Counter">>,
        <<"x">>       => <<"Integer">>
    }
},

%% Response (Rust → Erlang)
{ok, <<"Integer">>}   %% Known type — offer Integer methods
{ok, <<"dynamic">>}   %% Unknown — fall back to current behaviour
{error, Reason}        %% Parse/compile failure — fall back
```

### Completion Engine Changes

`beamtalk_repl_ops_dev:get_context_completions/2` is extended with an expression-level path triggered when the receiver is not a single classifiable token:

```erlang
get_context_completions(Code, Bindings) ->
    case parse_receiver_and_prefix(Code) of
        {single_token, Receiver, Prefix} ->
            %% existing path — classify_receiver/2
            classify_and_complete(Receiver, Prefix, Bindings);
        {expression, ReceiverExpr, Prefix} ->
            %% new path — delegate to Rust compiler
            complete_via_type_inference(ReceiverExpr, Prefix, Bindings);
        _ ->
            get_completions(Code)
    end.

complete_via_type_inference(ReceiverExpr, Prefix, Bindings) ->
    BindingTypes = extract_binding_types(Bindings),
    case beamtalk_compiler_port:type_at_cursor(ReceiverExpr, BindingTypes) of
        {ok, <<"dynamic">>} -> [];
        {ok, ClassName}     -> complete_instance_methods(ClassName, Prefix);
        {error, _}          -> []
    end.
```

### Prefix Stripping

The incomplete token at the cursor (the completion prefix) is stripped before sending to the compiler, leaving a syntactically valid expression:

```
User types:  'hello' size cl
             ─────────────── ──
             ReceiverExpr   Prefix
Sent to Rust: 'hello' size
Returned:     Integer
Offered:      clock, collect:, collect:separatedBy:, ...
```

Stripping is whitespace-aware: if the final token is attached (no preceding space), it is removed; if the cursor is immediately after a complete expression with no prefix, `Prefix = ""` and no stripping is needed.

### Rust Compiler Changes

A new `CompilerPort` operation `type_at_cursor(source, bindings)` is added:

```rust
// crates/beamtalk-core/src/semantic_analysis/type_checker.rs (conceptual)
pub fn infer_expression_type(
    source: &str,
    binding_types: HashMap<String, String>,
) -> InferredType {
    let ast = parse_expression(source)?;
    let mut checker = TypeChecker::with_bindings(binding_types);
    checker.infer_expression(&ast)
}
```

The TypeChecker already resolves `'hello' size → Known("Integer")` when `String#size` is registered in the `ClassHierarchy`. The new entry point parses a **single expression** (not a full class definition) and returns the inferred type of the final expression. Binding types from the workspace are injected as `Known(ClassName)` entries in the TypeChecker's environment.

### Degradation

If the compiler port is unavailable, if parsing fails, or if inference returns `dynamic`, the completion engine falls back to the existing single-token behavior. This means expression-level completions silently degrade to empty rather than error — the REPL remains usable.

### REPL Session Example

```
bt> 'hello' size <TAB>
clock  collect:  collect:separatedBy:  isZero  max:  min:  ...

bt> #(1 2 3) size <TAB>
clock  collect:  collect:separatedBy:  isZero  max:  min:  ...

bt> counter <TAB>          % binding — existing path, unchanged
deposit:  withdraw:  balance  ...

bt> 'hello' size unknown_method_name <TAB>
(no completions — dynamic fallback)
```

## Prior Art

**Pharo Smalltalk**: The completion system uses AST-level type hints without evaluation. It relies on method argument naming conventions (`aString`, `anInteger`) for ~36% type coverage, with bytecode-level type reconstruction (without side effects) improving this to ~50-75% precision. Pharo avoids evaluation in completions; type information flows from the compiler's AST and class hierarchy reflection. Beamtalk adopts the same "no evaluation" principle but uses its existing gradual type system rather than convention-based hints.

**Elixir IEx / ElixirSense**: Completion is entirely static — it uses `@spec` type annotations on compiled module metadata. For chained calls, IEx does not infer intermediate types; it relies on explicit annotations. Beamtalk's approach is more dynamic: the TypeChecker uses the class hierarchy for inference even without explicit return-type annotations on every method.

**Gleam**: No traditional REPL; completion is provided at the language-server level using full static types. Not directly applicable (Gleam has no dynamic dispatch).

**LSP hover types (existing Beamtalk)**: The `TypeChecker` is already used to power LSP hover type display (ADR 0024, ADR 0025). The `type_at_cursor` operation is a natural extension of this: the same inference engine that tells the LSP "this variable has type Integer" now tells the REPL "the receiver of this completion has type Integer." This reuse preserves a single source of truth.

## User Impact

**Newcomer** (Python/JS/Ruby background): Completions "just work" for the natural patterns they would write (`'hello' size <TAB>`, `myList collect:... <TAB>`). They don't need to understand type inference or annotate their code. When completions don't appear (for unannotated/dynamic expressions), the REPL is still fully functional — they just type the whole method name. Error messages are unchanged.

**Smalltalk developer**: This matches Pharo/Squeak workspace ergonomics, where completions are available after evaluating expressions or from known types. The inference-without-evaluation constraint is well understood in the Smalltalk world (Pharo uses the same principle). The Smalltalk dev may notice that unannotated methods don't propagate completion — they are incentivised to add return-type annotations, which also improves `:h` documentation output.

**Erlang/BEAM developer**: The OTP Port communication for completion adds a synchronous call per completion request. This is comparable to existing LSP operations. The port is already used for compilation. The degradation path (fall back when port is unavailable) respects OTP fault tolerance principles — the completion system has no hard dependency on the compiler.

**Production operator**: Completion is a development-time feature; no production impact. The port communication is bounded in time by an existing timeout mechanism. If the compiler port crashes, completions degrade gracefully with no user-visible error.

**Tooling developer** (LSP, debugger): The `type_at_cursor` operation is a clean, well-typed RPC. It takes source text and binding types, returns a class name or `"dynamic"`. This is also useful for LSP semantic token colouring and future debugger variable inspection. The Rust side is testable in isolation.

## Steelman Analysis

### Option A: Runtime Return-Type Registry (Erlang-side chain inference)

Compiler emits a machine-readable `method_return_types :: #{selector() => class_name() | dynamic}` map per class. Erlang completion walks the chain using this registry without any IPC.

| Cohort | Strongest argument |
|--------|--------------------|
| **Newcomer** | "Completions work instantly — no port, no latency, no startup dependency" |
| **Smalltalk purist** | "The runtime is the source of truth at runtime; inference should live there" |
| **BEAM veteran** | "Pure Erlang. No synchronous port call in the completion hot path. Respects the nothing-shared-by-default BEAM model" |
| **Operator** | "Zero additional failure modes — completion works even if the compiler port has crashed" |
| **Language designer** | "Incentivises annotating return types; annotations improve both completion and docs simultaneously" |

**Why rejected**: Option A creates a parallel type inference engine in Erlang that will diverge from the Rust `TypeChecker`. When a method's return type cannot be statically annotated (e.g., it depends on an argument type, or it is inherited), the Erlang engine has no way to resolve it. The existing `TypeChecker` already handles these cases via class hierarchy walking. Maintaining two inference systems doubles the surface area for bugs and inconsistencies.

### Option C: Hybrid — Erlang Parses Chain, Compiler Resolves Ambiguity

Fast path: Erlang resolves annotated chains via the runtime registry. Fallback: delegate to Rust compiler via port for unknown types.

| Cohort | Strongest argument |
|--------|--------------------|
| **Newcomer** | "Most common cases (annotated methods) are instant; complex cases still work" |
| **BEAM veteran** | "Fault-tolerant by default: fast path has no external dependencies" |
| **Operator** | "Two independently observable paths — can trace fast vs slow separately" |
| **Language designer** | "Pragmatic: commit to the right long-term design (Rust compiler) without blocking on full coverage" |

**Why rejected**: The fast path and fallback path use different inference engines with different coverage. Users experience inconsistent completion quality depending on annotation coverage — annotated methods get completions, unannotated ones silently don't. This creates implicit pressure to annotate *for completion* rather than for correctness, producing annotations that are semantically inaccurate. Option C's maintenance cost (two engines, two test surfaces, a seam between them) is not justified when the Rust TypeChecker can handle the full case.

**Tension points**: BEAM veterans and operators legitimately prefer decoupled completions (Option A/C). The chosen design mitigates this with the degradation path: if the port is down, completions fall back to current behaviour rather than erroring.

## Alternatives Considered

### Speculative evaluation for pure expressions

Evaluate the receiver expression only when it is statically determined to be "pure" (no sends to actor objects, no I/O primitives). This would catch `#(1 2 3) size` and similar literal-heavy chains.

Rejected because: the purity analysis is itself a non-trivial static analysis problem. Actor status may not be known statically (a binding could hold either a value object or an actor). The boundary between "safe to evaluate" and "has side effects" is the same problem as full type inference, but with worse failure modes (incorrect evaluation causes visible state mutation). The Rust TypeChecker is strictly safer.

### Parse display signature strings in Erlang

The `method_signatures` map already stores strings like `"size -> Integer"`. Parse the `-> Type` suffix in Erlang to extract return types.

Rejected because: display signatures are not guaranteed to be present (methods without explicit return-type annotations have no `->` clause), their format is for human display and subject to change, and this is effectively Option A with fragile string parsing bolted on. It also creates an implicit contract between the display format and the inference engine.

## Consequences

### Positive
- Expression-level completions work for chained sends without any user annotation requirement, for methods whose return types are already resolvable by the TypeChecker.
- Single source of truth: the same inference engine powers LSP hover, diagnostic warnings, and REPL completion.
- Workspace binding types are propagated into the TypeChecker, enabling completions like `x` (bound to a Counter) followed by a method chain.
- The `type_at_cursor` operation is reusable for future features (LSP semantic tokens, debugger variable inspection, inline type display).

### Negative
- A synchronous port call is added to each expression-level completion request. This adds latency compared to the current in-process single-token completion.
- Methods without inferred return types (deeply dynamic code, unannotated Erlang interop) produce no expression-level completions.
- The partial-expression parsing step (stripping the prefix) must handle edge cases: empty prefix, multi-line expressions, cursor mid-token.

### Neutral
- Return-type annotation coverage directly determines completion quality. This creates organic incentive to annotate methods — a side effect that also improves `:h` documentation output.
- The existing single-token completion path is unchanged; expression-level completion is additive.
- The TypeChecker already has test coverage for the inference cases this feature exercises.

## Implementation

### Phase 1: Port operation

Add `type_at_cursor` to the compiler port protocol:

- **`crates/beamtalk-core/src/port_protocol.rs`** (or equivalent): new `TypeAtCursorRequest` / `TypeAtCursorResponse` variants.
- **`crates/beamtalk-core/src/semantic_analysis/type_checker.rs`**: new `infer_expression_type(source, bindings)` entry point that parses a single expression and returns `InferredType`.
- **`runtime/apps/beamtalk_workspace/src/beamtalk_compiler_port.erl`**: new `type_at_cursor/2` function wrapping the port call.

### Phase 2: Prefix stripping

- **`runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_dev.erl`**: extend `parse_receiver_and_prefix/1` to return `{expression, ReceiverExpr, Prefix}` for multi-token receivers. Strip the trailing incomplete token from `ReceiverExpr` before the port call.

### Phase 3: Completion integration

- **`beamtalk_repl_ops_dev.erl`**: add `complete_via_type_inference/3` and wire it into `get_context_completions/2` after the existing single-token path fails to classify.

### Phase 4: Binding type extraction

- **`beamtalk_repl_ops_dev.erl`**: implement `extract_binding_types/1` to convert workspace bindings (live values) into `#{ClassName => binary()}` using `beamtalk_primitive:class_of/1`.

### Affected components

| Component | Change |
|-----------|--------|
| Rust compiler (`beamtalk-core`) | New `infer_expression_type` entry point in TypeChecker; new port protocol variant |
| OTP Port (`beamtalk_compiler_port.erl`) | New `type_at_cursor/2` wrapper |
| Completion engine (`beamtalk_repl_ops_dev.erl`) | Multi-token receiver path; prefix stripping; binding type extraction |
| Tests | Unit tests for `infer_expression_type`; e2e REPL completion tests |

## References

- Related issues: BT-989
- Related ADRs: ADR 0022 (embedded compiler via OTP Port), ADR 0024 (static-first IDE tooling), ADR 0025 (gradual typing and protocols)
- Current completion implementation: `runtime/apps/beamtalk_workspace/src/beamtalk_repl_ops_dev.erl`
- TypeChecker: `crates/beamtalk-core/src/semantic_analysis/type_checker.rs`
- Port protocol: `crates/beamtalk-core/src/`
