# ADR 0003: Keep Core Erlang as Primary Code Generation Target

## Status
Implemented (2026-02-08)

## Context

Beamtalk currently generates Core Erlang, a simplified intermediate representation (IR) that is compiled to BEAM bytecode by `erlc +from_core`. This decision was made early in development (BT-10) to quickly achieve working code generation.

Issue BT-28 raised the question: should Beamtalk generate human-readable Erlang source code instead, following Gleam's approach?

### The Case for Erlang Source

Gleam, a successful Rust-to-BEAM compiler, generates Erlang source rather than Core Erlang. The Gleam team's rationale:

1. **Debuggability**: Generated Erlang is human-readable, making stack traces and debugging easier
2. **Tooling integration**: Full support for Erlang formatters, linters, and syntax highlighting
3. **Ecosystem fit**: Erlang developers can inspect generated code using familiar tools
4. **Simplicity**: No need to learn Core Erlang's quirks (map syntax `~{}~`, guard expressions, etc.)

### The Case for Core Erlang

1. **Direct compilation**: Fewer transformation stages (AST → Core Erlang → BEAM)
2. **Explicit semantics**: Core Erlang makes control flow and evaluation order explicit
3. **No syntactic sugar**: What you generate is what you get—no hidden transformations
4. **Compiler internals alignment**: Easier to debug compilation issues with `erlc +core`
5. **Type system compatibility**: Core Erlang's simplified semantics are better suited for static analysis and type inference (see Alpaca's rationale below)

### Beamtalk-Specific Considerations

Beamtalk has unique requirements that differ from Gleam:

| Factor | Gleam | Beamtalk |
|--------|-------|----------|
| **Primary paradigm** | Functional, static types | Actor-based, dynamic Smalltalk-style |
| **Generated code pattern** | Direct function calls | gen_server message dispatch |
| **State handling** | Immutable data flow | Simulated mutation via state threading |
| **Target audience** | Erlang/Elixir developers | Smalltalk developers, AI agents |
| **Debugging focus** | Source-level debugging | Live inspection of running actors |

### Code Generation Complexity Comparison

**Current Core Erlang implementation**: ~10,300 lines of Rust across 12 modules

The complexity comes from:
- **gen_server scaffolding** (~1,400 lines): `init/1`, `handle_cast/2`, `handle_call/3`, `code_change/3`
- **State threading** (~1,500 lines): Simulating mutable `self` via threaded state maps
- **Control flow** (~1,400 lines): `whileTrue:`, `timesRepeat:`, `to:do:` iteration
- **Message dispatch** (~560 lines): Selector mangling, method tables

Switching to Erlang source would require:
1. Rewriting all generation code (~10k lines)
2. Learning Erlang source formatting conventions
3. Handling Erlang's syntactic sugar (records, macros, specs)
4. No reduction in actual complexity—gen_server patterns remain the same

### Debugging Reality Check

For Beamtalk's actor-based model, the debugging story differs from Gleam:

**What developers debug:**
- Actor state inspection (via Observer, `sys:get_state/1`)
- Message flow tracing (via `dbg`, `recon_trace`)
- Process supervision trees (via Observer)

**What Core Erlang affects:**
- Stack traces (function names, line numbers)
- Source correlation in debugger

For actor debugging, the generated code format is less important than runtime introspection tools. Beamtalk's REPL and live inspection features matter more than generated code readability.

### Line Numbers and Stack Traces

A key concern with Core Erlang is that stack traces reference the generated code, not the original source. However, **this problem exists regardless of output format**:

| Language | Output Format | Stack Trace Quality |
|----------|---------------|---------------------|
| Erlang | Native | Accurate |
| Elixir | Erlang AST | Accurate (deep BEAM integration) |
| Gleam | Erlang Source | **Limited/Inaccurate** |
| Beamtalk | Core Erlang | **Limited/Inaccurate** |

Gleam generates Erlang source but still has "limited or inaccurate" line numbers and stack traces according to their FAQ. This is because the BEAM runtime refers to line numbers in the *generated* Erlang code, not the original Gleam source.

**The same limitation applies to Beamtalk regardless of whether we generate Core Erlang or Erlang source.** Solving this requires source map support in the BEAM ecosystem, which neither approach provides.

### OTP Interoperability

Both Core Erlang and Erlang source have equivalent OTP interop:

- **gen_server**: Works identically—both compile to the same BEAM bytecode
- **Supervisors**: No difference—supervision trees work at the process level
- **Hot code reload**: Supported equally by both compilation paths
- **Distribution**: No difference—BEAM handles inter-node communication

The `erlc +from_core` compilation path is fully supported and produces standard BEAM modules that are indistinguishable from Erlang-compiled modules at runtime.

### Empirical Evidence

Generated Core Erlang from Beamtalk is already reasonably readable:

```erlang
module 'counter' ['init'/1, 'handle_cast'/2, 'spawn'/0]
  attributes ['behaviour' = ['gen_server']]

'init'/1 = fun (_Args) ->
    let InitialState = ~{
      '__class__' => 'Counter',
      'value' => 0
    }~
    in {'ok', InitialState}
```

Compare to what equivalent Erlang source would look like:

```erlang
-module(counter).
-behaviour(gen_server).
-export([init/1, handle_cast/2, spawn/0]).

init(_Args) ->
    InitialState = #{
      '__class__' => 'Counter',
      value => 0
    },
    {ok, InitialState}.
```

The difference is primarily syntactic—the structure and debugging experience are equivalent.

## Benefits Analysis

We systematically evaluated each claimed benefit of switching to Erlang source:

| Claimed Benefit | Evaluation | Verdict |
|-----------------|------------|---------|
| **Readable stack traces** | BEAM references generated code line numbers, not original source. Gleam has the same problem despite generating Erlang source. | ❌ No improvement |
| **Better debugging** | Beamtalk debugging is actor-focused: `Observer`, `sys:get_state/1`, `dbg`. Source-level debugging is secondary. | ❌ No improvement |
| **Erlang tooling integration** | Formatters/linters would operate on generated code. Linting generated code is not useful—lint the Beamtalk source instead. | ❌ Not useful |
| **Erlang developer familiarity** | Target audience is Smalltalk developers, not Erlang developers. Generated code inspection is rare. | ⚠️ Marginal |
| **OTP interoperability** | Both paths produce identical BEAM modules. gen_server, supervisors, hot reload all work identically. | ❌ No difference |
| **Compile speed** | Both require `erlc` invocation. Core Erlang skips parsing/expansion but difference is negligible (<100ms). | ❌ No difference |
| **Future maintainability** | Erlang source would allow "forking" generated code to maintain as pure Erlang. Edge case—not a design goal. | ⚠️ Edge case only |

### What Would Actually Help Debugging?

The stack trace problem requires **source maps**—metadata mapping generated code locations back to original Beamtalk source. This is orthogonal to output format:

1. Core Erlang with source maps → Good stack traces
2. Erlang source with source maps → Good stack traces  
3. Either format without source maps → Poor stack traces (current state for Gleam and Beamtalk)

If we prioritize debugging improvements, investing in source map infrastructure would help. Switching output formats would not.

### Why Gleam's Decision Doesn't Apply

Gleam chose Erlang source for these reasons (from maintainer discussions):

1. **Erlang developer audience**: Gleam targets Erlang/Elixir developers who can read generated code
2. **Functional paradigm**: Generated code maps closely to idiomatic Erlang patterns
3. **Simplicity**: Avoids learning Core Erlang's syntax quirks

Beamtalk's situation differs:

1. **Smalltalk developer audience**: Users expect live inspection, not source reading
2. **Actor paradigm**: Generated code is gen_server boilerplate regardless of format
3. **Already invested**: 10k lines of working Core Erlang codegen exists

### What Other BEAM Languages Chose (and Why)

| Language | Target | Rationale |
|----------|--------|-----------|
| **Alpaca** | Core Erlang | Static typing with Hindley-Milner inference; Core Erlang's simplified semantics support type analysis and exhaustiveness checking |
| **Efene** | Erlang source | "Different syntax for Erlang"; 1:1 mapping for readable output; leverage existing tooling without reinventing |
| **Gleam** | Erlang source | Maintainer preferred simplicity; targets Erlang developers who read generated code |
| **LFE** | Core Erlang | Lisp macros need explicit control over generated code; Core Erlang provides predictable output |
| **Clojerl** | Erlang source | Clojure compatibility; familiar patterns for Clojure developers |

**Pattern:** Languages prioritizing **static analysis and type systems** (Alpaca, LFE) tend toward Core Erlang. Languages prioritizing **readable output and ecosystem familiarity** (Efene, Gleam, Clojerl) tend toward Erlang source.

**Beamtalk's trajectory:** Planned Hindley-Milner type inference aligns with Core Erlang choice. The explicit semantics will support:
- Type inference across message sends
- Exhaustive pattern matching in dispatch
- Static analysis of actor protocols

## Decision

**Keep Core Erlang as the primary code generation target.**

The expected benefits of switching to Erlang source do not justify the cost:

1. **No debugging improvement**: Actor debugging relies on runtime tools, not source inspection
2. **High migration cost**: ~10k lines of working code would need rewriting
3. **Marginal readability gain**: Core Erlang is already readable for debugging purposes
4. **Gleam's rationale doesn't apply**: Beamtalk's actor model and target audience differ

## Consequences

### Positive

- **No disruption**: Existing codegen continues working
- **Development velocity**: Focus on language features, not infrastructure rewrites
- **Explicit semantics**: Core Erlang's explicitness aids debugging the compiler itself
- **Lower risk**: Proven implementation vs. speculative benefits

### Negative

- **Learning curve**: Contributors must understand Core Erlang syntax
- **Tooling gaps**: No syntax highlighting for `.core` files in editors
- **Erlang expertise assumption**: Debugging requires Core Erlang familiarity

### Neutral

- **Documentation**: Add Core Erlang syntax guide to contributor docs
- **Future option**: Nothing prevents adding Erlang source output later as an optional target

## Critique and Response

We stress-tested this decision by arguing from opposing viewpoints. Here's what critics might say and our responses:

### Critique from "Direct to BEAM" Advocates

| Critique | Our Response |
|----------|--------------|
| **"6+ months is a guess, not data"** | Fair. We didn't prototype. However, the BEAM Book documents the complexity, and zero production languages have taken this path. The burden of proof is on the novel approach. |
| **"BEAM bytecode is more stable than claimed"** | Core opcodes are stable, but optimization passes change. OTP 26 added new JIT optimizations that require specific bytecode patterns. Tracking this is ongoing work. |
| **"Optimizations may not matter for actor workloads"** | Possibly true. But we'd need to measure, and "probably doesn't matter" isn't a reason to abandon free optimizations. |
| **"No OTP dependency enables single-binary distribution"** | Valid benefit we underweighted. However, Beamtalk *runs* on BEAM—users need OTP anyway at runtime. Compile-time independence has limited value. |
| **"Being first isn't automatically wrong"** | True, but being first with no clear benefit *is* wrong. We'd need a compelling reason Beamtalk specifically needs this. |

**Acknowledged gap:** We should spike a trivial BEAM generator to validate effort estimates if this question resurfaces.

### Critique from "Erlang Source" Advocates

| Critique | Our Response |
|----------|--------------|
| **"10k lines is sunk cost fallacy"** | Partially fair. But the rewrite has *opportunity cost*—2-3 weeks not spent on language features. The benefit must exceed that cost. |
| **"Didn't compare actual stack traces"** | Valid criticism. We relied on Gleam's FAQ rather than empirical comparison. See "Future Work" below. |
| **"Target audience assumption may be wrong"** | We should validate. If Erlang developers are debugging Beamtalk agents, Erlang source becomes more valuable. |
| **"Dialyzer could catch errors in generated code"** | Interesting point. However, Dialyzer on generated code catches codegen bugs, not user bugs. Limited value. |
| **"Observer code view would be readable"** | True, but Observer's code view is rarely used vs. process inspection. Minor benefit. |
| **"2-3 weeks is cheap insurance"** | If we were starting fresh, yes. But we're not—we have working code. "Cheap insurance" against an unproven risk isn't compelling. |

**Acknowledged gap:** We should capture actual stack traces from both approaches for empirical comparison.

### Future Work (If This Decision Is Revisited)

Before reconsidering this decision, gather:

1. **Empirical stack trace comparison**: Generate identical crash in both formats, compare traces
2. **User research**: Survey target users—do they want to read generated code?
3. **BEAM generation spike**: Weekend prototype to validate/invalidate effort estimates
4. **Optimization impact measurement**: Profile Beamtalk workloads with/without erlc optimizations

Until this evidence exists, the decision stands.

## Alternatives Considered

### 1. Full Migration to Erlang Source

Rejected due to high cost (~2-3 weeks of work) with uncertain benefit. Gleam took this approach, but Gleam's functional-first design differs significantly from Beamtalk's actor model.

### 2. Dual Output (Core Erlang + Erlang Source)

Rejected as maintenance burden. Two codegen paths would need to stay synchronized, doubling the testing and debugging surface.

### 3. Erlang Source with Pretty-Printing

Partial approach: keep Core Erlang as primary, add `--emit=erlang` option that converts Core Erlang to Erlang source using `erlc +to_core` in reverse. Rejected because this tooling doesn't exist in standard Erlang/OTP.

### 4. Direct BEAM Bytecode Generation

Skip `erlc` entirely and emit `.beam` files directly from Rust.

**Potential benefits:**
- No Erlang/OTP dependency for compilation
- Full control over generated bytecode
- Potentially faster compilation (no `erlc` subprocess)

**Why rejected:**

1. **Massive implementation effort**: The BEAM file format is complex—opcodes, metadata, exports, imports, literals, atoms table, and module info. Estimated 6+ months of work.

2. **Unstable target**: BEAM bytecode changes between OTP versions. We'd need to track and support multiple BEAM versions, essentially maintaining our own backend for each OTP release.

3. **Loss of optimizations**: The Erlang compiler applies significant optimizations during Core Erlang → BEAM translation. These are battle-tested and constantly improved by the OTP team:

   | Optimization | Description |
   |--------------|-------------|
   | **Constant folding** | Evaluate constant expressions at compile time |
   | **Constant propagation** | Replace variables bound to constants with their values |
   | **Dead code elimination** | Remove unreachable code paths |
   | **Pattern match compilation** | Convert pattern matches to efficient decision trees |
   | **Function inlining** | Inline small functions to reduce call overhead |
   | **Case simplification** | Flatten unnecessary case structures, drop unreachable clauses |
   | **Guard optimization** | Simplify and reorder guard expressions |

   Reimplementing these would take years. Skipping them would produce slower code.

4. **No ecosystem precedent**: Every BEAM language (Elixir, Gleam, LFE, Clojerl, Alpaca, Efene) uses `erlc` or Erlang's compiler module. None generate BEAM directly. This is a strong signal.

5. **Debugging nightmare**: BEAM bytecode issues are much harder to debug than Core Erlang syntax errors. `erlc` provides excellent error messages.

6. **Hot code reload complexity**: BEAM's code loading has subtle requirements around module metadata that `erlc` handles correctly.

The only project attempting direct BEAM generation is [Firefly](https://github.com/GetFirefly/firefly), which compiles to native code (not BEAM bytecode) for standalone binaries—a different goal entirely.

**Bottom line:** Using `erlc` is the right abstraction boundary. It's stable, optimized, and battle-tested. Fighting it would be pure hubris.

## References

- [BT-28](https://linear.app/beamtalk/issue/BT-28): Evaluate Erlang source generation as alternative to Core Erlang
- [BT-241](https://linear.app/beamtalk/issue/BT-241): Spike - Prototype Erlang source generation for empirical comparison
- [BT-10](https://linear.app/beamtalk/issue/BT-10): Implement Core Erlang code generation (completed)
- [Gleam Discussion on Erlang vs Core Erlang](https://github.com/gleam-lang/gleam/discussions/1317)
- [Gleam FAQ on Stack Traces](https://gleam.run/frequently-asked-questions/)
- [Core Erlang Specification](https://www.it.uu.se/research/group/hipe/cerl/)
- [Gleam Erlang Codegen](https://github.com/gleam-lang/gleam/blob/main/compiler-core/src/erlang.rs)
- [The BEAM Book](http://blog.stenmans.org/theBeamBook/) - BEAM internals and bytecode format
- [Firefly Project](https://github.com/GetFirefly/firefly) - Native compilation for BEAM languages
