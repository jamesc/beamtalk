# Beamtalk Gap Analysis

**Status:** Draft - Analysis of current implementation vs. DDD model and architecture

This document identifies where the current codebase diverges from the DDD model and architectural design documented in `beamtalk-ddd-model.md` and `beamtalk-architecture.md`.

---

## Table of Contents

- [Executive Summary](#executive-summary)
- [Compiler Domain Gaps](#compiler-domain-gaps)
- [Runtime Domain Gaps](#runtime-domain-gaps)
- [Architecture Pattern Gaps](#architecture-pattern-gaps)
- [Missing Domain Services](#missing-domain-services)
- [Bounded Context Violations](#bounded-context-violations)
- [Priority Recommendations](#priority-recommendations)

---

## Executive Summary

### Maturity Assessment

| Bounded Context | Implementation Status | Gap Severity | Notes |
|----------------|----------------------|--------------|-------|
| SOURCE ANALYSIS | 🟢 **Mature** | Low | Lexer, parser, AST complete with error recovery |
| SEMANTIC ANALYSIS | 🔴 **Stub** | **Critical** | `analyse()` returns empty result, no name resolution |
| CODE GENERATION | 🟡 **Partial** | Medium | Core Erlang gen for classes, missing dispatch logic |
| LANGUAGE SERVICE | 🟡 **Partial** | Medium | Basic structure exists, queries not implemented |
| ACTOR SYSTEM | 🟢 **Mature** | Low | gen_server wrapper, dispatch, error isolation working |
| CONCURRENCY | 🟢 **Mature** | Low | Future state machine fully implemented |
| OBJECT SYSTEM | 🟢 **Mature** | Low | Class registry, instance tracking operational |
| HOT RELOAD | 🟡 **Stub** | Medium | `code_change/3` stub, no state migration logic |
| REPL | 🟢 **Mature** | Low | Expression eval, bindings, result formatting working |

**Overall Assessment:** The runtime is significantly ahead of the compiler. Most critical gaps are in the compiler domain (semantic analysis, complete code generation).

### Critical Blockers

1. **No Semantic Analysis** - Name resolution, type checking, scope analysis not implemented
2. **Incomplete Code Generation** - Method dispatch code generation incomplete
3. **No State Migration** - Hot reload's `code_change/3` callback is stubbed
4. **Language Service Queries** - Completions, hover, go-to-def not implemented

---

## Compiler Domain Gaps

### 1. SOURCE ANALYSIS Context - ✅ Mostly Complete

**What's Working:**
- Lexer with error recovery (token errors, not hard failures)
- Parser with synchronization points
- Rich AST with spans on all nodes
- Comment preservation in trivia
- Expression parsing (literals, sends, blocks, cascades)
- Class definition parsing

**Gaps:**

#### Gap 1.1: Trivia Span Tracking (Low Priority)
```rust
// crates/beamtalk-core/src/source_analysis/parser/mod.rs:366
comments.push(Comment {
    content: trivia.as_str().into(),
    span: start, // TODO: track trivia spans  <-- Using wrong span
    kind: match trivia { ... },
});
```

**DDD Model Expectation:** All AST nodes (including comments) have accurate spans for IDE features.

**Current State:** Comments use the module start span, not their actual location.

**Impact:** Minor - hover over comments won't show correct range.

**Fix Complexity:** Low - lexer needs to track trivia spans alongside tokens.

---

### 2. SEMANTIC ANALYSIS Context - ❌ NOT IMPLEMENTED

**DDD Model Expectation:** 
- Name resolution with scope chain walking
- Type inference and checking
- Semantic validation (arity, undefined vars)
- Scope graph for nested contexts

**Current State:**
```rust
// crates/beamtalk-core/src/semantic_analysis/mod.rs:131-135
pub fn analyse(_module: &Module) -> AnalysisResult {
    // TODO: Implement semantic analysis
    // For now, return an empty result
    AnalysisResult::new()
}
```

**Impact:** **CRITICAL** - Without semantic analysis:
- No undefined variable detection
- No type checking (when optional types added)
- No arity validation for message sends
- No scope-aware completions in language service
- Code generation cannot be validated

**Missing Components:**

#### Gap 2.1: Name Resolution Service
```rust
// Expected (from DDD model):
impl NameResolver {
    fn resolve(&self, name: &Identifier, scope: &Scope) -> Option<Binding> {
        // Try current scope
        if let Some(binding) = scope.get(name) {
            return Some(binding.clone());
        }
        // Walk parent chain
        scope.parent.as_ref().and_then(|p| self.resolve(name, p))
    }
}
```

**Not Present:** No `NameResolver` struct or implementation.

#### Gap 2.2: Scope Graph Construction

**Expected:** Hierarchical scope structure:
- Global scope (classes, REPL bindings)
- Class scope (methods, instance variables)
- Method scope (parameters, locals)
- Block scope (parameters, captures)

**Not Present:** Only stub types exist (`scope.rs` has empty module).

#### Gap 2.3: Type Checker Service

**Expected:** Even without full type system, basic checks:
- Method exists in receiver class
- Arity matches keyword message parts
- Return statement outside method/block

**Not Present:** No type checking infrastructure.

---

### 3. CODE GENERATION Context - 🟡 PARTIAL

**What's Working:**
- Core Erlang module boilerplate
- gen_server callbacks (init, handle_cast, handle_call, terminate)
- Method table generation
- State map initialization
- Error isolation pattern (safe_dispatch)
- Two-module pattern structure (instance + class modules)

**Gaps:**

#### Gap 3.1: State Migration in code_change/3 (Medium Priority)

**DDD Model Expectation:**
```erlang
code_change(OldVsn, OldState, Extra) ->
    %% Get new field defaults from class metadata
    Class = maps:get('__class__', OldState),
    {ok, ClassInfo} = beamtalk_classes:lookup(Class),
    DefaultFields = maps:get(default_fields, ClassInfo),
    
    %% Merge: new defaults + existing fields
    NewState = maps:merge(DefaultFields, OldState),
    
    %% Call user-defined migration if present
    case maps:find('__migrate__', maps:get('__methods__', NewState, #{})) of
        {ok, MigrateFun} -> {ok, MigrateFun(OldVsn, NewState, Extra)};
        error -> {ok, NewState}
    end.
```

**Current State:**
```rust
// crates/beamtalk-core/src/codegen/core_erlang/gen_server.rs:316
writeln!(self.output, "%% TODO: Add state migration logic")?;
writeln!(self.output, "{{'ok', State}}")?;
```

**Impact:** Hot reload works but loses new fields, can't handle schema changes.

**Workaround:** Currently just preserves old state, no migration.

#### Gap 3.2: Method Dispatch Code Generation (High Priority)

**Expected:** Generate `dispatch/4` function that:
1. Looks up selector in method table
2. Calls corresponding handler with Self, State, Args
3. Catches errors and returns `{error, Reason, State}` (error isolation)
4. Falls back to `doesNotUnderstand:` if method not found

**Partial Implementation:** Infrastructure exists but dispatch logic may be incomplete for all message patterns (unary, binary, keyword).

#### Gap 3.3: Super Dispatch Code Generation

**Expected:** Generate `super_dispatch/4` calls that use `beamtalk_classes:super_dispatch/4`.

**Current State:** May not generate super dispatch code for `super` keyword usage in methods.

#### Gap 3.4: Class Module Generation

**Expected (per DDD model):** Two modules per class:
- `beamtalk_<class>_class` - Class metadata, factory methods
- `beamtalk_<class>` - Instance gen_server

**Current State:** Code generator structure suggests this, but class metadata module generation may be incomplete.

#### Gap 3.5: Future Creation in Async Sends

**DDD Model Example:**
```rust
impl CoreErlangGenerator {
    fn generate_async_send(&self, receiver: Expr, selector: Selector, args: Vec<Expr>) -> ErlangExpr {
        // 1. Create future
        let future = ErlangExpr::call("beamtalk_future", "new", vec![]);
        
        // 2. Extract pid from #beamtalk_object
        let pid = ErlangExpr::call("erlang", "element", vec![
            ErlangExpr::literal(4), // pid is 4th field
            receiver.clone(),
        ]);
        
        // 3. Async cast
        let cast = ErlangExpr::call("gen_server", "cast", vec![
            pid,
            ErlangExpr::tuple(vec![
                ErlangExpr::atom(selector.to_atom()),
                ErlangExpr::list(args),
                future.clone(),
            ]),
        ]);
        
        // 4. Return future
        ErlangExpr::seq(vec![cast, future])
    }
}
```

**Status:** Need to verify this is fully implemented for all message send forms.

---

### 4. LANGUAGE SERVICE Context - 🟡 PARTIAL

**What's Working:**
- Position ↔ byte offset conversion
- Basic structure (`SimpleLanguageService`)
- File update mechanism
- Diagnostic types defined

**Gaps:**

#### Gap 4.1: Query Implementations (High Priority)

**DDD Model Expectation:**
```rust
impl LanguageService {
    pub fn completions(&mut self, path: &Path, position: Position) -> Vec<Completion>;
    pub fn hover(&mut self, path: &Path, position: Position) -> Option<HoverInfo>;
    pub fn definition(&mut self, path: &Path, position: Position) -> Option<Location>;
    pub fn references(&mut self, path: &Path, position: Position) -> Vec<Location>;
}
```

**Current State:** Trait defined but query methods return empty/None.

**Dependencies:** Requires Semantic Analysis (Gap 2.x) to be implemented first.

#### Gap 4.2: File Cache Invalidation

**Expected:** Incremental parsing that only re-parses changed files.

**Status:** Basic file tracking exists, but cache invalidation strategy not clear.

#### Gap 4.3: Query Cache (Salsa-style)

**DDD Model Expectation:** Memoize expensive queries (scope construction, type inference).

**Current State:** No query caching infrastructure.

**Impact:** Acceptable for now (files are small), but needed for large projects.

---

## Runtime Domain Gaps

### 5. ACTOR SYSTEM Context - ✅ Mostly Complete

**What's Working:**
- gen_server wrapper with dual dispatch (async/sync)
- Error isolation (safe_dispatch catches errors, returns to caller)
- State map with `__class__` and `__methods__`
- Message routing (handle_cast/handle_call)
- Self reference (`#beamtalk_object{}` record)
- Reflection APIs (class, respondsTo, instVarNames, instVarAt)

**Gaps:**

#### Gap 5.1: doesNotUnderstand Dispatch (Low Priority)

**Expected (from DDD model):**
```erlang
handle_dnu(Selector, Args, Self, State) ->
    case maps:find('doesNotUnderstand:args:', maps:get('__methods__', State)) of
        {ok, DnuFun} ->
            DnuFun([Selector, Args], State);
        error ->
            error({unknown_message, Selector, maps:get('__class__', State)})
    end.
```

**Current State:** Dispatch logic exists in `beamtalk_actor:dispatch/4`, need to verify DNU fallback is complete.

---

### 6. CONCURRENCY Context - ✅ COMPLETE

**All Expected Features Present:**
- Future state machine (pending → resolved/rejected)
- Waiter registration (await + callbacks)
- Timeout handling
- Automatic termination after 5 min idle
- Error propagation via rejection

**No Gaps Identified**

---

### 7. OBJECT SYSTEM Context - ✅ Mostly Complete

**What's Working:**
- Class registry (gen_server with class map)
- Class registration/lookup APIs
- Subclass enumeration
- Method add/remove for live development
- Super dispatch (walks inheritance chain)
- Instance tracking (ETS + process monitors)

**Gaps:**

#### Gap 7.1: Default Field Values Storage (Medium Priority)

**DDD Model Expectation:** ClassInfo includes `default_fields` map for state migration.

**Current State (beamtalk_classes.erl):**
```erlang
-type class_info() :: #{
    module := atom(),
    superclass := class_name() | none,
    methods := #{selector() => method_info()},
    instance_variables := [atom()],
    class_variables := map(),
    source_file => string()
    %% Missing: default_fields map for code_change/3
}.
```

**Impact:** Can't do automatic state migration during hot reload.

**Fix:** Add `default_fields => #{atom() => term()}` to ClassInfo when registering classes.

#### Gap 7.2: Abstract Class Optimization

**DDD Model Expectation:** Abstract classes skip instance module generation (compiler optimization).

**Current State:** Unclear if code generator checks `is_abstract` flag and skips instance module.

**Impact:** Minor inefficiency - generates unused modules.

---

### 8. HOT RELOAD Context - 🟡 PARTIAL

**What's Working:**
- BEAM's two-version coexistence (automatic)
- gen_server code_change/3 callback structure
- sys:change_code/4 support (OTP standard)

**Gaps:**

#### Gap 8.1: State Migration Logic (High Priority)

**Already covered in Gap 3.1** - Same issue from code generation and runtime perspectives.

**Additional Runtime Need:** Helper functions in runtime to:
1. Get default fields from class registry
2. Merge old state with new defaults
3. Call user-defined `__migrate__` method if present

#### Gap 8.2: Hot Reload Event Emission

**DDD Model Expectation:** Domain events for observability:
- `CodeUpgradeStarted` event
- `CodeUpgradeCompleted` event  
- `StateMigrationFailed` event

**Current State:** No event emission infrastructure.

**Impact:** Can't monitor/debug hot reload process.

---

### 9. REPL Context - ✅ COMPLETE

**All Expected Features Present:**
- Expression compilation on demand
- Binding persistence across expressions
- Result formatting
- Error handling

**No Gaps Identified**

---

## Architecture Pattern Gaps

### Pattern 1: Two-Module Generation (Flavors Pattern)

**DDD Model Expectation:** Every class generates:
1. `beamtalk_<class>_class` - Metadata, factory
2. `beamtalk_<class>` - Instance gen_server

**Current State:** Code generator has structure for this, but need to verify:
- Class module generation is complete
- Factory methods (`new`, `spawn`) generated correctly
- Metadata includes all required fields

**Verification Needed:** Inspect generated Core Erlang for a sample class.

---

### Pattern 2: Error Isolation (Flavors Pattern)

**DDD Model Expectation:** Actors catch errors and return to caller, don't crash instance.

**Current State:** ✅ **IMPLEMENTED**
```rust
// gen_server.rs generates safe_dispatch calls
case call 'module':'safe_dispatch'(Selector, Args, State) of
    <{'reply', Result, NewState}> when 'true' -> ...
    <{'error', Error, NewState}> when 'true' -> ...
```

**Erlang Runtime (beamtalk_actor.erl):** Implements error catching.

**Status:** ✅ Complete

---

### Pattern 3: Process-per-Future

**DDD Model Decision (ADR-3):** Each future is a BEAM process.

**Current State:** ✅ **IMPLEMENTED** (beamtalk_future.erl)

**Status:** ✅ Complete

---

### Pattern 4: Self Record (Object Reference)

**DDD Model Expectation:** Pass `#beamtalk_object{class, class_mod, pid}` to methods.

**Current State:** ✅ **IMPLEMENTED**
- Record defined in `runtime/include/beamtalk.hrl`
- `beamtalk_actor:make_self/1` creates record
- Generated code uses `make_self` before dispatch

**Status:** ✅ Complete

---

## Missing Domain Services

### Compiler Domain

| Service | Context | Status | Priority |
|---------|---------|--------|----------|
| **NameResolver** | SEMANTIC ANALYSIS | ❌ Missing | Critical |
| **TypeChecker** | SEMANTIC ANALYSIS | ❌ Missing | High |
| **ScopeGraph** | SEMANTIC ANALYSIS | ❌ Missing | Critical |
| **SemanticValidator** | SEMANTIC ANALYSIS | ❌ Missing | High |
| **CompletionProvider** | LANGUAGE SERVICE | ❌ Missing | High |
| **HoverProvider** | LANGUAGE SERVICE | ❌ Missing | Medium |
| **DefinitionProvider** | LANGUAGE SERVICE | ❌ Missing | Medium |
| **SelectorMangler** | CODE GENERATION | ⚠️ Unclear | Medium |
| **DispatchCodegen** | CODE GENERATION | ⚠️ Partial | High |
| **FutureCodegen** | CODE GENERATION | ⚠️ Unclear | High |

### Runtime Domain

| Service | Context | Status | Priority |
|---------|---------|--------|----------|
| **StateMigrator** | HOT RELOAD | ❌ Missing | High |
| **EventEmitter** | Cross-cutting | ❌ Missing | Low |
| **ErrorFormatter** | Cross-cutting | ⚠️ Unclear | Low |

---

## Bounded Context Violations

### Violation 1: Direct BEAM Calls in Generated Code

**Issue:** Generated Core Erlang directly calls `gen_server:cast`, `gen_server:call`, etc.

**DDD Concern:** CODE GENERATION context depends on BEAM primitives, not abstracted.

**Assessment:** **Not a violation** - This is the Published Language boundary (Core Erlang → BEAM). Direct calls are appropriate here.

---

### Violation 2: No Anti-Corruption Layer for REPL → ACTOR SYSTEM

**DDD Model Expectation:** REPL wraps raw gen_server calls with translation layer.

**Current State:** Need to verify if `beamtalk_repl_eval` directly calls `gen_server` or goes through `beamtalk_actor` API.

**Priority:** Low - If it's a violation, it's minor.

---

## Priority Recommendations

### P0 - Critical (Blocks Core Functionality)

1. **Implement Semantic Analysis** (Gap 2.x)
   - Name resolution with scope chain
   - Basic semantic validation (undefined vars, arity)
   - **Blocks:** Language service queries, full code generation validation
   - **Effort:** 2-3 weeks (substantial)

2. **Complete Method Dispatch Code Generation** (Gap 3.2)
   - Generate full `dispatch/4` implementation
   - Handle all selector types (unary, binary, keyword)
   - Integrate with error isolation
   - **Blocks:** Running non-trivial programs
   - **Effort:** 1 week

### P1 - High (Needed for Production Readiness)

3. **Implement State Migration in code_change/3** (Gap 3.1 + 8.1)
   - Generate migration logic in code_change/3
   - Store default fields in class registry
   - Support user-defined `__migrate__` method
   - **Blocks:** Hot reload with schema changes
   - **Effort:** 1 week (compiler + runtime)

4. **Implement Language Service Queries** (Gap 4.1)
   - Completions (scope-aware)
   - Hover (show types/docs)
   - Go-to-definition
   - **Blocks:** IDE adoption
   - **Effort:** 2 weeks (depends on Gap 2.x)

5. **Verify/Complete Future Code Generation** (Gap 3.5)
   - Ensure async sends create futures
   - Verify return value is future pid
   - Test with REPL
   - **Blocks:** Async-first execution model
   - **Effort:** 3-5 days

### P2 - Medium (Quality & Optimization)

6. **Add Default Fields to Class Registry** (Gap 7.1)
   - Extend ClassInfo with default_fields map
   - Populate from class definitions
   - **Blocks:** Gap 3 (state migration)
   - **Effort:** 2-3 days

7. **Implement Abstract Class Optimization** (Gap 7.2)
   - Check is_abstract flag in code generator
   - Skip instance module for abstract classes
   - **Benefit:** Faster compilation, smaller output
   - **Effort:** 2 days

8. **Complete Super Dispatch Generation** (Gap 3.3)
   - Detect `super` keyword in AST
   - Generate call to `beamtalk_classes:super_dispatch/4`
   - **Blocks:** Inheritance use cases
   - **Effort:** 3-4 days

9. **Implement Query Caching** (Gap 4.3)
   - Integrate Salsa or similar
   - Memoize scope construction, type inference
   - **Benefit:** Sub-50ms LSP responses at scale
   - **Effort:** 1 week

### P3 - Low (Polish & Observability)

10. **Fix Trivia Span Tracking** (Gap 1.1)
    - Lexer tracks comment/whitespace spans
    - Parser uses actual trivia spans
    - **Benefit:** Accurate hover over comments
    - **Effort:** 1 day

11. **Add Hot Reload Event Emission** (Gap 8.2)
    - Define event protocol
    - Emit events from code_change/3
    - **Benefit:** Monitoring, debugging
    - **Effort:** 2-3 days

12. **Verify doesNotUnderstand Dispatch** (Gap 5.1)
    - Test DNU fallback in dispatch
    - Add REPL test cases
    - **Benefit:** Metaprogramming support
    - **Effort:** 1 day

---

## Dependency Graph

```
P0-1 (Semantic Analysis)
  ├──> P1-4 (Language Service Queries)
  └──> P0-2 (Method Dispatch Generation) [partial dependency]

P0-2 (Method Dispatch)
  └──> P2-8 (Super Dispatch) [builds on top]

P1-3 (State Migration)
  ├──> P2-6 (Default Fields in Registry)
  └──> Generated by P0-2 (Method Dispatch)

P1-4 (Language Service)
  ├──> P0-1 (Semantic Analysis) [hard dependency]
  └──> P2-9 (Query Caching) [performance optimization]

P1-5 (Future Codegen)
  ├──> P0-2 (Method Dispatch) [related]
  └──> Independent (can be done in parallel)
```

**Critical Path:** P0-1 → P1-4 (8 weeks)

**Parallel Streams:**
- Stream A: P0-1 → P1-4 → P2-9 (Semantic → Language Service → Caching)
- Stream B: P0-2 → P2-8 (Dispatch → Super Dispatch)
- Stream C: P1-3 → P2-6 (State Migration → Default Fields)
- Stream D: P1-5 (Future Codegen - independent)

---

## Testing Coverage Gaps

### Compiler Tests

**Present:**
- Lexer tests (token generation)
- Parser tests (AST construction)
- Some codegen snapshot tests

**Missing:**
- Semantic analysis tests (can't test what doesn't exist)
- End-to-end compiler tests (source → BEAM → execute)
- Error recovery tests (parser synchronization)
- Language service query tests

### Runtime Tests

**Present:**
- EUnit tests for actor dispatch
- EUnit tests for class registry
- EUnit tests for futures

**Missing:**
- Hot reload integration tests (code_change/3)
- Property tests for future state machine (PropEr)
- Supervision tree tests
- Cross-context integration tests

---

## Summary

### By the Numbers

- **9 Bounded Contexts** total
- **3 Complete** (Future, REPL, Source Analysis)
- **3 Mature** (Actor, Object System, Source Analysis)
- **2 Partial** (Code Gen, Language Service)
- **1 Stub** (Semantic Analysis) - **CRITICAL**
- **1 Stub** (Hot Reload) - Medium priority

### Critical Blockers

1. **Semantic Analysis** - The biggest gap. Without it:
   - Language service can't provide intelligent features
   - Can't validate code before running
   - No foundation for optional type system

2. **Method Dispatch Generation** - Incomplete. Without full implementation:
   - Complex message sends may not work
   - Super calls may fail
   - REPL may not handle all cases

3. **State Migration** - Stubbed. Without it:
   - Hot reload doesn't preserve new fields
   - Can't handle breaking schema changes
   - Limits usefulness of "live programming" promise

### Strengths

- **Runtime is solid** - Actor system, futures, class registry all working
- **Error isolation pattern** - Fully implemented (Flavors pattern)
- **Two-module structure** - Compiler generates correct boilerplate
- **Parser with recovery** - Can handle errors gracefully

### Next Actions

**Immediate (This Sprint):**
1. Verify method dispatch code generation completeness
2. Verify future creation in async message sends
3. Add integration test: compile simple class → spawn → send message

**Next Sprint:**
1. Start semantic analysis implementation (name resolution first)
2. Complete method dispatch generation (all selector types)
3. Add default fields to class registry

**Within Month:**
1. Implement state migration in code_change/3
2. Basic language service queries (completions)
3. End-to-end tests (source → BEAM → execute)

---

## References

- [Beamtalk DDD Model](beamtalk-ddd-model.md) - Domain model this analysis is based on
- [Beamtalk Architecture](beamtalk-architecture.md) - Intended architecture
- [Beamtalk Principles](beamtalk-principles.md) - Core design philosophy
- [Beamtalk Object Model (ADR 0005)](../ADR/0005-beam-object-model-pragmatic-hybrid.md) - Smalltalk → BEAM mapping
- [LFE Flavors](https://github.com/rvirding/flavors) - Reference implementation for OOP patterns
