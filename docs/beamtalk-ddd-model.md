# Beamtalk Domain-Driven Design Model

**Status:** Draft - Comprehensive domain model for compiler and runtime

This document presents a domain-driven design (DDD) analysis of the Beamtalk compiler and runtime system, identifying core domains, bounded contexts, domain entities, value objects, aggregates, and ubiquitous language.

---

## Table of Contents

- [Overview](#overview)
- [Strategic Design](#strategic-design)
  - [Core Domains](#core-domains)
  - [Bounded Contexts](#bounded-contexts)
  - [Context Map](#context-map)
- [Ubiquitous Language](#ubiquitous-language)
- [Compiler Domain Model](#compiler-domain-model)
  - [Source Analysis Context](#source-analysis-context)
  - [Semantic Analysis Context](#semantic-analysis-context)
  - [Code Generation Context](#code-generation-context)
  - [Language Service Context](#language-service-context)
- [Runtime Domain Model](#runtime-domain-model)
  - [Actor System Context](#actor-system-context)
  - [Concurrency Context](#concurrency-context)
  - [Object System Context](#object-system-context)
  - [Hot Reload Context](#hot-reload-context)
  - [Workspace Context](#workspace-context)
  - [REPL Session Context](#repl-session-context)
  - [Beamtalk Global Context](#beamtalk-global-context)
- [Cross-Cutting Concerns](#cross-cutting-concerns)
- [Domain Events](#domain-events)
- [Architecture Decision Records](#architecture-decision-records)

---

## Overview

Beamtalk is a live, interactive Smalltalk-like language for the BEAM VM that brings Smalltalk's legendary live programming experience to Erlang's battle-tested runtime. While inspired by Smalltalk's syntax and philosophy, Beamtalk makes pragmatic choices for BEAM compatibility (see [Syntax Rationale](beamtalk-syntax-rationale.md)). The system comprises two major subsystems:

1. **Compiler** (Rust) - Compiles `.bt` source to BEAM bytecode via Core Erlang
2. **Runtime** (Erlang) - Provides actor execution, futures, class registry, and REPL

The DDD model helps us understand the distinct problem spaces, maintain clear boundaries, and establish a common vocabulary across the team.

---

## Strategic Design

### Core Domains

#### 1. Live Programming Domain (Core - Differentiator)

**What it does:** Enables continuous modification of running code without restarts

**Why it matters:** This is Beamtalk's unique value proposition - Smalltalk-style liveness on BEAM

**Subdomains:**
- Hot code reload with state migration
- Interactive REPL with persistent bindings
- Live actor inspection and modification
- Workspace-driven development

**Strategic importance:** This is what makes Beamtalk different from Gleam, Elixir, or LFE.

#### 2. Actor-Based Execution Domain (Core - Essential)

**What it does:** Maps Smalltalk objects to BEAM processes with async message passing

**Why it matters:** Bridges Smalltalk's object model with BEAM's process model

**Subdomains:**
- Actor lifecycle management
- Async-first message dispatch
- Future/promise implementation
- Fault isolation and supervision

#### 3. Language Compilation Domain (Supporting - Complex but not differentiating)

**What it does:** Transforms Beamtalk source code into executable BEAM bytecode

**Why it matters:** Required for the system to work, but not what makes Beamtalk special

**Subdomains:**
- Lexical analysis and parsing
- Semantic analysis and type checking
- Core Erlang code generation
- BEAM bytecode compilation (via `erlc`)

#### 4. Developer Tooling Domain (Supporting - Enables adoption)

**What it does:** Provides IDE integration, completions, diagnostics, hover info

**Why it matters:** Modern developers expect excellent tooling

**Subdomains:**
- Language Server Protocol (LSP)
- Incremental compilation
- Error recovery and diagnostics
- Source code queries

### Bounded Contexts

A bounded context is an explicit boundary within which a domain model is defined and applicable. Each context has its own ubiquitous language, and terms may mean different things across contexts.

```
┌─────────────────────────────────────────────────────────────────────┐
│                         DEVELOPER MACHINE                            │
│                                                                       │
│  ┌───────────────────────────────────────────────────────────────┐  │
│  │              LANGUAGE SERVICE CONTEXT (Rust)                   │  │
│  │  - Queries: Completions, Hover, Diagnostics                   │  │
│  │  - Incremental: File cache, Query cache                       │  │
│  │  - IDE Integration: LSP Protocol                              │  │
│  └───────────────────────────────────────────────────────────────┘  │
│                              ▲                                        │
│                              │ uses                                   │
│  ┌───────────────────────────┴───────────────────────────────────┐  │
│  │              COMPILER CONTEXT (Rust)                           │  │
│  │  ┌────────────────┐  ┌────────────────┐  ┌─────────────────┐ │  │
│  │  │ SOURCE         │→ │ SEMANTIC       │→ │ CODE            │ │  │
│  │  │ ANALYSIS       │  │ ANALYSIS       │  │ GENERATION      │ │  │
│  │  │ (Lexer/Parser) │  │ (Type/Name)    │  │ (Core Erlang)   │ │  │
│  │  └────────────────┘  └────────────────┘  └─────────────────┘ │  │
│  └───────────────────────────────────────────────────────────────┘  │
│                              │ produces                              │
│                              ▼                                        │
│                      [ .beam bytecode ]                               │
└───────────────────────────────┼───────────────────────────────────────┘
                                │ hot loads into
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                         BEAM RUNTIME                                 │
│                                                                       │
│  ┌───────────────────────────────────────────────────────────────┐  │
│  │              OBJECT SYSTEM CONTEXT (Erlang)                    │  │
│  │  - Class Registry: Global class metadata                      │  │
│  │  - Instance Tracking: ETS-based instance registry             │  │
│  │  - Method Dispatch: Super dispatch, DNU handling              │  │
│  └───────────────────────────────────────────────────────────────┘  │
│                              ▲                                        │
│                              │ uses                                   │
│  ┌───────────────────────────┴───────────────────────────────────┐  │
│  │              ACTOR SYSTEM CONTEXT (Erlang)                     │  │
│  │  - Actor Lifecycle: spawn, init, terminate                     │  │
│  │  - Message Dispatch: async/sync routing                       │  │
│  │  - State Management: Field maps with migration                │  │
│  └───────────────────────────────────────────────────────────────┘  │
│                              ▲                                        │
│                              │ uses                                   │
│  ┌───────────────────────────┴───────────────────────────────────┐  │
│  │              CONCURRENCY CONTEXT (Erlang)                      │  │
│  │  - Future/Promise: Async result handling                       │  │
│  │  - Process Monitors: Lifecycle tracking                        │  │
│  │  - Supervision: Restart strategies                             │  │
│  └───────────────────────────────────────────────────────────────┘  │
│                                                                       │
│  ┌───────────────────────────────────────────────────────────────┐  │
│  │              HOT RELOAD CONTEXT (Erlang)                       │  │
│  │  - Code Loading: BEAM code upgrade                            │  │
│  │  - State Migration: code_change/3 callbacks                   │  │
│  │  - Version Coexistence: Old/new code                          │  │
│  └───────────────────────────────────────────────────────────────┘  │
│                                                                       │
│  ┌───────────────────────────────────────────────────────────────┐  │
│  │              WORKSPACE CONTEXT (Erlang)                        │  │
│  │  - Lifecycle: detached BEAM node, idle cleanup                │  │
│  │  - Supervision: ActorSupervisor, SessionSupervisor            │  │
│  │  - Metadata: workspace_id, project_path, last_activity        │  │
│  └───────────────────────────────────────────────────────────────┘  │
│                              ▲                                        │
│                              │ hosts                                  │
│  ┌───────────────────────────┴───────────────────────────────────┐  │
│  │              REPL SESSION CONTEXT (Erlang)                     │  │
│  │  - Expression Evaluation: On-demand compilation               │  │
│  │  - Binding Management: Per-session variable state             │  │
│  │  - Protocol: WebSocket JSON (nREPL-inspired)                  │  │
│  └───────────────────────────────────────────────────────────────┘  │
│                              ▲                                        │
│                              │ exposes via                            │
│  ┌───────────────────────────┴───────────────────────────────────┐  │
│  │              BEAMTALK GLOBAL CONTEXT (Erlang)                  │  │
│  │  - Runtime introspection: actors, modules, sessions           │  │
│  │  - Project metadata: version, nodeName, projectPath           │  │
│  └───────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────┘
```

### Context Map

The context map shows how bounded contexts relate and communicate:

| Upstream Context | Downstream Context | Relationship | Integration |
|-----------------|-------------------|--------------|-------------|
| SOURCE ANALYSIS | SEMANTIC ANALYSIS | Customer-Supplier | AST + Spans |
| SEMANTIC ANALYSIS | CODE GENERATION | Customer-Supplier | Typed AST |
| CODE GENERATION | ACTOR SYSTEM | Published Language | Core Erlang → BEAM |
| COMPILER | LANGUAGE SERVICE | Shared Kernel | AST, Spans, Errors |
| OBJECT SYSTEM | ACTOR SYSTEM | Conformist | Calls class registry APIs |
| CONCURRENCY | ACTOR SYSTEM | Partnership | Bidirectional dependencies |
| HOT RELOAD | ACTOR SYSTEM | Customer-Supplier | Triggers state migration |
| ACTOR SYSTEM | REPL SESSION | Anti-Corruption Layer | REPL wraps raw gen_server |
| WORKSPACE | REPL SESSION | Customer-Supplier | Workspace provides actor sup, module registry |
| WORKSPACE | HOT RELOAD | Customer-Supplier | Workspace triggers code upgrades via loader |
| BEAMTALK GLOBAL | WORKSPACE | Façade | Single entry point exposing workspace state to user code |

**Key relationships:**

- **Customer-Supplier:** Clear upstream/downstream with explicit contracts
- **Partnership:** Mutual dependency requires close collaboration
- **Shared Kernel:** Carefully managed shared code (minimize this)
- **Published Language:** Well-defined intermediate format (Core Erlang)
- **Conformist:** Downstream accepts upstream's model wholesale
- **Anti-Corruption Layer:** Downstream protects itself with translation layer

---

## Ubiquitous Language

A shared vocabulary used consistently across team, code, and documentation. Terms have precise meanings within their bounded context.

### Compiler Domain Terms

| Term | Definition | Context |
|------|------------|---------|
| **Span** | Source code location (byte offset range) | SOURCE ANALYSIS |
| **Token** | Lexical unit with type and span | SOURCE ANALYSIS |
| **AST Node** | Abstract syntax tree element with span | SOURCE ANALYSIS |
| **Expression** | AST node that evaluates to a value | SOURCE ANALYSIS |
| **Identifier** | Named reference (variable, class, method) | SOURCE ANALYSIS |
| **Selector** | Message name (unary, binary, keyword) | SOURCE ANALYSIS |
| **Binding** | Variable name to value association | SEMANTIC ANALYSIS |
| **Scope** | Naming context (local, instance, class) | SEMANTIC ANALYSIS |
| **Type Annotation** | Optional type constraint on identifier | SEMANTIC ANALYSIS |
| **Core Erlang** | Intermediate representation (IR) | CODE GENERATION |
| **Module** | Compilation unit (file containing classes/expressions) | All COMPILER |
| **Diagnostic** | Error, warning, or info message with span | LANGUAGE SERVICE |
| **Query** | Request for IDE information (hover, completion) | LANGUAGE SERVICE |
| **Completion** | Suggested code at cursor position | LANGUAGE SERVICE |

### Runtime Domain Terms

| Term | Definition | Context |
|------|------------|---------|
| **Actor** | BEAM process representing a Beamtalk object | ACTOR SYSTEM |
| **Message Send** | Async or sync communication between actors | ACTOR SYSTEM |
| **Selector** | Method identifier (atom like `increment` or `'at:put:'`) | ACTOR SYSTEM |
| **State Map** | Actor's field storage (Erlang map) | ACTOR SYSTEM |
| **Self** | Actor's own reference (#beamtalk_object record) | ACTOR SYSTEM |
| **Method** | Function implementing message handler | ACTOR SYSTEM |
| **doesNotUnderstand** | Catch-all handler for unknown messages | ACTOR SYSTEM |
| **Future** | Async result handle (BEAM process) | CONCURRENCY |
| **Promise** | (Synonym for Future) | CONCURRENCY |
| **Await** | Blocking wait for future result | CONCURRENCY |
| **Resolve** | Complete future with success value | CONCURRENCY |
| **Reject** | Complete future with error reason | CONCURRENCY |
| **Callback** | Function executed on future completion | CONCURRENCY |
| **Class** | Template defining actor behavior | OBJECT SYSTEM |
| **Instance** | Running actor (gen_server process) | OBJECT SYSTEM |
| **Superclass** | Parent class in inheritance hierarchy | OBJECT SYSTEM |
| **Method Dispatch** | Routing selector to implementation | OBJECT SYSTEM |
| **Super Dispatch** | Invoke superclass method implementation | OBJECT SYSTEM |
| **Class Registry** | Global map of class metadata | OBJECT SYSTEM |
| **Instance Tracking** | ETS table of all live instances | OBJECT SYSTEM |
| **Hot Reload** | Loading new code into running system | HOT RELOAD |
| **Code Upgrade** | BEAM's hot swap mechanism | HOT RELOAD |
| **State Migration** | Transforming state for new code version | HOT RELOAD |
| **code_change/3** | OTP callback for state migration | HOT RELOAD |
| **REPL** | Read-Eval-Print Loop | REPL SESSION |
| **Binding** | Variable name to value association (session-local, not persisted across reconnects) | REPL SESSION |
| **Evaluation** | Compile + load + execute expression in REPL session | REPL SESSION |
| **Session** | Single REPL connection with its own bindings and eval counter; ephemeral | REPL SESSION |
| **EvalRequest** | Protocol message requesting expression evaluation | REPL SESSION |
| **EvalResponse** | Protocol message with result value, stdout output, warnings, status | REPL SESSION |
| **ProtocolMessage** | JSON WebSocket frame following nREPL-inspired op/id/session schema | REPL SESSION |
| **Workspace** | Long-lived detached BEAM node providing shared actors, modules, metadata | WORKSPACE |
| **WorkspaceId** | Unique identifier for a workspace (SHA256 of project path or explicit name) | WORKSPACE |
| **ProjectPath** | Filesystem path of the project this workspace belongs to | WORKSPACE |
| **IdleTimeout** | Inactivity period after which a workspace self-terminates (default 4 hours) | WORKSPACE |
| **ActivityTimestamp** | Unix timestamp of last REPL, actor, or code-reload activity in the workspace | WORKSPACE |
| **Beamtalk Global** | Runtime global object (analogous to `Smalltalk`) accessible in all sessions | BEAMTALK GLOBAL |

### Shared Terms (Cross-Context)

| Term | Definition | Usage |
|------|------------|-------|
| **Class Definition** | Declaration of actor template | COMPILER → RUNTIME |
| **Method Definition** | Declaration of message handler | COMPILER → RUNTIME |
| **State Declaration** | Instance variable definition | COMPILER → RUNTIME |
| **Beamtalk Object** | Record bundling class + pid | RUNTIME (cross-context) |

---

## Compiler Domain Model

### Source Analysis Context

**Purpose:** Transform raw text into structured AST

**Aggregates:**

#### 1. Module (Aggregate Root)

**Invariants:**
- Must have a valid span covering entire source
- Expressions and classes must have non-overlapping spans
- All child nodes' spans must be contained within module span

**Entities:**
- `ClassDefinition`: Name, superclass, state, methods
- `Expression`: Assignments, literals, message sends, blocks, etc.
- `Comment`: Documentation and inline comments

**Value Objects:**
- `Span`: (start: usize, end: usize) - immutable source location
- `Identifier`: (name: EcoString, span: Span)
- `Literal`: Integer, Float, String, Symbol, Array, Map
- `Token`: (type: TokenType, text: EcoString, span: Span)

**Repositories:**
- `FileCache`: In-memory map of path → parsed Module
- `SourceRepository`: Reads files from filesystem

**Domain Services:**
- `Lexer`: source text → token stream
- `Parser`: token stream → AST (with error recovery)
- `SyntaxValidator`: AST → syntax diagnostics

**Key Patterns:**
- **Error Recovery:** Parser continues after errors, inserts `Expression::Error` nodes
- **Immutable AST:** Once parsed, AST nodes never mutate (use new parse for changes)
- **Span Tracking:** Every node knows its source location for IDE features

**Example Domain Logic:**

```rust
// Parsing maintains span invariants
impl Parser {
    fn parse_expression(&mut self) -> Expression {
        let start = self.current_token().span.start;
        let expr = self.parse_primary();
        let end = self.previous_token().span.end;
        expr.with_span(Span::new(start, end)) // Span encapsulates child spans
    }
}
```

### Semantic Analysis Context

**Purpose:** Resolve names, check types, validate semantics

**Aggregates:**

#### 1. Scope (Aggregate Root)

**Invariants:**
- Child scopes can access parent bindings (shadowing allowed)
- Bindings within scope must have unique names
- Each scope has exactly one parent (except global scope)

**Entities:**
- `LocalScope`: Block or method local variables
- `InstanceScope`: Actor instance fields
- `ClassScope`: Class-level bindings

**Value Objects:**
- `Binding`: (name: Identifier, type: Type, kind: BindingKind)
- `Type`: Declared or inferred type information
- `TypeAnnotation`: Optional type constraint

**Repositories:**
- `ScopeGraph`: Hierarchical scope structure for name resolution

**Domain Services:**
- `NameResolver`: Resolves identifiers to bindings
- `TypeChecker`: Validates type constraints
- `SemanticValidator`: Validates message send arity, undefined names
- `BlockContextClassifier`: Determines block context (ControlFlow/Stored/Passed/Other)

**Key Patterns:**
- **Scope Chain:** Walk parent scopes for name resolution
- **Type Inference:** Bottom-up type propagation through AST
- **Lazy Analysis:** Only analyze what's queried (for IDE responsiveness)

**Example Domain Logic:**

```rust
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

impl BlockContextClassifier {
    fn classify(&self, block_span: Span, parent: &Expression, in_assignment: bool) -> BlockContext {
        // Stored: block on RHS of assignment
        if in_assignment {
            return BlockContext::Stored;
        }
        // ControlFlow: literal block in control flow selector
        // Passed: block variable in any argument position
        // Other: return value, nested blocks, etc.
        // (see block_context.rs for full implementation)
    }
}
```

### Code Generation Context

**Purpose:** Transform AST into executable Core Erlang

**Class Kind Routing** (see [ADR 0007](ADR/0007-compilable-stdlib-with-primitive-injection.md)):

The code generator routes class definitions through one of three paths based on class kind:

| Class Kind | Routing | Generated Code |
|------------|---------|----------------|
| **Actor** | `generate_actor_module()` | gen_server with `init/1`, `handle_call/3`, `handle_cast/3`, `spawn/0` |
| **Value Type** | `generate_value_type_module()` | Map-backed with `new/0`, `new/1`, pure function methods |
| **Primitive Type** | `generate_primitive_module()` | Method table only — no `new`, no state management |

Class kind is determined from compiled stdlib metadata: Primitive Types match a known set (Integer, String, etc.), Actors inherit from Actor, everything else is a Value Type.

**Aggregates:**

#### 1. CompilationUnit (Aggregate Root)

**Invariants:**
- Every class generates exactly two modules (class + instance)
- Generated module names must be valid Erlang atoms
- All method arities must match selector keyword count

**Entities:**
- `GeneratedModule`: Core Erlang module definition
- `GeneratedFunction`: Erlang function with arity
- `GeneratedClause`: Function clause with pattern + body

**Value Objects:**
- `ErlangAtom`: Validated Erlang atom (no spaces, lowercase start)
- `ErlangVar`: Erlang variable (uppercase start)
- `ErlangExpr`: Core Erlang expression tree
- `ModuleName`: Namespaced module identifier

**Factories:**
- `CoreErlangGenerator`: Creates Core Erlang AST from Beamtalk AST
- `ModuleFactory`: Generates module boilerplate

**Domain Services:**
- `SelectorMangler`: Converts Beamtalk selectors to Erlang atoms (`'at:put:'`)
- `DispatchCodegen`: Generates method dispatch logic
- `StateCodegen`: Generates state map initialization
- `FutureCodegen`: Generates async message send with future

**Key Patterns:**
- **Two-Module Pattern:** Each class → `beamtalk_<class>_class` + `beamtalk_<class>` (following Flavors)
- **Dispatch Table:** Method map in state enables dynamic dispatch
- **Self Record:** `#beamtalk_object{}` bundles class + pid for reflection

**Example Domain Logic:**

```rust
impl CoreErlangGenerator {
    fn generate_async_send(&self, receiver: Expr, selector: Selector, args: Vec<Expr>) -> ErlangExpr {
        // Future creation
        let future = ErlangExpr::call("beamtalk_future", "new", vec![]);
        
        // Extract pid from #beamtalk_object
        let pid = ErlangExpr::call("erlang", "element", vec![
            ErlangExpr::literal(4), // pid is 4th field
            receiver.clone(),
        ]);
        
        // Async cast
        let cast = ErlangExpr::call("gen_server", "cast", vec![
            pid,
            ErlangExpr::tuple(vec![
                ErlangExpr::atom(selector.to_atom()),
                ErlangExpr::list(args),
                future.clone(),
            ]),
        ]);
        
        // Return future
        ErlangExpr::let_binding("Future", future, ErlangExpr::seq(vec![cast, future]))
    }
}
```

### Standard Library Context

**Purpose:** Compile `stdlib/src/*.bt` files into class metadata and BEAM modules via pragma-based primitive injection

**DDD Context:** Standard Library (see [ADR 0007](ADR/0007-compilable-stdlib-with-primitive-injection.md))

**Key Concepts:**

| Term | Definition |
|------|-----------|
| **Pragma** | In-body annotation (`@primitive name`) declaring a method's implementation |
| **Named Intrinsic** | Entry in compiler's finite registry mapping name → code generation function |
| **Primitive Type** | Class backed by native Erlang value (Integer, String, etc.) — method table only, no constructor |
| **Runtime Dispatch Module** | Erlang module (e.g., `beamtalk_integer.erl`) providing type checking, structured errors, extension registry |

**Relationships:**
- **Consumes from:** Source Analysis Context (parsed AST from `stdlib/src/*.bt` files)
- **Feeds into:** Code Generation Context (class metadata, three-kind routing, intrinsic bindings)
- **Collaborates with:** Object System Context (runtime dispatch modules execute the primitives)

**Domain Services:**
- `IntrinsicRegistry`: Maps intrinsic names to code generation functions (~35 entries)
- `StdlibCompiler`: Compiles `stdlib/src/*.bt` through the normal pipeline, producing class metadata
- `ClassKindResolver`: Determines Actor / Value Type / Primitive from class name and superclass

**Key Invariant:** Every intrinsic name must resolve to exactly one code generation function. Unknown names are compile errors.

### Language Service Context

**Purpose:** Answer IDE queries incrementally

**Aggregates:**

#### 1. LanguageService (Aggregate Root)

**Invariants:**
- File cache entries match filesystem state (or marked stale)
- Query results always reference current file versions
- Response times < 100ms (soft invariant)

**Entities:**
- `CachedFile`: Parsed module + version + last modified time
- `DiagnosticSet`: Errors/warnings for a file
- `CompletionList`: Suggestions at a position

**Value Objects:** (defined in `language_service/value_objects.rs`)
- `ByteOffset`: Type-safe byte offset in source text
- `Position`: (line: u32, column: u32) with UTF-8 aware conversion
- `Location`: (file: Utf8PathBuf, span: Span)
- `Diagnostic`: Re-exported from parse diagnostics
- `Completion`: (label, kind, detail, documentation)
- `CompletionKind`: Enum for completion types (Function, Variable, Class, etc.)
- `HoverInfo`: (contents, documentation, span)

**Repositories:**
- `FileCache`: Map of path → CachedFile
- `QueryCache`: Memoized query results (Salsa-style)

#### 2. ProjectIndex (Aggregate Root)

**Purpose:** Cross-file class hierarchy and symbol index (ADR 0024, Phase 1)

**Invariants:**
- Merged hierarchy always reflects the current set of indexed files
- Stdlib classes are pre-indexed and never removed during file updates
- Built-in classes are never overwritten or removed

**Value Objects:**
- Per-file class name tracking (which classes came from which file)

**Key Operations:**
- `update_file`: Add/replace a file's class contributions
- `remove_file`: Remove a file's classes (preserving stdlib)
- `with_stdlib`: Pre-index stdlib class definitions from `stdlib/src/*.bt`

**Domain Services:**
- `CompletionProvider`: Suggest completions at cursor
- `DiagnosticProvider`: Collect errors/warnings
- `HoverProvider`: Show info on hover
- `DefinitionProvider`: Go-to-definition support (single-file and cross-file)

**Key Patterns:**
- **Incremental Parsing:** Only re-parse changed files
- **Query Caching:** Memoize expensive analysis results
- **Lazy Evaluation:** Don't compute until queried
- **Error Recovery:** Never block IDE on parse errors

**Example Domain Logic:**

```rust
impl LanguageService {
    pub fn completions(&mut self, path: &Path, position: Position) -> Vec<Completion> {
        // Get or parse file
        let file = self.get_or_parse_file(path);
        
        // Find AST node at position
        let node = file.module.node_at_position(position)?;
        
        // Scope-aware completions
        let scope = self.scope_at_position(&file.module, position);
        scope.bindings().iter()
            .map(|b| Completion {
                label: b.name.clone(),
                kind: CompletionKind::Variable,
                detail: Some(b.type_.to_string()),
                insert_text: b.name.clone(),
            })
            .collect()
    }
}
```

---

## Runtime Domain Model

### Actor System Context

**Purpose:** Execute Beamtalk actors as BEAM processes

**Aggregates:**

#### 1. Actor (Aggregate Root)

**Invariants:**
- State map must contain `'__class__'` and `'__methods__'` keys
- Actor pid must be valid and alive
- Method arity must match selector keyword count
- Self reference must match actor's actual class and pid

**Entities:**
- `ActorProcess`: gen_server implementing message dispatch
- `StateMap`: Field storage (Erlang map)
- `MethodTable`: Selector → implementation function map

**Value Objects:**
- `BeamtalkObject`: Record (class, class_mod, pid)
- `Selector`: Method name atom
- `MessageArgs`: List of argument values
- `DispatchResult`: {reply, Result, NewState} | {noreply, NewState} | {error, Reason, State}

**Factories:**
- `ActorFactory`: Spawns new actor instances via `start_link/2`

**Repositories:**
- (None - actors are ephemeral processes)

**Domain Services:**
- `MessageDispatcher`: Routes selector to method implementation
- `SuperDispatcher`: Walks class chain for super method
- `ErrorIsolator`: Catches errors, converts to future rejections (from Flavors)

**Key Patterns:**
- **Process-per-Actor:** Each Beamtalk object is a BEAM process
- **Error Isolation:** Errors don't crash the instance, returned to caller (Flavors pattern)
- **Self Record:** Pass `#beamtalk_object{}` to methods for reflection
- **Dual Dispatch Modes:** Async (cast + future) vs. Sync (call)

**Example Domain Logic:**

```erlang
%% Message dispatch with error isolation (from Flavors)
dispatch(Selector, Args, Self, State) ->
    Methods = maps:get('__methods__', State),
    case maps:find(Selector, Methods) of
        {ok, Fun} ->
            try
                {Result, NewState} = Fun(Self, State, Args),
                {reply, Result, NewState}
            catch
                error:Error -> {error, {error, Error}, State};
                exit:Exit -> {error, {exit, Exit}, State};
                throw:Thrown -> {error, {throw, Thrown}, State}
            end;
        error ->
            handle_dnu(Selector, Args, Self, State)
    end.
```

### Concurrency Context

**Purpose:** Manage async communication via futures

**Aggregates:**

#### 1. Future (Aggregate Root)

**Invariants:**
- Can only transition pending → resolved OR pending → rejected (one-way)
- Once resolved/rejected, value is immutable
- Waiters list only exists in pending state
- Resolved future terminates after 5 minutes of inactivity

**State Machine:**
```
    ┌─────────┐  resolve(Value)   ┌──────────┐
    │ pending │──────────────────→ │ resolved │─→ terminates
    └─────────┘                    └──────────┘   (5 min idle)
         │
         │ reject(Reason)
         ▼
    ┌──────────┐
    │ rejected │─→ terminates
    └──────────┘   (5 min idle)
```

**Entities:**
- `FutureProcess`: Process holding future state

**Value Objects:**
- `FuturePid`: Process ID of future (lightweight ~2KB)
- `FutureResult`: Value or reason
- `Waiter`: {await, Pid, Timeout} or {callback, Kind, Fun}

**Repositories:**
- (None - futures are ephemeral processes)

**Domain Services:**
- `FutureFactory`: Creates new pending futures
- `CallbackExecutor`: Runs callbacks on resolution/rejection
- `WaiterNotifier`: Sends messages to awaiting processes

**Key Patterns:**
- **Process-per-Future:** Each future is a lightweight process
- **Mailbox as State:** Future state = message handling behavior
- **Callback Registration:** Store callbacks as waiters
- **Timeout Handling:** Send timeout messages to waiters
- **Natural GC:** Futures garbage collected when no references remain

**Example Domain Logic:**

```erlang
%% Future state machine
pending(Waiters) ->
    receive
        {resolve, Value} ->
            notify_waiters(Waiters, resolved, Value),
            resolved(Value);
        {await, Pid} ->
            pending([{await, Pid, infinity} | Waiters]);
        {add_callback, resolved, Callback} ->
            pending([{callback, resolved, Callback} | Waiters])
    end.

resolved(Value) ->
    receive
        {await, Pid} ->
            Pid ! {future_resolved, self(), Value},
            resolved(Value);
        {add_callback, resolved, Callback} ->
            Callback(Value),
            resolved(Value)
    after 300000 -> % 5 minutes
        ok  % Terminate to prevent leaks
    end.
```

### Object System Context

**Purpose:** Manage class metadata and instance tracking

**Aggregates:**

#### 1. ClassRegistry (Aggregate Root)

**Invariants:**
- Each class name maps to exactly one ClassInfo
- Superclass must exist in registry (except for root classes)
- Method selectors must be unique within a class
- Instance variables must have unique names

**Entities:**
- `ClassInfo`: Metadata for a single class
- `MethodInfo`: Metadata for a single method

**Value Objects:**
- `ClassName`: Atom identifying class
- `ModuleName`: Compiled BEAM module name
- `InstanceVariable`: Field name atom
- `MethodArity`: Number of arguments

**Repositories:**
- `ClassRegistry`: gen_server holding class map
- `InstanceRegistry`: ETS table tracking live instances

**Domain Services:**
- `ClassLookup`: Find class by name
- `InheritanceWalker`: Traverse class hierarchy
- `MethodResolver`: Find method in class or superclasses
- `InstanceTracker`: Monitor and count instances

**Key Patterns:**
- **Centralized Registry:** Single gen_server for all class metadata
- **ETS for Instances:** Fast concurrent instance lookup
- **Process Monitors:** Auto-unregister instances on termination
- **Lazy Instance Tracking:** Only track if `allInstances` used

**Example Domain Logic:**

```erlang
%% Super method dispatch - walks inheritance chain
find_and_invoke_super_method(ServerRef, Superclass, Selector, Args, State) ->
    case lookup(ServerRef, Superclass) of
        {ok, ClassInfo} ->
            Methods = maps:get(methods, ClassInfo),
            case maps:find(Selector, Methods) of
                {ok, MethodInfo} ->
                    invoke_method(MethodInfo, Args, State);
                error ->
                    %% Not in this class, try its superclass
                    case maps:get(superclass, ClassInfo) of
                        none -> {error, {method_not_found, Superclass, Selector}};
                        NextSuper -> find_and_invoke_super_method(ServerRef, NextSuper, Selector, Args, State)
                    end
            end;
        undefined ->
            {error, {class_not_found, Superclass}}
    end.
```

### Hot Reload Context

**Purpose:** Update running code without losing state

**Aggregates:**

#### 1. CodeUpgrade (Aggregate Root)

**Invariants:**
- Old and new code versions coexist during upgrade
- State migration must succeed or rollback
- All instances receive code_change/3 callback opportunity

**Entities:**
- `ModuleVersion`: Old or new version of module
- `StateMigration`: Transformation from old state schema to new

**Value Objects:**
- `OldState`: State map before upgrade
- `NewState`: State map after migration
- `UpgradeVersion`: Old version identifier

**Repositories:**
- (Uses BEAM's internal code server)

**Domain Services:**
- `CodeLoader`: Loads new BEAM bytecode
- `StateMigrator`: Executes code_change/3 callbacks (implemented in `beamtalk_hot_reload`)
- `InstanceUpgrader`: Triggers sys:change_code/4 for instances

**Key Patterns:**
- **Two-Version Coexistence:** Old and new code both loaded
- **Lazy Upgrade:** Processes upgrade on next fully-qualified call
- **code_change/3 Callback:** OTP's state migration mechanism
- **Automatic Field Migration:** Add defaults, preserve unknowns

**Example Domain Logic:**

```erlang
%% Domain service: beamtalk_hot_reload
%% Centralizes code_change/3 callback logic for all gen_server behaviors
code_change(OldVsn, OldState, Extra) ->
    %% Current implementation: preserve state unchanged
    %% Future: automatic field migration as shown below
    {ok, OldState}.

%% Future implementation (when field defaults are stored in class registry):
%% code_change(OldVsn, OldState, Extra) ->
%%     %% Get new field defaults from class metadata
%%     Class = maps:get('__class__', OldState),
%%     {ok, ClassInfo} = beamtalk_classes:lookup(Class),
%%     DefaultFields = maps:get(default_fields, ClassInfo),
%%     
%%     %% Merge: new defaults + existing fields (existing take precedence)
%%     NewState = maps:merge(DefaultFields, OldState),
%%     
%%     %% Call user-defined migration if present
%%     case maps:find('__migrate__', maps:get('__methods__', NewState, #{})) of
%%         {ok, MigrateFun} ->
%%             {ok, MigrateFun(OldVsn, NewState, Extra)};
%%         error ->
%%             {ok, NewState}
%%     end.
```

### Workspace Context

**Purpose:** Manage the lifecycle and shared state of a long-lived detached BEAM node (the "workspace") that survives REPL disconnects, providing persistent actors, loaded modules, and project metadata to all connected sessions.

**Aggregates:**

#### 1. WorkspaceMeta (Aggregate Root)

**Invariants:**
- Workspace ID is immutable once assigned (SHA256 of project path or explicit name)
- `last_activity` must be updated on every user interaction (session connect, actor spawn, code reload)
- `supervised_actors` tracks only live PIDs (dead PIDs are unregistered via process monitors)
- `loaded_modules` tracks only modules loaded through the workspace bootstrap or REPL load ops

**Entities:**
- `WorkspaceMeta`: gen_server holding workspace identity, timestamps, actor PIDs, loaded module list

**Value Objects:**
- `WorkspaceId`: binary identifier (e.g. `<<"my-feature">>` or SHA256 hex digest of project path)
- `ProjectPath`: absolute filesystem path for the project
- `ActivityTimestamp`: Unix timestamp (integer seconds) of last observed activity
- `NodeName`: Erlang atom identifying this workspace's BEAM node

**Repositories:**
- `WorkspaceMetaServer`: gen_server (`beamtalk_workspace_meta`) holding state; persists to `~/.beamtalk/workspaces/{id}/metadata.json` with debounced writes

**Domain Services:**
- `IdleMonitor` (`beamtalk_idle_monitor`): Periodic timer; calls `init:stop/0` if idle time exceeds `max_idle_seconds`. Disabled for persistent/production nodes.
- `ActorSupervisor` (`beamtalk_actor_sup`): `simple_one_for_one` supervisor for all user actors in the workspace; actors are `temporary` (no restart on crash by default)
- `SessionSupervisor` (`beamtalk_session_sup`): `simple_one_for_one` supervisor for REPL session shell processes; sessions are `temporary`
- `WorkspaceBootstrap` (`beamtalk_workspace_bootstrap`): Startup logic — reads config, starts stdlib, initialises metadata
- `WorkspaceInterface` (`beamtalk_workspace_interface`): Public API façade used by the REPL ops handlers

**Key Patterns:**
- **Node-per-Workspace:** Each workspace is a separate BEAM node (isolated code path, separate port/cookie)
- **Self-Terminating Idle Monitor:** No external daemon needed; each node watches itself and calls `init:stop/0` when idle
- **Process Monitors for Actor Tracking:** `WorkspaceMeta` monitors each supervised actor PID; cleans up registry on `DOWN` messages
- **Debounced Metadata Persistence:** Disk writes are coalesced with a 2-second timer to avoid write storms during high-activity periods

**Example Domain Logic:**

```erlang
%% Domain service: beamtalk_idle_monitor
%% Periodically checks idle time and self-terminates
handle_info(check_idle, #state{enabled = true, max_idle_seconds = Max} = State) ->
    LastActivity = beamtalk_workspace_meta:get_last_activity(),
    IdleSeconds = erlang:system_time(second) - LastActivity,
    case IdleSeconds > Max of
        true ->
            ?LOG_INFO("Workspace idle for ~p seconds, shutting down", [IdleSeconds]),
            init:stop();
        false ->
            ok
    end,
    TRef = erlang:send_after(?CHECK_INTERVAL, self(), check_idle),
    {noreply, State#state{timer_ref = TRef}}.

%% Aggregate root: beamtalk_workspace_meta
%% Tracks supervised actors via process monitors for automatic cleanup
register_actor(Pid) ->
    MonRef = erlang:monitor(process, Pid),
    gen_server:call(?MODULE, {register_actor, Pid, MonRef}).

handle_info({'DOWN', _MonRef, process, Pid, _Reason}, State) ->
    NewActors = lists:delete(Pid, State#state.supervised_actors),
    {noreply, State#state{supervised_actors = NewActors}}.
```

### REPL Session Context

**Purpose:** Manage a single REPL client connection — evaluate expressions, maintain per-session variable bindings, and translate protocol messages to/from runtime operations.

**Aggregates:**

#### 1. Session (Aggregate Root)

**Invariants:**
- Each session has a unique session ID assigned at connection time
- Bindings map is consistent: variable names map to live values (dead PIDs are not automatically removed — this is intentional, matching Erlang shell semantics)
- `eval_counter` is monotonically increasing; used to generate unique temp module names (`beamtalk_repl_eval_{N}`)
- Sessions are ephemeral: when the WebSocket closes, the session process terminates and bindings are lost

**Entities:**
- `SessionProcess` (`beamtalk_repl_shell`): gen_server owning session bindings, eval counter, module tracker, class source cache

**Value Objects:**
- `Binding`: `{Name :: atom(), Value :: term()}` — variable name to runtime value
- `SessionId`: binary identifier, assigned at connection (e.g. `<<"session_1234_5678">>`)
- `EvalRequest`: `{expression: string(), subscriber: pid() | undefined}` — expression to evaluate with optional streaming subscriber
- `EvalResponse`: `{ok | error, Value, StdoutCapture :: binary(), Warnings :: [binary()], NewState}` — structured result
- `ProtocolMessage`: JSON map with `op`, `id`, `session` fields following the nREPL-inspired schema
- `ClassSource`: `{ClassName :: binary(), Source :: string()}` — cached inline class source for method patching (BT-571)

**Repositories:**
- (None — sessions are ephemeral processes; bindings live only in process memory)

**Domain Services:**
- `Evaluator` (`beamtalk_repl_eval`): Core domain service. Compiles expression via compiler port, loads bytecode, executes, captures stdout. Handles class definitions, method patches, and module lifecycle separately from plain expression evaluation.
- `BindingManager` (`beamtalk_repl_state`, `beamtalk_repl_ops_session`): Get/set/clear bindings. Injects bindings into compiled module before execution; extracts new bindings after.
- `ProtocolHandler` (`beamtalk_repl_protocol`): Encodes/decodes JSON WebSocket frames. Detects legacy vs. nREPL-style format. Routes ops to operation handlers.
- `IoCapture` (`beamtalk_io_capture`): Intercepts `io:format` and `io:put_chars` calls during evaluation; forwards chunks to streaming subscriber for real-time output delivery (BT-696).
- `ModuleTracker` (`beamtalk_repl_modules`): Tracks which modules were loaded in this session, their source files, and load timestamps. Used by `modules` and `unload` ops.
- `ActorRegistry` (`beamtalk_repl_actors`): Per-session registry of spawned actors (pid → class mapping). Used by `actors`, `inspect`, `kill` ops.

**Operation Handlers (Domain Services):**
- `OpsEval` (`beamtalk_repl_ops_eval`): `eval`, `show-codegen`, `stdin`
- `OpsSession` (`beamtalk_repl_ops_session`): `bindings`, `clear`, `clone`, `close`, `sessions`
- `OpsLoad` (`beamtalk_repl_ops_load`): `load-file`, `load-source`, `reload`
- `OpsActors` (`beamtalk_repl_ops_actors`): `actors`, `inspect`, `kill`
- `OpsDev` (`beamtalk_repl_ops_dev`): `test`, `test-all`, `describe`, `health`, `shutdown`
- `Docs` (`beamtalk_repl_docs`): `info`, `complete`, `docs`

**Key Patterns:**
- **Ephemeral Sessions:** Session process dies when WebSocket closes; actors spawned during session continue in `ActorSupervisor`
- **Eval Counter Isolation:** Each expression compiles to a unique temp module name, preventing collisions across concurrent sessions
- **Streaming Output:** IoCapture intercepts stdout during eval and forwards chunks to the WebSocket handler before the final response (BT-696)
- **Stdin Blocking:** Eval can suspend waiting for `stdin` op from client; handled via process mailbox (BT-698)
- **Anti-Corruption Layer:** REPL never calls `gen_server:cast/call` on actor PIDs directly; routes through workspace-level ActorSupervisor and class registry APIs

**Example Domain Logic:**

```erlang
%% Domain service: beamtalk_repl_eval
%% Core evaluation pipeline: compile → load → execute → capture result
do_eval(Expression, State, Subscriber) ->
    Counter = beamtalk_repl_state:get_eval_counter(State),
    ModuleName = list_to_atom("beamtalk_repl_eval_" ++ integer_to_list(Counter)),
    NewState = beamtalk_repl_state:increment_eval_counter(State),
    Bindings = beamtalk_repl_state:get_bindings(State),
    case compile_expression(Expression, ModuleName, Bindings) of
        {ok, class_definition, ClassInfo, Warnings} ->
            handle_class_definition(ClassInfo, Warnings, Expression, NewState);
        {ok, Beam, NewBindings, Warnings} ->
            {Output, Result} = beamtalk_io_capture:capture(Subscriber, fun() ->
                activate_module(ModuleName, Beam),
                ModuleName:eval()
            end),
            FinalState = beamtalk_repl_state:set_bindings(NewState, NewBindings),
            {ok, Result, Output, Warnings, FinalState};
        {error, Reason, Warnings} ->
            {error, Reason, <<>>, Warnings, NewState}
    end.
```

### Beamtalk Global Context

**Purpose:** Expose a stable, Smalltalk-inspired global object (`Beamtalk`) that gives user code and REPL sessions a single entry point for runtime introspection — listing actors, modules, classes, and sessions — without exposing raw Erlang APIs.

**Analogy:** `Beamtalk` is to Beamtalk what `Smalltalk` is to Pharo: a globally accessible system dictionary and runtime façade.

**Aggregates:**

#### 1. BeamtalkGlobal (Singleton Façade)

**Invariants:**
- Always available in every REPL session (no import needed)
- Delegates to `beamtalk_workspace_meta`, `beamtalk_class_registry`, and `beamtalk_object_instances` — never holds state itself
- Read-only introspection API; mutation is done via normal actor message sends

**Entities:**
- (None — stateless façade, no mutable entities)

**Value Objects:**
- `ClassList`: Ordered list of all registered class names
- `ActorList`: List of `{pid, class, spawned_at}` tuples for all supervised actors
- `SessionList`: List of active REPL session IDs
- `ModuleList`: List of loaded Beamtalk module atoms

**Domain Services:**
- `BeamtalkSystemDictionary` (`stdlib/src/SystemDictionary.bt`, `beamtalk_stdlib.erl`): Implements `Beamtalk allClasses`, `Beamtalk classNamed:`, `Beamtalk globals`, and delegates to runtime for actors/sessions/modules

**Key Operations (available in all REPL sessions):**

| Message | Return | Delegates To |
|---------|--------|-------------|
| `Beamtalk allClasses` | List of class names | `beamtalk_class_registry` |
| `Beamtalk classNamed: #Counter` | Class metadata | `beamtalk_class_registry` |
| `Beamtalk actors` | List of running actors | `beamtalk_workspace_meta` |
| `Beamtalk modules` | List of loaded modules | `beamtalk_workspace_meta` |
| `Beamtalk version` | Version string | Runtime app env |
| `Beamtalk nodeName` | BEAM node atom | `erlang:node()` |
| `Beamtalk projectPath` | Project path binary | `beamtalk_workspace_meta` |

**Key Patterns:**
- **Façade over Multiple Contexts:** Beamtalk Global hides the Workspace, Object System, and Actor System contexts behind a single object; callers don't need to know which gen_server to query
- **Read-Only:** No mutating operations — actors are spawned via `ClassName spawn`, not via `Beamtalk spawn:`
- **Always Available:** Implemented as a globally registered class in `beamtalk_class_registry`; available before any user code loads

**Example Domain Logic:**

```beamtalk
// User code in any REPL session
Beamtalk allClasses          // => #('Counter', 'Logger', 'Actor', 'Object', ...)
Beamtalk classNamed: #Counter // => <ClassInfo for Counter>
Beamtalk actors               // => #(<0.123.0> Counter, <0.124.0> Logger)
Beamtalk version              // => '0.1.0'
```

```erlang
%% Erlang implementation delegates to workspace context
beamtalk_actors() ->
    case beamtalk_workspace_meta:supervised_actors() of
        Pids when is_list(Pids) ->
            [{Pid, beamtalk_class_registry:class_of(Pid)} || Pid <- Pids, is_process_alive(Pid)];
        _ ->
            []
    end.
```

---

## Cross-Cutting Concerns

These concerns span multiple bounded contexts:

### 1. Error Handling

**Strategy:** Different approaches per context

| Context | Strategy | Rationale |
|---------|----------|-----------|
| COMPILER | Result types (`Result<T, E>`) | Rust idiomatic, explicit error handling |
| LANGUAGE SERVICE | Diagnostic collection | IDE needs all errors, not just first |
| ACTOR SYSTEM | Error isolation + rejection | Actors don't crash, errors returned to caller |
| FUTURE | Rejection state | Async errors propagate via future rejection |

**Domain Service:** `ErrorFormatter` - consistent error messages across contexts

### 2. Logging and Observability

**Strategy:** Context-specific logging

| Context | Mechanism | What to Log |
|---------|-----------|-------------|
| COMPILER | `tracing` crate | Parse errors, codegen steps |
| LANGUAGE SERVICE | `tracing` | Query times, cache hits/misses |
| ACTOR SYSTEM | `error_logger` | Actor crashes, DNU calls |
| HOT RELOAD | `error_logger` | Code upgrades, migration failures |

**Domain Event:** `CodeUpgradeCompleted` - notify observers of successful reload

### 3. Performance Monitoring

**Critical Paths:**
- Keystroke → Diagnostics: < 50ms (LANGUAGE SERVICE)
- Save → Hot Reload: < 100ms (COMPILER → HOT RELOAD)
- REPL Expression: < 100ms (REPL → COMPILER → ACTOR)

**Metrics:**
- Query cache hit rate (LANGUAGE SERVICE)
- Parse time per file size (COMPILER)
- Actor spawn rate (ACTOR SYSTEM)
- Future completion rate (CONCURRENCY)

### 4. Testing Strategy

**Per-Context Testing:**

| Context | Test Type | Focus |
|---------|-----------|-------|
| SOURCE ANALYSIS | Property tests | Parse roundtrip, error recovery |
| SEMANTIC ANALYSIS | Example tests | Name resolution, type checking |
| CODE GENERATION | Snapshot tests | Core Erlang output stability |
| ACTOR SYSTEM | Unit tests (EUnit) | Message dispatch, error isolation |
| CONCURRENCY | Property tests (PropEr) | Future state machine invariants |
| HOT RELOAD | Integration tests | State migration correctness |

**Snapshot Testing:** `test-package-compiler/` directory contains expected Core Erlang outputs

---

## Domain Events

Domain events represent significant occurrences in the system. They enable loose coupling between bounded contexts.

### Compiler Domain Events

| Event | Payload | Triggered When | Subscribers |
|-------|---------|----------------|-------------|
| `FileParsed` | Module, Diagnostics | Parse completes | LANGUAGE SERVICE |
| `TypeCheckCompleted` | Module, Errors | Type checking done | LANGUAGE SERVICE |
| `CodeGenerated` | ModuleName, CoreErlang | Code generation completes | HOT RELOAD |
| `CompilationFailed` | Path, Errors | Compilation fails | LANGUAGE SERVICE |

### Runtime Domain Events

| Event | Payload | Triggered When | Subscribers |
|-------|---------|----------------|-------------|
| `ActorSpawned` | Class, Pid | Actor created | INSTANCE TRACKING |
| `ActorTerminated` | Class, Pid, Reason | Actor dies | INSTANCE TRACKING |
| `MessageDispatched` | Selector, Result | Message handled | (Tracing/debugging) |
| `FutureResolved` | FuturePid, Value | Future completes | Waiters |
| `FutureRejected` | FuturePid, Reason | Future fails | Waiters |
| `CodeUpgradeStarted` | Module, Version | Hot reload begins | Actor instances |
| `CodeUpgradeCompleted` | Module, Stats | Hot reload succeeds | Monitoring |
| `StateMigrationFailed` | Pid, Reason | code_change/3 fails | Supervision |
| `SessionStarted` | SessionId, Pid | WebSocket authenticated | WORKSPACE (update activity) |
| `SessionClosed` | SessionId, Reason | WebSocket closes | WORKSPACE (cleanup session sup) |
| `WorkspaceStarted` | WorkspaceId, ProjectPath, Port | Node startup complete | Monitoring |
| `WorkspaceStopped` | WorkspaceId, Reason | `init:stop/0` called | Monitoring |
| `WorkspaceIdleTimeout` | WorkspaceId, IdleSeconds | Idle limit exceeded | (triggers WorkspaceStopped) |
| `ModuleLoaded` | ModuleName, SourcePath | `load-file` or `load-source` op | WORKSPACE (register module) |
| `ModuleUnloaded` | ModuleName | `unload` op or class redefinition | WORKSPACE (deregister module) |

**Event Flow Example: File Save → Hot Reload**

```
1. IDE saves file
2. LANGUAGE SERVICE → FileParsed event
3. COMPILER → CodeGenerated event
4. HOT RELOAD receives event
5. HOT RELOAD → CodeUpgradeStarted event
6. Actors receive code_change/3
7. HOT RELOAD → CodeUpgradeCompleted event
```

---

## Architecture Decision Records

### ADR-1: Rust for Compiler, Erlang for Runtime

**Context:** Need to choose languages for compiler and runtime subsystems

**Decision:** 
- Compiler in Rust (lexer, parser, codegen, LSP)
- Runtime in Erlang (actors, futures, class registry, REPL)

**Rationale:**
- Rust excels at compiler workloads (fast parsing, low memory)
- Erlang excels at runtime concurrency (millions of processes)
- BEAM is target platform, native Erlang integrates seamlessly
- No need to bootstrap (Rust compiles to machine code)

**Status:** Accepted

### ADR-2: Two Modules per Class (Flavors Pattern)

**Context:** Need to decide module generation strategy

**Decision:** Generate two modules per class:
- `beamtalk_<class>_class` - Class metadata (superclass, methods, ivars)
- `beamtalk_<class>` - Instance gen_server implementation

**Rationale:**
- Separates class-level (static) from instance-level (dynamic) concerns
- Abstract classes skip instance module generation (optimization)
- Follows proven LFE Flavors pattern
- Enables class introspection without spawning instance

**Status:** Accepted

### ADR-3: Process-per-Future

**Context:** Need async result representation for async-first message sending

**Decision:** Each future is a lightweight BEAM process (~2KB)

**Alternatives Considered:**
- Ref + registry: Complex tracking, no isolation
- ETS-based: Fast lookup but manual cleanup

**Rationale:**
- BEAM processes are cheap enough (millions possible)
- Natural garbage collection (future dies when unreferenced)
- Isolation (future crash doesn't affect actors)
- Simple implementation (state machine via receive loop)

**Status:** Accepted

### ADR-4: Error Isolation in Actors (Flavors Pattern)

**Context:** Should actor crashes propagate to caller?

**Decision:** Actors catch errors and return them to caller via future rejection or error tuple

**Rationale:**
- Prevents cascading failures
- Caller chooses whether to crash or handle error
- Preserves actor state across errors
- Matches Flavors' successful approach

**Status:** Accepted

### ADR-5: ETS-based Instance Tracking

**Context:** Need to implement `Class allInstances` for introspection

**Decision:** Use ETS bag table with process monitors for auto-cleanup

**Alternatives Considered:**
- Global heap scan: Impossible on BEAM (per-process heaps)
- Manual registration: Requires discipline, error-prone

**Rationale:**
- ETS provides fast concurrent access
- Process monitors enable automatic cleanup on termination
- More efficient than Smalltalk's heap scan for large systems

**Status:** Accepted

### ADR-6: Pragmatic Hybrid Object Model

**Context:** How to map Smalltalk's object model to BEAM?

**Decision:** Embrace BEAM's actor model, reify what's natural, document limitations

**Alternatives Considered:**
- Meta-circular interpreter: Full Smalltalk semantics but 10-100x slower
- Dual-mode execution: Complex to maintain two implementations
- CPS transformation: 2-5x slower, complex debugging

**Rationale:**
- Performance critical for agent systems (millions of actors)
- BEAM processes ≈ Smalltalk objects (identity, state, behavior)
- Most Smalltalk features map cleanly (classes, methods, blocks, dispatch)
- Accept limitations (`thisContext`, `become:`) for performance
- Matches Gleam and LFE Flavors' successful approaches

**Status:** Implemented (see [ADR 0005](ADR/0005-beam-object-model-pragmatic-hybrid.md))

### ADR-7: Compiler as Language Service

**Context:** How to architect compiler for IDE integration?

**Decision:** TypeScript approach - compiler IS the language service, not separate

**Rationale:**
- Single code path for compilation and IDE queries
- Incremental parsing and caching built-in from day one
- Error recovery is mandatory, not optional
- Query-based architecture (Salsa-style)
- Sub-100ms response times feasible

**Status:** Accepted (see Principle 13 in beamtalk-principles.md)

---

## Summary

This DDD model provides:

1. **Strategic Clarity:** Core vs. supporting domains, bounded contexts, context map
2. **Ubiquitous Language:** Shared vocabulary across team, code, and docs
3. **Domain Models:** Aggregates, entities, value objects, services per context
4. **Event-Driven Integration:** Domain events for loose coupling
5. **Architectural Decisions:** Rationale for key design choices

**Key Insights:**

- **Live Programming** is the core differentiator (not just compilation)
- **Actor System** bridges Smalltalk objects and BEAM processes elegantly
- **Futures/Concurrency** enable async-first without blocking actors
- **Object System** provides Smalltalk semantics on BEAM primitives
- **Hot Reload** leverages BEAM's native code upgrade capabilities
- **Two subsystems** (Rust compiler + Erlang runtime) with clear boundaries
- **Flavors patterns** (two modules, error isolation, object records) proven successful

**Next Steps:**

1. Map existing code to bounded contexts (identify misalignments)
2. Establish context integration tests (validate anti-corruption layers)
3. Define domain events schema (enable event sourcing/auditing)
4. Document aggregate invariants in code (runtime assertions)
5. Refine ubiquitous language (update team communication)

---

## References

- [Domain-Driven Design](https://www.domainlanguage.com/ddd/) - Eric Evans
- [Implementing Domain-Driven Design](https://vaughnvernon.com/) - Vaughn Vernon
- [Beamtalk Architecture](beamtalk-architecture.md) - Technical details
- [Beamtalk Object Model (ADR 0005)](ADR/0005-beam-object-model-pragmatic-hybrid.md) - Smalltalk → BEAM mapping
- [Beamtalk Principles](beamtalk-principles.md) - Core design philosophy
- [LFE Flavors](https://github.com/rvirding/flavors) - OOP on BEAM reference implementation
