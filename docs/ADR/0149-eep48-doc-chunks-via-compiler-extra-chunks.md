# ADR 0149: EEP-48 Doc Chunks Written by the OTP Compiler (`extra_chunks`)

## Status
Proposed (2026-11-14). Tracking: BT-4852 (research spike).

This ADR is the output of a spike. It recommends a design and records what was
measured. Nothing is implemented. The prototypes are in
[`spikes/eep48-doc-chunks/`](../../spikes/eep48-doc-chunks/README.md).

## Context

### Problem

A compiled Beamtalk module carries no EEP-48 `Docs` chunk, so the rest of the
BEAM cannot see Beamtalk documentation:

```erlang
1> code:get_doc('bt@stdlib@integer').
{error,missing}
```

Erlang's `h/1`, Elixir's `h` and `Code.fetch_docs/1` all read that chunk. A
Beamtalk package used from an Erlang or Elixir project is undocumented there.

### History

- **ADR 0008** (implemented 2026-02-15) compiled `///` comments to EEP-48.
  `doc_chunks.rs` built a `docs_v1` term as text, the CLI wrote it to a `.docs`
  file next to the `.core` file, and the compile escript injected it into each
  `.beam` after compilation (PR #433, BT-499).
- **ADR 0033** (implemented 2026-02-24) removed all of that (PR #785, BT-772).
  Docs moved onto the runtime objects: `class_state` holds `doc`,
  `method_docs` and `class_method_docs`, and `:h`, `Counter doc` and the MCP
  `docs` tool read them there. Its reasons:
  1. rewriting the `.beam` after compilation was "the most fragile part of the
     compilation pipeline";
  2. EEP-48 could not cover dynamic classes, which have no `.beam` file;
  3. the only consumer was the Beamtalk REPL.

  It left the door open: EEP-48 "can be re-added as a thin layer that reads
  from `class_state`".
- EEP-48 is still *read* for native Erlang modules (`beamtalk_native_docs`,
  `:h Erlang <module>`), and native `.erl` files are compiled with their
  `-doc` attributes (BT-1900, BT-1905).

### What has changed since ADR 0033

- Compilation moved from an escript to `beamtalk_build_worker`, which calls
  `compile:forms/2` on a parsed Core Erlang module. The REPL and LSP path
  (`beamtalk_compiler_server`) does the same. Both own their compiler options.
- The build worker already reads structured data out of a Core Erlang module
  before compiling it: `beamtalk_compiler:class_meta_from_core_erlang/1`
  reads the constant `__beamtalk_meta/0`.
- Codegen already emits every doc string and every signature into the module,
  in the `register_class/0` builder map (`classDoc`, `methodDocs`,
  `classMethodDocs`, `methodSignatures`, `classMethodSignatures`).

### Constraints

- The supported OTP window is 28 to 29 (`otp-support.toml`).
- ADR 0033 stays in force. Runtime docs on `class_state` are the source of
  truth for `:h`, `doc`, and the MCP tool. This ADR must not add a second
  source that those read.
- No step may rewrite a `.beam` after the compiler has produced it, and no
  side file may travel next to the `.core` file.

## Experiments

All of these ran on OTP 28.5 and OTP 29.1.2, with Elixir 1.20.1 and ExDoc
0.40.4. `spikes/eep48-doc-chunks/run.sh` repeats them. The fixture is two small
classes, a `Value` (`Counter`) and an `Actor` (`Tally`), compiled with the real
compiler.

### Q1. Can Core Erlang carry `-doc`/`-moduledoc` so the compiler builds the chunk?

No, on both OTP versions.

| Input | `Docs` chunk | `code:get_doc/1` |
|---|---|---|
| `plain.erl` with `-moduledoc`/`-doc`, normal pipeline | yes | `{ok, ...}` |
| `incore.core` with `'moduledoc'`/`'doc'` attributes, `erlc` | no | `{error, missing}` |
| the same Core module through `compile:forms/2` with the build worker's options | no | n/a |

The reason is in `compile.erl`: the `beam_docs` pass runs only in the
abstract-code pass list. `from_core` starts after it. Doc attributes written in
Core Erlang survive as ordinary module attributes (`module_info(attributes)`
shows `doc` and `moduledoc`) and nothing else happens. In the other direction,
`erlc +to_core plain.erl` produces Core Erlang with no doc attributes at all;
they are consumed before Core is generated.

So this works only from `.erl` or Erlang abstract code. Beamtalk generates
neither.

### Q1b. Can the compiler still write the chunk, some other way?

Yes. `compile` has a documented option, `{extra_chunks, [{binary(),
binary()}]}`, that stores extra chunks in the `.beam` it produces. It works
with `from_core`:

```erlang
compile:forms(CoreModule, [
    from_core, binary, return_errors, return_warnings, debug_info, clint,
    {extra_chunks, [{<<"Docs">>, term_to_binary(DocsV1, [deterministic, compressed])}]}
]).
```

On both OTP versions the output has a `Docs` chunk, `code:get_doc/1` returns
it, and compiling twice gives byte-identical binaries. Nothing touches the
`.beam` afterwards.

How the `docs_v1` term reaches the build worker was tested too (E9). A Core
Erlang module attribute cannot carry it: binaries are a syntax error in Core
attribute constants (`core_parse: syntax error before: '#'`). A constant
function can. The prototype has codegen emit `'__beamtalk_docs'/0` whose body
is the literal `docs_v1` term; the worker folds it to a literal, removes the
function and its export from the module, and passes the term as the chunk. The
compiled `.beam` has the chunk and no `__beamtalk_docs/0`.

### Q2. Can `code:get_doc/1` and `h/1` be served from runtime class data?

There is no hook. `code:get_doc/1` and `shell_docs` take no callback and read
no application environment. `code:get_doc/1` does exactly this:

1. `code:which(Mod)` gives a file name;
2. read the `Docs` chunk of that file;
3. if that fails, read `<dir>/doc/chunks/<Mod>.chunk`, walking up from the
   file's directory;
4. if that fails, synthesise from Erlang abstract code.

Step 3 is the same fallback OTP uses for its own applications. It can be used
as the hook. For a module loaded with `code:load_binary/3`:

| Load path given to `code:load_binary/3` | `code:get_doc/1` |
|---|---|
| `""` (what `beamtalk_repl_eval` does today) | `{error, missing}` |
| the `.bt` source path | `{error, missing}` |
| a real file in a session directory that holds the same binary | `{ok, ...}` |
| a path with no `.beam` behind it, but `<dir>/doc/chunks/<Mod>.chunk` exists | `{ok, ...}` |

In the last case, rewriting the side file changes what the next
`code:get_doc/1` and `h/1` return, with no reload. That is what a `doc:`
setter would need.

Building `docs_v1` from live `class_state` is cheap. For the whole stdlib (125
classes, 1717 method entries) the prototype built and wrote every chunk file
in about 50 ms, 0.4 ms per class.

So the answer is yes, with files: the runtime would have to write a chunk file
per class into a directory it owns and load REPL classes with a path under
that directory. It cannot be done in memory.

One class in the stdlib (`beamtalk_protocol_object`) has no `.beam` at all.
`code:which/1` returns `non_existing` for it, and no route reaches it.

### Q3. Mapping selectors to `{function, Name, Arity}`

The prototype used:

| Method | EEP-48 key | Example |
|---|---|---|
| instance side | `{function, Selector, MessageArity + 1}` | `add:` becomes `{function, 'add:', 2}` |
| class side | `{function, 'class_' ++ Selector, MessageArity + 1}` | `startingAt:` becomes `{function, 'class_startingAt:', 2}` |

These are the names and arities of the functions codegen already exports, so
the entry documents something an Erlang caller can really call. Measured over
the stdlib:

| | Entries | The key is a real export |
|---|---|---|
| instance side | 1235 | 1158 |
| class side | 474 | 474 |

The 77 instance entries with no matching export are mostly actor methods.
An actor module exports its `gen_server` callbacks and `dispatch/4`, not one
function per selector. `h/2` and IEx `h` still show those entries, because
neither checks exports.

Signature and metadata that rendered well:

- signature: `[<<"Counter >> add: n :: Integer -> Counter">>]`, and
  `Counter class >> ...` for the class side;
- entry metadata: `selector`, `class_side`, and `exported` (whether the key is
  a callable function);
- module metadata: `beamtalk_class`, `superclass`, `kind`, `uses`.

**Inherited methods.** EEP-48 has no inheritance. `h(Mod, Fun)` looks in that
module's chunk only. Copying inherited entries into every class was measured:

| | Entries | Compressed bytes |
|---|---|---|
| local methods only | 1711 | 261,368 |
| inherited methods copied into every class | 6945 | 1,217,647 (4.7 times) |

A copy also goes stale when a superclass is recompiled on its own.

One hazard: on an actor, a unary selector named `init` maps to
`{function, init, 1}`, which is the `gen_server` callback, not the method. The
`exported` flag does not catch that. It was not hit in the stdlib.

### Q4. Cost

| | Result |
|---|---|
| `.beam` size, fixture | `Counter` 11,196 to 11,772 bytes (+576); `Tally` 13,192 to 13,492 (+300) |
| `.beam` size, whole stdlib | 263,749 bytes of compressed chunks on 3,337,400 bytes of `.beam` (+7.9%) |
| Compile time | Building and encoding the chunk took 27 to 85 µs per module. The with/without difference in `compile:forms/2` (50 to 115 ms per module) was inside run-to-run noise (-14% to +11%). |
| Build caching | No new inputs and no new outputs. The chunk is a function of the same source that produces the `.beam`. There is no side file to clean up when a class loses its docs, which ADR 0008's `.docs` file needed. |
| Reproducible builds | With `term_to_binary(_, [deterministic])` two compiles are byte-identical. |
| Releases | `beam_lib:strip/1` removes the chunk (verified), so a stripped release pays nothing. |
| Memory | None. The chunk is not loaded with the module. |

The doc strings are then in the `.beam` twice: once as literals in
`register_class/0` (loaded, and what ADR 0033 reads) and once in the chunk (on
disk only).

### Q5. Consumers

| Consumer | `beam_language = beamtalk` | `beam_language = erlang` | Notes |
|---|---|---|---|
| Erlang `h/1`, `h/2`, `h/3` (OTP 28, 29) | works | not needed | Renders the Markdown. Does not load the module. Shows non-exported entries. |
| `code:get_doc/1` | works | works | |
| Elixir `Code.fetch_docs/1` | works | works | |
| IEx `h Mod`, `h Mod.fun`, `h Mod.fun/n` | works | works | IEx loads the module first. `on_load` runs `register_class/0`, so without the Beamtalk runtime on the code path IEx prints `Could not load module ..., got: on_load_failure`. |
| ExDoc 0.40.4 | skipped: `unsupported language (beamtalk)` | crashes: `could not find base directory` | See below. |
| ELP hover, `erlang_ls` hover | not tested | not tested | The ELP binary could not be downloaded where the spike ran. |

ExDoc picks a backend from `beam_language` with a fixed two-entry table
(`:elixir`, `:erlang`). There is no plug-in point. Its Erlang backend also
reads the module's Erlang abstract code: it needs a `-file` attribute for the
base directory and a function form for each documented function. A
from-Core `.beam` has an empty abstract-code list. With `beam_language` set to
`erlang` and a synthetic abstract-code chunk swapped in after compilation,
ExDoc did render `Counter`, with the Beamtalk signatures. It listed exported
functions only, so `Tally`'s two methods were missing. That test rewrote the
`.beam` after compilation, which this ADR rules out.

## Decision

Bring EEP-48 back as a **derived, compile-time export**, written by the OTP
compiler through `extra_chunks`. Runtime docs stay the source of truth.

1. **Codegen builds the `docs_v1` term** and emits it into the Core Erlang
   module as the body of one constant function, `'__beamtalk_docs'/0`. The
   mapping rule lives in Rust only, written with the `Document` and typed-leaf
   API like the rest of codegen.
2. **The compile step lifts it out.** `beamtalk_build_worker` and
   `beamtalk_compiler_server` fold that function's body to a literal, remove
   the function and its export from the module, and pass
   `{extra_chunks, [{<<"Docs">>, term_to_binary(Docs, [deterministic,
   compressed])}]}` to `compile:forms/2`. The Erlang side does not know the
   mapping; it moves a term. A module without the function compiles as today.
3. **`beam_language` is `beamtalk` and the format is `text/markdown`.**
4. **Keys are the exported function names**: `{function, Selector, Arity + 1}`
   for the instance side and `{function, 'class_' ++ Selector, Arity + 1}` for
   the class side. Entry metadata carries `selector`, `class_side` and
   `exported`. Module metadata carries `beamtalk_class`, `superclass`, `kind`
   and `uses`.
5. **Local methods only.** Inherited methods are not copied. A consumer that
   wants them follows `superclass` in the module metadata.
6. **Methods with no `///` comment get an entry with an empty doc map**, so
   their signature still shows. Methods that are not public get `hidden`.
7. **Classes with no `.beam` on disk are out of scope for now.** REPL-defined
   classes and runtime `doc:` edits are not reflected in EEP-48. `:h`,
   `Counter doc` and the MCP tool, which read `class_state`, are unchanged and
   still cover them.

What an Erlang or Elixir user gets:

```erlang
1> h('bt@demo@counter', increment).

  Counter >> increment -> Counter

  Add one and return the new counter.
```

```elixir
iex> h :"bt@demo@counter"."add:"/2

                    Counter >> add: n :: Integer -> Counter

Add `n` to the counter.
```

A class or method with no docs:

```erlang
2> h('bt@demo@counter', nope).
{error,function_missing}
```

A REPL-defined class, from the Erlang side (unchanged from today), where
`Mod` is the module the REPL loaded for it:

```erlang
3> code:get_doc(Mod).
{error,missing}
```

### Open choices for the maintainer

- **Always on, or a build flag?** The recommendation is always on in `dev`
  builds and for the stdlib. The cost is 7.9% of `.beam` size on disk and no
  measurable compile time.
- **Rule 6.** The alternative is to omit undocumented methods, which makes the
  chunk smaller and `h/2` return `function_missing` for them.
- **The actor `init` hazard** (Q3). The options are to accept it, or to skip
  the entry when the key collides with a generated callback export.

## Prior Art

| Language | How the chunk is written | Notes |
|---|---|---|
| **Erlang** (OTP 27+) | The `beam_docs` compiler pass, from `-doc` attributes in abstract code | Not reachable from Core Erlang (Q1). |
| **Elixir** | Its own compiler builds `docs_v1` and writes the chunk when it assembles the `.beam` | Same shape as this decision: the language's compiler owns the term, the chunk is written once. |
| **Gleam** | Compiles to `.erl` and lets `erlc` do it | Needs Erlang source. Beamtalk emits Core Erlang. |
| **LFE** | Builds the chunk in its own compiler | `beam_language` is `lfe`; ExDoc does not render it either. |
| **Pharo** | Class comments and method comments are objects in the image | This is ADR 0033's model, which stays. |

The Gleam and LFE rows are from general knowledge of those projects and were
not re-checked in this spike.

Adopted: Elixir's shape. Not adopted: Erlang's attribute route, because it
does not exist for Core Erlang input.

## User Impact

- **Newcomer.** No change. `:h` and `Counter doc` work as before.
- **Smalltalk developer.** No change. Docs remain live objects; the chunk is an
  export, like a fileout.
- **Erlang/Elixir developer.** `h/1` and IEx `h` work on Beamtalk modules.
  Keys are real function names, so `h(Mod, 'add:', 2)` documents the function
  they would call. Two things will surprise them: inherited methods are not
  listed, and IEx `h` needs the Beamtalk runtime on the code path because it
  loads the module.
- **Operator.** Larger `.beam` files on disk (7.9% for the stdlib), nothing in
  memory. Stripped releases are unaffected.
- **Tooling developer.** `code:get_doc/1` becomes a second, static way to read
  docs without a running workspace. ExDoc still does not work.

## Steelman Analysis

### For "do nothing" (keep ADR 0033 as is)

- **Operator:** "Nobody has asked for `h/1` on a Beamtalk module. You are
  adding 8% to every `.beam` for a consumer that does not exist yet."
- **Language designer:** "Two representations of the same docs is how the first
  version went wrong. One is enough."

### For the runtime chunk-file layer (Q2) now, not later

- **Smalltalk developer:** "If `Counter doc: '...'` changes what `:h` says, it
  should change what `h/1` says. A static chunk that disagrees with the live
  class is a lie."
- **Newcomer:** "I defined a class in the REPL and Erlang says it has no docs.
  Why does it matter where I typed it?"

### For `beam_language = erlang`

- **BEAM veteran:** "ExDoc and hexdocs are how the ecosystem reads docs. A
  chunk that ExDoc skips is half a feature."

### Tension points

- Static chunk against live docs. This ADR accepts that they can differ after
  a runtime `doc:` edit. ADR 0033 already says those edits are session-local.
- Reach against honesty about the language. Claiming `erlang` reaches ExDoc
  only together with fake abstract code, and tells every tool that selectors
  are Erlang functions.

## Alternatives Considered

### `-doc`/`-moduledoc` attributes in Core Erlang

Emit the attributes and let the compiler build the chunk. **Rejected: it does
not work** on OTP 28 or 29 (Q1). The pass that reads them is not in the
`from_core` pipeline.

### Generate Erlang abstract code or `.erl` for a docs-only companion

Compile a stub `.erl` with `-doc` attributes to get a chunk, then move it.
**Rejected:** it is the post-compilation rewrite again, plus a second
compilation per module.

### Inject after compilation with `beam_lib` (ADR 0008's design)

**Rejected** for ADR 0033's reason. `extra_chunks` gives the same result
inside the one compiler call.

### `doc/chunks/<Mod>.chunk` side files at build time

`code:get_doc/1` reads them (Q2), and they keep `.beam` files unchanged.
**Rejected for compiled classes:** it is a second output per module that
caching, packaging and releases all have to carry, which is the `.docs` file
problem. Elixir's `Code.fetch_docs/1` on a module name and ExDoc were not
tested against side files.

### Serve EEP-48 from `class_state` at runtime

Feasible only through chunk files and a changed load path (Q2).
**Deferred, not rejected.** It needs the workspace to own a directory, to
write a file per class and per `doc:` edit, and to pass a real path to
`code:load_binary/3`. `beamtalk_module_activation` documents code that relies
on REPL modules not having a real path, so that change needs its own review.
It can be added later without changing this decision: the chunk format and
mapping would be the same.

### `beam_language = erlang`, for ExDoc

**Rejected.** ExDoc also needs Erlang abstract code, which a from-Core module
does not have, and it hides non-exported entries, which is every actor method
(Q5). `beamtalk doc` already generates HTML. The better fix is a Beamtalk
backend in ExDoc, which is an upstream change.

### Copy inherited methods into each class

**Rejected.** 4.7 times the size (Q3), and stale after a superclass is
rebuilt.

### A separate entry kind, such as `{beamtalk_method, Selector, Arity}`

EEP-48 allows other kinds. **Rejected:** `h/2` and IEx `h` look up `function`
entries, so nothing would be shown. Not tested; this is from reading
`shell_docs` and the IEx results above.

## Consequences

### Positive
- `code:get_doc/1`, Erlang `h/1` and IEx `h` work on compiled Beamtalk modules.
- No step runs after the compiler and no side file exists.
- The mapping rule is in one place (Rust codegen). The Erlang side moves a
  term and needs no conformance fixture for the rule.
- No measurable compile-time cost. Deterministic output.
- ADR 0033 is untouched.

### Negative
- Doc strings are stored twice in each `.beam` (7.9% on disk for the stdlib).
- The chunk can disagree with live docs after a runtime `doc:` edit.
- REPL-defined classes and the one `.beam`-less stdlib class have no EEP-48
  docs.
- ExDoc does not render Beamtalk modules. ELP and `erlang_ls` are unknown.
- Inherited methods do not appear in `h/1`.
- The compile step gains one more thing it reads out of the Core module.

### Neutral
- `:h`, `Counter doc`, the MCP `docs` tool and `beamtalk doc` do not change.
- Native Erlang modules keep getting their chunk from `-doc` attributes.

## Implementation

Not part of the spike. A rough shape, if accepted:

1. **Codegen (M).** Emit `'__beamtalk_docs'/0` with the `docs_v1` literal.
   Before writing any helper, check what `class_registry.rs` already computes
   for `methodDocs` and `methodSignatures` and reuse it; do not build a second
   signature formatter.
2. **Compile step (S).** One shared function, used by both
   `beamtalk_build_worker` and `beamtalk_compiler_server`, that lifts the
   function out and returns the extra compiler option. It should sit next to
   `constant_meta/1` in `beamtalk_compiler`, which does the same kind of read.
3. **Tests (S).** A BUnit or runtime test that `code:get_doc/1` on a compiled
   fixture class returns the expected entries, on both OTP versions in the
   matrix. A build test that two builds are byte-identical.
4. **Docs (S).** `docs/beamtalk-language-features.md` and
   `docs/development/surface-parity.md` (a new way to read docs from Erlang).

The prototype's two shortcuts should not be copied: it finds the doc maps by
scanning `register_class/0`, and the runtime variant reads `#class_state{}` by
field position.

## References
- Related issues: BT-4852 (this spike), BT-499 (first EEP-48 implementation), BT-772 (its removal), BT-1851, BT-1900, BT-1905 (EEP-48 for native Erlang modules)
- Related ADRs: [ADR 0008](0008-doc-comments-and-api-documentation.md), [ADR 0033](0033-runtime-embedded-documentation.md), [ADR 0089](0089-typed-document-leaves.md)
- Prototypes: [`spikes/eep48-doc-chunks/`](../../spikes/eep48-doc-chunks/README.md)
- EEP-48: https://www.erlang.org/eeps/eep-0048.html
- EEP-59: https://www.erlang.org/eeps/eep-0059.html
- `compile` options (`extra_chunks`): https://www.erlang.org/doc/apps/compiler/compile.html
