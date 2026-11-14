# ADR 0149: EEP-48 Doc Chunks for Beamtalk Modules

## Status
Proposed (2026-11-14). Tracking: BT-4852 (research spike).

This ADR is the output of a spike. It records what was measured, sets out two
workable designs, and recommends one. The choice is the maintainer's. Nothing
is implemented. The prototypes are in
[`spikes/eep48-doc-chunks/`](../../spikes/eep48-doc-chunks/README.md).

Revised once after review (2026-11-14). The first draft recommended the
in-`.beam` design and understated its cost; see "Revision notes" at the end.

## Context

### Problem

A compiled Beamtalk module carries no EEP-48 documentation, so the rest of the
BEAM cannot see Beamtalk docs:

```erlang
1> code:get_doc('bt@stdlib@integer').
{error,missing}
```

Erlang's `h/1`, Elixir's `h` and `Code.fetch_docs/1` all read EEP-48. A
Beamtalk package used from an Erlang or Elixir project is undocumented there.

No user has asked for this yet. ADR 0033's third reason for removing EEP-48
("the only consumer was the REPL") still holds. This ADR answers "how, if we
do it", and the cost of each answer should be weighed against that.

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

### Current state that matters here

- **There are three ways Core Erlang becomes a `.beam`.**
  1. `beamtalk_build_worker` (`beamtalk build`): `compile:forms/2` with
     `[from_core, binary, return_errors, return_warnings, debug_info, clint]`.
     It runs in a `-mode minimal` node with only the compiler application's
     `ebin` on the path.
  2. `beamtalk_compiler_server` (REPL, LSP, in-memory): `compile:forms/2`
     with a different option list (no `debug_info`).
  3. The embedded `templates/compile.escript` (`compile:file`, `from_core`).
     `beam_compiler.rs` falls back to it when the runtime or build worker is
     unavailable, or when `BEAMTALK_COMPILER=escript`. Some codegen tests use
     `erlc +from_core` directly.
- **Something already reads data out of a Core module before compiling it:**
  `beamtalk_compiler:class_meta_from_core_erlang/1` reads the constant
  `__beamtalk_meta/0`. Its only production caller is `beamtalk_repl_loader`.
  The build worker does not call it.
- **Codegen already emits every doc string and signature** in the
  `register_class/0` builder map (`classDoc`, `methodDocs`,
  `classMethodDocs`, `methodSignatures`, `classMethodSignatures`). There is a
  second emitter of that map for native facades
  (`gen_server/native_facade.rs`, ADR 0056).
- **Codegen writes every binary one segment per byte**
  (`beamtalk-cerl-doc`, `byte_segments`): about 41 characters of Core text per
  byte. The stdlib has about 645 KB of `///` text.
- **REPL and workspace loads do not point at a `.beam`.**
  `beamtalk_repl_eval` calls `code:load_binary(M, "", Bin)`, and
  `beamtalk_repl_loader` passes the `.bt` source path. After a class from a
  compiled package is reloaded in a workspace, `code:which/1` for its module
  is the `.bt` path.
- **`beamtalk_native_docs` ignores `beam_language`.** It serves REPL
  `:h Erlang`, LSP hover and MCP from any `docs_v1` chunk it finds.
  `beamtalk_erlang_help` already skips `bt@` modules when it lists modules.

### Constraints

- OTP 28 is the minimum. The supported window is 28 to 29
  (`otp-support.toml`).
- ADR 0033 stays in force. Runtime docs on `class_state` are the source of
  truth for `:h`, `doc`, LSP hover and the MCP tool. Nothing here may become a
  second source for those.
- No step may rewrite a `.beam` after the compiler has produced it.
- ADR 0139 keeps compiler-made names (the `class_` prefix, inflated arities,
  `dispatch/4`) off user-facing Beamtalk surfaces.

## Experiments

All of these ran on OTP 28.5 and OTP 29.1.2 with the same results, plus
Elixir 1.20.1 and ExDoc 0.40.4. `spikes/eep48-doc-chunks/run.sh` repeats them.
The fixture is two small classes, a `Value` (`Counter`) and an `Actor`
(`Tally`), compiled with the real compiler. Stdlib figures come from the 125
classes of the installed stdlib.

### Q1. Can Core Erlang carry `-doc`/`-moduledoc` so the compiler builds the chunk?

No.

| Input | `Docs` chunk | `code:get_doc/1` |
|---|---|---|
| `plain.erl` with `-moduledoc`/`-doc`, normal pipeline | yes | `{ok, ...}` |
| `incore.core` with `'moduledoc'`/`'doc'` attributes, `erlc` | no | `{error, missing}` |
| the same Core module through `compile:forms/2` with the build worker's options | no | n/a |

The `beam_docs` pass runs only in the abstract-code pass list of
`compile.erl`. `from_core` starts after it. Doc attributes written in Core
Erlang survive as ordinary module attributes and nothing else happens. In the
other direction, `erlc +to_core plain.erl` produces Core Erlang with no doc
attributes; they are consumed before Core is generated.

It works only from `.erl` or Erlang abstract code. Beamtalk generates neither.

### Q1b. Can the compiler still write the chunk?

Yes. `compile` has a documented option, `{extra_chunks, [{binary(),
binary()}]}`, and it works with `from_core`:

```erlang
compile:forms(CoreModule, [
    from_core, binary, return_errors, return_warnings, debug_info, clint,
    {extra_chunks, [{<<"Docs">>, DocsBin}]}
]).
```

The output has a `Docs` chunk, `code:get_doc/1` returns it, and two compiles
give byte-identical binaries. Nothing touches the `.beam` afterwards (E3).

The term has to reach the compile step somehow. Carrying it inside the Core
module was tested (E9, E10):

- A module attribute cannot carry it. Binaries are a syntax error in Core
  attribute constants (`core_parse: syntax error before: '#'`).
- A constant function `'__beamtalk_docs'/0` can. The compile step folds its
  body to a literal, removes the function and its export, and passes the
  bytes as the chunk. The `.beam` then has the chunk and no such function.
- How the carrier is written matters a great deal. See Q4.

### Q2. Can `code:get_doc/1` and `h/1` be served from runtime class data?

There is no hook. `code:get_doc/1` and `shell_docs` take no callback and read
no application environment. `code:get_doc/1` does exactly this:

1. `code:which(Mod)` gives a file name;
2. read the `Docs` chunk of that file;
3. if that fails, read `<dir>/doc/chunks/<Mod>.chunk`, walking up from the
   file's directory;
4. if that fails, synthesise from Erlang abstract code.

Step 3 is how OTP ships docs for its own applications
(`lib/<app>/doc/chunks`). For a module loaded with `code:load_binary/3` (E5):

| Load path given to `code:load_binary/3` | `code:get_doc/1` |
|---|---|
| `""` (what `beamtalk_repl_eval` does today) | `{error, missing}` |
| the `.bt` source path (what `beamtalk_repl_loader` does) | `{error, missing}` |
| a real file in a session directory that holds the same binary | `{ok, ...}` |
| a path with no `.beam` behind it, but `<dir>/doc/chunks/<Mod>.chunk` exists | `{ok, ...}` |

In the last case, rewriting the side file changes what the next
`code:get_doc/1` returns, with no reload. That is what a `doc:` setter would
need.

Elixir reads the side file too (E11): with no `Docs` chunk in the `.beam` and
only `doc/chunks/bt@stdlib@integer.chunk` present, `Code.fetch_docs/1`
returned the `docs_v1` term.

Building `docs_v1` from live `class_state` took about 50 ms for the whole
stdlib (125 classes, 1717 method entries).

So the answer is yes, through files. The runtime would write a chunk file per
class into a directory it owns and load REPL classes with a path under it. It
cannot be done in memory.

### Q3. Mapping selectors to `{function, Name, Arity}`

The prototype used the names and arities codegen exports:

| Method | EEP-48 key | Example |
|---|---|---|
| instance side | `{function, Selector, MessageArity + 1}` | `add:` becomes `{function, 'add:', 2}` |
| class side | `{function, 'class_' ++ Selector, MessageArity + 1}` | `startingAt:` becomes `{function, 'class_startingAt:', 2}` |

Over the stdlib modules:

| | Entries | The key is a real export |
|---|---|---|
| instance side | 1235 | 1158 |
| class side | 474 | 474 |

The 77 instance entries with no export are mostly actor methods. An actor
module exports its `gen_server` callbacks and `dispatch/4`, not one function
per selector. `h/2` and IEx `h` still show such entries; neither checks
exports.

What the prototype did **not** cover, and a real implementation must:

- **Mangled names.** `selector_mangler.rs` hashes class-side function names
  over 255 bytes and uses `__sealed_` and `dispatch_` prefixes. The prototype
  concatenated `"class_"` by hand. Keys must come from the same functions
  codegen uses (`safe_class_method_fn_name` and friends), or they will
  disagree with the exports.
- **Native facades** (ADR 0056), which have their own `register_class`
  emitter.
- **Extension methods** (ADR 0066). They are registered in ETS at load time
  and are not exports of the target class's module. They have no natural home
  in that module's chunk.
- **A name collision.** On an actor, a unary selector `init` maps to
  `{function, init, 1}`, which is the `gen_server` callback.

**"Real export" is not "callable".** Name and arity match, but an Erlang
caller cannot simply call them. A class-side function takes a class-self
value the caller cannot build. An actor method is reached through
`gen_server:call` (`docs/learning/16-beam-interop.md` §3). Only a `Value`
class's instance functions can be called directly, with the receiver as the
first argument.

**Inherited methods.** EEP-48 has no inheritance. `h(Mod, Fun)` looks in that
module's chunk only. Copying inherited entries into every class was measured:

| | Entries | Compressed bytes |
|---|---|---|
| local methods only | 1711 | 261,368 |
| inherited methods copied into every class | 6945 | 1,217,647 (4.7 times) |

A copy also goes stale when a superclass is recompiled on its own. A class
name in the metadata is not enough for a static consumer to follow the chain:
class to module resolution goes through `ClassModuleRegistry` (ADR 0119). The
superclass's module atom has to be in the metadata.

### Q4. Cost

**Size.** Compressed `docs_v1` terms for the stdlib total 263,749 bytes
(847,474 uncompressed), against 3,337,400 bytes of stdlib `.beam` files: 7.9%.
Fixture: `Counter` 11,196 to 11,772 bytes, `Tally` 13,192 to 13,492.

**Compile time, chunk passed out of band (E7).** Building and encoding the
term took 27 to 85 µs per module. `compile:forms/2` with and without
`extra_chunks` differed by less than run-to-run noise. This measures the
compiler option only. It does not include getting the term through Core text.

**Compile time, term carried in the Core text (E10).** `core_scan` +
`core_parse` + `compile:forms`, extra time over the same module with no
carrier, OTP 28.5:

| Carrier | Largest class (`SystemNavigation`, 95,676 bytes of docs) | Median class | Whole stdlib | Core text added |
|---|---|---|---|---|
| `docs_v1` literal, binaries one segment per byte (how codegen writes binaries today) | +848 ms, 3.5 MB | +23 ms | **+6.4 s** | 27.3 MB |
| ETF bytes as one Core string literal, lifted with `list_to_binary/1` | +4.2 ms, 111 KB | +0.5 ms | **+0.1 s** | 1.1 MB |
| the same, zlib-compressed first | +4.0 ms, 72 KB | +0.4 ms | +0.1 s | 0.8 MB |

OTP 29.1.2 gave 6.0 s and 0.09 s for the first two rows. These are sums of
single-threaded times; a parallel build divides them.

The per-byte form is unusable. The string form is cheap. Two more facts:

- The doc text is already in the Core text once, per byte, inside
  `register_class/0`. That existing cost is the same order as the first row
  and is not caused by this ADR. A cheaper binary literal in
  `beamtalk-cerl-doc` would help every compile; that is a separate change.
- The REPL parses each class's Core text twice (`class_meta_from_core_erlang`
  in the loader, then the compile).

**Other.**

| | Result |
|---|---|
| Build caching | The chunk or side file is a function of the same source as the `.beam`. |
| Reproducible output | `term_to_binary(_, [deterministic])` is stable. `compressed` output depends on the zlib version, so compressed bytes are reproducible on one machine but not guaranteed across machines. Annotations must be line numbers only, with no file path. |
| Memory | A `Docs` chunk is not loaded with the module. A carrier function that is not lifted out is. |
| Releases | `beam_lib:strip/1` removes the chunk (verified). ADR 0125 defaults to `strip-beams = false`, and stripping also removes the abstract code ADR 0075 reads. So a default release would carry an in-`.beam` chunk. |

### Q5. Consumers

| Consumer | `beam_language = beamtalk` | `beam_language = erlang` | Notes |
|---|---|---|---|
| Erlang `h/1`, `h/2`, `h/3` | works | not needed | Renders the Markdown. Does not load the module. Shows non-exported entries. |
| `code:get_doc/1` | works, from a chunk or a side file | works | |
| Elixir `Code.fetch_docs/1` | works, from a chunk or a side file | works | |
| IEx `h Mod`, `h Mod.fun`, `h Mod.fun/n` | works | works | IEx loads the module first, which runs `on_load` and so `register_class/0`. Without the Beamtalk runtime on the code path it prints `Could not load module ..., got: on_load_failure`. With it, asking for help registers the class. |
| ExDoc 0.40.4 | skipped: `unsupported language (beamtalk)` | crashes: `could not find base directory` | See below. |
| ELP hover, `erlang_ls` hover | not tested | not tested | The ELP binary could not be downloaded where the spike ran. |
| Beamtalk's own `beamtalk_native_docs` | would read it | would read it | It ignores `beam_language`. See the constraint on ADR 0033. |

ExDoc picks a backend from `beam_language` with a fixed two-entry table
(`:elixir`, `:erlang`). Its Erlang backend also needs the module's Erlang
abstract code: a `-file` attribute and a function form per documented
function. A from-Core `.beam` has neither. With `beam_language = erlang` and a
synthetic abstract-code chunk swapped in after compilation, ExDoc rendered
`Counter` with the Beamtalk signatures and listed exported functions only, so
`Tally`'s two methods were missing. That test rewrote the `.beam`, which the
constraints rule out. IEx `h` was tested with an in-`.beam` chunk only.

## The two workable designs

Both produce the same `docs_v1` term with the same mapping. They differ in
where the bytes live and what has to change to put them there.

### Design A: inside the `.beam`, written by the compiler

Codegen emits `'__beamtalk_docs'/0` returning the ETF bytes as one Core string
literal. Each compile path lifts it out and passes it as `extra_chunks`.

- For: the docs travel with the `.beam` everywhere a `.beam` goes (packages,
  releases, a copied `ebin`). No extra file. On by default, so it needs no
  action from a package author.
- Against:
  - All three compile paths must lift the carrier, or the escript fallback
    must go. Where it is not lifted, the function stays exported and loaded,
    and the output differs by backend. A build test must assert that no
    `bt@*` module exports `__beamtalk_docs/0`, and a carrier that does not
    fold to a literal must be a hard error.
  - On the compiler-server path the chunk is built for a binary that nothing
    can read docs from (Q2). It should be built only on paths that write a
    `.beam` to disk.
  - 7.9% on every stdlib `.beam`, including in default releases.
  - The doc text is in each `.beam` twice.
  - `compressed` bytes inside the `.beam` make cross-machine reproducibility
    depend on zlib; uncompressed costs about 25% of `.beam` size instead of
    7.9%.

### Design B: `doc/chunks/<Mod>.chunk` side files, opt-in

The CLI writes one chunk file per module into `doc/chunks/` beside `ebin`, on
request: a build flag, or `beamtalk doc --format eep48`. The Rust side already
has an ETF encoder (`beamtalk-compiler-port` uses one), so the CLI can write
the file from the same analysis that feeds codegen, with no Erlang step.

- For:
  - Zero change to any `.beam`, to codegen output, or to any of the three
    compile paths. No carrier.
  - Zero cost unless asked for.
  - `code:get_doc/1`, `h/1` and `Code.fetch_docs/1` read it natively
    (Q2, E11). It is the layout OTP uses for its own docs.
  - The deferred runtime layer (Q2) would write the same file format into a
    workspace directory.
- Against:
  - It is a second build output. Packaging and release assembly must copy
    `doc/chunks/` or the docs are lost. Whether Beamtalk packages and releases
    carry a `doc/` directory today was not checked.
  - Off by default means most packages will ship without it unless publishing
    turns it on.
  - If the runtime layer is later written in Erlang from `class_state`, the
    mapping exists in Rust and Erlang, and needs a shared conformance fixture.

ADR 0008's `.docs` file was different in kind: it was an *input* to a
post-compile rewrite, written next to the `.core` file and deleted or left
stale by hand. A chunk file is a final output read directly by OTP.

## Decision

**Recommended: Design B.** Emit EEP-48 as opt-in `doc/chunks/<Mod>.chunk`
side files, and turn it on for `beamtalk build` of a package being published
and for the stdlib. Keep Design A as the documented next step if docs need to
travel inside the `.beam`.

The reason is proportion. No consumer has asked. Design B adds no cost and no
risk to the default build, touches none of the three compile paths, and can be
removed by deleting a flag. Design A is sound with the string carrier, but it
puts a new obligation on every compile path for a feature nobody is waiting
for. The maintainer may reasonably weigh "works with no flag" higher and pick
A; the sections above are written so that either can be accepted.

The rest of the decision applies to both designs.

1. **Runtime docs stay the source of truth** (ADR 0033). EEP-48 is a derived
   export for Erlang and Elixir tooling.
2. **`beam_language` is `beamtalk`; the format is `text/markdown`.**
3. **Beamtalk's own tools do not read it.** `beamtalk_native_docs` filters
   `beam_language = beamtalk` (or `bt@` modules) and answers as it does today,
   so REPL `:h Erlang`, LSP hover and MCP keep one doc source for Beamtalk
   classes.
4. **Keys are the compiled function names**, produced by the same
   `selector_mangler` functions codegen uses. This is a deliberate exception
   to ADR 0139, and a narrow one: EEP-48 is an Erlang-facing index, its key is
   an MFA by specification, and `h(Mod, Fun, Arity)` is typed by someone
   already working in raw MFA. ADR 0139 itself writes Erlang frames as
   `module:function/arity`. No Beamtalk-facing surface shows these keys. The
   *signature* shown for each entry is Beamtalk notation
   (`Counter >> add: n :: Integer -> Counter`).
5. **No claim that entries are callable.** Entry metadata carries `selector`,
   `class_side` and `exported`. Module metadata carries `beamtalk_class`,
   `kind` (`value`, `actor`, ...), `uses`, `superclass` and
   `superclass_module` (an atom, so a static consumer can follow the chain).
   The calling convention per kind is documented once, in
   `docs/learning/16-beam-interop.md`, not implied by the key.
6. **Local methods only.** Inherited methods are not copied.
7. **Skip an entry whose key collides with a generated export** that is not
   the method (the actor `init/1` case). The list of generated exports comes
   from codegen, not from a hand-written table.
8. **Extension methods (ADR 0066) are omitted.** They are not part of the
   target class's module.
9. **Undocumented methods get an entry with doc `none`**, so the signature
   still shows. Non-public methods get `hidden`.
10. **Annotations are line numbers only.** Side files are written
    uncompressed and with `deterministic` encoding.
11. **Classes with no `.beam` on disk are out of scope for now**: REPL-defined
    classes, and runtime `doc:` edits. `:h`, `Counter doc` and the MCP tool
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

A selector the class does not define:

```erlang
2> h('bt@demo@counter', nope).
{error,function_missing}
```

A package built without the flag, or a REPL-defined class:

```erlang
3> code:get_doc(Mod).
{error,missing}
```

### Known regression to accept or fix

With either design, a compiled class that is reloaded inside a workspace
stops having EEP-48 docs: after the reload `code:which/1` is the `.bt` path,
so `h/1` goes from documented to `{error, missing}` until the node restarts.
Beamtalk's own `:h` is unaffected. The fix is the deferred runtime layer.

### Build modes

- `dev` builds: off unless the flag is given.
- Publishing a package and building the stdlib: on.
- Dependencies: a dependency carries whatever its own build wrote. One built
  by an older toolchain has no chunk files and reports `{error, missing}`;
  that is not an error. No provenance stamp (ADR 0098) changes.
- Releases: release assembly copies `doc/chunks/` only if asked. The default
  release carries no EEP-48 docs and no extra bytes.

## Prior Art

| Language | How EEP-48 docs are produced | Notes |
|---|---|---|
| **Erlang** (OTP 27+) | The `beam_docs` compiler pass, from `-doc` attributes in abstract code | Not reachable from Core Erlang (Q1). OTP's own applications also ship `doc/chunks/*.chunk` files. |
| **Elixir** | Its compiler builds `docs_v1` and writes the chunk when it assembles the `.beam` | The shape of Design A. |
| **Gleam** | Compiles to `.erl` and lets `erlc` do it | Needs Erlang source. |
| **LFE** | Builds the chunk in its own compiler | `beam_language` is `lfe`. |
| **Pharo** | Class and method comments are objects in the image | ADR 0033's model, which stays. |

The Gleam and LFE rows are from general knowledge and were not re-checked.

## User Impact

- **Newcomer.** No change. `:h` and `Counter doc` work as before.
- **Smalltalk developer.** No change. Docs remain live objects; EEP-48 is an
  export, like a fileout.
- **Erlang/Elixir developer.** `h/1` and IEx `h` work on Beamtalk packages
  that were built with docs. They see compiled names as keys and Beamtalk
  signatures as text. Inherited methods are not listed. IEx `h` loads the
  module, so it needs the Beamtalk runtime on the code path.
- **Operator.** Nothing by default. No change to `.beam` files or releases.
- **Tooling developer.** `code:get_doc/1` becomes a static way to read docs
  without a running workspace, where the files exist. ExDoc still does not
  work.

## Steelman Analysis

### For doing nothing

- **Operator:** "Nobody has asked for `h/1` on a Beamtalk module."
- **Language designer:** "Two representations of the same docs is how the
  first version went wrong."

### For Design A over Design B

- **BEAM veteran:** "Every other BEAM language puts docs in the `.beam`. A
  side file that someone forgets to copy is docs that silently vanish. On by
  default is the only version people will actually get."
- **Newcomer from Elixir:** "I added a Beamtalk dependency and `h` shows
  nothing, because its author did not pass a flag."

### For the runtime layer now

- **Smalltalk developer:** "If `Counter doc: '...'` changes what `:h` says, it
  should change what `h/1` says."
- **Workspace user:** "Reloading a class should not delete its docs from the
  Erlang shell."

### For `beam_language = erlang`

- **BEAM veteran:** "ExDoc and hexdocs are how the ecosystem reads docs."

### Tension points

- Default-on reach (A) against default-off cost (B).
- Static export against live docs. Both designs accept that they can differ
  after a runtime `doc:` edit or a reload.
- ADR 0139 against EEP-48's MFA index. The decision treats EEP-48 as outside
  0139's surfaces; a reader could instead argue that anything `h` prints is
  user-facing.

## Alternatives Considered

### `-doc`/`-moduledoc` attributes in Core Erlang

**Rejected: it does not work** on OTP 28 or 29 (Q1).

### A docs-only companion `.erl` compiled for its chunk

**Rejected:** a second compilation per module, and the chunk still has to be
moved into the real `.beam`.

### Inject after compilation with `beam_lib` (ADR 0008's design)

**Rejected** for ADR 0033's reason.

### Design A with the `docs_v1` term written as an ordinary Core literal

**Rejected:** +6.4 s and 27 MB of Core text for the stdlib (Q4).

### Design A with the term sent beside the Core text on the port protocol

Not prototyped. It avoids the carrier, but only the build worker speaks that
protocol; the escript fallback and `erlc +from_core` would produce a `.beam`
with no chunk, so output would still differ by backend.

### Serve EEP-48 from `class_state` at runtime

Feasible through chunk files and a changed load path (Q2). **Deferred.** It
needs the workspace to own a directory, write a file per class and per `doc:`
edit, and pass a real path to `code:load_binary/3`.
`beamtalk_module_activation` documents code that relies on REPL modules not
having a real path, so that change needs its own review. It would also fix
the reload regression. It uses Design B's file format.

### `beam_language = erlang`, for ExDoc

**Rejected.** ExDoc also needs Erlang abstract code, which a from-Core module
lacks, and hides non-exported entries, which is every actor method.
`beamtalk doc` already generates HTML. A Beamtalk backend in ExDoc is an
upstream change.

### Copy inherited methods into each class

**Rejected.** 4.7 times the size, and stale after a superclass is rebuilt.

### Selector keys instead of compiled names

Key entries as `{function, Selector, MessageArity}` to keep compiled names
out, per ADR 0139. **Rejected:** instance and class methods with the same
selector collide, and the key would name no function in the module, so
`h(Mod, Fun, Arity)` would not match what an Erlang user sees in
`Mod:module_info(exports)`.

### A separate entry kind, such as `{beamtalk_method, Selector, Arity}`

**Rejected:** `h/2` and IEx `h` look up `function` entries. Not tested; this
is from reading `shell_docs` and the IEx results.

## Consequences

Stated for the recommended Design B.

### Positive
- `code:get_doc/1`, Erlang `h/1` and `Code.fetch_docs/1` work on Beamtalk
  packages built with docs.
- No `.beam` changes, no carrier, no change to any compile path.
- No cost to the default build or the default release.
- The mapping is written once, in Rust, from the functions codegen uses.
- ADR 0033 is untouched.

### Negative
- A second build output that packaging and release assembly must carry.
- Off by default, so docs exist only where a build asked for them.
- A class reloaded in a workspace loses its EEP-48 docs until restart.
- REPL-defined classes and runtime `doc:` edits are not reflected.
- ExDoc does not render Beamtalk modules. ELP and `erlang_ls` are unknown.
- Inherited and extension methods do not appear.
- Compiled names (`class_` prefix, inflated arities) are visible as keys to
  Erlang-side users, as an explicit exception to ADR 0139.
- `beamtalk_native_docs` needs a filter so it does not become a second doc
  source for `bt@` modules.

### Neutral
- `:h`, `Counter doc`, the MCP `docs` tool and `beamtalk doc` (HTML) do not
  change.
- Native Erlang modules keep getting their chunk from `-doc` attributes.

### If Design A is chosen instead
- Add: every compile path lifts the carrier or the escript fallback is
  removed; a build test that no `bt@*` module exports `__beamtalk_docs/0`; a
  hard error when the carrier does not fold; the chunk is built only where a
  `.beam` is written to disk; 7.9% on `.beam` size, shipped in default
  releases unless a docs-only strip is added.
- Remove: the second build output and the opt-in.
- Check first that the lift can live in `beamtalk_compiler` next to
  `constant_meta/1`. The build worker's node has only the compiler
  application's `ebin` on its path; `beamtalk_compiler` is in that
  application.

## Implementation

Not part of the spike. A rough shape for Design B, if accepted:

1. **Docs term builder (M).** One Rust function from the analysed class to
   the `docs_v1` structure. Keys from `selector_mangler`; signatures from
   whatever `class_registry.rs` and `native_facade.rs` already use for
   `methodSignatures`, not a second formatter.
2. **Writer and flag (S).** ETF-encode with the existing encoder and write
   `doc/chunks/<Mod>.chunk` beside `ebin`. Wire the flag, and
   `beamtalk doc --format eep48`.
3. **`beamtalk_native_docs` filter (S).**
4. **Tests (S).** `code:get_doc/1` on a fixture package returns the expected
   entries on OTP 28 and 29; every key that claims `exported` is in the
   module's exports (this is the check that the keys and codegen agree); two
   builds write identical files.
5. **Packaging (S).** Decide and implement whether published packages and
   releases carry `doc/chunks/`.
6. **Docs (S).** `docs/learning/16-beam-interop.md`,
   `docs/beamtalk-language-features.md`,
   `docs/development/surface-parity.md`.

The prototype's shortcuts should not be copied: it finds the doc maps by
scanning `register_class/0`, builds class-side names by string concatenation,
and reads `#class_state{}` by field position.

## Revision notes

Changes from the first draft, after a three-pass review:

- The compile-time cost of Design A was measured without the carrier going
  through Core text. E10 measures it end to end; the per-byte literal costs
  seconds, the string literal does not.
- The escript fallback compile path was missing from the analysis.
- The ADR 0139 conflict was not cited, and "callable" was claimed for keys
  that are not.
- The side-file design was dismissed in a paragraph. It is now a full design
  and the recommendation.
- Corrected: the build worker does not read `__beamtalk_meta/0`; the two
  `compile_core_forms` use different options; default releases are not
  stripped; `beamtalk_protocol_object` does have a `.beam` (it is a
  hand-written runtime module, in a different application from the stdlib
  directory the spike script looked in).
- Added: `superclass_module`, mangled names, native facades, extension
  methods, the `beamtalk_native_docs` filter, the workspace reload regression,
  build modes, annotation and compression rules.

## References
- Related issues: BT-4852 (this spike), BT-499 (first EEP-48 implementation), BT-772 (its removal), BT-1851, BT-1900, BT-1905 (EEP-48 for native Erlang modules)
- Related ADRs: [ADR 0008](0008-doc-comments-and-api-documentation.md), [ADR 0033](0033-runtime-embedded-documentation.md), [ADR 0056](0056-native-erlang-backed-actors.md), [ADR 0066](0066-open-class-extension-methods.md), [ADR 0075](0075-erlang-ffi-type-definitions.md), [ADR 0089](0089-typed-document-leaves.md), [ADR 0098](0098-build-artifact-provenance.md), [ADR 0119](0119-class-module-name-resolution-registry.md), [ADR 0125](0125-otp-releases-and-upgrade-compatibility.md), [ADR 0139](0139-beam-abstraction-boundaries.md)
- Prototypes: [`spikes/eep48-doc-chunks/`](../../spikes/eep48-doc-chunks/README.md)
- EEP-48: https://www.erlang.org/eeps/eep-0048.html
- EEP-59: https://www.erlang.org/eeps/eep-0059.html
- `compile` options (`extra_chunks`): https://www.erlang.org/doc/apps/compiler/compile.html
