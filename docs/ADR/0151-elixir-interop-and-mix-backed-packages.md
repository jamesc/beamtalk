# ADR 0151: Elixir Interop and Mix-Backed Packages

## Status
Proposed (2026-11-16)

Evidence for every factual claim marked "verified" below is in
[0151-elixir-interop-evidence.md](0151-elixir-interop-evidence.md): commands,
versions (Elixir 1.20.1, OTP 28.5, rebar3 3.27.0, `rebar_mix` 0.5.1, Hex 2.7.0,
Beamtalk 0.5.0-edge+02378f0) and a list of what was **not** run.

## Decisions the maintainer has to make

Each is argued in the section named. The recommendation is the first option in
each section.

| # | Decision | Recommendation | Section |
|---|---|---|---|
| D1 | How Elixir packages are built and declared: Mix driven directly with a new `[elixir.dependencies]` table, or rebar3's `rebar_mix` plugin under `[native.dependencies]` | Mix direct, new table | Part 1 |
| D2 | What happens to `String`s nested in arguments (and, secondarily, in results) | Deep-unwrap arguments to Elixir modules; leave results as they are | Part 3 |
| D3 | How `?` and `!` function names are written | `_question` / `_bang` suffix | Part 2 |
| D4 | Whether a Beamtalk class can implement an Elixir protocol or behaviour | No, out of scope; a later ADR | Part 4 |
| D5 | Whether Elixir structs get a Beamtalk class of their own | No, plain `Dictionary` for now | Part 3 |
| D6 | The supported Elixir window and the Elixir prerequisite | Elixir 1.19 or later, only needed by packages that use Elixir | Part 1 |

## Context

### Problem

Much new BEAM library work is written in Elixir: Req and Finch (HTTP), Jason,
Swoosh, Nx and Explorer, among others. Elixir modules are plain BEAM modules,
so Beamtalk can already reach them. Reaching them *usefully* needs four things
that do not exist:

1. **A way to depend on an Elixir package.** `[native.dependencies]` builds
   through rebar3, which cannot build a Mix project.
2. **A way to write the call.** `Erlang lists` works because `lists` is a plain
   word. `'Elixir.Enum'` has a dot, so it cannot follow `Erlang`.
3. **Correct values at the boundary.** The FFI rules were designed for Erlang
   modules. Several of them are wrong or dangerous for Elixir (see below).
4. **Types.** Elixir calls type as `Dynamic` unless the spec reader is told
   how to reach Elixir's specs.

ADR 0028 left an `Elixir` global as future work ("`Elixir` global — same proxy
pattern, maps `Elixir enum` → `'Elixir.Enum'`"). ADR 0075 left "Elixir stubs"
as future work. This ADR picks both up and answers the five questions in
BT-4870.

### What works today (verified)

Elixir was added to `ERL_LIBS` by hand for all of these.

```beamtalk
Erlang erlang apply: #'Elixir.Enum' with: #sum with: #(#(1, 2, 3))
// => 6
Erlang erlang apply: #'Elixir.Enum' with: #map with: #(#(1, 2, 3), [:x | x * 10])
// => #(10, 20, 30)
Erlang erlang apply: #'Elixir.URI' with: #parse with: #("http://x.com:8080/a?b=1")
// => #{#__struct__ => #Elixir.URI, #host => "x.com", #port => 8080, ...}
```

Blocks are Elixir functions. `nil` is `nil`. `{:ok, v}` and `{:error, r}`
become a `Result` (ADR 0076). Protocol functions are ordinary functions
(`Enumerable.count`, `String.Chars.to_string`). Default arguments need nothing:
Elixir exports one function per arity, and Beamtalk's arity is the keyword
count. An end-to-end HTTP request through Req, Finch and Mint answered `200`
from Beamtalk once the URL was a raw binary. Starting `:req` with
`application:ensure_all_started/1` started Elixir's own `elixir` and `logger`
applications as ordinary dependencies.

### What fails today (verified)

| Problem | Observed |
|---|---|
| No Elixir on the code path | `Undefined function: Elixir.Enum:sum/1`, a clean error; nothing tells the user why |
| Elixir package in `[native.dependencies]` | `rebar3 compile failed: No project builder is configured for type mix` |
| `Erlang perform: #'Elixir.Enum'` | `Erlang does not understand 'Elixir.Enum'` |
| A Beamtalk `String` nested in an argument | Tagged `{beamtalk_string, <<"a">>}` reaches Elixir. `Enum.join(["a","b"])` and `Jason.encode!(%{"a" => "b"})` raise `Protocol.UndefinedError`. `ArgumentError.exception(message: "boom")` **succeeds** and builds a struct whose message is the tagged tuple: silent corruption. |
| Through `erlang:apply/3`, a top-level `String` | Also nested (it is inside the argument list), so the boundary's top-level unwrap never runs. `Req.get!("http://...")` raises. |
| An Elixir exception | `runtime_error` whose message is the raw printed struct (`#{reason => enoent, path => ..., '__struct__' => 'Elixir.File.Error', ...}`), not `Exception.message/1` |
| A macro (`Logger.info`, `Kernel.is_nil`) | `Undefined function: Elixir.Logger:info/1`, with no mention that it is a macro |
| A numeric list that happens to be printable | `Enum.map([1,2,3], &(&1 + 64))` answers the String `"ABC"`, because stock-mode result coercion treats it as a charlist. (`Erlang lists seq: 65 with: 70` answers `"ABCDEF"` today.) |
| Decoded JSON | `Jason.decode!("{\"a\":1}") at: "a"` raises `key_error`: decoded keys are raw binaries (class `Binary`), a Beamtalk String key is tagged. This is ADR 0140's mismatch, not new. |
| Elixir's VM warning | `warning: the VM is running with native name encoding of latin1 ...` on every run unless `+fnu` is set |

### Constraints

- **Transparent interop** (ADR 0028 §2): values are not wrapped. Anything below
  that changes a value at the boundary has to be a rule that stays a pure
  function of the target module.
- **No new user prerequisite** (ADR 0125, Constraints): "Erlang/OTP 27+ with
  `erl` and `erlc` on PATH". Elixir can only be required by packages that
  choose to use Elixir.
- **One code-path source.** `BeamEnvironment::from_layout`
  (`crates/beamtalk-cli/src/commands/beam_environment.rs`) is "the single
  source of truth for what the BEAM needs to know about this project", used by
  `build`, `test`, `run`, `repl` and `workspace`. Elixir must enter there, not
  per command.
- **One coercion-mode recognizer.** `beamtalk_erlang_proxy:coercion_mode/1`
  (`stock | beamtalk`) mirrors `beamtalk_core::ast::is_beamtalk_erlang_module`,
  held together by the shared fixture
  `runtime/apps/beamtalk_runtime/test/fixtures/is_beamtalk_module_conformance.json`.
  A new mode must extend that pair, not add a second one (AGENTS.md, no
  duplicate implementations).
- **ADR 0125.** A release stages applications from their generated `.app`,
  boots with `systools`, and must not re-derive the OTP window.
- **No compiler or runtime change in this session.** This ADR is a document.

## Decision

### Part 1: Elixir packages and the Elixir code path (Q1)

#### 1.1 Build mechanism (D1)

**Recommended: drive Mix directly.** For a package with
`[elixir.dependencies]`, `beamtalk build` writes a `mix.exs` into
`_build/dev/mix/`, runs `mix deps.get` and `mix deps.compile` with
`MIX_ENV=prod`, then `mix compile` for protocol consolidation, and collects
`_build/prod/lib/*/ebin` plus `consolidated/` into the code path.

```toml
[package]
name = "fetcher"
version = "0.1.0"

[elixir]
version = "~> 1.19"          # optional; checked against the installed Elixir

[elixir.dependencies]
req   = "~> 0.5"
jason = "~> 1.4"
```

Mix, not rebar3, resolves the whole Hex graph in this case. The
`[native.dependencies]` Erlang packages of the same package are added to the
generated `mix.exs` as ordinary Hex dependencies, so there is **one** graph and
one version of a shared package such as `telemetry`. (Mix built `telemetry`, a
rebar3 package, in the probe; which rebar3 it used was not checked.)

| | Mix direct (recommended) | `rebar_mix` under `[native.dependencies]` |
|---|---|---|
| Change to the generated config | New generator for `mix.exs`, new lock section | One line in `rebar.config`: `{plugins, [rebar_mix]}` (verified: Jason and Req both built) |
| Req, one cold run each (not like for like: the plugin also fetched itself, built 15 packages including six that Mix never selected, and ran `mix deps.get` per dependency) | 9 packages fetched in 5.5 s, compiled in 8.3 s | 1 m 56 s |
| Closure matches the lock | Yes: Mix's closure is exactly what `mix.lock` lists | No: `rebar.lock` lists 9 packages, `lib/` holds 15 (plus the project); `brotli`, `castore`, `decimal`, `nimble_csv`, `plug`, `plug_crypto` are not in it (verified) |
| Dependency's dev-only dependencies fetched | No | Yes: the plugin runs `mix deps.get` inside each dependency, which fetched `ex_doc`, `dialyxir` and others for Jason (verified) |
| Elixir version constraint | `elixir: "~> 1.19"` in `mix.exs`, enforced by Mix | None |
| Checksums | `mix.lock` carries inner and outer checksums for the full closure (verified), the same fields `beamtalk.lock` already stores for native packages | Only the top-level resolution reaches `rebar.lock` |
| Without Elixir installed | Beamtalk can check first and print an actionable error | `error: missing_elixir`, shown only as "Failed creating providers" (verified) |
| Third-party code in the build | None beyond Mix | The plugin itself is fetched from Hex on every clean build |
| `[native.overrides]` (rebar3 `erl_opts` override) | **Not available** for packages that use `[elixir.dependencies]` (see Consequences) | Still works |
| Packages with both Erlang and Elixir dependencies | One graph | Two resolvers that can disagree |

`rebar_mix` is the smaller change and keeps `[native.overrides]`. It is a
reasonable choice if the maintainer prefers to minimise new code. The measured
costs are the unpinned closure and the build time. The recommendation is Mix
direct because a lockfile that does not name everything that gets built breaks
the reproducibility `beamtalk.lock` exists to give.

Rejected without a full option: **require pre-built Elixir beams** (user builds
Elixir outside Beamtalk and points a `path` dependency at it). That works today
and stays available; it is not a package story.

**Resolver selection for the whole build.** The rule is per build, not per
package: if the root package or any `path`/`git` Beamtalk dependency declares
`[elixir.dependencies]`, Mix resolves the **union** of every package's
`[native.dependencies]` and `[elixir.dependencies]`. Otherwise nothing changes
and rebar3 resolves as today. Adding a first Elixir dependency must not move
versions already locked: the existing `[[native_package]]` pins are written into
the generated `mix.lock` before resolution, so a locked `cowboy` stays put
unless the new dependency makes it impossible, in which case the build fails
with the conflict instead of upgrading silently. Locked native packages keep
their `[[native_package]]` entries; `[[elixir_package]]` holds only packages
Mix added. Phase 2 starts with a test that adds `jason` to a project with
pinned native packages and asserts every pin is unchanged. Not yet run.

#### 1.2 Lockfile

`beamtalk.lock` gains `[[elixir_package]]` entries for the packages **Mix
adds to the closure** (native pins stay where they are, see above), the same shape as `[[native_package]]`: name, version, inner
checksum, outer checksum, plus the build tool (`mix` or `rebar3`). Before
`mix deps.get`, the build writes `_build/dev/mix/mix.lock` from those entries,
so Mix reproduces the locked set. This is the mechanism `prepare_rebar_lock`
already uses for `rebar.lock` (`crates/beamtalk-cli/src/commands/build/native.rs`).
After a re-resolve, the entries are read back from `mix.lock`. The round trip
(`beamtalk.lock` to `mix.lock` and back) was **not** tried.

#### 1.3 Getting Elixir's own applications onto the code path

Elixir's `elixir` and `logger` applications live in the Elixir installation,
not in `_build`. They must reach every node Beamtalk starts.

- **Locate once.** Run `elixir -e 'IO.puts(:code.lib_dir(:elixir))'` (about
  0.35 s per call here), or read `[elixir] lib-dir` / `ELIXIR_LIB_DIR` when
  set. Cache the answer under `_build/`, keyed by the resolved real path of the
  `elixir` executable and the `ELIXIR_LIB_DIR` value, and revalidate it on use by
  reading `<lib>/elixir/ebin/elixir.app` (its `vsn` must match the cached one).
  An mtime key is wrong under mise or asdf, where `elixir` is a shim (this
  container's is); a version switch would not change it. Only packages with `[elixir]` pay this.
- **One place.** `BeamEnvironment::from_layout` gets a new step: when the
  package or any dependency has Elixir packages, add `<lib>/elixir/ebin` and
  `<lib>/logger/ebin`, then the Mix `_build/prod/lib/*/ebin`, with the
  `consolidated` directory first on the code path. Every command that already
  consumes `BeamEnvironment` (`build`, `test`, `run`, `repl`, `workspace`)
  inherits it. Other Elixir applications (`eex`, `iex`, `mix`) are added only
  when a staged `.app` names them in `{applications, ...}`.
- **Starting applications.** `BeamEnvironment.otp_apps` already feeds
  `application:ensure_all_started/1`. It gains the top-level
  `[elixir.dependencies]` names. That call starts `elixir` and `logger` as
  dependencies (verified for `:req`).
- **VM flag.** The warning means the VM started with a non-UTF-8 locale. The
  probes silenced it with `ELIXIR_ERL_OPTIONS=+fnu`, which only the `elixir`
  launcher reads, not Beamtalk's own `erl` launch. Phase 2 must check the
  locale and Beamtalk's real launch path first; the proposal is `+fnu` added by
  whatever launches the VM when Elixir is on the path, and in the generated
  `vm.args` for releases. Its effect on Beamtalk's own file names was not
  checked, so this is a proposal, not a verified requirement.

#### 1.4 Releases (ADR 0125)

An Elixir package's applications join the release closure like any other
`.app`-declared dependency (`resolve_declared_deps`,
`crates/beamtalk-cli/src/commands/release/closure.rs`), with the Elixir
installation as one more place to look after the OTP lib dir and the rebar3
lib dir. Verified with a hand-built `.rel`:

- `elixir.app` declares the OTP `compiler` application, so the `.rel` must list
  it; otherwise `systools:make_script` fails with
  `{undefined_applications, [compiler]}`.
- Elixir's lib dirs are `lib/elixir/ebin` with no version in the name. Staging
  reads the `.app` `vsn` and creates `lib/elixir-1.20.1/ebin`, as ADR 0125
  §1.3 already prescribes.
- ADR 0125's absolute-path-plus-`RELDIR`-variable rule is required here too: a
  relative path built a `.boot` that died with `undef` on `elixir:start/2`.
- With that rule, a release holding `elixir`, `logger`, `compiler` and `jason`
  booted and `Elixir.Enum:sum([1,2,3])` answered 6.
- The release is self-contained: Elixir is copied from the **build** host like
  ERTS, so the target needs no Elixir. `beamtalk-provenance.json` records the
  Elixir version next to `otp_release`, and the `--upgrade-from` preflight's
  "Toolchain" block (ADR 0125 §2.3) gains an Elixir row.

#### 1.5 Pinning versions (D6)

- `[elixir] version` is checked against `elixir --version`. A mismatch is a
  build error naming both.
- Elixir's own compatibility page (fetched 2026-11-16) lists OTP support as
  1.20: 27 to 29; 1.19: 26 to 28; 1.18: 25 to 27. ADR 0125 §3.1 fixes the
  Beamtalk OTP window at 27 to 28 today. **Only Elixir 1.19 and later cover
  the whole window**, so the recommendation is a documented minimum of
  Elixir 1.19, declared in `otp-support.toml` next to the OTP window and read
  by `beamtalk doctor`, `build` and the CI matrix the same way (one declared
  source, ADR 0125 §3.1). When the OTP window moves to 28 to 29, 1.19 stops
  covering it and the minimum rises mechanically.
- The invariant is **build and run on the same Elixir**. Beamtalk compiles Mix
  dependencies on the machine that runs them and bundles that Elixir in
  releases, so a skew arises only with pre-built `path` dependencies. Whether
  beams built by one Elixir minor load on another was **not tested** (the
  second Elixir download was blocked); the ADR does not rely on it.
- **Protocol consolidation.** `mix compile` in the generated project writes
  `consolidated/` (8 protocol beams in the probe). Putting it first on the code
  path is what `mix release` does. It made about 7% difference in one
  micro-benchmark (not run through Beamtalk) and unconsolidated dispatch worked
  in every probe, so it is an optimisation, on by default. It is incompatible
  with Beamtalk-defined protocol implementations (Part 4), which would need an
  unconsolidated build.

#### 1.6 Prerequisite policy

Elixir is required **only** to build a package that declares
`[elixir.dependencies]`, and then only on the build machine. Packages without
it, and every release of any package, need nothing new. `beamtalk doctor` gains
an Elixir check that is informational unless the current package needs it.

### Part 2: Naming (Q2)

#### 2.1 The `Elixir` global

`Elixir` is a sealed, typed `ProtoObject` subclass in `stdlib/src/elixir.bt`
beside `Erlang`, with the same `@intrinsic` lookup. A capitalised unary send
names an Elixir module; consecutive capitalised sends build a dotted path; the
first lowercase or keyword selector is the function.

```beamtalk
Elixir Enum sum: #(1, 2, 3)                   // 'Elixir.Enum':sum([1,2,3])
Elixir String Chars to_string: 42             // 'Elixir.String.Chars':to_string(42)
Elixir Enum map: xs with: [:x | x * 10]       // arity 2
Elixir String split: "a,b,c"                  // split/1
Elixir String split: "a,b,c" with: ","        // split/2
Elixir String split: "a,b,c" with: "," with: #({#parts, 2})   // split/3
proxy := Elixir Enum                          // #ErlangModule<Elixir.Enum>
```

No parser change is needed: `Erlang Enum` already parses as a unary send and
reaches the FFI module check (verified, E1g). The compiler recognises the
chain the way it recognises `Erlang <mod> <fn>:` and emits a direct call with
the new `elixir` coercion mode (Part 3). The value form `Elixir Enum` answers an
`ErlangModule` proxy for the atom `'Elixir.Enum'`, so `call:args:` and dynamic
dispatch work. One proxy rule is new: in `elixir` mode a capitalised unary
selector on a proxy extends the module path, so `proxy := Elixir String. proxy
Chars to_string: 42` also reaches `'Elixir.String.Chars'` (without it the proxy
would call a function named `Chars`). The static chain and the dynamic form must
agree, which a test pins. The "unknown module" diagnostic gains a hint when the
module is missing and no Elixir is on the path: `Elixir is not on the code
path; add [elixir] to beamtalk.toml`.

Not chosen: writing `Erlang perform: #'Elixir.Enum'`-style dynamic lookup. It
stays possible through `Erlang erlang apply:with:with:`, but the arguments are
then nested in a list and miss the boundary's top-level handling (see Context).

#### 2.2 Function names ending in `?` or `!` (D3)

A Beamtalk selector cannot contain `?` or `!`. The mapping has to be
mechanical, reversible and collision-free.

| Option | `String.valid?/1` | `File.read!/1` | Collisions in 3,847 real names | Verdict |
|---|---|---|---|---|
| `_question` / `_bang` suffix (recommended) | `String valid_question: s` | `File read_bang: p` | 0 | Verbose, unambiguous, readable without a table |
| `_p` / `_bang` | `valid_p:` | `read_bang:` | 1 (`mkdir_p`) | Short; lisp-style `p` is not a Beamtalk convention |
| `call:args:` only | `(Elixir String) call: #'valid?' args: #("x")` | same | none | No new rule, but every predicate and bang call is noisy; 416 such names exist |
| Stubs supply idiomatic aliases (`isValid:`) | `String isValid: s` | `File readOrRaise:` | none | Best reading, but needs per-library curation and a stub feature that renames; ADR 0075 stubs cannot rename today |

The runtime mapping lives in `selector_to_function/1`
(`beamtalk_erlang_proxy.erl`): in `elixir` mode a trailing `_question` or
`_bang` is replaced by `?` or `!` before the atom lookup. The compile-time side
uses the same table, shared through a fixture (Part 3's conformance rule).
Operators (`&&&`, `<<<`, `!=`) use `call:args:`. They are rare, and most Elixir
operators are macros anyway. The escape hatch works for everything else.

The stubs option is the long-term answer to *readability* and is not exclusive:
the suffix rule is the always-works fallback, as `with:` is for keywords
(ADR 0028).

#### 2.3 Default arguments

No change. Elixir defines `String.split/1`, `/2` and `/3` as three exports and
Beamtalk selects by keyword count (verified for `String.split` and
`Enum.join`). The `Docs` chunk records the defaults (`defaults: N` in the
metadata), which tooling can use to show that the three are one function.
Whether the LSP groups them was not looked at.

### Part 3: Values at the boundary (Q3)

#### 3.1 An `elixir` coercion mode

`stock` and `beamtalk` modes are a pure function of the module atom. Add a
third, `elixir`, for atoms beginning `Elixir.`. It is recognised in one place
in Rust (`is_elixir_erlang_module`, beside `is_beamtalk_erlang_module` in
`beamtalk-core/src/ast`), mirrored by `beamtalk_erlang_proxy:coercion_mode/1`,
and covered by the existing `is_beamtalk_module_conformance.json` fixture
extended with `elixir` cases (renamed to `ffi_coercion_mode_conformance.json`).
Codegen precomputes it as it does the other two (`ffi_coercion_mode` in
`beamtalk-codegen/src/core_erlang/util.rs`).

The mode follows the **statically named module**. `Erlang erlang apply: #'Elixir.X' ...` keeps the `stock` rules, because its module is `erlang`: the same Elixir function follows two rule sets depending on how it is reached. That is accepted for the escape hatch and documented; the alternative, making `erlang:apply/3` inspect its first argument, would add a per-call check to an Erlang BIF path and is not proposed.

The rules of the `elixir` mode, each justified by a failure above:

| Rule | `stock` today | `elixir` | Why |
|---|---|---|---|
| Nested `String` in arguments | shallow unwrap, then retry on `badarg` / `function_clause` | **deep-unwrap before the first call; no retry** | Retry cannot see `Protocol.UndefinedError`, and a call that *succeeds* with a tagged String inside is undetectable (D2) |
| Binary to charlist retry on `badarg` | yes | **no** | Elixir raises `ArgumentError`, not `badarg`; strings are binaries |
| Beamtalk actor in arguments | converted to its pid only inside the `badarg` retry (`coerce_arg`) | **converted to its pid in the up-front walk** | With no retry, `Process.monitor(actor)` or a PubSub subscribe would otherwise receive the object record |
| Value returned by a Beamtalk block called from Elixir | not unwrapped | **block wrapped at the boundary so its return value is deep-unwrapped** (design; blocks nested in arguments are wrapped by the same walk) | `Enum.map_join(xs, ",", [:x \| x printString])` and `Enum.sort_by` on a String field otherwise hand tagged Strings to Elixir. Cost: one extra fun layer per block. |
| Charlist result to `String` scan | yes | **no** | Turns `[65,66,67]` into `"ABC"`. A user who wants a charlist conversion calls `Elixir List to_string:` (verified) |
| `{:ok, v}` / `{:error, r}` to `Result` (ADR 0076) | yes | yes | Verified on `File.read`, `Enumerable.count` |
| Bare `:error` to `Result error: nil` | yes | yes | Verified on `Integer.parse`; the idiom in Elixir for "no value" |
| `error:Struct` with `__exception__ := true` | generic `runtime_error`, raw dump | mapped (3.4) | Verified failure |
| Missing function | `does_not_understand` | same, plus a macro hint (Part 4) | |

#### 3.2 Strings (D2)

| Option | What it does | Fixes | Costs |
|---|---|---|---|
| **A. Deep-unwrap arguments (recommended)** | Every `String` anywhere in the arguments becomes a raw binary before the call. Results are untouched. | All the failures above that involve passing Strings in (JSON encode, `Req` options, struct construction). `Jason.encode!(%{"a" => "b"})` works. | A walk over the arguments per call. A `String` that goes into an Elixir container and comes back is a `Binary`, not a `String` (the shallow unwrap today keeps the tag on round trips such as `Enum.sort`). Beamtalk objects held in arguments have their `String` fields rewritten too (the note in `docs/beamtalk-native-erlang.md` already warns about this for the retry). |
| B. A plus tag results | Also convert every valid-UTF-8 binary in results to a `String` | Also fixes `decode! at: "a"` | Rewrites every result; turns byte data that happens to be UTF-8 into a `String`; the representation flip ADR 0140 rejected, applied on one side. O(result) per call (a DataFrame or a large decoded document). |
| C. Status quo (shallow unwrap and retry) | | | The silent-corruption and `Protocol.UndefinedError` failures stay. |
| D. Spec-directed conversion | Use the callee's `@spec` | | ADR 0140 rejected it: the signature registry is missing in the REPL compiler, dependency builds and single-file builds, so behaviour would differ by surface. |

Option A has its own silent failures, and the ADR does not hide them. A `String`
that goes through Elixir comes back as a `Binary`, and ADR 0140 records that
`"a" = <<"a">>` is false with different hashes. So after
`names := Elixir Enum sort: names`, `names includes: "a"`, a `Set` or `Dictionary`
lookup with a `String` key, and `=` can all answer "no" without an error, and a
`Dictionary` can end up holding both `"a"` and `<<"a">>` as separate keys. A is
chosen over C because these failures are deterministic and documentable, where
C's depend on which Elixir function the data reaches. It is still the weakest
part of this ADR.

Option E, **ADR 0140's own tool**: leave the boundary as it is and rely on the
BT-4243 lint plus explicit conversion by the caller. It adds no mode, but the
Elixir cases that fail are not detectable by a lint (the call succeeds). Two
mitigations belong to A: the compiler's `ffi-nested-string` hint is extended to
mention that a nested `String` returns as `Binary` for Elixir modules, and the
deep walk must be **copy-on-change**: today's `deep_unwrap_strings/1`
(`beamtalk_primitive.erl`) rebuilds every tuple and map even when nothing
changes, so a large list passed in a loop would pay for it on every call.
Phase 1 measures the walk before it ships; no number exists yet.

Recommendation: **A**. It is the smallest change that stops silent corruption,
it is the same direction as the existing top-level unwrap, and it adds no new
representation. The remaining daily annoyance, `Binary` keys from decoded JSON,
is exactly ADR 0140's open problem and is not made worse. **B is the
maintainer's call**: it makes Jason and Req pleasant, and it is the part of
ADR 0140 the project already declined once. This ADR does not reopen it.

Rejecting C needs care, because C is what ships. The deciding evidence is that
the *successful-but-wrong* case exists (`ArgumentError.exception(message: ...)`
answers a struct with no error); a retry-based design cannot be made correct
for it.

#### 3.3 Keyword lists and structs

**Keyword lists** are lists of `{atom, value}` tuples and already work
(`Keyword.get`, `String.split` with `{#parts, 2}`, `Req.get!` with
`{#receive_timeout, 5000}`, verified). No literal syntax is added. A
Dictionary is unordered and Elixir option lists are ordered, so converting
automatically at the boundary would hide a real difference. The ADR proposes
one stdlib helper to be designed in the implementation issue: a Dictionary to
keyword-list conversion with sorted-by-key order, named after what the grep of
`stdlib/src` shows to be free (no keyword-list helper exists today).

**Structs** arrive as a `Dictionary` with a `#__struct__` key (verified: class
`Dictionary`, `at: #host` works, `Kernel.struct` builds one from Beamtalk).
Options (D5):

| Option | Verdict |
|---|---|
| Plain `Dictionary` (recommended) | Works today, no runtime change, the type checker already maps struct types to typed maps (Part 5). Downside: `class` answers `Dictionary` and `printString` is a map dump. |
| A thin `ElixirStruct` class (`Dictionary` subclass) answered by `class_of` when `__struct__` is present | Better printing and a place for `module`; costs a key check in `class_of`, which is on the hot path, and a new stdlib class. No measurement made. |

Revisit with usage. Nothing in this ADR blocks adding the class later.

#### 3.4 Exceptions

In `elixir` mode, `classify_ffi_exception` handles `error:Reason` where
`Reason` is a map with `__exception__ := true`: call
`'Elixir.Exception':message(Reason)` (guarded; on any failure fall back to
today's dump) and raise a `#beamtalk_error{}` with that message. The kind comes
from the struct module: `KeyError` to `key_error`, `ArgumentError` and
`FunctionClauseError` to `type_error`, everything else `runtime_error`. The
struct itself is kept in `details` under `elixir_exception`, and the module
under `elixir_module`, so Beamtalk code can inspect the original. `exit` and
`throw` are unchanged. The text `Exception.message/1` yields was confirmed on
`File.Error` in plain Elixir
(`could not read file "/x": no such file or directory`); the mapping itself is
a design, unimplemented.

```beamtalk
> [Elixir File read_bang: "/nonexistent"] on: Error do: [:e | e message]
"could not read file \"/nonexistent\": no such file or directory"
```

#### 3.5 Charlists

Covered by the table: the `elixir` mode neither scans results for charlists nor
retries with them. Elixir text is binaries; the rare charlist API is reached
through the explicit `Elixir List to_string:` / `Elixir String to_charlist:`
(both verified), which then returns what was asked for.

### Part 4: Scope (Q4)

**Not callable: macros.** A macro exists only at Elixir compile time. This
rules out `|>`, `Logger.info`, `use`, `is_nil`, `defstruct`, and Ecto queries.
A call to one already fails as `undef`. With the `Elixir` global the compiler
can say why: when a name is missing as a function but `macro_exported?`
reports it as a macro, the diagnostic reads

```text
error: Logger.info/1 is a macro. Macros exist only at Elixir compile time and
cannot be called from Beamtalk.
  help: Logger.bare_log/3 is a function. For logging from Beamtalk, use the
        OTP logger: Erlang logger info: ...
```

The macro check needs Elixir on the compile-time code path, which `[elixir]`
provides. Several libraries ship function twins of their macros (Explorer's
`filter_with/2`, `mutate_with/2`, `sort_with/2`, `summarise_with/2`, verified
as functions); the hint names a twin when one exists by the `_with` convention.

**Callable: protocols.** A protocol is a module of functions.
`Elixir Enumerable count: xs` and `Elixir String Chars to_string: 42` work.

**Callable, with caveats: libraries that need a callback module.** Broadway,
GenStage, Plug and Oban workers want a *module* implementing a behaviour.
Beamtalk classes are not that (actors are gen_servers, but their module is
`bt@...` with a different contract). They are out of reach until D4 changes.

**Beamtalk implementing an Elixir protocol or behaviour (D4).** Options:

| Option | Mechanism | Verdict |
|---|---|---|
| Out of scope (recommended) | A Beamtalk value is converted to a `Dictionary` or struct before it crosses (`Elixir Jason encode_bang: dict`) | Honest and small. Library authors use the conversion. |
| Generated `defimpl` for value classes | Protocol dispatch (`Protocol.impl_for`) picks an implementation from the value's `__struct__` module, or from its built-in type. Beamtalk value objects are maps tagged `'$beamtalk_class'`, which should dispatch as a plain `Map` (protocol dispatch was not run against a Beamtalk value). To get a per-class impl, value objects would need a `__struct__` key and generated modules named `<Protocol>.<Struct>`. | A representation change on a hot path plus generated modules; consolidation (Part 1.5) would have to include them or be turned off. Actors are pids: by Elixir's type-based dispatch they would fall to the `Pid` impl and could not be specialised per class (not run). Not worth deciding without a concrete need. |
| `@fallback_to_any` catch-all | One `Any` impl that asks the Beamtalk object to render itself | Only works for protocols that opt in to `Any`. |

A follow-up ADR should start from the second row if a library genuinely needs it
(Jason encoding of Beamtalk values is the likeliest first case).

### Part 4b: Target libraries and the plain-function check

The check is mechanical: for each entry module, count the documented public
functions and macros from the `Docs` chunk, the behaviour callbacks, and whether
a `use` macro exists. The script and full table are in the evidence document
(E10). All 24 libraries below resolved together (53 locked packages) and
compiled on Elixir 1.20.1.

**A. Plain-function APIs (callable as is).** Entry-module macros are absent or
are only `is_*` guards, which exist for pattern matching and have no use at a
call site.

| Library | Entry modules (functions / macros) | Notes |
|---|---|---|
| Req 0.7.5 | `Req` 23/0, `Req.Request` 30/0, `Req.Response` 10/0 | `Req.get!` answered 200 from Beamtalk (verified) |
| Finch 0.24.0 | `Finch` 16/2 | macros are guards |
| Mint 1.11.0 | `Mint.HTTP` 18/1 | guard only |
| Jason 1.4.5 | `Jason` 6/0 | `decode!` and `encode!` of an atom-keyed map verified; `encode!` of a map or list holding Strings fails today (D2); with raw binaries it was not run |
| Decimal 2.4.1 | `Decimal` 42/1 | guard only |
| NimbleOptions 1.2.0 | `NimbleOptions` 5/0 | |
| Swoosh 1.29.3 | `Swoosh.Email` 17/0, `Swoosh.Mailer` 2/0 | `Swoosh.Mailer.deliver/2` is a function; the `use Swoosh.Mailer` form is a macro and is not needed |
| Nx 0.13.1 | `Nx` 212/3 | tensor functions; the `Nx.Defn` macros (6) are out; EXLA and other backends were not run |
| Explorer 0.11.1 | `Explorer.DataFrame` 102/4, `Explorer.Series` 159/3 | the 4 macros (`filter`, `mutate`, `sort_by`, `summarise`) have `*_with` function twins (verified); the native library download was not exercised |
| Plug 1.20.3 | `Plug.Conn` 52/0 | the conn functions only |
| Phoenix.PubSub 2.4.1 | `Phoenix.PubSub` 22/0 | |
| Cachex 4.1.1 | `Cachex` 38/0 | |
| Floki 0.38.5 | `Floki` 17/1 | guard only |
| Ecto 3.13.6 | `Ecto.Changeset` 50/0, `Ecto.Multi` 21/0 | the data-structure parts only; see C |
| Telemetry.Metrics 1.2.0 | `Telemetry.Metrics` 5/0 | |
| Timex 3.7.13 | `Timex` 96/0 | |
| Earmark 1.4.49 | `Earmark` 8/0 | |
| Elixir standard library | `Enum` 89/0, `String`, `URI`, `Map`, `Keyword`, `Kernel.inspect/2` | `Kernel` itself 82/64: only its functions |

**B. Functions, but the main use needs a callback module (out of reach until
D4 changes).**

| Library | Why |
|---|---|
| Broadway 1.3.1 | `Broadway` 10/0 with 6 callbacks and `use Broadway`; you pass a module that implements `handle_message/3` |
| GenStage 1.3.2 | 19/0 functions, 12 callbacks, `use GenStage` |
| Bandit 1.14.1 | `Bandit` 1/0 (`start_link`), but it serves a Plug *module* |
| Oban 2.24.2 | 26/1, workers are `use Oban.Worker` modules |

**C. Macro-first (stay out of reach whatever the interop design).**

| Library | Evidence |
|---|---|
| Ecto queries and Repo | `Ecto.Query` 10 functions / **28 macros**; `Ecto.Repo` is `use Ecto.Repo` with 43 callbacks |
| NimbleParsec | `defparsec/3`, `defcombinator/3` and friends are macros that generate the parser module |
| Phoenix routing and endpoints | `Phoenix.Router` 4 / **20** macros; `Phoenix.Endpoint` `use` with 21 callbacks |
| Plug.Router / Plug.Builder | 1/9 and 1/2: a DSL |
| Logger | 28/10: `Logger.info`, `error`, `debug` and `log/3` are macros; `bare_log/3` is a function |

Conclusion for the issue's question: the HTTP, JSON, mail, numerics and
dataframe libraries Beamtalk users are likely to want are plain-function
APIs; the three named in the issue as macro-first (Ecto queries, NimbleParsec,
Phoenix routing) are confirmed so.

### Part 5: Types (Q5)

**Read Elixir's specs; ship no stubs yet.** Elixir beams carry no `Abst`
chunk. They carry `Dbgi` with the `elixir_erl` backend. With that backend on the
code path, `beam_lib:chunks(File, [abstract_code])` returns standard Erlang
abstract forms with `-spec` attributes (verified), and the **unchanged**
`beamtalk_spec_reader:read_specs/1` produces typed signatures from them:

| Elixir spec (module) | Beamtalk type |
|---|---|
| `String.upcase/2` | `(String \| Binary, #default \| #ascii \| #greek \| #turkic) -> String \| Binary` |
| `Req.get/2` | `(<URI map> \| String \| Binary \| List(Tuple(Symbol, Dynamic)) \| Dynamic, List(Tuple(Symbol, Dynamic))) -> Result(Dynamic, Exception)` |
| `URI.encode_query/2` | `(Dynamic, #rfc3986 \| #www_form) -> String \| Binary` |

On the sampled modules (Req 22 specs, URI 20, String 56, Enum 95) no spec was
fully `Dynamic` except 4 in `Enum`. Parameter names come out as `arg`, because
Elixir specs do not carry them.

What this needs:

1. **Elixir on the spec reader's node.** Without the backend the read fails
   with `{missing_backend, File, elixir_erl}`. The build's reader node gets the
   Elixir code path from `BeamEnvironment` (Part 1.3). The "no debug_info" info
   diagnostic (ADR 0075) gets a second wording for this case: `Elixir is not on
   the code path; types for 'Elixir.Req' unavailable`.
2. **The mode-aware passthrough rules.** BT-4485 compiles some FFI calls
   without the proxy. Third-party specs never prove a result (documented in
   `docs/beamtalk-native-erlang.md`), so Elixir calls stay on the proxy, which
   is also where the `elixir` mode lives.
3. **Stubs are an optimisation, not a prerequisite.** ADR 0075 listed
   `declare native:` files for Elixir modules as future work. Whether
   `declare native: 'Elixir.Enum'` parses (a dotted module atom) was not
   checked. Stubs would also be the place for the idiomatic aliases from
   Part 2.2 and for keyword names (the `Docs` chunk's signatures, e.g.
   `split(string, pattern \\ nil, options \\ [])`, are the source). Do this when
   there is a library whose auto-extracted types are poor.

**Releases and stripped beams.** `mix release` strips `Dbgi`, `Docs`, `ExCk`,
`CInf` and `LocT` from the beams it ships (verified on Jason). That does **not**
affect type checking, because Beamtalk reads specs from the beams it built in
the Mix build tree *before* staging them. It affects: (a) `:help` on a node
running a stripped release (the EEP-48 `Docs` chunk is gone), which is already
true for any stripped Erlang module; (b) any package that depends on a
pre-built, already-stripped Hex package, where specs fall back to `Dynamic`
with the info message above. `beamtalk release` keeps `strip-beams = false` by
default (ADR 0125 §1.2), so a Beamtalk release of Elixir code keeps its
chunks. A package that wants smaller releases can set `strip-beams = true` and
loses runtime docs for it; ADR 0125 §1.2 already refuses that setting together
with `include-compiler = true`, because a live compiler would then see
`Dynamic` for the release's own modules, and the same reasoning covers the
Elixir modules in it.

Elixir 1.18 and later also write an `ExCk` chunk of inferred types. Its format
is internal to the Elixir compiler and was not read; reading it is not
proposed.

## Prior Art

| System | How it handles Elixir (or the equivalent) | Taken, adapted, rejected |
|---|---|---|
| **Gleam** | `@external(erlang, "Elixir.CSV", "encode")` names an Elixir function by its prefixed atom; the types are declared by the author, not checked (official tour). Gleam's `gleam.toml` reference reportedly lets rebar3 and Mix packages be added as dependencies; the page fetched was a redirect, so this is secondhand. Whether Gleam shells out to Mix, and how it treats `?` and `!` names or macros: **unverified**. | Same atom convention (`'Elixir.'` prefix). Rejected: per-function declarations; Beamtalk keeps the call dynamic and types it from specs (ADR 0075). |
| **Erlang with rebar3** | The `rebar_mix` plugin (0.5.1) builds Elixir dependencies with Mix; it needs Elixir installed and offers a `{mix, consolidate_protocols}` hook that writes `_build/<profile>/consolidated`, which releases must add with `-pa` (plugin README). Alternatives `exerl` and `rebar3_elixir_plugin` exist. Which is recommended, its maintenance status and lock behaviour: **unverified** from the web; measured behaviour is in D1. | Considered seriously as option 2 of D1. Taken: the consolidated-directory-first idea. |
| **Elixir calling Erlang** | `:lists.reverse(xs)` | The mirror of ADR 0028's `Erlang` global; here the host is Beamtalk and the guest is Elixir. |
| **Kotlin (Java interop)** | Java types become "platform types" with relaxed null checks; annotations turn them into real nullable types; Java names that are Kotlin keywords are backtick-escaped (Kotlin docs). | The analogue of `Dynamic` for unspecced calls and of the `_question`/`_bang` escape for names the host syntax cannot spell. |
| **Clojure (Java interop)** | Special forms `.`, `new`, `Classname/member` reach the host; `..`, `doto`, `memfn` are macros. The page does not discuss calling host macros; its null handling is **unverified**. | Same shape as the `Elixir` global: a short syntax reaches host members. |
| **Pharo / Squeak** | Smalltalk images call C through UFFI; there is no guest *language* with its own libraries on the same VM. | No direct analogue. Beamtalk's situation is closer to Clojure on the JVM. |
| **LFE** | `(: lists reverse '(1 2 3))`; Elixir modules are reached by atom. | Same as Erlang row. |

No official page documenting the Erlang-to-Elixir module convention was found;
the `'Elixir.Enum'` prefix is confirmed by community sources and by Beamtalk's
own probes.

## User Impact

### Newcomer
`Elixir Enum sum: xs` is discoverable the way `Erlang lists` is: send a message
to `Elixir` in the REPL. The new things to learn are the `_question`/`_bang`
spelling and that macros are not callable. **Risk:** a newcomer who copies
Elixir docs hits `|>` and `Logger.info` first. Mitigation: the macro
diagnostic names the function alternative and the REPL `:help Elixir` lists the
rule.

### Smalltalk developer
Message sends all the way down: no new syntax, a class-side global like
`Erlang`. **Concern:** the Elixir function name is a keyword selector's first
word and the other keywords carry no meaning (`with:`), which is the same
compromise ADR 0028 made; stubs with real keyword names (Part 5) are the
improvement.

### Erlang/Elixir developer
Sees the dotted atom they know (`'Elixir.Enum'`) and the standard mix project
behind it; `mix.lock` fields match `beamtalk.lock`. **Concern:** structs are
maps with a key, exceptions are structs; both are visible as `Dictionary` and
a mapped error. They cannot implement an Elixir protocol with a Beamtalk class
(D4) or pass a Beamtalk class as a Plug or Broadway callback.

### Operator
A release of an Elixir-using package carries `elixir`, `logger` and `compiler`
from the build host, and records the Elixir version in provenance; the target
needs no Elixir. **Concerns:** the release grows by the Elixir applications
(size not measured); starting Elixir's `logger` application in a Beamtalk node
was run but its effect on Beamtalk's OTP logging was not examined; `+fnu` is
required in `vm.args`; a package with Elixir dependencies needs Elixir and Hex
on the build machine, and reproducing a build offline needs a Hex mirror (the
probes used one; offline use was not tested).

### Tooling developer
The LSP gets types for Elixir calls from the existing spec reader once Elixir
is on the reader's path, and a precise macro diagnostic. `declare native:` for
dotted module names, and an LSP grouping of default-argument arities, are open.

## Steelman Analysis

### `rebar_mix` (keep rebar3 as the only resolver)
| Cohort | Strongest argument |
|---|---|
| Newcomer | "One `[native.dependencies]` table, whatever the language. I do not want to learn which table a package belongs in." |
| Smalltalk purist | "Fewer concepts in the manifest." |
| BEAM veteran | "rebar3 plus `rebar_mix` is what Erlang teams already run; Mix is a guest in someone else's build. It is a one-line change and it works today, as the Jason and Req builds showed." |
| Operator | "The build tool I already pin and cache is rebar3." |
| Language designer | "Smallest diff, keeps `[native.overrides]`." |

The argument is strong for small cases and the recommendation gives ground to
it: `rebar_mix` is the fallback if the maintainer prefers minimal code. It is
not chosen because its closure is not the locked closure (six unlocked
packages in the Req build), it took minutes against seconds on the same top-level dependency (one run each, cold; not a like-for-like comparison, see 1.1), and its failure
without Elixir is unreadable.

### Wrap the libraries in the Beamtalk stdlib instead
(The existing approach for HTTP and HPACK.) *Newcomer:* "A `Json` class that
takes Strings and answers Strings never shows me `Binary`." *Smalltalk
purist:* "Idiomatic keyword selectors, not `with:`." *Operator:* "One audited
dependency." *Language designer:* "No boundary rules." It is the best
experience per library and the worst coverage: the stdlib cannot wrap what the
ecosystem ships. It stays available on top of this ADR, which makes wrapping
cheaper (a wrapper class calls `Elixir Jason decode_bang:`).

### Keep the shallow String unwrap (D2 option C)
*Erlang veteran:* "It preserves `String` identity through containers and costs
nothing." *Operator:* "No per-call walk." *Language designer:* "No new mode."
Against it: a call can succeed with corrupted data, and the cases that fail
(`Protocol.UndefinedError`) are not retried. The identity loss in A is real:
`Enum.sort(["b","a"])` returns `Binary` elements. Accepted as the price of
correctness; B recovers it for results at a bigger price.

### Per-function `@external` declarations (Gleam)
*Type-safety advocate:* "Declared types, checked arity, no runtime surprises."
Beamtalk already reads specs and checks arity at compile time through the
FFI table (ADR 0075); declarations would add the REPL friction ADR 0028
rejected.

### Tensions
- Newcomers and the Smalltalk cohort want readable names (`isValid:`) while the
  BEAM cohort wants a mechanical mapping they can predict: the suffix rule is
  the predictable base, stubs the readable layer.
- Operators want one lockfile; the Erlang cohort wants one build tool. Both
  cannot be fully met when a package mixes Erlang and Elixir; this ADR picks
  one lockfile and one Mix graph.
- The String decision (D2) has no option every cohort accepts. It is the main
  open question.

## Alternatives Considered

### `rebar_mix` plugin
Described and measured in Part 1.1. Rejected as the default, kept as a viable
alternative for D1.

### Two resolvers, merged by a check
Run rebar3 for Erlang packages and Mix for Elixir packages and reject
conflicting versions. Rejected: two lock states, two failure modes, and still no
answer for a package both want.

### Do nothing
Users continue with `ERL_LIBS` and `erlang:apply/3`. Works for `Enum.sum`.
Fails, silently in the worst case, for any call with a nested String, any
exception, and any package. Rejected: the boundary rules are wrong for Elixir
whether or not packages are supported.

### A new `ElixirModule` proxy class
Instead of reusing `ErlangModule`. Rejected: the proxy differs only in
that capitalised-selector rule and the coercion mode, both functions of the module atom.

## Consequences

### Positive
- Elixir libraries (HTTP, JSON, mail, numerics, dataframes) become usable from
  a package manifest, not from a hand-set `ERL_LIBS`.
- One lockfile names everything that gets built.
- Silent corruption of strings in arguments stops.
- Elixir exceptions read like Elixir's own messages.
- Types for Elixir calls come from the spec reader that already exists.
- No parser change.

### Negative
- **Two toolchains in one build.** Packages with `[elixir.dependencies]` need
  Elixir and Hex on the build machine, and Mix resolves Erlang Hex packages
  for those packages too.
- **`[native.overrides]` is unavailable** for such packages (a rebar3
  `erl_opts` replacement has no Mix equivalent in a generated `mix.exs`). A
  package that needs both must keep the Erlang dependency on a path.
- **Strings lose identity through Elixir containers** under D2-A, and decoded
  data still has `Binary` keys.
- **A third coercion mode** to keep in step across Rust and Erlang, mitigated by
  the shared fixture.
- Elixir's `logger` application and the `elixir` application run in the node:
  memory, boot time and logging interaction unmeasured.
- The macro/callback boundary excludes some popular libraries (Ecto,
  Phoenix, Broadway, Plug apps), and that will be a recurring question.
- Verbose suffix names (`read_bang:`).

### Neutral
- Consolidation is on by default and worth about 7% in one micro-benchmark.
- `Dynamic` remains where an Elixir spec is `term()`.
- `:help` for Elixir modules works from the `Docs` chunk where it exists.

## Implementation

Phases are in dependency order. Phase 0 is a napkin check.

**Staging and the cheaper path.** Phases 0 and 1 (the `Elixir` global and the
`elixir` mode) deliver most of the day-to-day value on their own and need no
build change: users supply Elixir packages as they do now, by `ERL_LIBS` or a
`path` dependency. For packages, the one-line `rebar_mix` route (D1's second
option) is a working interim. Phase 2 (Mix direct) should therefore start with
a spike that proves the lock round trip and the "adding `jason` keeps native
pins" test, and the maintainer can stop after Phase 1 or take `rebar_mix` if the
spike does not pay for itself.

| Phase | Work | Components | Tests | Size |
|---|---|---|---|---|
| 0. Wire check | `Elixir Enum sum:` against a hand-set `ERL_LIBS`, no new mode: prove the global, the dotted chain and the `_bang` mapping on `Enum`, `String`, `URI`. BT-4866 has landed (the `function_clause` retry and report are in `beamtalk_erlang_proxy.erl`). | `stdlib/src/elixir.bt`, `beamtalk-core` parser tests, codegen `Erlang`-chain recogniser, `selector_to_function/1` | one stdlib BUnit file, one REPL protocol case | S |
| 1. `elixir` mode | `is_elixir_erlang_module`, `coercion_mode/1`, deep-unwrap, no charlist rules, exception mapping, fixture `ffi_coercion_mode_conformance.json` | `beamtalk-core/src/ast`, `beamtalk-codegen/.../util.rs`, `beamtalk_erlang_proxy.erl` | EUnit, conformance fixture, a corpus for deep-unwrap, `ci:corpus` not needed (no ThreadedIr change) | M |
| 2. Mix-backed packages (begin with the lock-round-trip spike) | `[elixir]` manifest, `mix.exs` generator, lock round trip, `BeamEnvironment` Elixir step, `+fnu`, `doctor` check, `[elixir] version` check, `otp-support.toml` minimum | `beamtalk-package/src/manifest/`, `beamtalk-cli/src/commands/build/`, `beam_environment.rs`, `deps/lockfile.rs` | CLI tests with a local Hex repo (the existing test hook), lock round trip | L |
| 3. Releases | Elixir apps in the closure, `compiler` app, `$RELDIR` rule, provenance and preflight row | `commands/release/closure.rs`, `assembly.rs`, ADR 0125 docs | release boot smoke test with `jason` | M |
| 4. Types and diagnostics | Elixir-not-on-path info, macro diagnostic, `_with` hint, spec-reader check on Elixir beams | `beamtalk_spec_reader.erl` wording, `native_types`, type checker | spec-reader EUnit on a fixture Elixir beam | M |
| 5. Optional | keyword-list helper, `ElixirStruct`, stubs for dotted modules, LSP grouping of default arities | decided after use | | per item |

Surface parity (`docs/development/surface-parity.md`): `Elixir` works in the
REPL, `run`, `test` and releases identically because the recogniser is
compile-time and the code path comes from `BeamEnvironment`. The REPL
compiler path has no `BeamEnvironment` of its own today; Phase 2 must show it
picks up the Elixir paths, or `:help` and type information will differ by
surface. This needs checking, not assuming.

## Migration Path

No existing code changes behaviour. `Erlang erlang apply: #'Elixir.X' ...`
keeps its stock-mode rules. Code that wants the Elixir rules moves to
`Elixir X ...`. The only visible difference is for a call that already goes
through the `Elixir` global, which does not exist yet.

## References

- Related issues: BT-4870 (this ADR), BT-4866 (function_clause report and
  retry, landed), BT-4485 (FFI calls without the proxy), BT-4153 (coercion mode)
- Related ADRs: ADR 0028 (BEAM interop; `Elixir` global as future work),
  ADR 0075 (FFI types; Elixir stubs as future work), ADR 0076 (`Result`
  coercion), ADR 0101 (unified native objects), ADR 0125 (releases), ADR 0140
  (rejected String flip; the open problem D2 touches), ADR 0149 (EEP-48 chunks,
  also checked against Elixir 1.20.1), ADR 0072 (rebar3 native dependencies)
- Evidence: [0151-elixir-interop-evidence.md](0151-elixir-interop-evidence.md)
- Code: `crates/beamtalk-cli/src/commands/build/native.rs` (`generate_rebar_config`,
  `compile_with_rebar3`), `crates/beamtalk-cli/src/commands/beam_environment.rs`,
  `crates/beamtalk-cli/src/commands/release/closure.rs`,
  `crates/beamtalk-codegen/src/core_erlang/util.rs` (`ffi_coercion_mode`),
  `runtime/apps/beamtalk_runtime/src/beamtalk_erlang_proxy.erl`,
  `runtime/apps/beamtalk_compiler/src/beamtalk_spec_reader.erl`
- External: Elixir compatibility table
  (https://elixir.hexdocs.pm/compatibility-and-deprecations.html),
  `rebar_mix` (https://readme.hex.pm/rebar_mix/0.5.1), Gleam externals tour
  (https://tour.gleam.run/advanced-features/externals/), Kotlin Java interop
  (https://kotlinlang.org/docs/java-interop.html), Clojure Java interop
  (https://clojure.org/reference/java_interop)
