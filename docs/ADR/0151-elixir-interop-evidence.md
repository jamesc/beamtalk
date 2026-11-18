# ADR 0151 evidence: what was run, on which versions

Supporting record for [ADR 0151](0151-elixir-interop-and-mix-backed-packages.md).
It lists every experiment behind a factual claim in the ADR, the result, and
what was **not** verified. Same role as
[`0075-gate-evaluation.md`](0075-gate-evaluation.md) for ADR 0075.

Nothing here is a test in the repo. These are one-off probes run in a scratch
directory on 2026-11-16. Timings are single runs on a shared cloud container,
so read them as orders of magnitude, not benchmarks.

## Environment

| Component | Version |
|---|---|
| Beamtalk | `beamtalk 0.5.0-edge+02378f0 (2026-11-16)` (installed edge build) |
| Erlang/OTP | 28.5 (ERTS 16.4) |
| Elixir | 1.20.1, compiled with OTP 28 |
| Mix | 1.20.1 |
| Hex (Mix archive) | 2.7.0 (installed with `mix local.hex --force`) |
| rebar3 | 3.27.0 (system) and the repo's `runtime/tools/rebar3`, also 3.27.0 |
| `rebar_mix` plugin | 0.5.1, fetched by rebar3 from Hex |
| OS | Linux 6.18 (cloud container) |
| Hex access | through the container's Hex mirror (`HEX_MIRROR`); `repo.hex.pm` answered 200 |

Only **one Elixir version and one OTP version** were available. Everything
about running several Elixir versions is unverified (see the last section).

The probes used a project made with `beamtalk new probe --app`, with
`ERL_LIBS` pointing at the Elixir `lib/` directory (and at the Mix build
output where a library was involved). `beamtalk run Main run` ran each probe.
Probe code called Elixir through `Erlang erlang apply:with:with:`, because the
`Elixir` global does not exist yet.

## E1. Baseline: what works today

| # | Probe | Result |
|---|---|---|
| E1a | `Erlang erlang apply: #'Elixir.Enum' with: #sum with: #(#(1, 2, 3))`, no Elixir on the code path | `Undefined function: Elixir.Enum:sum/1` (a clean `does_not_understand`) |
| E1b | Same, with `ERL_LIBS=<elixir>/lib` | `6` |
| E1c | `Enum.map` with block `[:x \| x * 10]` | `#(10, 20, 30)` |
| E1d | `String.upcase("hello")`, `String.valid?`, `String.split/1,2,3` (with `{#parts, 2}`), `Enum.join/2` | all answered as expected |
| E1e | `Map.get(map, :b)` where the key is absent | `nil` |
| E1f | `Erlang perform: #'Elixir.Enum'` | `Erlang does not understand 'Elixir.Enum'` |
| E1g | `Erlang Enum sum: #(1, 2)` (uppercase unary after `Erlang`), `beamtalk check` | parses; reports `Unknown Erlang module 'Enum' in FFI call`, so the parser already accepts `X Enum` as a unary send |

`:code.lib_dir(:elixir)` is not resolvable without Elixir on the path
(`{error, bad_name}`). Locating it with `elixir -e 'IO.puts(:code.lib_dir(:elixir))'`
takes about 0.35 s per call here (three runs: 0.34 to 0.39 s; a bare
`erl -noshell` takes 0.25 to 0.27 s).

## E2. Value boundary probes

Run through `erlang:apply/3`, so every argument was **inside a list**.

| Probe | Result | Reading |
|---|---|---|
| `Enum.join(["a","b"])` (Strings) | `Protocol.UndefinedError` for `String.Chars` on `{beamtalk_string, <<"a">>}`, reported as `runtime_error` | Nested String reaches Elixir tagged. The BT-4866 retry does not fire: this is not `function_clause`. |
| `Jason.encode!(%{"a" => "b"})` | `Protocol.UndefinedError` for `String.Chars` on the tagged String | Same cause |
| `Jason.encode!(["x","y"])` | `Protocol.UndefinedError` for `Jason.Encoder` on the tuple | Same cause |
| `Exception.message(ArgumentError.exception(message: "boom"))` | the message is the printed tagged tuple, with **no error** | **Silent corruption**: the struct was built with a tagged String inside it |
| `Req.get!("http://127.0.0.1:8765/")` | `ArgumentError: expected 1st argument to be a url ... got: {:beamtalk_string, ...}` | Through `apply:` even the URL is nested in the argument list, so the top-level unwrap never runs |
| Same, URL made raw with `Erlang unicode characters_to_binary:` | `200` | The rest of the stack works |
| `Jason.decode!("{\"a\":1}") at: "a"` | `key_error: key not found: {beamtalk_string, <<"a">>}` | Decoded keys are raw binaries (class `Binary`); a Beamtalk String key is tagged |
| class of `String.upcase("hello")` result | `Binary` | Elixir text comes back as `Binary`, as for any Erlang library |
| `Integer.parse("42abc")` / `Integer.parse("abc")` | `{42, "abc"}` / `Result error: nil` | bare `:error` is coerced to `Result error: nil` |
| `File.read("/nonexistent")` | `Result error: enoent` | ADR 0076 coercion works on Elixir returns |
| `Enumerable.count(#(1, 2))`, `String.Chars.to_string(42)`, `Kernel.inspect(%{a: 1})` | `Result ok: 2`, `"42"`, `"%{a: 1}"` | Protocol functions are callable; `{:ok, n}` becomes a `Result` |
| `URI.parse(...)` | `#{#__struct__ => #Elixir.URI, #host => "x.com", ...}`, class `Dictionary`, `at: #host` works | Structs arrive as plain maps |
| `Kernel.struct(URI, [host: "x.com"])` | a `URI` struct map | Structs can be built from Beamtalk with a plain function |
| `File.read!("/nonexistent")` | `runtime_error`, message = the raw printed struct `#{reason => enoent, path => ..., '__struct__' => 'Elixir.File.Error', ...}` | Exceptions reach Beamtalk as `error:Struct` and are not mapped |
| `Enum.fetch!([1], 5)` | `runtime_error`, message = raw printed `Enum.OutOfBoundsError` struct | same |
| `Map.fetch!(%{a: 1}, :zz)` | `key_error: ErlangModule>>apply: key not found: zz` | One struct happens to be classified by the existing generic wrapper |
| `Exception.message(%File.Error{reason: :enoent, action: "read file", path: "/x"})` run in plain Elixir | `could not read file "/x": no such file or directory` | The text a mapping would use is available |
| `Logger.info("hi")` | `Undefined function: Elixir.Logger:info/1` | A macro is a clean `undef`; the message says nothing about macros |
| `Kernel.is_nil(nil)` | `Undefined function: Elixir.Kernel:is_nil/1` | same |
| `String.to_charlist("hi")` | `"hi"` | A charlist result is coerced to a String |
| `Enum.map([1,2,3], fn x -> x + 64 end)` | `"ABC"`, class `String` | **Hazard**: stock-mode result coercion turns a printable integer list into a String. `Erlang lists seq: 65 with: 70` answers `"ABCDEF"` today, so this is existing behaviour that Elixir code would hit far more often. |

## E3. Req end to end

Built with Mix (E6), `ERL_LIBS=<elixir>/lib:<mix _build>/prod/lib`,
`ELIXIR_ERL_OPTIONS=+fnu`, a local `python3 -m http.server` on 127.0.0.1:

- `Erlang application ensure_all_started: #req` answered
  `Result ok: #(#elixir, #logger, #hpax, #mint, #nimble_pool, #nimble_options, #mime, #finch, #jason, #req)`.
  Elixir's own `elixir` and `logger` applications start as ordinary
  dependencies of the library.
- `Req.get!(url)`, `Req.get!(url, receive_timeout: 5000)` and
  `Req.get!(url, headers: [{"x-test", "1"}])` each answered a struct whose
  `status` is `200`, once the strings were raw binaries.
- `Req.get(url)` answered a `Result` that `isOk`.
- Without `+fnu` the VM prints `warning: the VM is running with native name
  encoding of latin1 which may cause Elixir to malfunction`.

## E4. Today's rebar3 path

`[native.dependencies] jason = "~> 1.4"` in `beamtalk.toml`, `beamtalk build`:

```text
===> Fetching jason v1.4.5
===> Compiling jason
===> Error building application jason:
     No project builder is configured for type mix
```

## E5. `rebar_mix`

A rebar3 project with `{plugins, [rebar_mix]}`:

| Project | Result | Time (one run) |
|---|---|---|
| `{deps, [{jason, "~> 1.4"}]}` | built; `_b/default/lib/jason/ebin` has 29 beams; `decimal` also present | 12 s |
| `{deps, [{req, "~> 0.5"}]}` | built; 16 directories in `lib/` | 1 m 56 s |
| `jason` with Elixir absent | `error: missing_elixir`, shown only as `Failed creating providers`; the details are in `rebar3.crashdump` | n/a |

For the Req build, `rebar.lock` listed 9 packages (finch, hpax, jason, mime,
mint, nimble_options, nimble_pool, req, telemetry). The `lib/` directory also
held `brotli`, `castore`, `decimal`, `nimble_csv`, `plug`, `plug_crypto`: six
packages **not in `rebar.lock`**. They came from `mix deps.get` run by the
plugin inside each dependency's own directory. In the Jason build that run
also fetched Jason's development dependencies (`ex_doc`, `dialyxir`,
`stream_data`, and others) into `jason/deps`. Reading the plugin source
(`rebar_mix_utils.erl`): it finds Elixir by running `elixir -e` on `PATH`, and
runs `mix deps.get` then `mix compile` with `MIX_ENV=prod` in each Mix
dependency.

The "Elixir absent" run needed the real Erlang install and the repo's bundled
rebar3 under `env -i`, because the container's mise `erl` shim prepends Elixir
to `PATH`. An earlier attempt with an edited `PATH` was invalid for that
reason and is not counted.

## E6. Mix driven directly

A generated-style `mix.exs` (`deps: [{:jason, "~> 1.4"}, {:req, "~> 0.5"}]`,
`MIX_ENV=prod`):

| Step | Result | Time |
|---|---|---|
| `mix deps.get` | 9 packages resolved and fetched: finch, hpax, jason, mime, mint, nimble_options, nimble_pool, req, telemetry | 5.5 s |
| `mix deps.compile` | all compiled to `_build/prod/lib/<app>/ebin`; `telemetry` (a rebar3 package) built too | 8.3 s |
| `mix compile` | produces `_build/prod/lib/<project>/consolidated/` with 8 protocol beams (Collectable, Enumerable, IEx.Info, Inspect, JSON.Encoder, Jason.Encoder, List.Chars, String.Chars) | n/a |

`mix.lock` records, per package, version, **inner and outer checksum**, build
tool (`[:mix]` or `[:rebar3]`) and requirements, for the whole transitive
closure. `beamtalk.lock` already records version, checksum and outer checksum
for native packages.

Not checked: which rebar3 Mix used to build `telemetry`.

A 24-library `mix.exs` (the audit set below) resolved to 53 locked packages
in one run: `mix deps.get`, 20 s. It also included the Erlang packages cowboy,
ranch and telemetry.

## E7. Protocol consolidation

`Jason.encode!` of a 200-key map x 3000 and `Enum.map/sum` over 50,000
elements x 200, with and without the `consolidated` directory on the path:

| | Jason encode | Enum map/sum |
|---|---|---|
| unconsolidated | 400 ms | 127 ms |
| consolidated | 370 ms | 117 ms |

One run each, plain `elixir`, not through Beamtalk. Unconsolidated dispatch
is functionally correct (every earlier probe ran unconsolidated). The
difference is about 7% here.

## E8. Releases (ADR 0125)

Hand-built `.rel` with `kernel`, `stdlib`, `sasl`, `compiler`, `elixir`,
`logger`, `jason`, using the staging rule from ADR 0125 §1.3:

- `elixir.app` declares `{applications, [kernel, stdlib, compiler]}`. With
  `compiler` missing from the `.rel`, `systools:make_script` fails with
  `{undefined_applications, [compiler]}`. `logger` declares `elixir`. Jason's
  `.app` declares `elixir` and lists `decimal` under `optional_applications`.
- With a relative `{path, ["lib/*/ebin"]}`, `make_script` succeeds but the
  node dies: `application: elixir exited: {bad_return, {{elixir,start,...},
  {'EXIT',{undef,[{elixir,start,[normal,[]],[]}...`. This is the `$ROOT` trap
  ADR 0125 §1.3 documents. With an absolute path plus
  `{variables, [{"RELDIR", Dir}]}` and `-boot_var RELDIR`, the node boots and
  `Elixir.Enum:sum([1,2,3])` answers 6. Running applications:
  `[jason, logger, elixir, compiler, sasl, stdlib, kernel]`.
- `mix release` (default `strip_beams`) drops chunks from the beams. The
  unstripped `Elixir.Jason.beam` has `AtU8 Code StrT ImpT ExpT LitT LocT Attr
  CInf Dbgi Docs ExCk Line Type`. The release copy has `Attr Line Type AtU8
  Code StrT ImpT ExpT LitT`, so **`Dbgi`, `Docs`, `ExCk`, `CInf` and `LocT`
  are gone**.

## E9. Specs

- Elixir beams have no `Abst` chunk. They have `Dbgi` whose backend is
  `elixir_erl`, `Docs` and `ExCk`.
- With `elixir_erl` on the code path, `beam_lib:chunks(File, [abstract_code])`
  answers `{raw_abstract_v1, Forms}` containing standard `{attribute, _, spec, _}`
  forms (43 forms, 7 specs for `Elixir.Jason`). Without it:
  `{error, beam_lib, {missing_backend, File, elixir_erl}}`.
- `beamtalk_spec_reader:read_specs/1` (unchanged, from the installed
  `beamtalk_compiler` app) on Elixir beams, Elixir on the path:

| Module | Specs read | Fully `Dynamic` (all params and return) |
|---|---|---|
| `Elixir.Jason` | 7 | not counted |
| `Elixir.Req` | 22 | 0 |
| `Elixir.URI` | 20 | 0 |
| `Elixir.String` | 56 | 0 |
| `Elixir.Enum` | 95 | 4 |

  Sample mappings: `Req.get/2` takes a URI-struct map, `String`, `Binary`,
  `List(Tuple(Symbol, Dynamic))` or `Dynamic`, then a keyword list, and
  answers `Result(Dynamic, Exception)`; `String.upcase/2` is
  `(String | Binary, #default | #ascii | #greek | #turkic) -> String | Binary`;
  `Enum.map/2` is `(Dynamic, Block) -> List`; `URI.encode_query/2` is
  `(Dynamic, #rfc3986 | #www_form) -> String | Binary`. Parameter names come
  out as `arg`, because Elixir specs carry no parameter names.

## E10. Macro and function audit

24 libraries from one `mix.exs` (`MIX_ENV=prod`, all compiled; exit 0):
req 0.7.5, finch 0.24.0, mint 1.11.0, jason 1.4.5, decimal 2.4.1,
nimble_options 1.2.0, swoosh 1.29.3, broadway 1.3.1, nx 0.13.1, explorer
0.11.1, plug 1.20.3, bandit 1.14.1, phoenix_pubsub 2.4.1, oban 2.24.2, cachex
4.1.1, floki 0.38.5, ecto 3.13.6, nimble_parsec 1.4.2, phoenix 1.8.16,
telemetry_metrics 1.2.0, gen_stage 1.3.2, timex 3.7.13, earmark 1.4.49,
cowboy 2.21.0. (Elixir 1.20's compiler printed type warnings for several of
these while compiling them; they did not stop the build.)

For each entry module the script counts **documented, non-hidden** public
functions and macros from the `Docs` chunk, the behaviour callbacks, and
whether `use Module` exists. The script:

```elixir
public = fn mod ->
  case Code.fetch_docs(mod) do
    {:docs_v1, _, _, _, moduledoc, _, docs} ->
      if moduledoc == :hidden do
        {:hidden, [], []}
      else
        f = for {{:function, n, a}, _, _, d, _} <- docs, d != :hidden, do: "#{n}/#{a}"
        m = for {{:macro, n, a}, _, _, d, _} <- docs, d != :hidden, do: "#{n}/#{a}"
        {:ok, f, m}
      end
    _ -> {:nodocs, [], []}
  end
end
# per entry module: length(f), length(m), length(mod.behaviour_info(:callbacks)),
# and macros(mod) has :__using__ (hidden or not): `Keyword.has_key?(mod.__info__(:macros), :__using__)`
```

| Library | Entry module | Public functions | Public macros | Callbacks | `use` macro | Macros (sample) |
|---|---|---|---|---|---|---|
| req | Req | 23 | 0 | 0 | no | |
| req | Req.Request | 30 | 0 | 0 | no | |
| req | Req.Response | 10 | 0 | 0 | no | |
| finch | Finch | 16 | 2 | 0 | no | `is_finch_error/1`, `is_request_ref/1` (guards) |
| mint | Mint.HTTP | 18 | 1 | 0 | no | `is_connection_message/2` (guard) |
| jason | Jason | 6 | 0 | 0 | no | |
| decimal | Decimal | 42 | 1 | 0 | no | `is_decimal/1` (guard) |
| nimble_options | NimbleOptions | 5 | 0 | 0 | no | |
| swoosh | Swoosh.Email | 17 | 0 | 0 | no | |
| swoosh | Swoosh.Mailer | 2 | 0 | 0 | **yes** | `deliver/2` is a function |
| broadway | Broadway | 10 | 0 | 6 | **yes** | |
| nx | Nx | 212 | 3 | 0 | no | `is_tensor/1`, `sigil_MAT/2`, `sigil_VEC/2` |
| nx | Nx.Defn | 15 | 6 | 0 | no | `defn/2`, `defnp/2`, `deftransform/1,2`, ... |
| explorer | Explorer.DataFrame | 102 | 4 | 0 | no | `filter/2`, `mutate/3`, `sort_by/3`, `summarise/2` |
| explorer | Explorer.Series | 159 | 3 | 0 | no | `filter/2`, `map/2`, `sort_by/3` |
| plug | Plug.Conn | 52 | 0 | 0 | no | |
| plug | Plug.Builder | 1 | 2 | 0 | **yes** | `builder_opts/0`, `plug/2` |
| plug | Plug.Router | 1 | 9 | 0 | **yes** | `get/3`, `post`, `match/3`, `forward/2`, ... |
| bandit | Bandit | 1 | 0 | 0 | no | |
| phoenix_pubsub | Phoenix.PubSub | 22 | 0 | 0 | no | |
| oban | Oban | 26 | 1 | 0 | **yes** | |
| cachex | Cachex | 38 | 0 | 0 | no | |
| floki | Floki | 17 | 1 | 0 | no | `is_html_node/1` (guard) |
| ecto | Ecto.Query | 10 | 28 | 0 | no | `from/2`, `distinct/3`, `dynamic/2`, ... |
| ecto | Ecto.Changeset | 50 | 0 | 0 | no | |
| ecto | Ecto.Repo | 1 | 0 | 43 | **yes** | |
| ecto | Ecto.Multi | 21 | 0 | 0 | no | |
| nimble_parsec | NimbleParsec | 37 | 4 | 0 | no | `defparsec/3`, `defcombinator/3`, ... |
| phoenix | Phoenix.Router | 4 | 20 | 0 | **yes** | `get/4`, `forward/4`, ... |
| phoenix | Phoenix.Endpoint | 1 | 1 | 21 | **yes** | |
| phoenix | Phoenix.Controller | 43 | 1 | 0 | **yes** | |
| telemetry_metrics | Telemetry.Metrics | 5 | 0 | 0 | no | |
| gen_stage | GenStage | 19 | 0 | 12 | **yes** | |
| timex | Timex | 96 | 0 | 0 | no | |
| earmark | Earmark | 8 | 0 | 0 | no | |
| (Elixir) | Logger | 28 | 10 | 0 | no | `info/2`, `error/2`, `debug/2`, ... |
| (Elixir) | Kernel | 82 | 64 | 0 | no | `is_nil/1`, `&&/2`, ... |
| (Elixir) | Enum | 89 | 0 | 0 | no | |

Follow-ups, same session:

- `macro_exported?` / `function_exported?`: `Explorer.DataFrame.filter_with/2`,
  `mutate_with/2`, `sort_with/2`, `summarise_with/2` are **functions**;
  `Swoosh.Mailer.deliver/2` is a function; `Logger.bare_log/3` is a
  function while `Logger.log/3` and `Logger.info/1` are macros; `Kernel.inspect/2` is a function while `Kernel.is_nil/1` is a macro;
  `Ecto.Query.from/2` is a macro; `Plug.Conn.put_resp_header/3` and
  `Ecto.Changeset.cast/4` are functions.
- Modules exporting any macro, per library: req `[Req.Utils]`; finch `[Finch,
  Finch.Pool.Manager]`; jason `[Jason.Codegen, Jason.Decoder.Unescape,
  Jason.Encoder.Any, Jason.Helpers, Jason.Sigil]`; mint `[Mint.HTTP,
  Mint.HTTP1.Parse, Mint.HTTP2.Frame]`; decimal `[Decimal, Decimal.Macros]`;
  nimble_options, telemetry_metrics, earmark, phoenix_pubsub: none; floki
  `[Floki]`; timex `[Timex.Macros, Timex.Protocol.Map, Timex.Translator]`;
  cachex `[Cachex.Error, Cachex.Router, Cachex.Spec]`. Most are internal
  modules, not the documented entry points above.

## E11. Function-name collisions for a suffix scheme

Over the 23 listed libraries (all of E10 except cowboy, an Erlang package) plus
the Elixir standard library (`elixir` application), 1,308 modules and 3,847
distinct exported function names:

| Name shape | Count |
|---|---|
| ends in `?` | 162 |
| ends in `!` | 254 |
| ends in `_bang` | 0 |
| ends in `_question` | 0 |
| ends in `_p` | 1 (`mkdir_p`) |
| starts with an uppercase letter | 0 |
| outside `[a-z_][a-zA-Z0-9_]*[?!]?` | `&&&`, `<<<`, `>>>`, `^^^`, `\|\|\|`, `~~~`, `!=`, `!==` (operators) |

## Not run, or not verified

1. **A second Elixir version.** A prebuilt Elixir 1.18.4 download and run was
   **blocked by the session's permission check** and was not retried by any
   other route. So "beams compiled by Elixir X load on Elixir Y" is **not
   tested**. The OTP-range table in the ADR comes from the official
   compatibility page (fetched 2026-11-16), not from a run.
2. **A second OTP version.** Everything ran on OTP 28.
3. **Windows and macOS.** Linux only. ADR 0027 puts Windows in tier 1; the
   `elixir` and `mix` launchers on Windows are a `.bat`, which was not looked at.
4. **The `Elixir` global, the `elixir` coercion mode, name mapping, error
   mapping and the Mix build step are designs.** No compiler or runtime code was
   changed; the probes use only what exists today.
5. **Rebuilding `mix.lock` from `beamtalk.lock`** (the lock round trip) was not
   tried.
6. **Which rebar3 Mix uses** for Erlang packages was not checked.
7. **Effect of starting Elixir's `logger` application** on Beamtalk's OTP
   logging, and the memory and boot-time cost of loading `elixir`, were not
   measured.
8. **Explorer's NIF, Nx backends, Swoosh delivery and Broadway** were not run;
   only their API shape was audited.
9. **`+fnu`**: shown to silence the Elixir warning when passed through
   `ELIXIR_ERL_OPTIONS`; its effect on Beamtalk's own file-name handling was not
   checked.
10. **Offline and private Hex use**: the probes used the container's Hex
    mirror. Behaviour with `HEX_MIRROR` unset, private repos, or no network
    was not tested.
