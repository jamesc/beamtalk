# EEP-48 doc chunk spike (BT-4852)

**Prototype code. Nothing here is on a production code path, and nothing in
the build references it.** It backs the measurements in
[ADR 0149](../../docs/ADR/0149-eep48-doc-chunks-via-compiler-extra-chunks.md).

## Run it

```bash
# OTP on PATH, plus optionally a second OTP and an installed runtime
ERL_BIN=/path/to/otp-29/bin BEAMTALK_LIB=/usr/local/lib/beamtalk/lib ./run.sh
```

Output goes to `./_out` (git-ignored). The Elixir and ExDoc steps run only when
`elixir` is on `PATH`; the ExDoc step fetches `ex_doc` from Hex into
`exdoc/deps`.

## What is here

| File | What it tests |
|---|---|
| `e1_core_attributes.escript` | Do `-doc`/`-moduledoc` attributes written in Core Erlang produce a `Docs` chunk? (No.) |
| `eep48_proto.erl` | Builds `docs_v1` from the doc maps already in a module's Core Erlang and passes it to `compile:forms/2` as `{extra_chunks, ...}` |
| `e3_extra_chunks.escript` | Compiles real Beamtalk Core Erlang that way; checks the chunk, the size and that the output is deterministic |
| `e5_memory_loaded.escript` | `code:get_doc/1` for a module loaded with `code:load_binary/3` (a REPL class), four load-path variants |
| `eep48_runtime_proto.erl` | Builds `docs_v1` from live `class_state` and writes `doc/chunks/<mod>.chunk` side files for the whole stdlib |
| `e6_mapping_stats.escript` | How selector entries line up with real exports; the cost of flattening inherited methods |
| `e7_compile_cost.escript` | Compile time with and without the chunk |
| `e9_docs_carrier.escript` | Can codegen hand the finished `docs_v1` term to the build worker inside the Core module? (Not as an attribute; yes as a constant function the worker lifts out.) |
| `e8_iex.exs` | `Code.fetch_docs/1` and IEx `h` |
| `e8_exdoc_fake_abstract_code.escript`, `exdoc/` | ExDoc over the prototype `.beam` files, with and without a synthetic Erlang abstract-code chunk |
| `fixture/` | Two small classes (a `Value` and an `Actor`) and the Erlang/Core baseline for E1 |

`eep48_proto.erl` finds the doc maps by scanning `register_class/0`, and
`eep48_runtime_proto.erl` reads the private `#class_state{}` record by field
position. Both are shortcuts for measuring. The ADR says what a real
implementation should do instead.

## Not tested

ELP and `erlang_ls` hover. The ELP release binary could not be downloaded in
the environment the spike ran in.
