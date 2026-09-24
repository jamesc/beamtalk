%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_release).

%%% **DDD Context:** Runtime Context

-moduledoc """
The two parity-neutral reflective sends ADR 0125 §1.8/§3.4 defines so an
operator or a deploy tool can ask a live node what it is: `info/0`
(`Beamtalk releaseInfo`) and `shape_manifest/0` (`Beamtalk shapeManifest`).
Both are reachable through `eval` (the instance-side `BeamtalkInterface`
methods delegate to the class-side ones here) **and** through `run-entry`
with no compiler (`{class: "BeamtalkInterface", selector: "releaseInfo" |
"shapeManifest"}`) — see `stdlib/src/beamtalk_interface.bt`'s `class`
methods, which are this module's only callers.

## `info/0` — `releases/<vsn>/beamtalk-provenance.json`, from the running node

A release boots via `-boot <dir>/start -boot_var RELEASE_DIR <dir>`
(`crates/beamtalk-cli/src/commands/release/templates/launcher.sh`), so
`init:get_argument(boot_var)` — not `code:root_dir/0`, which is the
*host's* OTP root under `--no-include-erts` (ADR 0125 §1.3's `$ROOT` trap) —
is the one value this module can trust to find the release tree back. The
release version comes from `init:script_id/0`, the `{Name, Vsn}` systools
wrote into the very boot script that got this node running. Neither is set
on a `run`/`workspace` node (no `-boot_var`, and `init:script_id/0` answers
the ordinary OTP install's own bootstrap release, whose `releases/<vsn>/`
holds no `beamtalk-provenance.json`) — either way, `info/0` degrades to
`#{release => nil}` rather than raising: a reflective send must answer a
question, not crash the session asking it.

## `shape_manifest/0` — the BT-3571 projection, reused, not re-derived

Every entry comes from `beamtalk_release_shapes:class_shape_entry/1` — **the
same projection function** `releases/<vsn>/shapes.json` is built from
(ADR 0125 §2.2/§3.4's own requirement: "using the same projection function
... Do not write a second flattener"). This module contributes only what a
live node has that a build-time extractor does not: the set of currently
*registered* classes (`beamtalk_class_registry:live_class_entries/0`),
filtered to the project/dependency set with
`beamtalk_module_activation:is_release_class_module/1` — the identical
`bt@stdlib@*`-exclusion predicate `find_bt_modules_in_dir/1` uses when
staging `shapes.json`, so a live `shapeManifest` and the release's own
`shapes.json` can never structurally disagree about which classes are
"the project's" (CLAUDE.md's no-duplicate-implementations rule).
""".

-export([info/0, shape_manifest/0, shapeManifest/0]).

-ifdef(TEST).
%% Exported for EUnit coverage of the internal pieces without booting a real
%% release node (no `-boot_var RELEASE_DIR`/systools boot script available
%% in a plain `rebar3 eunit` run).
-export([release_dir_and_vsn/0, read_provenance/2, atomize_json_map/1]).
-endif.

%%====================================================================
%% Public API
%%====================================================================

-doc """
This node's release provenance (ADR 0125 §1.8), as a `Dictionary`-ready map
with atom keys — `#{release => <<"orders">>, release_version => <<"1.4.0">>,
otp_release => <<"28-16.0.2">>, required_otp => #{min => 28, max => 30}, ...}`
mirrors `beamtalk-provenance.json`'s own field names verbatim.

On a `run`/`workspace` node, or if the provenance file cannot be found or
parsed, returns `#{release => nil}` — never raises.
""".
-spec info() -> map().
info() ->
    case release_dir_and_vsn() of
        {ok, Dir, Vsn} ->
            read_provenance(Dir, Vsn);
        not_a_release ->
            #{release => nil}
    end.

-doc """
`class name -> #{version, fields, migrations}` for every registered
project/dependency class on this node (ADR 0125 §3.4) — the live-node twin
of `releases/<vsn>/shapes.json`. Each entry is
`beamtalk_release_shapes:class_shape_entry/1`'s own return value, unmodified
— see the moduledoc.

Stdlib classes are excluded, matching `shapes.json`'s own scope: they are
versioned with the toolchain (ADR 0098), not a project's `shapeVersion:`
declarations.
""".
-spec shape_manifest() -> #{atom() => beamtalk_release_shapes:shape_entry()}.
shape_manifest() ->
    lists:foldl(
        fun(ClassName, Acc) ->
            case beamtalk_release_shapes:class_shape_entry(ClassName) of
                undefined -> Acc;
                Entry -> Acc#{ClassName => Entry}
            end
        end,
        #{},
        project_class_names()
    ).

-doc "camelCase alias for the `(Erlang beamtalk_release) shapeManifest` FFI call — see the moduledoc.".
-spec shapeManifest() -> #{atom() => beamtalk_release_shapes:shape_entry()}.
shapeManifest() ->
    shape_manifest().

%%====================================================================
%% Internal
%%====================================================================

-doc """
Every currently registered class name this node hosts, excluding stdlib
classes — see the moduledoc's "shape_manifest/0" section.
""".
-spec project_class_names() -> [atom()].
project_class_names() ->
    [
        Name
     || {Name, ModuleName, _Pid} <- beamtalk_class_registry:live_class_entries(),
        beamtalk_module_activation:is_release_class_module(ModuleName)
    ].

-doc """
The release directory (from `-boot_var RELEASE_DIR <dir>`) and version (from
`init:script_id/0`) this node booted from, or `not_a_release` when either is
absent — see the moduledoc for why neither BIF is trusted alone.
""".
-spec release_dir_and_vsn() -> {ok, file:filename_all(), string()} | not_a_release.
release_dir_and_vsn() ->
    case {release_dir(), release_vsn()} of
        {{ok, Dir}, {ok, Vsn}} -> {ok, Dir, Vsn};
        _ -> not_a_release
    end.

-spec release_dir() -> {ok, file:filename_all()} | error.
release_dir() ->
    case init:get_argument(boot_var) of
        {ok, Args} ->
            case [Dir || ["RELEASE_DIR", Dir] <- Args] of
                [Dir | _] -> {ok, Dir};
                [] -> error
            end;
        error ->
            error
    end.

-spec release_vsn() -> {ok, string()} | error.
release_vsn() ->
    case init:script_id() of
        {_Name, Vsn} when is_list(Vsn) -> {ok, Vsn};
        {_Name, Vsn} when is_binary(Vsn) -> {ok, binary_to_list(Vsn)};
        _ -> error
    end.

-doc """
Read and decode `<Dir>/releases/<Vsn>/beamtalk-provenance.json`
(`json:decode/1`, OTP 27+ — the same decoder `beamtalk_release_shapes`
already uses to write it). Any failure (missing file, malformed JSON, an
unexpected top-level shape) degrades to `#{release => nil}` rather than
raising — see the moduledoc.
""".
-spec read_provenance(file:filename_all(), string()) -> map().
read_provenance(Dir, Vsn) ->
    Path = filename:join([Dir, "releases", Vsn, "beamtalk-provenance.json"]),
    case file:read_file(Path) of
        {ok, Bin} ->
            try json:decode(Bin) of
                Decoded when is_map(Decoded) -> atomize_json_map(Decoded);
                _ -> #{release => nil}
            catch
                _:_ -> #{release => nil}
            end;
        {error, _} ->
            #{release => nil}
    end.

-doc "Recursively convert a `json:decode/1` binary-keyed map to an atom-keyed one.".
-spec atomize_json_map(map()) -> map().
atomize_json_map(Map) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            Acc#{binary_to_atom(Key, utf8) => atomize_json_value(Value)}
        end,
        #{},
        Map
    ).

-spec atomize_json_value(term()) -> term().
atomize_json_value(Value) when is_map(Value) -> atomize_json_map(Value);
atomize_json_value(Value) when is_list(Value) -> [atomize_json_value(Item) || Item <- Value];
atomize_json_value(Value) -> Value.
