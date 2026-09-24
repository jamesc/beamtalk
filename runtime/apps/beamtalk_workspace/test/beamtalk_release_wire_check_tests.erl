%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_release_wire_check_tests).

%%% **DDD Context:** Workspace Context

-moduledoc """
ADR 0125 Phase 0 wire check, promoted into CI (BT-3569).

Proves the assumption every later release-mode phase rests on: **a node
boots from `start.boot`, in interactive mode, with the real
`beamtalk_runtime` + `beamtalk_stdlib` + `beamtalk_workspace` (staged from
their **generated** `.app` files, never `.app.src`) plus one fixture project
app, and every fixture `bt@*` class is registered before the project's root
supervisor's first actor starts** — with `sys.config` setting
`beamtalk_workspace`'s `mode` to `release`, so it is
`beamtalk_workspace_app:start/2`'s app-env-driven path (not a CLI `-eval`
string) that brings the workspace up.

## What this suite stages, and what it does not

It computes the real application-dependency closure of `beamtalk_workspace`
(via `application:get_key/2`, starting from the *already loaded* apps this
test itself runs with — the same compiled `_build/default/lib/*/ebin` a
`just build` produces) and copies every app **not** already under
`code:root_dir()` into a fresh `lib/<app>-<vsn>/ebin/` tree — exactly ADR
0125 §1.3's "stage from the generated `.app`" rule, since `code:lib_dir/1`
for `beamtalk_runtime`/`beamtalk_stdlib`/`beamtalk_workspace` already points
at their `_build/default/lib/<app>/ebin` (the rebar3-resolved `.app`, not
the `{cmd, …}`-templated `.app.src`). Apps already under `code:root_dir()`
(`kernel`, `stdlib`, `sasl`, `crypto`, …) are left where the host OTP
install already has them, resolvable without staging.

A minimal fixture OTP application (`wc_fixture`) stands in for "one project
app": a hand-compiled `bt@wc_fixture@*` class module (built the same way
`beamtalk_workspace_bootstrap_tests` builds its activation fixtures — via
`compile:forms/2`, not the real Beamtalk compiler, since this suite proves
the *wire*, not the compiler) plus a root supervisor whose one child records,
at `init/1` time, whether the class was already registered — the literal
ADR 0125 ordering claim, not just "registered by the time boot finishes".

Writing the `.rel`, calling `systools:make_script/2` with absolute paths and
`{variables, [{"RELEASE_DIR", Dir}]}`, and driving `wc_fixture`'s own
`beamtalk.toml`/`app_file.rs` output are **not** reproduced here — the ADR
scopes real `[release]` assembly to BT-3570 ("the next issue reuses this
test's staging recipe in Rust"). This suite hand-writes the `.rel` and the
fixture `.app` term directly, which is enough to prove the runtime wiring
(`beamtalk_workspace_app`/`beamtalk_workspace_app_sup`/
`beamtalk_module_activation:collect_loaded_app_modules/0`) without waiting
on that later issue.
""".

-include_lib("eunit/include/eunit.hrl").

-define(REL_NAME, "wirecheck").
-define(REL_VSN, "0.1.0").
-define(FIXTURE_APP, wc_fixture).
-define(FIXTURE_VSN, "0.1.0").
-define(FIXTURE_CLASS, 'WcFixtureClass').
-define(FIXTURE_MODULE, 'bt@wc_fixture@wc_fixture_class').

wire_check_test_() ->
    {timeout, 120, fun() -> run_wire_check() end}.

run_wire_check() ->
    Stage = stage_dir(),
    _ = file:del_dir_r(Stage),
    ok = filelib:ensure_dir(filename:join([Stage, "lib", "x", "ebin", "x"])),
    try
        {AppVsns, ExtraPaths} = stage_release_tree(Stage),
        BootPath = write_rel_and_boot_script(Stage, AppVsns, ExtraPaths),
        SysConfigPath = write_sys_config(Stage),
        {ok, PeerPid, _Node} = boot_release_node(BootPath, Stage, SysConfigPath),
        try
            assert_wire_check(PeerPid)
        after
            catch peer:stop(PeerPid)
        end
    after
        _ = file:del_dir_r(Stage)
    end.

%%====================================================================
%% Staging
%%====================================================================

-doc """
Compute `beamtalk_workspace`'s real application-dependency closure (via
`application:get_key(App, applications)`, topologically sorted so a
dependency precedes its dependent) and stage every app not already under
`code:root_dir()` into `<Stage>/lib/<app>-<vsn>/ebin/`. Returns the
`{App, Vsn}` list in dependency order (with the fixture app appended last)
and the absolute list of staged ebin directories for `systools`'s `path`
option.
""".
-spec stage_release_tree(file:filename()) ->
    {[{atom(), string()}], [file:filename()]}.
stage_release_tree(Stage) ->
    {_Seen, Closure} = lists:foldl(
        fun(App, {Seen, Acc}) -> topo_load(App, Seen, Acc) end,
        {sets:new(), []},
        [beamtalk_workspace]
    ),
    RootDir = code:root_dir(),
    AppVsns = lists:map(
        fun(App) ->
            {ok, Vsn} = application:get_key(App, vsn),
            case lists:prefix(RootDir, code:lib_dir(App)) of
                true -> ok;
                false -> stage_app_ebin(Stage, App, Vsn, code:lib_dir(App))
            end,
            {App, Vsn}
        end,
        Closure
    ),
    FixtureEbin = staged_ebin_dir(Stage, ?FIXTURE_APP, ?FIXTURE_VSN),
    ok = filelib:ensure_dir(filename:join(FixtureEbin, "x")),
    build_fixture_app(FixtureEbin),
    ExtraPaths = [
        filename:absname(staged_ebin_dir(Stage, App, Vsn))
     || {App, Vsn} <- AppVsns, not lists:prefix(RootDir, code:lib_dir(App))
    ],
    {AppVsns ++ [{?FIXTURE_APP, ?FIXTURE_VSN}], [filename:absname(FixtureEbin) | ExtraPaths]}.

-doc "Post-order dependency-closure walk: a dependency's entry precedes its dependent's.".
topo_load(App, Seen, Acc) ->
    case sets:is_element(App, Seen) of
        true ->
            {Seen, Acc};
        false ->
            _ = application:load(App),
            Deps =
                case application:get_key(App, applications) of
                    {ok, D} -> D;
                    undefined -> []
                end,
            {Seen1, Acc1} = lists:foldl(
                fun(Dep, {S, A}) -> topo_load(Dep, S, A) end,
                {sets:add_element(App, Seen), Acc},
                Deps
            ),
            {Seen1, Acc1 ++ [App]}
    end.

staged_ebin_dir(Stage, App, Vsn) ->
    filename:join([Stage, "lib", atom_to_list(App) ++ "-" ++ Vsn, "ebin"]).

stage_app_ebin(Stage, App, Vsn, SrcAppDir) ->
    Dest = staged_ebin_dir(Stage, App, Vsn),
    ok = filelib:ensure_dir(filename:join(Dest, "x")),
    SrcEbin = filename:join(SrcAppDir, "ebin"),
    {ok, Files} = file:list_dir(SrcEbin),
    lists:foreach(
        fun(F) ->
            {ok, _} = file:copy(filename:join(SrcEbin, F), filename:join(Dest, F))
        end,
        Files
    ),
    ok.

-doc """
Build the fixture project app: one `bt@*` class module (compiled the way
`beamtalk_workspace_bootstrap_tests:compile_activation_fixture/3` does —
abstract forms through `compile:forms/2`, calling
`beamtalk_object_class:start/2` from `register_class/0`), a supervisor whose
one worker records — at its own `init/1`, the earliest moment the fixture
app's own code runs — whether the class was already registered, and the
`.app`/`{mod, …}` wiring `app_file.rs`/`outputs.rs` would otherwise
generate.
""".
build_fixture_app(Ebin) ->
    ok = write_fixture_class_beam(Ebin),
    ok = write_compiled_module(
        Ebin,
        wc_fixture_worker,
        "-module(wc_fixture_worker).\n"
        "-behaviour(gen_server).\n"
        "-export([start_link/0, init/1, handle_call/3, handle_cast/2]).\n"
        "start_link() -> gen_server:start_link({local, wc_fixture_worker}, wc_fixture_worker, [], []).\n"
        "init([]) ->\n"
        "    Registered = beamtalk_class_registry:whereis_class('" ++
            atom_to_list(?FIXTURE_CLASS) ++
            "') =/= undefined,\n"
            "    persistent_term:put(wc_fixture_class_registered_at_root_start, Registered),\n"
            "    {ok, #{}}.\n"
            "handle_call(_Msg, _From, State) -> {reply, ok, State}.\n"
            "handle_cast(_Msg, State) -> {noreply, State}.\n"
    ),
    ok = write_compiled_module(
        Ebin,
        wc_fixture_sup,
        "-module(wc_fixture_sup).\n"
        "-behaviour(supervisor).\n"
        "-export([start_link/0, init/1]).\n"
        "start_link() -> supervisor:start_link({local, wc_fixture_sup}, wc_fixture_sup, []).\n"
        "init([]) ->\n"
        "    ChildSpec = #{id => wc_fixture_worker,\n"
        "                  start => {wc_fixture_worker, start_link, []},\n"
        "                  restart => permanent, shutdown => 5000, type => worker,\n"
        "                  modules => [wc_fixture_worker]},\n"
        "    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, [ChildSpec]}}.\n"
    ),
    ok = write_compiled_module(
        Ebin,
        wc_fixture_app,
        "-module(wc_fixture_app).\n"
        "-behaviour(application).\n"
        "-export([start/2, stop/1]).\n"
        "start(_Type, _Args) -> wc_fixture_sup:start_link().\n"
        "stop(_State) -> ok.\n"
    ),
    AppTerm =
        {application, ?FIXTURE_APP, [
            {description, "ADR 0125 wire-check fixture project app"},
            {vsn, ?FIXTURE_VSN},
            {modules, [wc_fixture_app, wc_fixture_sup, wc_fixture_worker, ?FIXTURE_MODULE]},
            {registered, []},
            %% Mirrors app_file.rs's format_applications_list/2 (ADR 0125 §1.4):
            %% beamtalk_workspace right after beamtalk_runtime.
            {applications, [kernel, stdlib, beamtalk_runtime, beamtalk_workspace]},
            {mod, {wc_fixture_app, []}},
            {env, []}
        ]},
    file:write_file(
        filename:join(Ebin, atom_to_list(?FIXTURE_APP) ++ ".app"),
        io_lib:format("~p.~n", [AppTerm])
    ).

write_fixture_class_beam(Ebin) ->
    ModName = ?FIXTURE_MODULE,
    ClassName = ?FIXTURE_CLASS,
    Forms = [
        {attribute, 1, module, ModName},
        {attribute, 2, export, [{register_class, 0}]},
        {attribute, 2, beamtalk_class, [{ClassName, 'Object'}]},
        {function, 3, register_class, 0, [
            {clause, 3, [], [], [
                {'case', 4,
                    {call, 4, {remote, 4, {atom, 4, beamtalk_object_class}, {atom, 4, start}}, [
                        {atom, 4, ClassName},
                        {map, 4, [
                            {map_field_assoc, 4, {atom, 4, name}, {atom, 4, ClassName}},
                            {map_field_assoc, 4, {atom, 4, superclass}, {atom, 4, 'Object'}},
                            {map_field_assoc, 4, {atom, 4, module}, {atom, 4, ModName}},
                            {map_field_assoc, 4, {atom, 4, instance_variables}, {nil, 4}},
                            {map_field_assoc, 4, {atom, 4, class_methods}, {map, 4, []}},
                            {map_field_assoc, 4, {atom, 4, instance_methods}, {map, 4, []}}
                        ]}
                    ]},
                    [
                        {clause, 5, [{tuple, 5, [{atom, 5, ok}, {var, 5, '_Pid'}]}], [], [
                            {atom, 5, ok}
                        ]},
                        {clause, 6, [{tuple, 6, [{atom, 6, error}, {var, 6, '_'}]}], [], [
                            {atom, 6, ok}
                        ]}
                    ]}
            ]}
        ]}
    ],
    {ok, ModName, BeamBin} = compile:forms(Forms, []),
    file:write_file(filename:join(Ebin, atom_to_list(ModName) ++ ".beam"), BeamBin).

-doc "Compile a hand-written module source (test-only: no epp, so no macros/records).".
write_compiled_module(Ebin, ExpectedModule, Src) ->
    {ok, Tokens, _} = erl_scan:string(Src),
    Forms = lists:map(
        fun(FormTokens) ->
            {ok, Form} = erl_parse:parse_form(FormTokens),
            Form
        end,
        split_on_dot(Tokens)
    ),
    {ok, ExpectedModule, BeamBin} = compile:forms(Forms, []),
    file:write_file(filename:join(Ebin, atom_to_list(ExpectedModule) ++ ".beam"), BeamBin).

split_on_dot(Tokens) ->
    split_on_dot(Tokens, [], []).

split_on_dot([], [], Acc) ->
    lists:reverse(Acc);
split_on_dot([{dot, _} = Dot | Rest], Cur, Acc) ->
    split_on_dot(Rest, [], [lists:reverse([Dot | Cur]) | Acc]);
split_on_dot([Tok | Rest], Cur, Acc) ->
    split_on_dot(Rest, [Tok | Cur], Acc).

%%====================================================================
%% .rel / boot script / sys.config
%%====================================================================

-doc """
Write the `.rel` and call `systools:make_script/2` with absolute staged
paths plus `{variables, [{"RELEASE_DIR", Stage}]}` — ADR 0125 §1.3's
`$ROOT`/`RELEASE_DIR` finding: a path found under `Stage` is recorded as
`$RELEASE_DIR/…` in the emitted script so the peer node (started with
`-boot_var RELEASE_DIR Stage`) resolves it, while apps already under
`code:root_dir()` keep their absolute host-OTP path. Returns the `.boot`
file's path without its extension, as `-boot` expects.
""".
-spec write_rel_and_boot_script(file:filename(), [{atom(), string()}], [file:filename()]) ->
    file:filename().
write_rel_and_boot_script(Stage, AppVsns, ExtraPaths) ->
    RelDir = filename:join([Stage, "releases", ?REL_VSN]),
    ok = filelib:ensure_dir(filename:join(RelDir, "x")),
    ErtsVsn = erlang:system_info(version),
    RelTerm = {release, {?REL_NAME, ?REL_VSN}, {erts, ErtsVsn}, AppVsns},
    RelFile = filename:join(RelDir, ?REL_NAME ++ ".rel"),
    ok = file:write_file(RelFile, io_lib:format("~p.~n", [RelTerm])),
    ScriptBase = filename:join(RelDir, ?REL_NAME),
    Result = systools:make_script(ScriptBase, [
        {path, ExtraPaths},
        {variables, [{"RELEASE_DIR", filename:absname(Stage)}]},
        silent
    ]),
    ?assertMatch({ok, systools_make, _}, Result),
    ScriptBase.

-doc """
`sys.config` setting `mode => release` in the `beamtalk_workspace`
application env (ADR 0125 §1.4) — the only way this wire check reaches
`beamtalk_workspace_app`'s app-env-driven start path, since nothing here
runs the CLI's `-eval` string.
""".
-spec write_sys_config(file:filename()) -> file:filename().
write_sys_config(Stage) ->
    RelDir = filename:join([Stage, "releases", ?REL_VSN]),
    SysConfig = [
        {beamtalk_workspace, [
            {mode, release},
            {console, false},
            {auto_cleanup, false}
        ]}
    ],
    Path = filename:join(RelDir, "sys"),
    ok = file:write_file(Path ++ ".config", io_lib:format("~p.~n", [SysConfig])),
    Path.

%%====================================================================
%% Boot + assertions
%%====================================================================

-doc """
Boot a `peer` node (ADR 0125 §1.3's own validation used exactly this
mechanism) from the staged `.boot` in interactive mode, with
`-boot_var RELEASE_DIR`. `connection => standard_io` needs no distribution
on either side — this test process stays non-distributed.

`shutdown => close` (rather than the `halt`/numeric-timeout variants, whose
docs describe them in terms of the Erlang *distribution* connection) just
closes the control port and returns — there is no distribution connection
to wait on here, and waiting anyway against a full workspace boot (cowboy,
telemetry_poller, the workspace supervision tree) was observed to make
`peer:stop/1` hang.
""".
-spec boot_release_node(file:filename(), file:filename(), file:filename()) ->
    {ok, pid(), atom()}.
boot_release_node(BootPath, Stage, SysConfigPath) ->
    Args = [
        "-noshell",
        "-boot",
        BootPath,
        "-boot_var",
        "RELEASE_DIR",
        Stage,
        "-config",
        SysConfigPath
    ],
    peer:start(#{
        args => Args,
        connection => standard_io,
        wait_boot => 30000,
        shutdown => close
    }).

-doc """
The ADR 0125 Phase 0 acceptance criterion, checked on the booted peer:
every fixture `bt@*` class is registered before the project's root
supervisor's first actor starts, and `beamtalk_idle_monitor`,
`beamtalk_compiler` and the ADR 0105 stores are not running.
""".
assert_wire_check(PeerPid) ->
    %% The literal ordering claim: wc_fixture_worker's own init/1 (the first
    %% actor the fixture's root supervisor starts) recorded whether the
    %% class was already registered at that exact moment.
    ?assertEqual(
        true,
        peer:call(PeerPid, persistent_term, get, [wc_fixture_class_registered_at_root_start])
    ),
    %% ...and it is still registered now, as a sanity check on the above.
    ?assertMatch(
        Pid when is_pid(Pid),
        peer:call(PeerPid, beamtalk_class_registry, whereis_class, [?FIXTURE_CLASS])
    ),
    %% The workspace itself came up under beamtalk_workspace_app_sup.
    ?assertMatch(
        Pid when is_pid(Pid), peer:call(PeerPid, erlang, whereis, [beamtalk_workspace_sup])
    ),
    %% The fixture's own root supervisor came up too (proves ADR 0125 §1.2's
    %% application ordering: beamtalk_workspace started, and activated
    %% classes, before wc_fixture_app:start/2 ran).
    ?assertMatch(Pid when is_pid(Pid), peer:call(PeerPid, erlang, whereis, [wc_fixture_sup])),
    %% Release mode starts none of these (ADR 0125 §1.4).
    ?assertEqual(undefined, peer:call(PeerPid, erlang, whereis, [beamtalk_idle_monitor])),
    ?assertEqual(
        undefined, peer:call(PeerPid, erlang, whereis, [beamtalk_workspace_signature_store])
    ),
    ?assertEqual(undefined, peer:call(PeerPid, erlang, whereis, [beamtalk_workspace_shape_store])),
    ?assertEqual(undefined, peer:call(PeerPid, erlang, whereis, [beamtalk_alias_xref])),
    ?assertEqual(
        undefined,
        peer:call(PeerPid, erlang, whereis, [beamtalk_workspace_shape_recheck_worker])
    ),
    ?assertEqual(
        undefined, peer:call(PeerPid, erlang, whereis, [beamtalk_workspace_findings_store])
    ),
    Apps = peer:call(PeerPid, application, which_applications, []),
    ?assertEqual(false, lists:keyfind(beamtalk_compiler, 1, Apps)),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

stage_dir() ->
    filename:join(
        get_temp_dir(),
        "beamtalk_wire_check_" ++ integer_to_list(erlang:unique_integer([positive]))
    ).

get_temp_dir() -> binary_to_list(beamtalk_file:'tempDirectory'()).
