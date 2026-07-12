%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_precheck_tests).

-moduledoc """
Tests for the pre-save advisory precheck (ADR 0105 Phase 3, BT-2782):
`beamtalk_repl_loader:precheck_method/4`, the read-only sibling of
`install_method_with_source/10` that backs `Behaviour>>precheckCompile:
source:`.

Integration tests against the real compiler port + a real `beamtalk_xref` +
`beamtalk_workspace_meta`, mirroring `beamtalk_recheck_tests.erl`'s fixture
pattern — but building the fixture classes through `beamtalk_repl_eval:
do_eval/2` (a real class-definition compile + install) rather than manually
registering ambient class-hierarchy metadata, because `previous/3` (the
signature-diff baseline) reads the REAL loaded module's `__beamtalk_meta/0` —
a hand-rolled `register_class/2` double, as `beamtalk_recheck_tests.erl`
uses, has no such module to seed from.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Integration fixture: real compiler port + xref + workspace_meta
%%====================================================================

precheck_setup() ->
    application:ensure_all_started(compiler),
    case application:ensure_all_started(beamtalk_compiler) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    application:ensure_all_started(beamtalk_runtime),
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    %% `repl => false` for the same test-isolation reason
    %% `beamtalk_recheck_tests:recheck_setup/0` uses it — see that module's
    %% comment for the disk-persistence gotcha this avoids.
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"precheck_test_ws">>,
        project_path => undefined,
        created_at => erlang:system_time(second),
        repl => false
    }),
    %% ADR 0105 Phase 1 (BT-2777): `previous/3` (the precheck's signature-diff
    %% baseline) needs a live store — without it every diff degrades to
    %% `noproc`, matching `beamtalk_repl_loader_tests.erl`'s same fixture note.
    case whereis(beamtalk_workspace_signature_store) of
        undefined -> {ok, _} = beamtalk_workspace_signature_store:start_link();
        _ -> ok
    end,
    clear_xref(),
    beamtalk_compiler_server:clear_classes(),
    ok.

precheck_teardown(_) ->
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    case whereis(beamtalk_workspace_signature_store) of
        undefined -> ok;
        SigStorePid -> gen_server:stop(SigStorePid)
    end,
    clear_xref(),
    _ = application:stop(beamtalk_compiler),
    ok.

clear_xref() ->
    case whereis(beamtalk_xref) of
        undefined ->
            ok;
        _Pid ->
            try
                sys:replace_state(beamtalk_xref, fun(S) ->
                    ets:delete_all_objects(beamtalk_xref_methods),
                    ets:delete_all_objects(beamtalk_xref_senders),
                    ets:delete_all_objects(beamtalk_xref_references),
                    ets:delete_all_objects(xref_class_gen),
                    S
                end)
            catch
                _:_ -> ok
            end,
            ok
    end.

state0() ->
    beamtalk_repl_state:new(undefined, 0).

%% Real class-definition compile + install (not a hand-rolled ambient-meta
%% double) — the changed class, with a real `size -> Integer => 42` method.
%% `Actor subclass:` (not `Object subclass:`) because the test instantiates
%% it (`PrecheckCounter spawn size`) — a plain `Object`-kind class is not
%% instantiable (mirrors `beamtalk_repl_eval_tests:do_eval_class_definition/0`'s
%% fixture choice).
counter_source() ->
    "Actor subclass: PrecheckCounter\n"
    "  size -> Integer => 42".

%% Real dependent — the compiler's own xref population (ADR 0087) records
%% this class's `(c size) + 1` send as a real site, no manual
%% `beamtalk_xref:register_class/2` needed.
dashboard_source() ->
    "Object subclass: PrecheckDashboard\n"
    "  refresh: c :: PrecheckCounter -> Integer => (c size) + 1".

install_fixture() ->
    {ok, <<"PrecheckCounter">>, _, _, _} = beamtalk_repl_eval:do_eval(counter_source(), state0()),
    {ok, <<"PrecheckDashboard">>, _, _, _} = beamtalk_repl_eval:do_eval(
        dashboard_source(), state0()
    ),
    ok.

%%====================================================================
%% Pre-save advisory flags a would-be-stale caller without installing
%%====================================================================

precheck_flags_stale_caller_without_installing_test_() ->
    {timeout, 30,
        {setup, fun precheck_setup/0, fun precheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    ok = install_fixture(),

                    %% Sanity: the real, currently-live behaviour before any
                    %% precheck — size returns 42 (an Integer).
                    {ok, 42, _, _, _} = beamtalk_repl_eval:do_eval(
                        "PrecheckCounter spawn size", state0()
                    ),

                    %% Pending edit: size -> String. Not yet saved.
                    {ok, Result} = beamtalk_repl_loader:precheck_method(
                        <<"PrecheckCounter">>,
                        <<"size">>,
                        <<"size -> String => \"forty-two\"">>,
                        false
                    ),
                    #{findings := Findings, checked := Checked} = Result,
                    ?assertEqual(1, Checked),
                    ?assertEqual(1, length(Findings)),
                    [Finding] = Findings,
                    ?assertEqual(<<"PrecheckDashboard">>, maps:get(owner, Finding)),
                    ?assertEqual(signature_change, maps:get(classification, Finding)),

                    %% Never installed: the class source on record is
                    %% byte-for-byte the original — precheck never called
                    %% `beamtalk_workspace_meta:set_class_source/2`.
                    ?assertEqual(
                        counter_source(),
                        beamtalk_workspace_meta:get_class_source(<<"PrecheckCounter">>)
                    ),

                    %% Never installed: sending the real message still runs
                    %% the ORIGINAL method — the strongest proof nothing
                    %% patched the live image.
                    {ok, 42, _, _, _} = beamtalk_repl_eval:do_eval(
                        "PrecheckCounter spawn size", state0()
                    )
                end)
            ]
        end}}.

%%====================================================================
%% Pre-save advisory flags a would-be-stale caller — class side (only the
%% instance side above was exercised until now: `IsClassMethod = true`
%% threads a different meta key (`class_method_info`, `method_info_key/1`)
%% through `override_method_signature/4` — worth its own coverage).
%%====================================================================

class_counter_source() ->
    "Object subclass: PrecheckClassCounter\n"
    "  class size -> Integer => 42".

%% Real dependent of the CLASS-side `size` — sends to the class object
%% itself (`PrecheckClassCounter size`), not an instance.
class_dashboard_source() ->
    "Object subclass: PrecheckClassDashboard\n"
    "  refresh -> Integer => (PrecheckClassCounter size) + 1".

install_class_side_fixture() ->
    {ok, <<"PrecheckClassCounter">>, _, _, _} = beamtalk_repl_eval:do_eval(
        class_counter_source(), state0()
    ),
    {ok, <<"PrecheckClassDashboard">>, _, _, _} = beamtalk_repl_eval:do_eval(
        class_dashboard_source(), state0()
    ),
    ok.

precheck_flags_stale_caller_class_side_test_() ->
    {timeout, 30,
        {setup, fun precheck_setup/0, fun precheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    ok = install_class_side_fixture(),

                    %% Sanity: the real, currently-live behaviour before any
                    %% precheck — class-side size returns 42 (an Integer).
                    {ok, 42, _, _, _} = beamtalk_repl_eval:do_eval(
                        "PrecheckClassCounter size", state0()
                    ),

                    %% Pending edit: class size -> String. Not yet saved.
                    %% `IsClassMethod = true` — the class-side sibling of
                    %% `precheck_flags_stale_caller_without_installing_test_`.
                    {ok, Result} = beamtalk_repl_loader:precheck_method(
                        <<"PrecheckClassCounter">>,
                        <<"size">>,
                        <<"class size -> String => \"forty-two\"">>,
                        true
                    ),
                    #{findings := Findings, checked := Checked} = Result,
                    ?assertEqual(1, Checked),
                    ?assertEqual(1, length(Findings)),
                    [Finding] = Findings,
                    ?assertEqual(<<"PrecheckClassDashboard">>, maps:get(owner, Finding)),
                    ?assertEqual(signature_change, maps:get(classification, Finding)),

                    %% Never installed: sending the real message still runs
                    %% the ORIGINAL class-side method.
                    {ok, 42, _, _, _} = beamtalk_repl_eval:do_eval(
                        "PrecheckClassCounter size", state0()
                    )
                end)
            ]
        end}}.

%%====================================================================
%% A pending edit with no type-relevant signature change reports empty
%%====================================================================

precheck_no_op_signature_reports_empty_test_() ->
    {timeout, 30,
        {setup, fun precheck_setup/0, fun precheck_teardown/1, fun(_) ->
            [
                ?_test(begin
                    ok = install_fixture(),

                    %% Same declared signature (still -> Integer), only the
                    %% body's literal changes — beamtalk_signature_diff:diff/2
                    %% classifies this `no_op`.
                    {ok, Result} = beamtalk_repl_loader:precheck_method(
                        <<"PrecheckCounter">>,
                        <<"size">>,
                        <<"size -> Integer => 43">>,
                        false
                    ),
                    ?assertEqual(
                        #{
                            findings => [],
                            checked => 0,
                            total_candidates => 0,
                            not_checked => 0,
                            cap_note => undefined,
                            checked_owners => [],
                            not_checked_owners => [],
                            not_verified_owners => []
                        },
                        Result
                    )
                end)
            ]
        end}}.

%%====================================================================
%% A stdlib-read-only-style class-source-missing error surfaces cleanly
%%====================================================================

precheck_unknown_class_is_compile_error_test_() ->
    {timeout, 30,
        {setup, fun precheck_setup/0, fun precheck_teardown/1, fun(_) ->
            [
                ?_test(
                    ?assertMatch(
                        {error, {compile_error, _}},
                        beamtalk_repl_loader:precheck_method(
                            <<"PrecheckNeverLoadedClass">>,
                            <<"foo">>,
                            <<"foo => 1">>,
                            false
                        )
                    )
                )
            ]
        end}}.
