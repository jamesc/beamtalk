%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_loader_leaf_change_recheck_tests).

-moduledoc """
End-to-end wiring tests for ADR 0107 Phase A (BT-2856): a real class-body
load (`beamtalk_repl_loader:handle_load/2`) that gives a previously-leaf
class its first subclass, through `superclasses_losing_leaf_status/1`'s
detection, `beamtalk_recheck:trigger_leaf_change/1`'s re-check, and the same
publish path BT-2779/BT-2780 established
(`beamtalk_workspace_findings_store` + the `'ReloadCheckCompleted'`
announcement).

Mirrors `beamtalk_repl_loader_shape_recheck_tests.erl`'s structure exactly
(same fixture shape, same real-compiler-port/real-class-hierarchy
philosophy) — this is its leaf-change counterpart, exercising a dependent
that has no xref-recorded call site at all (there is no selector to record
a `Type`-pattern/`matchExhaustive:` site under; the whole reason
`trigger_leaf_change/1` re-checks every live class's own source instead of
an xref-filtered candidate set — see that function's doc).
""".

-include_lib("eunit/include/eunit.hrl").

temp_dir() -> binary_to_list(beamtalk_file:'tempDirectory'()).

%%====================================================================
%% Pure helpers — superclasses_losing_leaf_status/1, was_leaf_class/1
%%====================================================================

%% Never having been loaded at all: not even an interned atom yet, so
%% `was_leaf_class/1` must degrade to `false` (nothing could depend on an
%% unloaded class being leaf) rather than crash.
was_leaf_class_false_for_never_loaded_class_test() ->
    ?assertNot(
        beamtalk_repl_loader:was_leaf_class(
            <<"LeafHelperNeverLoadedClass_",
                (integer_to_binary(
                    erlang:unique_integer([positive])
                ))/binary>>
        )
    ).

superclasses_losing_leaf_status_ignores_classes_without_a_superclass_key_test() ->
    %% A class map missing the `superclass` key entirely (list comprehension
    %% generator silently skips a non-matching element) contributes nothing —
    %% must not crash.
    ?assertEqual(
        [],
        beamtalk_repl_loader:superclasses_losing_leaf_status([#{name => "NoSuperclassHere"}])
    ).

%% This function runs in the class-install hot path with no surrounding
%% `try/catch` at any of its four call sites (`load_class_module/3`,
%% `load_compiled_module/6`, `reload_compile_and_load/4`,
%% `new_class_install/7`) — a malformed `superclass` value (here, an atom
%% `unicode:characters_to_binary/1` cannot coerce) must degrade to `[]`
%% rather than crash the class load itself (ADR 0105: advisory, never
%% blocking). Regression pin for exactly this defensive wrapping.
superclasses_losing_leaf_status_never_crashes_the_caller_test() ->
    ?assertEqual(
        [],
        beamtalk_repl_loader:superclasses_losing_leaf_status([
            #{name => "Malformed", superclass => not_a_string_or_binary}
        ])
    ).

superclasses_losing_leaf_status_dedupes_test_() ->
    {timeout, 30,
        {setup, fun leaf_loader_setup/0, fun leaf_loader_teardown/1, fun(_) ->
            [
                ?_test(begin
                    ShapePath = filename:join(
                        temp_dir(),
                        io_lib:format("leaf_helper_shape_~p.bt", [
                            erlang:unique_integer([positive])
                        ])
                    ),
                    ok = file:write_file(ShapePath, <<"Object subclass: LeafHelperShape\n">>),
                    State0 = beamtalk_repl_state:new(undefined, 0),
                    {ok, _, _State1} = beamtalk_repl_loader:handle_load(ShapePath, State0),

                    %% Two classes declaring the SAME superclass in one
                    %% `Classes` list (as if compiled from the same file) —
                    %% the result names it once, not twice.
                    Classes = [
                        #{name => "LeafHelperCircle", superclass => "LeafHelperShape"},
                        #{name => "LeafHelperSquare", superclass => "LeafHelperShape"}
                    ],
                    ?assertEqual(
                        [<<"LeafHelperShape">>],
                        beamtalk_repl_loader:superclasses_losing_leaf_status(Classes)
                    )
                end)
            ]
        end}}.

%% `superclass` arriving as a `binary()` (as it does on the real
%% `load_class_module/3` -> `handle_load/2` path) must not badarg
%% `unicode:characters_to_binary/1` the way a naive `list_to_binary/1` would
%% — this is a regression pin for exactly that bug (found via this feature's
%% own end-to-end test failing with `{error, badarg, [{erlang, list_to_binary, ...`).
superclasses_losing_leaf_status_accepts_binary_superclass_test_() ->
    {timeout, 30,
        {setup, fun leaf_loader_setup/0, fun leaf_loader_teardown/1, fun(_) ->
            [
                ?_test(begin
                    ShapePath = filename:join(
                        temp_dir(),
                        io_lib:format("leaf_helper_binary_shape_~p.bt", [
                            erlang:unique_integer([positive])
                        ])
                    ),
                    ok = file:write_file(
                        ShapePath, <<"Object subclass: LeafHelperBinaryShape\n">>
                    ),
                    State0 = beamtalk_repl_state:new(undefined, 0),
                    {ok, _, _State1} = beamtalk_repl_loader:handle_load(ShapePath, State0),

                    Classes = [
                        #{
                            name => "LeafHelperBinaryCircle",
                            superclass => <<"LeafHelperBinaryShape">>
                        }
                    ],
                    ?assertEqual(
                        [<<"LeafHelperBinaryShape">>],
                        beamtalk_repl_loader:superclasses_losing_leaf_status(Classes)
                    )
                end)
            ]
        end}}.

%%====================================================================
%% Integration fixture: real compiler port + xref + workspace_meta + stores
%%====================================================================

leaf_loader_setup() ->
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
    {ok, _} = beamtalk_workspace_meta:start_link(#{
        workspace_id => <<"leaf_change_loader_test_ws">>,
        project_path => undefined,
        created_at => erlang:system_time(second),
        repl => false
    }),
    clear_xref(),
    beamtalk_compiler_server:clear_classes(),
    case whereis(beamtalk_workspace_shape_recheck_worker) of
        undefined -> ok;
        WorkerPid -> gen_server:stop(WorkerPid)
    end,
    {ok, _} = beamtalk_workspace_shape_recheck_worker:start_link(),
    case whereis(beamtalk_workspace_findings_store) of
        undefined -> ok;
        FindingsPid -> gen_server:stop(FindingsPid)
    end,
    {ok, _} = beamtalk_workspace_findings_store:start_link(),
    ok = beamtalk_announcements:ensure_started(),
    ok.

leaf_loader_teardown(_) ->
    beamtalk_repl_subscriptions:unsubscribe(reload_check, self()),
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        MetaPid -> gen_server:stop(MetaPid)
    end,
    case whereis(beamtalk_workspace_shape_recheck_worker) of
        undefined -> ok;
        WorkerPid -> gen_server:stop(WorkerPid)
    end,
    case whereis(beamtalk_workspace_findings_store) of
        undefined -> ok;
        FindingsPid -> gen_server:stop(FindingsPid)
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

subscribe_self_to_reload_check() ->
    ok = beamtalk_repl_subscriptions:subscribe(reload_check, self()).

receive_reload_check_announcement() ->
    receive
        {beamtalk_announcement, _SubRef, 'ReloadCheckCompleted', _Handler, Event} ->
            Event
    after 2000 ->
        error(timeout_waiting_for_reload_check_announcement)
    end.

%% Forces the (asynchronous, `beamtalk_workspace_shape_recheck_worker`-queued)
%% leaf-change re-check to have run to completion before a test asserts on
%% its outcome — mirrors `beamtalk_repl_loader_shape_recheck_tests.erl`'s own
%% reliance on the worker's mailbox being strictly FIFO: a `sys:get_state/1`
%% round trip only returns once every message enqueued ahead of it
%% (including our `{leaf_change, _}` cast) has already been processed.
wait_for_leaf_change_worker() ->
    _ = sys:get_state(beamtalk_workspace_shape_recheck_worker),
    ok.

%%====================================================================
%% Fixtures
%%====================================================================

shape_source() ->
    <<"Object subclass: LeafRecheckShape\n">>.

%% Uses a plain `Type` pattern (not `matchExhaustive:`) — the "has
%% subclasses" compile error (BT-2854) fires on *any* `match:` with a
%% `Type` pattern on a non-leaf class, independent of exhaustiveness
%% assertions, so this is the simplest possible dependent.
user_source() ->
    <<
        "Object subclass: LeafRecheckUser\n"
        "  test: x => x match: [s :: LeafRecheckShape -> 1; _ -> 0]\n"
    >>.

circle_source() ->
    <<"LeafRecheckShape subclass: LeafRecheckCircle\n">>.

%%====================================================================
%% End-to-end: a leaf class gaining its first subclass flags a `Type`
%% pattern dependent that has no xref-recorded call site at all
%%====================================================================

gaining_a_first_subclass_flags_a_type_pattern_dependent_test_() ->
    {timeout, 30,
        {setup, fun leaf_loader_setup/0, fun leaf_loader_teardown/1, fun(_) ->
            [
                ?_test(begin
                    subscribe_self_to_reload_check(),

                    %% `LeafRecheckShape` — leaf, zero subclasses.
                    ShapePath = filename:join(
                        temp_dir(),
                        io_lib:format("leaf_recheck_shape_~p.bt", [
                            erlang:unique_integer([positive])
                        ])
                    ),
                    ok = file:write_file(ShapePath, shape_source()),
                    State0 = beamtalk_repl_state:new(undefined, 0),
                    {ok, _, State1} = beamtalk_repl_loader:handle_load(ShapePath, State0),

                    %% `LeafRecheckUser` — the dependent, compiles clean while
                    %% `LeafRecheckShape` is still leaf. Its live source is
                    %% recorded the same way `beamtalk_repl_loader_shape_recheck_tests`'s
                    %% dependent fixtures are (`set_class_source/2` directly),
                    %% since `trigger_leaf_change/1` reads from
                    %% `beamtalk_workspace_meta:all_class_sources/0`, not xref.
                    ok = beamtalk_workspace_meta:set_class_source(
                        <<"LeafRecheckUser">>, user_source()
                    ),

                    %% `LeafRecheckShape subclass: LeafRecheckCircle` —
                    %% `LeafRecheckShape` was leaf until this exact install;
                    %% `activate_module/3`'s `superclasses_losing_leaf_status/1`
                    %% detects the transition before this install even lands,
                    %% enqueues the leaf-change re-check, and the worker runs
                    %% it to completion (`wait_for_leaf_change_worker/0`) —
                    %% asynchronously, off this call's own return.
                    CirclePath = filename:join(
                        temp_dir(),
                        io_lib:format("leaf_recheck_circle_~p.bt", [
                            erlang:unique_integer([positive])
                        ])
                    ),
                    ok = file:write_file(CirclePath, circle_source()),
                    {ok, _, _State2} = beamtalk_repl_loader:handle_load(CirclePath, State1),

                    Event = receive_reload_check_announcement(),
                    ?assertEqual(<<"LeafRecheckShape">>, maps:get(changedClass, Event)),
                    ?assertEqual(leaf_change, maps:get(classification, Event)),
                    ?assert(
                        lists:member(<<"LeafRecheckUser">>, maps:get(checkedOwners, Event))
                    ),

                    StoredFindings = beamtalk_workspace_findings_store:for_owner(
                        <<"LeafRecheckUser">>
                    ),
                    ?assertEqual(1, length(StoredFindings)),
                    [Finding] = StoredFindings,
                    ?assertEqual(leaf_change, maps:get(classification, Finding)),
                    ?assertEqual(<<"LeafRecheckShape">>, maps:get(changed_class, Finding)),
                    ?assertEqual(<<"Type">>, maps:get(category, Finding)),
                    ?assert(
                        binary:match(maps:get(message, Finding), <<"LeafRecheckShape">>) =/=
                            nomatch
                    )
                end)
            ]
        end}}.

%%====================================================================
%% Regression: reloading the SAME subclass again (e.g. a method-only edit
%% to `LeafRecheckCircle`, unchanged superclass) must NOT re-fire — the
%% superclass was already non-leaf the first time this subclass installed.
%%====================================================================

reloading_the_same_subclass_again_does_not_refire_test_() ->
    {timeout, 30,
        {setup, fun leaf_loader_setup/0, fun leaf_loader_teardown/1, fun(_) ->
            [
                ?_test(begin
                    ShapePath = filename:join(
                        temp_dir(),
                        io_lib:format("leaf_recheck_shape2_~p.bt", [
                            erlang:unique_integer([positive])
                        ])
                    ),
                    ok = file:write_file(ShapePath, <<"Object subclass: LeafRecheckShape2\n">>),
                    State0 = beamtalk_repl_state:new(undefined, 0),
                    {ok, _, State1} = beamtalk_repl_loader:handle_load(ShapePath, State0),

                    CirclePath = filename:join(
                        temp_dir(),
                        io_lib:format("leaf_recheck_circle2_~p.bt", [
                            erlang:unique_integer([positive])
                        ])
                    ),
                    ok = file:write_file(
                        CirclePath, <<"LeafRecheckShape2 subclass: LeafRecheckCircle2\n">>
                    ),
                    {ok, _, State2} = beamtalk_repl_loader:handle_load(CirclePath, State1),
                    wait_for_leaf_change_worker(),

                    %% Second install of the *same* subclass file (e.g. a
                    %% no-op re-save) — `LeafRecheckShape2` was already
                    %% non-leaf before this call, so
                    %% `superclasses_losing_leaf_status/1` must find nothing
                    %% and no second re-check/announcement fires.
                    subscribe_self_to_reload_check(),
                    {ok, _, _State3} = beamtalk_repl_loader:handle_load(CirclePath, State2),
                    wait_for_leaf_change_worker(),

                    ?assertError(
                        timeout_waiting_for_reload_check_announcement,
                        receive_reload_check_announcement()
                    )
                end)
            ]
        end}}.
