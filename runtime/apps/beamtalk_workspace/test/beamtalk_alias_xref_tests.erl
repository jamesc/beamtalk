%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_alias_xref_tests).

-moduledoc """
Unit tests for beamtalk_alias_xref — the alias-name → dependent-class index
(ADR 0108, BT-2899).

Covers uncovered paths from the 2026-07-16 coverage report (CI run 29535121541,
54.5% line coverage):
* `clear/0` gen_server call handler (line 173)
* Catch-all `handle_call` unknown-request guard (line 175)
* `handle_cast` noreply catch-all (line 201)
* `remove_dependent/3` internal helper (lines 225-232): only reached when a class
  re-registers with a *smaller* alias set; integration tests only ever grow the set
* `sets:is_empty(NewAliasSet) = true` branch (lines 196-197): reached when a class
  re-registers with an empty alias list
* `noproc` defensive catches in `register_class/2` (line 125), `dependents_of/1`
  (lines 141-142), and `clear/0` (lines 148-149)

All tests run against a freshly-started gen_server with no compiler, runtime, or
workspace dependencies.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Fixture: server running
%%====================================================================

alias_xref_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun empty_index_returns_empty/1,
        fun register_and_lookup_single_alias/1,
        fun multiple_classes_same_alias_sorted/1,
        fun multiple_aliases_same_class/1,
        fun clear_resets_index/1,
        fun whole_set_replacement_removes_stale_edges/1,
        fun reregister_with_empty_list_removes_class/1,
        fun unknown_handle_call_returns_error/1,
        fun unknown_handle_cast_is_safe/1
    ]}.

setup() ->
    case whereis(beamtalk_alias_xref) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    {ok, _} = beamtalk_alias_xref:start_link(),
    ok.

cleanup(_) ->
    case whereis(beamtalk_alias_xref) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end.

%%====================================================================
%% Tests (server running)
%%====================================================================

empty_index_returns_empty(_) ->
    [?_assertEqual([], beamtalk_alias_xref:dependents_of(<<"UnknownAlias">>))].

register_and_lookup_single_alias(_) ->
    %% `register_class/2` is an async cast; `dependents_of/1` is a sync call
    %% processed after the cast in FIFO mailbox order — no explicit flush needed.
    beamtalk_alias_xref:register_class(<<"MyClass">>, [<<"MyAlias">>]),
    [?_assertEqual([<<"MyClass">>], beamtalk_alias_xref:dependents_of(<<"MyAlias">>))].

multiple_classes_same_alias_sorted(_) ->
    %% Both casts are queued before any sync call — gen_server processes them in order.
    beamtalk_alias_xref:register_class(<<"Zebra">>, [<<"Shape">>]),
    beamtalk_alias_xref:register_class(<<"Apple">>, [<<"Shape">>]),
    [
        ?_assertEqual(
            [<<"Apple">>, <<"Zebra">>],
            beamtalk_alias_xref:dependents_of(<<"Shape">>)
        )
    ].

multiple_aliases_same_class(_) ->
    beamtalk_alias_xref:register_class(<<"Poly">>, [<<"AliasA">>, <<"AliasB">>]),
    [
        ?_assertEqual([<<"Poly">>], beamtalk_alias_xref:dependents_of(<<"AliasA">>)),
        ?_assertEqual([<<"Poly">>], beamtalk_alias_xref:dependents_of(<<"AliasB">>))
    ].

clear_resets_index(_) ->
    %% Exercises handle_call(clear, ...) path (line 173): the full index is wiped.
    beamtalk_alias_xref:register_class(<<"ClearMe">>, [<<"Alias1">>]),
    beamtalk_alias_xref:clear(),
    [?_assertEqual([], beamtalk_alias_xref:dependents_of(<<"Alias1">>))].

whole_set_replacement_removes_stale_edges(_) ->
    %% Exercises remove_dependent/3 (lines 225-232): re-registering a class with a
    %% *smaller* alias set removes it from the aliases it no longer references.
    beamtalk_alias_xref:register_class(<<"Dep">>, [<<"AlphaAlias">>, <<"BetaAlias">>]),
    beamtalk_alias_xref:register_class(<<"Dep">>, [<<"BetaAlias">>]),
    [
        ?_assertEqual([], beamtalk_alias_xref:dependents_of(<<"AlphaAlias">>)),
        ?_assertEqual([<<"Dep">>], beamtalk_alias_xref:dependents_of(<<"BetaAlias">>))
    ].

reregister_with_empty_list_removes_class(_) ->
    %% Exercises the sets:is_empty(NewAliasSet) = true branch (lines 196-197):
    %% maps:remove is called on class_to_aliases, purging the class entirely.
    beamtalk_alias_xref:register_class(<<"GoAway">>, [<<"TransientAlias">>]),
    beamtalk_alias_xref:register_class(<<"GoAway">>, []),
    [?_assertEqual([], beamtalk_alias_xref:dependents_of(<<"TransientAlias">>))].

unknown_handle_call_returns_error(_) ->
    %% Exercises the catch-all handle_call/3 clause (line 175).
    Result = gen_server:call(beamtalk_alias_xref, totally_unknown_request),
    [?_assertEqual({error, unknown_request}, Result)].

unknown_handle_cast_is_safe(_) ->
    %% Exercises the catch-all handle_cast/2 clause (line 201): unknown casts
    %% must not crash the server.
    gen_server:cast(beamtalk_alias_xref, totally_unknown_cast),
    %% Sync barrier: a call after the cast is processed only after the cast completes.
    _ = beamtalk_alias_xref:dependents_of(<<"FlushBarrier">>),
    [?_assert(is_process_alive(whereis(beamtalk_alias_xref)))].

%%====================================================================
%% Tests (server NOT running — noproc defensive paths)
%%====================================================================

dependents_of_when_not_running_test() ->
    %% Exercises exit:{noproc, _} catch in dependents_of/1 (lines 141-142).
    stop_server_if_running(),
    ?assertEqual([], beamtalk_alias_xref:dependents_of(<<"AnyAlias">>)).

clear_when_not_running_test() ->
    %% Exercises exit:{noproc, _} catch in clear/0 (lines 148-149).
    stop_server_if_running(),
    ?assertEqual(ok, beamtalk_alias_xref:clear()).

register_class_when_not_running_test() ->
    %% Validates that register_class/2 returns ok when the server is not running.
    %% gen_server:cast/2 already swallows send errors internally in OTP, so the
    %% outer try/catch in register_class/2 is a belt-and-suspenders guard for
    %% older/unusual paths; the function returns ok via its trailing `ok` (line 127).
    stop_server_if_running(),
    ?assertEqual(ok, beamtalk_alias_xref:register_class(<<"Ghost">>, [<<"A">>])).

%%====================================================================
%% Internal helpers
%%====================================================================

stop_server_if_running() ->
    case whereis(beamtalk_alias_xref) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end.
