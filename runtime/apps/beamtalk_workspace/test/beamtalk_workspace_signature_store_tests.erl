%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_workspace_signature_store_tests).

-moduledoc """
Unit tests for beamtalk_workspace_signature_store (ADR 0105 Phase 1, BT-2777).

Covers:
- capture-before-install ordering (the store update itself, independent of any
  install call — capture/4 is what the install hook calls before code:load_binary)
- repeated-edit generation chaining (generation N's "previous" is exactly
  what generation N-1 recorded, not generation 0 or wiped class state)
- diff classification threaded through from beamtalk_signature_diff
- removal + re-add
- rollback/4 undoing a capture whose install/removal subsequently failed
  (including that it only undoes the single capture it's paired with)
- clear/0 resets the session
- __beamtalk_meta/0 seeding for a never-patched selector (both the "no meta
  exported" degrade and the real read path via beamtalk_signature_store_fixture)

The end-to-end thread from a real install (`beamtalk_repl_loader:install_method/9`)
through to a correctly-populated store entry is covered in
`beamtalk_repl_loader_tests.erl`, not here — this module tests the store's own
API contract in isolation.
""".

-include_lib("eunit/include/eunit.hrl").

-define(TABLE, beamtalk_class_metadata).

sig(ReturnType, ParamTypes) ->
    #{return_type => ReturnType, param_types => ParamTypes}.

%%====================================================================
%% Fixtures
%%====================================================================

setup() ->
    {ok, Pid} = beamtalk_workspace_signature_store:start_link(),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true ->
            Ref = monitor(process, Pid),
            unlink(Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 1000 -> ok
            end;
        false ->
            ok
    end.

store_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun first_capture_has_no_previous_and_is_no_op/1,
        fun repeated_capture_chains_generations/1,
        fun identical_capture_is_no_op/1,
        fun previous_does_not_mutate_store/1,
        fun capture_removal_then_readd/1,
        fun clear_resets_the_session/1,
        fun different_selectors_are_independent/1,
        fun different_sides_are_independent/1,
        fun rollback_to_a_signature_restores_it_as_previous/1,
        fun rollback_to_undefined_removes_the_key/1,
        fun rollback_after_a_chain_only_undoes_the_last_capture/1
    ]}.

%%====================================================================
%% Generation handling (core AC coverage)
%%====================================================================

%% Nothing recorded yet, and the class isn't registered — no baseline exists,
%% so the classification is no_op (never a manufactured signature_change).
first_capture_has_no_previous_and_is_no_op(_Pid) ->
    New = sig(<<"Integer">>, []),
    {Prev, Classification} =
        beamtalk_workspace_signature_store:capture(<<"Unregistered">>, <<"foo">>, instance, New),
    [
        ?_assertEqual(undefined, Prev),
        ?_assertEqual(no_op, Classification)
    ].

%% Three edits to the same selector: each capture's "previous" must be exactly
%% the immediately-prior generation, not generation 0 and not wiped state.
repeated_capture_chains_generations(_Pid) ->
    Sig1 = sig(<<"Integer">>, []),
    Sig2 = sig(<<"String">>, []),
    Sig3 = sig(<<"Object">>, [<<"Integer">>]),
    {Prev1, C1} =
        beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"getCount">>, instance, Sig1),
    {Prev2, C2} =
        beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"getCount">>, instance, Sig2),
    {Prev3, C3} =
        beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"getCount">>, instance, Sig3),
    [
        %% Generation 1: no baseline.
        ?_assertEqual(undefined, Prev1),
        ?_assertEqual(no_op, C1),
        %% Generation 2: previous is generation 1 (Sig1), not undefined.
        ?_assertEqual(Sig1, Prev2),
        ?_assertEqual(signature_change, C2),
        %% Generation 3: previous is generation 2 (Sig2) — NOT Sig1 (generation 1)
        %% and not the wiped/absent class state. This is the chaining property
        %% the ADR calls out explicitly.
        ?_assertEqual(Sig2, Prev3),
        ?_assertEqual(signature_change, C3)
    ].

%% Capturing the same signature twice in a row is a no_op — nothing to report.
identical_capture_is_no_op(_Pid) ->
    Sig = sig(<<"Integer">>, [<<"String">>]),
    {_, _} = beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, Sig),
    {Prev, Classification} =
        beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, Sig),
    [
        ?_assertEqual(Sig, Prev),
        ?_assertEqual(no_op, Classification)
    ].

%% previous/3 is read-only: calling it repeatedly must not change what the
%% next capture/4 sees as "previous".
previous_does_not_mutate_store(_Pid) ->
    Sig1 = sig(<<"Integer">>, []),
    {_, _} = beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, Sig1),
    P1 = beamtalk_workspace_signature_store:previous(<<"Counter">>, <<"m">>, instance),
    P2 = beamtalk_workspace_signature_store:previous(<<"Counter">>, <<"m">>, instance),
    Sig2 = sig(<<"String">>, []),
    {Prev3, _} =
        beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, Sig2),
    [
        ?_assertEqual(Sig1, P1),
        ?_assertEqual(Sig1, P2),
        ?_assertEqual(Sig1, Prev3)
    ].

%% A removal records `removed`; a subsequent re-add diffs against `removed`
%% (a signature_change — the interface is different from "gone").
capture_removal_then_readd(_Pid) ->
    Sig1 = sig(<<"Integer">>, []),
    {_, _} = beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, Sig1),
    {PrevAtRemoval, RemovalClass} =
        beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, removed),
    Sig2 = sig(<<"String">>, []),
    {PrevAtReadd, ReaddClass} =
        beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, Sig2),
    [
        ?_assertEqual(Sig1, PrevAtRemoval),
        ?_assertEqual(removal, RemovalClass),
        ?_assertEqual(removed, PrevAtReadd),
        ?_assertEqual(signature_change, ReaddClass)
    ].

%% clear/0 drops every recorded generation — the next capture behaves as if
%% this were a fresh workspace session.
clear_resets_the_session(_Pid) ->
    Sig1 = sig(<<"Integer">>, []),
    {_, _} = beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, Sig1),
    ok = beamtalk_workspace_signature_store:clear(),
    {Prev, Classification} =
        beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, Sig1),
    [
        ?_assertEqual(undefined, Prev),
        ?_assertEqual(no_op, Classification)
    ].

%% Two different selectors on the same class keep independent generation chains.
different_selectors_are_independent(_Pid) ->
    SigA = sig(<<"Integer">>, []),
    SigB = sig(<<"String">>, []),
    {_, _} = beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"a">>, instance, SigA),
    {PrevB, ClassB} =
        beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"b">>, instance, SigB),
    [
        ?_assertEqual(undefined, PrevB),
        ?_assertEqual(no_op, ClassB)
    ].

%% Instance-side and class-side methods with the same selector name on the
%% same class are independent generation chains (Side is part of the key).
different_sides_are_independent(_Pid) ->
    InstanceSig = sig(<<"Integer">>, []),
    ClassSig = sig(<<"Counter">>, []),
    {_, _} =
        beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"new">>, instance, InstanceSig),
    {PrevClassSide, ClassificationClassSide} =
        beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"new">>, class, ClassSig),
    [
        ?_assertEqual(undefined, PrevClassSide),
        ?_assertEqual(no_op, ClassificationClassSide)
    ].

%%====================================================================
%% rollback/4 (ADR 0105 Phase 1, BT-2777 — undo a failed install/removal)
%%====================================================================

%% Rolling back to a prior signature restores it as "previous" for the next
%% capture — as if the failed capture had never happened.
rollback_to_a_signature_restores_it_as_previous(_Pid) ->
    Sig1 = sig(<<"Integer">>, []),
    FailedSig = sig(<<"String">>, []),
    {_, _} = beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, Sig1),
    %% Simulate: a second edit is captured, but its install then fails.
    {PrevAtFailedCapture, _} =
        beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, FailedSig),
    ok = beamtalk_workspace_signature_store:rollback(
        <<"Counter">>, <<"m">>, instance, PrevAtFailedCapture
    ),
    %% The next real capture must see Sig1 as previous, not FailedSig.
    Sig2 = sig(<<"Object">>, []),
    {Prev, Classification} =
        beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, Sig2),
    [
        ?_assertEqual(Sig1, PrevAtFailedCapture),
        ?_assertEqual(Sig1, Prev),
        ?_assertEqual(signature_change, Classification)
    ].

%% Rolling back with `undefined` (the failed capture was itself the first-ever
%% capture for this selector) removes the key entirely — the next capture
%% re-seeds from scratch rather than treating the failed attempt as "previous".
rollback_to_undefined_removes_the_key(_Pid) ->
    FailedSig = sig(<<"String">>, []),
    {PrevAtFailedCapture, _} =
        beamtalk_workspace_signature_store:capture(
            <<"NeverPatched">>, <<"m">>, instance, FailedSig
        ),
    ok = beamtalk_workspace_signature_store:rollback(
        <<"NeverPatched">>, <<"m">>, instance, PrevAtFailedCapture
    ),
    Sig1 = sig(<<"Object">>, []),
    {Prev, Classification} =
        beamtalk_workspace_signature_store:capture(<<"NeverPatched">>, <<"m">>, instance, Sig1),
    [
        ?_assertEqual(undefined, PrevAtFailedCapture),
        %% Same outcome as a genuine first-ever capture — the failed attempt
        %% left no trace.
        ?_assertEqual(undefined, Prev),
        ?_assertEqual(no_op, Classification)
    ].

%% A rollback only undoes the single capture it corresponds to — it must not
%% reach further back into the generation chain than the one write it's
%% paired with.
rollback_after_a_chain_only_undoes_the_last_capture(_Pid) ->
    Sig1 = sig(<<"Integer">>, []),
    Sig2 = sig(<<"String">>, []),
    FailedSig3 = sig(<<"Object">>, []),
    {_, _} = beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, Sig1),
    {_, _} = beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, Sig2),
    {PrevAtFailedCapture, _} =
        beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, FailedSig3),
    ok = beamtalk_workspace_signature_store:rollback(
        <<"Counter">>, <<"m">>, instance, PrevAtFailedCapture
    ),
    %% Rolled back to Sig2 (generation 2), not Sig1 (generation 1).
    Sig4 = sig(<<"Nil">>, []),
    {Prev, _} = beamtalk_workspace_signature_store:capture(<<"Counter">>, <<"m">>, instance, Sig4),
    [
        ?_assertEqual(Sig2, PrevAtFailedCapture),
        ?_assertEqual(Sig2, Prev)
    ].

%%====================================================================
%% __beamtalk_meta/0 seeding (the never-patched-method baseline)
%%====================================================================

%% Registered class + module, but the module exports no __beamtalk_meta/0
%% (e.g. a plain Erlang module registered defensively) — seeding degrades to
%% `undefined` rather than crashing the capture hook.
seed_degrades_to_undefined_when_no_meta_exported_test() ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert('NoMetaClass', lists, [foo], 'Object'),
    try
        {ok, Pid} = beamtalk_workspace_signature_store:start_link(),
        try
            New = sig(<<"Integer">>, []),
            {Prev, Classification} =
                beamtalk_workspace_signature_store:capture(
                    <<"NoMetaClass">>, <<"foo">>, instance, New
                ),
            ?assertEqual(undefined, Prev),
            ?assertEqual(no_op, Classification)
        after
            cleanup(Pid)
        end
    after
        ets:delete(?TABLE, 'NoMetaClass')
    end.

%% A registered class whose module exports a realistic __beamtalk_meta/0 (via
%% beamtalk_signature_store_fixture) seeds the ORIGINAL, never-patched
%% signature — read once, on the first capture for that selector.
seed_reads_original_signature_from_meta_test() ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'FixtureClass', beamtalk_signature_store_fixture, [greet], 'Object'
    ),
    try
        {ok, Pid} = beamtalk_workspace_signature_store:start_link(),
        try
            New = sig(<<"String">>, [<<"String">>]),
            {Prev, Classification} =
                beamtalk_workspace_signature_store:capture(
                    <<"FixtureClass">>, <<"greet">>, instance, New
                ),
            ?assertEqual(sig(<<"Integer">>, [<<"String">>]), Prev),
            ?assertEqual(signature_change, Classification)
        after
            cleanup(Pid)
        end
    after
        ets:delete(?TABLE, 'FixtureClass')
    end.

%% An unannotated (return_type = none) never-patched method seeds as the
%% "Dynamic" sentinel, matching what the compiler side reports for an
%% unannotated method — so a freshly-compiled Dynamic-returning method
%% diffs as no_op against its own never-patched original, not a false positive.
seed_reads_dynamic_for_unannotated_meta_entry_test() ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'FixtureClass', beamtalk_signature_store_fixture, [untyped], 'Object'
    ),
    try
        {ok, Pid} = beamtalk_workspace_signature_store:start_link(),
        try
            New = sig(<<"Dynamic">>, []),
            {Prev, Classification} =
                beamtalk_workspace_signature_store:capture(
                    <<"FixtureClass">>, <<"untyped">>, instance, New
                ),
            ?assertEqual(sig(<<"Dynamic">>, []), Prev),
            ?assertEqual(no_op, Classification)
        after
            cleanup(Pid)
        end
    after
        ets:delete(?TABLE, 'FixtureClass')
    end.

%% Class-side selectors seed from `class_method_info`, not `method_info`.
seed_reads_class_side_from_class_method_info_test() ->
    beamtalk_class_metadata:new(),
    ok = beamtalk_class_metadata:insert(
        'FixtureClass', beamtalk_signature_store_fixture, ['new'], 'Object'
    ),
    try
        {ok, Pid} = beamtalk_workspace_signature_store:start_link(),
        try
            New = sig(<<"FixtureClass">>, []),
            {Prev, Classification} =
                beamtalk_workspace_signature_store:capture(
                    <<"FixtureClass">>, <<"new">>, class, New
                ),
            ?assertEqual(sig(<<"FixtureClass">>, []), Prev),
            ?assertEqual(no_op, Classification)
        after
            cleanup(Pid)
        end
    after
        ets:delete(?TABLE, 'FixtureClass')
    end.

%%====================================================================
%% meta_type_to_binary/1 (exported for TEST)
%%====================================================================

meta_type_to_binary_test_() ->
    [
        ?_assertEqual(<<"Dynamic">>, beamtalk_workspace_signature_store:meta_type_to_binary(none)),
        ?_assertEqual(
            <<"Integer">>, beamtalk_workspace_signature_store:meta_type_to_binary('Integer')
        ),
        ?_assertEqual(
            <<"T">>,
            beamtalk_workspace_signature_store:meta_type_to_binary({type_param, 'T', 0})
        ),
        ?_assertEqual(
            <<"Result(Integer, String)">>,
            beamtalk_workspace_signature_store:meta_type_to_binary(
                {generic, 'Result', ['Integer', 'String']}
            )
        )
    ].
