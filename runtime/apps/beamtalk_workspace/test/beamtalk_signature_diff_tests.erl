%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_signature_diff_tests).

-moduledoc "Unit tests for beamtalk_signature_diff (ADR 0105 Phase 1, BT-2777).".

-include_lib("eunit/include/eunit.hrl").

sig(ReturnType, ParamTypes) ->
    #{return_type => ReturnType, param_types => ParamTypes}.

%% Identical signatures classify as no_op — nothing worth a re-check finding.
identical_signatures_is_no_op_test() ->
    Sig = sig(<<"Integer">>, [<<"Integer">>]),
    ?assertEqual(no_op, beamtalk_signature_diff:diff(Sig, Sig)).

%% A changed return type is a signature_change.
changed_return_type_is_signature_change_test() ->
    Old = sig(<<"Integer">>, []),
    New = sig(<<"String">>, []),
    ?assertEqual(signature_change, beamtalk_signature_diff:diff(Old, New)).

%% A changed parameter type is a signature_change, even with the same return type.
changed_param_type_is_signature_change_test() ->
    Old = sig(<<"Object">>, [<<"Integer">>]),
    New = sig(<<"Object">>, [<<"String">>]),
    ?assertEqual(signature_change, beamtalk_signature_diff:diff(Old, New)).

%% Adding/removing a parameter (arity change) is a signature_change.
changed_arity_is_signature_change_test() ->
    Old = sig(<<"Object">>, [<<"Integer">>]),
    New = sig(<<"Object">>, [<<"Integer">>, <<"String">>]),
    ?assertEqual(signature_change, beamtalk_signature_diff:diff(Old, New)).

%% New =:= removed is always classified as removal, regardless of Old.
removed_selector_is_removal_test() ->
    Old = sig(<<"Integer">>, []),
    ?assertEqual(removal, beamtalk_signature_diff:diff(Old, removed)).

%% Even a prior removal followed by another `removed` classifies as removal
%% (diff/2 does not special-case "was already gone" — it just reports what New says).
double_removal_is_still_removal_test() ->
    ?assertEqual(removal, beamtalk_signature_diff:diff(removed, removed)).

%% No previous generation (never patched, unresolvable original) is a no_op —
%% there is nothing to compare against, so no finding is manufactured.
no_previous_generation_is_no_op_test() ->
    New = sig(<<"Integer">>, []),
    ?assertEqual(no_op, beamtalk_signature_diff:diff(undefined, New)).

%% A method reappearing after a removal (Old =:= removed, New a real signature)
%% is a signature_change — the interface is different from what it was.
readded_after_removal_is_signature_change_test() ->
    New = sig(<<"Integer">>, []),
    ?assertEqual(signature_change, beamtalk_signature_diff:diff(removed, New)).
