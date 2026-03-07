%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc EUnit tests for beamtalk_transcript_stream_primitives (BT-1173).
%%%
%%% **DDD Context:** REPL Session Context
%%%
%%% This module is a compatibility shim delegating to beamtalk_transcript_stream.
%%% Tests verify delegation works correctly for all supported selectors.

-module(beamtalk_transcript_stream_primitives_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%%% ============================================================================
%%% dispatch/3 — show: delegates and returns Self
%%% ============================================================================

dispatch_show_returns_self_test() ->
    Self = make_ref(),
    Result = beamtalk_transcript_stream_primitives:dispatch('show:', [<<"hello">>], Self),
    ?assertEqual(Self, Result).

dispatch_show_integer_returns_self_test() ->
    Self = make_ref(),
    Result = beamtalk_transcript_stream_primitives:dispatch('show:', [42], Self),
    ?assertEqual(Self, Result).

%%% ============================================================================
%%% dispatch/3 — cr delegates and returns Self
%%% ============================================================================

dispatch_cr_returns_self_test() ->
    Self = make_ref(),
    Result = beamtalk_transcript_stream_primitives:dispatch(cr, [], Self),
    ?assertEqual(Self, Result).

%%% ============================================================================
%%% dispatch/3 — recent returns list
%%% ============================================================================

dispatch_recent_returns_list_test() ->
    Self = make_ref(),
    Result = beamtalk_transcript_stream_primitives:dispatch(recent, [], Self),
    ?assert(is_list(Result)).

%%% ============================================================================
%%% dispatch/3 — clear delegates and returns Self
%%% ============================================================================

dispatch_clear_returns_self_test() ->
    Self = make_ref(),
    Result = beamtalk_transcript_stream_primitives:dispatch(clear, [], Self),
    ?assertEqual(Self, Result).

%%% ============================================================================
%%% dispatch/3 — unknown selector raises DNU
%%% ============================================================================

dispatch_unknown_selector_raises_dnu_test() ->
    Self = make_ref(),
    ?assertError(
        #{'$beamtalk_class' := _, error := #beamtalk_error{kind = does_not_understand}},
        beamtalk_transcript_stream_primitives:dispatch(unknown_selector, [], Self)
    ).
