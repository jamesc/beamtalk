%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Compatibility shim — delegates to beamtalk_transcript_stream (BT-1163).
%%%
%%% **DDD Context:** REPL Session Context
%%%
%%% This module was merged into beamtalk_transcript_stream (BT-1163).
%%% It exists only as a delegation shim for the compiled
%%% bt@stdlib@transcript_stream actor, which calls dispatch/3 here.

-module(beamtalk_transcript_stream_primitives).

-export([dispatch/3]).

%% @doc Delegate primitive dispatch to beamtalk_transcript_stream.
-spec dispatch(atom(), list(), term()) -> term().
dispatch(Selector, Args, Self) ->
    beamtalk_transcript_stream:dispatch(Selector, Args, Self).
