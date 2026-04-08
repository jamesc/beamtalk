%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_compiler_app).

-behaviour(application).

%%% **DDD Context:** Compilation (Anti-Corruption Layer)

-moduledoc """
OTP application callback for the Beamtalk compiler (ADR 0022, Phase 1).

Starts the compiler supervision tree.
""".

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    beamtalk_compiler_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
