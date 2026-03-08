%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc OS class implementation — shell command execution.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% OS provides class-side methods for executing shell commands
%%% and capturing their output as a String.
%%%
%%% ## Methods
%%%
%%% | Selector  | Description                              |
%%% |-----------|------------------------------------------|
%%% | `run:`    | Execute shell command, return trimmed stdout |

-module(beamtalk_os).

-export(['run:'/1, run/1]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Execute a shell command and return its trimmed stdout as a binary.
-spec 'run:'(binary()) -> binary().
'run:'(Cmd) when is_binary(Cmd) ->
    Result = os:cmd(binary_to_list(Cmd)),
    Bin = list_to_binary(Result),
    string:trim(Bin, trailing);
'run:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'OS'),
    Error1 = beamtalk_error:with_selector(Error0, 'run:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc FFI shim for (Erlang beamtalk_os) run: cmd dispatch.
-spec run(binary()) -> binary().
run(Cmd) -> 'run:'(Cmd).
