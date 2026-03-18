%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Logger class implementation — setLevel: via Erlang logger.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Log methods (info:, warn:, error:, debug: and their metadata: variants)
%%% are now compiler intrinsics (BT-1478) that generate inline `logger:log/3`
%%% calls with domain metadata at the call site. Only `setLevel:` remains as
%%% an Erlang FFI function since it requires real Erlang runtime calls.

-module(beamtalk_logger).

-export(['setLevel:'/1]).
%% FFI shim
-export([setLevel/1]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Set the primary log level.
%% @deprecated Use {@link beamtalk_system:logLevel/1} (`Beamtalk logLevel:`) instead.
-spec 'setLevel:'(atom()) -> 'nil'.
'setLevel:'(Level) when is_atom(Level) ->
    maybe_emit_deprecation_warning(),
    case logger:set_primary_config(level, Level) of
        ok ->
            nil;
        {error, _} ->
            Error0 = beamtalk_error:new(argument_error, 'Logger'),
            Error1 = beamtalk_error:with_selector(Error0, 'setLevel:'),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<"Level must be one of: #emergency, #alert, #critical, #error, #warning, #notice, #info, #debug">>
            ),
            beamtalk_error:raise(Error2)
    end;
'setLevel:'(_) ->
    raise_type_error('setLevel:', <<"Level must be a Symbol">>).

%%% ============================================================================
%%% FFI Shim
%%% ============================================================================

-spec setLevel(atom()) -> 'nil'.
setLevel(Level) -> 'setLevel:'(Level).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private Emit a deprecation warning for setLevel: on first call only.
-spec maybe_emit_deprecation_warning() -> ok.
maybe_emit_deprecation_warning() ->
    Key = beamtalk_logger_setlevel_deprecated,
    case persistent_term:get(Key, false) of
        true ->
            ok;
        false ->
            persistent_term:put(Key, true),
            ?LOG_WARNING("Logger setLevel: is deprecated. Use Beamtalk logLevel: instead.")
    end.

%% @private Raise a type error for the Logger class.
-spec raise_type_error(atom(), binary()) -> no_return().
raise_type_error(Selector, Hint) ->
    Error0 = beamtalk_error:new(type_error, 'Logger'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, Hint),
    beamtalk_error:raise(Error2).
