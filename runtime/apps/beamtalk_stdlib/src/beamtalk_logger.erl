%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Logger class implementation — structured logging via Erlang logger.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Thin wrapper around Erlang's `logger` module providing idiomatic
%%% Beamtalk logging with optional structured metadata.
%%%
%%% ## Methods
%%%
%%% | Selector           | Description                              |
%%% |--------------------|------------------------------------------|
%%% | `info:`            | Log at info level                        |
%%% | `info:metadata:`   | Log at info with metadata                |
%%% | `warn:`            | Log at warning level                     |
%%% | `warn:metadata:`   | Log at warning with metadata             |
%%% | `error:`           | Log at error level                       |
%%% | `error:metadata:`  | Log at error with metadata               |
%%% | `debug:`           | Log at debug level                       |
%%% | `debug:metadata:`  | Log at debug with metadata               |
%%% | `setLevel:`        | Set the primary log level                |

-module(beamtalk_logger).

-compile({no_auto_import, [error/1, error/2]}).

-export(['info:'/1, 'info:metadata:'/2]).
-export(['warn:'/1, 'warn:metadata:'/2]).
-export(['error:'/1, 'error:metadata:'/2]).
-export(['debug:'/1, 'debug:metadata:'/2]).
-export(['setLevel:'/1]).
%% FFI shims (keyword selector → first keyword dispatch)
-export([info/1, info/2]).
-export([warn/1, warn/2]).
-export([error/1, error/2]).
-export([debug/1, debug/2]).
-export([setLevel/1]).
%% FFI shims for error: (avoids Object#error: intrinsic in codegen)
-export([logError/1, logError/2]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Log a message at info level.
-spec 'info:'(binary()) -> 'nil'.
'info:'(Msg) when is_binary(Msg) ->
    ?LOG_INFO(Msg),
    nil;
'info:'(_) ->
    raise_type_error('info:', <<"Message must be a String">>).

%% @doc Log a message at info level with metadata.
-spec 'info:metadata:'(binary(), map()) -> 'nil'.
'info:metadata:'(Msg, Meta) when is_binary(Msg), is_map(Meta) ->
    ?LOG_INFO(Meta#{msg => Msg}),
    nil;
'info:metadata:'(Msg, _Meta) when not is_binary(Msg) ->
    raise_type_error('info:metadata:', <<"Message must be a String">>);
'info:metadata:'(_Msg, _Meta) ->
    raise_type_error('info:metadata:', <<"Metadata must be a Dictionary">>).

%% @doc Log a message at warning level.
-spec 'warn:'(binary()) -> 'nil'.
'warn:'(Msg) when is_binary(Msg) ->
    ?LOG_WARNING(Msg),
    nil;
'warn:'(_) ->
    raise_type_error('warn:', <<"Message must be a String">>).

%% @doc Log a message at warning level with metadata.
-spec 'warn:metadata:'(binary(), map()) -> 'nil'.
'warn:metadata:'(Msg, Meta) when is_binary(Msg), is_map(Meta) ->
    ?LOG_WARNING(Meta#{msg => Msg}),
    nil;
'warn:metadata:'(Msg, _Meta) when not is_binary(Msg) ->
    raise_type_error('warn:metadata:', <<"Message must be a String">>);
'warn:metadata:'(_Msg, _Meta) ->
    raise_type_error('warn:metadata:', <<"Metadata must be a Dictionary">>).

%% @doc Log a message at error level.
-spec 'error:'(binary()) -> 'nil'.
'error:'(Msg) when is_binary(Msg) ->
    ?LOG_ERROR(Msg),
    nil;
'error:'(_) ->
    raise_type_error('error:', <<"Message must be a String">>).

%% @doc Log a message at error level with metadata.
-spec 'error:metadata:'(binary(), map()) -> 'nil'.
'error:metadata:'(Msg, Meta) when is_binary(Msg), is_map(Meta) ->
    ?LOG_ERROR(Meta#{msg => Msg}),
    nil;
'error:metadata:'(Msg, _Meta) when not is_binary(Msg) ->
    raise_type_error('error:metadata:', <<"Message must be a String">>);
'error:metadata:'(_Msg, _Meta) ->
    raise_type_error('error:metadata:', <<"Metadata must be a Dictionary">>).

%% @doc Log a message at debug level.
-spec 'debug:'(binary()) -> 'nil'.
'debug:'(Msg) when is_binary(Msg) ->
    ?LOG_DEBUG(Msg),
    nil;
'debug:'(_) ->
    raise_type_error('debug:', <<"Message must be a String">>).

%% @doc Log a message at debug level with metadata.
-spec 'debug:metadata:'(binary(), map()) -> 'nil'.
'debug:metadata:'(Msg, Meta) when is_binary(Msg), is_map(Meta) ->
    ?LOG_DEBUG(Meta#{msg => Msg}),
    nil;
'debug:metadata:'(Msg, _Meta) when not is_binary(Msg) ->
    raise_type_error('debug:metadata:', <<"Message must be a String">>);
'debug:metadata:'(_Msg, _Meta) ->
    raise_type_error('debug:metadata:', <<"Metadata must be a Dictionary">>).

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
%%% FFI Shims
%%% ============================================================================

%% @doc Shims for (Erlang beamtalk_logger) FFI dispatch.
-spec info(binary()) -> 'nil'.
info(Msg) -> 'info:'(Msg).

-spec info(binary(), map()) -> 'nil'.
info(Msg, Meta) -> 'info:metadata:'(Msg, Meta).

-spec warn(binary()) -> 'nil'.
warn(Msg) -> 'warn:'(Msg).

-spec warn(binary(), map()) -> 'nil'.
warn(Msg, Meta) -> 'warn:metadata:'(Msg, Meta).

-spec error(binary()) -> 'nil'.
error(Msg) -> 'error:'(Msg).

-spec error(binary(), map()) -> 'nil'.
error(Msg, Meta) -> 'error:metadata:'(Msg, Meta).

-spec debug(binary()) -> 'nil'.
debug(Msg) -> 'debug:'(Msg).

-spec debug(binary(), map()) -> 'nil'.
debug(Msg, Meta) -> 'debug:metadata:'(Msg, Meta).

-spec setLevel(atom()) -> 'nil'.
setLevel(Level) -> 'setLevel:'(Level).

%% @doc FFI shims for error: — named logError to avoid Object#error: intrinsic
%% in the Beamtalk codegen dispatch chain.
-spec logError(binary()) -> 'nil'.
logError(Msg) -> 'error:'(Msg).

-spec logError(binary(), map()) -> 'nil'.
logError(Msg, Meta) -> 'error:metadata:'(Msg, Meta).

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
