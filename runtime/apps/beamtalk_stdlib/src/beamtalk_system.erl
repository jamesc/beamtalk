%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc System class implementation — OS environment, platform, and process info.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% System provides class-side methods for accessing environment variables,
%%% detecting the operating system and architecture, and querying process info.
%%%
%%% ## Methods
%%%
%%% | Selector            | Description                              |
%%% |---------------------|------------------------------------------|
%%% | `getEnv:`           | Read environment variable (nil if unset) |
%%% | `getEnv:default:`   | Read env var with fallback               |
%%% | `setEnv:value:`     | Set environment variable                 |
%%% | `unsetEnv:`         | Remove environment variable              |
%%% | `osPlatform`        | OS name: linux, darwin, win32            |
%%% | `osFamily`          | OS family: unix or win32                 |
%%% | `architecture`      | System architecture string               |
%%% | `hostname`          | Machine hostname                         |
%%% | `erlangVersion`     | OTP version string                       |
%%% | `pid`               | OS process ID                            |
%%% | `uniqueId`          | Unique positive monotonic integer         |

-module(beamtalk_system).

-export(['getEnv:'/1, 'getEnv:default:'/2]).
-export(['setEnv:value:'/2, 'unsetEnv:'/1]).
-export([osPlatform/0, osFamily/0, architecture/0, hostname/0]).
-export([erlangVersion/0, pid/0, uniqueId/0]).
-export([getEnv/1, getEnv/2]).
-export([setEnv/2, unsetEnv/1]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Read an environment variable. Returns nil if not set.
-spec 'getEnv:'(binary()) -> binary() | 'nil'.
'getEnv:'(Name) when is_binary(Name) ->
    case os:getenv(binary_to_list(Name)) of
        false -> nil;
        Value -> list_to_binary(Value)
    end;
'getEnv:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'System'),
    Error1 = beamtalk_error:with_selector(Error0, 'getEnv:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Read an environment variable with a default fallback.
-spec 'getEnv:default:'(binary(), binary()) -> binary().
'getEnv:default:'(Name, Default) when is_binary(Name), is_binary(Default) ->
    case os:getenv(binary_to_list(Name)) of
        false -> Default;
        Value -> list_to_binary(Value)
    end;
'getEnv:default:'(Name, _Default) when not is_binary(Name) ->
    Error0 = beamtalk_error:new(type_error, 'System'),
    Error1 = beamtalk_error:with_selector(Error0, 'getEnv:default:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Name argument must be a String">>),
    beamtalk_error:raise(Error2);
'getEnv:default:'(_Name, _Default) ->
    Error0 = beamtalk_error:new(type_error, 'System'),
    Error1 = beamtalk_error:with_selector(Error0, 'getEnv:default:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Default argument must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Set an environment variable. Returns true.
-spec 'setEnv:value:'(binary(), binary()) -> 'true'.
'setEnv:value:'(Name, Value) when is_binary(Name), is_binary(Value) ->
    os:putenv(binary_to_list(Name), binary_to_list(Value)),
    true;
'setEnv:value:'(Name, _Value) when not is_binary(Name) ->
    Error0 = beamtalk_error:new(type_error, 'System'),
    Error1 = beamtalk_error:with_selector(Error0, 'setEnv:value:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Name argument must be a String">>),
    beamtalk_error:raise(Error2);
'setEnv:value:'(_Name, _Value) ->
    Error0 = beamtalk_error:new(type_error, 'System'),
    Error1 = beamtalk_error:with_selector(Error0, 'setEnv:value:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Value argument must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Remove an environment variable. Returns true.
-spec 'unsetEnv:'(binary()) -> 'true'.
'unsetEnv:'(Name) when is_binary(Name) ->
    os:unsetenv(binary_to_list(Name)),
    true;
'unsetEnv:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'System'),
    Error1 = beamtalk_error:with_selector(Error0, 'unsetEnv:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Return the OS platform name.
%% Maps Erlang os:type() to familiar platform strings.
-spec osPlatform() -> binary().
osPlatform() ->
    {_Family, Name} = os:type(),
    platform_name(Name).

%% @doc Return the OS family: unix or win32.
-spec osFamily() -> binary().
osFamily() ->
    {Family, _Name} = os:type(),
    atom_to_binary(Family, utf8).

%% @doc Return the system architecture string.
-spec architecture() -> binary().
architecture() ->
    list_to_binary(erlang:system_info(system_architecture)).

%% @doc Return the machine hostname.
-spec hostname() -> binary().
hostname() ->
    case catch inet:gethostname() of
        {ok, Hostname} ->
            list_to_binary(Hostname);
        {error, Reason} ->
            ?LOG_ERROR("Failed to resolve hostname", #{
                module => ?MODULE,
                function => hostname,
                reason => Reason
            }),
            Error0 = beamtalk_error:new(runtime_error, 'System'),
            Error1 = beamtalk_error:with_selector(Error0, hostname),
            Error2 = beamtalk_error:with_details(Error1, #{reason => Reason}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Could not resolve hostname from OS">>),
            beamtalk_error:raise(Error3);
        {'EXIT', Reason} ->
            ?LOG_ERROR("Hostname lookup crashed", #{
                module => ?MODULE,
                function => hostname,
                reason => Reason
            }),
            Error0 = beamtalk_error:new(runtime_error, 'System'),
            Error1 = beamtalk_error:with_selector(Error0, hostname),
            Error2 = beamtalk_error:with_details(Error1, #{reason => Reason}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Could not resolve hostname from OS">>),
            beamtalk_error:raise(Error3)
    end.

%% @doc Return the OTP version string.
-spec erlangVersion() -> binary().
erlangVersion() ->
    list_to_binary(erlang:system_info(otp_release)).

%% @doc Return the OS process ID as an integer.
-spec pid() -> integer().
pid() ->
    list_to_integer(os:getpid()).

%% @doc Return a unique positive monotonic integer.
%% Each call returns a value strictly greater than any previous call.
-spec uniqueId() -> pos_integer().
uniqueId() ->
    erlang:unique_integer([positive, monotonic]).

%% @doc Shims for (Erlang beamtalk_system) FFI dispatch.
%% The Erlang FFI proxy (beamtalk_erlang_proxy) maps any keyword selector to
%% its first keyword before calling direct_call/3, so `getEnv:` → `getEnv`
%% and `getEnv:default:` → `getEnv`. These shims exist so the Beamtalk
%% selectors `(Erlang beamtalk_system) getEnv: name` and
%% `(Erlang beamtalk_system) getEnv: name default: fallback` dispatch to
%% getEnv/1 and getEnv/2 respectively, delegating to the canonical forms.
-spec getEnv(binary()) -> binary() | 'nil'.
getEnv(Name) -> 'getEnv:'(Name).

-spec getEnv(binary(), binary()) -> binary().
getEnv(Name, Default) -> 'getEnv:default:'(Name, Default).

-spec setEnv(binary(), binary()) -> 'true'.
setEnv(Name, Value) -> 'setEnv:value:'(Name, Value).

-spec unsetEnv(binary()) -> 'true'.
unsetEnv(Name) -> 'unsetEnv:'(Name).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private Map os:type() Name atom to platform string.
-spec platform_name(atom()) -> binary().
platform_name(darwin) -> <<"darwin">>;
platform_name(linux) -> <<"linux">>;
platform_name(nt) -> <<"win32">>;
platform_name(Other) -> atom_to_binary(Other, utf8).
