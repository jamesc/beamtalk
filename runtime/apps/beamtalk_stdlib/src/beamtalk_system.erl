%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_system).

%%% **DDD Context:** Object System Context

-moduledoc """
System class implementation — OS environment, platform, and process info.

System provides class-side methods for accessing environment variables,
detecting the operating system and architecture, and querying process info.

## Methods

| Selector            | Description                              |
|---------------------|------------------------------------------|
| `getEnv:`           | Read environment variable (nil if unset) |
| `getEnv:default:`   | Read env var with fallback               |
| `setEnv:value:`     | Set environment variable                 |
| `unsetEnv:`         | Remove environment variable              |
| `osPlatform`        | OS name: linux, darwin, win32            |
| `osFamily`          | OS family: unix or win32                 |
| `architecture`      | System architecture string               |
| `hostname`          | Machine hostname                         |
| `erlangVersion`     | OTP version string                       |
| `pid`               | OS process ID                            |
| `uniqueId`          | Unique positive monotonic integer         |
""".

%% `halt/0,1` shadow the auto-imported `erlang:halt/0,1` BIFs; we always call the
%% BIF qualified (`erlang:halt/1`).
-compile({no_auto_import, [halt/0, halt/1]}).

-export(['getEnv:'/1, 'getEnv:default:'/2]).
-export(['setEnv:value:'/2, 'unsetEnv:'/1]).
-export([osPlatform/0, osFamily/0, architecture/0, hostname/0]).
-export([erlangVersion/0, pid/0, uniqueId/0]).
-export([halt/0, 'halt:'/1, halt/1]).
-export([getEnv/1, getEnv/2]).
-export([setEnv/2, unsetEnv/1]).

-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc "Read an environment variable. Returns nil if not set.".
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

-doc "Read an environment variable with a default fallback.".
-spec 'getEnv:default:'(binary(), Default :: binary()) -> binary().
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

-doc "Set an environment variable. Returns true.".
-spec 'setEnv:value:'(binary(), Value :: binary()) -> 'true'.
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

-doc "Remove an environment variable. Returns true.".
-spec 'unsetEnv:'(binary()) -> 'true'.
'unsetEnv:'(Name) when is_binary(Name) ->
    os:unsetenv(binary_to_list(Name)),
    true;
'unsetEnv:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'System'),
    Error1 = beamtalk_error:with_selector(Error0, 'unsetEnv:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be a String">>),
    beamtalk_error:raise(Error2).

-doc """
Return the OS platform name.
Maps Erlang os:type() to familiar platform strings.
""".
-spec osPlatform() -> binary().
osPlatform() ->
    {_Family, Name} = os:type(),
    platform_name(Name).

-doc "Return the OS family: unix or win32.".
-spec osFamily() -> binary().
osFamily() ->
    {Family, _Name} = os:type(),
    atom_to_binary(Family, utf8).

-doc "Return the system architecture string.".
-spec architecture() -> binary().
architecture() ->
    list_to_binary(erlang:system_info(system_architecture)).

-doc "Return the machine hostname.".
-spec hostname() -> binary().
hostname() ->
    %% inet:gethostname/0 spec says it always returns {ok, Hostname},
    %% but we wrap in try for defensive safety.
    try
        {ok, Hostname} = inet:gethostname(),
        list_to_binary(Hostname)
    catch
        Class:Reason ->
            ?LOG_ERROR("Hostname lookup failed", #{
                domain => [beamtalk, stdlib],
                module => ?MODULE,
                function => hostname,
                class => Class,
                reason => Reason
            }),
            Error0 = beamtalk_error:new(runtime_error, 'System'),
            Error1 = beamtalk_error:with_selector(Error0, hostname),
            Error2 = beamtalk_error:with_details(Error1, #{class => Class, reason => Reason}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Could not resolve hostname from OS">>),
            beamtalk_error:raise(Error3)
    end.

-doc "Return the OTP version string.".
-spec erlangVersion() -> binary().
erlangVersion() ->
    list_to_binary(erlang:system_info(otp_release)).

-doc "Return the OS process ID as an integer.".
-spec pid() -> integer().
pid() ->
    list_to_integer(os:getpid()).

-doc """
Return a unique positive monotonic integer.
Each call returns a value strictly greater than any previous call.
""".
-spec uniqueId() -> pos_integer().
uniqueId() ->
    erlang:unique_integer([positive, monotonic]).

-doc "Halt the whole VM with status 0 (the nuclear, whole-node option). Does not return.".
-spec halt() -> no_return().
halt() ->
    do_halt(0, halt).

-doc """
Halt the whole VM with status `Code` (ADR 0099 §3) — the nuclear option.

`erlang:halt/1` is an *immediate* halt: it does **not** run OTP shutdown or
`terminate/2` callbacks (it does flush the `io` layer, so no explicit `Console`
pre-flush is needed). Correct for a standalone app that owns the node and wants
to stop *now*.

`Code` must be an `Integer` (non-Integer → `#type_error`, consistent with
`setEnv:`) in the POSIX range 0–255 (out-of-range → `#beamtalk_error`, a wrong
*value* not a wrong *type*).

**Refused outside a node-owning context.** Halting the node inside a live,
shared workspace would kill the service and every other connected session, so
`System halt:` raises a `#beamtalk_error` (pointing at `Program exit:`) unless
the `beamtalk_runtime` `node_owning` app env is set — which only the run-mode
and escript entry harnesses set, at boot.
""".
-spec 'halt:'(integer()) -> no_return().
'halt:'(Code) when is_integer(Code), Code >= 0, Code =< 255 ->
    do_halt(Code, 'halt:');
'halt:'(Code) when is_integer(Code) ->
    Error0 = beamtalk_error:new(invalid_argument, 'System'),
    Error1 = beamtalk_error:with_selector(Error0, 'halt:'),
    Error2 = beamtalk_error:with_details(Error1, #{got => Code}),
    Error3 = beamtalk_error:with_hint(
        Error2, <<"Exit status must be in the POSIX range 0..255">>
    ),
    beamtalk_error:raise(Error3);
'halt:'(_Code) ->
    beamtalk_error:raise_type_error('System', 'halt:', <<"Exit status must be an Integer">>).

-doc "FFI shim for `(Erlang beamtalk_system) halt:` (proxy strips the colon).".
-spec halt(integer()) -> no_return().
halt(Code) ->
    'halt:'(Code).

-doc """
Shims for (Erlang beamtalk_system) FFI dispatch.
The Erlang FFI proxy (beamtalk_erlang_proxy) maps any keyword selector to
its first keyword before calling direct_call/3, so `getEnv:` → `getEnv`
and `getEnv:default:` → `getEnv`. These shims exist so the Beamtalk
selectors `(Erlang beamtalk_system) getEnv: name` and
`(Erlang beamtalk_system) getEnv: name default: fallback` dispatch to
getEnv/1 and getEnv/2 respectively, delegating to the canonical forms.
""".
-spec getEnv(binary()) -> binary() | 'nil'.
getEnv(Name) -> 'getEnv:'(Name).

-spec getEnv(binary(), Default :: binary()) -> binary().
getEnv(Name, Default) -> 'getEnv:default:'(Name, Default).

-spec setEnv(binary(), Value :: binary()) -> 'true'.
setEnv(Name, Value) -> 'setEnv:value:'(Name, Value).

-spec unsetEnv(binary()) -> 'true'.
unsetEnv(Name) -> 'unsetEnv:'(Name).

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

-doc """
Halt the node with `Code` if this invocation owns it; otherwise refuse with a
`#beamtalk_error` directing the caller to the safe `Program exit:`.
""".
-spec do_halt(integer(), atom()) -> no_return().
do_halt(Code, Selector) ->
    case application:get_env(beamtalk_runtime, node_owning, false) of
        true ->
            erlang:halt(Code);
        _ ->
            Error0 = beamtalk_error:new(unsupported, 'System'),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error2 = beamtalk_error:with_hint(
                Error1,
                <<
                    "System halt: halts the whole node, which would kill a shared "
                    "workspace and every other connected session. Use Program exit: "
                    "to end just this program/session."
                >>
            ),
            beamtalk_error:raise(Error2)
    end.

-doc "Map os:type() Name atom to platform string.".
-spec platform_name(atom()) -> binary().
platform_name(darwin) -> <<"darwin">>;
platform_name(linux) -> <<"linux">>;
platform_name(nt) -> <<"win32">>;
platform_name(Other) -> atom_to_binary(Other, utf8).
