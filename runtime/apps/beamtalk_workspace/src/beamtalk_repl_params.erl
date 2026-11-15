%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_params).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Request-parameter validation shared by the REPL op modules.

* `nonempty_binary/2` — a required, non-empty string param;
  `nonempty_or_undefined/1` — an optional one (`""` reads as absent).
* `existing_atom_param/2` — "name → *existing* atom" with an **explicit
  unknown-name policy**, so client text never creates an atom and each op says
  what an unknown name means for it:
  * `sentinel` — an unknown name answers `{ok, unknown_name()}`; the query then
    finds nothing (an empty result), not a validation failure (`senders:`,
    `implementors:`, `browse-source`, ...).
  * `{error, Fun}` — an unknown name answers `{error, Fun(Name)}` (a
    not-found message).
* `actor_pid/1`, `validate_actor_pid/1`, `invalid_pid_error/2` — the
  actor-pid param used by `inspect`, `kill` and `pid-stats`.
""".

-export([
    nonempty_binary/2,
    nonempty_or_undefined/1,
    existing_atom_param/2,
    unknown_name/0,
    actor_pid/1,
    validate_actor_pid/1,
    is_known_actor/1,
    invalid_pid_error/2
]).

-export_type([unknown_policy/0]).

-type unknown_policy() :: sentinel | {error, fun((binary()) -> term())}.

-doc """
The single placeholder atom `existing_atom_param/2` answers under the
`sentinel` policy for a name that is not an existing atom. No registry knows
this atom, so every lookup keyed by it misses.
""".
-spec unknown_name() -> atom().
unknown_name() -> '$beamtalk_unknown_name'.

-doc "Fetch `Key` from `Params`: `{ok, Bin}` for a non-empty binary, else `error`.".
-spec nonempty_binary(map(), binary()) -> {ok, binary()} | error.
nonempty_binary(Params, Key) ->
    case maps:get(Key, Params, undefined) of
        Bin when is_binary(Bin), byte_size(Bin) > 0 -> {ok, Bin};
        _ -> error
    end.

-doc """
Normalise an optional string param: `""` is treated the same as absent, and so
is a wrong-typed value (a number, list or map from a malformed request) — the
op then reports the param as missing instead of crashing. Answers the
non-empty binary, or `undefined`.
""".
-spec nonempty_or_undefined(term()) -> binary() | undefined.
nonempty_or_undefined(<<>>) -> undefined;
nonempty_or_undefined(undefined) -> undefined;
nonempty_or_undefined(Bin) when is_binary(Bin) -> Bin;
nonempty_or_undefined(_WrongType) -> undefined.

-doc "Resolve a name to an existing atom, applying the `Policy` for an unknown name.".
-spec existing_atom_param(binary(), unknown_policy()) -> {ok, atom()} | {error, term()}.
existing_atom_param(Name, Policy) ->
    case beamtalk_text:to_existing_atom(Name) of
        {ok, _} = Ok -> Ok;
        error -> unknown(Name, Policy)
    end.

-spec unknown(binary(), unknown_policy()) -> {ok, atom()} | {error, term()}.
unknown(_Name, sentinel) -> {ok, unknown_name()};
unknown(Name, {error, Fun}) -> {error, Fun(Name)}.

-doc """
Read the `actor` param as a pid and check it is a registered actor. Answers
`{ok, Pid, PidBin}`, or `{error, #beamtalk_error{}}` for an invalid or
unknown pid (see `invalid_pid_error/2`). A missing or non-string `actor`
param is an `invalid_pid` error like any other unparseable pid.
""".
-spec actor_pid(map()) -> {ok, pid(), binary()} | {error, beamtalk_error:error()}.
actor_pid(Params) ->
    PidStr = pid_param_string(maps:get(<<"actor">>, Params, <<>>)),
    case validate_actor_pid(PidStr) of
        {ok, Pid} -> {ok, Pid, list_to_binary(PidStr)};
        {error, Reason} -> {error, invalid_pid_error(Reason, PidStr)}
    end.

%% A wrong-typed `actor` value is never echoed (it can be arbitrarily large and
%% hold code points `list_to_binary/1` rejects); the error shows a placeholder.
%% It can never parse as a pid, so `validate_actor_pid/1` answers `invalid_pid`.
-spec pid_param_string(term()) -> string().
pid_param_string(Bin) when is_binary(Bin) -> binary_to_list(Bin);
pid_param_string(_Other) -> "<not a string>".

-spec validate_actor_pid(string()) -> {ok, pid()} | {error, invalid_pid | unknown_actor}.
validate_actor_pid(PidStr) ->
    try
        Pid = list_to_pid(PidStr),
        case is_known_actor(Pid) of
            true -> {ok, Pid};
            false -> {error, unknown_actor}
        end
    catch
        _:_ -> {error, invalid_pid}
    end.

-spec is_known_actor(pid()) -> boolean().
is_known_actor(Pid) when is_pid(Pid) ->
    case whereis(beamtalk_actor_registry) of
        undefined ->
            false;
        RegistryPid ->
            case beamtalk_repl_actors:get_actor(RegistryPid, Pid) of
                {ok, _} -> true;
                {error, not_found} -> false
            end
    end.

-doc "Structured error for a pid param that is not a (known) actor.".
-spec invalid_pid_error(atom(), string()) -> beamtalk_error:error().
invalid_pid_error(Reason, PidStr) ->
    PidBin = list_to_binary(PidStr),
    beamtalk_repl_errors:make(
        Reason,
        'Actor',
        iolist_to_binary([<<"Invalid actor PID: ">>, PidBin]),
        <<"Use :actors to list valid actor PIDs.">>
    ).
