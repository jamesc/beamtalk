%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_browse_support).

%%% **DDD Context:** Workspace Context

-moduledoc """
Shared leaf helpers for the system-browser ops: argument/not-found error builders and the defensive class-call / null-coercion shapes used by every browse op. Moved verbatim out of `beamtalk_repl_ops_browse` (BT-4907).
""".

-include_lib("kernel/include/logger.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    arg_error/2,
    atom_or_null/1,
    class_not_found_message/1,
    not_found_error/2,
    safe_bool/1,
    safe_class_call/2
]).

-spec class_not_found_message(binary()) -> binary().
class_not_found_message(Cls) ->
    iolist_to_binary([<<"class `">>, Cls, <<"` not found">>]).

-spec atom_or_null(atom() | none) -> binary() | null.
atom_or_null(none) -> null;
atom_or_null(A) when is_atom(A) -> atom_to_binary(A, utf8).

-spec safe_bool(fun(() -> boolean())) -> boolean().
safe_bool(F) ->
    %% bt-catcher-audit: not-applicable-no-block - F is an Erlang reflection predicate, not a
    %% Beamtalk block
    try F() of
        B when is_boolean(B) -> B
    catch
        _:_ -> false
    end.

%% Reflection gen_server call with the same defensive guards list-classes uses —
%% a dead / slow class object never crashes a browse.
-spec safe_class_call(pid(), term()) -> term().
safe_class_call(Pid, Call) ->
    try
        gen_server:call(Pid, Call, 5000)
    catch
        exit:{noproc, _} -> undefined;
        exit:{timeout, _} -> undefined;
        _:_ -> undefined
    end.

%%% ====================================================================
%%% Errors
%%% ====================================================================

-spec arg_error(binary(), binary()) -> {error, #beamtalk_error{}}.
arg_error(Op, Reason) ->
    {error,
        beamtalk_repl_errors:make(
            argument_error, 'REPL', iolist_to_binary([Op, <<": ">>, Reason])
        )}.

-spec not_found_error(binary(), atom()) -> {error, #beamtalk_error{}}.
not_found_error(Op, ClassName) ->
    arg_error(Op, class_not_found_message(atom_to_binary(ClassName, utf8))).
