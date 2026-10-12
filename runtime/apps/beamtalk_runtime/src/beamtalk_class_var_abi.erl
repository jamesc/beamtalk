%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_class_var_abi).
-moduledoc """
The `class_var_abi` load gate (ADR 0130 §3, Implementation Phase 3).

A compiled Beamtalk class module declares, in `__beamtalk_meta/0`, the
class-method calling convention it was compiled for (`class_var_abi`). Class
registration (`beamtalk_object_class:start/2`) and hot reload
(`beamtalk_object_class:update_class/2`) call `check_class_info_abi/2` before
installing anything and refuse a module whose value is not exactly `abi/0`.
The release preflight (`beamtalk_release_shapes:extract_shapes/2`) wraps its
module loads in `collect_abi_refusals/1` to turn a refusal into a failed
preflight.

This is module-loading policy, kept apart from `beamtalk_class_vars` (the leaf
that owns the class-variable key and every access). The accepted value comes
from `?BT_CLASS_VAR_ABI` in the generated `beamtalk_class_vars_keys.hrl`, the
same Rust constant codegen emits into every module's meta, so the two cannot
disagree.
""".

-include_lib("kernel/include/logger.hrl").
-include("beamtalk.hrl").
-include("beamtalk_class_vars_keys.hrl").

-export([
    abi/0,
    check_class_info_abi/2,
    collect_abi_refusals/1
]).

-define(ABI_REFUSALS_TABLE, beamtalk_abi_refusals).

-doc """
The `class_var_abi` value this runtime accepts: the calling convention of
compiled class methods (`class_<sel>(ClassSelf, Args...)`, class variables in
the class process's dictionary). Generated from the Rust `ABI_VERSION`
(`class_var_keys` in `beamtalk-codegen`), the same constant codegen emits
into every module's `__beamtalk_meta/0`, so the two cannot disagree.
""".
-spec abi() -> pos_integer().
abi() -> ?BT_CLASS_VAR_ABI.

-doc """
Run `Fun` while collecting the ABI refusals `check_class_info_abi/2` records,
returning `{Result, Refusals}` (`Refusals :: [{Module, #beamtalk_error{}}]`,
sorted). `beamtalk_release_shapes:extract_shapes/2` uses it to turn a refused
module into a failed release preflight: a refusal happens inside a module's
`-on_load` hook, whose failure reason the code server does not return.

The collector is a named public ETS table owned by the caller (the `-on_load`
hook runs in the code server, not the caller, so it cannot be passed by
argument). Creating a named table is atomic: a second concurrent collection, or
a leftover table, raises `abi_collection_in_progress` (an `error` exit the
caller maps to `{error, _}`) instead of blaming one collection's refusals on
another. The table is deleted afterwards, and the VM deletes it if the caller is
killed, so a dead caller never leaves the collector claimed.
""".
-spec collect_abi_refusals(fun(() -> Result)) ->
    {Result, [{atom() | undefined, #beamtalk_error{}}]}
when
    Result :: term().
collect_abi_refusals(Fun) ->
    Table =
        try
            ets:new(?ABI_REFUSALS_TABLE, [named_table, public, set])
        catch
            error:badarg -> erlang:error(abi_collection_in_progress)
        end,
    try
        Result = Fun(),
        {Result, lists:sort(ets:tab2list(Table))}
    after
        ets:delete(Table)
    end.

-doc """
Refuse a compiled Beamtalk class module whose `class_var_abi` is not exactly
`abi/0` (ADR 0130 §3, Implementation Phase 3): `{error, #beamtalk_error{kind =
abi_mismatch}}` naming the module and saying to recompile.

Called by `beamtalk_object_class:start/2` (registration) and
`beamtalk_object_class:update_class/2` (hot reload) before anything is
installed, with the `ClassInfo` the registration carries. The gate covers
*compiled Beamtalk class modules*, identified the way the loader identifies
them, by `__beamtalk_meta/0`:

- the compiler-emitted meta map in `ClassInfo` (`meta`, which always has the
  `class` key; the only form available while the module's `-on_load` hook is
  still running, when `erlang:function_exported/3` is `false`), or
- an exported `__beamtalk_meta/0` on `ClassInfo`'s `module`.

Among those, a missing `class_var_abi` key (every module compiled before the
flip) or any value other than `abi/0` is refused: the check is "equal to the
current value", never "present and unequal". A module without
`__beamtalk_meta/0` (a hand-written Erlang class module, an EUnit fixture, a
ClassBuilder class) is outside the gate; its obligation is the FFI rule (use
`beamtalk_class_vars`, never return `class_var_result`).
""".
-spec check_class_info_abi(atom(), map()) -> ok | {error, #beamtalk_error{}}.
check_class_info_abi(ClassName, ClassInfo) ->
    Module = maps:get(module, ClassInfo, undefined),
    case compiled_meta(Module, maps:get(meta, ClassInfo, undefined)) of
        none ->
            ok;
        {invalid, Why} ->
            %% A crashing or non-map `__beamtalk_meta/0` is a different failure
            %% from "compiled before ADR 0130": report it as such.
            refuse_abi(ClassName, Module, abi_mismatch_error(ClassName, Module, {invalid, Why}));
        {ok, Meta} ->
            case maps:find(class_var_abi, Meta) of
                {ok, ?BT_CLASS_VAR_ABI} ->
                    ok;
                Found ->
                    refuse_abi(ClassName, Module, abi_mismatch_error(ClassName, Module, Found))
            end
    end.

%%====================================================================
%% Internal
%%====================================================================

-spec refuse_abi(atom(), atom() | undefined, #beamtalk_error{}) -> {error, #beamtalk_error{}}.
refuse_abi(ClassName, Module, Error) ->
    ?LOG_ERROR(
        "Refused ~p: ~ts",
        [Module, Error#beamtalk_error.message],
        #{class => ClassName, module => Module, domain => [beamtalk, runtime]}
    ),
    record_abi_refusal(Module, Error),
    record_pending_load_error(ClassName, Error),
    {error, Error}.

%% BT-3722: a refusal inside a module's `-on_load` hook is reported by the code
%% server as a bare `{error, on_load_failure}`. Park the structured error in the
%% pending-load-error table (the same channel `stdlib_shadowing` uses) so the
%% REPL/CLI loaders, which drain it by class name after a failed load, show the
%% `abi_mismatch` (expected/found/remedy) instead of `on_load_failure`. Skipped
%% while a release preflight is collecting refusals: that caller reads them from
%% the collector and never drains this table, so an entry would go stale.
-spec record_pending_load_error(atom(), #beamtalk_error{}) -> ok.
record_pending_load_error(ClassName, Error) ->
    case ets:info(?ABI_REFUSALS_TABLE) of
        undefined -> beamtalk_class_registry:record_pending_load_error(ClassName, Error);
        _ -> ok
    end.

-spec compiled_meta(atom() | undefined, term()) -> {ok, map()} | {invalid, term()} | none.
compiled_meta(_Module, #{class := _} = Meta) ->
    {ok, Meta};
compiled_meta(Module, _) when Module =/= undefined ->
    beamtalk_class_metadata:read_meta_detailed(Module);
compiled_meta(_, _) ->
    none.

-spec abi_mismatch_error(atom(), atom() | undefined, {ok, term()} | error | {invalid, term()}) ->
    #beamtalk_error{}.
abi_mismatch_error(ClassName, Module, Found) ->
    {Reported, Declared} =
        case Found of
            {ok, Value} ->
                {Value, io_lib:format("class_var_abi ~p", [Value])};
            error ->
                {missing, "no class_var_abi entry (compiled before ADR 0130)"};
            {invalid, Why} ->
                {invalid_meta,
                    io_lib:format(
                        "an invalid __beamtalk_meta/0 (~0p), so its class_var_abi is unknown", [
                            Why
                        ]
                    )}
        end,
    Message = iolist_to_binary(
        io_lib:format(
            "Module ~s (class ~s) was compiled with ~s, but this runtime requires "
            "class_var_abi ~p",
            [Module, ClassName, Declared, ?BT_CLASS_VAR_ABI]
        )
    ),
    Error0 = beamtalk_error:new(abi_mismatch, ClassName),
    Error1 = beamtalk_error:with_message(Error0, Message),
    Error2 = beamtalk_error:with_details(Error1, #{
        module => Module, expected => ?BT_CLASS_VAR_ABI, found => Reported
    }),
    beamtalk_error:with_hint(
        Error2,
        <<
            "Recompile the package with the current beamtalk compiler. The class-variable "
            "calling convention changed (ADR 0130), so a module compiled by an older compiler "
            "cannot be loaded."
        >>
    ).

-spec record_abi_refusal(atom() | undefined, #beamtalk_error{}) -> ok.
record_abi_refusal(Module, Error) ->
    try
        ets:insert(?ABI_REFUSALS_TABLE, {Module, Error}),
        ok
    catch
        error:badarg -> ok
    end.
