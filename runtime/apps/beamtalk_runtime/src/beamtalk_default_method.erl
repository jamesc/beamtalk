%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_default_method).

%%% **DDD Context:** Object System Context

-moduledoc """
Whether an instance of a class runs `Object`'s own method for a selector.

A primitive receiver's dynamic send walks its class's dispatch chain —
`Binary` → `Collection` → `Value` → `Object`, with an extension-registry probe
at every level — before it reaches an `Object` method. When nothing on that
chain overrides the selector, a caller that already knows what `Object`'s
method does can answer without the walk. `beamtalk_equality:primitive_equals/2`
does this for `equals:` (BT-4416) and
`beamtalk_behaviour_intrinsics:primitiveIsKindOf/2` for `isKindOf:` (BT-4473).

`is_default/2` answers that question; `owner/2` is the walk behind it, for a
caller that folds the answer into a derived-cache entry of its own.
`overridden_anywhere/1` is the node-wide, one-read version for a watched
selector: whether any class other than `Object` has ever defined it, compiled
or as an extension. The walk
follows the order a compiled primitive `dispatch/3` resolves a selector in: at
each class, its own methods first, then its extensions, then the superclass. A `sealed` method
does not make the walk unnecessary: an extension may still define the
selector on a subclass.
""".

-include("beamtalk.hrl").

-export([
    is_default/2,
    owner/2,
    overridden_anywhere/1,
    override_flag_key/1,
    note_override/2,
    note_overrides/2
]).

%% The selectors whose override anywhere below `Object` is tracked by
%% `overridden_anywhere/1` (BT-4473): those a caller answers without dispatch
%% on the strength of that flag.
-define(WATCHED, ['isKindOf:']).

-doc """
True if an instance of `Class` runs `Object>>Selector`: nothing on its
superclass chain below `Object` defines `Selector` or has it as an extension.

The answer is memoised in `beamtalk_class_derived_cache`, and a `true` answer
additionally in the calling process's dictionary, tagged with the cache
generation read before computing it. A hot loop then costs a generation read
and a `get/1` rather than an ETS read, and any class write or extension
(un)registration bumps the generation, which turns every per-process memo into
a miss. Until every class on the chain has published its method records
(early bootstrap, an unregistered class) the answer is `false`, so the caller
sends.
""".
-spec is_default(atom(), atom()) -> boolean().
is_default(Class, Selector) ->
    Gen = beamtalk_class_derived_cache:generation(),
    MemoKey = {?MODULE, Class, Selector},
    case get(MemoKey) of
        Gen when Gen =/= undefined ->
            true;
        _ ->
            Default = beamtalk_class_derived_cache:fetch(
                {default_method, Class, Selector}, fun() ->
                    case owner(Class, Selector) of
                        unpublished -> {nocache, false};
                        Owner -> {cache, Owner =:= 'Object'}
                    end
                end
            ),
            case Default andalso Gen =/= undefined of
                true -> put(MemoKey, Gen);
                false -> ok
            end,
            Default
    end.

-doc """
The class whose `Selector` an instance of `Class` runs, `{extension, C}` for
an extension on `C`, `none` if the chain defines none, or `unpublished` while
some class on the chain has not published its method records. Not memoised:
callers cache it in `beamtalk_class_derived_cache`, under a key of their own.
""".
-spec owner(atom(), atom()) -> atom() | {extension, atom()} | unpublished.
owner(Class, Selector) ->
    StepFun = fun(Name, _Depth) ->
        case beamtalk_class_metadata:lookup_method_record(Name, instance, Selector) of
            {ok, _} ->
                {found, Name};
            not_found ->
                case beamtalk_extensions:has(Name, Selector) of
                    true ->
                        {found, {extension, Name}};
                    false ->
                        case
                            {
                                beamtalk_class_metadata:lookup_instance_methods(Name),
                                beamtalk_class_metadata:lookup_superclass(Name)
                            }
                        of
                            {{ok, _}, {ok, Root}} when Root =:= none; Root =:= nil -> not_found;
                            {{ok, _}, {ok, Super}} -> {next, Super};
                            _ -> {found, unpublished}
                        end
                end
        end
    end,
    try beamtalk_hierarchy:walk_ancestors(Class, StepFun, ?MAX_HIERARCHY_DEPTH + 1) of
        {found, Owner} -> Owner;
        not_found -> none;
        {max_depth_exceeded, _} -> unpublished
    catch
        %% Extension or metadata table not created yet (early bootstrap).
        error:badarg -> unpublished
    end.

-doc """
Whether a watched selector (`isKindOf:`) has ever been defined on this node by
a class other than `Object`, as a compiled instance method or as an extension
on any class (BT-4473). One `persistent_term` read; `true` for a selector that
is not watched.

The flag is raised before the defining record or extension row is written
(`note_override/2`) and is never lowered: removing the override leaves it set.
So `false` proves that every class's chain runs `Object`'s method for the
selector; `true` only that one may not. A caller that answers `Object`'s
method without dispatch on the strength of a `false` uses it as its whole
override check (`beamtalk_behaviour_intrinsics:isKindOfBuiltin/3`).
""".
-spec overridden_anywhere(atom()) -> boolean().
overridden_anywhere(Selector) ->
    case lists:member(Selector, ?WATCHED) of
        true -> persistent_term:get(override_flag_key(Selector), false);
        false -> true
    end.

-doc """
The `persistent_term` key of a watched selector's override flag (BT-4769).

Compiled `isKindOf:` call sites read the flag with one `persistent_term:get/2`
on this key instead of calling `overridden_anywhere/1` (a `lists:member/2`
scan per call). The compiler spells the key itself, so the shape is pinned to
the shared fixture `override_flag_key_conformance.json`, asserted from both
sides.
""".
-spec override_flag_key(atom()) -> {?MODULE, overridden, atom()}.
override_flag_key(Selector) ->
    {?MODULE, overridden, Selector}.

-doc """
Record that `Class` defines `Selector` (a compiled method or an extension):
raise `overridden_anywhere/1` for a watched selector, unless `Class` is
`Object`. Called before the definition becomes visible. A watched selector's
key is written once, so `persistent_term` sees at most one write per watched
selector per node.
""".
-spec note_override(atom(), atom()) -> ok.
note_override('Object', _Selector) ->
    ok;
note_override(_Class, Selector) ->
    case lists:member(Selector, ?WATCHED) andalso not overridden_anywhere(Selector) of
        true -> persistent_term:put(override_flag_key(Selector), true);
        false -> ok
    end.

-doc "`note_override/2` for each watched selector among `Records`' keys.".
-spec note_overrides(atom(), #{atom() => term()}) -> ok.
note_overrides(Class, Records) ->
    lists:foreach(
        fun(Selector) -> note_override(Class, Selector) end,
        [S || S <- ?WATCHED, is_map_key(S, Records)]
    ).
