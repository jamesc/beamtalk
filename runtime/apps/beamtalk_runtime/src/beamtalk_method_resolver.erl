%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_method_resolver).

%%% **DDD Context:** Object System Context

-moduledoc """
Method Resolver domain service.

Resolves compiled method metadata from class references. Accepts
various class reference formats (pid, atom, class object tuple) and
returns a CompiledMethod map or nil.

This is a stateless domain service that delegates to the class
gen_server for the actual method lookup. It owns the logic for
normalizing class references and producing structured errors.

See also: beamtalk_object_class for class process management
See also: beamtalk_compiled_method_ops for CompiledMethod field access
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([resolve/2]).

-type selector() :: atom().

-doc """
Resolve a compiled method from a class reference.

Accepts:
- Class process pid
- Class name atom (e.g., 'Counter')
- Class object tuple ({beamtalk_object, 'Counter class', Module, Pid})

Returns a CompiledMethod map or nil if the method is not found.
Raises beamtalk_error for invalid class references.
""".
-spec resolve(ClassRef, selector()) -> compiled_method() | 'nil' when
    ClassRef :: pid() | atom() | tuple().
resolve(ClassPid, Selector) when is_pid(ClassPid) ->
    resolve_with_hierarchy(ClassPid, Selector);
%% BT-2195: Metaclass receiver — look up class-side methods of the described
%% class. The 'Metaclass'-tagged beamtalk_object holds the *described* class's
%% pid in its `pid` field (see beamtalk_behaviour_intrinsics:classClass/1); we
%% ask that class gen_server for its class methods (which walks the metaclass
%% parallel hierarchy via find_inherited_class_method/2).
resolve(#beamtalk_object{class = 'Metaclass', pid = ClassPid}, Selector) when
    is_pid(ClassPid)
->
    resolve_class_side(ClassPid, Selector);
resolve(#beamtalk_object{class = ClassTag, pid = ClassPid} = Obj, Selector) when
    is_atom(ClassTag), is_pid(ClassPid)
->
    case beamtalk_class_registry:is_class_name(ClassTag) of
        true ->
            resolve_with_hierarchy(ClassPid, Selector);
        false ->
            Error1 = beamtalk_error:new(type_error, ClassTag, '>>'),
            Error2 = beamtalk_error:with_hint(
                Error1,
                iolist_to_binary(
                    io_lib:format(">> expects a class, got instance ~p", [Obj])
                )
            ),
            beamtalk_error:raise(Error2)
    end;
resolve(ClassName, Selector) when is_atom(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            Error1 = beamtalk_error:new(does_not_understand, ClassName, '>>'),
            Error2 = beamtalk_error:with_hint(Error1, <<"Class not found. Is it loaded?">>),
            beamtalk_error:raise(Error2);
        Pid ->
            resolve_with_hierarchy(Pid, Selector)
    end;
resolve(Other, _Selector) ->
    Class = beamtalk_primitive:class_of(Other),
    Error1 = beamtalk_error:new(type_error, Class, '>>'),
    Error2 = beamtalk_error:with_hint(
        Error1,
        iolist_to_binary(
            io_lib:format(">> expects a class, got ~p", [Other])
        )
    ),
    beamtalk_error:raise(Error2).

-doc """
Resolve a method, walking the superclass chain if not found locally.

BT-2786: The walk itself (depth guard, cycle warning, advance-to-superclass)
is `beamtalk_hierarchy:walk_ancestors/3`; this function supplies only the
per-class `{method, Selector}` gen_server probe.

`ClassPid`'s own method table is checked here, outside the depth-counted
walk — matching the pre-BT-2786 behaviour where the receiver's own class was
"free" and the `?MAX_HIERARCHY_DEPTH` budget applied only to the superclass
chain above it (the same split `beamtalk_class_dispatch:find_class_method_in_chain/2`
uses). The walk itself starts at the immediate superclass.
""".
-spec resolve_with_hierarchy(pid(), selector()) -> compiled_method() | 'nil'.
resolve_with_hierarchy(ClassPid, Selector) ->
    case gen_server:call(ClassPid, {method, Selector}) of
        nil ->
            StepFun = fun(Pid, _Depth) -> method_step(Pid, Selector) end,
            case
                beamtalk_hierarchy:walk_ancestors(
                    superclass_pid(ClassPid), StepFun, ?MAX_HIERARCHY_DEPTH
                )
            of
                {found, Method} ->
                    Method;
                not_found ->
                    nil;
                max_depth_exceeded ->
                    ?LOG_WARNING(">> hierarchy walk exceeded ~p levels", [?MAX_HIERARCHY_DEPTH], #{
                        domain => [beamtalk, runtime]
                    }),
                    nil
            end;
        Method ->
            Method
    end.

-doc """
BT-2195: Resolve a class-side method on the class identified by `ClassPid`.

Delegates to the class gen_server's `{class_method, Selector}` handler which
returns a `CompiledMethod` map carrying the class-side source / signature /
doc, or walks the metaclass parallel hierarchy to find an inherited class
method (via `find_inherited_class_method/2` on the gen_server side). Returns
`nil` when no class method with the selector is defined in the chain.
""".
-spec resolve_class_side(pid(), selector()) -> compiled_method() | 'nil'.
resolve_class_side(ClassPid, Selector) ->
    gen_server:call(ClassPid, {class_method, Selector}).

-doc """
Per-node probe for the `beamtalk_hierarchy:walk_ancestors/3` walk: check
`ClassPid`'s own method table for `Selector`, and if absent, resolve its
superclass's pid as the next node.
""".
-spec method_step(pid(), selector()) -> beamtalk_hierarchy:step_result(compiled_method()).
method_step(ClassPid, Selector) ->
    case gen_server:call(ClassPid, {method, Selector}) of
        nil ->
            case superclass_pid(ClassPid) of
                none -> not_found;
                SuperPid -> {next, SuperPid}
            end;
        Method ->
            {found, Method}
    end.

-doc """
Resolve `ClassPid`'s immediate superclass to its process pid, or `none` if
there is no superclass or its process is not registered.
""".
-spec superclass_pid(pid()) -> pid() | none.
superclass_pid(ClassPid) ->
    case beamtalk_object_class:superclass(ClassPid) of
        none ->
            none;
        SuperName ->
            case beamtalk_class_registry:whereis_class(SuperName) of
                undefined -> none;
                SuperPid -> SuperPid
            end
    end.
