%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Method Resolver domain service.
%%
%% DDD Context: Object System
%%
%% Resolves compiled method metadata from class references. Accepts
%% various class reference formats (pid, atom, class object tuple) and
%% returns a CompiledMethod map or nil.
%%
%% This is a stateless domain service that delegates to the class
%% gen_server for the actual method lookup. It owns the logic for
%% normalizing class references and producing structured errors.
%%
%% @see beamtalk_object_class for class process management
%% @see beamtalk_compiled_method_ops for CompiledMethod field access
-module(beamtalk_method_resolver).

-include("beamtalk.hrl").

-export([resolve/2]).

-type selector() :: atom().

%% @doc Resolve a compiled method from a class reference.
%%
%% Accepts:
%% - Class process pid
%% - Class name atom (e.g., 'Counter')
%% - Class object tuple ({beamtalk_object, 'Counter class', Module, Pid})
%%
%% Returns a CompiledMethod map or nil if the method is not found.
%% Raises beamtalk_error for invalid class references.
-spec resolve(ClassRef, selector()) -> compiled_method() | nil when
    ClassRef :: pid() | atom() | tuple().
resolve(ClassPid, Selector) when is_pid(ClassPid) ->
    resolve_with_hierarchy(ClassPid, Selector);
resolve({beamtalk_object, ClassTag, _Module, ClassPid} = Obj, Selector) when
    is_atom(ClassTag), is_pid(ClassPid)
->
    case beamtalk_class_registry:is_class_name(ClassTag) of
        true ->
            resolve_with_hierarchy(ClassPid, Selector);
        false ->
            Error0 = beamtalk_error:new(type_error, ClassTag),
            Error1 = beamtalk_error:with_selector(Error0, '>>'),
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
            Error0 = beamtalk_error:new(does_not_understand, ClassName),
            Error1 = beamtalk_error:with_selector(Error0, '>>'),
            Error2 = beamtalk_error:with_hint(Error1, <<"Class not found. Is it loaded?">>),
            beamtalk_error:raise(Error2);
        Pid ->
            resolve_with_hierarchy(Pid, Selector)
    end;
resolve(Other, _Selector) ->
    Class = beamtalk_primitive:class_of(Other),
    Error0 = beamtalk_error:new(type_error, Class),
    Error1 = beamtalk_error:with_selector(Error0, '>>'),
    Error2 = beamtalk_error:with_hint(
        Error1,
        iolist_to_binary(
            io_lib:format(">> expects a class, got ~p", [Other])
        )
    ),
    beamtalk_error:raise(Error2).

%% @private
%% @doc Resolve a method, walking the superclass chain if not found locally.
-spec resolve_with_hierarchy(pid(), selector()) -> compiled_method() | nil.
resolve_with_hierarchy(ClassPid, Selector) ->
    case gen_server:call(ClassPid, {method, Selector}) of
        nil ->
            walk_superclass_chain(ClassPid, Selector);
        Method ->
            Method
    end.

%% @private
%% @doc Walk the superclass chain to find an inherited method.
%%
%% Gets the superclass name from the current class, looks up its pid,
%% and checks for the method. Recurses until the method is found or
%% the chain is exhausted (superclass = none).
-spec walk_superclass_chain(pid(), selector()) -> compiled_method() | nil.
walk_superclass_chain(ClassPid, Selector) ->
    case beamtalk_object_class:superclass(ClassPid) of
        none ->
            nil;
        SuperName ->
            case beamtalk_class_registry:whereis_class(SuperName) of
                undefined ->
                    nil;
                SuperPid ->
                    case gen_server:call(SuperPid, {method, Selector}) of
                        nil ->
                            walk_superclass_chain(SuperPid, Selector);
                        Method ->
                            Method
                    end
            end
    end.
