%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Sole cross-context API boundary between Workspace and Runtime.
%%%
%%% **DDD Context:** Object System Context / Workspace Context boundary
%%%
%%% This module is the **single approved entry point** for workspace code
%%% (`beamtalk_workspace/src/*.erl`) to call into the runtime. All direct
%%% calls to internal runtime modules from workspace source files must be
%%% routed through this facade.
%%%
%%% Adding a function here constitutes explicit approval of the cross-context
%%% call. Any call to a runtime module not delegated from here requires team
%%% review and a corresponding update to `docs/development/erlang-guidelines.md`.
%%%
%%% See `docs/development/erlang-guidelines.md` § Approved Cross-Context API.
-module(beamtalk_runtime_api).

-include("beamtalk.hrl").

%%% ===================================================================
%%% Class Registry
%%% ===================================================================
-export([
    all_classes/0,
    whereis_class/1,
    user_classes/0,
    inherits_from/2,
    class_object_tag/1,
    class_display_name/1,
    is_class_name/1,
    drain_class_warnings_by_names/1,
    drain_pending_load_errors_by_names/1,
    get_method_return_type/2,
    get_class_method_return_type/2
]).

%%% ===================================================================
%%% Class Objects
%%% ===================================================================
-export([
    class_name/1,
    module_name/1,
    set_class_var/3,
    start_link_class/2,
    class_methods/1,
    local_class_methods/1,
    local_class_methods_map/1,
    local_instance_methods/1,
    instance_variables/1,
    superclass/1
]).

%%% ===================================================================
%%% Class Removal (BT-1239)
%%% ===================================================================
-export([
    remove_class_from_system/1
]).

%%% ===================================================================
%%% Class Introspection (docs, sealing, abstractness)
%%% ===================================================================
-export([
    is_sealed/1,
    is_abstract/1
]).

%%% ===================================================================
%%% Object Instances
%%% ===================================================================
-export([
    all_instances/1
]).

%%% ===================================================================
%%% Message Dispatch
%%% ===================================================================
-export([
    dispatch_lookup/5,
    message_send/3
]).

%%% ===================================================================
%%% Reflection
%%% ===================================================================
-export([
    field_names/1
]).

%%% ===================================================================
%%% Tagged Maps
%%% ===================================================================
-export([
    tagged_map_class_of/1,
    is_tagged/1
]).

%%% ===================================================================
%%% Primitives
%%% ===================================================================
-export([
    print_string/1,
    primitive_class_of/1
]).

%%% ===================================================================
%%% Hot Reload
%%% ===================================================================
-export([
    trigger_code_change/2,
    trigger_code_change/3,
    hot_reload_code_change/3
]).

%%% ===================================================================
%%% Futures
%%% ===================================================================
-export([
    future_resolve/2,
    future_reject/2,
    future_await/2
]).

%%% ===================================================================
%%% Class Hierarchy Table
%%% ===================================================================
-export([
    hierarchy_foldl/2
]).

%%% ====================================================================
%%% Class Registry Delegators
%%% ====================================================================

-spec all_classes() -> [pid()].
all_classes() ->
    beamtalk_class_registry:all_classes().

-spec whereis_class(atom()) -> pid() | undefined.
whereis_class(ClassName) ->
    beamtalk_class_registry:whereis_class(ClassName).

-spec user_classes() -> [beamtalk_class_registry:user_class_entry()].
user_classes() ->
    beamtalk_class_registry:user_classes().

-spec inherits_from(atom() | none, atom()) -> boolean().
inherits_from(ClassName, SuperName) ->
    beamtalk_class_registry:inherits_from(ClassName, SuperName).

-spec class_object_tag(atom()) -> atom().
class_object_tag(ClassName) ->
    beamtalk_class_registry:class_object_tag(ClassName).

-spec class_display_name(atom()) -> binary().
class_display_name(Tag) ->
    beamtalk_class_registry:class_display_name(Tag).

-spec is_class_name(atom()) -> boolean().
is_class_name(Name) ->
    beamtalk_class_registry:is_class_name(Name).

-spec drain_class_warnings_by_names([atom()]) -> [{atom(), atom(), atom()}].
drain_class_warnings_by_names(Names) ->
    beamtalk_class_registry:drain_class_warnings_by_names(Names).

-spec drain_pending_load_errors_by_names([atom()]) -> [{atom(), #beamtalk_error{}}].
drain_pending_load_errors_by_names(Names) ->
    beamtalk_class_registry:drain_pending_load_errors_by_names(Names).

-spec get_method_return_type(atom(), atom()) -> {ok, atom() | tuple()} | {error, not_found}.
get_method_return_type(ClassName, Selector) ->
    beamtalk_class_registry:get_method_return_type(ClassName, Selector).

-spec get_class_method_return_type(atom(), atom()) -> {ok, atom() | tuple()} | {error, not_found}.
get_class_method_return_type(ClassName, Selector) ->
    beamtalk_class_registry:get_class_method_return_type(ClassName, Selector).

%%% ====================================================================
%%% Class Removal (BT-1239)
%%% ====================================================================

%% @doc Remove a class from the system by name, with full cleanup.
%%
%% Returns the BEAM module atom of the removed class on success (so the caller
%% can update its own tracking state), or {error, Reason} if the removal fails
%% (class not found, stdlib class, has subclasses, etc.).
-spec remove_class_from_system(atom()) -> {ok, module()} | {error, #beamtalk_error{}}.
remove_class_from_system(ClassName) ->
    try
        %% Look up the module name inside the protected block to catch races
        %% where the class dies between whereis_class/1 and module_name/1.
        ModuleName =
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined -> undefined;
                ClassPid -> beamtalk_object_class:module_name(ClassPid)
            end,
        beamtalk_behaviour_intrinsics:classRemoveFromSystemByName(ClassName),
        {ok, ModuleName}
    catch
        error:#{error := #beamtalk_error{} = Error} -> {error, Error}
    end.

%%% ====================================================================
%%% Class Object Delegators
%%% ====================================================================

-spec class_name(pid()) -> atom().
class_name(Pid) ->
    beamtalk_object_class:class_name(Pid).

-spec module_name(pid()) -> module().
module_name(Pid) ->
    beamtalk_object_class:module_name(Pid).

-spec set_class_var(atom(), atom(), term()) -> term().
set_class_var(ClassName, VarName, Value) ->
    beamtalk_object_class:set_class_var(ClassName, VarName, Value).

-spec start_link_class(atom(), map()) -> {ok, pid()} | {error, term()}.
start_link_class(ClassName, ClassInfo) ->
    beamtalk_object_class:start_link(ClassName, ClassInfo).

-spec class_methods(pid()) -> [atom()].
class_methods(Pid) ->
    beamtalk_object_class:methods(Pid).

-spec local_class_methods(pid()) -> [atom()].
local_class_methods(Pid) ->
    beamtalk_object_class:local_class_methods(Pid).

-spec local_class_methods_map(pid()) -> map().
local_class_methods_map(Pid) ->
    beamtalk_object_class:local_class_methods_map(Pid).

-spec local_instance_methods(pid()) -> [atom()].
local_instance_methods(Pid) ->
    beamtalk_object_class:local_instance_methods(Pid).

-spec instance_variables(pid()) -> [atom()].
instance_variables(Pid) ->
    beamtalk_object_class:instance_variables(Pid).

-spec superclass(pid()) -> atom() | none.
superclass(Pid) ->
    beamtalk_object_class:superclass(Pid).

%%% ====================================================================
%%% Class Introspection Delegators
%%% ====================================================================

-spec is_sealed(pid()) -> boolean().
is_sealed(ClassPid) ->
    beamtalk_object_class:is_sealed(ClassPid).

-spec is_abstract(pid()) -> boolean().
is_abstract(ClassPid) ->
    beamtalk_object_class:is_abstract(ClassPid).

%%% ====================================================================
%%% Object Instances Delegators
%%% ====================================================================

-spec all_instances(atom()) -> [pid()].
all_instances(ClassName) ->
    beamtalk_object_instances:all(ClassName).

%%% ====================================================================
%%% Message Dispatch Delegators
%%% ====================================================================

-spec dispatch_lookup(atom(), list(), term(), term(), atom()) ->
    {reply, term(), term()} | {error, #beamtalk_error{}}.
dispatch_lookup(Selector, Args, Self, State, ClassName) ->
    beamtalk_dispatch:lookup(Selector, Args, Self, State, ClassName).

-spec message_send(term(), atom(), list()) -> term().
message_send(Receiver, Selector, Args) ->
    beamtalk_message_dispatch:send(Receiver, Selector, Args).

%%% ====================================================================
%%% Reflection Delegators
%%% ====================================================================

-spec field_names(map()) -> [atom()].
field_names(Tagged) ->
    beamtalk_reflection:field_names(Tagged).

%%% ====================================================================
%%% Tagged Map Delegators
%%% ====================================================================

-spec tagged_map_class_of(term()) -> atom() | undefined.
tagged_map_class_of(Value) ->
    beamtalk_tagged_map:class_of(Value).

-spec is_tagged(term()) -> boolean().
is_tagged(Value) ->
    beamtalk_tagged_map:is_tagged(Value).

%%% ====================================================================
%%% Primitive Delegators
%%% ====================================================================

-spec print_string(term()) -> binary().
print_string(Value) ->
    beamtalk_primitive:print_string(Value).

-spec primitive_class_of(term()) -> atom().
primitive_class_of(Value) ->
    beamtalk_primitive:class_of(Value).

%%% ====================================================================
%%% Hot Reload Delegators
%%% ====================================================================

-spec trigger_code_change(atom(), [pid()]) ->
    {ok, non_neg_integer(), [{pid(), term()}]}.
trigger_code_change(Mod, Pids) ->
    beamtalk_hot_reload:trigger_code_change(Mod, Pids).

-spec trigger_code_change(atom(), [pid()], term()) ->
    {ok, non_neg_integer(), [{pid(), term()}]}.
trigger_code_change(Mod, Pids, Extra) ->
    beamtalk_hot_reload:trigger_code_change(Mod, Pids, Extra).

-spec hot_reload_code_change(term(), term(), term()) ->
    {ok, term()} | {error, term()}.
hot_reload_code_change(OldVsn, State, Extra) ->
    beamtalk_hot_reload:code_change(OldVsn, State, Extra).

%%% ====================================================================
%%% Future Delegators
%%% ====================================================================

-spec future_resolve(beamtalk_future:future() | pid(), term()) -> ok.
future_resolve(FuturePid, Value) ->
    beamtalk_future:resolve(FuturePid, Value).

-spec future_reject(beamtalk_future:future() | pid(), term()) -> ok.
future_reject(FuturePid, Reason) ->
    beamtalk_future:reject(FuturePid, Reason).

-spec future_await(beamtalk_future:future() | pid() | term(), timeout()) -> term().
future_await(Future, Timeout) ->
    beamtalk_future:await(Future, Timeout).

%%% ====================================================================
%%% Class Hierarchy Table Delegators
%%% ====================================================================

-spec hierarchy_foldl(fun(), term()) -> term().
hierarchy_foldl(Fun, Acc) ->
    beamtalk_class_hierarchy_table:foldl(Fun, Acc).
