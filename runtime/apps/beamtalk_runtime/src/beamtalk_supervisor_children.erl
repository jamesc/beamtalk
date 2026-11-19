%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor_children).

%%% **DDD Context:** Runtime Context

-moduledoc """
Introspection internals for a supervisor's children: turning OTP child entries
into child handles and classes, and the shared child-id lookup.

Split out of `beamtalk_supervisor` (BT-5047); the entry points generated code and
recorded child MFAs call keep their module and name there.
""".

-include("beamtalk.hrl").
-include("beamtalk_supervisor_config.hrl").
-include("beamtalk_supervisor_capacity.hrl").
-include_lib("kernel/include/logger.hrl").
-include("beamtalk_supervisor_internal.hrl").
-export([
    child_object/3,
    children_for_module/2,
    class_for_modules/1,
    entry_class/2,
    handle_terminate_child_result/3,
    module_classes/1,
    restarted_child/3,
    supervisor_child_entries/1,
    supervisor_child_entry/2,
    with_child_id/4
]).

%% Shared outcome dispatcher for both static and dynamic terminateChild paths.
%% `ok` → {ok, nil}; `{error, not_found}` → {ok, nil} (idempotent);
%% other `{error, Reason}` → {error, #beamtalk_error{kind = terminate_failed}}.
-spec handle_terminate_child_result(
    atom(), map(), ok | {error, not_found} | {error, term()}
) ->
    {ok, nil} | {error, #beamtalk_error{}}.

handle_terminate_child_result(SupClass, LogCtx, ok) ->
    ?LOG_INFO(
        "Supervisor child terminated",
        (LogCtx)#{supervisor => SupClass, domain => [beamtalk, runtime]}
    ),
    {ok, nil};
handle_terminate_child_result(SupClass, LogCtx, {error, not_found}) ->
    %% ADR 0080 Phase 1: idempotent — "child already gone" is success.
    ?LOG_DEBUG(
        "Supervisor child already terminated",
        (LogCtx)#{supervisor => SupClass, domain => [beamtalk, runtime]}
    ),
    {ok, nil};
handle_terminate_child_result(SupClass, _LogCtx, {error, Reason}) ->
    {error, beamtalk_error:new_fmt(terminate_failed, SupClass, 'terminateChild:', "~tp", [Reason])}.

%%% ============================================================================
%%% Child introspection by id (BT-3966)
%%% ============================================================================
-doc """
Map `supervisor:which_children/1` output to `SupervisorChild` values in
declaration order. OTP lists a static supervisor's children newest first, so
the list is reversed. Module-to-class resolution reads the class metadata
table once for the whole list.
""".
-spec supervisor_child_entries([beamtalk_supervisor:which_child()]) ->
    [beamtalk_supervisor:supervisor_child()].
supervisor_child_entries(Children) ->
    ModClasses = module_classes(Children),
    lists:reverse([supervisor_child_entry(Entry, ModClasses) || Entry <- Children]).

-doc """
Map one `supervisor:which_children/1` entry to a `SupervisorChild` value
(the tagged map behind `stdlib/src/supervisor_child.bt`). A pid gives
`status => running` and the wrapped child object; `restarting` gives
`status => restarting`, anything else (`undefined`) `status => stopped`, both
with `child => nil`. `ModClasses` maps child modules to Beamtalk class names
(`module_classes/1`); a module with no class gives `childClass => nil` and,
when running, the bare pid as `child`. Never raises.
""".
-spec supervisor_child_entry(beamtalk_supervisor:which_child(), #{module() => atom()}) ->
    beamtalk_supervisor:supervisor_child().
supervisor_child_entry({Id, PidOrStatus, Type, Mods}, ModClasses) ->
    {ClassName, ChildModule} = entry_class(Mods, ModClasses),
    {Status, Child} =
        case PidOrStatus of
            ChildPid when is_pid(ChildPid) ->
                {running, child_object(ClassName, ChildModule, ChildPid)};
            restarting ->
                {restarting, nil};
            _ ->
                {stopped, nil}
        end,
    #{
        ?BT_CLASS_TAG_KEY => 'SupervisorChild',
        id => Id,
        child => Child,
        status => Status,
        type => Type,
        childClass => child_class_object(ClassName)
    }.

%% `{ClassName | nil, Module | nil}` for one child's modules: the single-child
%% counterpart of `module_classes/1` + `entry_class/2`.
-spec class_for_modules([module()] | dynamic) -> {atom() | nil, module() | nil}.

class_for_modules(Mods) ->
    entry_class(Mods, beamtalk_class_registry:module_to_class_map()).

%% Module -> Beamtalk class name map for resolving many entries' modules: one
%% read of the class metadata table (`module_to_class_map/0`), not one per
%% child, and none when there are no entries.
-spec module_classes([beamtalk_supervisor:which_child()]) -> #{module() => atom()}.

module_classes([]) ->
    #{};
module_classes(_Children) ->
    beamtalk_class_registry:module_to_class_map().

-spec entry_class([module()] | dynamic, #{module() => atom()}) ->
    {atom() | nil, module() | nil}.

entry_class([Module | _], ModClasses) when is_atom(Module) ->
    {maps:get(Module, ModClasses, nil), Module};
entry_class(_Mods, _ModClasses) ->
    {nil, nil}.

-spec child_object(atom() | nil, module() | nil, pid()) -> beamtalk_object() | pid().

child_object(nil, _Module, ChildPid) ->
    ChildPid;
child_object(ClassName, Module, ChildPid) ->
    beamtalk_supervisor_announce:wrap_child(ClassName, Module, ChildPid).

-spec child_class_object(atom() | nil) -> beamtalk_object() | nil.

child_class_object(nil) ->
    nil;
child_class_object(ClassName) ->
    case beamtalk_class_registry:resolve_class_object(ClassName) of
        undefined -> nil;
        ClassObj -> ClassObj
    end.

%% The `which_children` entries whose modules include `Module`, in any state.
-spec children_for_module([beamtalk_supervisor:which_child()], module()) ->
    [beamtalk_supervisor:which_child()].

%% A `simple_one_for_one` child has no id (`undefined`) and cannot be addressed
%% by id, so it never matches: `terminate:`/`which:` by class on a
%% DynamicSupervisor find nothing rather than an `undefined` id per child.
children_for_module(Children, Module) ->
    [
        Entry
     || {Id, _P, _T, Mods} = Entry <- Children,
        Id =/= undefined,
        is_list(Mods),
        lists:member(Module, Mods)
    ].

%% The child object for a pid `supervisor:restart_child/2` just returned.
-spec restarted_child(pid(), atom() | integer(), pid() | undefined) ->
    beamtalk_object() | pid() | nil.

restarted_child(SupPid, Id, ChildPid) when is_pid(ChildPid) ->
    %% `get_childspec/2` reads one spec, not the whole child list.
    case supervisor:get_childspec(SupPid, Id) of
        {ok, #{modules := Mods}} ->
            {ClassName, Module} = class_for_modules(Mods),
            child_object(ClassName, Module, ChildPid);
        _ ->
            ChildPid
    end;
restarted_child(_SupPid, _Id, _NoPid) ->
    nil.

%% Validate a child id (a Symbol or an Integer, ADR 0135 §3) and run `Fun`
%% against the live supervisor.
-spec with_child_id(tuple(), atom(), term(), fun(() -> Result)) ->
    Result | {error, #beamtalk_error{}}
when
    Result :: term().

with_child_id(Self, Selector, Id, Fun) when is_atom(Id); is_integer(Id) ->
    beamtalk_supervisor:with_live_supervisor(Self, Selector, Fun);
with_child_id(Self, Selector, Id, _Fun) ->
    SupClass = element(2, Self),
    Error = beamtalk_error:new(
        type_error,
        SupClass,
        Selector,
        <<"A child id is a Symbol or an Integer">>
    ),
    {error, beamtalk_error:with_details(Error, #{id => Id})}.
