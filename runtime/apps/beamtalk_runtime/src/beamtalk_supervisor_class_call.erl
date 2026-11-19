%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor_class_call).

%%% **DDD Context:** Runtime Context

-moduledoc """
Calling a class method in the supervisor's init or a child start without a
round trip to the class process, and the structured error for an actor child
that failed to start.

Split out of `beamtalk_supervisor` (BT-5047); the entry points generated code and
recorded child MFAs call keep their module and name there.
""".

-include("beamtalk.hrl").
-include("beamtalk_supervisor_config.hrl").
-include("beamtalk_supervisor_capacity.hrl").
-include_lib("kernel/include/logger.hrl").
-include("beamtalk_supervisor_internal.hrl").
-export([
    actor_child_start_error/2,
    call_class_method_direct/5,
    make_init_class_self/2
]).

%% BT-4033: the hint for a supervised actor child whose `initialize` did not
%% return within the start timeout (`?BT_INITIALIZE_TIMEOUT`, formatted in).
-define(ACTOR_CHILD_INIT_TIMEOUT_HINT_FMT,
    "initialize did not return within ~b ms. A supervisor waits for each actor "
    "child's initialize, so a synchronous call from initialize to the child's "
    "supervisor or any ancestor (which:, childWithId:, children, a send to a "
    "sibling not yet started), or to the actor or class that is starting the "
    "supervisor or calling startChild, blocks until this timeout (ADR 0135 "
    "hang rule). Move such calls, and slow setup, into a continueWith: step."
).

-doc """
Build a ClassSelf tuple for use in direct class method calls during supervisor init.
The pid field is set to the class gen_server pid (may be blocked, but ClassSelf is
used only as a value object — pure class methods do not send messages to self).
""".
-spec make_init_class_self(atom(), module()) -> beamtalk_object().
make_init_class_self(ClassName, Module) ->
    ClassPid = beamtalk_class_registry:whereis_class(ClassName),
    ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
    {beamtalk_object, ClassTag, Module, ClassPid}.

-doc """
Call a class method directly by invoking the module function, bypassing the
class gen_server, with extra user-facing arguments. Tries the subclass module
first, then walks the class hierarchy via ETS until the method is found in an
ancestor's module.
The compiled convention is `class_<sel>(ClassSelf, Args...)` returning the bare
result (ADR 0130 §3), so the arity is `1 + length(ExtraArgs)`. The call runs in
`beamtalk_class_vars:with_snapshot/2`: class-variable reads see the ETS mirror
(or the live map if this process is already inside the class's invocation) and
writes raise `class_state_read_only`.
""".
-spec call_class_method_direct(atom(), module(), atom(), tuple(), [term()]) -> term().
call_class_method_direct(ClassName, Module, FunName, ClassSelf, ExtraArgs) ->
    beamtalk_class_vars:with_snapshot(ClassSelf, fun() ->
        Arity = 1 + length(ExtraArgs),
        case erlang:function_exported(Module, FunName, Arity) of
            true ->
                erlang:apply(Module, FunName, [ClassSelf | ExtraArgs]);
            false ->
                call_inherited_class_method_direct(ClassName, FunName, ClassSelf, ExtraArgs, 0)
        end
    end).

-spec call_inherited_class_method_direct(
    atom(), atom(), tuple(), [term()], non_neg_integer()
) ->
    term().

call_inherited_class_method_direct(_ClassName, FunName, _ClassSelf, _ExtraArgs, Depth) when
    Depth > 30
->
    error({supervisor_init_method_not_found, FunName});
call_inherited_class_method_direct(ClassName, FunName, ClassSelf, ExtraArgs, Depth) ->
    Arity = 1 + length(ExtraArgs),
    case beamtalk_class_metadata:lookup_superclass(ClassName) of
        not_found ->
            error({supervisor_init_method_not_found, FunName});
        {ok, SuperclassName} ->
            %% Look up the ancestor module via ETS instead of gen_server:call.
            %% If the ancestor is itself a Supervisor subclass currently being initialised,
            %% its class gen_server is blocked inside startLink/1 waiting for OTP
            %% supervisor:start_link to return.  A gen_server:call to it would deadlock.
            %% beamtalk_class_metadata stores module names written during class init
            %% and is safe to read from any process without coordination.
            case beamtalk_class_metadata:lookup_module(SuperclassName) of
                not_found ->
                    %% Class not yet registered or module not yet recorded — skip upward.
                    call_inherited_class_method_direct(
                        SuperclassName, FunName, ClassSelf, ExtraArgs, Depth + 1
                    );
                {ok, SuperModule} ->
                    case erlang:function_exported(SuperModule, FunName, Arity) of
                        true ->
                            erlang:apply(SuperModule, FunName, [ClassSelf | ExtraArgs]);
                        false ->
                            call_inherited_class_method_direct(
                                SuperclassName, FunName, ClassSelf, ExtraArgs, Depth + 1
                            )
                    end
            end
    end.

%% BT-4033: the reason a supervised actor child's start failed.
%%
%% A top-level `#beamtalk_error{}` is the start's own error (`spawnAs/3`'s
%% `#name_registered`, `#reserved_name`, `#type_error`) and is kept as it is.
%% A failed `initialize` becomes `{initialize_failed, Error}`, with the
%% `#beamtalk_error{}` lifted out of its stop reason, so the parent's
%% `#supervisor_start_failed` / `#child_start_failed` hint carries the
%% child's own error (`start_failed_error/4` unwraps it) and the error's kind
%% is never mistaken for the start's own (an `initialize` that itself hits a
%% `#name_registered`). An init timeout names the ADR 0135 hang rule. A stop
%% reason with no structured error (a hand-written gen_server's own) is kept.
-spec actor_child_start_error(module(), term()) -> term().

actor_child_start_error(_ChildModule, #beamtalk_error{} = Error) ->
    Error;
actor_child_start_error(ChildModule, timeout) ->
    {initialize_failed,
        beamtalk_error:new_fmt(
            instantiation_error,
            child_class_name(ChildModule),
            initialize,
            ?ACTOR_CHILD_INIT_TIMEOUT_HINT_FMT,
            [?BT_INITIALIZE_TIMEOUT]
        )};
actor_child_start_error(_ChildModule, Reason) ->
    %% The stop reason of a raising `initialize` nests the error in an
    %% exception object and `{Error, Stacktrace}` / `{error, _}` tuples.
    case beamtalk_error:extract_beamtalk_error(Reason, 3) of
        #beamtalk_error{} = Error -> {initialize_failed, Error};
        undefined -> Reason
    end.

%% The Beamtalk class name of an actor module, for error attribution; the
%% module itself when it is not a registered class (a hand-written
%% gen_server).
-spec child_class_name(module()) -> atom().

child_class_name(ChildModule) ->
    case beamtalk_class_registry:class_name_for_module(ChildModule) of
        {ok, ClassName} -> ClassName;
        not_found -> ChildModule
    end.
