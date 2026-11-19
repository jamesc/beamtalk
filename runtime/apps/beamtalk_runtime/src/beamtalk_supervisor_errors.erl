%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor_errors).

%%% **DDD Context:** Runtime Context

-moduledoc """
Structured `#beamtalk_error{}` builders for supervisor handle operations (start
failures, child lookups, stop timeouts, an unavailable supervisor).

Split out of `beamtalk_supervisor` (BT-5047); the entry points generated code and
recorded child MFAs call keep their module and name there.
""".

-include("beamtalk.hrl").
-include("beamtalk_supervisor_config.hrl").
-include("beamtalk_supervisor_capacity.hrl").
-include_lib("kernel/include/logger.hrl").
-include("beamtalk_supervisor_internal.hrl").
-export([
    ambiguous_child_error/3,
    child_not_found_error/3,
    child_running_error/4,
    delete_failed_error/3,
    start_failed_error/4,
    stop_timeout_error/2,
    supervisor_unavailable_error/3
]).

-doc """
Build the `Kind` error (`supervisor_start_failed` for `supervise`,
`child_start_failed` for `startChild`) for a failed start.

A configuration failure raised inside a supervisor's `init/1`
(`static_init/3`, `dynamic_init/3`) exits with a `#beamtalk_error{}`
(ADR 0135 §2): for `supervise` that is the start reason, and for a
DynamicSupervisor whose child class is a supervisor it is the child's start
reason; for a static parent whose nested supervisor child failed it is
wrapped in OTP's `{shutdown, {failed_to_start_child, Id, _}}`. Its hint (the
validator's message, or the `calling_self` hint) — or, when it has none, its
message — becomes the hint of the start error, with the original error in
`details`. Any other reason is formatted into the hint as before.
""".
-spec start_failed_error(atom(), atom(), atom(), term()) -> #beamtalk_error{}.
start_failed_error(Kind, ClassName, Selector, #beamtalk_error{} = Cause) ->
    Error0 = beamtalk_error:new(Kind, ClassName, Selector, cause_hint(Cause)),
    beamtalk_error:with_details(Error0, #{cause => Cause});
start_failed_error(Kind, ClassName, Selector, {initialize_failed, #beamtalk_error{} = Cause}) ->
    %% BT-4033: an actor child whose `initialize` failed (start_actor_child/3).
    start_failed_error(Kind, ClassName, Selector, Cause);
start_failed_error(
    Kind, ClassName, Selector, {shutdown, {failed_to_start_child, _Id, _Inner}} = Reason
) ->
    case nested_start_cause(Reason) of
        {ok, Cause} -> start_failed_error(Kind, ClassName, Selector, Cause);
        none -> format_start_failed_error(Kind, ClassName, Selector, Reason)
    end;
start_failed_error(Kind, ClassName, Selector, Reason) ->
    format_start_failed_error(Kind, ClassName, Selector, Reason).

%% The `#beamtalk_error{}` at the bottom of any depth of nested
%% `failed_to_start_child` wrappers (Root -> MidSup -> Pool), if there is one.
-spec nested_start_cause(term()) -> {ok, #beamtalk_error{}} | none.

nested_start_cause(#beamtalk_error{} = Cause) ->
    {ok, Cause};
nested_start_cause({initialize_failed, #beamtalk_error{} = Cause}) ->
    {ok, Cause};
nested_start_cause({shutdown, {failed_to_start_child, _Id, Inner}}) ->
    nested_start_cause(Inner);
nested_start_cause(_) ->
    none.

-spec format_start_failed_error(atom(), atom(), atom(), term()) -> #beamtalk_error{}.

format_start_failed_error(Kind, ClassName, Selector, Reason) ->
    Verb =
        case Kind of
            child_start_failed -> "start_child";
            _ -> "start_link"
        end,
    beamtalk_error:new_fmt(Kind, ClassName, Selector, "supervisor ~ts failed: ~tp", [Verb, Reason]).

%% The text to show for a start failure's cause: its hint, else its message,
%% else its kind.
-spec cause_hint(#beamtalk_error{}) -> binary().

%% BT-4173: message/hint fields are tagged `String`s (see beamtalk.hrl).
cause_hint(#beamtalk_error{hint = {beamtalk_string, Hint}}) when is_binary(Hint), Hint =/= <<>> ->
    Hint;
cause_hint(#beamtalk_error{message = {beamtalk_string, Message}}) when
    is_binary(Message), Message =/= <<>>
->
    Message;
cause_hint(#beamtalk_error{kind = Kind}) ->
    atom_to_binary(Kind, utf8).

-spec child_not_found_error(atom(), atom(), atom() | integer()) -> #beamtalk_error{}.

child_not_found_error(SupClass, Selector, Id) ->
    Error0 = beamtalk_error:new(child_not_found, SupClass, Selector),
    Error1 = beamtalk_error:with_message_fmt(Error0, "No child with id ~tp", [Id]),
    beamtalk_error:with_details(Error1, #{id => Id}).

-spec child_running_error(atom(), atom(), atom() | integer(), running | restarting) ->
    #beamtalk_error{}.

child_running_error(SupClass, Selector, Id, Status) ->
    Error0 = beamtalk_error:new(
        child_running,
        SupClass,
        Selector,
        <<"Terminate the child with terminateChild: first">>
    ),
    Error1 = beamtalk_error:with_message_fmt(Error0, "Child ~tp is ~ts", [Id, Status]),
    beamtalk_error:with_details(Error1, #{id => Id, status => Status}).

-spec ambiguous_child_error(atom(), atom(), [beamtalk_supervisor:which_child()]) ->
    #beamtalk_error{}.

ambiguous_child_error(SupClass, Selector, Matches) ->
    %% Declaration order, as `whichChildren` lists them.
    Ids = lists:reverse([Id || {Id, _, _, _} <- Matches]),
    Error0 = beamtalk_error:new(
        ambiguous_child,
        SupClass,
        Selector,
        <<"Several children share this class; use childWithId: / terminateChild: with an id">>
    ),
    Error1 = beamtalk_error:with_message_fmt(Error0, "Several children share this class: ~tp", [Ids]),
    beamtalk_error:with_details(Error1, #{ids => Ids}).

-spec delete_failed_error(atom(), atom(), term()) -> #beamtalk_error{}.

delete_failed_error(SupClass, Selector, Reason) ->
    beamtalk_error:new_fmt(delete_failed, SupClass, Selector, "~tp", [Reason]).

-doc """
The `{error, #beamtalk_error{}}` for a `stopWithin:` wait the shutdown
overran (BT-4047): `kind = timeout`, naming the wait in the hint and
`details`, since the supervisor keeps shutting down in the background.
""".
-spec stop_timeout_error(atom(), non_neg_integer() | infinity) -> #beamtalk_error{}.
stop_timeout_error(ClassName, Ms) ->
    Error0 = beamtalk_error:new_fmt(
        timeout,
        ClassName,
        'stopWithin:',
        "Supervisor did not stop within ~tp ms; it keeps shutting down in the background",
        [Ms]
    ),
    beamtalk_error:with_details(Error0, #{timeout_ms => Ms}).

-doc """
The `{error, #beamtalk_error{}}` for a supervisor the handle `Self` could not
reach: `stale_handle` (`stale_handle_error/2`) for `dead`; for
`{unreachable, Why}`, `node_down` (the kind a remote actor send reports for
the same condition, ADR 0126 §7.1) with `details => #{node => Node, reason =>
Why}`.
""".
-spec supervisor_unavailable_error(tuple(), atom(), beamtalk_supervisor:supervisor_unavailable()) ->
    {error, #beamtalk_error{}}.
supervisor_unavailable_error(Self, Selector, dead) ->
    beamtalk_supervisor_pool:stale_handle_error(element(2, Self), Selector);
supervisor_unavailable_error(Self, Selector, {unreachable, Why}) ->
    ClassName = element(2, Self),
    Node = node(element(4, Self)),
    ?LOG_WARNING("Supervisor node unreachable", #{
        supervisor => ClassName,
        selector => Selector,
        node => Node,
        reason => Why,
        domain => [beamtalk, runtime]
    }),
    Error0 = beamtalk_error:new(
        node_down,
        ClassName,
        Selector,
        <<"The supervisor may still be running; retry when its node is reachable">>
    ),
    Error1 = beamtalk_error:with_message_fmt(
        Error0, "Supervisor's node ~ts is unreachable (~ts)", [Node, Why]
    ),
    {error, beamtalk_error:with_details(Error1, #{node => Node, reason => Why})}.
