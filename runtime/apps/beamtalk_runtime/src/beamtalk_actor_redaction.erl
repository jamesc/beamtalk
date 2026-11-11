%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Actor System Context

-module(beamtalk_actor_redaction).
-moduledoc """
Redaction of an actor's secret state and secret messages in its
`format_status/1` (BT-4567).

An actor class declares what is secret with two class-side methods,
`class secretState` (state slot names) and `class secretSelectors` (message
selectors). The compiler reads both at compile time and bakes them into the
actor module's generated `'__beamtalk_secrets'/0`, which answers
`{Slots, Selectors}` for the class and all its superclasses (each class's
function appends its own lists to its superclass's, through
`merge_secrets/3`). Every compiled actor module exports
`format_status/1`, which calls `format_status/2` here with its own module.

No user code runs on this path: `'__beamtalk_secrets'/0` is compiled
constant data, so redaction cannot raise, hang or leak through a user
method. If anything here fails anyway, `format_status/2` fails closed: the
state, message and log are all replaced.

## What is redacted

`format_status/1` is called by `sys:get_status/1` (keys `state`, `log`) and
by `gen_server`'s crash report (keys `state`, `message`, `reason`, `log`).

- `state`: each secret slot's value becomes `<<"[redacted]">>`.
- `message`: a send of a secret selector keeps its selector but its
  arguments become `<<"[redacted]">>`.
- `log`: each `sys` log entry is redacted the same way: incoming sends,
  states, continuations, and the reply to a secret call. A reply whose
  incoming call is no longer in the log (the log is a bounded ring) is also
  redacted, since it may have answered a secret call.
- `reason` is left as is.

A crashing actor also gets a `proc_lib` crash report, which does not go
through `format_status/1`. The logger primary filter crash_report_filter/2
(installed when the runtime starts, BT-4666) redacts it instead: secret
sends still queued in its `messages` (the mailbox) and secret-selector
entries in the pending `continueWith:` queue in its `dictionary`.

`sys:get_state/1`, `sys:replace_state/2`, `sys:log(Pid, get)`,
`sys:log(Pid, print)` and `sys:trace/2` do not go through `format_status/1`
(OTP gives them no hook), so they show raw values. Neither do the raw
process dictionary and `Debug` terms `sys:get_status/1` returns next to the
formatted status: with `sys:log` on, the debug term holds the raw log. Only
the formatted part (the "State" and "Logged events" data) is redacted. These
are debugging tools with full access to the process.

## Message shapes

The request shapes recognised are those `beamtalk_actor`'s send helpers
put on the wire:

- sync call: `{Selector, Args, PropCtx}` (or `{Selector, Args}`), and
  `{'$beamtalk_wire', V, call, Selector, WireArgs, PropCtx}` from another node;
- fire-and-forget cast: `{cast, Selector, Args, PropCtx}` (or without
  `PropCtx`), and `{'$beamtalk_wire', V, cast, Selector, WireArgs, PropCtx}`;
- async send: `{Selector, Args, FuturePid, PropCtx}`, and
  `{'$beamtalk_wire', V, async, Selector, WireArgs, FuturePid, PropCtx}`;
- continuation: `{continue, {'$bt_continue', [{Selector, Args}]}}`.

A `perform:` send naming a secret selector, at any depth of nested
`perform:` forms, is treated as a send of that selector.
""".

-include("beamtalk.hrl").

-export([format_status/2, merge_secrets/3, redacted/0]).
-export([install_crash_report_filter/0, remove_crash_report_filter/0, crash_report_filter/2]).

-define(REDACTED, <<"[redacted]">>).

%% BT-4666: the logger primary filter that redacts `proc_lib` crash reports.
-define(CRASH_FILTER_ID, beamtalk_actor_secrets).

-type secrets() :: {Slots :: [atom()], Selectors :: [atom()]}.
-export_type([secrets/0]).

-doc "The value that stands in for a redacted term.".
-spec redacted() -> binary().
redacted() -> ?REDACTED.

-doc """
`'__beamtalk_secrets'/0` helper: append a class's own secret slots and
selectors to its superclass's `{Slots, Selectors}`.
""".
-spec merge_secrets(secrets(), [atom()], [atom()]) -> secrets().
merge_secrets({ParentSlots, ParentSelectors}, OwnSlots, OwnSelectors) ->
    {ParentSlots ++ OwnSlots, ParentSelectors ++ OwnSelectors}.

-doc """
The body of every compiled actor's `format_status/1`. Reads `Module`'s
compiled secret metadata and redacts `Status`. Answers `Status` unchanged
when the class declares no secrets. Never raises: on any failure the state,
message and log are all replaced.
""".
-spec format_status(module(), map()) -> map().
format_status(Module, Status) ->
    %% bt-catcher-audit: not-applicable-no-block - '__beamtalk_secrets'/0 is a generated constant getter
    try Module:'__beamtalk_secrets'() of
        {[], []} ->
            Status;
        {Slots, Selectors} when is_list(Slots), is_list(Selectors) ->
            redact_status(Status, Slots, Selectors);
        _Malformed ->
            %% An exception in an `of` clause is not caught below.
            fail_closed(Status)
    catch
        _:_ -> fail_closed(Status)
    end.

-spec redact_status(map(), [atom()], [atom()]) -> map().
redact_status(Status, Slots, Selectors) ->
    try
        maps:map(
            fun
                (state, State) -> redact_state(State, Slots);
                (message, Msg) -> redact_msg(Msg, Selectors);
                (log, Log) when is_list(Log) -> redact_log(Log, Slots, Selectors);
                (_Key, Value) -> Value
            end,
            Status
        )
    catch
        _:_ -> fail_closed(Status)
    end.

-spec fail_closed(map()) -> map().
fail_closed(Status) ->
    maps:map(
        fun
            (state, _) -> ?REDACTED;
            (message, _) -> ?REDACTED;
            (log, _) -> [];
            (_Key, Value) -> Value
        end,
        Status
    ).

%%% proc_lib crash reports (BT-4666)

-doc """
Install the logger primary filter that redacts the `proc_lib` crash report
of a crashing actor (see crash_report_filter/2). Called once when the
runtime application starts; installing it again is a no-op.
""".
-spec install_crash_report_filter() -> ok.
install_crash_report_filter() ->
    case logger:add_primary_filter(?CRASH_FILTER_ID, {fun ?MODULE:crash_report_filter/2, []}) of
        ok -> ok;
        {error, {already_exist, _}} -> ok
    end.

-doc "Remove the filter install_crash_report_filter/0 installed.".
-spec remove_crash_report_filter() -> ok.
remove_crash_report_filter() ->
    _ = logger:remove_primary_filter(?CRASH_FILTER_ID),
    ok.

-doc """
Logger primary filter for the `proc_lib` crash report (`label =>
{proc_lib, crash}`) that OTP writes when a process crashes. That report does
not go through `format_status/1`: it lists the process's mailbox
(`messages`) and process dictionary (`dictionary`) raw.

For an actor whose class declares secrets, a queued send of a secret
selector keeps its selector but its arguments become `<<"[redacted]">>`, as
in format_status/2; so do secret-selector entries in the pending
`continueWith:` queue in the dictionary, and secret slots in the actor-state
cell if one is still installed. The actor is found by
the report's `initial_call`, `{Module, init, _}`, and its compiled
`'__beamtalk_secrets'/0`; no user code runs. Any other event, a non-actor
process, or an actor with no secrets passes unchanged.

The filter never raises: logger removes a primary filter that crashes. If
redacting an actor's report fails, its `messages` and `dictionary` are
replaced (fail closed).
""".
-spec crash_report_filter(logger:log_event(), term()) -> logger:filter_return().
crash_report_filter(
    #{msg := {report, #{label := {proc_lib, crash}, report := [Info | Linked]} = Report}} = Event,
    _Extra
) when is_list(Info) ->
    case actor_module(Info) of
        none ->
            ignore;
        Module ->
            Redacted = redact_crash_info(Module, Info),
            Event#{msg := {report, Report#{report := [Redacted | Linked]}}}
    end;
crash_report_filter(_Event, _Extra) ->
    ignore.

%% The compiled actor module behind a crash report, from its
%% `{initial_call, {Module, init, _}}` (gen_server's `'$initial_call'`), or
%% `none` for a process that is not a loaded Beamtalk actor.
-spec actor_module(list()) -> module() | none.
actor_module(Info) ->
    case lists:keyfind(initial_call, 1, Info) of
        {initial_call, {Module, init, _}} when is_atom(Module) ->
            case erlang:function_exported(Module, '__beamtalk_secrets', 0) of
                true -> Module;
                false -> none
            end;
        _ ->
            none
    end.

-spec redact_crash_info(module(), list()) -> list().
redact_crash_info(Module, Info) ->
    %% bt-catcher-audit: not-applicable-no-block - '__beamtalk_secrets'/0 is a generated constant getter
    try
        case Module:'__beamtalk_secrets'() of
            {[], []} ->
                Info;
            {Slots, Selectors} when is_list(Slots), is_list(Selectors) ->
                [redact_info_item(Item, Slots, Selectors) || Item <- Info]
        end
    catch
        _:_ -> fail_closed_info(Info)
    end.

-spec redact_info_item(term(), [atom()], [atom()]) -> term().
redact_info_item({messages, Messages}, _Slots, Selectors) when is_list(Messages) ->
    {messages, [redact_msg(Msg, Selectors) || Msg <- Messages]};
redact_info_item({dictionary, Dict}, Slots, Selectors) when is_list(Dict) ->
    {dictionary, [redact_dict_entry(Entry, Slots, Selectors) || Entry <- Dict]};
redact_info_item(Item, _Slots, _Selectors) ->
    Item.

%% Pending continuations to a secret selector, and secret slots in the
%% actor-state cell. Every crash path uninstalls the cell before re-raising,
%% so it is normally gone by the time proc_lib reports; redacting it anyway
%% keeps the secretState guarantee from resting on that.
-spec redact_dict_entry(term(), [atom()], [atom()]) -> term().
redact_dict_entry({?PENDING_CONTINUE_KEY, Queue}, _Slots, Selectors) when is_list(Queue) ->
    {?PENDING_CONTINUE_KEY, [redact_request(Step, Selectors) || Step <- Queue]};
redact_dict_entry(Entry, Slots, _Selectors) ->
    beamtalk_actor_state:map_cell_entry(Entry, fun(State) -> redact_state(State, Slots) end).

-spec fail_closed_info(list()) -> list().
fail_closed_info(Info) ->
    [
        case Item of
            {messages, _} -> {messages, ?REDACTED};
            {dictionary, _} -> {dictionary, ?REDACTED};
            _ -> Item
        end
     || Item <- Info
    ].

%%% State

-spec redact_state(term(), [atom()]) -> term().
redact_state(State, []) ->
    State;
redact_state(State, Slots) when is_map(State) ->
    lists:foldl(
        fun(Slot, Acc) ->
            case Acc of
                #{Slot := _} -> Acc#{Slot := ?REDACTED};
                _ -> Acc
            end
        end,
        State,
        Slots
    );
redact_state(State, _Slots) ->
    State.

%%% Messages

-spec redact_msg(term(), [atom()]) -> term().
redact_msg(Msg, []) ->
    Msg;
redact_msg({'$gen_call', From, Call}, Selectors) ->
    {'$gen_call', From, redact_request(Call, Selectors)};
redact_msg({'$gen_cast', Cast}, Selectors) ->
    {'$gen_cast', redact_request(Cast, Selectors)};
redact_msg({continue, {?BT_CONTINUE_TAG, Queue}}, Selectors) when is_list(Queue) ->
    {continue, {?BT_CONTINUE_TAG, [redact_request(Step, Selectors) || Step <- Queue]}};
redact_msg(Msg, Selectors) ->
    redact_request(Msg, Selectors).

-spec redact_request(term(), [atom()]) -> term().
redact_request({'$beamtalk_wire', V, Kind, Sel, WireArgs, Ctx}, Ss) when is_atom(Sel) ->
    {'$beamtalk_wire', V, Kind, Sel, redact_args(Sel, WireArgs, Ss), Ctx};
redact_request({'$beamtalk_wire', V, async, Sel, WireArgs, Fut, Ctx}, Ss) when is_atom(Sel) ->
    {'$beamtalk_wire', V, async, Sel, redact_args(Sel, WireArgs, Ss), Fut, Ctx};
redact_request({cast, Sel, Args, Ctx}, Ss) when is_atom(Sel), is_list(Args) ->
    {cast, Sel, redact_args(Sel, Args, Ss), Ctx};
redact_request({cast, Sel, Args}, Ss) when is_atom(Sel), is_list(Args) ->
    {cast, Sel, redact_args(Sel, Args, Ss)};
redact_request({Sel, Args, Fut, Ctx}, Ss) when is_atom(Sel), is_list(Args), is_pid(Fut) ->
    {Sel, redact_args(Sel, Args, Ss), Fut, Ctx};
redact_request({Sel, Args, Ctx}, Ss) when is_atom(Sel), is_list(Args) ->
    {Sel, redact_args(Sel, Args, Ss), Ctx};
redact_request({Sel, Args}, Ss) when is_atom(Sel), is_list(Args) ->
    {Sel, redact_args(Sel, Args, Ss)};
redact_request(Other, _Ss) ->
    Other.

-spec redact_args(atom(), term(), [atom()]) -> term().
redact_args(Sel, Args, Selectors) ->
    case is_secret_send(Sel, Args, Selectors) of
        true -> ?REDACTED;
        false -> Args
    end.

-doc """
Whether a send of `Sel` with `Args` is a send of a secret selector: `Sel`
itself is secret, or `Sel` is a `perform:` form that names a secret selector
at any depth (`beamtalk_actor:performed_selector/2`).
""".
-spec is_secret_send(atom(), term(), [atom()]) -> boolean().
is_secret_send(Sel, Args, Selectors) ->
    lists:member(Sel, Selectors) orelse
        lists:member(beamtalk_actor:performed_selector(Sel, Args), Selectors).

%%% sys log

-spec redact_log(list(), [atom()], [atom()]) -> list().
redact_log(Log, Slots, Selectors) ->
    {Redacted, _Calls} = lists:mapfoldl(
        fun(Event, Calls) -> redact_event(Event, Calls, Slots, Selectors) end,
        #{},
        Log
    ),
    Redacted.

%% `Calls` maps the `From` of each call seen so far in the log to whether
%% it was a secret send, so the matching `out` event's reply can be redacted.
-spec redact_event(term(), map(), [atom()], [atom()]) -> {term(), map()}.
redact_event({in, {'$gen_call', From, Call} = Msg}, Calls, _Slots, Selectors) ->
    {{in, redact_msg(Msg, Selectors)}, Calls#{From => is_secret_call(Call, Selectors)}};
redact_event({in, Msg}, Calls, _Slots, Selectors) ->
    {{in, redact_msg(Msg, Selectors)}, Calls};
redact_event({in, Msg, From}, Calls, _Slots, Selectors) ->
    {{in, redact_msg(Msg, Selectors), From}, Calls#{From => is_secret_call(Msg, Selectors)}};
redact_event({out, Reply, From, State}, Calls, Slots, Selectors) ->
    {{out, redact_reply(Reply, From, Calls, Selectors), From, redact_state(State, Slots)}, Calls};
redact_event({out, Reply, From}, Calls, _Slots, Selectors) ->
    {{out, redact_reply(Reply, From, Calls, Selectors), From}, Calls};
redact_event({noreply, State}, Calls, Slots, _Selectors) ->
    {{noreply, redact_state(State, Slots)}, Calls};
redact_event({continue, _} = Msg, Calls, _Slots, Selectors) ->
    {redact_msg(Msg, Selectors), Calls};
redact_event(Event, Calls, _Slots, _Selectors) ->
    {Event, Calls}.

-spec is_secret_call(term(), [atom()]) -> boolean().
is_secret_call(Call, Selectors) ->
    Call =/= redact_request(Call, Selectors).

%% A reply to a secret call is redacted, and so is a reply whose call has
%% already dropped out of the bounded log, since it may have been secret.
-spec redact_reply(term(), term(), map(), [atom()]) -> term().
redact_reply(Reply, _From, _Calls, []) ->
    Reply;
redact_reply(Reply, From, Calls, _Selectors) ->
    case Calls of
        #{From := false} -> Reply;
        _ -> ?REDACTED
    end.
