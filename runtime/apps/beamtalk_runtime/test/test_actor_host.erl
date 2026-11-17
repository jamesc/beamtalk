%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(test_actor_host).

%%% **DDD Context:** Runtime Context

-moduledoc """
Test-only gen_server host for the hand-written `test_*_actor` fixtures (BT-4871).

Until BT-4871 these fixtures delegated their gen_server callbacks to an
interpretive path inside `beamtalk_actor` that no production actor used. That
path was deleted in BT-4871; this
module is the small stand-in the fixtures call instead. It
speaks the same wire protocol a compiled actor's generated callbacks speak
(`{Selector, Args}` calls, `{cast, Selector, Args}` and `{Selector, Args,
FuturePid}` casts, propagated context, `'\$beamtalk_wire'` envelopes) and
reuses the helpers `beamtalk_actor` keeps for generated code (`decode_wire_call/1`,
`decode_wire_cast/1`, `encode_reply_for/3`, `apply_pending_actions/1`,
`take_init_ack/2`, `mailbox_continuation_check/2`, ...).

A fixture's method table lives in its state under `'__methods__'`: selector to
`fun(Args, State)` (or `fun(Selector, Args, Self, State)`) returning `{reply, V, S}`, `{noreply, S}` or
`{error, Reason, S}`. Anything not in
the table goes through the class hierarchy (`beamtalk_dispatch:lookup/5`), so
Object's methods are reachable exactly as they are for a compiled actor.

It is deliberately not a model of generated dispatch: behaviour of compiled
actors is tested against real compiled fixtures (`test_fixtures/*.bt`). This
module only gives the plumbing tests (spawn, sync/async send, lifecycle,
registration) a process to talk to.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_continue/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, map()} | {stop, term()}.
init(State) when is_map(State) ->
    beamtalk_logging_config:set_domain(runtime),
    ClassKey = beamtalk_tagged_map:class_key(),
    case maps:find(ClassKey, State) of
        {ok, Class} when is_atom(Class) ->
            case maps:is_key('__methods__', State) of
                true ->
                    erlang:put('$beamtalk_actor', Class),
                    erlang:put('$beamtalk_actor_started_at', erlang:system_time(microsecond)),
                    beamtalk_actor:maybe_execute_telemetry(
                        [beamtalk, actor, lifecycle, start],
                        #{},
                        #{pid => self(), class => Class}
                    ),
                    {ok, beamtalk_actor:take_init_ack(State, initialized)};
                false ->
                    {stop, {missing_key, '__methods__'}}
            end;
        {ok, _NonAtom} ->
            {stop, {invalid_value, ClassKey}};
        error ->
            {stop, {missing_key, ClassKey}}
    end;
init(_NonMapState) ->
    {stop, {invalid_state, not_a_map}}.

handle_call(Msg, From, State) ->
    ok = beamtalk_actor:clear_pending_actions(),
    beamtalk_actor:apply_pending_actions(do_call(Msg, From, State)).

handle_cast(Msg, State) ->
    ok = beamtalk_actor:clear_pending_actions(),
    beamtalk_actor:apply_pending_actions(do_cast(Msg, State)).

handle_continue(
    {?BT_CONTINUE_TAG, [{Selector, Args} | Rest]}, State
) ->
    ok = beamtalk_actor:clear_pending_actions(),
    ok = beamtalk_actor:seed_pending_continuations(Rest),
    beamtalk_actor:apply_pending_actions(run_cast(Selector, Args, State));
handle_continue(_Continue, State) ->
    {noreply, State}.

handle_info(Msg, State) -> beamtalk_actor:handle_info(Msg, State).

code_change(OldVsn, State, Extra) -> beamtalk_hot_reload:code_change(OldVsn, State, Extra).

terminate(Reason, State) ->
    Class = beamtalk_tagged_map:class_of(State, unknown),
    beamtalk_actor:maybe_execute_telemetry(
        [beamtalk, actor, lifecycle, stop],
        #{},
        #{pid => self(), class => Class, reason => Reason}
    ),
    ok.

%%====================================================================
%% Calls
%%====================================================================

do_call({?BT_WIRE_TAG, _, call, _, _, _} = Msg, From, State) ->
    case beamtalk_actor:decode_wire_call(Msg) of
        {redispatch, Msg2} -> do_call(Msg2, From, State);
        {reply, ReplyVal} -> {reply, ReplyVal, State}
    end;
do_call({Selector, Args, PropCtx}, From, State) when is_map(PropCtx) ->
    beamtalk_actor:restore_propagated_ctx(PropCtx),
    do_call({Selector, Args}, From, State);
do_call({Selector, Args}, From, State) ->
    case beamtalk_actor:mailbox_continuation_check(Selector, Args) of
        ok -> run_call(Selector, Args, From, State);
        {error, Error} -> {reply, reply_for(From, Selector, {error, Error}), State}
    end;
do_call(Msg, _From, State) ->
    ClassName = beamtalk_tagged_map:class_of(State, unknown),
    Error0 = beamtalk_error:new(does_not_understand, ClassName),
    Error1 = beamtalk_error:with_details(Error0, #{raw_message => Msg}),
    {reply, {error, beamtalk_error:with_hint(Error1, <<"Expected {Selector, Args} tuple">>)},
        State}.

run_call(Selector, Args, From, State) ->
    in_dispatch(State, fun() ->
        ok = beamtalk_actor:install_call_from(From),
        case with_state(dispatch(Selector, Args, State)) of
            {reply, Result, NewState} ->
                finish_call(From, Selector, Result, NewState);
            {noreply, NewState} ->
                finish_call(From, Selector, nil, NewState);
            {error, Reason, NewState} ->
                ok = beamtalk_actor:discard_pending_continuations(),
                {reply, reply_for(From, Selector, {error, Reason}), NewState}
        end
    end).

%% A deferred call (`self deferReply`) is answered `{noreply, _}` before encoding.
finish_call(From, Selector, Result, NewState) ->
    case beamtalk_actor:uninstall_call_from() of
        true -> {noreply, NewState};
        false -> {reply, reply_for(From, Selector, Result), NewState}
    end.

reply_for(From, Selector, Payload) ->
    beamtalk_actor:encode_reply_for(From, Selector, Payload).

%%====================================================================
%% Casts
%%====================================================================

do_cast({?BT_WIRE_TAG, _, cast, _, _, _} = Msg, State) ->
    wire_cast(Msg, State);
do_cast({?BT_WIRE_TAG, _, async, _, _, _, _} = Msg, State) ->
    wire_cast(Msg, State);
do_cast({cast, Selector, Args, PropCtx}, State) when
    is_atom(Selector), is_list(Args), is_map(PropCtx)
->
    beamtalk_actor:restore_propagated_ctx(PropCtx),
    do_cast({cast, Selector, Args}, State);
do_cast({cast, Selector, Args}, State) when is_atom(Selector), is_list(Args) ->
    case beamtalk_actor:mailbox_continuation_check(Selector, Args) of
        ok -> run_cast(Selector, Args, State);
        {error, _} -> {noreply, State}
    end;
do_cast({Selector, Args, FuturePid, PropCtx}, State) when is_map(PropCtx) ->
    beamtalk_actor:restore_propagated_ctx(PropCtx),
    do_cast({Selector, Args, FuturePid}, State);
do_cast({Selector, Args, FuturePid}, State) ->
    case beamtalk_actor:mailbox_continuation_check(Selector, Args) of
        ok ->
            run_async(Selector, Args, FuturePid, State);
        {error, Error} ->
            reject(FuturePid, Error),
            {noreply, State}
    end;
do_cast(Msg, State) ->
    ?LOG_WARNING("Unknown cast message", #{message => Msg, domain => [beamtalk, runtime]}),
    {noreply, State}.

wire_cast(Msg, State) ->
    case beamtalk_actor:decode_wire_cast(Msg) of
        {redispatch, Msg2} -> do_cast(Msg2, State);
        noreply -> {noreply, State}
    end.

%% Fire-and-forget: an error has no caller, so it crashes the actor (ADR 0134).
run_cast(Selector, Args, State) ->
    in_dispatch(State, fun() ->
        case with_state(dispatch(Selector, Args, State)) of
            {reply, _Result, NewState} -> {noreply, NewState};
            {noreply, NewState} -> {noreply, NewState};
            {error, Reason, _} -> beamtalk_exception_handler:reraise(Reason)
        end
    end).

run_async(Selector, Args, FuturePid, State) ->
    in_dispatch(State, fun() ->
        case with_state(dispatch(Selector, Args, State)) of
            {reply, Result, NewState} ->
                resolve(FuturePid, Result),
                {noreply, NewState};
            {noreply, NewState} ->
                resolve(FuturePid, nil),
                {noreply, NewState};
            {error, Reason, NewState} ->
                ok = beamtalk_actor:discard_pending_continuations(),
                reject(FuturePid, Reason),
                {noreply, NewState}
        end
    end).

resolve(FuturePid, Value) when is_pid(FuturePid), node(FuturePid) =/= node() ->
    case beamtalk_wire:encode(Value) of
        {ok, Enc} ->
            beamtalk_future:resolve(FuturePid, {'$beamtalk_wire_reply', ?BT_WIRE_VERSION, Enc});
        {error, EncErr} ->
            beamtalk_future:reject(FuturePid, EncErr)
    end;
resolve(FuturePid, Value) ->
    beamtalk_future:resolve(FuturePid, Value).

reject(FuturePid, Reason) when is_pid(FuturePid), node(FuturePid) =/= node() ->
    case beamtalk_wire:encode(Reason) of
        {ok, Enc} ->
            beamtalk_future:reject(FuturePid, {'$beamtalk_wire_reply', ?BT_WIRE_VERSION, Enc});
        {error, EncErr} ->
            beamtalk_future:reject(FuturePid, EncErr)
    end;
reject(FuturePid, Reason) ->
    beamtalk_future:reject(FuturePid, Reason).

%%====================================================================
%% Dispatch
%%====================================================================

%% Stash the state for re-entrant self-sends (`beamtalk_actor:self_dispatch/2`)
%% for the duration of one message.
in_dispatch(State, Fun) ->
    OldState = beamtalk_actor_state:install(State),
    try
        Fun()
    after
        ok = beamtalk_actor:restore_dispatch_pdict(OldState)
    end.

%% The method may have written the cell (a self-send); the cell is the truth.
with_state(Result) ->
    case beamtalk_actor_state:adapt(Result) of
        {reply, Value} -> {reply, Value, beamtalk_actor_state:take()};
        noreply -> {noreply, beamtalk_actor_state:take()};
        {error, Reason} -> {error, Reason, beamtalk_actor_state:take()}
    end.

dispatch(Selector, Args, State) ->
    Methods = maps:get('__methods__', State),
    case maps:find(Selector, Methods) of
        {ok, Fun} when is_function(Fun, 2) ->
            call_method(fun() -> Fun(Args, State) end, Selector, State);
        {ok, Fun} when is_function(Fun, 4) ->
            Self = beamtalk_actor:make_self(State),
            call_method(fun() -> Fun(Selector, Args, Self, State) end, Selector, State);
        error ->
            case maps:find('doesNotUnderstand:args:', Methods) of
                {ok, DnuFun} when is_function(DnuFun, 2) ->
                    call_method(
                        fun() -> DnuFun([Selector, Args], State) end,
                        'doesNotUnderstand:args:',
                        State
                    );
                {ok, DnuFun} when is_function(DnuFun, 3) ->
                    Self = beamtalk_actor:make_self(State),
                    call_method(
                        fun() -> DnuFun([Selector, Args], Self, State) end,
                        'doesNotUnderstand:args:',
                        State
                    );
                error ->
                    via_hierarchy(Selector, Args, State)
            end
    end.

call_method(Run, Selector, State) ->
    try
        Run()
    catch
        error:#beamtalk_error{} = BtError ->
            {error, BtError, State};
        throw:Nlr when ?IS_NLR(Nlr) ->
            {error, Nlr, State};
        Class:Reason:Stacktrace ->
            #{error := Error} = beamtalk_exception_handler:ensure_wrapped(
                Class, Reason, Stacktrace
            ),
            ClassName = beamtalk_tagged_map:class_of(State, unknown),
            {error, Error#beamtalk_error{class = ClassName, selector = Selector}, State}
    end.

via_hierarchy(Selector, Args, State) ->
    Self = beamtalk_actor:make_self(State),
    ClassName = beamtalk_tagged_map:class_of(State, unknown),
    try beamtalk_dispatch:lookup(Selector, Args, Self, State, ClassName) of
        {reply, Result, NewState} ->
            {reply, Result, NewState};
        {error, #beamtalk_error{kind = Kind}} when
            Kind =:= does_not_understand; Kind =:= class_not_found
        ->
            dnu(Selector, Args, ClassName, State);
        {error, Error} ->
            {error, Error, State}
    catch
        throw:Nlr when ?IS_NLR(Nlr) -> {error, Nlr, State};
        exit:{_, _} -> dnu(Selector, Args, ClassName, State)
    end.

%% Object's base methods, as the class-hierarchy walk reaches them for a
%% compiled actor; also the answer when no class is registered.
dnu(Selector, Args, ClassName, State) ->
    Self = beamtalk_actor:make_self(State),
    case beamtalk_object_ops:dispatch(Selector, Args, Self, State) of
        {error, #beamtalk_error{kind = does_not_understand}, _} ->
            {error, beamtalk_error:does_not_understand(ClassName, Selector), State};
        Result ->
            Result
    end.
