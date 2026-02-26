%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc TranscriptStream actor — shared workspace log with pub/sub dispatch.
%%%
%%% **DDD Context:** Runtime Context
%%%
%%% TranscriptStream is the actor backing the `Transcript` binding in a
%%% workspace.  It maintains a ring buffer of recent output and pushes text
%%% to subscribers via `{transcript_output, Text}' messages.
%%%
%%% ## Instance Methods
%%%
%%% | Selector      | Dispatch | Returns | Description                          |
%%% |---------------|----------|---------|--------------------------------------|
%%% | `show:'       | cast     | self    | Buffer text + push to subscribers    |
%%% | `cr'          | cast     | self    | Buffer newline + push to subscribers |
%%% | `subscribe'   | cast     | self    | Add caller to subscriber list        |
%%% | `unsubscribe' | cast     | self    | Remove caller from subscriber list   |
%%% | `recent'      | call     | list    | Return buffer contents as list       |
%%% | `clear'       | call     | self    | Empty the buffer                     |
%%%
%%% ## Design
%%%
%%% - Ring buffer: `queue:queue()' with configurable max size (default 1000)
%%% - Subscribers: `#{pid() => reference()}' (pid → monitor ref)
%%% - Dead subscribers auto-removed via `handle_info({'DOWN', ...})'
%%% - When no subscribers: output goes to buffer only (no stdout)

-module(beamtalk_transcript_stream).
-behaviour(gen_server).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, start_link/1, start_link/2, spawn/0, spawn/1]).
-export([has_method/1, class_info/0]).
-export([ensure_utf8/1]).
-export([subscribe/1, unsubscribe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%% ============================================================================
%%% Types
%%% ============================================================================

-type max_buffer() :: pos_integer().

-record(state, {
    buffer :: queue:queue(binary()),
    buffer_size :: non_neg_integer(),
    max_buffer :: max_buffer(),
    subscribers :: #{pid() => reference()},
    self_ref :: tuple() | undefined
}).

-type state() :: #state{}.

%%% ============================================================================
%%% API
%%% ============================================================================

%% @doc Start a linked TranscriptStream with default buffer size (1000).
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(1000).

%% @doc Start a linked TranscriptStream with the given max buffer size.
-spec start_link(max_buffer()) -> {ok, pid()} | {error, term()}.
start_link(MaxBuffer) ->
    gen_server:start_link(?MODULE, [MaxBuffer], []).

%% @doc Start a linked, named TranscriptStream for workspace use.
%% Registers with the given name via gen_server name registration.
-spec start_link({local, atom()}, max_buffer()) -> {ok, pid()} | {error, term()}.
start_link(ServerName, MaxBuffer) ->
    gen_server:start_link(ServerName, ?MODULE, [MaxBuffer], []).

%% @doc Spawn an unlinked TranscriptStream with default buffer size.
-spec spawn() -> {ok, pid()} | {error, term()}.
spawn() ->
    ?MODULE:spawn(1000).

%% @doc Spawn an unlinked TranscriptStream with the given max buffer size.
-spec spawn(max_buffer()) -> {ok, pid()} | {error, term()}.
spawn(MaxBuffer) ->
    gen_server:start(?MODULE, [MaxBuffer], []).

%% @doc Check if TranscriptStream responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method('show:') -> true;
has_method(cr) -> true;
has_method(subscribe) -> true;
has_method(unsubscribe) -> true;
has_method(recent) -> true;
has_method(clear) -> true;
has_method(_) -> false.

%% @doc Return class registration metadata for TranscriptStream.
%%
%% Used by beamtalk_stdlib to register this singleton's class.
%% Single source of truth for class name, superclass, and method table.
-spec class_info() -> map().
class_info() ->
    #{
        name => 'TranscriptStream',
        module => ?MODULE,
        superclass => 'Actor',
        instance_methods => #{
            'show:' => #{arity => 1},
            cr => #{arity => 0},
            subscribe => #{arity => 0},
            unsubscribe => #{arity => 0},
            recent => #{arity => 0},
            clear => #{arity => 0}
        },
        class_methods => #{},
        fields => []
    }.

%%% ============================================================================
%%% Module-level subscribe/unsubscribe API
%%% ============================================================================

%% @doc Subscribe a process to Transcript push messages.
%% The subscriber will receive `{transcript_output, Text}' messages.
%% The Transcript must be a named process (e.g. registered as 'Transcript').
-spec subscribe(pid() | atom()) -> ok.
subscribe(TranscriptRef) ->
    gen_server:cast(TranscriptRef, {subscribe, self()}).

%% @doc Unsubscribe a process from Transcript push messages.
-spec unsubscribe(pid() | atom()) -> ok.
unsubscribe(TranscriptRef) ->
    gen_server:cast(TranscriptRef, {unsubscribe, self()}).

%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================

%% @private
%% @doc Initialize the TranscriptStream gen_server with the given buffer size.
-spec init([max_buffer()]) -> {ok, state()} | {stop, term()}.
init([MaxBuffer]) when is_integer(MaxBuffer), MaxBuffer > 0 ->
    SelfRef = {beamtalk_object, 'TranscriptStream', beamtalk_transcript_stream, self()},
    {ok, #state{
        buffer = queue:new(),
        buffer_size = 0,
        max_buffer = MaxBuffer,
        subscribers = #{},
        self_ref = SelfRef
    }};
init([MaxBuffer]) ->
    {stop, {invalid_max_buffer, MaxBuffer}}.

%% @private
%% @doc Handle synchronous calls: recent, clear, and unknown selectors.
-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.
handle_call(recent, _From, #state{buffer = Buffer} = State) ->
    {reply, queue:to_list(Buffer), State};
handle_call(clear, _From, #state{self_ref = SelfRef} = State) ->
    {reply, SelfRef, State#state{buffer = queue:new(), buffer_size = 0}};
handle_call({recent, []}, From, State) ->
    handle_call(recent, From, State);
handle_call({clear, []}, From, State) ->
    handle_call(clear, From, State);
handle_call({Selector, Args}, From, State) when is_atom(Selector), is_list(Args) ->
    %% Unknown selector - try hierarchy walk for inherited methods (e.g. printString, class)
    SelfRef = State#state.self_ref,
    erlang:spawn(fun() ->
        try
            beamtalk_dispatch:lookup(
                Selector,
                Args,
                SelfRef,
                #{'$beamtalk_class' => 'TranscriptStream'},
                'TranscriptStream'
            )
        of
            {reply, Value, _NewState} ->
                gen_server:reply(From, Value);
            {error, Error} ->
                gen_server:reply(From, {error, Error})
        catch
            Class:Reason:Stack ->
                ?LOG_ERROR("TranscriptStream dispatch handler crashed", #{
                    selector => Selector, class => Class, reason => Reason, stacktrace => Stack
                }),
                Err = beamtalk_error:new(runtime_error, 'TranscriptStream'),
                gen_server:reply(
                    From,
                    {error,
                        beamtalk_error:with_selector(
                            beamtalk_error:with_message(Err, <<"Internal error">>), Selector
                        )}
                )
        end
    end),
    {noreply, State};
handle_call(Request, _From, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'TranscriptStream'),
    Error1 = beamtalk_error:with_selector(Error0, Request),
    {reply, {error, Error1}, State}.

%% @private
%% @doc Handle asynchronous casts: show:, cr, subscribe, unsubscribe.
-spec handle_cast(term(), state()) -> {noreply, state()}.
%% Actor protocol: {Selector, Args, FuturePid} from beamtalk_actor:async_send/4
handle_cast({'show:', [Value], FuturePid}, #state{self_ref = SelfRef} = State) when
    is_pid(FuturePid)
->
    Text = beamtalk_primitive:display_string(Value),
    State1 = buffer_text(Text, State),
    push_to_subscribers(Text, State1),
    beamtalk_future:resolve(FuturePid, SelfRef),
    {noreply, State1};
handle_cast({cr, [], FuturePid}, #state{self_ref = SelfRef} = State) when is_pid(FuturePid) ->
    Text = <<"\n">>,
    State1 = buffer_text(Text, State),
    push_to_subscribers(Text, State1),
    beamtalk_future:resolve(FuturePid, SelfRef),
    {noreply, State1};
handle_cast({recent, [], FuturePid}, State) when is_pid(FuturePid) ->
    beamtalk_future:resolve(FuturePid, queue:to_list(State#state.buffer)),
    {noreply, State};
handle_cast({clear, [], FuturePid}, #state{self_ref = SelfRef} = State) when is_pid(FuturePid) ->
    beamtalk_future:resolve(FuturePid, SelfRef),
    {noreply, State#state{buffer = queue:new(), buffer_size = 0}};
handle_cast({subscribe, [], FuturePid}, #state{self_ref = SelfRef} = State) when
    is_pid(FuturePid)
->
    CallerPid = caller_from_future(FuturePid),
    beamtalk_future:resolve(FuturePid, SelfRef),
    {noreply, add_subscriber(CallerPid, State)};
handle_cast({unsubscribe, [], FuturePid}, #state{self_ref = SelfRef} = State) when
    is_pid(FuturePid)
->
    CallerPid = caller_from_future(FuturePid),
    beamtalk_future:resolve(FuturePid, SelfRef),
    {noreply, remove_subscriber(CallerPid, State)};
%% Legacy format (direct gen_server:cast without Future)
handle_cast({'show:', Value}, State) ->
    Text = beamtalk_primitive:display_string(Value),
    State1 = buffer_text(Text, State),
    push_to_subscribers(Text, State1),
    {noreply, State1};
handle_cast(cr, State) ->
    Text = <<"\n">>,
    State1 = buffer_text(Text, State),
    push_to_subscribers(Text, State1),
    {noreply, State1};
handle_cast({subscribe, Pid}, State) ->
    {noreply, add_subscriber(Pid, State)};
handle_cast({unsubscribe, Pid}, State) ->
    {noreply, remove_subscriber(Pid, State)};
%% Actor protocol catch-all: try hierarchy walk for inherited methods
handle_cast({Selector, Args, FuturePid}, State) when
    is_pid(FuturePid), is_atom(Selector), is_list(Args)
->
    SelfRef = State#state.self_ref,
    erlang:spawn(fun() ->
        try
            beamtalk_dispatch:lookup(
                Selector,
                Args,
                SelfRef,
                #{'$beamtalk_class' => 'TranscriptStream'},
                'TranscriptStream'
            )
        of
            {reply, Value, _NewState} ->
                beamtalk_future:resolve(FuturePid, Value);
            {error, Error} ->
                beamtalk_future:reject(FuturePid, Error)
        catch
            Class:Reason:Stack ->
                ?LOG_ERROR("TranscriptStream dispatch handler crashed", #{
                    selector => Selector, class => Class, reason => Reason, stacktrace => Stack
                }),
                Err = beamtalk_error:new(runtime_error, 'TranscriptStream'),
                beamtalk_future:reject(
                    FuturePid,
                    beamtalk_error:with_selector(
                        beamtalk_error:with_message(Err, <<"Internal error">>), Selector
                    )
                )
        end
    end),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc Handle monitor DOWN messages to remove dead subscribers.
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    {noreply, remove_subscriber(Pid, State)};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc Clean up on termination (no-op).
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private
%% @doc Derive the caller's session PID from a future process.
%% Uses group_leader to identify the session that spawned the future.
%% Falls back to the FuturePid itself if the future has already exited.
-spec caller_from_future(pid()) -> pid().
caller_from_future(FuturePid) ->
    case erlang:process_info(FuturePid, group_leader) of
        {group_leader, GL} -> GL;
        undefined -> FuturePid
    end.

%% @private
%% @doc Add text to the ring buffer, dropping oldest if at capacity.
-spec buffer_text(binary(), state()) -> state().
buffer_text(Text, #state{buffer = Buffer, buffer_size = Size, max_buffer = Max} = State) ->
    Buffer1 = queue:in(Text, Buffer),
    case Size >= Max of
        true ->
            {_, Buffer2} = queue:out(Buffer1),
            State#state{buffer = Buffer2};
        false ->
            State#state{buffer = Buffer1, buffer_size = Size + 1}
    end.

%% @private
%% @doc Send text to all subscribers.
-spec push_to_subscribers(binary(), state()) -> ok.
push_to_subscribers(Text, #state{subscribers = Subs}) ->
    maps:foreach(
        fun(Pid, _Ref) ->
            Pid ! {transcript_output, Text}
        end,
        Subs
    ),
    ok.

%% @private
%% @doc Add a subscriber and monitor it. Ignores non-pid values.
-spec add_subscriber(pid(), state()) -> state().
add_subscriber(Pid, #state{subscribers = Subs} = State) when is_pid(Pid) ->
    case maps:is_key(Pid, Subs) of
        true ->
            State;
        false ->
            Ref = monitor(process, Pid),
            State#state{subscribers = Subs#{Pid => Ref}}
    end;
add_subscriber(_NotPid, State) ->
    State.

%% @private
%% @doc Remove a subscriber and demonitor it.
-spec remove_subscriber(pid(), state()) -> state().
remove_subscriber(Pid, #state{subscribers = Subs} = State) ->
    case maps:find(Pid, Subs) of
        {ok, Ref} ->
            demonitor(Ref, [flush]),
            State#state{subscribers = maps:remove(Pid, Subs)};
        error ->
            State
    end.

%% @doc Ensure a binary is valid UTF-8. Returns the binary unchanged if valid,
%% or a ~p (io_lib:format("~p", ...)) representation if it contains invalid
%% or incomplete UTF-8 data.
-spec ensure_utf8(binary()) -> binary().
ensure_utf8(Bin) ->
    case unicode:characters_to_binary(Bin) of
        Utf8 when is_binary(Utf8) -> Utf8;
        {error, _, _} -> list_to_binary(io_lib:format("~p", [Bin]));
        {incomplete, _, _} -> list_to_binary(io_lib:format("~p", [Bin]))
    end.
