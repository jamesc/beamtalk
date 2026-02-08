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
%%% | Selector      | Dispatch | Description                          |
%%% |---------------|----------|--------------------------------------|
%%% | `show:'       | cast     | Buffer text + push to subscribers    |
%%% | `cr'          | cast     | Buffer newline + push to subscribers |
%%% | `subscribe'   | cast     | Add caller to subscriber list        |
%%% | `unsubscribe' | cast     | Remove caller from subscriber list   |
%%% | `recent'      | call     | Return buffer contents as list       |
%%% | `clear'       | call     | Empty the buffer                     |
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

%% API
-export([start_link/0, start_link/1, spawn/0, spawn/1]).
-export([has_method/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%% ============================================================================
%%% Types
%%% ============================================================================

-type max_buffer() :: pos_integer().

-record(state, {
    buffer      :: queue:queue(binary()),
    buffer_size :: non_neg_integer(),
    max_buffer  :: max_buffer(),
    subscribers :: #{pid() => reference()}
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
has_method('show:')      -> true;
has_method(cr)           -> true;
has_method(subscribe)    -> true;
has_method(unsubscribe)  -> true;
has_method(recent)       -> true;
has_method(clear)        -> true;
has_method(_)            -> false.

%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================

%% @private
-spec init([max_buffer()]) -> {ok, state()} | {stop, term()}.
init([MaxBuffer]) when is_integer(MaxBuffer), MaxBuffer > 0 ->
    {ok, #state{
        buffer      = queue:new(),
        buffer_size = 0,
        max_buffer  = MaxBuffer,
        subscribers = #{}
    }};
init([MaxBuffer]) ->
    {stop, {invalid_max_buffer, MaxBuffer}}.

%% @private
-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.
handle_call(recent, _From, #state{buffer = Buffer} = State) ->
    {reply, queue:to_list(Buffer), State};
handle_call(clear, _From, State) ->
    {reply, ok, State#state{buffer = queue:new(), buffer_size = 0}};
handle_call(_Request, _From, State) ->
    Error0 = beamtalk_error:new(does_not_understand, 'TranscriptStream'),
    {reply, {error, Error0}, State}.

%% @private
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({'show:', Value}, State) ->
    Text = to_string(Value),
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
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    {noreply, remove_subscriber(Pid, State)};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

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
    maps:foreach(fun(Pid, _Ref) ->
        Pid ! {transcript_output, Text}
    end, Subs),
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

%% @private
%% @doc Convert a value to its string representation for display.
-spec to_string(term()) -> binary().
to_string(Value) when is_binary(Value) ->
    Value;
to_string(Value) when is_integer(Value) ->
    integer_to_binary(Value);
to_string(Value) when is_float(Value) ->
    float_to_binary(Value, [{decimals, 10}, compact]);
to_string(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_string(Value) when is_list(Value) ->
    try
        unicode:characters_to_binary(Value)
    catch
        _:_ -> list_to_binary(io_lib:format("~p", [Value]))
    end;
to_string(#beamtalk_object{class = Class}) ->
    <<"a ", (atom_to_binary(Class, utf8))/binary>>;
to_string(Value) when is_map(Value) ->
    case maps:find('__class__', Value) of
        {ok, Class} when is_atom(Class) ->
            <<"a ", (atom_to_binary(Class, utf8))/binary>>;
        _ -> list_to_binary(io_lib:format("~p", [Value]))
    end;
to_string(Value) ->
    list_to_binary(io_lib:format("~p", [Value])).
