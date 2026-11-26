%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_process_tests).

%%% **DDD Context:** REPL Session Context

-behaviour(supervisor).

-moduledoc """
The process-navigation ops `children` and `process-info`, and the live-pid
form of the `actor` param on `actors`, `inspect`, `kill` and `pid-stats`
(BT-5205). Driven through `beamtalk_repl_ops:request/3`, the entry point
LiveView uses over distribution.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([init/1, start_idle/0, idle_loop/0, handle_call/3, handle_cast/2]).

%% gen_server callbacks of the crash-on-unexpected-call fixture. The supervisor
%% and the gen_server share `init/1`, told apart by their argument.
handle_call(known, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

%% supervisor: one foreign worker child, or (`many`) a pool started on demand.
init(crash_on_call) ->
    {ok, undefined};
init(many) ->
    {ok,
        {#{strategy => simple_one_for_one, intensity => 1, period => 5}, [
            #{
                id => idle,
                start => {?MODULE, start_idle, []},
                restart => temporary,
                shutdown => brutal_kill,
                type => worker,
                modules => [?MODULE]
            }
        ]}};
init([]) ->
    {ok,
        {#{strategy => one_for_one, intensity => 1, period => 5}, [
            #{
                id => idle,
                start => {?MODULE, start_idle, []},
                restart => temporary,
                shutdown => brutal_kill,
                type => worker,
                modules => [?MODULE]
            }
        ]}}.

start_idle() ->
    {ok, proc_lib:spawn_link(?MODULE, idle_loop, [])}.

idle_loop() ->
    receive
        stop -> ok
    end.

request(Op, Params) ->
    beamtalk_repl_ops:request(Op, Params, self()).

with_supervisor(Fun) ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    {ok, Sup} = supervisor:start_link(?MODULE, []),
    try
        Fun(Sup)
    after
        unlink(Sup),
        exit(Sup, shutdown)
    end.

pid_text(Pid) -> list_to_binary(pid_to_list(Pid)).

%%% children

children_by_live_pid_and_by_text_test() ->
    with_supervisor(fun(Sup) ->
        Expected = fun({value, #{<<"children">> := [Row], <<"truncated">> := false}}) ->
            ?assertMatch(
                #{
                    <<"kind">> := <<"otpProcess">>,
                    <<"isSupervisor">> := false,
                    <<"handle">> := null,
                    <<"label">> := <<"process">>
                },
                Row
            ),
            ?assert(is_binary(maps:get(<<"pid">>, Row)))
        end,
        Expected(request(<<"children">>, #{<<"supervisor">> => Sup})),
        Expected(request(<<"children">>, #{<<"supervisor">> => pid_text(Sup)}))
    end).

children_of_a_worker_is_empty_test() ->
    with_supervisor(fun(Sup) ->
        [{_, Worker, _, _}] = supervisor:which_children(Sup),
        ?assertEqual(
            {value, #{<<"children">> => [], <<"truncated">> => false}},
            request(<<"children">>, #{<<"supervisor">> => Worker})
        )
    end).

%% A process that is not a supervisor is never sent `which_children`: a
%% gen_server with no catch-all `handle_call` would crash on it.
children_never_calls_a_non_supervisor_test() ->
    {ok, Server} = gen_server:start(?MODULE, crash_on_call, []),
    try
        ?assertEqual(
            {value, #{<<"children">> => [], <<"truncated">> => false}},
            request(<<"children">>, #{<<"supervisor">> => Server})
        ),
        ?assert(is_process_alive(Server))
    after
        exit(Server, kill)
    end.

%% A pid of another node cannot be asked: an error, not a crashed handler.
children_of_a_remote_pid_is_an_error_test() ->
    ?assertMatch(
        {error, #beamtalk_error{}},
        request(<<"children">>, #{<<"supervisor">> => remote_pid()})
    ).

children_of_a_dead_supervisor_is_stale_handle_test() ->
    Dead = beamtalk_test_proc:spawn_idle(),
    exit(Dead, kill),
    ok = wait_dead(Dead),
    ?assertMatch(
        {error, #beamtalk_error{kind = stale_handle}},
        request(<<"children">>, #{<<"supervisor">> => Dead})
    ).

children_refuses_a_bad_pid_test_() ->
    [
        ?_assertMatch(
            {error, #beamtalk_error{kind = invalid_pid}},
            request(<<"children">>, Params)
        )
     || Params <- [
            #{},
            #{<<"supervisor">> => <<"not-a-pid">>},
            #{<<"supervisor">> => <<>>},
            #{<<"supervisor">> => 42},
            #{<<"supervisor">> => null},
            #{<<"actor">> => <<"<0.1.0>">>}
        ]
    ].

children_caps_the_row_count_test() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    {ok, Sup} = supervisor:start_link(?MODULE, many),
    try
        [{ok, _} = supervisor:start_child(Sup, []) || _ <- lists:seq(1, 520)],
        {value, #{<<"children">> := Rows, <<"truncated">> := Truncated}} =
            request(<<"children">>, #{<<"supervisor">> => Sup}),
        ?assertEqual(500, length(Rows)),
        ?assertEqual(true, Truncated)
    after
        unlink(Sup),
        exit(Sup, shutdown)
    end.

%% The WebSocket edge cannot carry a tuple holding a pid: a live handle is
%% sent as its stable id, and `null` stays `null`.
wire_projects_live_handles_to_stable_ids_test() ->
    Pid = beamtalk_test_proc:spawn_idle(),
    Rows = [
        #{<<"label">> => <<"Counter">>, <<"handle">> => {beamtalk_object, 'Counter', counter, Pid}},
        #{<<"label">> => <<"Sup">>, <<"handle">> => {beamtalk_supervisor, 'Sup', sup, Pid}},
        #{<<"label">> => <<"process">>, <<"handle">> => null}
    ],
    {value, #{<<"children">> := Wired}} =
        beamtalk_repl_ops_process:wire(
            {value, #{<<"children">> => Rows, <<"truncated">> => false}}
        ),
    Text = pid_text(Pid),
    ?assertMatch(
        [
            #{
                <<"handle">> := #{
                    <<"kind">> := <<"actor">>, <<"class">> := <<"Counter">>, <<"pid">> := Text
                }
            },
            #{<<"handle">> := #{<<"kind">> := <<"supervisor">>, <<"class">> := <<"Sup">>}},
            #{<<"handle">> := null}
        ],
        Wired
    ).

children_encodes_as_json_at_the_edge_test() ->
    with_supervisor(fun(Sup) ->
        Msg = {protocol_msg, <<"children">>, <<"m1">>, <<"s1">>, #{}},
        Json = beamtalk_repl_ops_process:handle(
            <<"children">>, #{<<"supervisor">> => pid_text(Sup)}, Msg, self()
        ),
        #{<<"value">> := #{<<"children">> := [#{<<"handle">> := null}]}} = json:decode(Json)
    end).

%%% process-info

process_info_of_a_live_pid_test() ->
    Pid = beamtalk_test_proc:spawn_idle(),
    Text = pid_text(Pid),
    lists:foreach(
        fun(Param) ->
            {value, Info} = request(<<"process-info">>, #{<<"pid">> => Param}),
            ?assertMatch(
                #{
                    <<"pid">> := Text,
                    <<"alive">> := true,
                    <<"status">> := Status,
                    <<"message_queue_len">> := 0,
                    <<"memory">> := Memory,
                    <<"reductions">> := Reductions
                } when is_binary(Status) andalso is_integer(Memory) andalso is_integer(Reductions),
                Info
            )
        end,
        [Pid, Text]
    ).

%% Not an actor: `pid-stats` refuses it, `process-info` reads it.
process_info_needs_no_actor_test() ->
    Pid = beamtalk_test_proc:spawn_idle(),
    ?assertMatch(
        {error, #beamtalk_error{kind = unknown_actor}},
        request(<<"pid-stats">>, #{<<"actor">> => Pid})
    ),
    ?assertMatch({value, #{<<"alive">> := true}}, request(<<"process-info">>, #{<<"pid">> => Pid})).

process_info_of_a_dead_pid_is_not_an_error_test() ->
    Pid = beamtalk_test_proc:spawn_idle(),
    exit(Pid, kill),
    ok = wait_dead(Pid),
    Text = pid_text(Pid),
    ?assertEqual(
        {value, #{<<"pid">> => Text, <<"alive">> => false}},
        request(<<"process-info">>, #{<<"pid">> => Pid})
    ).

%% A pid of another node cannot be read here: not alive, never a crash.
process_info_of_a_remote_pid_reads_as_not_alive_test() ->
    Remote = remote_pid(),
    ?assertMatch(
        {value, #{<<"alive">> := false}}, request(<<"process-info">>, #{<<"pid">> => Remote})
    ).

process_info_refuses_a_bad_pid_test_() ->
    [
        ?_assertMatch(
            {error, #beamtalk_error{kind = invalid_pid}},
            request(<<"process-info">>, Params)
        )
     || Params <- [#{}, #{<<"pid">> => <<"nope">>}, #{<<"pid">> => 1}, #{<<"pid">> => #{}}]
    ].

%%% the actor param takes a live pid

actors_carry_the_workspace_pid_text_test() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    {ok, Actor} = beamtalk_actor:safe_spawn_linked(test_counter, 0),
    try
        {actors, Actors} = request(<<"actors">>, #{}),
        [Listed] = [A || #{pid := P} = A <- Actors, P =:= Actor],
        ?assertEqual(pid_text(Actor), maps:get(pid_text, Listed)),
        %% The wire encoder picks its own fields: `pid_text` is not on it.
        Msg = {protocol_msg, <<"actors">>, <<"m1">>, <<"s1">>, #{}},
        Json = beamtalk_repl_ops_actors:handle(<<"actors">>, #{}, Msg, self()),
        ?assertEqual(nomatch, binary:match(Json, <<"pid_text">>))
    after
        unlink(Actor),
        exit(Actor, kill)
    end.

actor_ops_accept_a_live_pid_test() ->
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    {ok, Actor} = beamtalk_actor:safe_spawn_linked(test_counter, 0),
    try
        Text = pid_text(Actor),
        ?assertMatch({inspect, _}, request(<<"inspect">>, #{<<"actor">> => Actor})),
        ?assertEqual(
            request(<<"inspect">>, #{<<"actor">> => Text}),
            request(<<"inspect">>, #{<<"actor">> => Actor})
        ),
        ?assertMatch(
            {value, #{<<"pid">> := Text, <<"alive">> := true}},
            request(<<"pid-stats">>, #{<<"actor">> => Actor})
        ),
        unlink(Actor),
        ?assertEqual({status, ok}, request(<<"kill">>, #{<<"actor">> => Actor})),
        ?assertNot(is_process_alive(Actor))
    after
        unlink(Actor),
        exit(Actor, kill)
    end.

actor_ops_still_refuse_a_live_pid_that_is_not_an_actor_test() ->
    Pid = beamtalk_test_proc:spawn_idle(),
    lists:foreach(
        fun(Op) ->
            ?assertMatch(
                {error, #beamtalk_error{kind = unknown_actor}}, request(Op, #{<<"actor">> => Pid})
            )
        end,
        [<<"inspect">>, <<"pid-stats">>]
    ),
    ?assert(is_process_alive(Pid)).

%%% Helpers

wait_dead(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    after 2000 -> erlang:error({still_alive, Pid})
    end.

%% A syntactically valid pid of a node that is not this one.
remote_pid() ->
    %% pid external format: 131, 88 (NEW_PID_EXT), atom node, id, serial, creation.
    Bin = <<131, 88, 119, 11, "other@nowhere", 1:32, 0:32, 1:32>>,
    binary_to_term(Bin).
