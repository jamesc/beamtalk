%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc IO capture for Beamtalk REPL evaluation.
%%%
%%% **DDD Context:** REPL
%%%
%%% Captures stdout during eval by temporarily replacing the group_leader
%%% with a custom IO server process.
%%% BT-696: Optionally forwards IO chunks to a subscriber for streaming.
%%% BT-698: Handles stdin requests by forwarding to subscriber.
%%%
%%% Extracted from beamtalk_repl_eval.erl (BT-706).

-module(beamtalk_io_capture).

-include_lib("kernel/include/logger.hrl").

-export([start/0, start/1, stop/1]).

%% Exported for testing
-ifdef(TEST).
-export([
    io_capture_loop/2,
    io_passthrough_loop/1,
    reset_captured_group_leaders/2,
    is_stdin_request/1,
    handle_stdin_request/2,
    prompt_to_binary/1,
    handle_io_request/2
]).
-endif.

-define(IO_CAPTURE_TIMEOUT, 5000).
%% BT-698: Timeout for stdin input during eval (30 seconds)
-define(STDIN_TIMEOUT, 30000).

%% @doc Start capturing IO output for the current process.
%% Convenience wrapper for start(undefined).
-dialyzer({no_unused, start/0}).
-spec start() -> {pid(), pid()}.
start() ->
    start(undefined).

%% @doc Start capturing IO output with optional streaming subscriber (BT-696).
%% When Subscriber is a pid, each IO chunk is forwarded as {eval_out, Chunk}.
-spec start(pid() | undefined) -> {pid(), pid()}.
start(Subscriber) ->
    OldGL = group_leader(),
    CapturePid = spawn(fun() -> io_capture_loop(<<>>, Subscriber) end),
    group_leader(CapturePid, self()),
    {CapturePid, OldGL}.

%% @doc Stop capturing IO and return all captured output as a binary.
%% Restores the original group_leader. Returns <<>> on timeout or if
%% the capture process has already exited.
%%
%% BT-358: After restoring the eval process's group_leader, also resets
%% the group_leader of any processes that inherited the capture process
%% as their group_leader during eval (e.g., spawned actors).
-spec stop({pid(), pid()}) -> binary().
stop({CapturePid, OldGL}) ->
    group_leader(OldGL, self()),
    %% BT-358: Reset group_leader for any processes spawned during eval
    %% that inherited the capture process as their group_leader.
    reset_captured_group_leaders(CapturePid, OldGL),
    case is_process_alive(CapturePid) of
        true ->
            CapturePid ! {get_captured, self(), OldGL},
            receive
                {captured_output, Output} -> Output
            after ?IO_CAPTURE_TIMEOUT ->
                ?LOG_WARNING("IO capture output retrieval timed out", #{}),
                <<>>
            end;
        false ->
            <<>>
    end.

%% @doc IO server loop that captures put_chars output.
%% Handles both {put_chars, Enc, Chars} and {put_chars, Enc, Mod, Func, Args}
%% (the latter is used by io:format).
%% BT-696: When Subscriber is a pid, forwards each chunk as {eval_out, Chunk}.
%% BT-698: When Subscriber is a pid, handles get_line/get_chars/get_until by
%% sending {need_input, CapturePid, Prompt} to Subscriber and waiting for
%% {stdin_input, Data} response.
%% After capture stops, proxies IO to the original group_leader so that
%% processes spawned during eval still have a working IO path.
-spec io_capture_loop(binary(), pid() | undefined) -> ok.
io_capture_loop(Buffer, Subscriber) ->
    receive
        {io_request, From, ReplyAs, Request} ->
            case is_stdin_request(Request) of
                {true, Prompt} ->
                    %% BT-698: Handle stdin request
                    Reply = handle_stdin_request(Subscriber, Prompt),
                    From ! {io_reply, ReplyAs, Reply},
                    io_capture_loop(Buffer, Subscriber);
                false ->
                    {Reply, NewBuffer} = handle_io_request(Request, Buffer),
                    From ! {io_reply, ReplyAs, Reply},
                    %% BT-696: Forward new chunk to subscriber if present
                    case is_pid(Subscriber) andalso byte_size(NewBuffer) > byte_size(Buffer) of
                        true ->
                            Chunk = binary:part(
                                NewBuffer,
                                byte_size(Buffer),
                                byte_size(NewBuffer) - byte_size(Buffer)
                            ),
                            Subscriber ! {eval_out, Chunk};
                        false ->
                            ok
                    end,
                    io_capture_loop(NewBuffer, Subscriber)
            end;
        {get_captured, Pid, OldGL} ->
            Pid ! {captured_output, Buffer},
            io_passthrough_loop(OldGL)
    end.

%% @doc After capture, forward all IO to the original group_leader.
%% Keeps this process alive so spawned children don't get a dead group_leader.
%% Exits after 60 seconds of inactivity to avoid leaking processes.
-spec io_passthrough_loop(pid()) -> ok.
io_passthrough_loop(OldGL) ->
    receive
        {io_request, From, ReplyAs, Request} ->
            OldGL ! {io_request, From, ReplyAs, Request},
            io_passthrough_loop(OldGL)
    after 60000 ->
        ok
    end.

%% @doc Reset group_leader for processes that inherited the capture process.
%% Scans all processes to find those whose group_leader is the capture
%% process and resets them to the original group_leader (BT-358).
-spec reset_captured_group_leaders(pid(), pid()) -> ok.
reset_captured_group_leaders(CapturePid, OldGL) ->
    lists:foreach(
        fun(Pid) ->
            case Pid =/= self() andalso is_process_alive(Pid) of
                true ->
                    case erlang:process_info(Pid, group_leader) of
                        {group_leader, CapturePid} ->
                            group_leader(OldGL, Pid);
                        {group_leader, _} ->
                            % Different GL, leave it alone
                            ok;
                        undefined ->
                            % Process died between checks, nothing to do
                            ok
                    end;
                false ->
                    ok
            end
        end,
        erlang:processes()
    ).

%% @doc Check if an IO request is a stdin (input) request.
%% Returns {true, Prompt} for get_line/get_chars/get_until, false otherwise.
%% BT-698: Supports the Erlang IO protocol input requests.
-spec is_stdin_request(term()) -> {true, binary()} | false.
is_stdin_request({get_line, _Encoding, Prompt}) ->
    {true, prompt_to_binary(Prompt)};
is_stdin_request({get_line, Prompt}) ->
    {true, prompt_to_binary(Prompt)};
is_stdin_request({get_chars, _Encoding, Prompt, _Count}) ->
    {true, prompt_to_binary(Prompt)};
is_stdin_request({get_chars, Prompt, _Count}) ->
    {true, prompt_to_binary(Prompt)};
is_stdin_request({get_until, _Encoding, Prompt, _Mod, _Func, _Args}) ->
    {true, prompt_to_binary(Prompt)};
is_stdin_request({get_until, Prompt, _Mod, _Func, _Args}) ->
    {true, prompt_to_binary(Prompt)};
is_stdin_request(_) ->
    false.

%% @doc Convert an IO prompt to a binary string.
-spec prompt_to_binary(term()) -> binary().
prompt_to_binary(Prompt) when is_binary(Prompt) -> Prompt;
prompt_to_binary(Prompt) when is_list(Prompt) ->
    try unicode:characters_to_binary(Prompt, utf8) of
        Bin when is_binary(Bin) -> Bin;
        _ -> <<"? ">>
    catch
        _:_ -> <<"? ">>
    end;
prompt_to_binary(Prompt) when is_atom(Prompt) ->
    atom_to_binary(Prompt, utf8);
prompt_to_binary(_) ->
    <<"? ">>.

%% @doc Handle a stdin IO request by notifying the subscriber and waiting for input.
%% When no subscriber is present (sync eval), returns {error, enotsup}.
%% BT-698: Sends {need_input, CapturePid, Ref, Prompt} to subscriber, waits for
%% {stdin_input, Ref, Data} response with timeout. The Ref prevents late replies
%% from a timed-out prompt being consumed by a subsequent prompt.
-spec handle_stdin_request(pid() | undefined, binary()) -> term().
handle_stdin_request(undefined, _Prompt) ->
    {error, enotsup};
handle_stdin_request(Subscriber, Prompt) when is_pid(Subscriber) ->
    Ref = make_ref(),
    Subscriber ! {need_input, self(), Ref, Prompt},
    receive
        {stdin_input, Ref, Data} when is_binary(Data) ->
            Data;
        {stdin_input, Ref, eof} ->
            eof
    after ?STDIN_TIMEOUT ->
        ?LOG_WARNING("Stdin input timed out after ~pms", [?STDIN_TIMEOUT]),
        {error, timeout}
    end.

%% @doc Handle a single IO protocol request.
%% Always replies ok for output requests to avoid changing program behavior.
-spec handle_io_request(term(), binary()) -> {term(), binary()}.
handle_io_request({put_chars, Encoding, Chars}, Buffer) ->
    try unicode:characters_to_binary(Chars, Encoding, utf8) of
        Bin when is_binary(Bin) -> {ok, <<Buffer/binary, Bin/binary>>};
        _ -> {ok, Buffer}
    catch
        _:_ -> {ok, Buffer}
    end;
handle_io_request({put_chars, Chars}, Buffer) ->
    %% Legacy IO protocol form without encoding
    try unicode:characters_to_binary(Chars, latin1, utf8) of
        Bin when is_binary(Bin) -> {ok, <<Buffer/binary, Bin/binary>>};
        _ -> {ok, Buffer}
    catch
        _:_ -> {ok, Buffer}
    end;
handle_io_request({put_chars, Encoding, Mod, Func, Args}, Buffer) ->
    try unicode:characters_to_binary(apply(Mod, Func, Args), Encoding, utf8) of
        Bin when is_binary(Bin) -> {ok, <<Buffer/binary, Bin/binary>>};
        _ -> {ok, Buffer}
    catch
        _:_ -> {ok, Buffer}
    end;
handle_io_request(_Other, Buffer) ->
    {{error, enotsup}, Buffer}.
