%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime Context

-module(beamtalk_log_domain_tests).
-moduledoc """
BT-5046: every `?LOG_*` call in the runtime app must reach domain-filtered
handlers (the transcript, `Logger enableDebug:`). A call carries a `domain`
either on the call itself or by a process-level default: the module sets one in
its `init` (`beamtalk_logging_config:set_domain/1` or
`logger:set_process_metadata/1`). A module with neither fails this test.
""".
-include_lib("eunit/include/eunit.hrl").

no_log_call_lacks_a_domain_test() ->
    SrcDir = filename:join([filename:dirname(?FILE), "..", "src"]),
    Files = filelib:wildcard(filename:join(SrcDir, "*.erl")),
    ?assert(Files =/= []),
    Offenders = lists:append([undomained_calls(F) || F <- Files]),
    ?assertEqual([], Offenders).

%% Calls without a domain, in modules with no process-level default.
undomained_calls(File) ->
    {ok, Bin} = file:read_file(File),
    Src = binary_to_list(Bin),
    HasDefault =
        string:find(Src, "set_domain(") =/= nomatch orelse
            string:find(Src, "set_process_metadata(") =/= nomatch,
    case HasDefault of
        true ->
            [];
        false ->
            [
                {filename:basename(File), Line}
             || {Line, Call} <- log_calls(Src), string:find(Call, "domain") =:= nomatch
            ]
    end.

%% `[{Line, CallText}]` for every `?LOG_<LEVEL>(...)` with balanced parens.
log_calls(Src) ->
    case
        re:run(Src, "\\?LOG_(ERROR|WARNING|INFO|DEBUG|NOTICE|CRITICAL)\\(", [
            global, {capture, first, index}
        ])
    of
        nomatch ->
            [];
        {match, Matches} ->
            [
                {
                    1 + length([C || C <- lists:sublist(Src, Start), C =:= $\n]),
                    balanced(lists:nthtail(Start + Len, Src), 1, [])
                }
             || [{Start, Len}] <- Matches
            ]
    end.

balanced([], _Depth, Acc) -> lists:reverse(Acc);
balanced([$( | T], D, Acc) -> balanced(T, D + 1, [$( | Acc]);
balanced([$) | _], 1, Acc) -> lists:reverse(Acc);
balanced([$) | T], D, Acc) -> balanced(T, D - 1, [$) | Acc]);
balanced([C | T], D, Acc) -> balanced(T, D, [C | Acc]).
