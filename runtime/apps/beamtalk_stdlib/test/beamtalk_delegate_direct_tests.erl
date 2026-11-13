%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_delegate_direct_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
BT-4765: conformance for `self delegate` methods the compiler switched from
`beamtalk_erlang_proxy:native_call/5` to a guarded direct call.

Each row calls the *compiled* stdlib method (the shape codegen emitted: a guard,
the direct call, `ffi_call_raised/10` on a raise, `native_call/5` as the
fallback) and the proxy's `native_call/5` for the same backing function, and
asserts the two agree on results and on structured errors, the
`{Class, Selector}` context included. Delegates whose two calls cannot be
compared (clocks, global state) are listed in `exempt/0` with the reason. Rows cover an argument that passes the
runtime guard, one the backing function rejects (the error path), and one the
guard rejects (a tagged String: the fallback arm).

`switched_delegates_are_covered_test/0` fails when a stdlib method becomes a
direct delegate without a row here, so a newly proved delegate cannot ship
without its differential.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% {CompiledModule, CompiledFun, BackingModule, BackingFun, Class, Selector,
%%  ArgSets}. A class-side method is called with a placeholder `ClassSelf`,
%% which the proxy call omits. `ArgSets` are the user arguments (the receiver
%% first for an instance method).
rows() ->
    Tagged = {beamtalk_string, <<"x">>},
    [
        {'bt@stdlib@binary', bitSize, beamtalk_binary, bitSize, 'Binary', bitSize, instance, [
            [<<"abc">>], [42], [Tagged], [foo]
        ]},
        {'bt@stdlib@binary', asHex, beamtalk_binary, asHex, 'Binary', asHex, instance, [
            [<<"abc">>], [42], [Tagged]
        ]},
        {'bt@stdlib@binary', asBase64, beamtalk_binary, asBase64, 'Binary', asBase64, instance, [
            [<<"abc">>], [42], [Tagged]
        ]},
        {'bt@stdlib@binary', asBase64Url, beamtalk_binary, asBase64Url, 'Binary', asBase64Url,
            instance, [[<<"abc">>], [42], [Tagged]]},
        %% BT-4770: a `Dictionary` parameter is proved behind `is_map`; a non-map
        %% reaches the backing function's own error, a tagged String takes the
        %% fallback arm.
        {'bt@stdlib@tracing', 'class_setContext:', beamtalk_tracing, setContext, 'Tracing',
            'setContext:', class, [[#{workflowId => <<"wf-bt4770">>}], [42], [Tagged]]},
        {'bt@stdlib@digest', 'class_sha256:', beamtalk_digest, sha256, 'Digest', 'sha256:', class, [
                [<<"abc">>], [42], [Tagged]
            ]},
        {'bt@stdlib@digest', 'class_md5:', beamtalk_digest, md5, 'Digest', 'md5:', class, [
            [<<"abc">>], [42], [Tagged]
        ]},
        {'bt@stdlib@file', 'class_exists:', beamtalk_file, exists, 'File', 'exists:', class, [
            [<<"no_such_file_bt4765">>], [42], [Tagged]
        ]},
        {'bt@stdlib@file', 'class_isFile:', beamtalk_file, isFile, 'File', 'isFile:', class, [
            [<<"no_such_file_bt4765">>], [42]
        ]},
        {'bt@stdlib@system', class_osFamily, beamtalk_system, osFamily, 'System', osFamily, class, [
                []
            ]},
        {'bt@stdlib@system', class_hostname, beamtalk_system, hostname, 'System', hostname, class, [
                []
            ]},
        {'bt@stdlib@system', class_osPlatform, beamtalk_system, osPlatform, 'System', osPlatform,
            class, [[]]},
        {'bt@stdlib@system', class_architecture, beamtalk_system, architecture, 'System',
            architecture, class, [[]]},
        {'bt@stdlib@system', class_erlangVersion, beamtalk_system, erlangVersion, 'System',
            erlangVersion, class, [[]]},
        {'bt@stdlib@system', 'class_getEnv:', beamtalk_system, getEnv, 'System', 'getEnv:', class, [
                [<<"BT4765_UNSET_VARIABLE">>], [42], [Tagged]
            ]},
        {'bt@stdlib@digest', 'class_sha512:', beamtalk_digest, sha512, 'Digest', 'sha512:', class, [
                [<<"abc">>], [42], [Tagged]
            ]},
        {'bt@stdlib@digest', 'class_hmacSha256:key:', beamtalk_digest, hmacSha256, 'Digest',
            'hmacSha256:key:', class, [[<<"abc">>, <<"k">>], [42, <<"k">>], [<<"abc">>, 42]]},
        {'bt@stdlib@digest', 'class_hmacSha512:key:', beamtalk_digest, hmacSha512, 'Digest',
            'hmacSha512:key:', class, [[<<"abc">>, <<"k">>], [42, <<"k">>]]},
        {'bt@stdlib@file', 'class_isDirectory:', beamtalk_file, isDirectory, 'File', 'isDirectory:',
            class, [[<<".">>], [<<"no_such_dir_bt4765">>], [42]]},
        {'bt@stdlib@file', class_cwd, beamtalk_file, cwd, 'File', cwd, class, [[]]},
        {'bt@stdlib@file', class_tempDirectory, beamtalk_file, tempDirectory, 'File', tempDirectory,
            class, [[]]},
        {'bt@stdlib@uuid', 'class_isValid:', beamtalk_uuid, isValid, 'Uuid', 'isValid:', class, [
            [<<"not-a-uuid">>], [42], [Tagged]
        ]},
        {'bt@stdlib@workspace', class_isAvailable, beamtalk_workspace_facade, isAvailable,
            'Workspace', isAvailable, class, [[]]}
    ].

%% Switched delegates with no row above, and why: each either answers a
%% different value on every call (clocks, ids, randomness), changes global
%% state (the environment, tracing, telemetry handlers, the console), blocks on
%% input, or needs a live workspace/package to answer at all. The differential
%% above cannot compare two calls of these, and the `{Class, Selector}` error
%% translation they share is covered for any backing function by
%% `beamtalk_erlang_proxy_tests:bt4765_delegate_direct_matches_native_call_test_/0`.
exempt() ->
    #{
        nondeterministic => [
            {'bt@stdlib@date_time', class_monotonicNow},
            {'bt@stdlib@random', class_next},
            {'bt@stdlib@random', 'class_nextInteger:'},
            {'bt@stdlib@system', class_pid},
            {'bt@stdlib@system', class_uniqueId},
            {'bt@stdlib@time', class_monotonic},
            {'bt@stdlib@time', class_nowMs},
            {'bt@stdlib@time', class_nowS},
            {'bt@stdlib@time', class_nowUs},
            {'bt@stdlib@time', class_systemTime},
            {'bt@stdlib@file', class_openHandles}
        ],
        global_state_or_blocking => [
            {'bt@stdlib@console', class_flush},
            {'bt@stdlib@console', class_readLine},
            {'bt@stdlib@console', 'class_readLine:'},
            {'bt@stdlib@system', 'class_setEnv:value:'},
            {'bt@stdlib@system', 'class_unsetEnv:'},
            {'bt@stdlib@system', 'class_getEnv:default:'},
            {'bt@stdlib@telemetry', 'class_detach:'},
            {'bt@stdlib@tracing', class_clear},
            {'bt@stdlib@tracing', class_clearContext},
            {'bt@stdlib@tracing', class_context},
            {'bt@stdlib@tracing', class_disable},
            {'bt@stdlib@tracing', class_enable},
            {'bt@stdlib@tracing', class_isEnabled},
            {'bt@stdlib@tracing', class_maxEvents},
            {'bt@stdlib@tracing', 'class_maxEvents:'},
            {'bt@stdlib@tracing', class_stats},
            {'bt@stdlib@tracing', class_systemHealth},
            {'bt@stdlib@transcript', class_clear},
            {'bt@stdlib@transcript', class_cr}
        ],
        needs_live_workspace => [
            {'bt@stdlib@beamtalk', 'class_erlangHelp:'},
            {'bt@stdlib@beamtalk', 'class_erlangHelp:selector:'},
            {'bt@stdlib@beamtalk', class_version},
            {'bt@stdlib@ets', 'class_exists:'},
            {'bt@stdlib@package', 'class_packageNameFor:'},
            {'bt@stdlib@program', class_commandName}
        ]
    }.

%% Outcome of a call, with a wrapped error reduced to its `#beamtalk_error{}`;
%% the recorded `erlang_stacktrace` differs by construction (the proxy's frame
%% on one path, the delegate's on the other).
outcome(Fun) ->
    try Fun() of
        V -> {ok, V}
    catch
        error:#{error := #beamtalk_error{details = D} = E} when is_map(D) ->
            {beamtalk_error, E#beamtalk_error{details = maps:remove(erlang_stacktrace, D)}};
        error:#{error := #beamtalk_error{} = E} ->
            {beamtalk_error, E};
        C:R ->
            {C, R}
    end.

delegate_matches_native_call_test_() ->
    [
        {lists:flatten(io_lib:format("~p:~p ~p", [CMod, CFun, Args])), fun() ->
            ?assertMatch({module, CMod}, code:ensure_loaded(CMod)),
            Compiled =
                case Kind of
                    instance -> Args;
                    class -> [nil | Args]
                end,
            ?assertEqual(
                outcome(fun() ->
                    beamtalk_erlang_proxy:native_call(BMod, BFun, Args, {Class, Sel}, beamtalk)
                end),
                outcome(fun() -> erlang:apply(CMod, CFun, Compiled) end)
            )
        end}
     || {CMod, CFun, BMod, BFun, Class, Sel, Kind, ArgSets} <- rows(), Args <- ArgSets
    ].

%% Every compiled stdlib method that calls `ffi_call_raised/10` (a switched
%% delegate) has a row above, so a delegate that becomes provable has to come
%% with a differential. Class-side methods are matched by name, since the
%% other half of the key is not in the compiled code.
switched_delegates_are_covered_test() ->
    Exempt = lists:append(maps:values(exempt())),
    Covered = ordsets:from_list(
        [{CMod, CFun} || {CMod, CFun, _, _, _, _, _, _} <- rows()] ++ Exempt
    ),
    Switched = ordsets:from_list(switched_delegates()),
    ?assert(ordsets:size(Switched) > 0),
    ?assertEqual(
        [],
        ordsets:to_list(ordsets:subtract(Switched, Covered)),
        "switched delegates without a conformance row"
    ).

switched_delegates() ->
    Dir = filename:dirname(code:which('bt@stdlib@binary')),
    Beams = filelib:wildcard(filename:join(Dir, "bt@stdlib@*.beam")),
    [
        {Mod, Fun}
     || Beam <- Beams,
        {beam_file, Mod, _, _, _, Funs} <- [beam_disasm:file(Beam)],
        {function, Fun, _Arity, _, Code} <- Funs,
        lists:any(fun calls_ffi_call_raised_10/1, Code)
    ].

calls_ffi_call_raised_10(Instr) when is_tuple(Instr) ->
    lists:any(
        fun(E) -> E =:= {extfunc, beamtalk_erlang_proxy, ffi_call_raised, 10} end,
        tuple_to_list(Instr)
    );
calls_ffi_call_raised_10(_) ->
    false.
