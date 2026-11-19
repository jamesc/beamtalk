%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_eval_self_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
EUnit tests for the stateless `evaluate:` path (`beamtalk_repl_eval:eval_with_self/2`)
— the Inspector's value `evaluate:`.

Focus: the per-process transient-module lifecycle. A hot `evaluate:` loop must not
leak a never-reclaimed atom per call, and each evaluation's compiled module must be
fully unloaded afterwards (the `code:delete`-then-`code:purge` order, not the
no-op reverse).
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").

%% A bare `self` expression evaluates to the bound value, with no runtime
%% primitives required (only the compiler, which this suite starts itself
%% rather than relying on an earlier suite in the same VM having done so).
eval_with_self_returns_bound_self_test_() ->
    {setup, fun compiler_setup/0, fun(_) -> ok end,
        ?_assertEqual({ok, 42}, beamtalk_repl_eval:eval_with_self(42, <<"self">>))}.

%% `eval_with_self/2` always rejects a class-definition-shaped
%% source via `eval_not_an_expression_error/0` — it must not register (or,
%% worse, clobber) a `beamtalk_alias_xref` edge along the way, since nothing
%% ever reads the ClassInfo that would drive that registration. A pre-existing
%% edge for the same class name (as a prior real `:load` would have left
%% behind) must survive the rejected `evaluate:` call completely untouched:
%% `compile_class_definition_result/2`'s `register_class(ClassNameBin,
%% ReferencedAliases)` is whole-set replacement, not a delta, so it must
%% never be reached for a rejected evaluation.
eval_with_self_class_definition_does_not_touch_alias_xref_test_() ->
    {setup, fun alias_xref_setup/0, fun alias_xref_teardown/1, fun() ->
        ok = beamtalk_alias_xref:register_class(<<"EvalSelfXrefClass">>, [
            <<"EvalSelfXrefAlias">>
        ]),
        Source = <<"Object subclass: EvalSelfXrefClass\n\n  bar => 1\n">>,
        ?assertMatch({error, _}, beamtalk_repl_eval:eval_with_self(nil, Source)),
        ?assertEqual(
            [<<"EvalSelfXrefClass">>],
            beamtalk_alias_xref:dependents_of(<<"EvalSelfXrefAlias">>)
        )
    end}.

compiler_setup() ->
    beamtalk_test_boot:compiler().

alias_xref_setup() ->
    ok = compiler_setup(),
    Started =
        case whereis(beamtalk_alias_xref) of
            undefined ->
                {ok, _} = beamtalk_alias_xref:start_link(),
                true;
            _Pid ->
                false
        end,
    ok = beamtalk_alias_xref:clear(),
    Started.

%% Stop the server only if this fixture started it: left running bare it makes
%% the workspace supervisor's own `beamtalk_alias_xref` child fail with
%% `already_started` in whichever suite starts the workspace next (BT-4934).
alias_xref_teardown(true) ->
    ok = beamtalk_alias_xref:clear(),
    beamtalk_test_proc:stop_registered(beamtalk_alias_xref);
alias_xref_teardown(false) ->
    ok = beamtalk_alias_xref:clear().

%% Repeated `evaluate:` calls in one process reuse a single module-name atom
%% (cached in the process dictionary), so a tight loop cannot exhaust the atom
%% table, and each transient module is fully unloaded after its call. The test
%% body runs in a fresh process (`{spawn, ...}`), so the cached name starts
%% empty here whatever ran earlier in the same EUnit process.
eval_with_self_reuses_module_and_unloads_test_() ->
    {setup, fun compiler_setup/0, fun(_) -> ok end,
        {spawn, ?_test(eval_with_self_reuses_module_and_unloads())}}.

eval_with_self_reuses_module_and_unloads() ->
    ?assertEqual(undefined, get('$beamtalk_inspector_eval_module')),
    ?assertEqual({ok, 1}, beamtalk_repl_eval:eval_with_self(1, <<"self">>)),
    Module = get('$beamtalk_inspector_eval_module'),
    ?assert(is_atom(Module)),
    %% The transient module is fully unloaded after the call.
    ?assertEqual(false, code:is_loaded(Module)),
    %% A second call recycles the same atom — no new module name is minted.
    ?assertEqual({ok, 2}, beamtalk_repl_eval:eval_with_self(2, <<"self">>)),
    ?assertEqual(Module, get('$beamtalk_inspector_eval_module')),
    ?assertEqual(false, code:is_loaded(Module)).

%% BT-5019: a block returned from `evaluate:` is a value the caller may call
%% later. The transient eval module must therefore stay loaded while the block
%% can still be called, instead of being hard-purged out from under it.
eval_with_self_returned_block_survives_test_() ->
    {setup, fun compiler_setup/0, fun(_) -> ok end,
        {spawn, ?_test(eval_with_self_returned_block_survives())}}.

eval_with_self_returned_block_survives() ->
    {ok, Block} = beamtalk_repl_eval:eval_with_self(nil, <<"[:x | x + 1]">>),
    ?assert(is_function(Block, 1)),
    ?assertEqual(8, Block(7)),
    %% A later `evaluate:` in the same process must not disturb the earlier block.
    ?assertEqual({ok, 5}, beamtalk_repl_eval:eval_with_self(5, <<"self">>)),
    ?assertEqual(8, Block(7)),
    %% ...including another block-returning call.
    {ok, Block2} = beamtalk_repl_eval:eval_with_self(nil, <<"[:x | x * 2]">>),
    ?assertEqual(8, Block(7)),
    ?assertEqual(14, Block2(7)).

%% A block nested inside a returned collection is held just the same.
eval_with_self_nested_block_survives_test_() ->
    {setup, fun compiler_setup/0, fun(_) -> ok end,
        {spawn, ?_test(eval_with_self_nested_block_survives())}}.

eval_with_self_nested_block_survives() ->
    {ok, {Block}} = beamtalk_repl_eval:eval_with_self(nil, <<"{[:x | x + 1]}">>),
    ?assertEqual({ok, 1}, beamtalk_repl_eval:eval_with_self(1, <<"self">>)),
    ?assertEqual(3, Block(2)).

%% A process still running eval-module code when `evaluate:` returns must not be
%% killed by the cleanup.
eval_with_self_running_process_survives_test_() ->
    {setup, fun compiler_setup/0, fun(_) -> ok end,
        {spawn, ?_test(eval_with_self_running_process_survives())}}.

eval_with_self_running_process_survives() ->
    %% The block calls the block it is given, so the holder is parked inside
    %% eval-module code until it is told to go.
    {ok, Block} = beamtalk_repl_eval:eval_with_self(nil, <<"[:f | f value. 42]">>),
    Parent = self(),
    Holder = spawn(fun() ->
        Parent !
            {done,
                Block(fun() ->
                    receive
                        go -> ok
                    end
                end)}
    end),
    Ref = erlang:monitor(process, Holder),
    %% sleep: let the holder enter the block before the next evaluate: runs
    timer:sleep(100),
    ?assertEqual({ok, 3}, beamtalk_repl_eval:eval_with_self(3, <<"self">>)),
    ?assert(erlang:is_process_alive(Holder)),
    Holder ! go,
    ?awaitMessage({done, 42}, 2000),
    ?awaitMessage({'DOWN', Ref, process, Holder, normal}, 1000).

%% A closure from another module that captures an eval-module block keeps the
%% module alive too.
eval_with_self_captured_block_survives_test_() ->
    {setup, fun compiler_setup/0, fun(_) -> ok end,
        {spawn, ?_test(eval_with_self_captured_block_survives())}}.

eval_with_self_captured_block_survives() ->
    Wrap = fun(B) -> fun(X) -> B(X) end end,
    {ok, Closure} = beamtalk_repl_eval:eval_with_self(Wrap, <<"self value: [:x | x + 1]">>),
    ?assertEqual({ok, 1}, beamtalk_repl_eval:eval_with_self(1, <<"self">>)),
    ?assertEqual(3, Closure(2)).

%% The result holds no block, but a process spawned by the evaluated source is
%% still running eval-module code. The cleanup must not kill it; the old code
%% goes on the pending list and a later `evaluate:` releases it once the process
%% has finished.
eval_with_self_pending_old_code_released_test_() ->
    {setup, fun compiler_setup/0, fun(_) -> ok end,
        {spawn, ?_test(eval_with_self_pending_old_code_released())}}.

eval_with_self_pending_old_code_released() ->
    %% `self` is a fun: given a block it runs the block in a new process and
    %% returns `started`; given anything else it parks until told to go.
    Parent = self(),
    Wrap = fun
        (B) when is_function(B, 0) ->
            Holder = spawn(fun() ->
                Parent ! {holder, self()},
                B(),
                Parent ! finished
            end),
            Holder,
            %% return only once the holder is parked inside eval-module code
            receive
                parked -> started
            end;
        (_) ->
            Parent ! parked,
            receive
                go -> ok
            end
    end,
    {ok, started} = beamtalk_repl_eval:eval_with_self(Wrap, <<"self value: [self value: 1. 5]">>),
    Holder =
        receive
            {holder, H} -> H
        after 2000 -> error(no_holder)
        end,
    Module = hd(get('$beamtalk_inspector_eval_pending')),
    %% The holder still runs the (now old) code: it must be alive, and the
    %% module is only released by a later call after it has finished.
    ?assert(erlang:is_process_alive(Holder)),
    ?assert(erlang:check_old_code(Module)),
    ?assertEqual(undefined, get('$beamtalk_inspector_eval_module')),
    Holder ! go,
    ?awaitMessage(finished, 2000),
    ?assertEqual({ok, 7}, beamtalk_repl_eval:eval_with_self(7, <<"self">>)),
    ?assertEqual(false, erlang:check_old_code(Module)),
    ?assertEqual([], get('$beamtalk_inspector_eval_pending')).
