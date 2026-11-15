%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_core_erlang).

%%% **DDD Context:** Compilation Context (test support)

-moduledoc """
Shared EUnit fixture for the two in-memory Core Erlang compile entry points,
`beamtalk_compiler_server:compile_core_erlang/1` and
`beamtalk_build_worker:compile_core_erlang/1` (BT-4900).

Both modules share one contract (valid source -> `{ok, Module, Binary}`,
scan / parse / lint failures -> structured `{error, ...}`, warnings on
stderr), so `compile_tests/2` builds the same six assertions against either,
and the `*_source/1` helpers supply the Core Erlang fixtures.
""".

%% No auto-exported test/0 in this non-test module (keeps dialyzer quiet).
-define(EUNIT_NOAUTO, true).
-include_lib("eunit/include/eunit.hrl").

-export([compile_tests/2, valid_source/1, unbound_var_source/1, warning_source/1]).

-doc """
EUnit tests for a `compile_core_erlang/1` implementation.

`Compile` is the function under test; `Names` gives the fixture module name
for each of `valid`, `unbound_var` and `warn` (so the two callers never share
module names).
""".
-spec compile_tests(fun((binary()) -> term()), #{
    valid := atom(), unbound_var := atom(), warn := atom()
}) -> [{string(), fun(() -> term())}].
compile_tests(Compile, #{valid := ValidMod, unbound_var := UnboundMod, warn := WarnMod}) ->
    [
        {"valid source compiles", fun() ->
            {ok, ModuleName, Binary} = Compile(valid_source(ValidMod)),
            ?assertEqual(ValidMod, ModuleName),
            ?assert(is_binary(Binary)),
            ?assert(byte_size(Binary) > 0)
        end},
        {"scan error", fun() ->
            ?assertMatch(
                {error, {core_scan_error, _}}, Compile(<<"not valid core erlang @#$">>)
            )
        end},
        {"parse error", fun() ->
            %% Scannable tokens but not valid Core Erlang structure
            ?assertMatch({error, {core_parse_error, _}}, Compile(<<"module 'x' []\n">>))
        end},
        {"empty input", fun() ->
            ?assertMatch({error, _}, Compile(<<>>))
        end},
        {"unbound variable is readable prose", fun() ->
            Result = Compile(unbound_var_source(UnboundMod)),
            ?assertMatch({error, {core_compile_error, #{message := _, raw := _}}}, Result),
            {error, {core_compile_error, #{message := Message}}} = Result,
            ?assert(is_binary(Message)),
            %% Readable prose, not the raw {unbound_var, 'State', {foo, 1}} term.
            ?assertEqual(nomatch, binary:match(Message, <<"{unbound_var,">>)),
            ?assertNotEqual(nomatch, binary:match(Message, <<"unbound variable">>)),
            ?assertNotEqual(nomatch, binary:match(Message, <<"'State'">>)),
            ?assertNotEqual(nomatch, binary:match(Message, <<"foo/1">>))
        end},
        {"warning reaches stderr", fun() ->
            Captured = beamtalk_stderr_capture:capture(fun() ->
                {ok, ModuleName, Binary} = Compile(warning_source(WarnMod)),
                ?assertEqual(WarnMod, ModuleName),
                ?assert(is_binary(Binary))
            end),
            ?assertNotEqual(nomatch, binary:match(Captured, <<"Warning:">>))
        end}
    ].

-doc "A valid module exporting `hello/0`.".
-spec valid_source(atom()) -> binary().
valid_source(ModName) ->
    iolist_to_binary([
        "module '",
        atom_to_binary(ModName, utf8),
        "' ['hello'/0]\n"
        "  attributes []\n"
        "  'hello'/0 = fun () -> 'world'\n"
        "end\n"
    ]).

-doc """
'foo'/1 references 'State', which is never a parameter or let-bound - a
genuine core_lint unbound_var failure, a real codegen failure mode
(docs/development/debugging.md § Codegen Debugging).
""".
-spec unbound_var_source(atom()) -> binary().
unbound_var_source(ModName) ->
    iolist_to_binary([
        "module '",
        atom_to_binary(ModName, utf8),
        "' ['foo'/1]\n"
        "  attributes []\n"
        "  'foo'/1 = fun (X) ->\n"
        "    let Y = call 'erlang':'+' (X, State)\n"
        "    in Y\n"
        "end\n"
    ]).

-doc """
'foo'/1 discards the result of `erlang:+/2` via a `do` sequence - a genuine
`sys_core_fold` "ignored result of a call" warning, while still compiling
successfully (the module returns X unchanged).
""".
-spec warning_source(atom()) -> binary().
warning_source(ModName) ->
    iolist_to_binary([
        "module '",
        atom_to_binary(ModName, utf8),
        "' ['foo'/1]\n"
        "  attributes []\n"
        "  'foo'/1 =\n"
        "    fun (X) ->\n"
        "        do\n"
        "            call 'erlang':'+' (1, 2)\n"
        "        X\n"
        "end\n"
    ]).
