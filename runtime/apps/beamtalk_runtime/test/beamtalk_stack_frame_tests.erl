%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_stack_frame_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
Unit tests for beamtalk_stack_frame module.

Tests wrap/1, dispatch/3, has_method/1, and internal helpers.
""".
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%% ===================================================================
%%% wrap/1 tests
%%% ===================================================================

wrap_valid_stacktrace_test() ->
    Stacktrace = [
        {some_module, some_fun, 2, [{file, "test.erl"}, {line, 42}]},
        {other_mod, other_fun, [arg1, arg2], [{file, "other.erl"}, {line, 10}]}
    ],
    Result = beamtalk_stack_frame:wrap(Stacktrace),
    ?assertEqual(2, length(Result)),
    [Frame1, Frame2] = Result,
    ?assertMatch(#{'$beamtalk_class' := 'StackFrame'}, Frame1),
    ?assertMatch(#{'$beamtalk_class' := 'StackFrame'}, Frame2).

wrap_empty_list_test() ->
    ?assertEqual([], beamtalk_stack_frame:wrap([])).

wrap_non_list_returns_empty_test() ->
    ?assertEqual([], beamtalk_stack_frame:wrap(not_a_list)).

wrap_frame_with_arity_test() ->
    Stacktrace = [{mod, fun1, 3, [{file, "f.erl"}, {line, 1}]}],
    [Frame] = beamtalk_stack_frame:wrap(Stacktrace),
    ?assertEqual(3, maps:get(arity, Frame)).

wrap_frame_with_args_list_test() ->
    Stacktrace = [{mod, fun1, [a, b, c], [{file, "f.erl"}, {line, 1}]}],
    [Frame] = beamtalk_stack_frame:wrap(Stacktrace),
    ?assertEqual(3, maps:get(arity, Frame)).

wrap_frame_no_location_test() ->
    Stacktrace = [{mod, fun1, 2, []}],
    [Frame] = beamtalk_stack_frame:wrap(Stacktrace),
    ?assertEqual(nil, maps:get(file, Frame)),
    ?assertEqual(nil, maps:get(line, Frame)).

wrap_frame_file_becomes_binary_test() ->
    Stacktrace = [{mod, fun1, 0, [{file, "src/test.erl"}, {line, 5}]}],
    [Frame] = beamtalk_stack_frame:wrap(Stacktrace),
    ?assertEqual(<<"src/test.erl">>, maps:get(file, Frame)),
    ?assertEqual(5, maps:get(line, Frame)).

wrap_malformed_frame_test() ->
    Stacktrace = [not_a_tuple],
    [Frame] = beamtalk_stack_frame:wrap(Stacktrace),
    ?assertEqual(undefined, maps:get(module, Frame)),
    ?assertEqual(undefined, maps:get(function, Frame)),
    ?assertEqual(0, maps:get(arity, Frame)),
    ?assertEqual(nil, maps:get(file, Frame)),
    ?assertEqual(nil, maps:get(line, Frame)),
    ?assertEqual(nil, maps:get(class_name, Frame)).

%%% ===================================================================
%%% dispatch/3 tests
%%% ===================================================================

make_test_frame() ->
    #{
        '$beamtalk_class' => 'StackFrame',
        module => beamtalk_integer,
        function => '+',
        arity => 2,
        file => <<"src/beamtalk_integer.erl">>,
        line => 42,
        class_name => 'Integer'
    }.

make_nil_frame() ->
    #{
        '$beamtalk_class' => 'StackFrame',
        module => undefined,
        function => undefined,
        arity => 0,
        file => nil,
        line => nil,
        class_name => nil
    }.

dispatch_class_test() ->
    ?assertEqual('StackFrame', beamtalk_stack_frame:dispatch('class', [], make_test_frame())).

dispatch_method_test() ->
    ?assertEqual('+', beamtalk_stack_frame:dispatch('method', [], make_test_frame())).

dispatch_receiver_class_test() ->
    ?assertEqual('Integer', beamtalk_stack_frame:dispatch('receiverClass', [], make_test_frame())).

dispatch_receiver_class_nil_test() ->
    ?assertEqual(nil, beamtalk_stack_frame:dispatch('receiverClass', [], make_nil_frame())).

dispatch_arguments_test() ->
    ?assertEqual(2, beamtalk_stack_frame:dispatch('arguments', [], make_test_frame())).

dispatch_source_location_test() ->
    Result = beamtalk_stack_frame:dispatch('sourceLocation', [], make_test_frame()),
    ?assertEqual({beamtalk_string, <<"src/beamtalk_integer.erl:42">>}, Result).

dispatch_source_location_nil_file_test() ->
    ?assertEqual(nil, beamtalk_stack_frame:dispatch('sourceLocation', [], make_nil_frame())).

dispatch_source_location_nil_line_test() ->
    Frame = (make_test_frame())#{line => nil},
    ?assertEqual(nil, beamtalk_stack_frame:dispatch('sourceLocation', [], Frame)).

dispatch_module_name_test() ->
    Result = beamtalk_stack_frame:dispatch('moduleName', [], make_test_frame()),
    ?assertEqual({beamtalk_string, <<"beamtalk_integer">>}, Result).

dispatch_module_name_undefined_test() ->
    ?assertEqual(nil, beamtalk_stack_frame:dispatch('moduleName', [], make_nil_frame())).

dispatch_line_test() ->
    ?assertEqual(42, beamtalk_stack_frame:dispatch('line', [], make_test_frame())).

dispatch_line_nil_test() ->
    ?assertEqual(nil, beamtalk_stack_frame:dispatch('line', [], make_nil_frame())).

dispatch_file_test() ->
    ?assertEqual(
        {beamtalk_string, <<"src/beamtalk_integer.erl">>},
        beamtalk_stack_frame:dispatch('file', [], make_test_frame())
    ).

dispatch_file_nil_test() ->
    ?assertEqual(nil, beamtalk_stack_frame:dispatch('file', [], make_nil_frame())).

dispatch_print_string_test() ->
    Result = beamtalk_stack_frame:dispatch('printString', [], make_test_frame()),
    ?assertMatch({beamtalk_string, _}, Result),
    ?assertNotEqual({beamtalk_string, <<>>}, Result).

%% dispatch/3 has no catch-all clause. Unknown selectors are handled by the
%% compiled bt@stdlib@stack_frame module via Object inheritance (dispatched
%% through value_type_send).
dispatch_unknown_selector_raises_test() ->
    Frame = make_test_frame(),
    ?assertError(function_clause, beamtalk_stack_frame:dispatch('nonExistent', [], Frame)).

%%% ===================================================================
%%% has_method/1 tests
%%% ===================================================================

has_method_class_test() -> ?assert(beamtalk_stack_frame:has_method('class')).
has_method_method_test() -> ?assert(beamtalk_stack_frame:has_method('method')).
has_method_receiver_class_test() -> ?assert(beamtalk_stack_frame:has_method('receiverClass')).
has_method_arguments_test() -> ?assert(beamtalk_stack_frame:has_method('arguments')).
has_method_source_location_test() -> ?assert(beamtalk_stack_frame:has_method('sourceLocation')).
has_method_module_name_test() -> ?assert(beamtalk_stack_frame:has_method('moduleName')).
has_method_line_test() -> ?assert(beamtalk_stack_frame:has_method('line')).
has_method_file_test() -> ?assert(beamtalk_stack_frame:has_method('file')).
has_method_print_string_test() -> ?assert(beamtalk_stack_frame:has_method('printString')).
has_method_unknown_test() -> ?assertNot(beamtalk_stack_frame:has_method('nonExistent')).

%%% ===================================================================
%%% format_frame tests (via printString dispatch)
%%% ===================================================================

format_frame_with_full_location_test() ->
    Result = beamtalk_stack_frame:dispatch('printString', [], make_test_frame()),
    ?assertEqual({beamtalk_string, <<"Integer>>+ (src/beamtalk_integer.erl:42)">>}, Result).

%% No class for a non-Beamtalk module: an Erlang frame, `module:function/arity`.
format_frame_nil_class_test() ->
    Frame = (make_test_frame())#{class_name => nil},
    Result = beamtalk_stack_frame:dispatch('printString', [], Frame),
    ?assertEqual(
        {beamtalk_string, <<"beamtalk_integer:+/2 (src/beamtalk_integer.erl:42)">>}, Result
    ).

format_frame_undefined_function_test() ->
    Frame = (make_test_frame())#{function => undefined},
    Result = beamtalk_stack_frame:dispatch('printString', [], Frame),
    ?assertEqual(
        {beamtalk_string, <<"Integer>>? (src/beamtalk_integer.erl:42)">>}, Result
    ).

format_frame_no_location_test() ->
    Frame = (make_test_frame())#{file => nil, line => nil},
    Result = beamtalk_stack_frame:dispatch('printString', [], Frame),
    ?assertEqual({beamtalk_string, <<"Integer>>+">>}, Result).

format_frame_file_no_line_test() ->
    Frame = (make_test_frame())#{line => nil},
    Result = beamtalk_stack_frame:dispatch('printString', [], Frame),
    ?assertEqual({beamtalk_string, <<"Integer>>+ (src/beamtalk_integer.erl)">>}, Result).

format_frame_block_marker_test() ->
    Frame = (make_test_frame())#{function => '-run/0-fun-3-', arity => 2},
    ?assertEqual(
        <<"[] in Integer>>run (src/beamtalk_integer.erl:42)">>,
        beamtalk_stack_frame:format_frame(Frame)
    ).

format_frame_beamtalk_never_emits_mfa_test() ->
    Out = beamtalk_stack_frame:format_frame(make_test_frame()),
    ?assertEqual(nomatch, re:run(Out, "[a-z_@]+:[^ ]+/[0-9]+")),
    ?assertEqual(nomatch, re:run(Out, "/[0-9]+( |$)")).

%%% ===================================================================
%%% module_to_class/1 tests
%%%
%%% Resolution is the runtime class table only (ADR 0139 decision 2): a module
%%% the table does not know is `nil`, whatever its name looks like. The
%%% registry-hit cases are in the "class registry resolution" section below.
%%% ===================================================================

%% Registry miss: a `bt@` module that no class is registered under is not
%% guessed into a class name, however plausible the guess ('bt@stdlib@integer'
%% is 'Integer' only when the class table says so).
module_to_class_unregistered_bt_module_is_nil_test() ->
    ?assertEqual(nil, beamtalk_stack_frame:module_to_class('bt@bt4815_never_registered')),
    ?assertEqual(
        nil, beamtalk_stack_frame:module_to_class('bt@stdlib@bt4815_never_registered')
    ),
    ?assertEqual(
        nil, beamtalk_stack_frame:module_to_class('beamtalk_bt4815_never_registered')
    ).

%% An Erlang module that was never a Beamtalk class.
module_to_class_plain_unknown_module_test() ->
    ?assertEqual(nil, beamtalk_stack_frame:module_to_class(lists)).

%% Non-atom input: guard clause returns nil.
module_to_class_non_atom_test() ->
    ?assertEqual(nil, beamtalk_stack_frame:module_to_class(42)),
    ?assertEqual(nil, beamtalk_stack_frame:module_to_class(<<"counter">>)).

%%% ===================================================================
%%% module_to_class/1 — class registry resolution (regression)
%%%
%%% The string heuristic (bt@/beamtalk_ prefix parsing + snake_to_class), removed
%%% in BT-4815, was provably lossy for acronym-cased classes: 'bt@stdlib@beamerror' can only
%%% capitalize back to 'Beamerror', never the real 'BEAMError' — the original
%%% casing inside the letter run is gone once folded to snake_case. A class
%%% registered with such a module must resolve via the live class registry
%%% instead, which knows the real name.
%%% ===================================================================

module_to_class_resolves_acronym_class_via_registry_test_() ->
    {setup,
        fun() ->
            beamtalk_test_boot:registry(),
            ClassInfo = #{
                superclass => none,
                methods => #{},
                class_methods => #{},
                %% Same shape as the real BEAMError/bt@stdlib@beamerror
                %% collision, namespaced so it can't collide with
                %% the real stdlib class if this suite runs against a live
                %% stdlib-loaded node.
                module => 'bt@stdlib@beamerror_bt3081_test'
            },
            {ok, Pid} = beamtalk_object_class:start_link(
                'BEAMErrorBT3081Test', ClassInfo
            ),
            Pid
        end,
        fun(Pid) ->
            (try
                gen_server:stop(Pid)
            catch
                _:_ -> ok
            end)
        end,
        fun(_Pid) ->
            [
                {"resolves the real acronym-preserving class name via the registry, not the lossy heuristic guess",
                    fun() ->
                        ?assertEqual(
                            'BEAMErrorBT3081Test',
                            beamtalk_stack_frame:module_to_class('bt@stdlib@beamerror_bt3081_test')
                        ),
                        %% Confirm the heuristic alone really would have gotten
                        %% this wrong — that's the bug this regression guards.
                        ?assertNotEqual(
                            'BEAMErrorBT3081Test',
                            beamtalk_module_name:snake_to_class("beamerror_bt3081_test")
                        )
                    end}
            ]
        end}.

%%% ===================================================================
%%% The ADR 0139 frame table (BT-4815)
%%%
%%% Frames go through `wrap/1`, so each row is resolved through the runtime
%%% class table exactly as `e stackTrace` does. Classes and modules carry a
%%% `bt4815` marker so they cannot collide with a live stdlib.
%%% ===================================================================

-define(SOLO_MOD, 'bt@bt4815pkg@bt4815_solo').
-define(DUPE_A, 'bt@bt4815a@bt4815_dupe').
-define(DUPE_B, 'bt@bt4815b@bt4815_dupe').
-define(OLD_A, 'bt@bt4815o1@bt4815_old').
-define(OLD_B, 'bt@bt4815o2@bt4815_old').
-define(NEST_A, 'bt@bt4815n@a@bt4815_nest').
-define(NEST_B, 'bt@bt4815n@b@bt4815_nest').

frame_table_test_() ->
    {setup,
        fun() ->
            beamtalk_test_boot:registry(),
            Mods = [?SOLO_MOD, ?DUPE_A, ?DUPE_B, ?NEST_A, ?NEST_B],
            [load_empty_module(M) || M <- Mods],
            Pids = [
                register_class('Bt4815Solo', ?SOLO_MOD),
                register_class('Bt4815Dupe', ?DUPE_A),
                register_class('Bt4815Nest', ?NEST_A)
            ],
            {Mods, Pids}
        end,
        fun({Mods, Pids}) ->
            _ = Pids,
            [
                beamtalk_test_class:stop_graceful(N)
             || N <- ['Bt4815Solo', 'Bt4815Dupe', 'Bt4815Nest']
            ],
            [unload_module(M) || M <- Mods]
        end,
        fun(_) ->
            [
                {"instance method", fun() ->
                    ?assertEqual(
                        <<"Bt4815Solo>>increment (counter.bt:12)">>,
                        render({?SOLO_MOD, increment, 1, loc(12)})
                    )
                end},
                {"class-side method has no class_ prefix", fun() ->
                    ?assertEqual(
                        <<"Bt4815Solo class>>new (counter.bt:3)">>,
                        render({?SOLO_MOD, class_new, 1, loc(3)})
                    ),
                    ?assertEqual(
                        <<"Bt4815Solo class>>encode:headers:table: (counter.bt:3)">>,
                        render({?SOLO_MOD, 'class_encode:headers:table:', 4, loc(3)})
                    )
                end},
                {"block in an instance method", fun() ->
                    ?assertEqual(
                        <<"[] in Bt4815Solo>>increment (counter.bt:14)">>,
                        render({?SOLO_MOD, '-increment/0-fun-0-', 2, loc(14)})
                    )
                end},
                {"block in a class-side method", fun() ->
                    ?assertEqual(
                        <<"[] in Bt4815Solo class>>new (counter.bt:14)">>,
                        render({?SOLO_MOD, '-class_new/1-fun-0-', 2, loc(14)})
                    )
                end},
                {"Erlang frame is module:function/arity, never Module>>function", fun() ->
                    ?assertEqual(
                        <<"lists:foldl/3 (lists.erl:2471)">>,
                        render({lists, foldl, 3, [{file, "lists.erl"}, {line, 2471}]})
                    ),
                    %% arity as recorded, also when the stacktrace carries the argument list
                    ?assertEqual(
                        <<"gen_server:try_handle_call/4 (gen_server.erl:2470)">>,
                        render(
                            {gen_server, try_handle_call, [a, b, c, d], [
                                {file, "gen_server.erl"}, {line, 2470}
                            ]}
                        )
                    ),
                    %% a lifted fun in Erlang code is the name its author's compiler recorded
                    ?assertEqual(
                        <<"lists:-foldl/3-fun-0-/1">>,
                        render({lists, '-foldl/3-fun-0-', 1, []})
                    )
                end},
                {"Erlang frame without a location has none", fun() ->
                    ?assertEqual(<<"erlang:apply/2">>, render({erlang, apply, 2, []}))
                end},
                {"registry miss: raw module, nothing guessed", fun() ->
                    ?assertEqual(
                        <<"bt@user@bt4815_hpack>>class_encode:headers:table: (counter.bt:40)">>,
                        render(
                            {'bt@user@bt4815_hpack', 'class_encode:headers:table:', 4, loc(40)}
                        )
                    ),
                    ?assertEqual(
                        <<"[] in bt@user@bt4815_hpack>>class_encode:headers:table: (counter.bt:40)">>,
                        render(
                            {'bt@user@bt4815_hpack', '-class_encode:headers:table:/4-fun-0-', 2,
                                loc(40)}
                        )
                    ),
                    %% the miss is not a class, so `receiverClass` stays nil
                    [Wrapped] = beamtalk_stack_frame:wrap([
                        {'bt@user@bt4815_hpack', run, 0, []}
                    ]),
                    ?assertEqual(nil, maps:get(class_name, Wrapped))
                end},
                {"a unique class name is not package-qualified", fun() ->
                    [Wrapped] = beamtalk_stack_frame:wrap([{?SOLO_MOD, run, 0, loc(1)}]),
                    ?assertEqual(nil, maps:get(package, Wrapped)),
                    ?assertEqual(<<"Bt4815Solo">>, maps:get(class, parts({?SOLO_MOD, run, 0, []})))
                end},
                {"a class name shared by two loaded classes is package-qualified", fun() ->
                    ?assertEqual(
                        <<"bt4815a@Bt4815Dupe>>go (dupe.bt:5)">>,
                        render({?DUPE_A, go, 1, [{file, "dupe.bt"}, {line, 5}]})
                    ),
                    ?assertEqual(
                        <<"bt4815a@Bt4815Dupe class>>make (dupe.bt:5)">>,
                        render({?DUPE_A, class_make, 1, [{file, "dupe.bt"}, {line, 5}]})
                    ),
                    %% the qualified class is still the class for `receiverClass`
                    [Wrapped] = beamtalk_stack_frame:wrap([{?DUPE_A, go, 1, []}]),
                    ?assertEqual('Bt4815Dupe', maps:get(class_name, Wrapped))
                end},
                {"nested packages are qualified by the whole package path", fun() ->
                    ?assertEqual(
                        <<"bt4815n@a@Bt4815Nest>>go">>,
                        render({?NEST_A, go, 1, []})
                    )
                end},
                {"a same-named module with only old code does not qualify", fun() ->
                    %% Delete without purging, as class removal does: the module
                    %% stays in erlang:loaded/0 as old code only.
                    load_empty_module(?OLD_B),
                    load_empty_module(?OLD_A),
                    register_class('Bt4815Old', ?OLD_A),
                    try
                        ?assertEqual(
                            <<"bt4815o1@Bt4815Old>>go">>, render({?OLD_A, go, 1, []})
                        ),
                        code:delete(?OLD_B),
                        ?assertEqual(<<"Bt4815Old>>go">>, render({?OLD_A, go, 1, []}))
                    after
                        beamtalk_test_class:stop_graceful('Bt4815Old'),
                        unload_module(?OLD_A),
                        unload_module(?OLD_B)
                    end
                end},
                {"the single-frame path resolves like wrap/1", fun() ->
                    Frame = {?DUPE_A, go, 1, []},
                    ?assertEqual(
                        beamtalk_stack_frame:format_frame(hd(beamtalk_stack_frame:wrap([Frame]))),
                        beamtalk_stack_frame:format_frame(beamtalk_stack_frame:wrap_frame(Frame))
                    )
                end},
                {"render_mfa/1 uses the same notation without a location", fun() ->
                    ?assertEqual(
                        <<"Bt4815Solo>>increment">>,
                        beamtalk_stack_frame:render_mfa({?SOLO_MOD, increment, 1})
                    ),
                    ?assertEqual(
                        <<"Bt4815Solo class>>new">>,
                        beamtalk_stack_frame:render_mfa({?SOLO_MOD, class_new, 1})
                    ),
                    ?assertEqual(
                        <<"gen_tcp:connect/3">>,
                        beamtalk_stack_frame:render_mfa({gen_tcp, connect, 3})
                    ),
                    ?assertEqual(
                        <<"bt@user@bt4815_hpack>>class_new">>,
                        beamtalk_stack_frame:render_mfa({'bt@user@bt4815_hpack', class_new, 1})
                    )
                end},
                {"BT-4816: undef on a Beamtalk class names the method", fun() ->
                    ST = [{?SOLO_MOD, class_start, [a], []}],
                    #{error := E} = beamtalk_exception_handler:ensure_wrapped(error, undef, ST),
                    ?assertEqual(
                        {beamtalk_string, <<"Undefined function: Bt4815Solo class>>start">>},
                        E#beamtalk_error.message
                    ),
                    ?assertEqual(
                        <<"bt@bt4815pkg@bt4815_solo:class_start/1">>,
                        maps:get(mfa, E#beamtalk_error.details)
                    )
                end},
                {"BT-4816: undef on an unresolved bt@ module keeps the raw MFA in details", fun() ->
                        ST = [{'bt@user@bt4816_unknown', run, 2, []}],
                        #{error := E} = beamtalk_exception_handler:ensure_wrapped(error, undef, ST),
                        ?assertEqual(
                            <<"bt@user@bt4816_unknown:run/2">>,
                            maps:get(mfa, E#beamtalk_error.details)
                        )
                    end},
                {"BT-4816: undef on foreign Erlang stays module:function/arity", fun() ->
                    ST = [{some_mod, some_fun, 2, []}],
                    #{error := E} = beamtalk_exception_handler:ensure_wrapped(error, undef, ST),
                    ?assertEqual(
                        {beamtalk_string, <<"Undefined function: some_mod:some_fun/2">>},
                        E#beamtalk_error.message
                    ),
                    ?assertNot(maps:is_key(mfa, E#beamtalk_error.details))
                end},
                {"BT-4816: method error tail renders a Beamtalk callee as Class>>selector", fun() ->
                        Msg = beamtalk_actor:format_method_error_message(
                            'Ticker', bump, error, badarg, [{?SOLO_MOD, class_go, 1, []}]
                        ),
                        ?assertEqual(
                            <<"Bad argument in Ticker>>bump (called Bt4815Solo class>>go)">>, Msg
                        )
                    end},
                {"BT-4816: method error tail keeps a foreign callee as module:function/arity",
                    fun() ->
                        Msg = beamtalk_actor:format_method_error_message(
                            'Ticker', bump, error, badarg, [{lists, reverse, [x, y], []}]
                        ),
                        ?assertEqual(
                            <<"Bad argument in Ticker>>bump (called lists:reverse/2)">>, Msg
                        )
                    end},
                {"raw_mfa/1 is always module:function/arity", fun() ->
                    ?assertEqual(
                        <<"bt@bt4815pkg@bt4815_solo:class_new/1">>,
                        beamtalk_stack_frame:raw_mfa({?SOLO_MOD, class_new, 1})
                    ),
                    ?assertEqual(
                        <<"lists:foldl/3">>, beamtalk_stack_frame:raw_mfa({lists, foldl, 3})
                    )
                end},
                {"frame_parts/1 carries the structured pieces", fun() ->
                    ?assertMatch(
                        #{
                            kind := beamtalk,
                            class := <<"Bt4815Solo">>,
                            selector := <<"new">>,
                            class_side := true,
                            block := true,
                            line := 3
                        },
                        parts({?SOLO_MOD, '-class_new/1-fun-0-', 2, loc(3)})
                    ),
                    ?assertMatch(
                        #{
                            kind := erlang,
                            class := <<"lists">>,
                            selector := <<"foldl">>,
                            arity := 3
                        },
                        parts({lists, foldl, 3, []})
                    ),
                    ?assertMatch(
                        #{kind := unresolved, class := <<"bt@user@bt4815_hpack">>},
                        parts({'bt@user@bt4815_hpack', run, 0, []})
                    )
                end}
            ]
        end}.

render(Frame) ->
    beamtalk_stack_frame:format_frame(hd(beamtalk_stack_frame:wrap([Frame]))).

parts(Frame) ->
    beamtalk_stack_frame:frame_parts(hd(beamtalk_stack_frame:wrap([Frame]))).

loc(Line) -> [{file, "counter.bt"}, {line, Line}].

register_class(Name, Module) ->
    beamtalk_test_class:start(Name, #{
        superclass => none,
        methods => #{},
        class_methods => #{},
        module => Module
    }).

load_empty_module(Module) ->
    {ok, Module, Bin} = compile:forms([{attribute, 1, module, Module}], [binary]),
    {module, Module} = code:load_binary(Module, atom_to_list(Module) ++ ".beam", Bin),
    ok.

unload_module(Module) ->
    code:purge(Module),
    code:delete(Module),
    code:purge(Module),
    ok.

%%% ===================================================================
%%% Frame visibility (BT-4818)
%%% ===================================================================

with_native_package(Modules, Fun) ->
    App = bt4818_pkg,
    ok = application:load(
        {application, App, [
            {description, "bt4818"},
            {vsn, "0.0.1"},
            {modules, []},
            {registered, []},
            {applications, [kernel]},
            {env, [{native_modules, Modules}]}
        ]}
    ),
    try
        Fun()
    after
        application:unload(App)
    end.

shown(Stack) ->
    [
        {M, F}
     || #{module := M, function := F} <- beamtalk_stack_frame:visible_frames(
            beamtalk_stack_frame:wrap(Stack)
        )
    ].

%% A package's declared native module is shown, wherever it sits in the trace,
%% even under the `beamtalk_` prefix the ADR tells packages to use.
visibility_package_native_module_shown_test() ->
    with_native_package([bt4818_native, beamtalk_bt4818_http], fun() ->
        Stack = [
            {lists, map, 2, []},
            {bt4818_native, decode, 1, [{file, "native/bt4818_native.erl"}, {line, 9}]},
            {beamtalk_bt4818_http, get, 1, []},
            {beamtalk_dispatch, lookup, 3, []},
            {gen_server, handle_msg, 6, []}
        ],
        ?assertEqual(
            [{lists, map}, {bt4818_native, decode}, {beamtalk_bt4818_http, get}],
            shown(Stack)
        )
    end).

%% Declared natives are read from the `native_modules` app env and nowhere else.
visibility_declared_native_modules_source_test() ->
    ?assertNot(maps:is_key(bt4818_native, beamtalk_package:declared_native_modules())),
    with_native_package([bt4818_native], fun() ->
        ?assert(maps:is_key(bt4818_native, beamtalk_package:declared_native_modules()))
    end),
    ?assertNot(maps:is_key(bt4818_native, beamtalk_package:declared_native_modules())).

%% A failure raised inside OTP: the raising frame is shown, the plumbing is not.
visibility_raising_otp_frame_shown_plumbing_hidden_test() ->
    Stack = [
        {lists, foldl, 3, [{file, "lists.erl"}, {line, 2471}]},
        {gen_server, try_handle_call, 4, []},
        {gen_server, handle_msg, 6, []},
        {proc_lib, init_p_do_apply, 3, []}
    ],
    ?assertEqual([{lists, foldl}], shown(Stack)),
    %% Only the innermost frame counts as the raising one.
    ?assertEqual([{gen_server, try_handle_call}], shown(tl(Stack))),
    %% With a runtime internal innermost, nothing of the OTP plumbing is listed.
    ?assertEqual([], shown([{beamtalk_error, raise, 1, []} | tl(Stack)])).

%% `beamtalk_*` runtime internals are hidden, the raising one included.
visibility_runtime_internal_hidden_test() ->
    Stack = [
        {beamtalk_error, raise, 1, []},
        {beamtalk_dispatch, send, 3, []}
    ],
    ?assertEqual([], shown(Stack)).

%% Compile and load a module from source text, for frames that need a real module.
load_module(Name, Source) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    Forms = [
        begin
            {ok, F} = erl_parse:parse_form(T),
            F
        end
     || T <- split_forms(Tokens)
    ],
    {ok, Name, Bin} = compile:forms(Forms),
    {module, Name} = code:load_binary(Name, "bt4818.erl", Bin),
    ok.

split_forms(Tokens) ->
    split_forms(Tokens, [], []).
split_forms([], [], Acc) ->
    lists:reverse(Acc);
split_forms([{dot, _} = D | Rest], Cur, Acc) ->
    split_forms(Rest, [], [lists:reverse([D | Cur]) | Acc]);
split_forms([T | Rest], Cur, Acc) ->
    split_forms(Rest, [T | Cur], Acc).

%% Generated callbacks of a Beamtalk actor module are hidden, by name and arity;
%% its methods and blocks are not. A value class's own `init`/`terminate`
%% method (a module with no `handle_call/3`) stays.
visibility_generated_callbacks_hidden_test() ->
    Actor = 'bt@bt4818@counter',
    Value = 'bt@bt4818@point',
    ok = load_module(
        Actor,
        "-module('bt@bt4818@counter'). -export([handle_call/3]). handle_call(_, _, S) -> S."
    ),
    ok = load_module(Value, "-module('bt@bt4818@point'). -export([init/1]). init(S) -> S."),
    try
        Stack = [
            {Actor, 'increment', 1, []},
            {Actor, '-increment/1-fun-0-', 2, []},
            {Actor, safe_dispatch, 3, []},
            {Actor, safe_dispatch, 4, []},
            {Actor, handle_call, 3, []},
            {Actor, '-handle_call/3-fun-0-', 1, []},
            {Actor, handle_info, 2, []},
            {Actor, handle_continue, 2, []},
            {Actor, init, 1, []},
            %% Same name, other arity: a method the user wrote.
            {Actor, init, 2, []},
            {Value, init, 1, []},
            {Value, '-init/1-fun-0-', 0, []}
        ],
        ?assertEqual(
            [
                {Actor, 'increment'},
                {Actor, '-increment/1-fun-0-'},
                {Actor, init},
                {Value, init},
                {Value, '-init/1-fun-0-'}
            ],
            shown(Stack)
        )
    after
        [unload_module(M) || M <- [Actor, Value]]
    end.

%% A malformed entry wraps to a placeholder frame; it is hidden by default but
%% stays in the full list.
visibility_malformed_frame_hidden_test() ->
    ?assertEqual([], shown([not_a_frame])),
    ?assertEqual(1, length(beamtalk_stack_frame:wrap([not_a_frame]))).

%% A frame wrapped alone carries the verdict too (the map shape is the same).
visibility_wrap_frame_stamps_visible_test() ->
    ?assertMatch(#{visible := true}, beamtalk_stack_frame:wrap_frame({lists, map, 2, []})),
    ?assertMatch(
        #{visible := false}, beamtalk_stack_frame:wrap_frame({beamtalk_error, raise, 1, []})
    ).

%% The unfiltered list stays whole and ordered; `wrap/1` only stamps a verdict.
visibility_full_list_is_kept_test() ->
    Stack = [
        {lists, foldl, 3, []},
        {gen_server, handle_msg, 6, []},
        {'bt@bt4818@counter', increment, 1, []}
    ],
    Wrapped = beamtalk_stack_frame:wrap(Stack),
    ?assertEqual([lists, gen_server, 'bt@bt4818@counter'], [M || #{module := M} <- Wrapped]),
    ?assertEqual([true, false, true], [V || #{visible := V} <- Wrapped]),
    ?assertEqual(
        [shown, hidden, shown],
        [V || {V, _} <- beamtalk_stack_frame:classify_frames(Wrapped)]
    ).

%%% ===================================================================
%%% Structured frames carry both forms (BT-4819, ADR 0139 decision 4)
%%% ===================================================================

structured_frame_carries_display_and_raw_mfa_test() ->
    [Frame] = beamtalk_stack_frame:wrap([
        {lists, foldl, 3, [{file, "lists.erl"}, {line, 2471}]}
    ]),
    S = beamtalk_stack_frame:structured_frame(Frame),
    ?assertEqual(<<"lists:foldl/3 (lists.erl:2471)">>, maps:get(<<"display">>, S)),
    ?assertEqual(<<"lists">>, maps:get(<<"module">>, S)),
    ?assertEqual(<<"foldl">>, maps:get(<<"function">>, S)),
    ?assertEqual(3, maps:get(<<"arity">>, S)),
    ?assertEqual(2471, maps:get(<<"line">>, S)),
    ?assertEqual(false, maps:get(<<"hidden">>, S)).

structured_frames_keep_hidden_frames_flagged_test() ->
    Stack = [
        {lists, foldl, 3, [{file, "lists.erl"}, {line, 1}]},
        {gen_server, try_handle_call, 4, [{file, "gen_server.erl"}, {line, 2}]}
    ],
    All = beamtalk_stack_frame:structured_frames(Stack),
    ?assertEqual(2, length(All)),
    [First, Second] = All,
    ?assertEqual(false, maps:get(<<"hidden">>, First)),
    ?assertEqual(true, maps:get(<<"hidden">>, Second)),
    ?assertEqual(
        1, length(beamtalk_stack_frame:visible_frames(beamtalk_stack_frame:wrap(Stack)))
    ).

%% A method named like a JSON literal must not become one on the wire.
structured_frame_function_named_like_json_literal_test() ->
    [Frame] = beamtalk_stack_frame:wrap([{'bt@user@x', true, 1, []}]),
    S = beamtalk_stack_frame:structured_frame(Frame),
    ?assertEqual(<<"true">>, maps:get(<<"function">>, S)),
    Decoded = json:decode(iolist_to_binary(json:encode(S))),
    ?assertEqual(<<"true">>, maps:get(<<"function">>, Decoded)).

structured_frame_unknown_module_is_null_test() ->
    [Frame] = beamtalk_stack_frame:wrap([not_a_frame]),
    S = beamtalk_stack_frame:structured_frame(Frame),
    ?assertEqual(null, maps:get(<<"module">>, S)),
    ?assertEqual(null, maps:get(<<"function">>, S)).

%% The key set is the one in tests/repl-protocol/frame_fields.json, which the
%% Rust `StackFrameInfo` is checked against too.
structured_frame_keys_match_shared_fixture_test() ->
    #{<<"frame_fields">> := Fields} = beamtalk_test_corpus:load_json_fixture([
        "tests", "repl-protocol", "frame_fields.json"
    ]),
    [Frame] = beamtalk_stack_frame:wrap([{lists, foldl, 3, []}]),
    S = beamtalk_stack_frame:structured_frame(Frame),
    ?assertEqual(lists:sort(Fields), lists:sort(maps:keys(S))).

dispatch_raw_mfa_and_is_hidden_test() ->
    [Frame] = beamtalk_stack_frame:wrap([{lists, foldl, 3, []}]),
    ?assertEqual(
        beamtalk_primitive:string_from_binary(<<"lists:foldl/3">>),
        beamtalk_stack_frame:dispatch('rawMfa', [], Frame)
    ),
    ?assertEqual(false, beamtalk_stack_frame:dispatch('isHidden', [], Frame)),
    ?assert(beamtalk_stack_frame:has_method('rawMfa')),
    ?assert(beamtalk_stack_frame:has_method('isHidden')),
    [Bad] = beamtalk_stack_frame:wrap([not_a_frame]),
    ?assertEqual(nil, beamtalk_stack_frame:dispatch('rawMfa', [], Bad)).

%% printString is the display name whatever the frame's flags say.
print_string_independent_of_hidden_flag_test() ->
    [Frame] = beamtalk_stack_frame:wrap([{lists, foldl, 3, []}]),
    Shown = beamtalk_stack_frame:dispatch('printString', [], Frame#{visible => true}),
    Hidden = beamtalk_stack_frame:dispatch('printString', [], Frame#{visible => false}),
    ?assertEqual(Shown, Hidden).
