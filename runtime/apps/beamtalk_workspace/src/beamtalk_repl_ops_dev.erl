%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Op handlers for complete, info, docs, describe, test, and show-codegen operations.
%%%
%%% **DDD Context:** REPL
%%%
%%% Extracted from beamtalk_repl_server (BT-705).

-module(beamtalk_repl_ops_dev).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    handle/4,
    get_completions/1,
    get_symbol_info/1,
    make_class_not_found_error/1,
    base_protocol_response/1
]).

%% @doc Handle complete/info/docs/describe ops.
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(<<"complete">>, Params, Msg, _SessionPid) ->
    Prefix = maps:get(<<"code">>, Params, <<>>),
    Completions = get_completions(Prefix),
    case beamtalk_repl_protocol:is_legacy(Msg) of
        true ->
            jsx:encode(#{
                <<"type">> => <<"completions">>,
                <<"completions">> => Completions
            });
        false ->
            Base = base_protocol_response(Msg),
            jsx:encode(Base#{<<"completions">> => Completions, <<"status">> => [<<"done">>]})
    end;
handle(<<"info">>, Params, Msg, _SessionPid) ->
    Symbol = maps:get(<<"symbol">>, Params, <<>>),
    Info = get_symbol_info(Symbol),
    case beamtalk_repl_protocol:is_legacy(Msg) of
        true ->
            jsx:encode(#{<<"type">> => <<"info">>, <<"info">> => Info});
        false ->
            Base = base_protocol_response(Msg),
            jsx:encode(Base#{<<"info">> => Info, <<"status">> => [<<"done">>]})
    end;
handle(<<"docs">>, Params, Msg, _SessionPid) ->
    ClassBin = maps:get(<<"class">>, Params, <<>>),
    case beamtalk_repl_server:safe_to_existing_atom(ClassBin) of
        {error, badarg} ->
            beamtalk_repl_protocol:encode_error(
                make_class_not_found_error(ClassBin),
                Msg,
                fun beamtalk_repl_json:format_error_message/1
            );
        {ok, ClassName} ->
            Selector = maps:get(<<"selector">>, Params, undefined),
            case Selector of
                undefined ->
                    case beamtalk_repl_docs:format_class_docs(ClassName) of
                        {ok, DocText} ->
                            beamtalk_repl_protocol:encode_docs(DocText, Msg);
                        {error, {class_not_found, _}} ->
                            beamtalk_repl_protocol:encode_error(
                                make_class_not_found_error(ClassName),
                                Msg,
                                fun beamtalk_repl_json:format_error_message/1
                            )
                    end;
                SelectorBin ->
                    case beamtalk_repl_docs:format_method_doc(ClassName, SelectorBin) of
                        {ok, DocText} ->
                            beamtalk_repl_protocol:encode_docs(DocText, Msg);
                        {error, {class_not_found, _}} ->
                            beamtalk_repl_protocol:encode_error(
                                make_class_not_found_error(ClassName),
                                Msg,
                                fun beamtalk_repl_json:format_error_message/1
                            );
                        {error, {method_not_found, _, _}} ->
                            NameBin = to_binary(ClassName),
                            SelectorAtom = binary_to_atom(SelectorBin, utf8),
                            Err0 = beamtalk_error:new(does_not_understand, ClassName),
                            Err1 = beamtalk_error:with_selector(Err0, SelectorAtom),
                            Err2 = beamtalk_error:with_message(
                                Err1,
                                iolist_to_binary([NameBin, <<" does not understand ">>, SelectorBin])
                            ),
                            Err3 = beamtalk_error:with_hint(
                                Err2,
                                iolist_to_binary([
                                    <<"Use :help ">>, NameBin, <<" to see available methods.">>
                                ])
                            ),
                            beamtalk_repl_protocol:encode_error(
                                Err3, Msg, fun beamtalk_repl_json:format_error_message/1
                            )
                    end
            end
    end;
handle(<<"test">>, Params, Msg, _SessionPid) ->
    %% Run BUnit tests for a TestCase class (BT-699).
    ClassBin = maps:get(<<"class">>, Params, <<>>),
    Method = maps:get(<<"method">>, Params, undefined),
    case ClassBin of
        <<>> ->
            Err0 = beamtalk_error:new(missing_parameter, 'REPL'),
            Err1 = beamtalk_error:with_message(Err0, <<"Missing required parameter: class">>),
            Err2 = beamtalk_error:with_hint(
                Err1,
                <<"Provide a TestCase class name, e.g. {\"op\": \"test\", \"class\": \"CounterTest\"}">>
            ),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1
            );
        _ when not is_binary(ClassBin) ->
            Err0 = beamtalk_error:new(type_error, 'REPL'),
            Err1 = beamtalk_error:with_message(Err0, <<"Parameter 'class' must be a string">>),
            Err2 = beamtalk_error:with_hint(Err1, <<"Provide a TestCase class name.">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1
            );
        _ ->
            case beamtalk_repl_server:safe_to_existing_atom(ClassBin) of
                {error, badarg} ->
                    beamtalk_repl_protocol:encode_error(
                        make_class_not_found_error(ClassBin),
                        Msg,
                        fun beamtalk_repl_json:format_error_message/1
                    );
                {ok, ClassAtom} ->
                    try
                        Results =
                            case Method of
                                undefined ->
                                    beamtalk_test_case:run_all_structured(ClassAtom);
                                MethodBin when is_binary(MethodBin) ->
                                    case beamtalk_repl_server:safe_to_existing_atom(MethodBin) of
                                        {error, badarg} ->
                                            Err3 = beamtalk_error:new(
                                                does_not_understand, ClassAtom
                                            ),
                                            Err4 = beamtalk_error:with_message(
                                                Err3,
                                                iolist_to_binary([
                                                    <<"Method '">>,
                                                    MethodBin,
                                                    <<"' not found in ">>,
                                                    ClassBin
                                                ])
                                            ),
                                            error(Err4);
                                        {ok, MethodAtom} ->
                                            beamtalk_test_case:run_single_structured(
                                                ClassAtom, MethodAtom
                                            )
                                    end;
                                _Other ->
                                    Err3 = beamtalk_error:new(type_error, 'REPL'),
                                    Err4 = beamtalk_error:with_message(
                                        Err3, <<"Parameter 'method' must be a string">>
                                    ),
                                    error(Err4)
                            end,
                        beamtalk_repl_protocol:encode_test_results(Results, Msg)
                    catch
                        error:Reason ->
                            WrappedReason = beamtalk_repl_server:ensure_structured_error(Reason),
                            beamtalk_repl_protocol:encode_error(
                                WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1
                            )
                    end
            end
    end;
handle(<<"test-all">>, _Params, Msg, _SessionPid) ->
    %% Run all loaded TestCase subclasses (BT-699).
    TestClasses = beamtalk_test_case:find_test_classes(),
    case TestClasses of
        [] ->
            EmptyResults = #{
                class => 'All',
                total => 0,
                passed => 0,
                failed => 0,
                duration => 0.0,
                tests => []
            },
            beamtalk_repl_protocol:encode_test_results(EmptyResults, Msg);
        _ ->
            try
                StartTime = erlang:monotonic_time(millisecond),
                AllResults = lists:map(
                    fun(ClassName) ->
                        beamtalk_test_case:run_all_structured(ClassName)
                    end,
                    TestClasses
                ),
                EndTime = erlang:monotonic_time(millisecond),
                Duration = (EndTime - StartTime) / 1000.0,
                {TotalTests, TotalPassed, TotalFailed, AllTestDetails} =
                    lists:foldl(
                        fun(
                            #{
                                total := T,
                                passed := P,
                                failed := F,
                                tests := Tests,
                                class := C
                            },
                            {AT, AP, AF, ATests}
                        ) ->
                            TaggedTests = [
                                Test#{class_name => C}
                             || Test <- Tests
                            ],
                            {AT + T, AP + P, AF + F, ATests ++ TaggedTests}
                        end,
                        {0, 0, 0, []},
                        AllResults
                    ),
                MergedResults = #{
                    class => 'All',
                    total => TotalTests,
                    passed => TotalPassed,
                    failed => TotalFailed,
                    duration => Duration,
                    tests => AllTestDetails
                },
                beamtalk_repl_protocol:encode_test_results(MergedResults, Msg)
            catch
                error:Reason ->
                    WrappedReason = beamtalk_repl_server:ensure_structured_error(Reason),
                    beamtalk_repl_protocol:encode_error(
                        WrappedReason, Msg, fun beamtalk_repl_json:format_error_message/1
                    )
            end
    end;
handle(<<"show-codegen">>, Params, Msg, SessionPid) ->
    %% BT-700: Compile expression and return Core Erlang source without evaluating.
    Code = binary_to_list(maps:get(<<"code">>, Params, <<>>)),
    case Code of
        [] ->
            Err = beamtalk_error:new(empty_expression, 'REPL'),
            Err1 = beamtalk_error:with_message(Err, <<"Empty expression">>),
            Err2 = beamtalk_error:with_hint(Err1, <<"Enter an expression to compile.">>),
            beamtalk_repl_protocol:encode_error(
                Err2, Msg, fun beamtalk_repl_json:format_error_message/1
            );
        _ ->
            case beamtalk_repl_shell:show_codegen(SessionPid, Code) of
                {ok, CoreErlang, Warnings} ->
                    Base = beamtalk_repl_protocol:base_response(Msg),
                    Result = Base#{
                        <<"core_erlang">> => CoreErlang,
                        <<"status">> => [<<"done">>]
                    },
                    Result1 =
                        case Warnings of
                            [] -> Result;
                            _ -> Result#{<<"warnings">> => Warnings}
                        end,
                    jsx:encode(Result1);
                {error, ErrorReason, Warnings} ->
                    WrappedReason = beamtalk_repl_server:ensure_structured_error(ErrorReason),
                    beamtalk_repl_protocol:encode_error(
                        WrappedReason,
                        Msg,
                        fun beamtalk_repl_json:format_error_message/1,
                        <<>>,
                        Warnings
                    )
            end
    end;
handle(<<"describe">>, _Params, Msg, _SessionPid) ->
    Ops = describe_ops(),
    BeamtalkVsnBin =
        case application:get_key(beamtalk_workspace, vsn) of
            {ok, Vsn} when is_list(Vsn) -> list_to_binary(Vsn);
            {ok, Vsn} when is_binary(Vsn) -> Vsn;
            _ -> <<"0.1.0">>
        end,
    Versions = #{
        <<"protocol">> => <<"1.0">>,
        <<"beamtalk">> => BeamtalkVsnBin
    },
    beamtalk_repl_protocol:encode_describe(Ops, Versions, Msg).

%%% Internal helpers

%% @private
-spec base_protocol_response(term()) -> map().
base_protocol_response(Msg) ->
    Id = beamtalk_repl_protocol:get_id(Msg),
    Session = beamtalk_repl_protocol:get_session(Msg),
    M0 = #{},
    M1 =
        case Id of
            undefined -> M0;
            _ -> M0#{<<"id">> => Id}
        end,
    case Session of
        undefined -> M1;
        _ -> M1#{<<"session">> => Session}
    end.

%% @private
-spec get_completions(binary()) -> [binary()].
get_completions(<<>>) ->
    [];
get_completions(Prefix) when is_binary(Prefix) ->
    PrefixStr = binary_to_list(Prefix),
    ClassPids =
        try
            beamtalk_class_registry:all_classes()
        catch
            _:_ -> []
        end,
    ClassNames = lists:filtermap(
        fun(Pid) ->
            try
                Name = beamtalk_object_class:class_name(Pid),
                {true, atom_to_binary(Name, utf8)}
            catch
                _:_ -> false
            end
        end,
        ClassPids
    ),
    [Name || Name <- ClassNames, binary:match(Name, Prefix) =:= {0, byte_size(Prefix)}] ++
        [
            atom_to_binary(B, utf8)
         || B <-
                try
                    beamtalk_workspace_config:binding_names()
                catch
                    _:_ -> []
                end,
            binary:match(atom_to_binary(B, utf8), Prefix) =:= {0, byte_size(Prefix)}
        ] ++
        [
            Kw
         || Kw <- builtin_keywords(),
            binary:match(Kw, Prefix) =:= {0, byte_size(Prefix)},
            PrefixStr =/= ""
        ].

%% @private
-spec builtin_keywords() -> [binary()].
builtin_keywords() ->
    [
        <<"self">>,
        <<"super">>,
        <<"true">>,
        <<"false">>,
        <<"nil">>,
        <<"ifTrue:">>,
        <<"ifFalse:">>,
        <<"ifTrue:ifFalse:">>,
        <<"whileTrue:">>,
        <<"timesRepeat:">>,
        <<"subclass:">>,
        <<"spawn">>,
        <<"new">>
    ].

%% @private
%% @doc Look up information about a symbol, returning enriched metadata for classes.
%% Returns superclass, superclass chain, methods, source, and doc comments
%% when the symbol is a known class.
-spec get_symbol_info(binary()) -> map().
get_symbol_info(Symbol) when is_binary(Symbol) ->
    SymAtom =
        try
            binary_to_existing_atom(Symbol, utf8)
        catch
            _:_ -> undefined
        end,
    case SymAtom of
        undefined ->
            #{
                <<"found">> => false,
                <<"symbol">> => Symbol
            };
        _ ->
            %% Look up via class registry (O(1) instead of scanning all PIDs)
            case
                try
                    beamtalk_class_registry:whereis_class(SymAtom)
                catch
                    _:_ -> undefined
                end
            of
                undefined ->
                    #{
                        <<"found">> => false,
                        <<"symbol">> => Symbol
                    };
                ClassPid ->
                    enrich_class_info(Symbol, SymAtom, ClassPid)
            end
    end.

%% @private
%% @doc Build enriched info map for a known class.
-spec enrich_class_info(binary(), atom(), pid()) -> map().
enrich_class_info(Symbol, _ClassName, ClassPid) ->
    Base = #{
        <<"found">> => true,
        <<"symbol">> => Symbol,
        <<"kind">> => <<"class">>
    },
    try
        Superclass = beamtalk_object_class:superclass(ClassPid),
        ModuleName = beamtalk_object_class:module_name(ClassPid),
        Methods = beamtalk_object_class:methods(ClassPid),

        %% All method selectors (own + inherited) as sorted binaries
        Selectors = lists:sort(
            [atom_to_binary(S, utf8) || S <- Methods]
        ),

        %% Superclass chain
        Chain = build_superclass_chain(Superclass),

        %% Source location from BEAM compile info
        {Source, Line} = get_source_location(ModuleName),

        %% Doc from runtime-embedded documentation (ADR 0033)
        Doc =
            try
                gen_server:call(ClassPid, get_doc, 5000)
            catch
                _:_ -> none
            end,

        %% Build result with optional fields
        Result0 = Base#{
            <<"superclass">> => format_class_name(Superclass),
            <<"methods">> => Selectors,
            <<"superclass_chain">> => Chain
        },
        Result1 = maybe_add(<<"source">>, Source, Result0),
        Result2 = maybe_add(<<"line">>, Line, Result1),
        maybe_add(<<"doc">>, Doc, Result2)
    catch
        _:_ -> Base
    end.

%% @private
%% @doc Walk the class hierarchy ETS table to build the superclass chain.
-spec build_superclass_chain(atom() | none) -> [binary()].
build_superclass_chain(none) -> [];
build_superclass_chain(StartClass) -> build_superclass_chain(StartClass, [], #{}).

build_superclass_chain(none, Acc, _Seen) ->
    lists:reverse(Acc);
build_superclass_chain(ClassName, Acc, Seen) ->
    case maps:is_key(ClassName, Seen) of
        true ->
            %% Cycle detected — stop to prevent infinite loop
            lists:reverse(Acc);
        false ->
            case ets:info(beamtalk_class_hierarchy) of
                undefined ->
                    lists:reverse([atom_to_binary(ClassName, utf8) | Acc]);
                _ ->
                    case ets:lookup(beamtalk_class_hierarchy, ClassName) of
                        [{_, SuperName}] ->
                            build_superclass_chain(
                                SuperName,
                                [atom_to_binary(ClassName, utf8) | Acc],
                                Seen#{ClassName => true}
                            );
                        [] ->
                            lists:reverse([atom_to_binary(ClassName, utf8) | Acc])
                    end
            end
    end.

%% @private
%% @doc Get source file and line from BEAM module compile info.
-spec get_source_location(atom()) -> {binary() | none, non_neg_integer() | none}.
get_source_location(ModuleName) ->
    try ModuleName:module_info(compile) of
        CompileInfo ->
            Source =
                case proplists:get_value(source, CompileInfo) of
                    undefined -> none;
                    Path when is_list(Path) -> list_to_binary(Path);
                    Path when is_binary(Path) -> Path
                end,
            {Source, none}
    catch
        _:_ -> {none, none}
    end.

%% @private
-spec format_class_name(atom() | none) -> binary() | null.
format_class_name(none) -> null;
format_class_name(Name) -> atom_to_binary(Name, utf8).

%% @private
%% @doc Add a key to a map only if the value is not `none`.
-spec maybe_add(binary(), term(), map()) -> map().
maybe_add(_Key, none, Map) -> Map;
maybe_add(Key, Value, Map) -> Map#{Key => Value}.

%% @private
-spec describe_ops() -> map().
describe_ops() ->
    #{
        <<"eval">> => #{<<"params">> => [<<"code">>]},
        <<"stdin">> => #{<<"params">> => [<<"value">>]},
        <<"complete">> => #{<<"params">> => [<<"code">>]},
        <<"info">> => #{<<"params">> => [<<"symbol">>]},
        <<"docs">> => #{
            <<"params">> => [<<"class">>],
            <<"optional">> => [<<"selector">>]
        },
        <<"load-file">> => #{<<"params">> => [<<"path">>]},
        <<"load-source">> => #{<<"params">> => [<<"source">>]},
        <<"reload">> => #{
            <<"params">> => [],
            <<"optional">> => [<<"module">>, <<"path">>]
        },
        <<"clear">> => #{<<"params">> => []},
        <<"bindings">> => #{<<"params">> => []},
        <<"sessions">> => #{<<"params">> => []},
        <<"clone">> => #{<<"params">> => []},
        <<"close">> => #{<<"params">> => []},
        <<"interrupt">> => #{<<"params">> => []},
        <<"modules">> => #{<<"params">> => []},
        <<"unload">> => #{<<"params">> => [<<"module">>]},
        <<"health">> => #{<<"params">> => []},
        <<"show-codegen">> => #{<<"params">> => [<<"code">>]},
        <<"describe">> => #{<<"params">> => []},
        <<"shutdown">> => #{<<"params">> => [<<"cookie">>]},
        <<"test">> => #{
            <<"params">> => [<<"class">>],
            <<"optional">> => [<<"method">>]
        },
        <<"test-all">> => #{<<"params">> => []}
    }.

%% @private
-spec make_class_not_found_error(atom() | binary()) -> #beamtalk_error{}.
make_class_not_found_error(ClassName) ->
    NameBin = to_binary(ClassName),
    Err0 = beamtalk_error:new(class_not_found, 'REPL'),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary([<<"Unknown class: ">>, NameBin])
    ),
    beamtalk_error:with_hint(
        Err1,
        <<"Use :modules to see loaded classes.">>
    ).

%% @private
-spec to_binary(atom() | binary()) -> binary().
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) -> B.
