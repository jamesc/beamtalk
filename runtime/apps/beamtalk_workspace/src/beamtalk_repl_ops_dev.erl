%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Op handlers for complete, docs, describe, and show-codegen operations.
%%%
%%% **DDD Context:** REPL Session Context
%%%
%%% Extracted from beamtalk_repl_server (BT-705).

-module(beamtalk_repl_ops_dev).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    handle/4,
    get_completions/1,
    get_context_completions/1,
    get_context_completions/2,
    parse_receiver_and_prefix/1,
    tokenise_send_chain/1,
    tokenise_binary_chain/1,
    resolve_chain_type/2,
    walk_chain/2,
    walk_chain_class/2,
    walk_mixed_chain/2,
    walk_mixed_chain_class/2,
    make_class_not_found_error/1,
    base_protocol_response/1,
    list_class_methods_for_ws/1
]).

-include_lib("kernel/include/logger.hrl").

%% BT-1045: Export internals for white-box testing of the binding-lookup pipeline.
-ifdef(TEST).
-export([
    get_session_bindings/1
]).
-endif.

%% Methods inherited from Object that are internal implementation protocol and
%% should not appear in user-facing completions. Long-term, reflection methods
%% (fieldNames, fieldAt:, fieldAt:put:) should move to Mirror classes (BT-1049).
-define(COMPLETION_HIDDEN_METHODS, [
    %% Abstract-class stubs — only meaningful inside a class body
    subclassResponsibility,
    notImplemented,
    %% Message-not-understood handler — internal dispatch protocol
    'doesNotUnderstand:args:',
    %% Reflection intrinsics — long-term these should move to a Mirror class
    fieldNames,
    'fieldAt:',
    'fieldAt:put:',
    %% Object constructors — valid on class objects but confusing on instances
    %% ("foo" new dispatches via basicNew which most classes don't support directly)
    new,
    'new:',
    %% Dynamic dispatch — advanced/meta protocol, rarely called directly
    'perform:',
    'perform:withArguments:'
]).

%% @doc Handle complete/docs/describe ops.
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(<<"complete">>, Params, Msg, SessionPid) ->
    Code = maps:get(<<"code">>, Params, <<>>),
    %% BT-783: New protocol includes "cursor" field — Code is the full line up to cursor.
    %% Old protocol omits "cursor" — Code is a bare prefix (backward compat).
    Completions =
        case maps:is_key(<<"cursor">>, Params) of
            true ->
                %% Completions run on a separate WebSocket session with no user bindings.
                %% If the client passes its main session ID, resolve bindings from that session
                %% so instance-method completions work for bound actor variables.
                %% BT-1045: session is decoded into Msg by the protocol layer and stripped
                %% from Params — use get_session(Msg), NOT maps:get(<<"session">>, Params).
                BindingPid = beamtalk_session_table:resolve_pid(
                    beamtalk_repl_protocol:get_session(Msg), SessionPid
                ),
                SessionBindings = get_session_bindings(BindingPid),
                WorkspaceBindings = get_workspace_bindings(),
                %% Session bindings take priority over workspace globals (e.g. Transcript)
                Bindings = maps:merge(WorkspaceBindings, SessionBindings),
                get_context_completions(Code, Bindings);
            false ->
                get_completions(Code)
        end,
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
handle(<<"docs">>, Params, Msg, _SessionPid) ->
    ClassBin = maps:get(<<"class">>, Params, <<>>),
    ClassSide = maps:get(<<"class_side">>, Params, false),
    case beamtalk_repl_errors:safe_to_existing_atom(ClassBin) of
        {error, badarg} ->
            beamtalk_repl_protocol:encode_error(
                make_class_not_found_error(ClassBin),
                Msg,
                fun beamtalk_repl_json:format_error_message/1
            );
        {ok, ClassName} ->
            Selector = maps:get(<<"selector">>, Params, undefined),
            Result =
                case {Selector, ClassSide} of
                    {undefined, false} ->
                        beamtalk_repl_docs:format_class_docs(ClassName);
                    {undefined, true} ->
                        beamtalk_repl_docs:format_class_docs_class_side(ClassName);
                    {SelectorBin, false} ->
                        beamtalk_repl_docs:format_method_doc(ClassName, SelectorBin);
                    {SelectorBin, true} ->
                        beamtalk_repl_docs:format_method_doc_class_side(ClassName, SelectorBin)
                end,
            case Result of
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
                    SelectorBin2 = maps:get(<<"selector">>, Params, <<"?">>),
                    SelectorAtom = binary_to_atom(SelectorBin2, utf8),
                    HintClass =
                        case ClassSide of
                            true ->
                                iolist_to_binary([NameBin, <<" class">>]);
                            false ->
                                NameBin
                        end,
                    Err0 = beamtalk_error:new(does_not_understand, ClassName),
                    Err1 = beamtalk_error:with_selector(Err0, SelectorAtom),
                    Err2 = beamtalk_error:with_message(
                        Err1,
                        iolist_to_binary([HintClass, <<" does not understand ">>, SelectorBin2])
                    ),
                    Err3 = beamtalk_error:with_hint(
                        Err2,
                        iolist_to_binary([
                            <<"Use :help ">>, HintClass, <<" to see available methods.">>
                        ])
                    ),
                    beamtalk_repl_protocol:encode_error(
                        Err3, Msg, fun beamtalk_repl_json:format_error_message/1
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
                    WrappedReason = beamtalk_repl_errors:ensure_structured_error(ErrorReason),
                    beamtalk_repl_protocol:encode_error(
                        WrappedReason,
                        Msg,
                        fun beamtalk_repl_json:format_error_message/1,
                        <<>>,
                        Warnings
                    )
            end
    end;
handle(<<"methods">>, Params, Msg, _SessionPid) ->
    %% BT-1026: Return instance and class-side methods for a loaded class.
    ClassBin = maps:get(<<"class">>, Params, <<>>),
    Methods = list_class_methods_for_ws(ClassBin),
    StateVars = list_state_vars_for_ws(ClassBin),
    Base = base_protocol_response(Msg),
    jsx:encode(Base#{
        <<"methods">> => Methods, <<"state_vars">> => StateVars, <<"status">> => [<<"done">>]
    });
handle(<<"test">>, Params, Msg, _SessionPid) ->
    ClassName = maps:get(<<"class">>, Params, undefined),
    FilePath = maps:get(<<"file">>, Params, undefined),
    case {ClassName, FilePath} of
        {CN, FP} when CN =/= undefined, FP =/= undefined ->
            Err0 = beamtalk_error:new(invalid_argument, 'TestRunner'),
            Err1 = beamtalk_error:with_message(
                Err0, <<"'class' and 'file' are mutually exclusive">>
            ),
            beamtalk_repl_protocol:encode_error(
                Err1, Msg, fun beamtalk_repl_json:format_error_message/1
            );
        {undefined, FP} when FP =/= undefined, not is_binary(FP) ->
            Err0 = beamtalk_error:new(invalid_argument, 'TestRunner'),
            Err1 = beamtalk_error:with_message(
                Err0, <<"'file' must be a binary path">>
            ),
            beamtalk_repl_protocol:encode_error(
                Err1, Msg, fun beamtalk_repl_json:format_error_message/1
            );
        {undefined, FP} when FP =/= undefined ->
            run_test_op_file(FP, Msg);
        _ ->
            run_test_op(ClassName, Msg)
    end;
handle(<<"test-all">>, _Params, Msg, _SessionPid) ->
    run_test_op(undefined, Msg);
handle(<<"describe">>, _Params, Msg, _SessionPid) ->
    Ops = describe_ops(),
    BeamtalkVsnBin =
        case application:get_key(beamtalk_workspace, vsn) of
            {ok, Vsn} when is_list(Vsn) -> list_to_binary(Vsn);
            {ok, Vsn} when is_binary(Vsn) -> Vsn;
            _ -> <<"unknown">>
        end,
    Versions = #{
        <<"protocol">> => <<"1.0">>,
        <<"beamtalk">> => BeamtalkVsnBin
    },
    beamtalk_repl_protocol:encode_describe(Ops, Versions, Msg).

%%% Test op helpers

%% @private
%% @doc Execute a test run and encode the result.
%%
%% When ClassName is undefined, runs all discovered TestCase subclasses.
%% When ClassName is a binary, runs tests for that specific class.
-spec run_test_op(binary() | undefined, beamtalk_repl_protocol:protocol_msg()) -> binary().
run_test_op(undefined, Msg) ->
    try
        TestResult = beamtalk_test_runner:run_all(),
        beamtalk_repl_protocol:encode_test_results(TestResult, Msg)
    catch
        error:#{error := #beamtalk_error{} = Err} ->
            beamtalk_repl_protocol:encode_error(
                Err, Msg, fun beamtalk_repl_json:format_error_message/1
            );
        _Class:Reason ->
            ?LOG_ERROR("test-all op failed: ~p", [Reason]),
            Err0 = beamtalk_error:new(runtime_error, 'TestRunner'),
            Err1 = beamtalk_error:with_message(
                Err0, iolist_to_binary(io_lib:format("Test run failed: ~p", [Reason]))
            ),
            beamtalk_repl_protocol:encode_error(
                Err1, Msg, fun beamtalk_repl_json:format_error_message/1
            )
    end;
run_test_op(ClassName, Msg) when is_binary(ClassName) ->
    case beamtalk_repl_errors:safe_to_existing_atom(ClassName) of
        {error, badarg} ->
            beamtalk_repl_protocol:encode_error(
                make_class_not_found_error(ClassName),
                Msg,
                fun beamtalk_repl_json:format_error_message/1
            );
        {ok, ClassAtom} ->
            try
                TestResult = beamtalk_test_runner:run_class_by_name(ClassAtom),
                beamtalk_repl_protocol:encode_test_results(TestResult, Msg)
            catch
                error:#{error := #beamtalk_error{} = Err} ->
                    beamtalk_repl_protocol:encode_error(
                        Err, Msg, fun beamtalk_repl_json:format_error_message/1
                    );
                _Class:Reason ->
                    ?LOG_ERROR("test op failed for ~s: ~p", [ClassName, Reason]),
                    Err0 = beamtalk_error:new(runtime_error, 'TestRunner'),
                    Err1 = beamtalk_error:with_message(
                        Err0,
                        iolist_to_binary(
                            io_lib:format("Test run failed for ~s: ~p", [ClassName, Reason])
                        )
                    ),
                    beamtalk_repl_protocol:encode_error(
                        Err1, Msg, fun beamtalk_repl_json:format_error_message/1
                    )
            end
    end.

%% @private
%% @doc Execute a file-scoped test run and encode the result.
%%
%% Discovers all TestCase subclasses whose beamtalk_source attribute matches
%% FilePath (by path suffix) and runs them. Returns an aggregated TestResult.
-spec run_test_op_file(binary(), beamtalk_repl_protocol:protocol_msg()) -> binary().
run_test_op_file(FilePath, Msg) ->
    try
        TestResult = beamtalk_test_runner:run_file(FilePath),
        beamtalk_repl_protocol:encode_test_results(TestResult, Msg)
    catch
        error:#{error := #beamtalk_error{} = Err} ->
            beamtalk_repl_protocol:encode_error(
                Err, Msg, fun beamtalk_repl_json:format_error_message/1
            );
        _Class:Reason ->
            ?LOG_ERROR("test file op failed for ~s: ~p", [FilePath, Reason]),
            Err0 = beamtalk_error:new(runtime_error, 'TestRunner'),
            Err1 = beamtalk_error:with_message(
                Err0,
                iolist_to_binary(
                    io_lib:format("Test run failed for file ~s: ~p", [FilePath, Reason])
                )
            ),
            beamtalk_repl_protocol:encode_error(
                Err1, Msg, fun beamtalk_repl_json:format_error_message/1
            )
    end.

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
            beamtalk_runtime_api:all_classes()
        catch
            _:_ -> []
        end,
    ClassNames = lists:filtermap(
        fun(Pid) ->
            try
                Name = beamtalk_runtime_api:class_name(Pid),
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
%% @doc Context-aware completion: parses the line to find a receiver and returns
%% matching method selectors (BT-783).  Falls back to get_completions/1 when
%% no receiver is detected.  Wrapper with no binding context.
-spec get_context_completions(binary()) -> [binary()].
get_context_completions(Line) ->
    get_context_completions(Line, #{}).

%% @private
%% @doc Context-aware completion with session bindings for variable lookup.
-spec get_context_completions(binary(), map()) -> [binary()].
get_context_completions(<<>>, _Bindings) ->
    [];
get_context_completions(Line, Bindings) when is_binary(Line) ->
    case parse_receiver_and_prefix(Line) of
        {undefined, Prefix} ->
            %% No receiver — use standard prefix completion
            get_completions(Prefix);
        {expression, ReceiverExpr, Prefix} ->
            %% Multi-token receiver expression — resolve via chain type inference (BT-1006)
            case resolve_chain_type(ReceiverExpr, Bindings) of
                {ok, ClassName} -> complete_instance_methods(ClassName, Prefix);
                undefined -> []
            end;
        {Receiver, <<>>} ->
            %% Empty prefix with receiver (e.g., "Integer ") — return all methods
            MethodSelectors = get_methods_for_receiver(Receiver, Bindings),
            lists:usort([atom_to_binary(S, utf8) || S <- MethodSelectors]);
        {Receiver, Prefix} ->
            %% Receiver with prefix — look up methods filtered by prefix
            MethodSelectors = get_methods_for_receiver(Receiver, Bindings),
            lists:usort([
                atom_to_binary(S, utf8)
             || S <- MethodSelectors,
                binary:match(atom_to_binary(S, utf8), Prefix) =:= {0, byte_size(Prefix)}
            ])
    end.

%% @private
%% @doc Parse the line up to the cursor into a receiver and prefix.
%%
%% Returns:
%%   {undefined, Prefix}                — no receiver (bare prefix, e.g. <<"s">>)
%%   {ReceiverToken, Prefix}            — single-token receiver (e.g. <<"Integer">>, <<"42">>)
%%   {expression, ReceiverExpr, Prefix} — multi-token receiver expression (e.g. <<"\"hello\" size">>) (BT-1006)
%%
%% Examples:
%%   <<"Integer s">>       → {<<"Integer">>, <<"s">>}
%%   <<"Integer ">>        → {<<"Integer">>, <<>>}
%%   <<"42 s">>            → {<<"42">>, <<"s">>}
%%   <<"\"hello\" size c">>→ {expression, <<"\"hello\" size">>, <<"c">>}
%%   <<"s">>               → {undefined, <<"s">>}
%%   <<>>                  → {undefined, <<>>}
-spec parse_receiver_and_prefix(binary()) ->
    {binary() | undefined, binary()} | {expression, binary(), binary()}.
parse_receiver_and_prefix(<<>>) ->
    {undefined, <<>>};
parse_receiver_and_prefix(Line) when is_binary(Line) ->
    Str = binary_to_list(Line),
    RevStr = lists:reverse(Str),
    %% Extract trailing identifier characters (the completion prefix)
    {PrefixCharsRev, Rest} = lists:splitwith(fun is_identifier_char/1, RevStr),
    Prefix = list_to_binary(lists:reverse(PrefixCharsRev)),
    %% Skip whitespace before the prefix
    {SpaceChars, ReceiverPartRev} = lists:splitwith(
        fun(C) -> C =:= $\s orelse C =:= $\t end, Rest
    ),
    case SpaceChars of
        [] ->
            %% No space before prefix — single token, no receiver
            {undefined, Prefix};
        _ ->
            %% Extract the token immediately before the space (the receiver)
            {ReceiverCharsRev, Tail} = lists:splitwith(
                fun(C) -> C =/= $\s andalso C =/= $\t end, ReceiverPartRev
            ),
            case ReceiverCharsRev of
                [] ->
                    {undefined, Prefix};
                _ ->
                    ReceiverToken = list_to_binary(lists:reverse(ReceiverCharsRev)),
                    %% Check if Tail contains non-whitespace — only then is it multi-token.
                    %% Leading indentation (e.g. "  Integer s") must not trigger the
                    %% expression path; Tail would be all-whitespace in that case.
                    case
                        lists:any(
                            fun(C) -> C =/= $\s andalso C =/= $\t end, Tail
                        )
                    of
                        false ->
                            %% Single-token receiver (possibly with leading whitespace)
                            {ReceiverToken, Prefix};
                        true ->
                            %% Multi-token receiver expression (BT-1006)
                            ReceiverExpr = list_to_binary(lists:reverse(ReceiverPartRev)),
                            {expression, ReceiverExpr, Prefix}
                    end
            end
    end.

%% @private
-spec is_identifier_char(char()) -> boolean().
is_identifier_char(C) ->
    (C >= $a andalso C =< $z) orelse
        (C >= $A andalso C =< $Z) orelse
        (C >= $0 andalso C =< $9) orelse
        C =:= $_ orelse
        %% Colons are identifier chars so keyword selectors like `ifTrue:` and
        %% `ifTrue:ifFalse:` complete as a unit.  Must stay in sync with
        %% word_start in crates/beamtalk-cli/src/commands/repl/helper.rs.
        C =:= $:.

%%% Chain resolution (BT-1006)

%% @private
%% @doc Parse a binary expression into a receiver token and a list of unary selectors.
%%
%% Accepts only simple unary send chains: whitespace-separated tokens where the first
%% is a receiver (literal, class name, or variable) and the rest are unary selectors.
%% Returns `error` for keyword sends mid-chain, parenthesised subexpressions, or `>>`.
%%
%% Examples:
%%   <<"\"hello\" size">>    → {ok, <<"\"hello\"">>, [size]}
%%   <<"counter getValue">>  → {ok, <<"counter">>, [getValue]}
%%   <<"\"hello\" size abs">>→ {ok, <<"\"hello\"">>, [size, abs]}
%%   <<"inject: 0 into:">>   → error  (keyword send)
%%   <<"(myList size)">>     → error  (paren)
-spec tokenise_send_chain(binary()) -> {ok, binary(), [atom()]} | error.
tokenise_send_chain(<<>>) ->
    error;
tokenise_send_chain(Expr) when is_binary(Expr) ->
    Parts = [list_to_binary(T) || T <- string:tokens(binary_to_list(Expr), " \t")],
    case Parts of
        [] ->
            error;
        [_] ->
            %% Single token — no sends, nothing to chain-walk
            error;
        [ReceiverToken | Selectors] ->
            case validate_chain_tokens(ReceiverToken, Selectors) of
                ok ->
                    %% Use binary_to_existing_atom to avoid atom table exhaustion from
                    %% user input.  Valid selectors are already loaded as method atoms;
                    %% unknown selectors mean the chain will break anyway.
                    try
                        SelectorAtoms = [binary_to_existing_atom(S, utf8) || S <- Selectors],
                        {ok, ReceiverToken, SelectorAtoms}
                    catch
                        error:badarg -> error
                    end;
                error ->
                    error
            end
    end.

%% @private
-spec validate_chain_tokens(binary(), [binary()]) -> ok | error.
validate_chain_tokens(ReceiverToken, Selectors) ->
    AllTokens = [ReceiverToken | Selectors],
    HasInvalid = lists:any(
        fun(T) ->
            binary:match(T, <<"(">>) =/= nomatch orelse
                binary:match(T, <<")">>) =/= nomatch orelse
                binary:match(T, <<">>">>) =/= nomatch
        end,
        AllTokens
    ),
    case HasInvalid of
        true ->
            error;
        false ->
            case lists:all(fun is_valid_unary_selector/1, Selectors) of
                true -> ok;
                false -> error
            end
    end.

%% @private
%% @doc A valid unary selector starts with a letter or underscore and contains no colon.
-spec is_valid_unary_selector(binary()) -> boolean().
is_valid_unary_selector(<<>>) ->
    false;
is_valid_unary_selector(<<H, _/binary>> = Sel) when
    (H >= $a andalso H =< $z) orelse (H >= $A andalso H =< $Z) orelse H =:= $_
->
    binary:match(Sel, <<":">>) =:= nomatch;
is_valid_unary_selector(_) ->
    false.

%% @private
%% @doc Returns true if the first character of a token is a binary selector character.
%%
%% Binary selector characters: + - * / < > = ~ % & ? , \
%% These are the same characters recognised by the Beamtalk lexer.
-spec is_binary_selector_token(binary()) -> boolean().
is_binary_selector_token(<<H, _/binary>>) ->
    lists:member(H, "+-*/<>=~%&?,\\");
is_binary_selector_token(<<>>) ->
    false.

%% @private
%% @doc Returns true if the token contains characters that make it un-parseable
%% as part of a simple chain (parentheses or the `>>` method-reference operator).
-spec has_invalid_chain_chars(binary()) -> boolean().
has_invalid_chain_chars(T) ->
    binary:match(T, <<"(">>) =/= nomatch orelse
        binary:match(T, <<")">>) =/= nomatch orelse
        binary:match(T, <<">>">>) =/= nomatch.

%% @private
%% @doc Parse a whitespace-separated expression into a receiver token and a list
%% of mixed unary/binary hops (BT-1071).
%%
%% Extends `tokenise_send_chain/1` to handle binary message sends mid-chain:
%% each binary operator token consumes the following token as its argument.
%% Binary hops are tagged `{binary, Selector}` and unary hops `{unary, Selector}`.
%%
%% Returns `error` for:
%% - Empty or single-token expressions (no chain to walk)
%% - Tokens containing parentheses or `>>` (complex sub-expressions)
%% - Binary operators with no following argument token
%% - Keyword sends (tokens containing `:`)
%%
%% Examples:
%%   <<"counter value + 1">>  → {ok, <<"counter">>, [{unary, value}, {binary, '+'}]}
%%   <<"\"foo\" , \"bar\"">>  → {ok, <<"\"foo\"">>, [{binary, ','}]}
%%   <<"myList size + offset">>→ {ok, <<"myList">>, [{unary, size}, {binary, '+'}]}
%%   <<"x + 1 * 2">>          → {ok, <<"x">>, [{binary, '+'}, {binary, '*'}]}
-type chain_hop() :: {unary, atom()} | {binary, atom()}.
-spec tokenise_binary_chain(binary()) -> {ok, binary(), [chain_hop()]} | error.
tokenise_binary_chain(<<>>) ->
    error;
tokenise_binary_chain(Expr) when is_binary(Expr) ->
    Parts = [list_to_binary(T) || T <- string:tokens(binary_to_list(Expr), " \t")],
    case Parts of
        [] ->
            error;
        [_] ->
            %% Single token — no sends to walk
            error;
        [ReceiverToken | HopTokens] ->
            case has_invalid_chain_chars(ReceiverToken) of
                true ->
                    error;
                false ->
                    case parse_binary_hops(HopTokens) of
                        error -> error;
                        {ok, []} -> error;
                        {ok, Hops} -> {ok, ReceiverToken, Hops}
                    end
            end
    end.

%% @private
-spec parse_binary_hops([binary()]) -> {ok, [chain_hop()]} | error.
parse_binary_hops([]) ->
    {ok, []};
parse_binary_hops([Token | Rest]) ->
    case has_invalid_chain_chars(Token) of
        true ->
            error;
        false ->
            case is_binary_selector_token(Token) of
                true ->
                    %% Binary op: consume the following token as the argument
                    case Rest of
                        [] ->
                            %% Binary operator with no argument — malformed
                            error;
                        [ArgToken | Rest2] ->
                            case
                                has_invalid_chain_chars(ArgToken) orelse
                                    binary:match(ArgToken, <<":">>) =/= nomatch
                            of
                                true ->
                                    error;
                                false ->
                                    try
                                        Sel = binary_to_existing_atom(Token, utf8),
                                        case parse_binary_hops(Rest2) of
                                            {ok, MoreHops} -> {ok, [{binary, Sel} | MoreHops]};
                                            error -> error
                                        end
                                    catch
                                        error:badarg -> error
                                    end
                            end
                    end;
                false ->
                    %% Must be a valid unary selector (no colon, starts with letter/_)
                    case is_valid_unary_selector(Token) of
                        true ->
                            try
                                Sel = binary_to_existing_atom(Token, utf8),
                                case parse_binary_hops(Rest) of
                                    {ok, MoreHops} -> {ok, [{unary, Sel} | MoreHops]};
                                    error -> error
                                end
                            catch
                                error:badarg -> error
                            end;
                        false ->
                            %% Keyword send or other unrecognised token
                            error
                    end
            end
    end.

%% @private
%% @doc Walk an instance-side send chain containing mixed unary and binary hops (BT-1071).
%%
%% For each `{unary, Sel}` hop: looks up `get_method_return_type(ClassName, Sel)`.
%% For each `{binary, Sel}` hop: looks up `get_method_return_type(ClassName, Sel)`.
%% Returns `undefined` (graceful fallback) when any hop lacks a return-type annotation.
-spec walk_mixed_chain(atom(), [chain_hop()]) -> {ok, atom()} | undefined.
walk_mixed_chain(ClassName, Hops) ->
    walk_mixed_chain(ClassName, Hops, 0).

-spec walk_mixed_chain(atom(), [chain_hop()], non_neg_integer()) -> {ok, atom()} | undefined.
walk_mixed_chain(ClassName, [], _Depth) ->
    {ok, ClassName};
walk_mixed_chain(_ClassName, _Hops, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    undefined;
walk_mixed_chain(ClassName, [{_Kind, Sel} | Rest], Depth) ->
    case beamtalk_class_registry:get_method_return_type(ClassName, Sel) of
        {ok, NextClass} -> walk_mixed_chain(NextClass, Rest, Depth + 1);
        {error, not_found} -> undefined
    end.

%% @private
%% @doc Walk a chain starting from the class side with mixed unary/binary hops (BT-1071).
%%
%% The first hop uses `get_class_method_return_type`; subsequent hops transition to
%% instance-side via `walk_mixed_chain/3`.
-spec walk_mixed_chain_class(atom(), [chain_hop()]) -> {ok, atom()} | undefined.
walk_mixed_chain_class(ClassName, []) ->
    {ok, ClassName};
walk_mixed_chain_class(ClassName, [{unary, class} | Rest]) ->
    %% `ClassName class` → metaclass; stay on the class side for completions.
    walk_mixed_chain_class(ClassName, Rest);
walk_mixed_chain_class(ClassName, [{_Kind, Sel} | Rest]) ->
    case beamtalk_class_registry:get_class_method_return_type(ClassName, Sel) of
        {ok, NextClass} -> walk_mixed_chain(NextClass, Rest, 1);
        {error, not_found} -> undefined
    end.

%% @private
%% @doc Resolve the type at the end of a send chain using static return-type metadata.
%%
%% Tokenises the expression, classifies the receiver, then walks the chain by
%% looking up each send's return type in `method_return_types` on the class registry.
%%
%% When the unary tokenizer fails, tries the binary/mixed tokenizer (BT-1071).
%% When both tokenizers fail (e.g. parenthesised subexpressions, keyword sends
%% mid-chain), falls back to compiler-based type resolution (BT-1068, ADR 0045 Option C).
-spec resolve_chain_type(binary(), map()) -> {ok, atom()} | undefined.
resolve_chain_type(Expr, Bindings) ->
    case tokenise_send_chain(Expr) of
        {ok, ReceiverToken, Selectors} ->
            case classify_receiver(ReceiverToken, Bindings) of
                {instance, ClassName} -> walk_chain(ClassName, Selectors);
                {class, ClassName} -> walk_chain_class(ClassName, Selectors);
                undefined -> undefined
            end;
        error ->
            %% BT-1071: try binary/mixed chain tokenizer before the compiler port.
            case tokenise_binary_chain(Expr) of
                {ok, ReceiverToken, Hops} ->
                    case classify_receiver(ReceiverToken, Bindings) of
                        {instance, ClassName} ->
                            walk_mixed_chain(ClassName, Hops);
                        {class, ClassName} ->
                            walk_mixed_chain_class(ClassName, Hops);
                        undefined ->
                            %% Receiver parsed but not classifiable — fall back to compiler.
                            resolve_type_via_compiler(Expr)
                    end;
                error ->
                    %% BT-1068: tokeniser can't parse the expression — try the compiler port.
                    resolve_type_via_compiler(Expr)
            end
    end.

%% @private
%% @doc Compiler-based type resolution fallback for complex expressions (BT-1068).
%%
%% Sends the expression to the Rust compiler via the port. The compiler parses
%% it fully, runs type inference, and returns the type of the last expression.
%% Falls back to `undefined' if the compiler is unavailable or the type is unknown.
-spec resolve_type_via_compiler(binary()) -> {ok, atom()} | undefined.
resolve_type_via_compiler(Expr) ->
    try beamtalk_compiler:resolve_completion_type(Expr) of
        {ok, ClassName} -> {ok, ClassName};
        {error, type_unknown} -> undefined
    catch
        _:_ -> undefined
    end.

%% @private
%% @doc Walk an instance-side send chain, following method return types at each hop.
%%
%% Returns `{ok, FinalClassName}` when every hop has a known return type,
%% or `undefined` when any hop breaks (annotation absent or non-Simple).
-spec walk_chain(atom(), [atom()]) -> {ok, atom()} | undefined.
walk_chain(ClassName, Selectors) ->
    walk_chain(ClassName, Selectors, 0).

-spec walk_chain(atom(), [atom()], non_neg_integer()) -> {ok, atom()} | undefined.
walk_chain(ClassName, [], _Depth) ->
    {ok, ClassName};
walk_chain(_ClassName, _Selectors, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    undefined;
walk_chain(ClassName, [Selector | Rest], Depth) ->
    case beamtalk_runtime_api:get_method_return_type(ClassName, Selector) of
        {ok, NextClass} -> walk_chain(NextClass, Rest, Depth + 1);
        {error, not_found} -> undefined
    end.

%% @private
%% @doc Walk a chain starting from the class side.
%%
%% The first hop uses `class_method_return_types`; subsequent hops transition to the
%% instance side via `walk_chain/2`.
%%
%% Special case: `class` is an instance method on ProtoObject (not annotated with a
%% return type). When a class object receives `class`, it returns its metaclass, which
%% has the same class-side methods for completion purposes. We stay on the class side
%% rather than failing the chain.
-spec walk_chain_class(atom(), [atom()]) -> {ok, atom()} | undefined.
walk_chain_class(ClassName, []) ->
    {ok, ClassName};
walk_chain_class(ClassName, [class | Rest]) ->
    %% `ClassName class` → metaclass; for completions treat as still on the class side.
    walk_chain_class(ClassName, Rest);
walk_chain_class(ClassName, [Selector | Rest]) ->
    case beamtalk_runtime_api:get_class_method_return_type(ClassName, Selector) of
        {ok, NextClass} -> walk_chain(NextClass, Rest, 1);
        {error, not_found} -> undefined
    end.

%% @private
%% @doc Remove methods that are internal Object protocol and should not appear
%% in user-facing completions. See ?COMPLETION_HIDDEN_METHODS.
-spec filter_hidden_methods([atom()]) -> [atom()].
filter_hidden_methods(Selectors) ->
    Hidden = sets:from_list(?COMPLETION_HIDDEN_METHODS, [{version, 2}]),
    [S || S <- Selectors, not sets:is_element(S, Hidden)].

%% @private
%% @doc Return all instance methods of a class filtered by the given prefix.
-spec complete_instance_methods(atom(), binary()) -> [binary()].
complete_instance_methods(ClassName, Prefix) ->
    MethodSelectors = filter_hidden_methods(collect_all_methods(ClassName, 0)),
    All = [atom_to_binary(S, utf8) || S <- MethodSelectors],
    case Prefix of
        <<>> ->
            lists:usort(All);
        _ ->
            lists:usort([
                M
             || M <- All,
                binary:match(M, Prefix) =:= {0, byte_size(Prefix)}
            ])
    end.

%% @private
%% @doc Get method selectors for a given receiver token.
%% For class-name receivers (uppercase), returns only class-side methods (via hierarchy
%% walk). Instance methods are excluded — they cannot be called on the class object.
%% For instance receivers (literals, bindings), returns instance methods.
-spec get_methods_for_receiver(binary(), map()) -> [atom()].
get_methods_for_receiver(Receiver, Bindings) when is_binary(Receiver) ->
    case classify_receiver(Receiver, Bindings) of
        undefined ->
            [];
        {class, ClassName} ->
            %% Class-object receiver: class-side methods plus the ProtoObject instance
            %% methods that are genuinely callable on any object (including class objects).
            %% Most instance methods are excluded — they can't be called on the class object
            %% and would cause misleading completions that always error.
            %% ProtoObject methods (e.g. `class`) are included because class objects ARE
            %% objects and `ClassName class` is meaningful (returns the metaclass).
            ProtoObjMethods = filter_hidden_methods(collect_all_methods('ProtoObject', 0)),
            collect_all_class_methods(ClassName, 0) ++ ProtoObjMethods;
        {instance, ClassName} ->
            filter_hidden_methods(collect_all_methods(ClassName, 0))
    end.

%% @private
%% @doc Classify a receiver token as a class object or instance, with optional binding lookup.
%% Returns {class, ClassName} for class-object receivers (uppercase class names),
%% {instance, ClassName} for instance receivers (literals, bindings), or undefined.
-spec classify_receiver(binary(), map()) -> {class, atom()} | {instance, atom()} | undefined.
classify_receiver(<<>>, _Bindings) ->
    undefined;
classify_receiver(<<H, _/binary>> = Receiver, Bindings) when H >= $A, H =< $Z ->
    %% Starts with uppercase — first try class registry, then fall back to bindings.
    %% The fallback handles uppercase global bindings like `Transcript` that are
    %% workspace actors, not class names.
    case beamtalk_repl_errors:safe_to_existing_atom(Receiver) of
        {ok, ClassName} ->
            case
                try
                    beamtalk_runtime_api:whereis_class(ClassName)
                catch
                    _:_ -> undefined
                end
            of
                undefined ->
                    %% Not a class — check if it's a named binding (e.g. Transcript)
                    classify_by_binding(ClassName, Bindings);
                _Pid ->
                    {class, ClassName}
            end;
        {error, _} ->
            undefined
    end;
classify_receiver(<<H, _/binary>> = Receiver, _Bindings) when H >= $0, H =< $9 ->
    %% Only treat as Integer when the whole token is digits (guards against
    %% float literals like "3.14" being misclassified as Integer).
    case lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(Receiver)) of
        true ->
            case maybe_class('Integer') of
                undefined -> undefined;
                ClassName -> {instance, ClassName}
            end;
        false ->
            undefined
    end;
classify_receiver(<<$", _/binary>>, _Bindings) ->
    %% String literal — complete String instance methods
    case maybe_class('String') of
        undefined -> undefined;
        ClassName -> {instance, ClassName}
    end;
classify_receiver(Receiver, Bindings) ->
    %% Lowercase identifier — look up in bindings to find the class
    case beamtalk_repl_errors:safe_to_existing_atom(Receiver) of
        {ok, VarAtom} -> classify_by_binding(VarAtom, Bindings);
        {error, _} -> undefined
    end.

%% @private
%% @doc Classify a receiver by looking it up in the bindings map.
%% Uses beamtalk_runtime_api:primitive_class_of/1 as the canonical type classifier,
%% which handles actors, tagged maps, primitives, symbols, blocks, nil, etc.
-spec classify_by_binding(atom(), map()) -> {instance, atom()} | undefined.
classify_by_binding(VarAtom, Bindings) ->
    case maps:find(VarAtom, Bindings) of
        {ok, Value} ->
            ClassName = beamtalk_runtime_api:primitive_class_of(Value),
            case maybe_class(ClassName) of
                undefined -> undefined;
                _ -> {instance, ClassName}
            end;
        error ->
            undefined
    end.

%% @private
-spec maybe_class(atom()) -> atom() | undefined.
maybe_class(ClassName) ->
    case
        try
            beamtalk_runtime_api:whereis_class(ClassName)
        catch
            _:_ -> undefined
        end
    of
        undefined -> undefined;
        _Pid -> ClassName
    end.

%% @private
%% @doc Get the session bindings map from a session PID.
%% Returns empty map if session is unavailable.
-spec get_session_bindings(pid()) -> map().
get_session_bindings(SessionPid) ->
    try
        {ok, Bindings} = beamtalk_repl_shell:get_bindings(SessionPid),
        Bindings
    catch
        _:_ -> #{}
    end.

%% @private
%% @doc Get workspace-level global bindings (e.g. Transcript, Beamtalk, Workspace).
%% Uses get_session_bindings/0 to include singletons as well as bind:as: entries.
%% Returns empty map if workspace interface is unavailable.
-spec get_workspace_bindings() -> map().
get_workspace_bindings() ->
    try
        beamtalk_workspace_interface_primitives:get_session_bindings()
    catch
        _:_ -> #{}
    end.

%% @private
%% @doc Collect all class-side method selectors for a class by walking the superclass chain.
%% Guards against excessive depth via ?MAX_HIERARCHY_DEPTH (codebase convention).
-spec collect_all_class_methods(atom(), non_neg_integer()) -> [atom()].
collect_all_class_methods(ClassName, Depth) ->
    collect_methods_with_fun(ClassName, Depth, fun beamtalk_runtime_api:local_class_methods/1).

%% @private
%% @doc Collect all instance method selectors for a class by walking the superclass chain.
%% Guards against excessive depth via ?MAX_HIERARCHY_DEPTH (codebase convention).
-spec collect_all_methods(atom(), non_neg_integer()) -> [atom()].
collect_all_methods(ClassName, Depth) ->
    collect_methods_with_fun(ClassName, Depth, fun beamtalk_runtime_api:class_methods/1).

%% @private
%% @doc Walk the superclass chain collecting methods via a caller-supplied getter fun.
-spec collect_methods_with_fun(atom(), non_neg_integer(), fun((pid()) -> [atom()])) -> [atom()].
collect_methods_with_fun(_ClassName, Depth, _Fun) when Depth > ?MAX_HIERARCHY_DEPTH ->
    [];
collect_methods_with_fun(ClassName, Depth, Fun) ->
    case
        try
            beamtalk_runtime_api:whereis_class(ClassName)
        catch
            _:_ -> undefined
        end
    of
        undefined ->
            [];
        ClassPid ->
            LocalMethods =
                try
                    Fun(ClassPid)
                catch
                    _:_ -> []
                end,
            Superclass =
                try
                    beamtalk_runtime_api:superclass(ClassPid)
                catch
                    _:_ -> none
                end,
            InheritedMethods =
                case Superclass of
                    none -> [];
                    Super -> collect_methods_with_fun(Super, Depth + 1, Fun)
                end,
            LocalMethods ++ InheritedMethods
    end.

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
%% @doc Describe available protocol operations.
%%
%% Deprecated ops (BT-849 / ADR 0040 Phase 6) include a `deprecated` flag
%% and a `migrate_to` hint so WebSocket clients can discover the migration path.
-spec describe_ops() -> map().
describe_ops() ->
    #{
        <<"eval">> => #{<<"params">> => [<<"code">>]},
        <<"stdin">> => #{<<"params">> => [<<"value">>]},
        <<"complete">> => #{<<"params">> => [<<"code">>], <<"optional">> => [<<"cursor">>]},
        <<"test">> => #{<<"params">> => [], <<"optional">> => [<<"class">>, <<"file">>]},
        <<"test-all">> => #{<<"params">> => []},
        <<"docs">> => #{
            <<"params">> => [<<"class">>],
            <<"optional">> => [<<"selector">>],
            <<"deprecated">> => true,
            <<"migrate_to">> => <<"eval: Beamtalk help: ClassName">>
        },
        <<"load-file">> => #{
            <<"params">> => [<<"path">>],
            <<"deprecated">> => true,
            <<"migrate_to">> => <<"eval: Workspace load: \"path\"">>
        },
        <<"load-source">> => #{<<"params">> => [<<"source">>]},
        <<"load-project">> => #{
            <<"params">> => [<<"path">>],
            <<"optional">> => [<<"include_tests">>]
        },
        <<"reload">> => #{
            <<"params">> => [],
            <<"optional">> => [<<"module">>, <<"path">>],
            <<"deprecated">> => true,
            <<"migrate_to">> => <<"eval: ClassName reload">>
        },
        <<"clear">> => #{
            <<"params">> => [],
            <<"deprecated">> => true
        },
        <<"bindings">> => #{
            <<"params">> => [],
            <<"deprecated">> => true
        },
        <<"sessions">> => #{<<"params">> => []},
        <<"clone">> => #{<<"params">> => []},
        <<"close">> => #{<<"params">> => []},
        <<"interrupt">> => #{<<"params">> => []},
        <<"modules">> => #{
            <<"params">> => [],
            <<"deprecated">> => true,
            <<"migrate_to">> => <<"eval: Workspace classes">>
        },
        <<"actors">> => #{<<"params">> => []},
        <<"inspect">> => #{<<"params">> => [<<"actor">>]},
        <<"kill">> => #{<<"params">> => [<<"actor">>]},
        <<"unload">> => #{<<"params">> => [<<"module">>]},
        <<"health">> => #{<<"params">> => []},
        <<"methods">> => #{<<"params">> => [<<"class">>]},
        <<"show-codegen">> => #{<<"params">> => [<<"code">>]},
        <<"describe">> => #{<<"params">> => []},
        <<"shutdown">> => #{<<"params">> => [<<"cookie">>]}
    }.

%% @doc Return a list of method descriptors for a class by name (BT-1026).
%%
%% Collects local instance methods and local class-side methods for the named
%% class. Returns an empty list if the class name is unknown or not loaded.
%% Each entry is a map with <<"name">>, <<"selector">>, and <<"side">> keys.
-spec list_class_methods_for_ws(binary()) -> [map()].
list_class_methods_for_ws(ClassBin) when is_binary(ClassBin) ->
    case beamtalk_repl_errors:safe_to_existing_atom(ClassBin) of
        {error, badarg} ->
            [];
        {ok, ClassName} ->
            case beamtalk_runtime_api:whereis_class(ClassName) of
                undefined ->
                    [];
                Pid ->
                    InstanceSelectors = lists:sort(
                        beamtalk_runtime_api:local_instance_methods(Pid)
                    ),
                    ClassSelectors = lists:sort(beamtalk_runtime_api:local_class_methods(Pid)),
                    InstanceEntries = [
                        #{
                            <<"name">> => atom_to_binary(S, utf8),
                            <<"selector">> => atom_to_binary(S, utf8),
                            <<"side">> => <<"instance">>
                        }
                     || S <- InstanceSelectors
                    ],
                    ClassEntries = [
                        #{
                            <<"name">> => atom_to_binary(S, utf8),
                            <<"selector">> => atom_to_binary(S, utf8),
                            <<"side">> => <<"class">>
                        }
                     || S <- ClassSelectors
                    ],
                    InstanceEntries ++ ClassEntries
            end
    end.

%% @private
-spec list_state_vars_for_ws(binary()) -> [binary()].
list_state_vars_for_ws(ClassBin) when is_binary(ClassBin) ->
    case beamtalk_repl_errors:safe_to_existing_atom(ClassBin) of
        {error, badarg} ->
            [];
        {ok, ClassName} ->
            case beamtalk_runtime_api:whereis_class(ClassName) of
                undefined ->
                    [];
                Pid ->
                    IVars = beamtalk_runtime_api:instance_variables(Pid),
                    lists:sort([atom_to_binary(V, utf8) || V <- IVars])
            end
    end.

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
        <<"Use Workspace classes or :modules to see loaded classes.">>
    ).

%% @private
-spec to_binary(atom() | binary()) -> binary().
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) -> B.
