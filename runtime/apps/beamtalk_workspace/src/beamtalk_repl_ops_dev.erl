%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Op handlers for complete, docs, describe, and show-codegen operations.
%%%
%%% **DDD Context:** REPL
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
    make_class_not_found_error/1,
    base_protocol_response/1
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
                Bindings = get_session_bindings(SessionPid),
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
%% @doc Parse the line up to the cursor into a {Receiver, Prefix} pair.
%%
%% Examples:
%%   <<"Integer s">>  → {<<"Integer">>, <<"s">>}
%%   <<"Integer ">>   → {<<"Integer">>, <<>>}
%%   <<"42 s">>       → {<<"42">>, <<"s">>}
%%   <<"s">>          → {undefined, <<"s">>}
%%   <<>>             → {undefined, <<>>}
-spec parse_receiver_and_prefix(binary()) -> {binary() | undefined, binary()}.
parse_receiver_and_prefix(<<>>) ->
    {undefined, <<>>};
parse_receiver_and_prefix(Line) when is_binary(Line) ->
    Str = binary_to_list(Line),
    RevStr = lists:reverse(Str),
    %% Extract trailing identifier characters (the completion prefix)
    {PrefixCharsRev, Rest} = lists:splitwith(fun is_identifier_char/1, RevStr),
    Prefix = list_to_binary(lists:reverse(PrefixCharsRev)),
    %% Skip whitespace before the prefix
    {SpaceChars, ReceiverPart} = lists:splitwith(
        fun(C) -> C =:= $\s orelse C =:= $\t end, Rest
    ),
    case SpaceChars of
        [] ->
            %% No space before prefix — single token, no receiver
            {undefined, Prefix};
        _ ->
            %% Extract the token immediately before the space (the receiver)
            {ReceiverCharsRev, _} = lists:splitwith(
                fun(C) -> C =/= $\s andalso C =/= $\t end, ReceiverPart
            ),
            case ReceiverCharsRev of
                [] -> {undefined, Prefix};
                _ -> {list_to_binary(lists:reverse(ReceiverCharsRev)), Prefix}
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

%% @private
%% @doc Get method selectors for a given receiver token.
%% For class-name receivers (uppercase), returns class-side methods.
%% For instance receivers (literals, bindings), returns instance methods.
-spec get_methods_for_receiver(binary(), map()) -> [atom()].
get_methods_for_receiver(Receiver, Bindings) when is_binary(Receiver) ->
    case classify_receiver(Receiver, Bindings) of
        undefined ->
            [];
        {class, ClassName} ->
            %% Class-object receiver: complete class-side methods + built-in class methods
            collect_all_class_methods(ClassName, 0) ++ builtin_class_methods();
        {instance, ClassName} ->
            collect_all_methods(ClassName, 0)
    end.

%% @private
%% @doc Classify a receiver token as a class object or instance, with optional binding lookup.
%% Returns {class, ClassName} for class-object receivers (uppercase class names),
%% {instance, ClassName} for instance receivers (literals, bindings), or undefined.
-spec classify_receiver(binary(), map()) -> {class, atom()} | {instance, atom()} | undefined.
classify_receiver(<<>>, _Bindings) ->
    undefined;
classify_receiver(<<H, _/binary>> = Receiver, _Bindings) when H >= $A, H =< $Z ->
    %% Starts with uppercase — treat as a class object; complete class-side methods
    case beamtalk_repl_server:safe_to_existing_atom(Receiver) of
        {ok, ClassName} ->
            case
                try
                    beamtalk_class_registry:whereis_class(ClassName)
                catch
                    _:_ -> undefined
                end
            of
                undefined -> undefined;
                _Pid -> {class, ClassName}
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
    %% Lowercase identifier — look up in session bindings to find the class
    case beamtalk_repl_server:safe_to_existing_atom(Receiver) of
        {ok, VarAtom} ->
            case maps:find(VarAtom, Bindings) of
                {ok, #beamtalk_object{class = ClassName}} ->
                    %% Actor binding: complete instance methods of the actor's class
                    case maybe_class(ClassName) of
                        undefined -> undefined;
                        _ -> {instance, ClassName}
                    end;
                _ ->
                    undefined
            end;
        {error, _} ->
            undefined
    end.

%% @private
-spec maybe_class(atom()) -> atom() | undefined.
maybe_class(ClassName) ->
    case
        try
            beamtalk_class_registry:whereis_class(ClassName)
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
%% @doc The built-in class-side methods available on every concrete class.
-spec builtin_class_methods() -> [atom()].
builtin_class_methods() ->
    [spawn, new, 'new:', 'spawnWith:', 'subclass:'].

%% @private
%% @doc Collect all class-side method selectors for a class by walking the superclass chain.
%% Guards against excessive depth via ?MAX_HIERARCHY_DEPTH (codebase convention).
-spec collect_all_class_methods(atom(), non_neg_integer()) -> [atom()].
collect_all_class_methods(_ClassName, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    [];
collect_all_class_methods(ClassName, Depth) ->
    case
        try
            beamtalk_class_registry:whereis_class(ClassName)
        catch
            _:_ -> undefined
        end
    of
        undefined ->
            [];
        ClassPid ->
            LocalMethods =
                try
                    beamtalk_object_class:local_class_methods(ClassPid)
                catch
                    _:_ -> []
                end,
            Superclass =
                try
                    beamtalk_object_class:superclass(ClassPid)
                catch
                    _:_ -> none
                end,
            InheritedMethods =
                case Superclass of
                    none -> [];
                    Super -> collect_all_class_methods(Super, Depth + 1)
                end,
            LocalMethods ++ InheritedMethods
    end.

%% @private
%% @doc Collect all instance method selectors for a class by walking the superclass chain.
%% Guards against excessive depth via ?MAX_HIERARCHY_DEPTH (codebase convention).
-spec collect_all_methods(atom(), non_neg_integer()) -> [atom()].
collect_all_methods(_ClassName, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    [];
collect_all_methods(ClassName, Depth) ->
    case
        try
            beamtalk_class_registry:whereis_class(ClassName)
        catch
            _:_ -> undefined
        end
    of
        undefined ->
            [];
        ClassPid ->
            LocalMethods =
                try
                    beamtalk_object_class:methods(ClassPid)
                catch
                    _:_ -> []
                end,
            Superclass =
                try
                    beamtalk_object_class:superclass(ClassPid)
                catch
                    _:_ -> none
                end,
            InheritedMethods =
                case Superclass of
                    none -> [];
                    Super -> collect_all_methods(Super, Depth + 1)
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
-spec describe_ops() -> map().
describe_ops() ->
    #{
        <<"eval">> => #{<<"params">> => [<<"code">>]},
        <<"stdin">> => #{<<"params">> => [<<"value">>]},
        <<"complete">> => #{<<"params">> => [<<"code">>], <<"optional">> => [<<"cursor">>]},
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
        <<"shutdown">> => #{<<"params">> => [<<"cookie">>]}
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
