%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Op handlers for complete, info, docs, and describe operations.
%%%
%%% **DDD Context:** REPL
%%%
%%% Extracted from beamtalk_repl_server (BT-705).

-module(beamtalk_repl_ops_dev).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([handle/4, get_completions/1, get_symbol_info/1, make_class_not_found_error/1,
         base_protocol_response/1]).

%% @doc Handle complete/info/docs/describe ops.
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(<<"complete">>, Params, Msg, _SessionPid) ->
    Prefix = maps:get(<<"code">>, Params, <<>>),
    Completions = get_completions(Prefix),
    case beamtalk_repl_protocol:is_legacy(Msg) of
        true ->
            jsx:encode(#{<<"type">> => <<"completions">>,
                        <<"completions">> => Completions});
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
                make_class_not_found_error(ClassBin), Msg, fun beamtalk_repl_json:format_error_message/1);
        {ok, ClassName} ->
            Selector = maps:get(<<"selector">>, Params, undefined),
            case Selector of
                undefined ->
                    case beamtalk_repl_docs:format_class_docs(ClassName) of
                        {ok, DocText} ->
                            beamtalk_repl_protocol:encode_docs(DocText, Msg);
                        {error, {class_not_found, _}} ->
                            beamtalk_repl_protocol:encode_error(
                                make_class_not_found_error(ClassName), Msg, fun beamtalk_repl_json:format_error_message/1)
                    end;
                SelectorBin ->
                    case beamtalk_repl_docs:format_method_doc(ClassName, SelectorBin) of
                        {ok, DocText} ->
                            beamtalk_repl_protocol:encode_docs(DocText, Msg);
                        {error, {class_not_found, _}} ->
                            beamtalk_repl_protocol:encode_error(
                                make_class_not_found_error(ClassName), Msg, fun beamtalk_repl_json:format_error_message/1);
                        {error, {method_not_found, _, _}} ->
                            NameBin = to_binary(ClassName),
                            SelectorAtom = binary_to_atom(SelectorBin, utf8),
                            Err0 = beamtalk_error:new(does_not_understand, ClassName),
                            Err1 = beamtalk_error:with_selector(Err0, SelectorAtom),
                            Err2 = beamtalk_error:with_message(Err1,
                                iolist_to_binary([NameBin, <<" does not understand ">>, SelectorBin])),
                            Err3 = beamtalk_error:with_hint(Err2,
                                iolist_to_binary([<<"Use :help ">>, NameBin, <<" to see available methods.">>])),
                            beamtalk_repl_protocol:encode_error(
                                Err3, Msg, fun beamtalk_repl_json:format_error_message/1)
                    end
            end
    end;

handle(<<"describe">>, _Params, Msg, _SessionPid) ->
    Ops = describe_ops(),
    BeamtalkVsnBin =
        case application:get_key(beamtalk_workspace, vsn) of
            {ok, Vsn} when is_list(Vsn)   -> list_to_binary(Vsn);
            {ok, Vsn} when is_binary(Vsn) -> Vsn;
            _                             -> <<"0.1.0">>
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
    M1 = case Id of undefined -> M0; _ -> M0#{<<"id">> => Id} end,
    case Session of undefined -> M1; _ -> M1#{<<"session">> => Session} end.

%% @private
-spec get_completions(binary()) -> [binary()].
get_completions(<<>>) -> [];
get_completions(Prefix) when is_binary(Prefix) ->
    PrefixStr = binary_to_list(Prefix),
    ClassPids = try beamtalk_class_registry:all_classes()
                catch _:_ -> [] end,
    ClassNames = lists:filtermap(
        fun(Pid) ->
            try
                Name = beamtalk_object_class:class_name(Pid),
                {true, atom_to_binary(Name, utf8)}
            catch _:_ -> false
            end
        end,
        ClassPids
    ),
    [Name || Name <- ClassNames, binary:match(Name, Prefix) =:= {0, byte_size(Prefix)}]
    ++
    [atom_to_binary(B, utf8) || B <- try beamtalk_workspace_config:binding_names()
                                      catch _:_ -> [] end,
     binary:match(atom_to_binary(B, utf8), Prefix) =:= {0, byte_size(Prefix)}]
    ++
    [Kw || Kw <- builtin_keywords(), binary:match(Kw, Prefix) =:= {0, byte_size(Prefix)},
           PrefixStr =/= ""].

%% @private
-spec builtin_keywords() -> [binary()].
builtin_keywords() ->
    [<<"self">>, <<"super">>, <<"true">>, <<"false">>, <<"nil">>,
     <<"ifTrue:">>, <<"ifFalse:">>, <<"ifTrue:ifFalse:">>,
     <<"whileTrue:">>, <<"timesRepeat:">>,
     <<"subclass:">>, <<"spawn">>, <<"new">>].

%% @private
-spec get_symbol_info(binary()) -> map().
get_symbol_info(Symbol) when is_binary(Symbol) ->
    SymAtom = try binary_to_existing_atom(Symbol, utf8)
              catch _:_ -> undefined end,
    case SymAtom of
        undefined ->
            #{<<"found">> => false,
              <<"symbol">> => Symbol};
        _ ->
            IsClass = try
                ClassPids2 = beamtalk_class_registry:all_classes(),
                lists:any(fun(Pid) ->
                    try
                        beamtalk_object_class:class_name(Pid) =:= SymAtom
                    catch _:_ -> false
                    end
                end, ClassPids2)
            catch _:_ -> false end,
            case IsClass of
                true ->
                    #{<<"found">> => true,
                      <<"symbol">> => Symbol,
                      <<"kind">> => <<"class">>};
                false ->
                    #{<<"found">> => false,
                      <<"symbol">> => Symbol}
            end
    end.

%% @private
-spec describe_ops() -> map().
describe_ops() ->
    #{
        <<"eval">>        => #{<<"params">> => [<<"code">>]},
        <<"stdin">>       => #{<<"params">> => [<<"value">>]},
        <<"complete">>    => #{<<"params">> => [<<"code">>]},
        <<"info">>        => #{<<"params">> => [<<"symbol">>]},
        <<"docs">>        => #{<<"params">> => [<<"class">>],
                               <<"optional">> => [<<"selector">>]},
        <<"load-file">>   => #{<<"params">> => [<<"path">>]},
        <<"load-source">> => #{<<"params">> => [<<"source">>]},
        <<"reload">>      => #{<<"params">> => [],
                               <<"optional">> => [<<"module">>, <<"path">>]},
        <<"clear">>       => #{<<"params">> => []},
        <<"bindings">>    => #{<<"params">> => []},
        <<"sessions">>    => #{<<"params">> => []},
        <<"clone">>       => #{<<"params">> => []},
        <<"close">>       => #{<<"params">> => []},
        <<"actors">>      => #{<<"params">> => []},
        <<"inspect">>     => #{<<"params">> => [<<"actor">>]},
        <<"kill">>        => #{<<"params">> => [<<"actor">>]},
        <<"interrupt">>   => #{<<"params">> => []},
        <<"modules">>     => #{<<"params">> => []},
        <<"unload">>      => #{<<"params">> => [<<"module">>]},
        <<"health">>      => #{<<"params">> => []},
        <<"describe">>    => #{<<"params">> => []},
        <<"shutdown">>    => #{<<"params">> => [<<"cookie">>]}
    }.

%% @private
-spec make_class_not_found_error(atom() | binary()) -> #beamtalk_error{}.
make_class_not_found_error(ClassName) ->
    NameBin = to_binary(ClassName),
    Err0 = beamtalk_error:new(class_not_found, 'REPL'),
    Err1 = beamtalk_error:with_message(Err0,
        iolist_to_binary([<<"Unknown class: ">>, NameBin])),
    beamtalk_error:with_hint(Err1,
        <<"Use :modules to see loaded classes.">>).

%% @private
-spec to_binary(atom() | binary()) -> binary().
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) -> B.
