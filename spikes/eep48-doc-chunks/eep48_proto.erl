%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%
%% PROTOTYPE for the BT-4852 spike (ADR 0149). Not production code.
%%
%% Builds an EEP-48 docs_v1 term for a Beamtalk module from the doc maps the
%% compiler already emits in `register_class/0`, and hands it to the OTP
%% compiler through the `extra_chunks` option, so `compile:forms/2` writes the
%% "Docs" chunk itself. Nothing rewrites the .beam afterwards.
-module(eep48_proto).
-export([compile_core/2, compile_core/3, docs_from_core/1, parse_core/1,
         chunk_file/2]).

-include_lib("kernel/include/eep48.hrl").

parse_core(File) ->
    {ok, Bin} = file:read_file(File),
    {ok, Toks, _} = core_scan:string(binary_to_list(Bin)),
    {ok, Core} = core_parse:parse(Toks),
    Core.

%% Same options as beamtalk_build_worker:compile_core_forms/1, plus the chunk.
compile_core(File, Mode) -> compile_core(File, Mode, beamtalk).
compile_core(File, Mode, Lang) ->
    Core0 = parse_core(File),
    %% `noload` drops on_load so consumer tests run without the Beamtalk runtime.
    Core = case get(eep48_proto_noload) of
        true ->
            As = [A || {K, _} = A <- cerl:module_attrs(Core0), cerl:concrete(K) =/= on_load],
            cerl:update_c_module(Core0, cerl:module_name(Core0), cerl:module_exports(Core0), As, cerl:module_defs(Core0));
        _ -> Core0
    end,
    Base = [from_core, binary, return_errors, return_warnings, debug_info, clint],
    Opts = case Mode of
        nodocs -> Base;
        docs ->
            Docs = docs_from_core(Core),
            [{extra_chunks, [{<<"Docs">>, term_to_binary(Docs#docs_v1{beam_language = Lang}, [deterministic, compressed])}]} | Base]
    end,
    case compile:forms(Core, Opts) of
        {ok, M, Beam, _W} -> {ok, M, Beam};
        {ok, M, Beam} -> {ok, M, Beam}
    end.

%% Side-file variant: doc/chunks/<mod>.chunk, the fallback code:get_doc/1 reads.
chunk_file(File, EbinDir) ->
    Core = parse_core(File),
    Mod = cerl:concrete(cerl:module_name(Core)),
    Dir = filename:join([EbinDir, "doc", "chunks"]),
    ok = filelib:ensure_path(Dir),
    ok = file:write_file(filename:join(Dir, atom_to_list(Mod) ++ ".chunk"),
                         term_to_binary(docs_from_core(Core), [deterministic, compressed])).

docs_from_core(Core) ->
    Mod = cerl:concrete(cerl:module_name(Core)),
    Exports = [{cerl:fname_id(E), cerl:fname_arity(E)} || E <- cerl:module_exports(Core)],
    Attrs = [{cerl:concrete(K), cerl:concrete(V)} || {K, V} <- cerl:module_attrs(Core)],
    [{Class, Super}] = proplists:get_value(beamtalk_class, Attrs, [{Mod, none}]),
    {_, Body} = lists:keyfind({register_class, 0}, 1,
        [{{cerl:fname_id(N), cerl:fname_arity(N)}, F} || {N, F} <- cerl:module_defs(Core)]),
    Maps = collect(Body, #{}),
    G = fun(K) -> maps:get(K, Maps, #{}) end,
    Meta = maps:get(meta, Maps, #{}),
    ModDoc = case maps:get(classDoc, Maps, nil) of
        B when is_binary(B) -> #{<<"en">> => B};
        _ -> none
    end,
    Inst = entries(false, Class, maps:get(method_info, Meta, #{}), G(methodDocs), G(methodSignatures), Exports),
    Cls = entries(true, Class, maps:get(class_method_info, Meta, #{}), G(classMethodDocs), G(classMethodSignatures), Exports),
    #docs_v1{
        anno = erl_anno:new(1),
        beam_language = beamtalk,
        format = <<"text/markdown">>,
        module_doc = ModDoc,
        metadata = #{beamtalk_class => Class, superclass => Super,
                     kind => maps:get(kind, Meta, undefined),
                     uses => maps:get(uses, Meta, [])},
        docs = lists:sort(Inst ++ Cls)
    }.

%% One entry per locally defined method.
%%   instance side: {function, Selector, MsgArity + 1}   (the exported fun of a Value class)
%%   class side:    {function, 'class_' ++ Selector, MsgArity + 1}
%% `exported` records whether that name/arity really is an exported function.
entries(ClassSide, Class, Info, Docs, Sigs, Exports) ->
    [begin
        Arity = maps:get(arity, I, 0) + 1,
        Name = case ClassSide of
            true -> list_to_atom("class_" ++ atom_to_list(Sel));
            false -> Sel
        end,
        Sig0 = maps:get(Sel, Sigs, atom_to_binary(Sel)),
        Sig = case ClassSide of
            true -> <<(atom_to_binary(Class))/binary, " class >> ", Sig0/binary>>;
            false -> <<(atom_to_binary(Class))/binary, " >> ", Sig0/binary>>
        end,
        Doc = case maps:get(Sel, Docs, none) of
            D when is_binary(D) -> #{<<"en">> => D};
            _ -> case maps:get(visibility, I, public) of public -> #{}; _ -> hidden end
        end,
        {{function, Name, Arity}, erl_anno:new(1), [Sig], Doc,
         #{selector => Sel, class_side => ClassSide,
           exported => lists:member({Name, Arity}, Exports)}}
     end || {Sel, I} <- maps:to_list(Info)].

%% Find the literal maps under well-known keys anywhere in register_class/0.
collect(Tree, Acc0) ->
    cerl_trees:fold(fun(Node, Acc) ->
        case cerl:type(Node) of
            map_pair ->
                K = cerl:map_pair_key(Node),
                case cerl:is_literal(K) andalso lists:member(cerl:concrete(K),
                        [classDoc, methodDocs, classMethodDocs, methodSignatures,
                         classMethodSignatures, meta]) of
                    true ->
                        try Acc#{cerl:concrete(K) => concrete(cerl:map_pair_val(Node))}
                        catch _:_ -> Acc end;
                    false -> Acc
                end;
            literal ->
                %% core_parse folds an all-literal map into one literal node.
                case cerl:concrete(Node) of
                    #{className := _} = M ->
                        maps:merge(Acc, maps:with([classDoc, methodDocs, classMethodDocs,
                            methodSignatures, classMethodSignatures, meta], M));
                    _ -> Acc
                end;
            _ -> Acc
        end
    end, Acc0, Tree).

concrete(Node) ->
    case cerl:type(Node) of
        literal -> cerl:concrete(Node);
        map -> maps:from_list([{concrete(cerl:map_pair_key(P)), concrete(cerl:map_pair_val(P))}
                               || P <- cerl:map_es(Node)]);
        binary -> << <<(cerl:concrete(cerl:bitstr_val(S))):8>> || S <- cerl:binary_segments(Node) >>;
        cons -> [concrete(cerl:cons_hd(Node)) | concrete(cerl:cons_tl(Node))];
        tuple -> list_to_tuple([concrete(E) || E <- cerl:tuple_es(Node)])
    end.
