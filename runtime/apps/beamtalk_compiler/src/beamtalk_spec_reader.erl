%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Reads `-spec` attributes and parameter names from `.beam` abstract code.
%%
%% **DDD Context:** Compilation (Anti-Corruption Layer)
%%
%% ADR 0075: Erlang FFI Type Definitions (Phase 1).
%% ADR 0076: ok/error → Result type mapping (Phase 2).
%%
%% Extracts spec forms and spec variable names from the `abstract_code` chunk
%% of a `.beam` file in a single pass, mapping Erlang types to Beamtalk types.
%%
%% Supports batch processing of multiple modules via `read_specs_batch/1' and
%% integration with the `beamtalk_build_worker' protocol.
%%
%% Multi-clause specs produce a union of return types. Parameter names and
%% types come from the first clause (Erlang convention).
%%
%% @end

-module(beamtalk_spec_reader).

-export([
    read_specs/1,
    read_specs_batch/1,
    read_raw_specs/1,
    read_types/1,
    format_erlang_spec/1,
    format_erlang_type/1,
    map_type/1
]).

-ifdef(TEST).
-export([
    extract_param_names/1,
    extract_specs_from_forms/1,
    merge_return_types/1
]).
-endif.

%% @doc Read specs from a `.beam` file.
%%
%% Returns `{ok, Specs}' where `Specs' is a list of spec entries, or
%% `{error, Reason}' if the file cannot be read or has no abstract code.
%%
%% Each spec entry is a map:
%% ```
%% #{name => atom(),
%%   arity => non_neg_integer(),
%%   params => [#{name => binary(), type => binary()}],
%%   return_type => binary()}
%% ```
-spec read_specs(file:filename_all()) ->
    {ok, [map()]} | {error, no_debug_info | {beam_lib, term()}}.
read_specs(BeamFile) ->
    case beam_lib:chunks(BeamFile, [abstract_code]) of
        {ok, {_Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            TypeRegistry = build_type_registry(Forms),
            OpaqueSet = build_opaque_set(Forms),
            set_type_context(TypeRegistry, OpaqueSet),
            try
                Specs = extract_specs_from_forms(Forms),
                {ok, Specs}
            after
                clear_type_context()
            end;
        {ok, {_Module, [{abstract_code, no_abstract_code}]}} ->
            {error, no_debug_info};
        {error, beam_lib, Reason} ->
            {error, {beam_lib, Reason}}
    end.

%% @doc Batch-read specs from multiple `.beam` files.
%%
%% Returns a list of `{Module, Specs}' tuples, one per input file.
%% Each result is:
%%   `{Module, {ok, Specs}}' on success
%%   `{Module, {error, Reason}}' on failure
%%
%% The module name is extracted from the `.beam` file's module attribute.
-spec read_specs_batch([file:filename_all()]) ->
    [{binary(), {ok, [map()]} | {error, term()}}].
read_specs_batch(BeamFiles) ->
    lists:map(
        fun(BeamFile) ->
            ModName = beam_file_to_module_name(BeamFile),
            case read_specs(BeamFile) of
                {ok, Specs} ->
                    {ModName, {ok, Specs}};
                {error, Reason} ->
                    {ModName, {error, Reason}}
            end
        end,
        BeamFiles
    ).

%% @doc Read raw spec abstract forms from a `.beam` file.
%%
%% Returns `{ok, RawSpecs}' where `RawSpecs' is a list of
%% `{Name, Arity, SpecForm}' tuples containing the original abstract form
%% suitable for pretty-printing with `erl_pp:attribute/1'.
%% Only specs for exported functions are returned.
-spec read_raw_specs(file:filename_all()) ->
    {ok, [{atom(), non_neg_integer(), tuple()}]} | {error, no_debug_info | {beam_lib, term()}}.
read_raw_specs(BeamFile) ->
    case beam_lib:chunks(BeamFile, [abstract_code]) of
        {ok, {_Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            Exports = extract_exports(Forms),
            RawSpecs = lists:filtermap(
                fun
                    ({attribute, _, spec, {{Name, Arity}, _Clauses}} = Form) ->
                        case sets:is_element({Name, Arity}, Exports) of
                            true -> {true, {Name, Arity, Form}};
                            false -> false
                        end;
                    ({attribute, _, spec, {{_Mod, Name, Arity}, _Clauses}} = Form) ->
                        case sets:is_element({Name, Arity}, Exports) of
                            true -> {true, {Name, Arity, Form}};
                            false -> false
                        end;
                    (_) ->
                        false
                end,
                Forms
            ),
            {ok, RawSpecs};
        {ok, {_Module, [{abstract_code, no_abstract_code}]}} ->
            {error, no_debug_info};
        {error, beam_lib, Reason} ->
            {error, {beam_lib, Reason}}
    end.

%% @doc Read type definitions from a `.beam` file.
%%
%% Returns `{ok, Types}' where `Types' is a list of
%% `{Name, Arity, TypeForm}' tuples containing the original abstract form
%% suitable for pretty-printing with `erl_pp:attribute/1'.
-spec read_types(file:filename_all()) ->
    {ok, [{atom(), non_neg_integer(), tuple()}]} | {error, no_debug_info | {beam_lib, term()}}.
read_types(BeamFile) ->
    case beam_lib:chunks(BeamFile, [abstract_code]) of
        {ok, {_Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            Types = lists:filtermap(
                fun
                    ({attribute, _, type, {Name, _Def, Args}} = Form) when is_atom(Name) ->
                        {true, {Name, length(Args), Form}};
                    (_) ->
                        false
                end,
                Forms
            ),
            {ok, Types};
        {ok, {_Module, [{abstract_code, no_abstract_code}]}} ->
            {error, no_debug_info};
        {error, beam_lib, Reason} ->
            {error, {beam_lib, Reason}}
    end.

%% @doc Pretty-print a raw spec abstract form as an Erlang `-spec' string.
%%
%% Takes the raw spec form from `read_raw_specs/1' and returns a formatted
%% binary like `<<"-spec next_file(Log) -> ok | {error, Reason}.">>'.
-spec format_erlang_spec(tuple()) -> binary().
format_erlang_spec(SpecForm) ->
    Formatted = erl_pp:attribute(SpecForm),
    Bin = iolist_to_binary(Formatted),
    string:trim(Bin).

%% @doc Pretty-print a raw type abstract form as an Erlang `-type' string.
%%
%% Takes the raw type form from `read_types/1' and returns a formatted
%% binary like `<<"-type my_type() :: a | b.">>'.
-spec format_erlang_type(tuple()) -> binary().
format_erlang_type(TypeForm) ->
    Formatted = erl_pp:attribute(TypeForm),
    Bin = iolist_to_binary(Formatted),
    string:trim(Bin).

%% Extract module name from a .beam file path as a binary.
-spec beam_file_to_module_name(file:filename_all()) -> binary().
beam_file_to_module_name(BeamFile) ->
    BaseName = filename:basename(BeamFile, ".beam"),
    iolist_to_binary(BaseName).

%% @doc Extract spec entries from abstract forms.
%%
%% Collects `{attribute, _, spec, ...}' forms and the export list from the
%% given forms. Only specs for exported functions are returned.
-spec extract_specs_from_forms([erl_parse:abstract_form()]) -> [map()].
extract_specs_from_forms(Forms) ->
    Exports = extract_exports(Forms),
    lists:filtermap(
        fun
            ({attribute, _, spec, {{Name, Arity}, Clauses}}) ->
                case sets:is_element({Name, Arity}, Exports) of
                    true ->
                        Entry = process_spec(Name, Arity, Clauses),
                        {true, Entry};
                    false ->
                        false
                end;
            ({attribute, _, spec, {{_Mod, Name, Arity}, Clauses}}) ->
                %% Remote spec form (module:function/arity)
                case sets:is_element({Name, Arity}, Exports) of
                    true ->
                        Entry = process_spec(Name, Arity, Clauses),
                        {true, Entry};
                    false ->
                        false
                end;
            (_) ->
                false
        end,
        Forms
    ).

%% Extract the set of exported {Name, Arity} pairs from abstract forms.
-spec extract_exports([erl_parse:abstract_form()]) -> sets:set({atom(), non_neg_integer()}).
extract_exports(Forms) ->
    lists:foldl(
        fun
            ({attribute, _, export, FnList}, Acc) ->
                lists:foldl(fun(FA, S) -> sets:add_element(FA, S) end, Acc, FnList);
            (_, Acc) ->
                Acc
        end,
        sets:new(),
        Forms
    ).

%% Process a single spec into our result format.
%%
%% Multi-clause specs: parameter names and types come from the first clause
%% (Erlang convention — all clauses share the same arity). The return type
%% is the union (deduped) of all clause return types.
-spec process_spec(atom(), non_neg_integer(), [tuple()]) -> map().
process_spec(Name, Arity, Clauses) ->
    %% Extract params from the first clause, return types from all clauses
    {Params, ReturnTypes} = extract_from_clauses(Clauses, Arity),
    ReturnType = merge_return_types(ReturnTypes),
    #{
        name => atom_to_binary(Name, utf8),
        arity => Arity,
        params => Params,
        return_type => ReturnType
    }.

%% Extract params (from first clause) and return types (from all clauses).
-spec extract_from_clauses([tuple()], non_neg_integer()) ->
    {[map()], [binary()]}.
extract_from_clauses(Clauses, Arity) ->
    extract_from_clauses(Clauses, Arity, first, []).

extract_from_clauses([], Arity, first, _RetAcc) ->
    %% No clauses at all — generate fallback
    Positional = [
        #{name => positional_name(I), type => <<"Dynamic">>}
     || I <- lists:seq(1, Arity)
    ],
    {Positional, [<<"Dynamic">>]};
extract_from_clauses([], _Arity, {have_params, Params}, RetAcc) ->
    {Params, lists:reverse(RetAcc)};
extract_from_clauses([Clause | Rest], Arity, ParamState, RetAcc) ->
    case extract_clause(Clause) of
        {ok, ClauseParams, ClauseRet} ->
            NewParamState =
                case ParamState of
                    first -> {have_params, ClauseParams};
                    {have_params, _} = S -> S
                end,
            extract_from_clauses(Rest, Arity, NewParamState, [ClauseRet | RetAcc]);
        fallback ->
            %% Unrecognized clause form — skip it
            extract_from_clauses(Rest, Arity, ParamState, RetAcc)
    end.

%% Extract params and return type from a single clause.
-spec extract_clause(tuple()) ->
    {ok, [map()], binary()} | fallback.
extract_clause({type, _, 'fun', [{type, _, product, ArgTypes}, RetType]}) ->
    ParamList = extract_param_names(ArgTypes),
    {ok, ParamList, map_type(RetType)};
extract_clause(
    {type, _, bounded_fun, [
        {type, _, 'fun', [{type, _, product, ArgTypes}, RetType]},
        Constraints
    ]}
) ->
    ConstraintMap = build_constraint_map(Constraints),
    ParamList = extract_param_names_with_constraints(ArgTypes, ConstraintMap),
    ResolvedRet = resolve_type_with_constraints(RetType, ConstraintMap),
    {ok, ParamList, ResolvedRet};
extract_clause(_) ->
    fallback.

%% Merge a list of return type binaries into a single type.
%%
%% - Single type: returns it directly
%% - Multiple distinct types: joins with ` | ` (union syntax)
%% - Duplicate types are removed
%% - Empty list: returns `<<"Dynamic">>'
-spec merge_return_types([binary()]) -> binary().
merge_return_types([]) ->
    <<"Dynamic">>;
merge_return_types(Types) ->
    Unique = dedup_types(Types),
    case Unique of
        [Single] -> Single;
        Multiple -> iolist_to_binary(lists:join(<<" | ">>, Multiple))
    end.

%% Remove duplicate types while preserving order.
-spec dedup_types([binary()]) -> [binary()].
dedup_types(Types) ->
    dedup_types(Types, [], #{}).

dedup_types([], Acc, _Seen) ->
    lists:reverse(Acc);
dedup_types([T | Rest], Acc, Seen) ->
    case maps:is_key(T, Seen) of
        true -> dedup_types(Rest, Acc, Seen);
        false -> dedup_types(Rest, [T | Acc], Seen#{T => true})
    end.

%% Extract parameter names from spec type arguments.
%% Spec variable names (e.g., `From :: integer()`) provide meaningful names.
-spec extract_param_names([tuple()]) -> [map()].
extract_param_names(ArgTypes) ->
    lists:map(
        fun
            ({ann_type, _, [{var, _, VarName}, Type]}) ->
                %% Annotated type: `VarName :: Type`
                #{
                    name => normalize_param_name(VarName),
                    type => map_type(Type)
                };
            ({var, _, VarName}) ->
                %% Bare type variable — unconstrained, maps to Dynamic
                #{
                    name => normalize_param_name(VarName),
                    type => <<"Dynamic">>
                };
            (Type) ->
                %% No variable name — use positional
                #{
                    name => <<"arg">>,
                    type => map_type(Type)
                }
        end,
        ArgTypes
    ).

%% Extract param names, resolving constrained type variables via the constraint map.
-spec extract_param_names_with_constraints([tuple()], map()) -> [map()].
extract_param_names_with_constraints(ArgTypes, ConstraintMap) ->
    lists:map(
        fun
            ({ann_type, _, [{var, _, VarName}, Type]}) ->
                #{
                    name => normalize_param_name(VarName),
                    type => resolve_type_with_constraints(Type, ConstraintMap)
                };
            ({var, _, VarName}) ->
                ResolvedType =
                    case maps:find(VarName, ConstraintMap) of
                        {ok, T} -> map_type(T);
                        error -> <<"Dynamic">>
                    end,
                #{
                    name => normalize_param_name(VarName),
                    type => ResolvedType
                };
            (Type) ->
                #{
                    name => <<"arg">>,
                    type => resolve_type_with_constraints(Type, ConstraintMap)
                }
        end,
        ArgTypes
    ).

%% Build a map from type variable names to their constraint types.
%% Constraints come from `when Var :: Type` clauses in bounded_fun specs.
-spec build_constraint_map([tuple()]) -> map().
build_constraint_map(Constraints) ->
    lists:foldl(
        fun
            (
                {type, _, constraint, [
                    {atom, _, is_subtype},
                    [{var, _, VarName}, Type]
                ]},
                Acc
            ) ->
                Acc#{VarName => Type};
            (_, Acc) ->
                Acc
        end,
        #{},
        Constraints
    ).

%% Resolve a type, substituting constrained type variables.
%%
%% For union types, each branch is resolved individually so that type
%% variables inside union branches get their constraint types applied
%% before ok/error Result recognition runs.
-spec resolve_type_with_constraints(tuple(), map()) -> binary().
resolve_type_with_constraints({var, _, VarName}, ConstraintMap) ->
    case maps:find(VarName, ConstraintMap) of
        {ok, Type} -> map_type(Type);
        error -> <<"Dynamic">>
    end;
resolve_type_with_constraints({type, Line, union, Branches}, ConstraintMap) ->
    ResolvedBranches = [
        resolve_branch_with_constraints(B, ConstraintMap)
     || B <- Branches
    ],
    map_union(ResolvedBranches, Line);
resolve_type_with_constraints(Type, _ConstraintMap) ->
    map_type(Type).

%% Resolve a single union branch, substituting constrained type variables
%% inside tuple elements. This preserves the abstract form structure so
%% that map_union/2 can pattern-match on ok/error tuples.
-spec resolve_branch_with_constraints(tuple(), map()) -> tuple().
resolve_branch_with_constraints({var, Line, VarName}, ConstraintMap) ->
    case maps:find(VarName, ConstraintMap) of
        {ok, Type} -> Type;
        error -> {var, Line, VarName}
    end;
resolve_branch_with_constraints({type, Line, tuple, Elements}, ConstraintMap) when
    is_list(Elements)
->
    ResolvedElements = [
        resolve_branch_with_constraints(E, ConstraintMap)
     || E <- Elements
    ],
    {type, Line, tuple, ResolvedElements};
resolve_branch_with_constraints(Other, _ConstraintMap) ->
    Other.

%% ---------------------------------------------------------------
%% Type registry: extract -type/-opaque definitions from abstract forms
%% ---------------------------------------------------------------

%% Build a map from {TypeName, Arity} to the type body abstract form.
%% Only includes -type definitions (not -opaque).
-spec build_type_registry([erl_parse:abstract_form()]) -> map().
build_type_registry(Forms) ->
    lists:foldl(
        fun
            ({attribute, _, type, {Name, TypeBody, TypeParams}}, Acc) when
                is_atom(Name)
            ->
                Acc#{{Name, length(TypeParams)} => TypeBody};
            (_, Acc) ->
                Acc
        end,
        #{},
        Forms
    ).

%% Build a set of opaque type names (with arity) to avoid resolving them.
-spec build_opaque_set([erl_parse:abstract_form()]) -> sets:set({atom(), non_neg_integer()}).
build_opaque_set(Forms) ->
    lists:foldl(
        fun
            ({attribute, _, opaque, {Name, _TypeBody, TypeParams}}, Acc) when
                is_atom(Name)
            ->
                sets:add_element({Name, length(TypeParams)}, Acc);
            (_, Acc) ->
                Acc
        end,
        sets:new(),
        Forms
    ).

%% Store the type registry and opaque set in process dictionary.
-spec set_type_context(map(), sets:set()) -> ok.
set_type_context(TypeRegistry, OpaqueSet) ->
    put(beamtalk_type_registry, TypeRegistry),
    put(beamtalk_opaque_set, OpaqueSet),
    put(beamtalk_type_depth, 0),
    ok.

%% Clear type context from process dictionary.
-spec clear_type_context() -> ok.
clear_type_context() ->
    erase(beamtalk_type_registry),
    erase(beamtalk_opaque_set),
    erase(beamtalk_type_depth),
    ok.

%% Maximum recursion depth for type resolution.
-define(MAX_TYPE_DEPTH, 5).

%% Try to resolve a user_type via the local type registry.
%% Returns the mapped Beamtalk type or <<"Dynamic">> if not resolvable.
-spec resolve_user_type(atom(), [tuple()]) -> binary().
resolve_user_type(Name, Args) ->
    Arity = length(Args),
    OpaqueSet = get(beamtalk_opaque_set),
    case OpaqueSet =/= undefined andalso sets:is_element({Name, Arity}, OpaqueSet) of
        true ->
            <<"Dynamic">>;
        false ->
            Registry = get(beamtalk_type_registry),
            Depth =
                case get(beamtalk_type_depth) of
                    undefined -> 0;
                    D -> D
                end,
            case Registry =/= undefined andalso Depth < ?MAX_TYPE_DEPTH of
                true ->
                    case maps:find({Name, Arity}, Registry) of
                        {ok, TypeBody} ->
                            put(beamtalk_type_depth, Depth + 1),
                            try
                                map_type(TypeBody)
                            after
                                put(beamtalk_type_depth, Depth)
                            end;
                        error ->
                            <<"Dynamic">>
                    end;
                false ->
                    <<"Dynamic">>
            end
    end.

%% Try to resolve a remote_type by loading the remote module's beam file.
%% Returns the mapped Beamtalk type or <<"Dynamic">> if not resolvable.
-spec resolve_remote_type(atom(), atom(), [tuple()]) -> binary().
resolve_remote_type(Mod, TypeName, Args) ->
    Arity = length(Args),
    Depth =
        case get(beamtalk_type_depth) of
            undefined -> 0;
            D -> D
        end,
    case Depth < ?MAX_TYPE_DEPTH of
        true ->
            case code:which(Mod) of
                non_existing ->
                    <<"Dynamic">>;
                preloaded ->
                    %% Preloaded modules (erlang, init, etc.) have no .beam on disk
                    <<"Dynamic">>;
                cover_compiled ->
                    <<"Dynamic">>;
                BeamFile ->
                    resolve_remote_type_from_beam(BeamFile, Mod, TypeName, Arity, Depth)
            end;
        false ->
            <<"Dynamic">>
    end.

-spec resolve_remote_type_from_beam(
    file:filename_all(), atom(), atom(), non_neg_integer(), non_neg_integer()
) -> binary().
resolve_remote_type_from_beam(BeamFile, _Mod, TypeName, Arity, Depth) ->
    case beam_lib:chunks(BeamFile, [abstract_code]) of
        {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            RemoteOpaqueSet = build_opaque_set(Forms),
            case sets:is_element({TypeName, Arity}, RemoteOpaqueSet) of
                true ->
                    <<"Dynamic">>;
                false ->
                    RemoteRegistry = build_type_registry(Forms),
                    case maps:find({TypeName, Arity}, RemoteRegistry) of
                        {ok, TypeBody} ->
                            %% Temporarily install remote registry for nested resolution
                            OldRegistry = get(beamtalk_type_registry),
                            OldOpaqueSet = get(beamtalk_opaque_set),
                            put(beamtalk_type_registry, RemoteRegistry),
                            put(beamtalk_opaque_set, RemoteOpaqueSet),
                            put(beamtalk_type_depth, Depth + 1),
                            try
                                map_type(TypeBody)
                            after
                                put(beamtalk_type_registry, OldRegistry),
                                put(beamtalk_opaque_set, OldOpaqueSet),
                                put(beamtalk_type_depth, Depth)
                            end;
                        error ->
                            <<"Dynamic">>
                    end
            end;
        _ ->
            <<"Dynamic">>
    end.

%% @doc Map an Erlang abstract type to a Beamtalk type name.
%%
%% Implements the reverse mapping from ADR 0075 Table 1.
%% When a type registry is available (set via process dictionary during
%% read_specs/1), user_type and remote_type references are resolved
%% instead of falling back to Dynamic.
-spec map_type(tuple()) -> binary().
%% Basic types
map_type({type, _, integer, []}) ->
    <<"Integer">>;
map_type({type, _, non_neg_integer, []}) ->
    <<"Integer">>;
map_type({type, _, pos_integer, []}) ->
    <<"Integer">>;
map_type({type, _, neg_integer, []}) ->
    <<"Integer">>;
map_type({type, _, float, []}) ->
    <<"Float">>;
map_type({type, _, number, []}) ->
    <<"Number">>;
map_type({type, _, binary, _}) ->
    <<"String">>;
map_type({type, _, boolean, []}) ->
    <<"Boolean">>;
map_type({type, _, atom, []}) ->
    <<"Symbol">>;
map_type({type, _, pid, []}) ->
    <<"Pid">>;
map_type({type, _, 'fun', _}) ->
    <<"Block">>;
%% List types
map_type({type, _, list, []}) ->
    <<"List">>;
map_type({type, _, list, [_ElemType]}) ->
    <<"List">>;
map_type({type, _, nonempty_list, _}) ->
    <<"List">>;
%% Tuple types
map_type({type, _, tuple, _}) ->
    <<"Tuple">>;
%% Map types
map_type({type, _, map, _}) ->
    <<"Dictionary">>;
%% Literal atoms
map_type({atom, _, true}) ->
    <<"True">>;
map_type({atom, _, false}) ->
    <<"False">>;
map_type({atom, _, nil}) ->
    <<"Nil">>;
map_type({atom, _, _}) ->
    <<"Symbol">>;
%% Catch-all types that map to Dynamic
map_type({type, _, term, []}) ->
    <<"Dynamic">>;
map_type({type, _, any, []}) ->
    <<"Dynamic">>;
map_type({type, _, no_return, []}) ->
    <<"Dynamic">>;
map_type({type, _, iodata, []}) ->
    <<"Dynamic">>;
map_type({type, _, iolist, []}) ->
    <<"Dynamic">>;
map_type({type, _, node, []}) ->
    <<"Symbol">>;
map_type({type, _, module, []}) ->
    <<"Symbol">>;
map_type({type, _, char, []}) ->
    <<"Integer">>;
map_type({type, _, byte, []}) ->
    <<"Integer">>;
map_type({type, _, string, []}) ->
    <<"List">>;
%% Union types — recognize ok/error patterns as Result(T, E)
map_type({type, Line, union, Branches}) ->
    map_union(Branches, Line);
%% Range type (e.g., 1..10)
map_type({type, _, range, _}) ->
    <<"Integer">>;
%% Remote types (e.g., sets:set()) — resolve via beam file when possible
map_type({remote_type, _, [{atom, _, Mod}, {atom, _, TypeName}, Args]}) ->
    resolve_remote_type(Mod, TypeName, Args);
map_type({remote_type, _, _}) ->
    <<"Dynamic">>;
%% User-defined types — resolve via local type registry when possible
map_type({user_type, _, Name, Args}) ->
    resolve_user_type(Name, Args);
%% Annotated types — unwrap the annotation
map_type({ann_type, _, [_Var, Type]}) ->
    map_type(Type);
%% Type variables — unconstrained maps to Dynamic
map_type({var, _, _}) ->
    <<"Dynamic">>;
%% Integer literals/singletons
map_type({integer, _, _}) ->
    <<"Integer">>;
%% Anything else
map_type(_) ->
    <<"Dynamic">>.

%% @doc Classify union branches and emit Result(T, E) for ok/error patterns.
%%
%% Recognizes these patterns in union types (ADR 0076 Phase 2):
%%   - {ok, T} | {error, E}  → Result(T, E)
%%   - ok | {error, E}       → Result(Nil, E)    (bare ok atom)
%%   - {ok, T} alone         → Result(T, Dynamic) (no error branch)
%%   - {error, E} alone      → Result(Dynamic, E) (no ok branch)
%%   - 3+ branch union with ok/error → Result(T, E) | Other
%%
%% Non-ok/error unions fall through to standard type mapping.
-spec map_union([tuple()], term()) -> binary().
map_union(Branches, _Line) ->
    {OkTypes, ErrTypes, OtherBranches} = classify_union_branches(Branches),
    case {OkTypes, ErrTypes} of
        {[], []} ->
            %% No ok/error branches — standard union, map each branch
            Mapped = dedup_types([map_type(B) || B <- Branches]),
            case Mapped of
                [Single] -> Single;
                Multiple -> iolist_to_binary(lists:join(<<" | ">>, Multiple))
            end;
        _ ->
            %% At least one ok or error branch — emit Result(T, E)
            OkType = resolve_ok_type(OkTypes),
            ErrType = resolve_err_type(ErrTypes),
            ResultType = format_result_type(OkType, ErrType),
            case OtherBranches of
                [] ->
                    ResultType;
                _ ->
                    OtherMapped = dedup_types([map_type(B) || B <- OtherBranches]),
                    AllTypes = dedup_types([ResultType | OtherMapped]),
                    iolist_to_binary(lists:join(<<" | ">>, AllTypes))
            end
    end.

%% Classify union branches into ok tuples, error tuples, and other branches.
-spec classify_union_branches([tuple()]) ->
    {OkTypes :: [tuple() | nil], ErrTypes :: [tuple() | nil], Others :: [tuple()]}.
classify_union_branches(Branches) ->
    classify_union_branches(Branches, [], [], []).

classify_union_branches([], OkAcc, ErrAcc, OtherAcc) ->
    {lists:reverse(OkAcc), lists:reverse(ErrAcc), lists:reverse(OtherAcc)};
classify_union_branches([Branch | Rest], OkAcc, ErrAcc, OtherAcc) ->
    case Branch of
        {type, _, tuple, [{atom, _, ok}, OkInner]} ->
            classify_union_branches(Rest, [OkInner | OkAcc], ErrAcc, OtherAcc);
        {type, _, tuple, [{atom, _, ok}]} ->
            %% {ok} — single-element ok tuple, treat as ok with Nil value
            classify_union_branches(Rest, [nil | OkAcc], ErrAcc, OtherAcc);
        {atom, _, ok} ->
            %% Bare ok atom — maps to ok value of Nil
            classify_union_branches(Rest, [nil | OkAcc], ErrAcc, OtherAcc);
        {type, _, tuple, [{atom, _, error}, ErrInner]} ->
            classify_union_branches(Rest, OkAcc, [ErrInner | ErrAcc], OtherAcc);
        {type, _, tuple, [{atom, _, error}]} ->
            %% {error} — single-element error tuple, treat as error with Nil reason
            classify_union_branches(Rest, OkAcc, [nil | ErrAcc], OtherAcc);
        {atom, _, error} ->
            %% Bare error atom — maps to error reason of Nil
            classify_union_branches(Rest, OkAcc, [nil | ErrAcc], OtherAcc);
        _ ->
            classify_union_branches(Rest, OkAcc, ErrAcc, [Branch | OtherAcc])
    end.

%% Determine the ok type from classified ok branches.
-spec resolve_ok_type([tuple() | nil]) -> binary().
resolve_ok_type([]) ->
    <<"Dynamic">>;
resolve_ok_type([nil]) ->
    <<"Nil">>;
resolve_ok_type([Type]) ->
    map_type(Type);
resolve_ok_type(Types) ->
    %% Multiple ok branches — union their inner types
    Mapped = dedup_types([
        case T of
            nil -> <<"Nil">>;
            _ -> map_type(T)
        end
     || T <- Types
    ]),
    case Mapped of
        [Single] -> Single;
        Multiple -> iolist_to_binary(lists:join(<<" | ">>, Multiple))
    end.

%% Determine the error type from classified error branches.
-spec resolve_err_type([tuple() | nil]) -> binary().
resolve_err_type([]) ->
    <<"Dynamic">>;
resolve_err_type([nil]) ->
    <<"Nil">>;
resolve_err_type([Type]) ->
    map_type(Type);
resolve_err_type(Types) ->
    Mapped = dedup_types([
        case T of
            nil -> <<"Nil">>;
            _ -> map_type(T)
        end
     || T <- Types
    ]),
    case Mapped of
        [Single] -> Single;
        Multiple -> iolist_to_binary(lists:join(<<" | ">>, Multiple))
    end.

%% Format Result(T, E) type string.
-spec format_result_type(binary(), binary()) -> binary().
format_result_type(<<"Dynamic">>, <<"Dynamic">>) ->
    <<"Result">>;
format_result_type(OkType, <<"Dynamic">>) ->
    iolist_to_binary([<<"Result(">>, OkType, <<")">>]);
format_result_type(OkType, ErrType) ->
    iolist_to_binary([<<"Result(">>, OkType, <<", ">>, ErrType, <<")">>]).

%% Normalize a spec variable name to a lowercase binary suitable for Beamtalk keywords.
-spec normalize_param_name(atom()) -> binary().
normalize_param_name(Name) ->
    Bin = atom_to_binary(Name, utf8),
    string:lowercase(Bin).

%% Generate a positional parameter name like "arg1", "arg2", etc.
-spec positional_name(pos_integer()) -> binary().
positional_name(N) ->
    iolist_to_binary([<<"arg">>, integer_to_binary(N)]).
