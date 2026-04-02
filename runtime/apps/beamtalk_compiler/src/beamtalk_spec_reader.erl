%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% @doc Reads `-spec` attributes and parameter names from `.beam` abstract code.
%%
%% **DDD Context:** Compilation (Anti-Corruption Layer)
%%
%% Phase 0 spike for ADR 0075: Erlang FFI Type Definitions.
%%
%% Extracts spec forms and spec variable names from the `abstract_code` chunk
%% of a `.beam` file in a single pass, mapping Erlang types to Beamtalk types.
%%
%% Protocol (stdin/stdout term format):
%%   INPUT:  Erlang term: `{read_specs, BeamFilePath}' (one per line)
%%   OUTPUT: JSON on stdout (one JSON object per line)
%%
%% @end

-module(beamtalk_spec_reader).

-export([
    read_specs/1,
    map_type/1
]).

-ifdef(TEST).
-export([
    extract_param_names/1,
    extract_specs_from_forms/1
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
            Specs = extract_specs_from_forms(Forms),
            {ok, Specs};
        {ok, {_Module, [{abstract_code, no_abstract_code}]}} ->
            {error, no_debug_info};
        {error, beam_lib, Reason} ->
            {error, {beam_lib, Reason}}
    end.

%% @doc Extract spec entries from abstract forms.
%%
%% Walks the forms list once, collecting `{attribute, _, spec, ...}' forms.
%% For each spec, extracts parameter names from spec variable annotations
%% and maps Erlang types to Beamtalk type names.
-spec extract_specs_from_forms([erl_parse:abstract_form()]) -> [map()].
extract_specs_from_forms(Forms) ->
    lists:filtermap(
        fun
            ({attribute, _, spec, {{Name, Arity}, Clauses}}) ->
                Entry = process_spec(Name, Arity, Clauses),
                {true, Entry};
            ({attribute, _, spec, {{_Mod, Name, Arity}, Clauses}}) ->
                %% Remote spec form (module:function/arity)
                Entry = process_spec(Name, Arity, Clauses),
                {true, Entry};
            (_) ->
                false
        end,
        Forms
    ).

%% Process a single spec into our result format.
-spec process_spec(atom(), non_neg_integer(), [tuple()]) -> map().
process_spec(Name, Arity, Clauses) ->
    %% Use the first clause for param names and types
    {Params, ReturnType} =
        case Clauses of
            [{type, _, 'fun', [{type, _, product, ArgTypes}, RetType]} | _] ->
                ParamList = extract_param_names(ArgTypes),
                {ParamList, map_type(RetType)};
            [
                {type, _, bounded_fun, [
                    {type, _, 'fun', [{type, _, product, ArgTypes}, RetType]},
                    Constraints
                ]}
                | _
            ] ->
                %% Bounded fun: resolve constrained type variables
                ConstraintMap = build_constraint_map(Constraints),
                ParamList = extract_param_names_with_constraints(ArgTypes, ConstraintMap),
                ResolvedRet = resolve_type_with_constraints(RetType, ConstraintMap),
                {ParamList, ResolvedRet};
            _ ->
                %% Fallback: generate positional params
                Positional = [
                    #{
                        name => positional_name(I),
                        type => <<"Dynamic">>
                    }
                 || I <- lists:seq(1, Arity)
                ],
                {Positional, <<"Dynamic">>}
        end,
    #{
        name => atom_to_binary(Name, utf8),
        arity => Arity,
        params => Params,
        return_type => ReturnType
    }.

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
-spec resolve_type_with_constraints(tuple(), map()) -> binary().
resolve_type_with_constraints({var, _, VarName}, ConstraintMap) ->
    case maps:find(VarName, ConstraintMap) of
        {ok, Type} -> map_type(Type);
        error -> <<"Dynamic">>
    end;
resolve_type_with_constraints(Type, _ConstraintMap) ->
    map_type(Type).

%% @doc Map an Erlang abstract type to a Beamtalk type name.
%%
%% Implements the reverse mapping from ADR 0075 Table 1.
-spec map_type(tuple()) -> binary().
%% Basic types
map_type({type, _, integer, []}) -> <<"Integer">>;
map_type({type, _, non_neg_integer, []}) -> <<"Integer">>;
map_type({type, _, pos_integer, []}) -> <<"Integer">>;
map_type({type, _, neg_integer, []}) -> <<"Integer">>;
map_type({type, _, float, []}) -> <<"Float">>;
map_type({type, _, number, []}) -> <<"Number">>;
map_type({type, _, binary, _}) -> <<"String">>;
map_type({type, _, boolean, []}) -> <<"Boolean">>;
map_type({type, _, atom, []}) -> <<"Symbol">>;
map_type({type, _, pid, []}) -> <<"Pid">>;
map_type({type, _, 'fun', _}) -> <<"Block">>;
%% List types
map_type({type, _, list, []}) -> <<"List">>;
map_type({type, _, list, [_ElemType]}) -> <<"List">>;
map_type({type, _, nonempty_list, _}) -> <<"List">>;
%% Tuple types
map_type({type, _, tuple, _}) -> <<"Tuple">>;
%% Map types
map_type({type, _, map, _}) -> <<"Dictionary">>;
%% Literal atoms
map_type({atom, _, true}) -> <<"True">>;
map_type({atom, _, false}) -> <<"False">>;
map_type({atom, _, nil}) -> <<"Nil">>;
map_type({atom, _, _}) -> <<"Symbol">>;
%% Catch-all types that map to Dynamic
map_type({type, _, term, []}) -> <<"Dynamic">>;
map_type({type, _, any, []}) -> <<"Dynamic">>;
map_type({type, _, no_return, []}) -> <<"Dynamic">>;
map_type({type, _, iodata, []}) -> <<"Dynamic">>;
map_type({type, _, iolist, []}) -> <<"Dynamic">>;
map_type({type, _, node, []}) -> <<"Symbol">>;
map_type({type, _, module, []}) -> <<"Symbol">>;
map_type({type, _, char, []}) -> <<"Integer">>;
map_type({type, _, byte, []}) -> <<"Integer">>;
map_type({type, _, string, []}) -> <<"List">>;
%% Union types — use the first branch (simplified)
map_type({type, _, union, [First | _]}) -> map_type(First);
%% Range type (e.g., 1..10)
map_type({type, _, range, _}) -> <<"Integer">>;
%% Remote types (e.g., sets:set())
map_type({remote_type, _, _}) -> <<"Dynamic">>;
%% User-defined types
map_type({user_type, _, _, _}) -> <<"Dynamic">>;
%% Annotated types — unwrap the annotation
map_type({ann_type, _, [_Var, Type]}) -> map_type(Type);
%% Type variables — unconstrained maps to Dynamic
map_type({var, _, _}) -> <<"Dynamic">>;
%% Integer literals/singletons
map_type({integer, _, _}) -> <<"Integer">>;
%% Anything else
map_type(_) -> <<"Dynamic">>.

%% Normalize a spec variable name to a lowercase binary suitable for Beamtalk keywords.
-spec normalize_param_name(atom()) -> binary().
normalize_param_name(Name) ->
    Bin = atom_to_binary(Name, utf8),
    string:lowercase(Bin).

%% Generate a positional parameter name like "arg1", "arg2", etc.
-spec positional_name(pos_integer()) -> binary().
positional_name(N) ->
    iolist_to_binary([<<"arg">>, integer_to_binary(N)]).
