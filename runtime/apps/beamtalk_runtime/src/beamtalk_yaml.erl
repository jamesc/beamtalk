%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc YAML class implementation — YAML parsing and generation via yamerl.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Yaml provides class-side methods for parsing and generating YAML strings.
%%% Uses the `yamerl` library (pure Erlang, no NIFs) with detailed node
%%% construction for reliable type-safe parsing.
%%%
%%% ## Type Mapping
%%%
%%% | YAML          | Beamtalk     |
%%% |---------------|--------------|
%%% | mapping       | Dictionary (keys typed per scalar) |
%%% | sequence      | List         |
%%% | string        | String       |
%%% | integer       | Integer      |
%%% | float         | Float        |
%%% | true/false    | Boolean      |
%%% | null/~        | nil          |
%%%
%%% ## Methods
%%%
%%% | Selector        | Description                                        |
%%% |-----------------|----------------------------------------------------|
%%% | `parse:`        | YAML string → Beamtalk value (first document)      |
%%% | `parseAll:`     | YAML string → List of all documents                |
%%% | `generate:`     | Beamtalk value → YAML string (flow style)          |
%%% | `parseFile:`    | Read file then parse first YAML document           |

-module(beamtalk_yaml).

-export(['parse:'/1, 'parseAll:'/1, 'generate:'/1, 'parseFile:'/1]).
-export([has_method/1]).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Parse a YAML string into a Beamtalk value (first document).
%%
%% YAML mappings become Dictionaries (maps whose key types follow the YAML
%% scalar type — string keys become Strings, integer keys become Integers),
%% sequences become Lists, strings become Strings, integers become Integers,
%% floats become Floats, true/false become booleans, null/~ become nil.
-spec 'parse:'(binary()) -> term().
'parse:'(YamlStr) when is_binary(YamlStr) ->
    Docs = parse_yaml_string(YamlStr, 'parse:'),
    case Docs of
        [] -> nil;
        [Doc | _] -> convert_node(Doc, 'parse:')
    end;
'parse:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'Yaml'),
    Error1 = beamtalk_error:with_selector(Error0, 'parse:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Parse a YAML string containing multiple documents.
%%
%% Returns a List of Beamtalk values, one per YAML document (separated by ---).
-spec 'parseAll:'(binary()) -> list().
'parseAll:'(YamlStr) when is_binary(YamlStr) ->
    Docs = parse_yaml_string(YamlStr, 'parseAll:'),
    [convert_node(Doc, 'parseAll:') || Doc <- Docs];
'parseAll:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'Yaml'),
    Error1 = beamtalk_error:with_selector(Error0, 'parseAll:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Generate a YAML string from a Beamtalk value (flow style).
%%
%% Produces valid YAML 1.2 in flow style. Dictionaries become YAML mappings,
%% Lists become sequences, Strings become YAML strings, Integer/Float become
%% numbers, true/false become YAML booleans, nil becomes null.
-spec 'generate:'(term()) -> binary().
'generate:'(Value) ->
    try
        Prepared = prepare_for_encode(Value),
        generate_flow(Prepared)
    catch
        error:#{error := #beamtalk_error{}} = E:_ ->
            error(E);
        _:Reason ->
            Error0 = beamtalk_error:new(type_error, 'Yaml'),
            Error1 = beamtalk_error:with_selector(Error0, 'generate:'),
            Error2 = beamtalk_error:with_details(Error1, #{reason => Reason}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Value cannot be converted to YAML">>),
            beamtalk_error:raise(Error3)
    end.

%% @doc Read a file and parse it as YAML (first document).
%%
%% Convenience method combining file reading with YAML parsing.
-spec 'parseFile:'(binary()) -> term().
'parseFile:'(Path) when is_binary(Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            'parse:'(Content);
        {error, Reason} ->
            Error0 = beamtalk_error:new(parse_error, 'Yaml'),
            Error1 = beamtalk_error:with_selector(Error0, 'parseFile:'),
            Error2 = beamtalk_error:with_details(Error1, #{path => Path, reason => Reason}),
            Error3 = beamtalk_error:with_hint(
                Error2, <<"Check that the file exists and is readable">>
            ),
            beamtalk_error:raise(Error3)
    end;
'parseFile:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'Yaml'),
    Error1 = beamtalk_error:with_selector(Error0, 'parseFile:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be a String">>),
    beamtalk_error:raise(Error2).

%% @doc Check if Yaml responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method('parse:') -> true;
has_method('parseAll:') -> true;
has_method('generate:') -> true;
has_method('parseFile:') -> true;
has_method(_) -> false.

%%% ============================================================================
%%% Internal Functions — Parsing
%%% ============================================================================

%% @private
%% @doc Parse a YAML binary using yamerl detailed mode.
%%
%% Uses `{detailed_constr, true}` to get typed yamerl node records, which
%% allows reliable type-safe conversion without the string/sequence ambiguity
%% present in simplified mode.
%%
%% Ensures yamerl is started before use. The test runner does not start OTP
%% applications via application:ensure_all_started, so yamerl must be started
%% lazily here.
-spec parse_yaml_string(binary(), atom()) -> [term()].
parse_yaml_string(YamlStr, Selector) ->
    ok = ensure_yamerl_started(Selector),
    try
        Docs = yamerl_constr:string(YamlStr, [{detailed_constr, true}]),
        [unwrap_doc(D) || D <- Docs]
    catch
        error:#{error := #beamtalk_error{}} = E:_ ->
            error(E);
        throw:{yamerl_exception, _} = Ex ->
            Error0 = beamtalk_error:new(parse_error, 'Yaml'),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error2 = beamtalk_error:with_details(Error1, #{reason => Ex}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check that the string is valid YAML">>),
            beamtalk_error:raise(Error3);
        _:Reason ->
            Error0 = beamtalk_error:new(parse_error, 'Yaml'),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error2 = beamtalk_error:with_details(Error1, #{reason => Reason}),
            Error3 = beamtalk_error:with_hint(Error2, <<"Check that the string is valid YAML">>),
            beamtalk_error:raise(Error3)
    end.

%% @private
%% @doc Ensure yamerl is started as an OTP application.
%%
%% yamerl requires its application to be started before use. The beamtalk
%% test runner does not use application:ensure_all_started, so yamerl must
%% be started lazily. This call is idempotent.
%%
%% Takes the calling selector so error messages attribute failures correctly.
-spec ensure_yamerl_started(atom()) -> ok.
ensure_yamerl_started(Selector) ->
    case application:ensure_all_started(yamerl) of
        {ok, _Started} ->
            ok;
        {error, Reason} ->
            Error0 = beamtalk_error:new(parse_error, 'Yaml'),
            Error1 = beamtalk_error:with_selector(Error0, Selector),
            Error2 = beamtalk_error:with_hint(
                Error1, <<"Failed to start yamerl YAML library">>
            ),
            Error3 = beamtalk_error:with_details(Error2, #{reason => Reason}),
            beamtalk_error:raise(Error3)
    end.

%% @private
%% @doc Unwrap a {yamerl_doc, Node} document wrapper.
-spec unwrap_doc(term()) -> term().
unwrap_doc({yamerl_doc, Node}) -> Node;
unwrap_doc(Node) -> Node.

%% @private
%% @doc Convert a yamerl detailed node to a Beamtalk value.
%%
%% yamerl detailed nodes are tagged tuples with type information.
%% This converter handles all standard YAML types. Unsupported node types
%% (anchors, aliases, custom tags) raise a parse_error.
%%
%% The Selector parameter is used for error attribution (parse: vs parseAll:).
-spec convert_node(term(), atom()) -> term().
convert_node({yamerl_str, _, _, _, Charlist}, _Selector) ->
    %% Strings are returned as charlists in yamerl detailed mode.
    %% Use unicode:characters_to_binary/1 (not list_to_binary/1) to safely
    %% handle codepoints > 255, which list_to_binary/1 would crash on.
    unicode:characters_to_binary(Charlist);
convert_node({yamerl_int, _, _, _, Value}, _Selector) ->
    Value;
convert_node({yamerl_float, _, _, _, Value}, _Selector) ->
    Value;
convert_node({yamerl_bool, _, _, _, true}, _Selector) ->
    true;
convert_node({yamerl_bool, _, _, _, false}, _Selector) ->
    false;
convert_node({yamerl_null, _, _, _}, _Selector) ->
    nil;
convert_node({yamerl_seq, _, _, _, Items, _Count}, Selector) ->
    [convert_node(Item, Selector) || Item <- Items];
convert_node({yamerl_map, _, _, _, Pairs}, Selector) ->
    maps:from_list([
        {convert_node(K, Selector), convert_node(V, Selector)}
     || {K, V} <- Pairs
    ]);
convert_node(Other, Selector) ->
    %% Fallback for unsupported node types: anchors, aliases, custom tags.
    %% Log only the node type tag to avoid exposing potentially sensitive values
    %% from YAML files (e.g. API keys, passwords) in log output.
    NodeType = element(1, Other),
    ?LOG_WARNING("beamtalk_yaml: unsupported yamerl node type", #{node_type => NodeType}),
    Error0 = beamtalk_error:new(parse_error, 'Yaml'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_details(Error1, #{node_type => NodeType}),
    Error3 = beamtalk_error:with_hint(
        Error2,
        <<"YAML anchors/aliases and custom tags are not supported">>
    ),
    beamtalk_error:raise(Error3).

%%% ============================================================================
%%% Internal Functions — Generation
%%% ============================================================================

%% @private
%% @doc Prepare a Beamtalk value for YAML encoding.
%%
%% Beamtalk uses `nil` for null; YAML generator expects `null`.
%% Maps with `$beamtalk_class` tags are stripped of metadata.
-spec prepare_for_encode(term()) -> term().
prepare_for_encode(nil) ->
    null;
prepare_for_encode(Map) when is_map(Map) ->
    %% Strip $beamtalk_class tag if present (e.g., from Dictionary)
    Cleaned = maps:remove('$beamtalk_class', Map),
    maps:map(fun(_K, V) -> prepare_for_encode(V) end, Cleaned);
prepare_for_encode(List) when is_list(List) ->
    lists:map(fun prepare_for_encode/1, List);
prepare_for_encode(true) ->
    true;
prepare_for_encode(false) ->
    false;
prepare_for_encode(V) when is_integer(V) -> V;
prepare_for_encode(V) when is_float(V) -> V;
prepare_for_encode(V) when is_binary(V) -> V;
prepare_for_encode(V) when is_atom(V) ->
    %% Convert atoms (symbols) to strings for YAML compatibility
    atom_to_binary(V, utf8);
prepare_for_encode(Other) ->
    Error0 = beamtalk_error:new(type_error, 'Yaml'),
    Error1 = beamtalk_error:with_details(Error0, #{value => Other}),
    Error2 = beamtalk_error:with_hint(
        Error1,
        <<"Only Dictionary, List, String, Integer, Float, Boolean, and nil can be converted to YAML">>
    ),
    beamtalk_error:raise(Error2).

%% @private
%% @doc Generate YAML in flow style from a prepared Erlang term.
%%
%% Flow-style YAML is valid YAML 1.2 and is parseable by yamerl.
%% All string values and map keys are double-quoted for unambiguous round-trip.
-spec generate_flow(term()) -> binary().
generate_flow(null) ->
    <<"null">>;
generate_flow(true) ->
    <<"true">>;
generate_flow(false) ->
    <<"false">>;
generate_flow(N) when is_integer(N) -> integer_to_binary(N);
generate_flow(F) when is_float(F) -> float_to_binary(F, [short]);
generate_flow(B) when is_binary(B) -> yaml_double_quote(B);
generate_flow(Map) when is_map(Map), map_size(Map) =:= 0 -> <<"{}">>;
generate_flow(Map) when is_map(Map) ->
    Pairs = [
        <<(render_key(K))/binary, ": ", (generate_flow(V))/binary>>
     || {K, V} <- maps:to_list(Map)
    ],
    <<"{", (iolist_to_binary(lists:join(<<", ">>, Pairs)))/binary, "}">>;
generate_flow([]) ->
    <<"[]">>;
generate_flow(List) when is_list(List) ->
    Items = [generate_flow(Item) || Item <- List],
    <<"[", (iolist_to_binary(lists:join(<<", ">>, Items)))/binary, "]">>.

%% @private
%% @doc Render a map key for YAML flow output.
%%
%% Typed scalar keys (integer, float, boolean, nil) are emitted unquoted so
%% a parse → generate round-trip preserves key types. String and atom keys
%% are double-quoted to prevent ambiguity with YAML reserved words.
-spec render_key(term()) -> binary().
render_key(nil) -> <<"null">>;
render_key(true) -> <<"true">>;
render_key(false) -> <<"false">>;
render_key(K) when is_integer(K) -> integer_to_binary(K);
render_key(K) when is_float(K) -> float_to_binary(K, [short]);
render_key(K) when is_atom(K) -> yaml_double_quote(atom_to_binary(K, utf8));
render_key(K) when is_binary(K) -> yaml_double_quote(K).

%% @private
%% @doc Wrap a binary in double quotes, escaping internal special characters.
-spec yaml_double_quote(binary()) -> binary().
yaml_double_quote(B) when is_binary(B) ->
    Escaped = escape_yaml_string(B),
    <<$", Escaped/binary, $">>.

%% @private
%% @doc Escape special characters in a YAML double-quoted string value.
-spec escape_yaml_string(binary()) -> binary().
escape_yaml_string(B) ->
    B1 = binary:replace(B, <<"\\">>, <<"\\\\">>, [global]),
    B2 = binary:replace(B1, <<"\"">>, <<"\\\"">>, [global]),
    B3 = binary:replace(B2, <<"\n">>, <<"\\n">>, [global]),
    B4 = binary:replace(B3, <<"\r">>, <<"\\r">>, [global]),
    binary:replace(B4, <<"\t">>, <<"\\t">>, [global]).
