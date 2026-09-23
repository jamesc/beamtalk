%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_wire).

%%% **DDD Context:** Runtime Context

-moduledoc """
The wire codec: a full term walk deciding what crosses a node boundary, and
how (ADR 0126 §5.1, Distribution and Location-Transparent Actors, Phase 3a).

`encode/1` and `decode/1` are **not** a call to `beamtalk_shape_migration:pack/1`
(which accepts one tagged instance and recurses only into fields *declared*
as `Value` types). The walk descends arbitrary Erlang terms — lists, tuples
and maps, including the `'data'`/`'elements'` payload of builtin collections
(`Array`, `Dictionary`, `Set`, `Bag`) — and dispatches on each tagged map's
**runtime class** (`beamtalk_tagged_map:class_of/1`), not a declared field
type, because message arguments are arbitrary runtime terms: `c addAll:
{money1. money2}` must envelope both `Money` instances even though the
`Array`'s element type is not (and need not be) declared as `Money`.

## Dispatch table (ADR 0126 §5.1)

| Term the walk meets | Encoded as |
|---|---|
| `Value`-kind instance | `beamtalk_shape_migration:pack_wire/1` envelope, recursively |
| builtin tagged map (`Array`, `Dictionary`, `Set`, `Bag`, …) | same builtin shape, contents walked (`beamtalk_tagged_map:is_builtin_collection/1`) |
| `Object`-kind instance with `handleScope:` (`HandleScoped`) | rejected: `not_serialisable`, naming the path |
| `Object`-kind instance without `handleScope:` (`Unknown`) | passed as a raw term, unwalked |
| `#beamtalk_error{}` / Exception | passed as a raw term (runtime-owned shape, BT-3528) |
| actor `#beamtalk_object{}` | kept; a local `{registered, Name}` ref is node-qualified to `{registered, Name, node()}` |
| **class object** `#beamtalk_object{}` | rewritten to `{'\$beamtalk_class_ref', ClassName}`, resolved on the *receiving* node's class registry on decode |
| NLR tuple (`?IS_NLR`) | `Value` slot walked; `Token`/`State` passed raw |
| fun (block), pid, port, ref, other | raw term |

The runtime class→tier classification for the two rejectable rows
(`Value`/`HandleScoped`) routes through `beamtalk_shape_migration:field_tier/1`
— the same table BT-3542's Rust↔Erlang conformance test pins against
`sendability.rs` — called directly with a runtime class atom (exactly as
`beamtalk_shape_migration:pack/1` already calls it with a *declared field
type* atom; the function is agnostic to which). No second tier table.

Builtin-collection-vs-opaque-Object is a *different* axis (representation,
not sendability) — `beamtalk_tagged_map:is_builtin_collection/1` decides it,
shared with `beamtalk_inspector:is_collection/1`'s display-drilling use of
the same distinction.

## Depth cap

Bounded by `beamtalk_shape_migration:max_pack_depth/0` — the same
`?MAX_PACK_DEPTH` cap `pack/1`'s own nested-`Value` recursion uses (ADR 0126
§5.1: "the same depth cap as pack/1"). `pack_wire/1`'s own internal
recursion (for a genuine `Value`-kind instance this walk delegates to) is
bounded by its own copy of that same cap independently — the two walks are
not depth-coupled to each other, only bounded by the same number.

## Explicitly out of scope for this module (ADR 0126 Phase 3a / BT-3600)

Wiring this codec into `beamtalk_actor`'s `handle_call`/`handle_cast`
dispatch, the `'\$beamtalk_wire'` tagged-message prelude, callee-side
encode-failure replies, and any two-node integration test — all BT-3601, a
separate, later issue this one is a prerequisite for.

## Known, inherited scope gap

Like `pack/1` (BT-3542's documented gap #1), a field/element whose *own*
type is not itself a `Value`-kind class is not composed generically — a
user `Value` class's untyped (`Unknown`-tier) field holding a `List` of
*other* `Value` instances is not individually enveloped by `pack_wire/1`'s
declared-field walk. `beamtalk_wire`'s own walk fixes this for the two cases
ADR 0126 §5.1 calls out explicitly (a *builtin* collection's contents, and a
`Value` instance met directly), not for every possible nesting shape.
""".

-include("beamtalk.hrl").

-export([encode/1, decode/1]).

%%====================================================================
%% Public API
%%====================================================================

-doc """
Walk `Term`, encoding it for a remote send per the ADR 0126 §5.1 table.

Returns `{error, #beamtalk_error{kind = not_serialisable}}`, naming the
path, the first time the walk meets a `HandleScoped` instance or exceeds the
recursion depth cap — encode failures never crash the caller (ADR 0126 §5.1:
"encode failures... never crash the callee", and on the sender side for a
request payload, per §5.2's "Request direction").
""".
-spec encode(term()) -> {ok, term()} | {error, #beamtalk_error{}}.
encode(Term) ->
    encode_at(Term, [], 0).

-doc """
The inverse walk: resolves each `{'\$beamtalk_shape_ref'...}`-free envelope
via `beamtalk_shape_migration:unpack_strict/1` (per envelope, per ADR 0125
§3.4) and each `{'\$beamtalk_class_ref', ClassName}` via
`beamtalk_class_registry:resolve_class_object/1` on **this** (the receiving)
node.
""".
-spec decode(term()) -> {ok, term()} | {error, #beamtalk_error{}}.
decode(Term) ->
    decode_at(Term, [], 0).

%%====================================================================
%% encode/1 — dispatch
%%====================================================================

-spec encode_at(term(), [term()], non_neg_integer()) -> {ok, term()} | {error, #beamtalk_error{}}.
encode_at(Term, Path, Depth) ->
    case Depth >= beamtalk_shape_migration:max_pack_depth() of
        true -> {error, depth_exceeded_error(Path)};
        false -> encode_term(Term, Path, Depth)
    end.

-spec encode_term(term(), [term()], non_neg_integer()) -> {ok, term()} | {error, #beamtalk_error{}}.
encode_term(Term, Path, Depth) when is_map(Term) ->
    encode_map(Term, Path, Depth);
encode_term(Term, Path, Depth) when is_list(Term) ->
    encode_list(Term, Path, Depth);
encode_term(Term, Path, Depth) when ?IS_NLR(Term) ->
    encode_nlr(Term, Path, Depth);
encode_term(#beamtalk_object{} = Obj, Path, _Depth) ->
    encode_beamtalk_object(Obj, Path);
encode_term(#beamtalk_error{} = Err, _Path, _Depth) ->
    %% Exception / #beamtalk_error{} — a raw term (runtime-owned shape,
    %% BT-3528), not walked element-by-element.
    {ok, Err};
encode_term(Term, Path, Depth) when is_tuple(Term) ->
    encode_tuple(Term, Path, Depth);
encode_term(Term, _Path, _Depth) ->
    %% fun (block), pid, port, ref, number, atom/symbol, binary/string,
    %% other — raw term.
    {ok, Term}.

%%====================================================================
%% encode/1 — lists and tuples
%%====================================================================

-spec encode_list(list(), [term()], non_neg_integer()) -> {ok, list()} | {error, #beamtalk_error{}}.
encode_list([], _Path, _Depth) ->
    {ok, []};
encode_list(List, Path, Depth) ->
    encode_list(List, Path, Depth, 0, []).

-spec encode_list(term(), [term()], non_neg_integer(), non_neg_integer(), list()) ->
    {ok, term()} | {error, #beamtalk_error{}}.
encode_list([], _Path, _Depth, _Index, Acc) ->
    {ok, lists:reverse(Acc)};
encode_list([H | T], Path, Depth, Index, Acc) when is_list(T) ->
    case encode_at(H, [{index, Index} | Path], Depth + 1) of
        {ok, EncH} -> encode_list(T, Path, Depth, Index + 1, [EncH | Acc]);
        {error, _} = Err -> Err
    end;
encode_list([H | T], Path, Depth, Index, Acc) ->
    %% Improper tail (rare — a foreign FFI result, never compiler-generated
    %% Beamtalk code): encode both parts rather than crashing, splice back.
    case encode_at(H, [{index, Index} | Path], Depth + 1) of
        {ok, EncH} ->
            case encode_at(T, [{index, Index + 1} | Path], Depth + 1) of
                {ok, EncT} -> {ok, lists:reverse(Acc, [EncH | EncT])};
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.

-spec encode_tuple(tuple(), [term()], non_neg_integer()) ->
    {ok, tuple()} | {error, #beamtalk_error{}}.
encode_tuple(Tuple, Path, Depth) ->
    case encode_list(tuple_to_list(Tuple), Path, Depth) of
        {ok, EncList} -> {ok, list_to_tuple(EncList)};
        {error, _} = Err -> Err
    end.

%%====================================================================
%% encode/1 — maps
%%====================================================================

-spec encode_map(map(), [term()], non_neg_integer()) -> {ok, map()} | {error, #beamtalk_error{}}.
encode_map(Map, Path, Depth) ->
    case beamtalk_tagged_map:class_of(Map) of
        undefined ->
            %% Untagged raw map == Dictionary (ADR 0090) — walk both keys
            %% and values.
            encode_dictionary_entries(Map, Path, Depth);
        Class ->
            case beamtalk_tagged_map:is_builtin_collection(Map) of
                true -> encode_builtin_collection(Map, Path, Depth);
                false -> encode_classified_instance(Class, Map, Path, Depth)
            end
    end.

-spec encode_dictionary_entries(map(), [term()], non_neg_integer()) ->
    {ok, map()} | {error, #beamtalk_error{}}.
encode_dictionary_entries(Map, Path, Depth) ->
    encode_dict_entries(maps:to_list(Map), Path, Depth, #{}).

-spec encode_dict_entries([{term(), term()}], [term()], non_neg_integer(), map()) ->
    {ok, map()} | {error, #beamtalk_error{}}.
encode_dict_entries([], _Path, _Depth, Acc) ->
    {ok, Acc};
encode_dict_entries([{K, V} | Rest], Path, Depth, Acc) ->
    case encode_at(K, [{key, K} | Path], Depth + 1) of
        {ok, EncK} ->
            case encode_at(V, [{key, K} | Path], Depth + 1) of
                {ok, EncV} -> encode_dict_entries(Rest, Path, Depth, Acc#{EncK => EncV});
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.

%% A builtin tagged map (Array/Set/Dictionary(never reached here, untagged)/
%% Bag): walk only the VALUES of its non-internal fields (their key atoms —
%% 'data', 'elements' — are structural, never re-encoded), preserving the
%% class tag and any other internal fields verbatim. Array's 'data' is
%% itself a plain (untagged) map, so recursing into it lands back in
%% encode_dictionary_entries/3 above, walking its index->value entries —
%% exactly the "Array's 'data'... walked" row of the ADR table.
-spec encode_builtin_collection(map(), [term()], non_neg_integer()) ->
    {ok, map()} | {error, #beamtalk_error{}}.
encode_builtin_collection(Map, Path, Depth) ->
    Internal = beamtalk_tagged_map:internal_fields(),
    {InternalPairs, UserPairs} = lists:partition(
        fun({K, _}) -> lists:member(K, Internal) end, maps:to_list(Map)
    ),
    encode_field_values(UserPairs, Path, Depth, maps:from_list(InternalPairs)).

-spec encode_field_values([{atom(), term()}], [term()], non_neg_integer(), map()) ->
    {ok, map()} | {error, #beamtalk_error{}}.
encode_field_values([], _Path, _Depth, Acc) ->
    {ok, Acc};
encode_field_values([{Field, Value} | Rest], Path, Depth, Acc) ->
    case encode_at(Value, [{field, Field} | Path], Depth + 1) of
        {ok, EncV} -> encode_field_values(Rest, Path, Depth, Acc#{Field => EncV});
        {error, _} = Err -> Err
    end.

%% A genuine user Value/Object/Actor-kind tagged instance (not a builtin
%% collection) — dispatch by beamtalk_shape_migration:field_tier/1, called
%% directly with the runtime class atom (ADR 0126 §5.1/§5.4).
-spec encode_classified_instance(atom(), map(), [term()], non_neg_integer()) ->
    {ok, term()} | {error, #beamtalk_error{}}.
encode_classified_instance(Class, Map, Path, _Depth) ->
    case beamtalk_shape_migration:field_tier(Class) of
        value_nested ->
            beamtalk_shape_migration:pack_wire(Map);
        handle_scoped ->
            {error, not_serialisable_error(Class, Path)};
        sendable_ref ->
            %% Defensive: actor state never reaches the wire walk as a
            %% tagged map (actor references are #beamtalk_object{} records,
            %% handled by encode_beamtalk_object/2) — but if it ever did,
            %% §5.4's "pids are fine on the wire" applies here too.
            {ok, Map};
        passthrough ->
            %% Object-kind instance without handleScope: (Unknown tier) —
            %% passed as a raw term, unwalked.
            {ok, Map}
    end.

%%====================================================================
%% encode/1 — actor refs and class objects
%%====================================================================

-spec encode_beamtalk_object(#beamtalk_object{}, [term()]) ->
    {ok, #beamtalk_object{} | {'$beamtalk_class_ref', atom()}}.
encode_beamtalk_object(#beamtalk_object{pid = Pid} = Obj, _Path) ->
    case beamtalk_class_registry:is_class_object(Obj) of
        true ->
            %% Class object — rewritten to a by-name reference, resolved on
            %% the receiving node's own class registry on decode (ADR 0126
            %% §5.1/§5.5): a class object that crosses a node is that node's
            %% class of the same name, never a remote class process.
            ClassName = beamtalk_object_class:class_name(Pid),
            {ok, {'$beamtalk_class_ref', ClassName}};
        false ->
            {ok, Obj#beamtalk_object{pid = encode_actor_pid(Pid)}}
    end.

%% A local `{registered, Name}` ref (ADR 0079) is node-qualified with the
%% sender's own node, so the receiver knows which node's registry it names
%% (ADR 0126 §3/§5.1). A ref already node-qualified (relayed from elsewhere)
%% and an ordinary pid (already node-qualified natively by BEAM) pass
%% through unchanged.
-spec encode_actor_pid(pid() | {registered, atom()} | {registered, atom(), node()}) ->
    pid() | {registered, atom(), node()}.
encode_actor_pid({registered, Name}) when is_atom(Name) ->
    {registered, Name, node()};
encode_actor_pid(Pid) ->
    Pid.

%%====================================================================
%% encode/1 — NLR relay
%%====================================================================

-spec encode_nlr(tuple(), [term()], non_neg_integer()) ->
    {ok, tuple()} | {error, #beamtalk_error{}}.
encode_nlr({'$bt_nlr', Token, Value}, Path, Depth) ->
    case encode_at(Value, [nlr_value | Path], Depth + 1) of
        {ok, EncValue} -> {ok, {'$bt_nlr', Token, EncValue}};
        {error, _} = Err -> Err
    end;
encode_nlr({'$bt_nlr', Token, Value, State}, Path, Depth) ->
    %% Value is walked; State (the defining method's actor state) and Token
    %% are passed raw — State returns to the node that owns it (ADR 0126
    %% §5.1/§5.5).
    case encode_at(Value, [nlr_value | Path], Depth + 1) of
        {ok, EncValue} -> {ok, {'$bt_nlr', Token, EncValue, State}};
        {error, _} = Err -> Err
    end.

%%====================================================================
%% decode/1 — dispatch
%%====================================================================

-spec decode_at(term(), [term()], non_neg_integer()) -> {ok, term()} | {error, #beamtalk_error{}}.
decode_at(Term, Path, Depth) ->
    case Depth >= beamtalk_shape_migration:max_pack_depth() of
        true -> {error, depth_exceeded_error(Path)};
        false -> decode_term(Term, Path, Depth)
    end.

-spec decode_term(term(), [term()], non_neg_integer()) -> {ok, term()} | {error, #beamtalk_error{}}.
decode_term(Term, Path, Depth) when is_map(Term) ->
    decode_map(Term, Path, Depth);
decode_term(Term, Path, Depth) when is_list(Term) ->
    decode_list(Term, Path, Depth);
decode_term({beamtalk_shape, Class, ShapeVersion, Fields}, _Path, _Depth) when
    is_atom(Class), is_integer(ShapeVersion), is_map(Fields)
->
    beamtalk_shape_migration:unpack_strict({beamtalk_shape, Class, ShapeVersion, Fields});
decode_term({'$beamtalk_class_ref', ClassName}, _Path, _Depth) when is_atom(ClassName) ->
    decode_class_ref(ClassName);
decode_term(Term, Path, Depth) when ?IS_NLR(Term) ->
    decode_nlr(Term, Path, Depth);
decode_term(#beamtalk_object{} = Obj, _Path, _Depth) ->
    {ok, Obj};
decode_term(#beamtalk_error{} = Err, _Path, _Depth) ->
    {ok, Err};
decode_term(Term, Path, Depth) when is_tuple(Term) ->
    decode_tuple(Term, Path, Depth);
decode_term(Term, _Path, _Depth) ->
    {ok, Term}.

%%====================================================================
%% decode/1 — lists and tuples
%%====================================================================

-spec decode_list(list(), [term()], non_neg_integer()) -> {ok, list()} | {error, #beamtalk_error{}}.
decode_list([], _Path, _Depth) ->
    {ok, []};
decode_list(List, Path, Depth) ->
    decode_list(List, Path, Depth, 0, []).

-spec decode_list(term(), [term()], non_neg_integer(), non_neg_integer(), list()) ->
    {ok, term()} | {error, #beamtalk_error{}}.
decode_list([], _Path, _Depth, _Index, Acc) ->
    {ok, lists:reverse(Acc)};
decode_list([H | T], Path, Depth, Index, Acc) when is_list(T) ->
    case decode_at(H, [{index, Index} | Path], Depth + 1) of
        {ok, DecH} -> decode_list(T, Path, Depth, Index + 1, [DecH | Acc]);
        {error, _} = Err -> Err
    end;
decode_list([H | T], Path, Depth, Index, Acc) ->
    case decode_at(H, [{index, Index} | Path], Depth + 1) of
        {ok, DecH} ->
            case decode_at(T, [{index, Index + 1} | Path], Depth + 1) of
                {ok, DecT} -> {ok, lists:reverse(Acc, [DecH | DecT])};
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.

-spec decode_tuple(tuple(), [term()], non_neg_integer()) ->
    {ok, tuple()} | {error, #beamtalk_error{}}.
decode_tuple(Tuple, Path, Depth) ->
    case decode_list(tuple_to_list(Tuple), Path, Depth) of
        {ok, DecList} -> {ok, list_to_tuple(DecList)};
        {error, _} = Err -> Err
    end.

%%====================================================================
%% decode/1 — maps
%%====================================================================

-spec decode_map(map(), [term()], non_neg_integer()) -> {ok, map()} | {error, #beamtalk_error{}}.
decode_map(Map, Path, Depth) ->
    case beamtalk_tagged_map:class_of(Map) of
        undefined ->
            decode_dictionary_entries(Map, Path, Depth);
        _Class ->
            case beamtalk_tagged_map:is_builtin_collection(Map) of
                true -> decode_builtin_collection(Map, Path, Depth);
                false -> {ok, Map}
            end
    end.

-spec decode_dictionary_entries(map(), [term()], non_neg_integer()) ->
    {ok, map()} | {error, #beamtalk_error{}}.
decode_dictionary_entries(Map, Path, Depth) ->
    decode_dict_entries(maps:to_list(Map), Path, Depth, #{}).

-spec decode_dict_entries([{term(), term()}], [term()], non_neg_integer(), map()) ->
    {ok, map()} | {error, #beamtalk_error{}}.
decode_dict_entries([], _Path, _Depth, Acc) ->
    {ok, Acc};
decode_dict_entries([{K, V} | Rest], Path, Depth, Acc) ->
    case decode_at(K, [{key, K} | Path], Depth + 1) of
        {ok, DecK} ->
            case decode_at(V, [{key, K} | Path], Depth + 1) of
                {ok, DecV} -> decode_dict_entries(Rest, Path, Depth, Acc#{DecK => DecV});
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.

-spec decode_builtin_collection(map(), [term()], non_neg_integer()) ->
    {ok, map()} | {error, #beamtalk_error{}}.
decode_builtin_collection(Map, Path, Depth) ->
    Internal = beamtalk_tagged_map:internal_fields(),
    {InternalPairs, UserPairs} = lists:partition(
        fun({K, _}) -> lists:member(K, Internal) end, maps:to_list(Map)
    ),
    decode_field_values(UserPairs, Path, Depth, maps:from_list(InternalPairs)).

-spec decode_field_values([{atom(), term()}], [term()], non_neg_integer(), map()) ->
    {ok, map()} | {error, #beamtalk_error{}}.
decode_field_values([], _Path, _Depth, Acc) ->
    {ok, Acc};
decode_field_values([{Field, Value} | Rest], Path, Depth, Acc) ->
    case decode_at(Value, [{field, Field} | Path], Depth + 1) of
        {ok, DecV} -> decode_field_values(Rest, Path, Depth, Acc#{Field => DecV});
        {error, _} = Err -> Err
    end.

%%====================================================================
%% decode/1 — class ref resolution and NLR relay
%%====================================================================

-spec decode_class_ref(atom()) -> {ok, #beamtalk_object{}} | {error, #beamtalk_error{}}.
decode_class_ref(ClassName) ->
    case beamtalk_class_registry:resolve_class_object(ClassName) of
        undefined -> {error, class_ref_not_found_error(ClassName)};
        ClassObj -> {ok, ClassObj}
    end.

-spec decode_nlr(tuple(), [term()], non_neg_integer()) ->
    {ok, tuple()} | {error, #beamtalk_error{}}.
decode_nlr({'$bt_nlr', Token, Value}, Path, Depth) ->
    case decode_at(Value, [nlr_value | Path], Depth + 1) of
        {ok, DecValue} -> {ok, {'$bt_nlr', Token, DecValue}};
        {error, _} = Err -> Err
    end;
decode_nlr({'$bt_nlr', Token, Value, State}, Path, Depth) ->
    case decode_at(Value, [nlr_value | Path], Depth + 1) of
        {ok, DecValue} -> {ok, {'$bt_nlr', Token, DecValue, State}};
        {error, _} = Err -> Err
    end.

%%====================================================================
%% Errors
%%====================================================================

-spec not_serialisable_error(atom(), [term()]) -> #beamtalk_error{}.
not_serialisable_error(Class, Path) ->
    %% unicode:characters_to_binary/1, not iolist_to_binary/1 — the message
    %% may contain non-ASCII path punctuation from a Symbol field name.
    PathBin = format_path(Path),
    Hint = unicode:characters_to_binary(
        io_lib:format(
            "~s at ~s is a node-scoped handle (HandleScoped) — not serialisable across a node boundary",
            [Class, PathBin]
        )
    ),
    beamtalk_error:with_details(
        beamtalk_error:with_hint(
            beamtalk_error:new(not_serialisable, Class),
            Hint
        ),
        #{path => PathBin}
    ).

-spec depth_exceeded_error([term()]) -> #beamtalk_error{}.
depth_exceeded_error(Path) ->
    PathBin = format_path(Path),
    Hint = unicode:characters_to_binary(
        io_lib:format("wire walk exceeded the recursion depth limit at ~s", [PathBin])
    ),
    beamtalk_error:with_hint(beamtalk_error:new(not_serialisable, 'Object'), Hint).

-spec class_ref_not_found_error(atom()) -> #beamtalk_error{}.
class_ref_not_found_error(ClassName) ->
    beamtalk_error:with_hint(
        beamtalk_error:new(class_not_found, ClassName),
        unicode:characters_to_binary(
            io_lib:format(
                "class '~s' is not registered on this node — a class object rewritten "
                "on the sending node resolves by name on the receiver",
                [ClassName]
            )
        )
    ).

%% Renders the accumulated (reverse-order) path of field/index/key/nlr-value
%% steps into a human-readable trail for an error hint, e.g.
%% "<value>.data[2].balance". Purely diagnostic — never round-tripped.
-spec format_path([term()]) -> binary().
format_path(RevPath) ->
    Segments = lists:reverse(RevPath),
    iolist_to_binary(["<value>" | [format_segment(S) || S <- Segments]]).

-spec format_segment(term()) -> iolist().
format_segment({field, Field}) when is_atom(Field) ->
    io_lib:format(".~s", [Field]);
format_segment({index, Index}) when is_integer(Index) ->
    io_lib:format("[~p]", [Index]);
format_segment({key, Key}) ->
    io_lib:format("{~p}", [Key]);
format_segment(nlr_value) ->
    ".<nlr value>";
format_segment(Other) ->
    io_lib:format("[~p]", [Other]).
