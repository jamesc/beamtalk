%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_browse).

%%% **DDD Context:** REPL Session Context (System Browser bridge)

-moduledoc """
Op handlers for the System Browser browse facade (ADR 0095, BT-2488).

The LiveView IDE's System Browser (ADR 0017 Phase 3, epic BT-2482) renders the
four-pane Smalltalk navigator — *classes → protocols → selectors → method
source* plus a class-definition pane. These four read-only term-ops populate
those panes against a live workspace, sourced **static-first / live-augmented**
(ADR 0024) with image/disk divergence carried as explicit per-row metadata
(never silently merged):

| Op | Pane | Term shape |
|----|------|------------|
| `browse-classes` | class tree | `{value, [ClassRow]}` |
| `browse-protocols` | protocol + selector tree | `{value, ProtocolTree}` |
| `browse-method-source` | method source pane | `{value, MethodSource}` |
| `browse-class-definition` | class-definition pane | `{value, ClassDefinition}` |

## Term contract (BT-2399)

Every op returns a `{value, JsonValue}` tagged term — the rows are already a
wire-shaped JSON value (maps/lists of binaries, integers, booleans, `null`), so
`beamtalk_repl_ops:encode/2` passes them through the identity encoder (no
`term_to_json` inspect-string round-trip). Dist-attached clients (Phoenix
LiveView, MCP) consume the live terms directly over distribution; JSON appears
only at the WebSocket edge. Validation failures return
`{error, #beamtalk_error{}}` (the structured error contract).

## Read-only / no-user-code guarantee (ADR 0091 Observer gate)

All four ops are **pure reflection**: class metadata (`get_doc`, `is_sealed`,
`is_abstract`, `is_internal`, `instance_variables`, `field_defaults`), xref rows
(`beamtalk_xref:defined_selectors/2`, `method_info/3`), and stored source text
(`{method, Sel}` / `{class_method, Sel}` → `__source__`). None sends a
user-defined method to a value (no `printOn:` / `displayString` / `printString`),
so the browse data source is safe to grant the **Observer** role (ADR 0091
Decision 4). This is asserted, not assumed (see the tests).

## Divergence: `origin` and `disk_differs`

Image/disk divergence is surfaced, never merged (Principle #5):

* `origin :: <<"both">> | <<"static">> | <<"runtime">>` — the row-level analog of
  ADR 0024's `TierSource`. `both` = present on disk and in the image; `static` =
  on disk, not loaded; `runtime` = in the image with no disk source (runtime-only
  method, live `>>`-added selector, `ClassBuilder` class).
* `disk_differs` (ops 3/4) — the finer *both-but-changed* signal: `true` when the
  live (patched) source differs from the static/disk source (an unflushed `>>`
  patch, ADR 0082). `null` when there is no static source to compare.
* `source_status` (op 2/3) — the xref tag verbatim: `indexed` | `synthetic` |
  `unindexed_runtime_fun`. How the browser knows a selector has no openable
  source.

See `docs/ADR/0095-system-browser-data-source.md`.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([handle/4, handle_term/4, describe_ops/0]).

-doc """
Handle a browse op for the WebSocket transport — encodes the term result to
JSON at the edge. Dist-attached clients call `handle_term/4` directly.
""".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(Op, Params, Msg, SessionPid), Msg).

-doc """
Term-returning handler for the four browse ops (ADR 0095). Returns
`{value, JsonValue}` on success or `{error, #beamtalk_error{}}` on a validation
failure (unknown class, bad side, missing selector).
""".
-spec handle_term(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) ->
    beamtalk_repl_ops:op_result().
handle_term(<<"browse-classes">>, _Params, _Msg, _SessionPid) ->
    {value, browse_classes()};
handle_term(<<"browse-protocols">>, Params, _Msg, _SessionPid) ->
    case validate_class_side(Params) of
        {ok, {ClassName, ClassSide}} ->
            browse_protocols(ClassName, ClassSide);
        {error, Reason} ->
            arg_error(<<"browse-protocols">>, Reason)
    end;
handle_term(<<"browse-method-source">>, Params, _Msg, _SessionPid) ->
    case validate_class_side_selector(Params) of
        {ok, {ClassName, ClassSide, Selector}} ->
            browse_method_source(ClassName, ClassSide, Selector);
        {error, Reason} ->
            arg_error(<<"browse-method-source">>, Reason)
    end;
handle_term(<<"browse-class-definition">>, Params, _Msg, _SessionPid) ->
    case validate_class(Params) of
        {ok, ClassName} ->
            browse_class_definition(ClassName);
        {error, Reason} ->
            arg_error(<<"browse-class-definition">>, Reason)
    end.

-doc "Advertise the four browse ops in `describe`.".
-spec describe_ops() -> map().
describe_ops() ->
    #{
        <<"browse-classes">> => #{<<"params">> => []},
        <<"browse-protocols">> => #{<<"params">> => [<<"class">>, <<"side">>]},
        <<"browse-method-source">> => #{
            <<"params">> => [<<"class">>, <<"side">>, <<"selector">>]
        },
        <<"browse-class-definition">> => #{<<"params">> => [<<"class">>]}
    }.

%%% ====================================================================
%%% Op 1 — browse-classes
%%% ====================================================================

%% Lists every class in scope with the edges the class tree needs: superclass
%% (hierarchy) + category (the Smalltalk class-category group). Extends the
%% list-classes field set with `category` and `origin`; it does not replace
%% list-classes (ADR 0095). Reuses `beamtalk_runtime_api:all_classes/0` and the
%% same per-class reflection list-classes uses — no new runtime bookkeeping.
-spec browse_classes() -> [map()].
browse_classes() ->
    ClassPids =
        try
            beamtalk_runtime_api:all_classes()
        catch
            _:_ -> []
        end,
    Rows = lists:filtermap(fun class_row/1, ClassPids),
    lists:sort(fun(A, B) -> maps:get(<<"name">>, A) =< maps:get(<<"name">>, B) end, Rows).

-spec class_row(pid()) -> {true, map()} | false.
class_row(Pid) ->
    try
        Name = beamtalk_runtime_api:class_name(Pid),
        Super = beamtalk_runtime_api:superclass(Pid),
        ModName = beamtalk_runtime_api:module_name(Pid),
        SourceFile = source_file_of(ModName),
        {true, #{
            <<"name">> => atom_to_binary(Name, utf8),
            <<"superclass">> => atom_or_null(Super),
            <<"category">> => category_of(ModName),
            <<"comment">> => first_line(class_doc(Pid)),
            <<"sealed">> => safe_bool(fun() -> beamtalk_runtime_api:is_sealed(Pid) end),
            <<"abstract">> => safe_bool(fun() -> beamtalk_runtime_api:is_abstract(Pid) end),
            <<"internal">> => safe_bool(fun() -> beamtalk_runtime_api:is_internal(Pid) end),
            <<"source_file">> => SourceFile,
            <<"origin">> => origin_of(SourceFile)
        }}
    catch
        exit:{noproc, _} ->
            false;
        exit:{timeout, _} ->
            false;
        Class:Reason ->
            ?LOG_WARNING(
                "browse-classes: skipping class ~p: ~p:~p",
                [Pid, Class, Reason],
                #{domain => [beamtalk, runtime]}
            ),
            false
    end.

%%% ====================================================================
%%% Op 2 — browse-protocols
%%% ====================================================================

%% Given {class, side}, returns the class's selectors grouped by protocol
%% (method category). Backed by the xref index (ADR 0087): `defined_selectors/2`
%% for the selector set, `method_info/3` for each selector's line / source_status
%% / provenance. Protocols and selectors are sorted for stable tree order.
-spec browse_protocols(atom(), boolean()) -> beamtalk_repl_ops:op_result().
browse_protocols(ClassName, ClassSide) ->
    Selectors = beamtalk_xref:defined_selectors(ClassName, ClassSide),
    SourceFile = source_file_for_class(ClassName),
    SelectorRows = [selector_row(ClassName, ClassSide, Sel, SourceFile) || Sel <- Selectors],
    Grouped = group_by_protocol(SelectorRows),
    {value, #{
        <<"class">> => atom_to_binary(ClassName, utf8),
        <<"side">> => side_to_binary(ClassSide),
        <<"protocols">> => Grouped
    }}.

-spec selector_row(atom(), boolean(), atom(), binary() | null) -> map().
selector_row(ClassName, ClassSide, Selector, SourceFile) ->
    Info = beamtalk_xref:method_info(ClassName, ClassSide, Selector),
    {Line, SourceStatus, Provenance} = info_fields(Info),
    #{
        <<"selector">> => atom_to_binary(Selector, utf8),
        <<"line">> => Line,
        <<"source_status">> => atom_to_binary(SourceStatus, utf8),
        <<"origin">> => origin_for_provenance(Provenance, SourceFile),
        %% carried internally for grouping; stripped before emit
        '__protocol__' => protocol_for_provenance(Provenance)
    }.

-spec info_fields(beamtalk_xref:method_info() | undefined) ->
    {pos_integer() | null, beamtalk_xref:source_status(), beamtalk_xref:provenance()}.
info_fields(#{line := Line, source_status := SS, provenance := Prov}) ->
    {Line, SS, Prov};
info_fields(_) ->
    %% No xref row: a method known to the class but not indexed (sourceless
    %% runtime fun). Mark it as such so the browser shows "no source".
    {null, unindexed_runtime_fun, put_method}.

%% Group selector rows into sorted protocol buckets. The protocol of a selector
%% is derived from its xref provenance (ADR 0095): class_body / extension map to
%% concrete buckets, everything else falls into "as yet unclassified" (Pharo
%% convention). Selectors within a protocol are sorted; protocols are sorted.
-spec group_by_protocol([map()]) -> [map()].
group_by_protocol(SelectorRows) ->
    Folded = lists:foldl(
        fun(Row, Acc) ->
            {Protocol, Clean} = take_protocol(Row),
            maps:update_with(Protocol, fun(Rs) -> [Clean | Rs] end, [Clean], Acc)
        end,
        #{},
        SelectorRows
    ),
    Names = lists:sort(maps:keys(Folded)),
    [
        #{
            <<"name">> => Name,
            <<"selectors">> => lists:sort(
                fun(A, B) -> maps:get(<<"selector">>, A) =< maps:get(<<"selector">>, B) end,
                maps:get(Name, Folded)
            )
        }
     || Name <- Names
    ].

-spec take_protocol(map()) -> {binary(), map()}.
take_protocol(Row) ->
    Protocol = maps:get('__protocol__', Row),
    {Protocol, maps:remove('__protocol__', Row)}.

%%% ====================================================================
%%% Op 3 — browse-method-source
%%% ====================================================================

%% Fetches one method's source, image-accurate. Source comes from the live class
%% object's stored method source (`{method, Sel}` / `{class_method, Sel}` →
%% `__source__`) — the patch-aware per-method text (BT-2196). source_status from
%% xref. `disk_differs` compares the image body against the static/disk class
%% source recorded at load time (`beamtalk_workspace_meta:get_class_source/1`).
-spec browse_method_source(atom(), boolean(), atom()) -> beamtalk_repl_ops:op_result().
browse_method_source(ClassName, ClassSide, Selector) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            not_found_error(<<"browse-method-source">>, ClassName);
        ClassPid ->
            {Line, SourceStatus, Provenance} = info_fields(
                beamtalk_xref:method_info(ClassName, ClassSide, Selector)
            ),
            Source = method_source_text(ClassPid, ClassSide, Selector, SourceStatus),
            SourceFile = source_file_for_class(ClassName),
            {value, #{
                <<"class">> => atom_to_binary(ClassName, utf8),
                <<"side">> => side_to_binary(ClassSide),
                <<"selector">> => atom_to_binary(Selector, utf8),
                <<"source">> => Source,
                <<"line">> => Line,
                <<"source_status">> => atom_to_binary(SourceStatus, utf8),
                <<"origin">> => origin_for_provenance(Provenance, SourceFile),
                <<"disk_differs">> => disk_differs(ClassName, Source)
            }}
    end.

%% Sourceless runtime methods (unindexed_runtime_fun): source is null, the
%% source_status says why — the browser shows "no source (runtime method)"
%% rather than an empty pane (ADR 0095).
-spec method_source_text(pid(), boolean(), atom(), beamtalk_xref:source_status()) ->
    binary() | null.
method_source_text(_ClassPid, _ClassSide, _Selector, unindexed_runtime_fun) ->
    null;
method_source_text(ClassPid, ClassSide, Selector, _SourceStatus) ->
    Call =
        case ClassSide of
            true -> {class_method, Selector};
            false -> {method, Selector}
        end,
    case safe_class_call(ClassPid, Call) of
        #{'__source__' := Src} when is_binary(Src), byte_size(Src) > 0 -> Src;
        _ -> null
    end.

%%% ====================================================================
%%% Op 4 — browse-class-definition
%%% ====================================================================

%% The class-definition pane: class header, state slots, and full comment. State
%% is field reflection (ADR 0035) — names + default expressions, no user code.
%% The definition string is the class skeleton (`Super subclass: Name` + state
%% slots), method bodies excluded (Pharo convention). File-less (ClassBuilder)
%% classes have null definition + origin = runtime.
-spec browse_class_definition(atom()) -> beamtalk_repl_ops:op_result().
browse_class_definition(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            not_found_error(<<"browse-class-definition">>, ClassName);
        ClassPid ->
            Super = beamtalk_runtime_api:superclass(ClassPid),
            ModName = beamtalk_runtime_api:module_name(ClassPid),
            SourceFile = source_file_of(ModName),
            State = state_slots(ClassPid),
            Definition = class_definition_text(ClassName, Super, State, SourceFile),
            {value, #{
                <<"class">> => atom_to_binary(ClassName, utf8),
                <<"superclass">> => atom_or_null(Super),
                <<"category">> => category_of(ModName),
                <<"definition">> => Definition,
                <<"state">> => State,
                <<"comment">> => class_doc(ClassPid),
                <<"origin">> => origin_of(SourceFile),
                %% The definition pane renders a *synthesized* class skeleton
                %% (header + state slots), not the verbatim on-disk class source
                %% — so a substring diff against the disk store would be unsound
                %% (false positives on formatting/syntax). With no separate
                %% image-side class-source snapshot to diff, the honest answer is
                %% `null` ("nothing to compare", ADR 0095). A precise class-level
                %% diff is future work; the per-method `disk_differs` (op 3) is
                %% the live-patch signal the browser relies on today.
                <<"disk_differs">> => class_definition_disk_differs(SourceFile)
            }}
    end.

%% Class-definition divergence is not soundly computable from the synthesized
%% skeleton (see the call site): `null` when there is no disk source, and `null`
%% (not-computed) when there is — never a misleading false positive.
-spec class_definition_disk_differs(binary() | null) -> null.
class_definition_disk_differs(_SourceFile) ->
    null.

%% State slots: field names (ADR 0035 reflection) paired with their default
%% expression text where the live class object carries one. Field reflection
%% only — no user code run.
-spec state_slots(pid()) -> [map()].
state_slots(ClassPid) ->
    Fields = safe_class_call(ClassPid, instance_variables),
    Defaults = safe_class_call(ClassPid, field_defaults),
    FieldList =
        case Fields of
            L when is_list(L) -> L;
            _ -> []
        end,
    DefaultsMap =
        case Defaults of
            M when is_map(M) -> M;
            _ -> #{}
        end,
    [
        #{
            <<"name">> => atom_to_binary(F, utf8),
            <<"default">> => default_text(maps:get(F, DefaultsMap, undefined))
        }
     || F <- FieldList
    ].

-spec default_text(term()) -> binary() | null.
default_text(undefined) ->
    null;
default_text(Value) ->
    %% Field reflection (ADR 0035): render the stored default term as text via
    %% io_lib, never by sending the value a user `printOn:` / `displayString`.
    iolist_to_binary(io_lib:format("~p", [Value])).

%% Synthesize the class-definition skeleton: header + state slots, method bodies
%% excluded. `null` for file-less classes (ClassBuilder) — the browser shows "no
%% source (programmatic class)" (ADR 0095, consistent with ADR 0085's non-goal).
-spec class_definition_text(atom(), atom() | none, [map()], binary() | null) ->
    binary() | null.
class_definition_text(_ClassName, _Super, _State, null) ->
    null;
class_definition_text(ClassName, Super, State, _SourceFile) ->
    SuperName =
        case Super of
            none -> <<"Object">>;
            S -> atom_to_binary(S, utf8)
        end,
    Header = [SuperName, <<" subclass: ">>, atom_to_binary(ClassName, utf8)],
    StateLines = [
        [<<"\n  state: ">>, Name, default_suffix(Default)]
     || #{<<"name">> := Name, <<"default">> := Default} <- State
    ],
    iolist_to_binary([Header | StateLines]).

-spec default_suffix(binary() | null) -> iolist().
default_suffix(null) -> [];
default_suffix(Default) -> [<<" = ">>, Default].

%%% ====================================================================
%%% Divergence — origin / disk_differs
%%% ====================================================================

%% origin :: both | static | runtime (ADR 0095). A class/row with a backing disk
%% source that is loaded in the image is `both`; a file-less / ClassBuilder class
%% (no disk source) is `runtime`. The `static` case (on disk, not loaded) is the
%% cold-mode answer and is produced by the static (Rust) layer, not here — a
%% class enumerated by `all_classes/0` is loaded by definition.
-spec origin_of(binary() | null) -> binary().
origin_of(null) -> <<"runtime">>;
origin_of(SourceFile) when is_binary(SourceFile) -> <<"both">>.

%% Per-selector origin from xref provenance: a runtime-installed selector
%% (put_method, class_builder) is runtime-only; a class_body / extension selector
%% in a file-backed class is `both`.
-spec origin_for_provenance(beamtalk_xref:provenance(), binary() | null) -> binary().
origin_for_provenance(put_method, _SourceFile) -> <<"runtime">>;
origin_for_provenance(class_builder, _SourceFile) -> <<"runtime">>;
origin_for_provenance(_Provenance, null) -> <<"runtime">>;
origin_for_provenance(_Provenance, _SourceFile) -> <<"both">>.

%% disk_differs (browse-method-source, op 3): true when the live per-method
%% source is absent from the static/disk class source recorded at load time —
%% the signature of an unflushed live `>>` patch (ADR 0082). null when there is
%% no static source to compare (file-less class, or a sourceless runtime method
%% whose source is itself null). Heuristic: the disk store is whole-class source
%% text and contains method bodies, so a patched body that no longer matches the
%% disk text reads as `differs`; a whitespace-only reformat could read as a
%% false positive, which is acceptable for a "you may be viewing unflushed
%% state" cue. The class-definition pane (op 4) does NOT use this — see
%% `class_definition_disk_differs/1`.
-spec disk_differs(atom(), binary() | null) -> boolean() | null.
disk_differs(_ClassName, null) ->
    null;
disk_differs(ClassName, ImageText) when is_binary(ImageText) ->
    case disk_source(ClassName) of
        undefined ->
            null;
        DiskSource when is_binary(DiskSource) ->
            %% The image text is a fragment of the whole-class disk source; it
            %% "differs" when the disk source does not contain it verbatim.
            binary:match(DiskSource, ImageText) =:= nomatch
    end.

-spec disk_source(atom()) -> binary() | undefined.
disk_source(ClassName) ->
    NameBin = atom_to_binary(ClassName, utf8),
    case beamtalk_workspace_meta:get_class_source(NameBin) of
        undefined -> undefined;
        Src when is_list(Src) -> unicode:characters_to_binary(Src)
    end.

%%% ====================================================================
%%% Validation
%%% ====================================================================

-spec validate_class(map()) -> {ok, atom()} | {error, binary()}.
validate_class(Params) ->
    case maps:get(<<"class">>, Params, undefined) of
        Cls when is_binary(Cls), byte_size(Cls) > 0 ->
            resolve_class(Cls);
        _ ->
            {error, <<"`class` (non-empty string) is required">>}
    end.

-spec validate_class_side(map()) ->
    {ok, {atom(), boolean()}} | {error, binary()}.
validate_class_side(Params) ->
    case validate_class(Params) of
        {ok, ClassName} ->
            case validate_side(Params) of
                {ok, ClassSide} -> {ok, {ClassName, ClassSide}};
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.

-spec validate_class_side_selector(map()) ->
    {ok, {atom(), boolean(), atom()}} | {error, binary()}.
validate_class_side_selector(Params) ->
    case validate_class_side(Params) of
        {ok, {ClassName, ClassSide}} ->
            case validate_selector(Params) of
                {ok, Selector} -> {ok, {ClassName, ClassSide, Selector}};
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.

-spec validate_side(map()) -> {ok, boolean()} | {error, binary()}.
validate_side(Params) ->
    case maps:get(<<"side">>, Params, undefined) of
        <<"instance">> ->
            {ok, false};
        <<"class">> ->
            {ok, true};
        undefined ->
            {error, <<"`side` is required (one of instance/class)">>};
        Other when is_binary(Other) ->
            {error,
                iolist_to_binary([
                    <<"`side` must be `instance` or `class`, got `">>, Other, <<"`">>
                ])};
        _ ->
            {error, <<"`side` must be a string (instance/class)">>}
    end.

-spec validate_selector(map()) -> {ok, atom()} | {error, binary()}.
validate_selector(Params) ->
    case maps:get(<<"selector">>, Params, undefined) of
        Sel when is_binary(Sel), byte_size(Sel) > 0 ->
            %% binary_to_existing_atom: untrusted client input must not grow the
            %% VM atom table. A selector the runtime has never seen reads as a
            %% sentinel, which yields a not-found (null source) result rather
            %% than a validation failure — mirrors beamtalk_repl_ops_nav.
            try
                {ok, binary_to_existing_atom(Sel, utf8)}
            catch
                error:badarg -> {ok, '__browse_unknown_selector__'}
            end;
        _ ->
            {error, <<"`selector` (non-empty string) is required">>}
    end.

%% Resolve a class-name binary to an atom only if it names a known class. An
%% unknown class is a structured not-found error, not an atom-table growth.
-spec resolve_class(binary()) -> {ok, atom()} | {error, binary()}.
resolve_class(Cls) ->
    try
        Name = binary_to_existing_atom(Cls, utf8),
        case beamtalk_runtime_api:whereis_class(Name) of
            undefined -> {error, class_not_found_message(Cls)};
            _Pid -> {ok, Name}
        end
    catch
        error:badarg -> {error, class_not_found_message(Cls)}
    end.

-spec class_not_found_message(binary()) -> binary().
class_not_found_message(Cls) ->
    iolist_to_binary([<<"class `">>, Cls, <<"` not found">>]).

%%% ====================================================================
%%% Shared reflection helpers (all field-reflection only, no user code)
%%% ====================================================================

-spec source_file_for_class(atom()) -> binary() | null.
source_file_for_class(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined -> null;
        Pid -> source_file_of(beamtalk_runtime_api:module_name(Pid))
    end.

-spec source_file_of(atom()) -> binary() | null.
source_file_of(ModName) ->
    case beamtalk_reflection:source_file_from_module(ModName) of
        nil -> null;
        Path when is_binary(Path) -> Path
    end.

%% Class category = the declared package (ADR 0070), read via field reflection —
%% never by sending the class a method. `null` when no package is declared (the
%% browser groups these under "Uncategorized", Pharo convention).
-spec category_of(atom()) -> binary() | null.
category_of(ModName) ->
    try beamtalk_runtime_api:extract_package_from_module(ModName) of
        undefined ->
            null;
        none ->
            null;
        Pkg when is_atom(Pkg) ->
            case atom_to_binary(Pkg, utf8) of
                <<>> -> null;
                Bin -> Bin
            end
    catch
        _:_ -> null
    end.

-spec class_doc(pid()) -> binary() | null.
class_doc(Pid) ->
    case safe_class_call(Pid, get_doc) of
        D when is_binary(D) -> D;
        _ -> null
    end.

-spec first_line(binary() | null) -> binary() | null.
first_line(null) ->
    null;
first_line(Doc) when is_binary(Doc) ->
    case binary:split(Doc, <<"\n">>) of
        [Line | _] -> Line;
        [] -> Doc
    end.

%% Map xref provenance to a browser protocol bucket (ADR 0095). class_body and
%% extension are the two source-bearing provenances; runtime-installed selectors
%% (put_method, class_builder) fall into "as yet unclassified" (Pharo).
-spec protocol_for_provenance(beamtalk_xref:provenance()) -> binary().
protocol_for_provenance(extension) -> <<"extensions">>;
protocol_for_provenance(_) -> <<"as yet unclassified">>.

-spec side_to_binary(boolean()) -> binary().
side_to_binary(true) -> <<"class">>;
side_to_binary(false) -> <<"instance">>.

-spec atom_or_null(atom() | none) -> binary() | null.
atom_or_null(none) -> null;
atom_or_null(A) when is_atom(A) -> atom_to_binary(A, utf8).

-spec safe_bool(fun(() -> boolean())) -> boolean().
safe_bool(F) ->
    try F() of
        B when is_boolean(B) -> B
    catch
        _:_ -> false
    end.

%% Reflection gen_server call with the same defensive guards list-classes uses —
%% a dead / slow class object never crashes a browse.
-spec safe_class_call(pid(), term()) -> term().
safe_class_call(Pid, Call) ->
    try
        gen_server:call(Pid, Call, 5000)
    catch
        exit:{noproc, _} -> undefined;
        exit:{timeout, _} -> undefined;
        _:_ -> undefined
    end.

%%% ====================================================================
%%% Errors
%%% ====================================================================

-spec arg_error(binary(), binary()) -> {error, #beamtalk_error{}}.
arg_error(Op, Reason) ->
    Err = beamtalk_error:new(argument_error, 'REPL'),
    {error, beamtalk_error:with_message(Err, iolist_to_binary([Op, <<": ">>, Reason]))}.

-spec not_found_error(binary(), atom()) -> {error, #beamtalk_error{}}.
not_found_error(Op, ClassName) ->
    arg_error(Op, class_not_found_message(atom_to_binary(ClassName, utf8))).
