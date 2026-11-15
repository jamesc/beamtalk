%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_browse).

%%% **DDD Context:** REPL Session Context (System Browser bridge)

-moduledoc """
Op handlers for the System Browser browse facade (ADR 0096).

The LiveView IDE's System Browser (ADR 0017 Phase 3) renders the
four-pane Smalltalk navigator — *classes → protocols → selectors → method
source* plus a class-definition pane. These read-only term-ops populate those
panes against a live workspace, sourced **static-first / live-augmented**
(ADR 0024) with image/disk divergence carried as explicit per-row metadata
(never silently merged):

| Op | Pane | Term shape |
|----|------|------------|
| `browse-classes` | class tree | `{value, [ClassRow]}` |
| `browse-protocols` | protocol + selector tree | `{value, ProtocolTree}` |
| `browse-method-source` | method source pane | `{value, MethodSource}` |
| `browse-class-definition` | class-definition pane | `{value, ClassDefinition}` |
| `browse-categories` | divider-grouped method view | `{value, CategoryView}` |
| `browse-native-source` | read-only native pane | `{value, NativeSource}` |
| `browse-native-modules` | native-modules section | `{value, [NativeModuleRow]}` |
| `browse-type-aliases` | type-aliases section | `{value, [AliasRow]}` |
| `browse-alias-source` | read-only alias source view | `{value, AliasSource}` |

## Term contract

Every op returns a `{value, JsonValue}` tagged term — the rows are already a
wire-shaped JSON value (maps/lists of binaries, integers, booleans, `null`), so
`beamtalk_repl_ops:encode/2` passes them through the identity encoder (no
`term_to_json` inspect-string round-trip). Dist-attached clients (Phoenix
LiveView, MCP) consume the live terms directly over distribution; JSON appears
only at the WebSocket edge. Validation failures return
`{error, #beamtalk_error{}}` (the structured error contract).

## Read-only / no-user-code guarantee (ADR 0091 Observer gate)

All these ops are **pure reflection**: class metadata (`get_doc`, `is_sealed`,
`is_abstract`, `is_internal`, `instance_variables`, `field_defaults`), xref rows
(`beamtalk_xref:defined_selectors/2`, `method_info/3`), stored method text
(`{method, Sel}` / `{class_method, Sel}` → `__source__` / `__doc__` /
`__signature__`), and — for `browse-native-source` — the facade's
`__beamtalk_meta/0` plus the backing module's on-disk `.erl` (read only).
None sends a
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

See `docs/ADR/0096-system-browser-data-source.md`.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([handle/4, handle_term/4, describe_ops/0]).

%% `info_fields/1` (method_info() -> {Line, SourceStatus, Provenance}),
%% `row_doc_signature/4` (the synthetic-only doc/signature
%% resolver), and `side_to_binary/1` (boolean class_side -> `<<"class">>` /
%% `<<"instance">>`) are the same per-selector shaping op 2 (`browse-protocols`)
%% already does. Exported so the `methods` ws op (`beamtalk_repl_ops_dev`) — the
%% VS Code Workspace Explorer sidebar's method-list channel — can badge/hover a
%% compiler-generated method the same honest way the LiveView IDE method list
%% already does, instead of a second `source_status`-shaping implementation.
-export([info_fields/1, row_doc_signature/4, side_to_binary/1]).

-ifdef(TEST).
%% Pure helpers exercised directly in EUnit: the class-definition skeleton
%% builder plus the dead-pid resilience paths (class_row/2, state_slots/2).
%% The native / alias / origin helpers are exported by their own modules.
-export([
    class_definition_text/7,
    class_row/2,
    state_slots/2
]).
-endif.

-doc """
Handle a browse op for the WebSocket transport — encodes the term result to
JSON at the edge. Dist-attached clients call `handle_term/4` directly.
""".
-spec handle(binary(), map(), beamtalk_repl_protocol:protocol_msg(), pid()) -> binary().
handle(Op, Params, Msg, SessionPid) ->
    beamtalk_repl_ops:encode(handle_term(Op, Params, Msg, SessionPid), Msg).

-doc """
Term-returning handler for the browse ops (ADR 0096). Returns
`{value, JsonValue}` on success or `{error, #beamtalk_error{}}` on a validation
failure (unknown class, bad side, missing selector).
""".
-spec handle_term(
    binary() | beamtalk_string:t(), map(), beamtalk_repl_protocol:protocol_msg(), pid()
) ->
    beamtalk_repl_ops:op_result().
%% BT-4173: direct FFI callers pass a tagged op name and tagged param text —
%% unwrap to the raw wire shape the clauses below match on, then tag the
%% text result back for Beamtalk lookup (`at: "protocols"`). Raw-op callers
%% (the wire path, eunit) see byte-identical results to before.
handle_term({beamtalk_string, Op}, Params, Msg, SessionPid) when is_binary(Op) ->
    RawParams = beamtalk_primitive:deep_unwrap_strings(Params),
    case handle_term(Op, RawParams, Msg, SessionPid) of
        {value, JsonValue} -> {value, beamtalk_primitive:deep_tag_strings(JsonValue)};
        {error, _} = Err -> Err
    end;
handle_term(<<"browse-classes">>, _Params, _Msg, _SessionPid) ->
    {value, browse_classes()};
handle_term(<<"browse-protocols">>, Params, _Msg, _SessionPid) ->
    case validate_class_side(Params) of
        {ok, {ClassName, ClassSide}} ->
            browse_protocols(ClassName, ClassSide);
        {error, Reason} ->
            beamtalk_repl_ops_browse_support:arg_error(<<"browse-protocols">>, Reason)
    end;
handle_term(<<"browse-method-source">>, Params, _Msg, _SessionPid) ->
    case validate_class_side_selector(Params) of
        {ok, {ClassName, ClassSide, Selector}} ->
            browse_method_source(ClassName, ClassSide, Selector);
        {error, Reason} ->
            beamtalk_repl_ops_browse_support:arg_error(<<"browse-method-source">>, Reason)
    end;
handle_term(<<"browse-class-definition">>, Params, _Msg, _SessionPid) ->
    case validate_class(Params) of
        {ok, ClassName} ->
            browse_class_definition(ClassName);
        {error, Reason} ->
            beamtalk_repl_ops_browse_support:arg_error(<<"browse-class-definition">>, Reason)
    end;
handle_term(<<"browse-categories">>, Params, _Msg, _SessionPid) ->
    case validate_class(Params) of
        {ok, ClassName} ->
            browse_categories(ClassName);
        {error, Reason} ->
            beamtalk_repl_ops_browse_support:arg_error(<<"browse-categories">>, Reason)
    end;
handle_term(<<"browse-native-source">>, Params, _Msg, _SessionPid) ->
    %% The native pane can be keyed by a standalone native `module`
    %% (a dependency's hand-written `.erl` with no `native:` class to back it,
    %% surfaced by `browse-native-modules`) as an alternative to a `class`. The
    %% `module` form takes precedence; the `class` form is the original
    %% path (resolve a native class's backing module).
    case validate_module(Params) of
        {ok, ModuleName} ->
            beamtalk_repl_ops_browse_native:browse_native_module_source(
                ModuleName, beamtalk_repl_ops_browse_native:optional_selector(Params)
            );
        none ->
            case validate_class(Params) of
                {ok, ClassName} ->
                    beamtalk_repl_ops_browse_native:browse_native_source(
                        ClassName, beamtalk_repl_ops_browse_native:optional_selector(Params)
                    );
                {error, Reason} ->
                    beamtalk_repl_ops_browse_support:arg_error(<<"browse-native-source">>, Reason)
            end;
        {error, Reason} ->
            beamtalk_repl_ops_browse_support:arg_error(<<"browse-native-source">>, Reason)
    end;
handle_term(<<"browse-native-modules">>, _Params, _Msg, _SessionPid) ->
    {value, beamtalk_repl_ops_browse_native:browse_native_modules()};
handle_term(<<"browse-type-aliases">>, _Params, _Msg, _SessionPid) ->
    {value, beamtalk_repl_ops_browse_aliases:browse_type_aliases()};
handle_term(<<"browse-alias-source">>, Params, _Msg, _SessionPid) ->
    case beamtalk_repl_ops_browse_aliases:validate_alias(Params) of
        {ok, {Name, Package}} ->
            beamtalk_repl_ops_browse_aliases:browse_alias_source(Name, Package);
        {error, Reason} ->
            beamtalk_repl_ops_browse_support:arg_error(<<"browse-alias-source">>, Reason)
    end.

-doc "Advertise the browse ops in `describe`.".
-spec describe_ops() -> map().
describe_ops() ->
    beamtalk_repl_op_table:describe_for(?MODULE).

%%% ====================================================================
%%% Op 1 — browse-classes
%%% ====================================================================

%% Lists every class in scope with the edges the class tree needs: superclass
%% (hierarchy) + category (the Smalltalk class-category group). Extends the
%% list-classes field set with `category` and `origin`; it does not replace
%% list-classes (ADR 0096). Reuses `beamtalk_runtime_api:all_classes/0` and the
%% same per-class reflection list-classes uses — no new runtime bookkeeping.
-spec browse_classes() -> [map()].
browse_classes() ->
    ClassPids =
        try
            beamtalk_runtime_api:all_classes()
        catch
            _:_ -> []
        end,
    %% The set of loaded TestCase subclasses, computed once, so each row
    %% can carry an `is_test` flag. The browser groups these under a synthetic
    %% "Tests" category — pure reflection over the class hierarchy, no user code.
    TestClasses = sets:from_list(safe_test_classes()),
    Rows = lists:filtermap(fun(Pid) -> class_row(Pid, TestClasses) end, ClassPids),
    lists:sort(fun(A, B) -> maps:get(<<"name">>, A) =< maps:get(<<"name">>, B) end, Rows).

%% The loaded TestCase subclass atoms, or [] if discovery raises (the browser
%% must still list classes even when the test registry is unavailable).
-spec safe_test_classes() -> [atom()].
safe_test_classes() ->
    try
        beamtalk_test_case:find_test_classes()
    catch
        _:_ -> []
    end.

-spec class_row(pid(), sets:set(atom())) -> {true, map()} | false.
class_row(Pid, TestClasses) ->
    try
        Name = beamtalk_runtime_api:class_name(Pid),
        Super = beamtalk_runtime_api:superclass(Pid),
        ModName = beamtalk_repl_ops_browse_origin:origin_module(
            Name, beamtalk_runtime_api:module_name(Pid)
        ),
        SourceFile = beamtalk_repl_row:source_file_of(ModName),
        SourceOrigin = beamtalk_repl_ops_browse_origin:source_origin_of(ModName, SourceFile),
        {true, #{
            <<"name">> => atom_to_binary(Name, utf8),
            <<"superclass">> => beamtalk_repl_ops_browse_support:atom_or_null(Super),
            <<"category">> => category_of(ModName),
            <<"comment">> => beamtalk_repl_row:first_line(class_doc(Pid)),
            <<"sealed">> => beamtalk_repl_ops_browse_support:safe_bool(fun() ->
                beamtalk_runtime_api:is_sealed(Pid)
            end),
            <<"typed">> => beamtalk_repl_ops_browse_support:safe_bool(fun() ->
                beamtalk_runtime_api:is_typed(Pid)
            end),
            <<"abstract">> => beamtalk_repl_ops_browse_support:safe_bool(fun() ->
                beamtalk_runtime_api:is_abstract(Pid)
            end),
            <<"internal">> => beamtalk_repl_ops_browse_support:safe_bool(fun() ->
                beamtalk_runtime_api:is_internal(Pid)
            end),
            <<"source_file">> => SourceFile,
            <<"origin">> => beamtalk_repl_ops_browse_origin:origin_of(SourceFile),
            <<"source_origin">> => SourceOrigin,
            <<"package">> => beamtalk_repl_ops_browse_origin:package_of(ModName, SourceOrigin),
            <<"is_test">> => sets:is_element(Name, TestClasses),
            %% Protocol class objects (ADR 0068) are sealed abstract
            %% subclasses of Protocol with no declared package, so they would
            %% otherwise fall into the "(uncategorized)" bucket. Flag them so the
            %% browser groups them under a dedicated "Protocols" category — pure
            %% registry reflection, mirroring the `is_test` → "Tests" treatment.
            <<"is_protocol">> => beamtalk_repl_ops_browse_support:safe_bool(fun() ->
                beamtalk_runtime_api:is_protocol(Name)
            end)
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
    %% Resolve the protocol's defining module (else the shared
    %% `beamtalk_protocol_object` dispatch module) so the method rows' source
    %% badges match where the protocol actually lives (stdlib vs project).
    ModName = beamtalk_repl_ops_browse_origin:origin_module(
        ClassName, mod_name_for_class(ClassName)
    ),
    SourceFile = beamtalk_repl_row:source_file_of(ModName),
    %% Resolve the class pid once here (not per selector) so
    %% `selector_row` can enrich each `synthetic` row with its resolved
    %% signature + doc for a VS Code-style method-list hover. `undefined` when
    %% the class is not live — rows then carry null signature/doc and the hover
    %% falls back to the bare selector.
    ClassPid = beamtalk_runtime_api:whereis_class(ClassName),
    SelectorRows = [
        selector_row(ClassName, ClassSide, Sel, SourceFile, ModName, ClassPid)
     || Sel <- Selectors
    ],
    %% A protocol class object (ADR 0068) carries no instance methods of
    %% its own — its xref rows are just the class-side reflection methods
    %% (`requiredMethods` / `conformingClasses`), so the instance side comes back
    %% empty and the browser shows "no methods". Surface the protocol's *required
    %% members* (registry reflection, no user code) so a protocol's contract is
    %% visible: the required instance selectors on the instance side, the required
    %% class selectors on the class side, grouped under a "requirements" protocol.
    RequirementRows = protocol_requirement_rows(ClassName, ClassSide, SourceFile, ModName),
    Grouped = group_by_protocol(SelectorRows ++ RequirementRows),
    {value, #{
        <<"class">> => atom_to_binary(ClassName, utf8),
        <<"side">> => side_to_binary(ClassSide),
        <<"protocols">> => Grouped
    }}.

%% The required-member rows for a protocol class object, or [] for a non-protocol
%% class. Instance side surfaces `required_methods`; class side surfaces
%% `required_class_methods`. The members are signatures, not implemented
%% methods — they have no openable source — so each row is `unindexed_runtime_fun`
%% / `runtime` origin, the same shape the browser already renders for a sourceless
%% selector. Reflection over the protocol registry only; no user code is run.
-spec protocol_requirement_rows(atom(), boolean(), binary() | null, atom()) -> [map()].
protocol_requirement_rows(ClassName, ClassSide, SourceFile, ModName) ->
    case beamtalk_runtime_api:protocol_info(ClassName) of
        Info when is_map(Info) ->
            Key =
                case ClassSide of
                    true -> required_class_methods;
                    false -> required_methods
                end,
            Members = maps:get(Key, Info, []),
            [
                requirement_row(Selector, SourceFile, ModName)
             || #{selector := Selector} <- Members
            ];
        _ ->
            []
    end.

-spec requirement_row(atom(), binary() | null, atom()) -> map().
requirement_row(Selector, SourceFile, ModName) ->
    SourceOrigin = beamtalk_repl_ops_browse_origin:source_origin_of(ModName, SourceFile),
    #{
        <<"selector">> => atom_to_binary(Selector, utf8),
        <<"line">> => null,
        <<"source_status">> => <<"unindexed_runtime_fun">>,
        <<"origin">> => <<"runtime">>,
        <<"source_origin">> => SourceOrigin,
        <<"package">> => beamtalk_repl_ops_browse_origin:package_of(ModName, SourceOrigin),
        %% A required member has no implemented method to resolve, so
        %% its hover carries null signature/doc — the same shape as an
        %% unenriched selector row (keeps the row shape uniform for the client).
        <<"signature">> => null,
        <<"doc">> => null,
        %% carried internally for grouping; stripped before emit
        '__protocol__' => <<"requirements">>
    }.

-spec selector_row(atom(), boolean(), atom(), binary() | null, atom(), pid() | undefined) ->
    map().
selector_row(ClassName, ClassSide, Selector, SourceFile, ModName, ClassPid) ->
    Info = beamtalk_xref:method_info(ClassName, ClassSide, Selector),
    {Line, SourceStatus, Provenance} = info_fields(Info),
    SourceOrigin = beamtalk_repl_ops_browse_origin:source_origin_of(ModName, SourceFile),
    {Doc, Signature} = row_doc_signature(ClassPid, ClassSide, Selector, SourceStatus),
    #{
        <<"selector">> => atom_to_binary(Selector, utf8),
        <<"line">> => Line,
        <<"source_status">> => atom_to_binary(SourceStatus, utf8),
        <<"origin">> => beamtalk_repl_ops_browse_origin:origin_for_provenance(
            Provenance, SourceFile
        ),
        <<"source_origin">> => SourceOrigin,
        <<"package">> => beamtalk_repl_ops_browse_origin:package_of(ModName, SourceOrigin),
        %% Signature + first-line-of-doc feed the method-row hover so a
        %% hover conveys what the method is without a click. Resolved only for
        %% `synthetic` rows (see `row_doc_signature/4`) — the client renders the
        %% full doc's first line and falls back to the selector when both null.
        <<"signature">> => Signature,
        <<"doc">> => Doc,
        %% carried internally for grouping; stripped before emit
        '__protocol__' => beamtalk_repl_ops_browse_origin:protocol_for_selector(
            Selector, Info, Provenance, SourceStatus, ClassSide
        )
    }.

%% A selector row's hover signature + doc, resolved via the same
%% hierarchy walk `:help` uses (`beamtalk_repl_docs:method_doc_signature_resolved/3`)
%% — so a compiler-derived `spawn` inherits `Actor`'s curated
%% doc and a value accessor shows its generated signature. Bounded to `synthetic`
%% rows: a small, known set of compiler-injected methods, the ones whose intent a
%% bare selector conveys least. Non-synthetic rows (and a missing class pid) stay
%% `{null, null}` so the browse hot path never does an unbounded per-method
%% CompiledMethod read for every hand-written selector. Returns `{Doc, Signature}`.
-spec row_doc_signature(pid() | undefined, boolean(), atom(), beamtalk_xref:source_status()) ->
    {binary() | null, binary() | null}.
row_doc_signature(ClassPid, ClassSide, Selector, synthetic) when is_pid(ClassPid) ->
    beamtalk_repl_docs:method_doc_signature_resolved(ClassPid, ClassSide, Selector);
row_doc_signature(_ClassPid, _ClassSide, _Selector, _SourceStatus) ->
    {null, null}.

-spec info_fields(beamtalk_xref:method_info() | undefined) ->
    {pos_integer() | null, beamtalk_xref:source_status(), beamtalk_xref:provenance()}.
info_fields(#{line := Line, source_status := SS, provenance := Prov}) ->
    {Line, SS, Prov};
info_fields(_) ->
    %% No xref row: a method known to the class but not indexed (sourceless
    %% runtime fun). Mark it as such so the browser shows "no source".
    {null, unindexed_runtime_fun, put_method}.

%% Group selector rows into sorted protocol buckets. The protocol of a selector
%% is decided by `protocol_for_selector/5` (ADR 0096): a declared category if
%% one exists, else the xref-extension source fact, else a Pharo-convention
%% selector-name heuristic; unrecognised selectors fall into "as yet
%% unclassified". Selectors within a protocol are sorted; protocols are sorted.
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
%% `__source__`) — the patch-aware per-method text. source_status from
%% xref. `disk_differs` compares the image body against a live re-read of the
%% on-disk class source (`current_disk_source/2`).
-spec browse_method_source(atom(), boolean(), atom()) -> beamtalk_repl_ops:op_result().
browse_method_source(ClassName, ClassSide, Selector) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            beamtalk_repl_ops_browse_support:not_found_error(<<"browse-method-source">>, ClassName);
        ClassPid ->
            {Line, SourceStatus, Provenance} = info_fields(
                beamtalk_xref:method_info(ClassName, ClassSide, Selector)
            ),
            {Source, Doc, Signature} =
                method_text_fields(ClassPid, ClassSide, Selector, SourceStatus),
            SourceFile = source_file_for_class(ClassName),
            ModName = beamtalk_runtime_api:module_name(ClassPid),
            SourceOrigin = beamtalk_repl_ops_browse_origin:source_origin_of(ModName, SourceFile),
            {value, #{
                <<"class">> => atom_to_binary(ClassName, utf8),
                <<"side">> => side_to_binary(ClassSide),
                <<"selector">> => atom_to_binary(Selector, utf8),
                <<"source">> => Source,
                %% True when this is a `self delegate` method (ADR 0056)
                %% on a native-backed class — the real implementation lives in a
                %% `handle_call` clause of the backing module, reachable via
                %% `browse-native-source`. The signal is the compiler's own
                %% `is_self_delegate` decision, made visible at runtime: the native
                %% facade generates a `dispatch_<selector>` function ONLY for a
                %% `self delegate` method (native_facade.rs), so its export is a
                %% precise marker — no body-text heuristic.
                <<"native_delegate">> => beamtalk_repl_ops_browse_native:is_native_delegate(
                    ModName, ClassSide, Selector
                ),
                %% The method's `///` doc-comment and rendered
                %% signature, carried alongside the editable source so the
                %% System Browser can present a read-only documentation block.
                %% Pulled from the same CompiledMethod map (`__doc__` /
                %% `__signature__`) that `Beamtalk help: aClass selector:`
                %% reads (`beamtalk_repl_docs:method_doc_info/2`), so the
                %% browser and the `help:` send agree on the docs for a method.
                <<"doc">> => Doc,
                <<"signature">> => Signature,
                <<"line">> => Line,
                <<"source_status">> => atom_to_binary(SourceStatus, utf8),
                <<"origin">> => beamtalk_repl_ops_browse_origin:origin_for_provenance(
                    Provenance, SourceFile
                ),
                <<"source_origin">> => SourceOrigin,
                <<"package">> => beamtalk_repl_ops_browse_origin:package_of(ModName, SourceOrigin),
                <<"disk_differs">> => disk_differs(SourceFile, ClassName, Source)
            }}
    end.

%% The method's editable source, `///` doc-comment, and signature — all read
%% from the one live CompiledMethod map so they stay mutually consistent.
%% Sourceless runtime methods (unindexed_runtime_fun) have no CompiledMethod to
%% read: source/doc/signature are all null, and the source_status says why — the
%% browser shows "no source (runtime method)" rather than an empty pane (ADR
%% 0096).
-spec method_text_fields(pid(), boolean(), atom(), beamtalk_xref:source_status()) ->
    {binary() | null, binary() | null, binary() | null}.
method_text_fields(_ClassPid, _ClassSide, _Selector, unindexed_runtime_fun) ->
    {null, null, null};
%% Synthetic methods (compiler-injected value-type
%% auto-accessors) carry no editable user source. `source` stays null so the
%% browser badges them read-only with no `[source]` jump. (The actor
%% `new`/`new:`/`spawn`/`spawn:` constructors are real, source-backed class
%% methods on `Actor`, not synthetic rows — so `Actor`'s indexed row is
%% reached the ordinary way, no special-casing needed here.)
%%
%% Doc/signature DO resolve — via the same hierarchy walk `Beamtalk
%% help:` uses (`method_doc_signature_resolved/3`) — so the read-only pane shows
%% the method's real curated docs (a value accessor shows its generated
%% signature) instead of a blank pane. Reusing the `help:` resolver guarantees
%% the browser and `:help` agree.
method_text_fields(ClassPid, ClassSide, Selector, synthetic) ->
    {Doc, Signature} =
        beamtalk_repl_docs:method_doc_signature_resolved(ClassPid, ClassSide, Selector),
    {null, Doc, Signature};
method_text_fields(ClassPid, ClassSide, Selector, _SourceStatus) ->
    Call =
        case ClassSide of
            true -> {class_method, Selector};
            false -> {method, Selector}
        end,
    case beamtalk_repl_ops_browse_support:safe_class_call(ClassPid, Call) of
        MethodObj when is_map(MethodObj) ->
            {
                method_field(MethodObj, '__source__'),
                method_field(MethodObj, '__doc__'),
                method_field(MethodObj, '__signature__')
            };
        _ ->
            {null, null, null}
    end.

%% A CompiledMethod text field (`__source__` / `__doc__` / `__signature__`):
%% the stored binary when present and non-empty, `null` otherwise.
-spec method_field(map(), atom()) -> binary() | null.
method_field(MethodObj, Key) ->
    case maps:get(Key, MethodObj, null) of
        Bin when is_binary(Bin), byte_size(Bin) > 0 -> Bin;
        _ -> null
    end.

%%% ====================================================================
%%% Op 4 — browse-class-definition
%%% ====================================================================

%% The class-definition pane: class header, state slots, and full comment. State
%% is field reflection (ADR 0035) — names, types, and default expressions, no
%% user code. The definition string is the class skeleton (`typed Super
%% subclass: Name` + `field:`/`state:` slots per ADR 0067), method bodies
%% excluded (Pharo convention), synthesized from reflection for every loaded
%% class. File-less (ClassBuilder) and stdlib classes keep the skeleton;
%% `origin = runtime` (not the definition) is what flags no disk source.
-spec browse_class_definition(atom()) -> beamtalk_repl_ops:op_result().
browse_class_definition(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            beamtalk_repl_ops_browse_support:not_found_error(
                <<"browse-class-definition">>, ClassName
            );
        ClassPid ->
            Super = beamtalk_runtime_api:superclass(ClassPid),
            %% A protocol class object's dispatch module is the shared
            %% `beamtalk_protocol_object`; resolve its defining module so the
            %% definition pane's category/origin/source_origin are accurate.
            ModName = beamtalk_repl_ops_browse_origin:origin_module(
                ClassName, beamtalk_runtime_api:module_name(ClassPid)
            ),
            SourceFile = beamtalk_repl_row:source_file_of(ModName),
            SourceOrigin = beamtalk_repl_ops_browse_origin:source_origin_of(ModName, SourceFile),
            %% Native-backed classes (ADR 0056) carry their backing
            %% Erlang module name so the System Browser can badge them and offer
            %% `browse-native-source`. Read from the facade module's
            %% `__beamtalk_meta/0` (`native => true, backing_module => atom()`).
            %% The same map also carries `kind` (object|value|actor,
            %% ADR 0070) and `field_types` (ADR 0067) — reused below to pick the
            %% `field:`/`state:` keyword and per-field `:: Type` annotations, so
            %% the skeleton is built from one `__beamtalk_meta/0` read.
            Meta = beamtalk_repl_ops_browse_native:native_meta_of(ModName),
            IsTyped = beamtalk_repl_ops_browse_support:safe_bool(fun() ->
                beamtalk_runtime_api:is_typed(ClassPid)
            end),
            IsSealed = beamtalk_repl_ops_browse_support:safe_bool(fun() ->
                beamtalk_runtime_api:is_sealed(ClassPid)
            end),
            IsAbstract = beamtalk_repl_ops_browse_support:safe_bool(fun() ->
                beamtalk_runtime_api:is_abstract(ClassPid)
            end),
            State = state_slots(ClassPid, Meta),
            %% `IsSealed`/`IsAbstract` are threaded into the skeleton
            %% itself (see `class_definition_text/7`), not just the sibling
            %% `sealed`/`abstract` reflection fields below — closing the round-trip
            %% gap left by covering only `typed`, `field:`/`state:`, and `::` type
            %% annotations previously.
            Definition = class_definition_text(
                ClassName, Super, State, IsTyped, IsSealed, IsAbstract, Meta
            ),
            {value, #{
                <<"class">> => atom_to_binary(ClassName, utf8),
                <<"superclass">> => beamtalk_repl_ops_browse_support:atom_or_null(Super),
                <<"category">> => category_of(ModName),
                <<"definition">> => Definition,
                <<"state">> => State,
                <<"comment">> => class_doc(ClassPid),
                <<"native">> => beamtalk_repl_ops_browse_native:meta_is_native(Meta),
                <<"backing_module">> =>
                    beamtalk_repl_ops_browse_support:atom_or_null(
                        beamtalk_class_registry:meta_backing_module(Meta)
                    ),
                %% Reflected class modifiers for the IDE's editor-header
                %% modifier badges. The synthesized `definition` skeleton
                %% above also carries these as leading modifier keywords (round-
                %% trip safety for the `:def` tab's resubmit path), but these fields
                %% remain the badge source of truth — same runtime reflection op 1
                %% (`browse-classes`) uses, not a string parse of `definition`.
                <<"sealed">> => IsSealed,
                <<"typed">> => IsTyped,
                <<"abstract">> => IsAbstract,
                %% A structural reflection boolean (not a header
                %% string-sniff) so the System Browser can reliably render the
                %% protocol-definition action row (Required methods / Conforming
                %% classes). Mirrors the `is_protocol` field op 1
                %% (`browse-classes`) already carries on each class row.
                <<"is_protocol">> => beamtalk_repl_ops_browse_support:safe_bool(fun() ->
                    beamtalk_runtime_api:is_protocol(ClassName)
                end),
                <<"origin">> => beamtalk_repl_ops_browse_origin:origin_of(SourceFile),
                <<"source_origin">> => SourceOrigin,
                <<"package">> => beamtalk_repl_ops_browse_origin:package_of(ModName, SourceOrigin),
                %% The definition pane renders a *synthesized* class skeleton
                %% (header + state slots), not the verbatim on-disk class source
                %% — so a substring diff against the disk store would be unsound
                %% (false positives on formatting/syntax). With no separate
                %% image-side class-source snapshot to diff, the honest answer is
                %% `null` ("nothing to compare", ADR 0096). A precise class-level
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
%% expression text where the live class object carries one, and their declared
%% type read from the class module's `__beamtalk_meta/0` `field_types`
%% map (`Meta`, already resolved by the caller — the same source
%% `beamtalk_workspace_shape_store:read_shape_from_meta/1` reads for hot-reload
%% shape diffing). Field reflection only — no user code run.
-spec state_slots(pid(), map()) -> [map()].
state_slots(ClassPid, Meta) ->
    Fields = beamtalk_repl_ops_browse_support:safe_class_call(ClassPid, instance_variables),
    Defaults = beamtalk_repl_ops_browse_support:safe_class_call(ClassPid, field_defaults),
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
    FieldTypes = maps:get(field_types, Meta, #{}),
    [
        #{
            <<"name">> => atom_to_binary(F, utf8),
            <<"default">> => default_text(maps:get(F, DefaultsMap, undefined)),
            <<"type">> => field_type_text(maps:get(F, FieldTypes, undefined))
        }
     || F <- FieldList
    ].

%% A field's declared type from `field_types`: `null` when untyped (the
%% generator's own `'none'` sentinel, see `meta_field_types_map` in
%% `crates/beamtalk-core/src/codegen/core_erlang/gen_server/methods.rs`) or
%% absent from the map (no `__beamtalk_meta/0`, e.g. a file-less ClassBuilder
%% class), else the type name as text.
-spec field_type_text(atom() | undefined) -> binary() | null.
field_type_text(none) -> null;
field_type_text(undefined) -> null;
field_type_text(Type) when is_atom(Type) -> atom_to_binary(Type, utf8).

-spec default_text(term()) -> binary() | null.
default_text(undefined) ->
    null;
default_text(Value) ->
    %% Field reflection (ADR 0035): render the stored default term as text via
    %% io_lib, never by sending the value a user `printOn:` / `displayString`.
    beamtalk_error:chars_to_binary(io_lib:format("~tp", [Value])).

%% Synthesize the class-definition skeleton: header + state slots, method bodies
%% excluded. Derived purely from the loaded class' reflected superclass + state
%% (ADR 0035), so it is available for *every* loaded class — file-backed user
%% classes, stdlib classes (whose BEAM carries no `beamtalk_source` attribute, so
%% `source_file_of` is null), and file-less ClassBuilder classes alike. The
%% runtime/disk distinction lives in `origin` (null SourceFile → runtime), not in
%% the skeleton: a class that is loaded always has a representable shape, and
%% returning null here only stranded stdlib classes on an empty editor.
%%
%% Matches the real `.bt` source syntax post-ADR-0067 — `typed ` is
%% prepended when `IsTyped`, the state-declaration keyword is `field:` for a
%% Value class and `state:` for everything else (Actor, and the Object/unknown
%% fallback, which under the ADR carries no data anyway), and each field's
%% `:: Type` suffix comes from its own entry in `State` (present iff that field
%% was declared with a type — independent of `IsTyped`, which only gates the
%% header prefix).
%%
%% `abstract `/`sealed ` are prepended the same way when `IsAbstract`/
%% `IsSealed`, closing the round-trip gap `add_class_def_flushability/2`'s doc
%% used to describe — the cockpit `:def` tab's resubmitted skeleton no longer
%% silently drops these modifiers on a redefine + flush. The parser
%% (`parse_class_definition`) accepts `abstract`/`sealed`/`typed`/`internal` in
%% ANY order (a `while` loop over identifier tokens, each just setting a flag —
%% see `crates/beamtalk-core/src/source_analysis/parser/declarations.rs`), so a
%% fixed emission order (`abstract sealed typed`, matching every combination
%% already used across stdlib — e.g. `abstract typed Value subclass:
%% Collection(E)`, `abstract sealed Value subclass: Boolean`) round-trips
%% correctly regardless of the class's original source order.
-spec class_definition_text(
    atom(), atom() | none, [map()], boolean(), boolean(), boolean(), map()
) -> binary().
class_definition_text(ClassName, Super, State, IsTyped, IsSealed, IsAbstract, Meta) ->
    SuperName =
        case Super of
            none -> <<"Object">>;
            S -> atom_to_binary(S, utf8)
        end,
    Header = [
        modifier_prefix(IsAbstract, <<"abstract">>),
        modifier_prefix(IsSealed, <<"sealed">>),
        modifier_prefix(IsTyped, <<"typed">>),
        SuperName,
        <<" subclass: ">>,
        atom_to_binary(ClassName, utf8)
    ],
    ShapeVersionLine = shape_version_line(Meta),
    Keyword = state_keyword(Meta),
    StateLines = [
        [<<"\n  ">>, Keyword, <<": ">>, Name, type_suffix(Type), default_suffix(Default)]
     || #{<<"name">> := Name, <<"default">> := Default, <<"type">> := Type} <- State
    ],
    iolist_to_binary([Header, ShapeVersionLine | StateLines]).

%% ADR 0123 §1: `shapeVersion: N` is a header clause, like `handleScope:`
%% (whose skeleton emission this module doesn't (yet) synthesize) — round-
%% tripping the compiler's own `'shape_version'` meta key (omitted when the
%% class is at the implicit default, version 1, mirroring
%% `class_meta.rs`'s `handle_scope`-pattern omission) so a `Workspace flush`
%% resubmit of the skeleton doesn't silently drop a declared version.
-spec shape_version_line(map()) -> iolist().
shape_version_line(#{shape_version := N}) when is_integer(N), N > 0 ->
    [<<"\n  shapeVersion: ">>, integer_to_binary(N)];
shape_version_line(_Meta) ->
    [].

-spec modifier_prefix(boolean(), binary()) -> iolist().
modifier_prefix(true, Keyword) -> [Keyword, <<" ">>];
modifier_prefix(false, _Keyword) -> [].

%% ADR 0067: Actor uses `state:`, Value uses `field:`; Object subclasses carry
%% no instance data under the ADR. `Meta`'s `kind` (ADR 0070, `object | value |
%% actor`) is the class-kind authority; anything other than `value` (including
%% a missing/empty `Meta` — a file-less ClassBuilder class, or a module with no
%% `__beamtalk_meta/0`) falls back to `state:`, matching the previous
%% behaviour for those classes.
-spec state_keyword(map()) -> binary().
state_keyword(#{kind := value}) -> <<"field">>;
state_keyword(_Meta) -> <<"state">>.

-spec type_suffix(binary() | null) -> iolist().
type_suffix(null) -> [];
type_suffix(Type) -> [<<" :: ">>, Type].

-spec default_suffix(binary() | null) -> iolist().
default_suffix(null) -> [];
default_suffix(Default) -> [<<" = ">>, Default].

%%% ====================================================================
%%% Op 4b — browse-categories
%%% ====================================================================

%% Groups a class's methods by `// === Name ===` section divider (the
%% shared, canonical recognizer — `beamtalk_compiler:categorize_methods/2',
%% bridged from `beamtalk_core::source_analysis::categorize_methods_in_source'
%% via the compiler port) for the System Browser's grouped method view.
%%
%% Unlike ops 1-4, this reads the class's *whole on-disk source text*, not
%% just its live/reflected metadata — divider categories are file structure,
%% invisible to a loaded class object (see `method_category.rs''s module doc:
%% "a class object built from a running system carries no category; that is
%% intentional, not a gap"). `current_disk_source/2' (already used by op 3's
%% `disk_differs' diff) supplies that text via a live re-read of the
%% recorded source file.
%%
%% `has_dividers` is `false' whenever `categorize_methods' would return a
%% single, unnamed leading category (no dividers in source) OR the class has
%% no on-disk source to read at all (runtime-only class) — either way, the
%% caller falls back to its pre-existing flat/protocol-grouped rendering,
%% mirroring `document_symbols_provider.rs''s LSP-side gate so the two
%% surfaces never disagree about whether a class "has dividers".
-spec browse_categories(atom()) -> beamtalk_repl_ops:op_result().
browse_categories(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            beamtalk_repl_ops_browse_support:not_found_error(<<"browse-categories">>, ClassName);
        _ClassPid ->
            %% Resolve the source file the SAME way
            %% `save-section` does (`beamtalk_repl_loader_changelog_entries:class_source_file/1`
            %% — the ADR 0082 install-hook resolver, not this module's
            %% `origin_module/2` + `source_file_of/1` reflection pair used by
            %% ops 1-4). Divergent resolvers for a browse-then-save round trip
            %% could read one file and write another for the same class (e.g.
            %% a protocol class, where `origin_module/2` maps to the
            %% protocol's defining module); a single shared resolver makes
            %% that structurally impossible.
            SourceFile =
                case
                    beamtalk_repl_loader_changelog_entries:class_source_file(
                        atom_to_binary(ClassName, utf8)
                    )
                of
                    nil -> null;
                    Path when is_binary(Path) -> Path
                end,
            case current_disk_source(SourceFile, ClassName) of
                undefined ->
                    {value, #{<<"has_dividers">> => false, <<"categories">> => []}};
                Source when is_binary(Source) ->
                    categorize_from_source(Source, ClassName)
            end
    end.

-spec categorize_from_source(binary(), atom()) -> beamtalk_repl_ops:op_result().
categorize_from_source(Source, ClassName) ->
    case beamtalk_compiler:categorize_methods(Source, ClassName) of
        {ok, Categories} ->
            {value, category_view(Categories)};
        {error, class_not_found, _Message} ->
            %% The recorded source file no longer declares this class (e.g.
            %% renamed/moved out from under the live class) — degrade to "no
            %% grouping available" rather than failing the browse, matching
            %% `class_definition_disk_differs/1''s "nothing to compare" stance.
            {value, #{<<"has_dividers">> => false, <<"categories">> => []}};
        {error, Reason, Message} ->
            beamtalk_repl_ops_browse_support:arg_error(
                <<"browse-categories">>,
                iolist_to_binary([atom_to_binary(Reason, utf8), <<": ">>, Message])
            )
    end.

%% Wire shape: `#{has_dividers, categories}'. `has_dividers' lets the client
%% branch without re-deriving the "any category named" check itself.
-spec category_view([map()]) -> map().
category_view(Categories) ->
    HasDividers = lists:any(fun(#{name := Name}) -> Name =/= undefined end, Categories),
    #{
        <<"has_dividers">> => HasDividers,
        <<"categories">> => [category_row(C) || C <- Categories]
    }.

-spec category_row(map()) -> map().
category_row(#{name := Name, methods := Methods}) ->
    #{
        <<"name">> => name_or_null(Name),
        <<"methods">> => [method_row(M) || M <- Methods]
    }.

-spec name_or_null(binary() | undefined) -> binary() | null.
name_or_null(undefined) -> null;
name_or_null(Name) when is_binary(Name) -> Name.

-spec method_row(map()) -> map().
method_row(#{selector := Selector, side := Side}) ->
    #{
        <<"selector">> => Selector,
        <<"side">> => atom_to_binary(Side, utf8)
    }.

%% disk_differs (browse-method-source, op 3): true when the live per-method
%% source is absent from the *current* on-disk class source — the signature of
%% an unflushed live `>>` patch (ADR 0082). null when there is no static source
%% to compare (file-less class, or a sourceless runtime method whose source is
%% itself null). Heuristic: the disk store is whole-class source text and
%% contains method bodies, so a patched body that no longer matches the disk
%% text reads as `differs`; a whitespace-only reformat could read as a false
%% positive, which is acceptable for a "you may be viewing unflushed state" cue.
%% The class-definition pane (op 4) does NOT use this — see
%% `class_definition_disk_differs/1`.
%%
%% The comparison source is a *live re-read* of the on-disk class
%% file (`current_disk_source/2`), not the load-time snapshot held in
%% `beamtalk_workspace_meta`. The snapshot goes stale under out-of-band writes
%% (an external editor, or another session flushing the file) and is mutated by
%% in-memory `>>` patches (`load_recompiled_method/7` appends the patched body
%% to it), both of which let `disk_differs` under-report divergence. Reading the
%% file each browse keeps the signal pinned to the actual disk state; the
%% snapshot remains the fallback when no source file is on record or the read
%% fails. `SourceFile` is threaded in from the caller (already resolved there)
%% so the divergence diff does not re-derive it with another class-process call.
-spec disk_differs(binary() | null, atom(), binary() | null) -> boolean() | null.
disk_differs(_SourceFile, _ClassName, null) ->
    null;
disk_differs(SourceFile, ClassName, ImageText) when is_binary(ImageText) ->
    case current_disk_source(SourceFile, ClassName) of
        undefined ->
            null;
        DiskSource when is_binary(DiskSource) ->
            %% The image text is a fragment of the whole-class disk source; it
            %% "differs" when the disk source does not contain it verbatim.
            binary:match(DiskSource, ImageText) =:= nomatch
    end.

%% The class' current on-disk source for the divergence diff. Prefer a
%% live re-read of the recorded source file so the comparison reflects the file
%% as it is *now*, not as it was at load time. Falls back to the in-memory
%% load-time snapshot when the class has no source file on record or the file
%% cannot be read (deleted/moved/permissions) — degrading to the previous
%% behaviour rather than dropping the signal entirely.
-spec current_disk_source(binary() | null, atom()) -> binary() | undefined.
current_disk_source(null, ClassName) ->
    disk_source(ClassName);
current_disk_source(Path, ClassName) when is_binary(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            Bin;
        {error, Reason} ->
            disk_read_fallback(Reason, Path, ClassName)
    end.

%% A vanished/moved file (`enoent`/`enotdir`) is the expected race when a tab
%% outlives its on-disk class — fall back to the snapshot quietly. Any other
%% error (`eacces`, I/O) is worth surfacing: behaviour is unchanged (still the
%% snapshot fallback), but a log line makes a permission/FS problem diagnosable.
-spec disk_read_fallback(file:posix() | badarg | terminated | system_limit, binary(), atom()) ->
    binary() | undefined.
disk_read_fallback(Reason, _Path, ClassName) when Reason =:= enoent; Reason =:= enotdir ->
    disk_source(ClassName);
disk_read_fallback(Reason, Path, ClassName) ->
    ?LOG_WARNING(
        "browse-method-source: disk_differs falling back to snapshot for ~p; "
        "could not read ~ts: ~p",
        [ClassName, Path, Reason],
        #{domain => [beamtalk, runtime]}
    ),
    disk_source(ClassName).

-spec disk_source(atom()) -> binary() | undefined.
disk_source(ClassName) ->
    NameBin = atom_to_binary(ClassName, utf8),
    case beamtalk_workspace_meta:get_class_source(NameBin) of
        undefined ->
            undefined;
        Src when is_list(Src) ->
            %% Guard the error/incomplete tuple the same way `backing_source/1`
            %% does, so a malformed stored source never leaks a non-binary into
            %% the disk_differs comparison.
            case unicode:characters_to_binary(Src) of
                B when is_binary(B) -> B;
                _ -> list_to_binary(Src)
            end
    end.

%%% ====================================================================
%%% Validation
%%% ====================================================================

%% Optional `module` param for `browse-native-source` — a standalone
%% native module key (no backing class). `none` when absent (caller falls back to
%% the `class` form); an error when present but not a loaded module. Resolved to
%% an existing atom only (untrusted client input must not grow the atom table).
-spec validate_module(map()) -> {ok, atom()} | none | {error, binary()}.
validate_module(Params) ->
    case beamtalk_repl_params:nonempty_binary(Params, <<"module">>) of
        {ok, Mod} -> resolve_module(Mod);
        error -> none
    end.

%% Resolve a module-name binary to an atom only if it names a loaded module. An
%% unknown module is a structured not-found error, never an atom-table growth.
-spec resolve_module(binary()) -> {ok, atom()} | {error, binary()}.
resolve_module(Mod) ->
    case beamtalk_repl_params:existing_atom_param(Mod, {error, fun module_not_found_message/1}) of
        {ok, Module} ->
            case code:is_loaded(Module) of
                {file, _} -> {ok, Module};
                false -> {error, module_not_found_message(Mod)}
            end;
        {error, _} = Err ->
            Err
    end.

-spec module_not_found_message(binary()) -> binary().
module_not_found_message(Mod) ->
    iolist_to_binary([<<"module `">>, Mod, <<"` not found">>]).

-spec validate_class(map()) -> {ok, atom()} | {error, binary()}.
validate_class(Params) ->
    case beamtalk_repl_params:nonempty_binary(Params, <<"class">>) of
        {ok, Cls} -> resolve_class(Cls);
        error -> {error, <<"`class` (non-empty string) is required">>}
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
    %% Untrusted client input must not grow the VM atom table. A selector the
    %% runtime has never seen reads as the sentinel, which yields a not-found
    %% (null source) result rather than a validation failure — as in
    %% beamtalk_repl_ops_nav.
    case beamtalk_repl_params:nonempty_binary(Params, <<"selector">>) of
        {ok, Sel} -> beamtalk_repl_params:existing_atom_param(Sel, sentinel);
        error -> {error, <<"`selector` (non-empty string) is required">>}
    end.

%% Resolve a class-name binary to an atom only if it names a known class. An
%% unknown class is a structured not-found error, not an atom-table growth.
-spec resolve_class(binary()) -> {ok, atom()} | {error, binary()}.
resolve_class(Cls) ->
    case
        beamtalk_repl_params:existing_atom_param(
            Cls, {error, fun beamtalk_repl_ops_browse_support:class_not_found_message/1}
        )
    of
        {ok, Name} ->
            case beamtalk_runtime_api:whereis_class(Name) of
                undefined -> {error, beamtalk_repl_ops_browse_support:class_not_found_message(Cls)};
                _Pid -> {ok, Name}
            end;
        {error, _} = Err ->
            Err
    end.

%%% ====================================================================
%%% Shared reflection helpers (all field-reflection only, no user code)
%%% ====================================================================

-spec source_file_for_class(atom()) -> binary() | null.
source_file_for_class(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined -> null;
        Pid -> beamtalk_repl_row:source_file_of(beamtalk_runtime_api:module_name(Pid))
    end.

-spec mod_name_for_class(atom()) -> atom().
mod_name_for_class(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined -> undefined;
        Pid -> beamtalk_runtime_api:module_name(Pid)
    end.

%% Class category = the declared package (ADR 0070), read via field reflection —
%% never by sending the class a method. `null` when no package is declared (the
%% browser groups these under "Uncategorized", Pharo convention).
-spec category_of(atom()) -> binary() | null.
category_of(ModName) ->
    case beamtalk_module_name:package_of(ModName) of
        undefined -> null;
        Pkg -> Pkg
    end.

-spec class_doc(pid()) -> binary() | null.
class_doc(Pid) ->
    case beamtalk_repl_ops_browse_support:safe_class_call(Pid, get_doc) of
        D when is_binary(D) -> D;
        _ -> null
    end.

-spec side_to_binary(boolean()) -> binary().
side_to_binary(true) -> <<"class">>;
side_to_binary(false) -> <<"instance">>.
