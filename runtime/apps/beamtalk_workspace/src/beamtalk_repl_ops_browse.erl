%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_browse).

%%% **DDD Context:** REPL Session Context (System Browser bridge)

-moduledoc """
Op handlers for the System Browser browse facade (ADR 0096, BT-2488).

The LiveView IDE's System Browser (ADR 0017 Phase 3, epic BT-2482) renders the
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
| `browse-native-source` | read-only native pane | `{value, NativeSource}` |
| `browse-native-modules` | native-modules section | `{value, [NativeModuleRow]}` |
| `browse-type-aliases` | type-aliases section | `{value, [AliasRow]}` |

## Term contract (BT-2399)

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

-export([handle/4, handle_term/4, describe_ops/0, source_origin_of/2]).

%% BT-2670: the editable-native-target resolver, called by the save-native-source
%% op (`beamtalk_repl_ops_load`) to re-derive — server-side, never trusting the
%% client — whether a native module is a project-owned, editable `.erl` and where
%% its source lives. Authorization seam: only `<<"project">>` is writable.
-export([native_module_editable_target/1]).

%% BT-2732: the ADR 0056 `self delegate` callers of a native module — the
%% complement of the explicit `(Erlang <module>)` FFI callers
%% `beamtalk_xref:callers_of_native_module/1` reports. Called by the
%% `callers_of_native_module` nav op (`beamtalk_repl_ops_nav`), which merges both
%% sets, so a native module that backs an actor class lists that class's
%% delegating methods under "Callers".
-export([delegate_callers_of_native_module/1]).

-ifdef(TEST).
%% Pure helpers exercised directly in EUnit (BT-2578): clause parsing and the
%% delegate-source marker have no live-class dependency. BT-2643: the
%% source_origin (classification) / package (name) split helpers. BT-2732: the
%% `dispatch_<selector>` export → `self delegate` selector recovery. BT-2903:
%% the seeding-boundary exclusion + row-shaping helpers behind
%% `browse-type-aliases`.
-export([
    handle_call_clause_lines/1,
    clause_selector/1,
    delegate_exported/2,
    self_delegate_selectors/1,
    package_of/2,
    package_of_module/1,
    alias_visible/2,
    alias_row/3,
    package_type_aliases/1
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
    end;
handle_term(<<"browse-native-source">>, Params, _Msg, _SessionPid) ->
    %% BT-2648: the native pane can be keyed by a standalone native `module`
    %% (a dependency's hand-written `.erl` with no `native:` class to back it,
    %% surfaced by `browse-native-modules`) as an alternative to a `class`. The
    %% `module` form takes precedence; the `class` form is the original BT-2578
    %% path (resolve a native class's backing module).
    case validate_module(Params) of
        {ok, ModuleName} ->
            browse_native_module_source(ModuleName, optional_selector(Params));
        none ->
            case validate_class(Params) of
                {ok, ClassName} ->
                    browse_native_source(ClassName, optional_selector(Params));
                {error, Reason} ->
                    arg_error(<<"browse-native-source">>, Reason)
            end;
        {error, Reason} ->
            arg_error(<<"browse-native-source">>, Reason)
    end;
handle_term(<<"browse-native-modules">>, _Params, _Msg, _SessionPid) ->
    {value, browse_native_modules()};
handle_term(<<"browse-type-aliases">>, _Params, _Msg, _SessionPid) ->
    {value, browse_type_aliases()}.

-doc "Advertise the browse ops in `describe`.".
-spec describe_ops() -> map().
describe_ops() ->
    #{
        <<"browse-classes">> => #{<<"params">> => []},
        <<"browse-protocols">> => #{<<"params">> => [<<"class">>, <<"side">>]},
        <<"browse-method-source">> => #{
            <<"params">> => [<<"class">>, <<"side">>, <<"selector">>]
        },
        <<"browse-class-definition">> => #{<<"params">> => [<<"class">>]},
        %% `selector` is optional: present → also resolve the matching
        %% `handle_call` clause; absent → whole-module view (BT-2578). BT-2648:
        %% `module` is an alternative key (a standalone native module with no
        %% backing class); one of `class` / `module` is required.
        <<"browse-native-source">> => #{<<"params">> => [<<"class">>, <<"module">>]},
        %% BT-2648: enumerate a loaded package's hand-written native Erlang
        %% modules (no params).
        <<"browse-native-modules">> => #{<<"params">> => []},
        %% BT-2903 (ADR 0108 Phase 8): enumerate every loaded package's
        %% declared `type` aliases (no params).
        <<"browse-type-aliases">> => #{<<"params">> => []}
    }.

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
    %% BT-2557: the set of loaded TestCase subclasses, computed once, so each row
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
        ModName = origin_module(Name, beamtalk_runtime_api:module_name(Pid)),
        SourceFile = source_file_of(ModName),
        SourceOrigin = source_origin_of(ModName, SourceFile),
        {true, #{
            <<"name">> => atom_to_binary(Name, utf8),
            <<"superclass">> => atom_or_null(Super),
            <<"category">> => category_of(ModName),
            <<"comment">> => first_line(class_doc(Pid)),
            <<"sealed">> => safe_bool(fun() -> beamtalk_runtime_api:is_sealed(Pid) end),
            <<"typed">> => safe_bool(fun() -> beamtalk_runtime_api:is_typed(Pid) end),
            <<"abstract">> => safe_bool(fun() -> beamtalk_runtime_api:is_abstract(Pid) end),
            <<"internal">> => safe_bool(fun() -> beamtalk_runtime_api:is_internal(Pid) end),
            <<"source_file">> => SourceFile,
            <<"origin">> => origin_of(SourceFile),
            <<"source_origin">> => SourceOrigin,
            <<"package">> => package_of(ModName, SourceOrigin),
            <<"is_test">> => sets:is_element(Name, TestClasses),
            %% BT-2615: protocol class objects (ADR 0068) are sealed abstract
            %% subclasses of Protocol with no declared package, so they would
            %% otherwise fall into the "(uncategorized)" bucket. Flag them so the
            %% browser groups them under a dedicated "Protocols" category — pure
            %% registry reflection, mirroring the `is_test` → "Tests" treatment.
            <<"is_protocol">> => safe_bool(fun() -> beamtalk_runtime_api:is_protocol(Name) end)
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
    %% BT-2615: resolve the protocol's defining module (else the shared
    %% `beamtalk_protocol_object` dispatch module) so the method rows' source
    %% badges match where the protocol actually lives (stdlib vs project).
    ModName = origin_module(ClassName, mod_name_for_class(ClassName)),
    SourceFile = source_file_of(ModName),
    %% BT-2735: resolve the class pid once here (not per selector) so
    %% `selector_row` can enrich each `synthetic` row with its resolved
    %% signature + doc for a VS Code-style method-list hover. `undefined` when
    %% the class is not live — rows then carry null signature/doc and the hover
    %% falls back to the bare selector.
    ClassPid = beamtalk_runtime_api:whereis_class(ClassName),
    SelectorRows = [
        selector_row(ClassName, ClassSide, Sel, SourceFile, ModName, ClassPid)
     || Sel <- Selectors
    ],
    %% BT-2615: a protocol class object (ADR 0068) carries no instance methods of
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
%% `required_class_methods` (BT-2615). The members are signatures, not implemented
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
    SourceOrigin = source_origin_of(ModName, SourceFile),
    #{
        <<"selector">> => atom_to_binary(Selector, utf8),
        <<"line">> => null,
        <<"source_status">> => <<"unindexed_runtime_fun">>,
        <<"origin">> => <<"runtime">>,
        <<"source_origin">> => SourceOrigin,
        <<"package">> => package_of(ModName, SourceOrigin),
        %% BT-2735: a required member has no implemented method to resolve, so
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
    SourceOrigin = source_origin_of(ModName, SourceFile),
    {Doc, Signature} = row_doc_signature(ClassPid, ClassSide, Selector, SourceStatus),
    #{
        <<"selector">> => atom_to_binary(Selector, utf8),
        <<"line">> => Line,
        <<"source_status">> => atom_to_binary(SourceStatus, utf8),
        <<"origin">> => origin_for_provenance(Provenance, SourceFile),
        <<"source_origin">> => SourceOrigin,
        <<"package">> => package_of(ModName, SourceOrigin),
        %% BT-2735: signature + first-line-of-doc feed the method-row hover so a
        %% hover conveys what the method is without a click. Resolved only for
        %% `synthetic` rows (see `row_doc_signature/4`) — the client renders the
        %% full doc's first line and falls back to the selector when both null.
        <<"signature">> => Signature,
        <<"doc">> => Doc,
        %% carried internally for grouping; stripped before emit
        '__protocol__' => protocol_for_selector(Selector, Info, Provenance, SourceStatus, ClassSide)
    }.

%% BT-2735: a selector row's hover signature + doc, resolved via the same
%% hierarchy walk `:help` uses (`beamtalk_repl_docs:method_doc_signature_resolved/3`,
%% shipped in BT-2714) — so a compiler-derived `spawn` inherits `Actor`'s curated
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
%% `__source__`) — the patch-aware per-method text (BT-2196). source_status from
%% xref. `disk_differs` compares the image body against a live re-read of the
%% on-disk class source (BT-2567; `current_disk_source/2`).
-spec browse_method_source(atom(), boolean(), atom()) -> beamtalk_repl_ops:op_result().
browse_method_source(ClassName, ClassSide, Selector) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            not_found_error(<<"browse-method-source">>, ClassName);
        ClassPid ->
            {Line, SourceStatus, Provenance} = info_fields(
                beamtalk_xref:method_info(ClassName, ClassSide, Selector)
            ),
            {Source, Doc, Signature} =
                method_text_fields(ClassPid, ClassSide, Selector, SourceStatus),
            SourceFile = source_file_for_class(ClassName),
            ModName = beamtalk_runtime_api:module_name(ClassPid),
            SourceOrigin = source_origin_of(ModName, SourceFile),
            {value, #{
                <<"class">> => atom_to_binary(ClassName, utf8),
                <<"side">> => side_to_binary(ClassSide),
                <<"selector">> => atom_to_binary(Selector, utf8),
                <<"source">> => Source,
                %% BT-2578: true when this is a `self delegate` method (ADR 0056)
                %% on a native-backed class — the real implementation lives in a
                %% `handle_call` clause of the backing module, reachable via
                %% `browse-native-source`. The signal is the compiler's own
                %% `is_self_delegate` decision, made visible at runtime: the native
                %% facade generates a `dispatch_<selector>` function ONLY for a
                %% `self delegate` method (native_facade.rs), so its export is a
                %% precise marker — no body-text heuristic.
                <<"native_delegate">> => is_native_delegate(ModName, ClassSide, Selector),
                %% BT-2558: the method's `///` doc-comment and rendered
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
                <<"origin">> => origin_for_provenance(Provenance, SourceFile),
                <<"source_origin">> => SourceOrigin,
                <<"package">> => package_of(ModName, SourceOrigin),
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
%% BT-2614/BT-2714: synthetic methods (compiler-injected — value-type
%% auto-accessors and actor `new`/`new:`/`spawn`/`spawn:` constructors) carry no
%% editable user source. `source` stays null so the browser badges them read-only
%% with no `[source]` jump — and, crucially, so a class-side `spawn` does NOT
%% surface `Actor`'s real `spawn` body under the subclass's synthetic row (an
%% inconsistent, misleading "source" for a generated method).
%%
%% BT-2714: but doc/signature DO resolve — via the same hierarchy walk `Beamtalk
%% help:` uses (`method_doc_signature_resolved/3`) — so the read-only pane shows
%% the method's real curated docs (the actor entry points inherit `Actor`'s
%% docs; a value accessor shows its generated signature) instead of a blank
%% pane. Reusing the `help:` resolver guarantees the browser and `:help` agree.
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
    case safe_class_call(ClassPid, Call) of
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
%% is field reflection (ADR 0035) — names + default expressions, no user code.
%% The definition string is the class skeleton (`Super subclass: Name` + state
%% slots), method bodies excluded (Pharo convention), synthesized from reflection
%% for every loaded class. File-less (ClassBuilder) and stdlib classes keep the
%% skeleton; `origin = runtime` (not the definition) is what flags no disk source.
-spec browse_class_definition(atom()) -> beamtalk_repl_ops:op_result().
browse_class_definition(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            not_found_error(<<"browse-class-definition">>, ClassName);
        ClassPid ->
            Super = beamtalk_runtime_api:superclass(ClassPid),
            %% BT-2615: a protocol class object's dispatch module is the shared
            %% `beamtalk_protocol_object`; resolve its defining module so the
            %% definition pane's category/origin/source_origin are accurate.
            ModName = origin_module(ClassName, beamtalk_runtime_api:module_name(ClassPid)),
            SourceFile = source_file_of(ModName),
            SourceOrigin = source_origin_of(ModName, SourceFile),
            State = state_slots(ClassPid),
            Definition = class_definition_text(ClassName, Super, State),
            %% BT-2578: native-backed classes (ADR 0056) carry their backing
            %% Erlang module name so the System Browser can badge them and offer
            %% `browse-native-source`. Read from the facade module's
            %% `__beamtalk_meta/0` (`native => true, backing_module => atom()`).
            NativeMeta = native_meta_of(ModName),
            {value, #{
                <<"class">> => atom_to_binary(ClassName, utf8),
                <<"superclass">> => atom_or_null(Super),
                <<"category">> => category_of(ModName),
                <<"definition">> => Definition,
                <<"state">> => State,
                <<"comment">> => class_doc(ClassPid),
                <<"native">> => meta_is_native(NativeMeta),
                <<"backing_module">> => atom_or_null(meta_backing_module(NativeMeta)),
                %% BT-2605: reflected class modifiers for the IDE's editor-header
                %% modifier badges. The synthesized `definition` skeleton above
                %% carries no leading modifier keywords, so these come from the
                %% same runtime reflection op 1 (`browse-classes`) uses — not a
                %% string parse.
                <<"sealed">> => safe_bool(fun() -> beamtalk_runtime_api:is_sealed(ClassPid) end),
                <<"typed">> => safe_bool(fun() -> beamtalk_runtime_api:is_typed(ClassPid) end),
                <<"abstract">> => safe_bool(fun() -> beamtalk_runtime_api:is_abstract(ClassPid) end),
                %% BT-2639: a structural reflection boolean (not a header
                %% string-sniff) so the System Browser can reliably render the
                %% protocol-definition action row (Required methods / Conforming
                %% classes). Mirrors the `is_protocol` field op 1
                %% (`browse-classes`) already carries on each class row.
                <<"is_protocol">> => safe_bool(fun() ->
                    beamtalk_runtime_api:is_protocol(ClassName)
                end),
                <<"origin">> => origin_of(SourceFile),
                <<"source_origin">> => SourceOrigin,
                <<"package">> => package_of(ModName, SourceOrigin),
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
%% excluded. Derived purely from the loaded class' reflected superclass + state
%% (ADR 0035), so it is available for *every* loaded class — file-backed user
%% classes, stdlib classes (whose BEAM carries no `beamtalk_source` attribute, so
%% `source_file_of` is null), and file-less ClassBuilder classes alike. The
%% runtime/disk distinction lives in `origin` (null SourceFile → runtime), not in
%% the skeleton: a class that is loaded always has a representable shape, and
%% returning null here only stranded stdlib classes on an empty editor.
-spec class_definition_text(atom(), atom() | none, [map()]) -> binary().
class_definition_text(ClassName, Super, State) ->
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
%%% Op 5 — browse-native-source (BT-2578)
%%% ====================================================================

%% The backing Erlang source for a `native:` class (ADR 0056). A native class
%% only has Beamtalk *delegating* methods (`=> self delegate`); the real logic is
%% gen_server callbacks (`handle_call/3`, `handle_info/2`) in a SEPARATE module
%% (e.g. `Subprocess` → `beamtalk_subprocess`). So the reliable unit is the whole
%% backing module, with a best-effort `handle_call` clause line-map and, when a
%% `selector` is given, the matching clause (the selector↔clause mapping is loose
%% — e.g. `Subprocess>>readLine` replies from `handle_info`, not its `handle_call`
%% clause). Read-only; `editable` reflects `source_origin` (project-owned native
%% is editable in a future R/W phase, stdlib/dependency never).
-spec browse_native_source(atom(), atom() | undefined) -> beamtalk_repl_ops:op_result().
browse_native_source(ClassName, Selector) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined ->
            not_found_error(<<"browse-native-source">>, ClassName);
        ClassPid ->
            ModName = beamtalk_runtime_api:module_name(ClassPid),
            NativeMeta = native_meta_of(ModName),
            case meta_backing_module(NativeMeta) of
                none ->
                    arg_error(
                        <<"browse-native-source">>,
                        iolist_to_binary([
                            <<"class `">>,
                            atom_to_binary(ClassName, utf8),
                            <<"` is not native-backed">>
                        ])
                    );
                Backing when is_atom(Backing) ->
                    native_source_value(ClassName, ModName, Backing, Selector)
            end
    end.

-spec native_source_value(atom(), atom(), atom(), atom() | undefined) ->
    beamtalk_repl_ops:op_result().
native_source_value(ClassName, ModName, Backing, Selector) ->
    {BackingFile, Content} = backing_source(Backing),
    %% `source_origin` keys editability off where the .erl lives: stdlib and
    %% dependency native are read-only; project-owned native is the seam where a
    %% future R/W phase enables editing (BT-2578 out-of-scope follow-up). Also
    %% require the source content to be present: a project class whose `.erl` was
    %% stripped from the release (or is unreadable) has nothing to edit even
    %% though its build-time path is known, so it must not advertise `editable`.
    SourceOrigin = source_origin_of(ModName, BackingFile),
    Clauses = handle_call_clause_lines(Content),
    {value, #{
        <<"class">> => atom_to_binary(ClassName, utf8),
        <<"backing_module">> => atom_to_binary(Backing, utf8),
        <<"source_file">> => BackingFile,
        <<"source_origin">> => SourceOrigin,
        <<"editable">> => SourceOrigin =:= <<"project">> andalso Content =/= null,
        <<"content">> => Content,
        <<"clauses">> => Clauses,
        <<"selected_clause">> => selected_clause(Clauses, Selector)
    }}.

%% BT-2648: the read-only native pane keyed directly by a standalone native
%% module (not a `native:` class's backing module). Reuses the exact same
%% reader/clause machinery `native_source_value/4` uses — the only difference is
%% the key (a module the `browse-native-modules` enumeration surfaced) and that
%% there is no owning class, so `class` is `null` and `backing_module` is the
%% module itself. A module with no readable on-disk source degrades to
%% `content = null` (the "source not available" empty state), never an error.
-spec browse_native_module_source(atom(), atom() | undefined) ->
    beamtalk_repl_ops:op_result().
browse_native_module_source(Module, Selector) ->
    {SourceFile, Content} = backing_source(Module),
    SourceOrigin = native_module_origin(Module, SourceFile),
    Clauses = handle_call_clause_lines(Content),
    {value, #{
        <<"class">> => null,
        <<"backing_module">> => atom_to_binary(Module, utf8),
        <<"source_file">> => SourceFile,
        <<"source_origin">> => SourceOrigin,
        %% BT-2670: project-owned native modules are editable (edit → compile →
        %% reload → write-back, gated to the Owner via the `save-native-source`
        %% op's `:execute` capability). Deps/stdlib natives stay strictly
        %% read-only. Require readable on-disk content too: a `.beam`-only
        %% (release-stripped) module has nothing to edit even though its path is
        %% known, so it must not advertise `editable`. The save op re-derives this
        %% same predicate server-side (`native_module_editable_target/1`) — the
        %% client flag is a UX hint, never the authority.
        <<"editable">> =>
            SourceOrigin =:= <<"project">> andalso SourceFile =/= null andalso Content =/= null,
        <<"content">> => Content,
        <<"clauses">> => Clauses,
        <<"selected_clause">> => selected_clause(Clauses, Selector)
    }}.

-doc """
Resolve a native module to its editable on-disk `.erl` target (BT-2670).

This is the **server-side authorization seam** for `save-native-source`: it
re-derives — independently of any client-supplied path — where the module's
source lives and whether it is a *project-owned* native that may be written. It
NEVER trusts a path from the request; the path is read from the module's own
compile info (`module_info(compile)` → `source`), so a crafted request cannot
redirect a write outside the project tree.

Returns:
  - `{ok, SourceFile}` — `SourceFile` is the absolute `.erl` path, and the module
    is project-owned (origin `<<"project">>`) with readable on-disk source.
  - `{error, not_editable}` — a deps/stdlib native, or no readable on-disk source
    (`.beam`-only / release-stripped). The caller maps this to a structured
    `#beamtalk_error{}`.
""".
-spec native_module_editable_target(atom()) -> {ok, string()} | {error, not_editable}.
native_module_editable_target(Module) when is_atom(Module) ->
    case backing_source_file(Module) of
        undefined ->
            {error, not_editable};
        Path when is_list(Path) ->
            SourceFileBin =
                case unicode:characters_to_binary(Path) of
                    B when is_binary(B) -> B;
                    _ -> list_to_binary(Path)
                end,
            case native_module_origin(Module, SourceFileBin) of
                <<"project">> -> {ok, Path};
                _ -> {error, not_editable}
            end
    end.

%%% ====================================================================
%%% Op 6 — browse-native-modules (BT-2648)
%%% ====================================================================

%% Enumerate the loaded packages' *hand-written* native Erlang modules so the
%% System Browser can navigate to a dependency's `.erl` files even when no
%% `native:` class backs them (the reported beamtalk-http case: a dependency
%% loaded without instantiating its classes is invisible to the class-keyed
%% `browse-classes`).
%%
%% **Filter rule (ADR 0072 `native_modules`, BT-2648):** a "native module" is a
%% module a Beamtalk package *declares* it ships, never the whole code path:
%%
%%   1. Walk `beamtalk_package:all/0` — every loaded OTP application that is a
%%      Beamtalk package (has a non-empty `classes` env, ADR 0070).
%%   2. For each package's app, take its declared native modules:
%%        * user / dependency packages: the generated `{native_modules, [...]}`
%%          app-env key (ADR 0072 Phase 1; the `native/*.erl` stems);
%%        * stdlib (whose `.app.src` carries no `native_modules` key): the app's
%%          OTP `modules` list filtered to non-`bt@` modules — i.e. the
%%          hand-written `beamtalk_*` backing modules, not the `bt@stdlib@*`
%%          class facades.
%%   3. EXCLUDE the auto-generated `bt@{pkg}@{class}` class facade modules by
%%      construction: those live in the OTP `modules` key, never in
%%      `native_modules`, and the stdlib filter drops the `bt@` prefix — so a
%%      class facade is never duplicated here (it is already in browse-classes).
%%
%% Each row carries the module name, on-disk source path (compile-info `source`,
%% like `backing_source/1`; `null` when no readable source → not openable),
%% package name, and origin (`project`/`dependency`/`stdlib`). Rows are sorted by
%% module name for a stable tree.
-spec browse_native_modules() -> [map()].
browse_native_modules() ->
    Packages =
        try
            beamtalk_package:all()
        catch
            _:_ -> []
        end,
    Rows = lists:flatmap(fun native_modules_of_package/1, Packages),
    %% Dedup by module name (a module declared by more than one app is listed
    %% once) then sort for a stable tree.
    Deduped = maps:values(
        lists:foldl(
            fun(#{<<"module">> := M} = Row, Acc) -> maps:put(M, Row, Acc) end,
            #{},
            Rows
        )
    ),
    lists:sort(
        fun(A, B) -> maps:get(<<"module">>, A) =< maps:get(<<"module">>, B) end,
        Deduped
    ).

%% The native-module rows for one package: resolve its owning app, read the
%% declared native modules, and build a row per module tagged with the package's
%% name + origin. A package whose app cannot be resolved contributes nothing.
-spec native_modules_of_package(binary()) -> [map()].
native_modules_of_package(PkgName) ->
    case find_app_for_package(PkgName) of
        {ok, App} ->
            Origin = package_origin(App, PkgName),
            [native_module_row(Mod, PkgName, Origin) || Mod <- package_native_modules(App)];
        error ->
            []
    end.

%% The OTP application hosting a Beamtalk package (mirrors
%% `beamtalk_package:find_app_for_package/1`, which is not exported).
-spec find_app_for_package(binary()) -> {ok, atom()} | error.
find_app_for_package(PkgName) ->
    Apps = application:loaded_applications(),
    find_app_for_package(PkgName, Apps).

-spec find_app_for_package(binary(), [{atom(), term(), term()}]) -> {ok, atom()} | error.
find_app_for_package(_PkgName, []) ->
    error;
find_app_for_package(PkgName, [{App, _Desc, _Vsn} | Rest]) ->
    case app_package_name(App) of
        PkgName -> {ok, App};
        _ -> find_app_for_package(PkgName, Rest)
    end.

%% The package name an app hosts (the package segment of its declared classes),
%% or `undefined` for a non-Beamtalk app.
-spec app_package_name(atom()) -> binary() | undefined.
app_package_name(App) ->
    case application:get_env(App, classes) of
        {ok, [#{package := Pkg} | _]} when is_atom(Pkg) -> atom_to_binary(Pkg, utf8);
        {ok, [#{package := Pkg} | _]} when is_binary(Pkg) -> Pkg;
        _ -> undefined
    end.

%% An app's declared native modules. User / dependency packages carry the
%% generated `{native_modules, [...]}` env key (ADR 0072); stdlib has none, so
%% fall back to the OTP `modules` list filtered to the non-`bt@` (hand-written)
%% modules — which excludes the `bt@stdlib@*` class facades by construction.
-spec package_native_modules(atom()) -> [atom()].
package_native_modules(App) ->
    case application:get_env(App, native_modules) of
        {ok, Mods} when is_list(Mods) ->
            [M || M <- Mods, is_atom(M)];
        _ ->
            stdlib_fallback_native_modules(App)
    end.

%% Stdlib (and any package with no `native_modules` env): the OTP `modules` list
%% minus the `bt@*` class facades — i.e. the hand-written `beamtalk_*` backing
%% modules. A non-stdlib app with no `native_modules` key (an older build) gets
%% nothing rather than its facade modules misreported as native.
-spec stdlib_fallback_native_modules(atom()) -> [atom()].
stdlib_fallback_native_modules(beamtalk_stdlib = App) ->
    case application:get_key(App, modules) of
        {ok, Mods} when is_list(Mods) ->
            [M || M <- Mods, is_atom(M), not is_bt_module(M)];
        _ ->
            []
    end;
stdlib_fallback_native_modules(_App) ->
    [].

%% True for an auto-generated `bt@{pkg}@{class}` class facade module atom.
-spec is_bt_module(atom()) -> boolean().
is_bt_module(Module) when is_atom(Module) ->
    case atom_to_binary(Module, utf8) of
        <<"bt@", _/binary>> -> true;
        _ -> false
    end.

%% Origin of a package's native modules: `stdlib` for the stdlib app, else the
%% project/dependency split from the package name vs the workspace's own package.
-spec package_origin(atom(), binary()) -> binary().
package_origin(beamtalk_stdlib, _PkgName) ->
    <<"stdlib">>;
package_origin(_App, PkgName) ->
    case project_package_name() of
        ProjectPkg when is_binary(ProjectPkg), ProjectPkg =:= PkgName ->
            <<"project">>;
        _ ->
            <<"dependency">>
    end.

%% Source origin for a standalone native module's pane. A native module is not a
%% `bt@{pkg}@{class}` atom, so `source_origin_of/2`'s package-segment path can't
%% classify it; instead, badge stdlib by app membership (the stdlib app declares
%% it) and fall back to the on-disk path check (`classify_origin/3`) for the
%% project/dependency split.
-spec native_module_origin(atom(), binary() | null) -> binary().
native_module_origin(Module, SourceFile) ->
    case is_stdlib_native_module(Module) of
        true -> <<"stdlib">>;
        false -> path_origin(SourceFile)
    end.

%% True when `Module` is one of the stdlib app's hand-written native modules.
-spec is_stdlib_native_module(atom()) -> boolean().
is_stdlib_native_module(Module) ->
    lists:member(Module, stdlib_fallback_native_modules(beamtalk_stdlib)).

%% One native-module row: module name, on-disk source path (compile-info
%% `source`, `null` when no readable source → not openable), package, and origin.
-spec native_module_row(atom(), binary(), binary()) -> map().
native_module_row(Module, PkgName, Origin) ->
    %% Path only (no `file:read_file/1`) — enumeration just needs the path +
    %% `openable` flag; the content is read lazily by `browse-native-source` when
    %% a module is actually opened. Mirrors `backing_source/1`'s path
    %% normalisation without the per-module read.
    SourceFile =
        case backing_source_file(Module) of
            undefined ->
                null;
            Path ->
                case unicode:characters_to_binary(Path) of
                    B when is_binary(B) -> B;
                    _ -> list_to_binary(Path)
                end
        end,
    #{
        <<"module">> => atom_to_binary(Module, utf8),
        <<"source_file">> => SourceFile,
        <<"package">> => PkgName,
        <<"source_origin">> => Origin,
        %% Openable iff a readable on-disk `.erl` was found (`.beam`-only /
        %% release-stripped modules are listed but not openable — the existing
        %% "Erlang source not available" path).
        <<"openable">> => SourceFile =/= null
    }.

%% The backing module's on-disk `.erl` and its content. The source path comes
%% from the module's own compile info (`module_info(compile)` → `source`), so a
%% `.beam`-only release (no shipped source) degrades to `{null, null}` —
%% "source not available" — rather than an error. A path that no longer exists
%% (moved/release-stripped) returns its known path with `null` content.
-spec backing_source(atom()) -> {binary() | null, binary() | null}.
backing_source(Backing) ->
    case backing_source_file(Backing) of
        undefined ->
            {null, null};
        Path ->
            %% `unicode:characters_to_binary/1` returns an `{error, _, _}` /
            %% `{incomplete, _, _}` tuple (not a binary) for a malformed path, which
            %% must not leak into the result map as `source_file`. Fall back to a
            %% raw Latin-1 binary so the field is always a binary. Build-machine
            %% paths are virtually always UTF-8, so the fallback is pathological-only.
            PathBin =
                case unicode:characters_to_binary(Path) of
                    B when is_binary(B) -> B;
                    _ -> list_to_binary(Path)
                end,
            case file:read_file(Path) of
                {ok, Bin} -> {PathBin, Bin};
                {error, _} -> {PathBin, null}
            end
    end.

-spec backing_source_file(atom()) -> string() | undefined.
backing_source_file(Backing) ->
    try Backing:module_info(compile) of
        Info when is_list(Info) ->
            case lists:keyfind(source, 1, Info) of
                {source, Path} when is_list(Path) -> Path;
                _ -> undefined
            end;
        _ ->
            undefined
    catch
        _:_ -> undefined
    end.

%%% ====================================================================
%%% Op 7 — browse-type-aliases (ADR 0108 Phase 8, BT-2903)
%%% ====================================================================

%% Enumerate every loaded package's declared `type` aliases (ADR 0108).
%%
%% Aliases erase entirely at compile time — no BEAM module, no live process —
%% so unlike `browse-classes` there is nothing to reflect on. The `.app`
%% file's `{type_aliases, [...]}` env key (BT-2903, `app_file.rs`
%% `AliasMetadata`/`format_type_aliases_entry`) is the only durable record of
%% a package's declarations once compiled, so this walks `beamtalk_package:
%% all/0` and reads each package's env, the same enumeration strategy
%% `browse-native-modules` uses.
%%
%% **Seeding-boundary exclusion (ADR 0108 Implementation — "System Browser /
%% LiveView IDE"):** an `internal` alias declared in a package other than the
%% browsing session's current project package is dropped — the browse-op
%% analogue of how `AliasRegistry::add_pre_loaded` (BT-2898) never seeds a
%% dependency's internal aliases into a consumer's alias table at compile
%% time. There is no live "current compilation" alias table to consult here
%% (this op runs against a loaded system, not a single in-flight compile), so
%% the equivalent guarantee is reconstructed per row instead of per seeded
%% table: an alias belonging to the current project package is visible
%% regardless of `internal` (an internal alias is usable within its own
%% package — ADR 0108 Semantics), and every other package's (including
%% stdlib's) internal aliases are excluded, mirroring the ADR's explicit
%% instruction that this filter "belongs at the seeding boundary ... not as a
%% query-time filter over `browse-type-aliases`'s result set" — the entries
%% are dropped during enumeration, before a row is ever built, not filtered
%% out of an already-built result list.
%%
%% Unlike `browse-native-modules`, rows are **not** deduped by name: a native
%% module atom is a genuine BEAM-global identity, but two independently
%% loaded packages can each legally declare an alias of the same name (only a
%% *consumer* of both would hit `add_pre_loaded`'s collision diagnostic) —
%% deduping here would silently hide one package's real declaration instead
%% of surfacing both, distinguished by their `package`/`source_origin` tags.
%%
%% **Known gap — a package with only `type` declarations (no classes) is
%% invisible here (BT-2915):** discovery walks `beamtalk_package:all/0`,
%% which recognises a package solely by a non-empty `classes` app-env key
%% (ADR 0070) — a hypothetical types-only "shared vocabulary" package with
%% zero classes would never be enumerated, so its aliases (even public ones)
%% never appear, regardless of the seeding-boundary logic above. Every
%% concrete consumer this ADR ships against (stdlib's `RestartStrategy`,
%% `JsonValue`) declares aliases alongside real classes, so this has not
%% bitten a real package yet; broadening `beamtalk_package:all/0`'s discovery
%% signal is shared infra touching every other consumer of that function
%% (`browse-native-modules`, `Package packageNameFor:`, …), so it is tracked
%% as a follow-up rather than folded into this op.
-spec browse_type_aliases() -> [map()].
browse_type_aliases() ->
    Packages =
        try
            beamtalk_package:all()
        catch
            _:_ -> []
        end,
    Rows = lists:flatmap(fun type_aliases_of_package/1, Packages),
    lists:sort(
        fun(A, B) ->
            {maps:get(<<"name">>, A), maps:get(<<"package">>, A)} =<
                {maps:get(<<"name">>, B), maps:get(<<"package">>, B)}
        end,
        Rows
    ).

%% The alias rows for one package: resolve its owning app, read its declared
%% `type_aliases` env, drop internal aliases that fail the seeding-boundary
%% check, and tag each surviving row with the package's name + origin.
%%
%% Wrapped in a try/catch — unlike `native_modules_of_package/1`, whose row
%% builder only touches `module_info/1` reflection (itself already
%% defensively wrapped in `backing_source_file/1`) — because `alias_row/3`
%% assumes each `type_aliases` env entry is a map (`maps:get/2,3` raises
%% `{badmap, _}` otherwise). That assumption holds for every entry this
%% workspace's own `beamtalk build` ever writes (`app_file.rs`
%% `format_type_aliases_entry`), but a hand-edited or toolchain-mismatched
%% `.app` file is still just a text file on disk — one malformed package must
%% not take down every other package's rows in the same response, mirroring
%% `class_row/2`'s per-class isolation.
-spec type_aliases_of_package(binary()) -> [map()].
type_aliases_of_package(PkgName) ->
    try
        case find_app_for_package(PkgName) of
            {ok, App} ->
                Origin = package_origin(App, PkgName),
                [
                    alias_row(Entry, PkgName, Origin)
                 || Entry <- package_type_aliases(App),
                    alias_visible(Entry, Origin)
                ];
            error ->
                []
        end
    catch
        Class:Reason ->
            ?LOG_WARNING(
                "browse-type-aliases: skipping package ~p: ~p:~p",
                [PkgName, Class, Reason],
                #{domain => [beamtalk, runtime]}
            ),
            []
    end.

%% An app's declared type aliases — the `.app` `{type_aliases, [...]}` env key
%% (BT-2903). `[]` for a package with no `type` declarations (most packages),
%% or one built before this key existed — no fallback reconstruction needed,
%% unlike `package_native_modules/1`'s stdlib special case, since there is no
%% legacy alias surface to recover data from.
-spec package_type_aliases(atom()) -> [map()].
package_type_aliases(App) ->
    case application:get_env(App, type_aliases) of
        {ok, Aliases} when is_list(Aliases) -> Aliases;
        _ -> []
    end.

%% Seeding-boundary exclusion (see `browse_type_aliases/0` doc): an internal
%% alias is visible only from its own package (`Origin =:= project`) — every
%% other package's internal aliases, including stdlib's, are dropped. A
%% non-internal (public) alias is always visible.
-spec alias_visible(map(), binary()) -> boolean().
alias_visible(#{internal := true}, Origin) ->
    Origin =:= <<"project">>;
alias_visible(#{internal := _}, _Origin) ->
    true;
alias_visible(_Entry, _Origin) ->
    %% No `internal` key at all (should not happen — every entry the Rust
    %% build pipeline emits carries it) — fail safe by treating as internal.
    false.

%% One AliasRow (ADR 0108: `name`, `expansion`, `doc`, `source_file`,
%% `internal`), extended with `package`/`source_origin` the way `ClassRow`
%% and `NativeModuleRow` are — the LiveView "Type Aliases" panel and the VS
%% Code sidebar both group/badge by package the same way the Classes and
%% Native panels already do.
-spec alias_row(map(), binary(), binary()) -> map().
alias_row(Entry, PkgName, Origin) ->
    #{
        <<"name">> => alias_field_binary(maps:get(name, Entry, <<>>)),
        <<"expansion">> => alias_field_binary(maps:get(expansion, Entry, <<>>)),
        <<"doc">> => alias_field_or_null(maps:get(doc, Entry, undefined)),
        <<"source_file">> => alias_field_binary(maps:get(source_file, Entry, <<>>)),
        <<"internal">> => maps:get(internal, Entry, false),
        <<"package">> => PkgName,
        <<"source_origin">> => Origin
    }.

%% `.app`-file alias fields round-trip as atoms (`name`), Erlang strings
%% (`expansion`/`source_file`/`doc`, via `escape_erlang_string` +
%% `file:consult`), or `undefined` (`doc`) — never binaries, since
%% `app_file.rs` emits a literal `.app` Erlang term, not a JSON payload.
%% Normalise whichever shape comes back to the binary the BT-2399 term
%% contract requires.
-spec alias_field_binary(atom() | binary() | string()) -> binary().
alias_field_binary(V) when is_binary(V) ->
    V;
alias_field_binary(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
alias_field_binary(V) when is_list(V) ->
    case unicode:characters_to_binary(V) of
        B when is_binary(B) -> B;
        _ -> list_to_binary(V)
    end;
alias_field_binary(_) ->
    <<>>.

-spec alias_field_or_null(undefined | atom() | binary() | string()) -> binary() | null.
alias_field_or_null(undefined) ->
    null;
alias_field_or_null(V) ->
    alias_field_binary(V).

%% Best-effort `handle_call` clause line-map: each clause whose first message
%% element is a concrete selector (a quoted atom like `'writeLine:'` or a bare
%% lowercase atom like `readLine`) becomes `#{selector, line}`. Generic clauses
%% — `handle_call({Selector, Args, _}, …)`, the catch-all `handle_call(Msg, …)`
%% — bind variables (uppercase / `_`), match no selector, and are skipped.
%% Matches single-line clause heads only; a head split across multiple lines is
%% not captured (the selector and its trailing comma must appear on one line).
-spec handle_call_clause_lines(binary() | null) -> [map()].
handle_call_clause_lines(null) ->
    [];
handle_call_clause_lines(Content) when is_binary(Content) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    {Rows, _} = lists:foldl(
        fun(Line, {Acc, N}) ->
            case clause_selector(Line) of
                {ok, Sel} -> {[#{<<"selector">> => Sel, <<"line">> => N} | Acc], N + 1};
                none -> {Acc, N + 1}
            end
        end,
        {[], 1},
        Lines
    ),
    lists:reverse(Rows).

%% A line's `handle_call` selector, or `none`. The first tuple element of the
%% call message, captured whether written as a quoted atom (`'writeLine:'`) or a
%% bare lowercase atom (`readLine`). The lowercase-initial requirement excludes
%% the generic clauses that bind variables (`{Selector, Args, _}`, `Msg`). A
%% single capture group avoids `re`'s truncation of trailing unmatched groups.
%%
%% BT-2582: this is one half of a shared selector→clause-line definition. The
%% Rust LSP mirrors this algorithm in
%% `definition_provider::clause_selector` (so go-to-implementation lands on the
%% same clause the System Browser jump does), and both are pinned to the shared
%% conformance corpus
%% `test/fixtures/handle_call_clause_corpus.json`
%% (asserted here by `clause_selector_corpus_test/0` and on the Rust side by
%% `definition_provider::tests::clause_selector_matches_shared_corpus`). Keep the
%% regex and the Rust matcher in lockstep; extend the corpus when adding a case.
-spec clause_selector(binary()) -> {ok, binary()} | none.
clause_selector(Line) ->
    %% Anchored to line start (after optional indentation): a `handle_call`
    %% mention mid-line — inside a comment (`%% see handle_call({foo, …`) or an
    %% assignment (`R = handle_call({…`) — is not a clause head and must not
    %% produce a spurious clause row.
    Pattern = <<"^\\s*handle_call\\(\\{\\s*'?([a-z][A-Za-z0-9_:]*)'?\\s*,">>,
    case re:run(Line, Pattern, [{capture, all_but_first, binary}]) of
        {match, [Sel]} when byte_size(Sel) > 0 -> {ok, Sel};
        _ -> none
    end.

%% The clause row for a requested selector, or `null` (no selector asked, or no
%% matching `handle_call` clause — a delegate whose work lives elsewhere).
-spec selected_clause([map()], atom() | undefined) -> map() | null.
selected_clause(_Clauses, undefined) ->
    null;
selected_clause(Clauses, Selector) ->
    SelBin = atom_to_binary(Selector, utf8),
    case lists:search(fun(#{<<"selector">> := S}) -> S =:= SelBin end, Clauses) of
        {value, Row} -> Row;
        false -> null
    end.

%% Native facade metadata (ADR 0056): the facade module exports
%% `__beamtalk_meta/0` carrying `native => true` and `backing_module => atom()`.
%% Non-native / non-Beamtalk modules don't export it → `#{}`.
-spec native_meta_of(atom()) -> map().
native_meta_of(ModName) when is_atom(ModName) ->
    case erlang:function_exported(ModName, '__beamtalk_meta', 0) of
        true ->
            try ModName:'__beamtalk_meta'() of
                Meta when is_map(Meta) -> Meta;
                _ -> #{}
            catch
                _:_ -> #{}
            end;
        false ->
            #{}
    end.

-spec meta_is_native(map()) -> boolean().
meta_is_native(Meta) ->
    maps:get(native, Meta, false) =:= true.

-spec meta_backing_module(map()) -> atom() | none.
meta_backing_module(Meta) ->
    case maps:get(backing_module, Meta, none) of
        M when is_atom(M), M =/= none, M =/= undefined -> M;
        _ -> none
    end.

%% True when `Selector` (instance side) is a `self delegate` method on the native
%% facade `ModName`. The compiler emits a `dispatch_<selector>` function ONLY for
%% a `self delegate` method (native_facade.rs collects `is_self_delegate()` methods
%% and generates one dispatch fun each), so the presence of that export is the
%% compiler's own decision surfaced at runtime — not a body-text guess. Class-side
%% selectors are never delegated this way, so they are always `false`.
-spec is_native_delegate(atom(), boolean(), atom()) -> boolean().
is_native_delegate(ModName, false, Selector) when is_atom(ModName) ->
    Exports =
        try
            ModName:module_info(exports)
        catch
            _:_ -> []
        end,
    delegate_exported(Exports, Selector);
is_native_delegate(_ModName, _ClassSide, _Selector) ->
    false.

%% Pure: does `Exports` contain a `dispatch_<Selector>` function (any arity)? The
%% dispatch fun name embeds the selector verbatim, keyword colon included
%% (`dispatch_readLine`, `'dispatch_writeLine:'`), so an exact name compare is
%% exact — no arity arithmetic, no atom-table growth.
-spec delegate_exported([{atom(), arity()}], atom()) -> boolean().
delegate_exported(Exports, Selector) ->
    Target = "dispatch_" ++ atom_to_list(Selector),
    lists:any(fun({Name, _Arity}) -> atom_to_list(Name) =:= Target end, Exports).

%%% ====================================================================
%%% Native-module delegate callers (BT-2732)
%%% ====================================================================

-doc """
The Beamtalk methods that delegate into native module `Module` via ADR 0056
`self delegate` — the complement of the explicit `(Erlang <module>)` FFI callers
`beamtalk_xref:callers_of_native_module/1` reports (BT-2732).

A `native:` class compiles each `self delegate` method into a `dispatch_<selector>`
function on its facade module, routed through `beamtalk_actor:sync_send/3`. Those
sends are generated dispatch, not `erlang_ffi` sends, so the xref FFI index never
sees them and the reverse "Callers" view would miss the most common native module
in a project — the gen_server that backs an actor class. Here we walk the loaded
classes, keep those whose backing native module is `Module`, and surface each
class's `self delegate` instance methods as `native_caller_row()`s anchored at the
delegating method's own header line (the same shape the FFI callers use, so the
IDE renders both together).

Class-side selectors are never delegated this way, so every row is instance-side.
Returns `[]` for a module that backs no loaded class (or the unresolved-module
sentinel), so a module with neither FFI callers nor delegating classes keeps the
honest empty state.
""".
-spec delegate_callers_of_native_module(atom()) -> [beamtalk_xref:native_caller_row()].
delegate_callers_of_native_module(none) ->
    %% `none` is `meta_backing_module/1`'s "no backing module" sentinel, never a
    %% real native module name. Guarding it here keeps a `module="none"` query
    %% from match-binding against that sentinel in `delegate_rows_for_class/2` and
    %% mis-attributing the delegates of a class that reports no backing module.
    [];
delegate_callers_of_native_module(Module) when is_atom(Module) ->
    ClassPids =
        try
            beamtalk_runtime_api:all_classes()
        catch
            _:_ -> []
        end,
    lists:flatmap(fun(Pid) -> delegate_rows_for_class(Pid, Module) end, ClassPids).

%% One `native_caller_row()` per `self delegate` instance method of the class
%% behind `Pid`, but only when that class's backing native module is `Module`.
%% Any reflection failure on a single class degrades to `[]` for that class — the
%% Callers view must still list the other classes' delegates — mirroring
%% `class_row/2`'s per-class isolation.
-spec delegate_rows_for_class(pid(), atom()) -> [beamtalk_xref:native_caller_row()].
delegate_rows_for_class(Pid, Module) ->
    try
        ModName = beamtalk_runtime_api:module_name(Pid),
        case meta_backing_module(native_meta_of(ModName)) of
            Module ->
                ClassName = beamtalk_runtime_api:class_name(Pid),
                Exports = safe_module_exports(ModName),
                [delegate_row(ClassName, Sel) || Sel <- self_delegate_selectors(Exports)];
            _ ->
                []
        end
    catch
        _:_ -> []
    end.

-spec safe_module_exports(atom()) -> [{atom(), arity()}].
safe_module_exports(ModName) ->
    try
        ModName:module_info(exports)
    catch
        _:_ -> []
    end.

%% The `self delegate` instance selectors a facade exposes, recovered from its
%% `dispatch_<selector>` exports (the compiler's own `is_self_delegate` decision,
%% native_facade.rs). The dispatch name embeds the selector verbatim — keyword
%% colon included (`dispatch_readLine`, `'dispatch_writeLine:'`) — so stripping the
%% `dispatch_` prefix recovers it exactly. `usort` collapses any duplicate export
%% pair; a stripped name that is not already an interned atom (never true for a
%% real, registered method) is skipped rather than growing the atom table.
-spec self_delegate_selectors([{atom(), arity()}]) -> [atom()].
self_delegate_selectors(Exports) ->
    lists:usort(lists:filtermap(fun dispatch_export_selector/1, Exports)).

-spec dispatch_export_selector({atom(), arity()}) -> {true, atom()} | false.
dispatch_export_selector({Name, _Arity}) ->
    case atom_to_list(Name) of
        "dispatch_" ++ Rest when Rest =/= [] ->
            try
                {true, list_to_existing_atom(Rest)}
            catch
                error:badarg -> false
            end;
        _ ->
            false
    end.

%% A single delegate caller row: the delegating class + selector, anchored at the
%% method's own header line from the xref index. A `self delegate` method is always
%% registered, so `method_info/3` resolves its line; the line=1 fallback is purely
%% defensive so the row stays clickable rather than being silently dropped.
-spec delegate_row(atom(), atom()) -> beamtalk_xref:native_caller_row().
delegate_row(ClassName, Selector) ->
    Line =
        case beamtalk_xref:method_info(ClassName, false, Selector) of
            #{line := L} when is_integer(L), L > 0 -> L;
            _ -> 1
        end,
    #{owner => ClassName, class_side => false, method => Selector, line => Line}.

%% Optional `selector` param for `browse-native-source`: absent/empty → no clause
%% selection; otherwise resolved to an existing atom (an unknown selector reads as
%% a sentinel → no matching clause, never an atom-table growth).
-spec optional_selector(map()) -> atom() | undefined.
optional_selector(Params) ->
    case maps:get(<<"selector">>, Params, undefined) of
        Sel when is_binary(Sel), byte_size(Sel) > 0 ->
            try
                binary_to_existing_atom(Sel, utf8)
            catch
                error:badarg -> '__browse_unknown_selector__'
            end;
        _ ->
            undefined
    end.

%%% ====================================================================
%%% Divergence — origin / disk_differs
%%% ====================================================================

%% origin :: both | static | runtime (ADR 0096). A class/row with a backing disk
%% source that is loaded in the image is `both`; a file-less / ClassBuilder class
%% (no disk source) is `runtime`. The `static` case (on disk, not loaded) is the
%% cold-mode answer and is produced by the static (Rust) layer, not here — a
%% class enumerated by `all_classes/0` is loaded by definition.
-spec origin_of(binary() | null) -> binary().
origin_of(null) -> <<"runtime">>;
origin_of(SourceFile) when is_binary(SourceFile) -> <<"both">>.

%% Class source origin: the *classification* of where a class comes from —
%% `project`, `dependency`, or `stdlib`. Used for IDE badges (BT-2552). This is
%% orthogonal to the *package name* (see `package_of/2`, BT-2643): the package is
%% carried in a separate `package` row field, never packed into this value.
-spec source_origin_of(atom(), binary() | null) -> binary().
source_origin_of(ModName, SourceFile) when is_atom(ModName) ->
    %% Stdlib always wins first (BT-2552).
    case beamtalk_class_registry:is_stdlib_module(ModName) of
        true ->
            <<"stdlib">>;
        false ->
            classify_origin(package_of_module(ModName), project_package_name(), SourceFile)
    end.

%% Classification rule (BT-2640): the module's package segment is the *primary*
%% signal, not the filesystem path. `bt@{pkg}@{class}` encodes the owning package;
%% comparing it to the project's own package name correctly labels dependency
%% classes whose sources happen to resolve under the project tree, and avoids
%% dumping everything into `project` when `project_path` metadata is missing.
%%
%%   - package present, ≠ project package         -> dependency
%%   - package present, == project package        -> project
%%   - package present but project package unknown -> path fallback (then project)
%%   - no package segment                          -> path fallback (null -> project)
-spec classify_origin(binary() | nil, binary() | null, binary() | null) -> binary().
classify_origin(Pkg, ProjectPkg, _SourceFile) when
    is_binary(Pkg), is_binary(ProjectPkg)
->
    case Pkg =:= ProjectPkg of
        true -> <<"project">>;
        false -> <<"dependency">>
    end;
classify_origin(Pkg, _ProjectPkg, SourceFile) when is_binary(Pkg) ->
    %% Package known but project package unresolvable: defer to the path check.
    path_origin(SourceFile);
classify_origin(nil, _ProjectPkg, SourceFile) ->
    %% No package segment in the module atom: fall back to the path-prefix check.
    path_origin(SourceFile).

%% Secondary, path-prefix fallback used only when the package can't decide.
-spec path_origin(binary() | null) -> binary().
path_origin(null) ->
    <<"project">>;
path_origin(SourceFile) when is_binary(SourceFile) ->
    case classify_source_origin(SourceFile) of
        project -> <<"project">>;
        dependency -> <<"dependency">>
    end.

%% Package name a class lives in, for ALL origins (BT-2643): `stdlib` for stdlib
%% classes, the dependency package for dependencies, and the project's own
%% package name for project classes. Orthogonal to `source_origin_of/2`'s
%% classification. The package comes from the module atom (`bt@{pkg}@{class}`)
%% when encoded there; for project classes whose module carries no package
%% segment, fall back to the workspace's configured package name. `null` when no
%% package can be determined (e.g. unknown dependency, no workspace metadata).
-spec package_of(atom(), binary()) -> binary() | null.
package_of(ModName, <<"stdlib">>) when is_atom(ModName) ->
    <<"stdlib">>;
package_of(ModName, <<"dependency">>) when is_atom(ModName) ->
    case package_of_module(ModName) of
        nil -> null;
        Pkg -> Pkg
    end;
package_of(ModName, <<"project">>) when is_atom(ModName) ->
    case package_of_module(ModName) of
        nil -> project_package_name();
        Pkg -> Pkg
    end.

%% The workspace's configured package name (project's own package), or `null`
%% when no workspace metadata is available (startup, no project).
-spec project_package_name() -> binary() | null.
project_package_name() ->
    case beamtalk_workspace_meta:get_package_name() of
        Name when is_binary(Name) -> Name;
        _ -> null
    end.

%% Extract the package name from a module atom (bt@{pkg}@{class} → {pkg}).
-spec package_of_module(atom()) -> binary() | nil.
package_of_module(ModName) when is_atom(ModName) ->
    ModStr = atom_to_list(ModName),
    case string:split(ModStr, "@", all) of
        ["bt", Pkg, _Class | _Rest] when Pkg =/= [] ->
            list_to_binary(Pkg);
        _ ->
            nil
    end.

%% Secondary fallback (BT-2640): determine if a source file belongs to the
%% project or a dependency by filesystem path. Used only by `classify_origin/3`
%% when the module's package segment can't decide. Falls back to `project` when
%% metadata is unavailable (startup, no workspace) — a wrong "project" badge is
%% less confusing than a wrong "dependency" badge.
-spec classify_source_origin(binary()) -> project | dependency.
classify_source_origin(SourceFile) ->
    SourceStr = binary_to_list(SourceFile),
    AbsPath = filename:absname(SourceStr),
    case beamtalk_workspace_meta:get_metadata() of
        {ok, #{project_path := ProjectPath}} when is_binary(ProjectPath) ->
            ProjectRoot = filename:absname(binary_to_list(ProjectPath)),
            RootParts = filename:split(ProjectRoot),
            PathParts = filename:split(AbsPath),
            case lists:prefix(RootParts, PathParts) of
                true -> project;
                false -> dependency
            end;
        _ ->
            project
    end.

%% Per-selector origin from xref provenance: a runtime-installed selector
%% (put_method, class_builder) is runtime-only; a class_body / extension selector
%% in a file-backed class is `both`.
-spec origin_for_provenance(beamtalk_xref:provenance(), binary() | null) -> binary().
origin_for_provenance(put_method, _SourceFile) -> <<"runtime">>;
origin_for_provenance(class_builder, _SourceFile) -> <<"runtime">>;
origin_for_provenance(_Provenance, null) -> <<"runtime">>;
origin_for_provenance(_Provenance, _SourceFile) -> <<"both">>.

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
%% BT-2567: the comparison source is a *live re-read* of the on-disk class
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

%% The class' current on-disk source for the divergence diff (BT-2567). Prefer a
%% live re-read of the recorded source file so the comparison reflects the file
%% as it is *now*, not as it was at load time. Falls back to the in-memory
%% load-time snapshot when the class has no source file on record or the file
%% cannot be read (deleted/moved/permissions) — degrading to the pre-BT-2567
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

%% BT-2648: optional `module` param for `browse-native-source` — a standalone
%% native module key (no backing class). `none` when absent (caller falls back to
%% the `class` form); an error when present but not a loaded module. Resolved to
%% an existing atom only (untrusted client input must not grow the atom table).
-spec validate_module(map()) -> {ok, atom()} | none | {error, binary()}.
validate_module(Params) ->
    case maps:get(<<"module">>, Params, undefined) of
        Mod when is_binary(Mod), byte_size(Mod) > 0 ->
            resolve_module(Mod);
        _ ->
            none
    end.

%% Resolve a module-name binary to an atom only if it names a loaded module. An
%% unknown module is a structured not-found error, never an atom-table growth.
-spec resolve_module(binary()) -> {ok, atom()} | {error, binary()}.
resolve_module(Mod) ->
    try
        Module = binary_to_existing_atom(Mod, utf8),
        case code:is_loaded(Module) of
            {file, _} -> {ok, Module};
            false -> {error, module_not_found_message(Mod)}
        end
    catch
        error:badarg -> {error, module_not_found_message(Mod)}
    end.

-spec module_not_found_message(binary()) -> binary().
module_not_found_message(Mod) ->
    iolist_to_binary([<<"module `">>, Mod, <<"` not found">>]).

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

-spec mod_name_for_class(atom()) -> atom().
mod_name_for_class(ClassName) ->
    case beamtalk_runtime_api:whereis_class(ClassName) of
        undefined -> undefined;
        Pid -> beamtalk_runtime_api:module_name(Pid)
    end.

%% The module whose package + on-disk source determine a class's origin badges
%% (category, origin, source_origin). For an ordinary class this is its own
%% dispatch module. A protocol class object (ADR 0068) is dispatched by the shared
%% `beamtalk_protocol_object` module, which carries no package and no
%% `beamtalk_source` — so origin would wrongly read "runtime"/"project" for every
%% protocol, stdlib ones included. BT-2615: resolve the protocol's *defining*
%% module from the registry (recorded at registration via the codegen `module`
%% key) so a stdlib protocol like `Printable` (`bt@stdlib@printable`) is badged
%% stdlib and a project protocol is badged project. Falls back to the dispatch
%% module for non-protocols and for protocols compiled before the `module` key
%% existed.
-spec origin_module(atom(), atom()) -> atom().
origin_module(ClassName, ModName) ->
    case beamtalk_runtime_api:protocol_info(ClassName) of
        #{module := M} when is_atom(M) -> M;
        _ -> ModName
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

%% Decide a selector's browser protocol bucket (ADR 0096 §"Protocol of a
%% selector": *the xref row's provenance plus the method's declared protocol
%% category*). The decision is layered, most-authoritative source first — a
%% declared category is a fact, a provenance is a fact, a name match is a guess:
%%
%%   1. **Pragma** — the method's *declared* protocol category. This is the
%%      ground truth a class author assigns (the spike's hand-written categories:
%%      "operations", "accessing", "instance creation", …). Beamtalk method
%%      metadata does not yet carry a category field — `beamtalk_xref:method_info/3`
%%      returns only line / source_status / provenance (no category) — so
%%      `declared_protocol/1` reads the (currently absent) `category` key off the
%%      xref `method_info` map: it is the single wiring point for when xref adds
%%      one (a `<category: ...>`-style pragma or an xref `category` field). Until
%%      then the key is absent, the lookup is `undefined`, and the decision falls
%%      through to the source-fact / name tiers.
%%   2. **xref-extension source** — `provenance = extension` (ADR 0066 package
%%      extension) is a *source* fact: the selector was added to this class from
%%      another package, so it belongs in the "extensions" bucket regardless of
%%      its name. This outranks the name heuristic (a fact beats a guess).
%%   3. **Heuristic** — a Pharo-convention bucket derived from the *selector name*
%%      (accessors → "accessing", `is`/`has` tests → "testing", `print*` →
%%      "printing", …). This is what gives ordinary `class_body` methods a
%%      meaningful protocol instead of collapsing every non-extension selector
%%      into one bucket. Selectors that match no convention fall into Pharo's
%%      "as yet unclassified".
-spec protocol_for_selector(
    atom(),
    beamtalk_xref:method_info() | undefined,
    beamtalk_xref:provenance(),
    beamtalk_xref:source_status(),
    boolean()
) -> binary().
protocol_for_selector(Selector, Info, Provenance, SourceStatus, ClassSide) ->
    case declared_protocol(Info) of
        Category when is_binary(Category) ->
            %% Tier 1 — declared category (author ground truth).
            Category;
        undefined ->
            protocol_from_source(Selector, Provenance, SourceStatus, ClassSide)
    end.

%% Tier 1 hook: the method's declared protocol category, or `undefined` when none
%% is declared. Beamtalk's `beamtalk_xref:method_info/3` does not carry a category
%% field today (see `protocol_for_selector/5`), so the `category` key is absent
%% and this returns `undefined`. It is the one place to wire a real category once
%% xref (or a method pragma) surfaces one — reading it here keeps the decision
%% precedence (declared > provenance > name) in a single function with no other
%% call-site change. ADR 0096 §"Protocol of a selector".
-spec declared_protocol(beamtalk_xref:method_info() | undefined) -> binary() | undefined.
declared_protocol(Info) when is_map(Info) ->
    case maps:get(category, Info, undefined) of
        Cat when is_binary(Cat), byte_size(Cat) > 0 -> Cat;
        _ -> undefined
    end;
declared_protocol(_Info) ->
    undefined.

%% Tiers 2–3: no declared category, so decide from the source facts we do have.
%% A compiler-generated accessor (source_status = synthetic) is "accessing" by
%% construction. An extension-provenance selector is "extensions" (source fact).
%% Otherwise fall to the selector-name heuristic.
-spec protocol_from_source(
    atom(), beamtalk_xref:provenance(), beamtalk_xref:source_status(), boolean()
) -> binary().
protocol_from_source(_Selector, _Provenance, synthetic, ClassSide) ->
    %% Synthetic methods are compiler-generated. Two kinds:
    %%   * BT-2614: the actor class-side constructors `new`/`new:`/`spawn`/`spawn:`
    %%     are "instance creation" — the canonical Pharo protocol for them.
    %%   * value-type field accessors (ADR 0087) are "accessing" by construction.
    %% Classify by `class_side`, not the selector name: a class-side synthetic
    %% method is a constructor, an instance-side one is an accessor. This avoids
    %% misclassifying an instance-side synthetic slot that happens to be named
    %% `new`/`spawn` (e.g. `state: new :: Integer = 0`) as "instance creation".
    case ClassSide of
        true -> <<"instance creation">>;
        false -> <<"accessing">>
    end;
protocol_from_source(_Selector, extension, _SourceStatus, _ClassSide) ->
    %% Tier 2 — package-extension provenance (ADR 0066) is a source fact that
    %% outranks the name heuristic.
    <<"extensions">>;
protocol_from_source(Selector, _Provenance, _SourceStatus, _ClassSide) ->
    %% Tier 3 — Pharo-convention name heuristic.
    protocol_from_name(Selector).

%% Pharo-convention protocol buckets derived from the selector name. Mirrors the
%% category vocabulary the spike (`spikes/cockpit-ux-spike/image.js`) and Pharo's
%% default protocols use. A guess, not a fact — only reached when no declared
%% category and no source-fact bucket applies. Unrecognised selectors land in
%% "as yet unclassified" (Pharo's name for the default bucket).
-spec protocol_from_name(atom()) -> binary().
protocol_from_name(Selector) ->
    Name = atom_to_binary(Selector, utf8),
    classify_name(Name).

-spec classify_name(binary()) -> binary().
classify_name(Name) ->
    Keyword = binary:match(Name, <<":">>) =/= nomatch,
    case Name of
        %% Object protocol / printing.
        _ when
            Name =:= <<"printString">>;
            Name =:= <<"displayString">>;
            Name =:= <<"printOn:">>;
            Name =:= <<"displayOn:">>
        ->
            <<"printing">>;
        %% Equality / comparison.
        _ when
            Name =:= <<"=">>;
            Name =:= <<"==">>;
            Name =:= <<"~=">>;
            Name =:= <<"hash">>;
            Name =:= <<"<">>;
            Name =:= <<">">>;
            Name =:= <<"<=">>;
            Name =:= <<">=">>;
            Name =:= <<"<=>">>
        ->
            <<"comparing">>;
        _ ->
            classify_by_prefix(Name, Keyword)
    end.

%% Prefix / shape conventions (checked after the exact-name special cases).
%%
%% Prefix matches respect camelCase *word boundaries* via `has_word_prefix/2`:
%% the prefix must be followed by an uppercase letter (`asString`, `toArray`) or
%% end the selector (`do:`, `add:`). This avoids the classic false positives a
%% naive `binary:match` prefix check produces — `toggle` is not a conversion,
%% `address` is not adding, `island` / `issue` are not tests, `domain` is not
%% enumeration. A guess remains a guess, but a disciplined one.
-spec classify_by_prefix(binary(), boolean()) -> binary().
classify_by_prefix(Name, Keyword) ->
    Prefixes = [
        %% {prefix, protocol} — first boundary-respecting match wins.
        {<<"is">>, <<"testing">>},
        {<<"has">>, <<"testing">>},
        {<<"includes">>, <<"testing">>},
        {<<"respondsTo">>, <<"testing">>},
        {<<"print">>, <<"printing">>},
        {<<"display">>, <<"printing">>},
        {<<"initialize">>, <<"initialization">>},
        {<<"init">>, <<"initialization">>},
        {<<"as">>, <<"converting">>},
        {<<"to">>, <<"converting">>},
        {<<"do">>, <<"enumerating">>},
        {<<"collect">>, <<"enumerating">>},
        {<<"select">>, <<"enumerating">>},
        {<<"reject">>, <<"enumerating">>},
        {<<"detect">>, <<"enumerating">>},
        {<<"inject">>, <<"enumerating">>},
        {<<"add">>, <<"adding">>},
        {<<"remove">>, <<"removing">>}
    ],
    case match_prefix(Name, Prefixes) of
        {ok, Protocol} -> Protocol;
        nomatch -> classify_shape(Name, Keyword)
    end.

%% First prefix in the list that matches `Name` at a camelCase word boundary.
-spec match_prefix(binary(), [{binary(), binary()}]) -> {ok, binary()} | nomatch.
match_prefix(_Name, []) ->
    nomatch;
match_prefix(Name, [{Prefix, Protocol} | Rest]) ->
    case has_word_prefix(Name, Prefix) of
        true -> {ok, Protocol};
        false -> match_prefix(Name, Rest)
    end.

%% True when `Name` starts with `Prefix` AND the prefix is a whole camelCase
%% word: the next character is an uppercase ASCII letter (a new word starts) or a
%% `:` (a keyword selector like `do:`), or the prefix consumes the whole name
%% (`add`, `remove` as bare unary words). A lowercase continuation (`toggle`,
%% `address`) is NOT a boundary and does not match.
-spec has_word_prefix(binary(), binary()) -> boolean().
has_word_prefix(Name, Prefix) ->
    PLen = byte_size(Prefix),
    case Name of
        <<Prefix:PLen/binary>> ->
            true;
        <<Prefix:PLen/binary, Next, _/binary>> ->
            (Next >= $A andalso Next =< $Z) orelse Next =:= $:;
        _ ->
            false
    end.

%% Final shape rule: a keyword selector (has a `:`) that matched no more specific
%% convention reads as an "operations" message — the spike's default protocol for
%% state-changing keyword sends (`setTo:`, `deposit:`). Everything else (a bare
%% unary word, a binary operator) is left "as yet unclassified": guessing
%% getter-vs-operation from a bare name like `increment` / `reset` / `value` is
%% unreliable, so we do not (genuine field accessors are caught earlier via
%% `source_status = synthetic`, which is a fact, not a guess). Pharo's default
%% bucket name is "as yet unclassified".
-spec classify_shape(binary(), boolean()) -> binary().
classify_shape(_Name, true) ->
    <<"operations">>;
classify_shape(_Name, false) ->
    <<"as yet unclassified">>.

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
    {error,
        beamtalk_repl_errors:make(
            argument_error, 'REPL', iolist_to_binary([Op, <<": ">>, Reason])
        )}.

-spec not_found_error(binary(), atom()) -> {error, #beamtalk_error{}}.
not_found_error(Op, ClassName) ->
    arg_error(Op, class_not_found_message(atom_to_binary(ClassName, utf8))).
