%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_xref).
-behaviour(gen_server).

%%% **DDD Context:** Runtime Context

-moduledoc """
Cross-reference index gen_server for SystemNavigation queries (ADR 0087).

Owns four ETS tables that maintain a runtime-resident `selector → sites` /
`class → references` index, populated at class-load time via
`register_class/2`. Reads (`senders_of/1`, `references_to/1`,
`implementors_of/1`, `defined_selectors/2`) hit ETS directly and do not
serialize through the gen_server. Writes serialize through the gen_server
so per-class generation bumps stay atomic.

Phase 1 (BT-2297) provides the skeleton: tables, API contract, supervisor
wiring. Subsequent phases land producers (codegen → `register_class/0`,
lifecycle hooks) and read-path migration in `SystemNavigation`. The
multi-generation reload sweep lands in Phase 4 (BT-2300).

Storage layout (all `protected, named_table, {read_concurrency, true}`):
- `beamtalk_xref_methods` (bag): per-method definitions
- `beamtalk_xref_senders` (bag): selector → call sites
- `beamtalk_xref_references` (bag): class → reference sites
- `xref_class_gen` (set): class → current generation

See also: docs/ADR/0087-maintained-xref-index-for-system-navigation.md
""".

-include_lib("kernel/include/logger.hrl").

%% API — write path
-export([
    start_link/0,
    register_class/2,
    purge_class/1,
    put_method/4
]).

%% API — read path
-export([
    senders_of/1,
    references_to/1,
    implementors_of/1,
    defined_selectors/2,
    method_info/3
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Table names
-define(METHODS_TABLE, beamtalk_xref_methods).
-define(SENDERS_TABLE, beamtalk_xref_senders).
-define(REFERENCES_TABLE, beamtalk_xref_references).
-define(CLASS_GEN_TABLE, xref_class_gen).

%%====================================================================
%% Types
%%====================================================================

-type class_name() :: atom().
-type selector() :: atom().
-type class_side() :: boolean().
-type gen() :: pos_integer().
-type source_status() :: indexed | unindexed_runtime_fun | synthetic.
-type provenance() :: class_body | extension | class_builder | put_method.
-type recv_kind() :: self_recv | super_recv | erlang_ffi | other.

-type send_entry() :: #{
    selector := selector(),
    line := pos_integer(),
    recv_kind => recv_kind()
}.

-type reference_entry() :: #{
    class := class_name(),
    line := pos_integer()
}.

-type method_xref_entry() :: #{
    class_side := class_side(),
    selector := selector(),
    line := pos_integer(),
    sends => [send_entry()],
    references => [reference_entry()],
    source_status => source_status(),
    provenance => provenance(),
    synthetic_origin => pos_integer() | undefined
}.

-type method_info() :: #{
    owner := class_name(),
    line := pos_integer(),
    source_status := source_status(),
    provenance := provenance(),
    gen := gen()
}.

-type site() :: #{
    owner := class_name(),
    class_side := class_side(),
    method := selector(),
    line := pos_integer(),
    recv_kind => recv_kind(),
    gen := gen()
}.

-export_type([
    method_xref_entry/0,
    method_info/0,
    site/0,
    source_status/0,
    provenance/0,
    recv_kind/0
]).

-record(state, {}).

%%====================================================================
%% API — write path
%%====================================================================

-doc "Start the xref gen_server.".
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-doc """
Register all per-method xref rows for a class. Synchronous.

Bumps the class's generation counter, inserts new rows under the new
generation, then atomically publishes the new generation in
`xref_class_gen`. Readers observing during the call see either the
pre- or post-write state (the gen-bump is the atomic publish).

Phase 1 (BT-2297): the multi-gen reload sweep (old-gen `select_delete`)
is out of scope and lands in Phase 4 / BT-2300. For now `purge_class/1`
is the primary way to drop old rows before re-registering.
""".
-spec register_class(class_name(), [method_xref_entry()]) -> ok.
register_class(Class, MethodXref) when is_atom(Class), is_list(MethodXref) ->
    gen_server:call(?MODULE, {register_class, Class, MethodXref}).

-doc """
Remove all rows belonging to a class from every xref table.
Synchronous. Idempotent — purging an unknown class is a no-op.
""".
-spec purge_class(class_name()) -> ok.
purge_class(Class) when is_atom(Class) ->
    gen_server:call(?MODULE, {purge_class, Class}).

-doc """
Replace the xref rows for a single method without touching siblings.

Used for method-level edits (`put_method/4` source patches, ADR 0082;
extension registers, ADR 0066) where only one method's data changes.
Synchronous. Bumps the owning class's generation so the new rows are
visible atomically.
""".
-spec put_method(class_name(), class_side(), selector(), method_xref_entry()) -> ok.
put_method(Class, ClassSide, Selector, MethodXref) when
    is_atom(Class), is_boolean(ClassSide), is_atom(Selector), is_map(MethodXref)
->
    gen_server:call(?MODULE, {put_method, Class, ClassSide, Selector, MethodXref}).

%%====================================================================
%% API — read path
%%====================================================================

-doc """
Return all sites that send `Selector`. Direct ETS lookup — does not
go through the gen_server.

Phase 1 (BT-2297): no miss-policy fallback. Callers should treat an
empty result as "no known senders"; the source-scan fallback for
unloaded / unindexed classes lands with Phase 3 (BT-2299).
""".
-spec senders_of(selector()) -> [site()].
senders_of(Selector) when is_atom(Selector) ->
    case ets:whereis(?SENDERS_TABLE) of
        undefined -> [];
        _ -> [Site || {_Sel, Site} <- ets:lookup(?SENDERS_TABLE, Selector)]
    end.

-doc """
Return all sites that reference `Class` (type annotations, class
literals, etc.). Direct ETS lookup.
""".
-spec references_to(class_name()) -> [site()].
references_to(Class) when is_atom(Class) ->
    case ets:whereis(?REFERENCES_TABLE) of
        undefined -> [];
        _ -> [Site || {_Cls, Site} <- ets:lookup(?REFERENCES_TABLE, Class)]
    end.

-doc """
Return all `{Class, ClassSide}` pairs that implement `Selector`.
Direct ETS lookup via `match_object` on the methods bag.
""".
-spec implementors_of(selector()) -> [{class_name(), class_side()}].
implementors_of(Selector) when is_atom(Selector) ->
    case ets:whereis(?METHODS_TABLE) of
        undefined ->
            [];
        _ ->
            %% Match keys of shape {Class, ClassSide, Selector} where Selector matches.
            %% Use ets:match/2 with key wildcard on Class and ClassSide.
            Matches = ets:match(?METHODS_TABLE, {{'$1', '$2', Selector}, '_'}),
            lists:usort([{Cls, CS} || [Cls, CS] <- Matches])
    end.

-doc """
Return the selectors defined on `Class` for the given side
(`ClassSide = true` for class-side methods, `false` for instance-side).
""".
-spec defined_selectors(class_name(), class_side()) -> [selector()].
defined_selectors(Class, ClassSide) when is_atom(Class), is_boolean(ClassSide) ->
    case ets:whereis(?METHODS_TABLE) of
        undefined ->
            [];
        _ ->
            Matches = ets:match(?METHODS_TABLE, {{Class, ClassSide, '$1'}, '_'}),
            lists:usort([Sel || [Sel] <- Matches])
    end.

-doc """
Return the `method_info()` record for a method, or `undefined` if the
class+selector+side triple is not registered.

Used by the LSP `nav-query` op (BT-2239) to surface the method-header line
number to `textDocument/implementation` consumers (BT-2241). Direct ETS
lookup — does not go through the gen_server.
""".
-spec method_info(class_name(), class_side(), selector()) -> method_info() | undefined.
method_info(Class, ClassSide, Selector) when
    is_atom(Class), is_boolean(ClassSide), is_atom(Selector)
->
    case ets:whereis(?METHODS_TABLE) of
        undefined ->
            undefined;
        _ ->
            case ets:lookup(?METHODS_TABLE, {Class, ClassSide, Selector}) of
                [{_Key, Info} | _] -> Info;
                [] -> undefined
            end
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    logger:set_process_metadata(#{domain => [beamtalk, runtime]}),

    %% Create the four ETS tables, owned by this process. Heir is undefined
    %% per the acceptance criteria — tables die with the process, supervisor
    %% restart triggers re-population from the next round of register_class/0
    %% calls (or from miss-policy fallback once Phase 3 lands).
    BagOpts = [bag, protected, named_table, {read_concurrency, true}],
    SetOpts = [set, protected, named_table, {read_concurrency, true}],
    ?METHODS_TABLE = ets:new(?METHODS_TABLE, BagOpts),
    ?SENDERS_TABLE = ets:new(?SENDERS_TABLE, BagOpts),
    ?REFERENCES_TABLE = ets:new(?REFERENCES_TABLE, BagOpts),
    ?CLASS_GEN_TABLE = ets:new(?CLASS_GEN_TABLE, SetOpts),

    ?LOG_INFO("xref started", #{
        tables => [?METHODS_TABLE, ?SENDERS_TABLE, ?REFERENCES_TABLE, ?CLASS_GEN_TABLE]
    }),
    {ok, #state{}}.

handle_call({register_class, Class, MethodXref}, _From, State) ->
    Gen = bump_gen(Class),
    insert_method_xref_rows(Class, MethodXref, Gen),
    {reply, ok, State};
handle_call({purge_class, Class}, _From, State) ->
    do_purge_class(Class),
    {reply, ok, State};
handle_call({put_method, Class, ClassSide, Selector, MethodXref}, _From, State) ->
    Gen = bump_gen(Class),
    delete_method_rows(Class, ClassSide, Selector),
    %% Force the entry's class_side/selector to match the addressed slot —
    %% protects against accidental drift between the call args and the map.
    Normalised = MethodXref#{class_side => ClassSide, selector => Selector},
    insert_method_xref_rows(Class, [Normalised], Gen),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% ETS tables are owned by this process; they die automatically. The
    %% supervisor restart brings up fresh empty tables — Phase 4's
    %% miss-policy fallback / re-register hook will repopulate from the
    %% class registry as classes are reloaded.
    ok.

code_change(OldVsn, State, Extra) ->
    beamtalk_hot_reload:code_change(OldVsn, State, Extra).

%%====================================================================
%% Internal: generation management
%%====================================================================

-doc """
Increment the per-class generation counter and return the new generation.
On first registration the counter starts at 1.

The increment + new-gen-row insert is the atomic publish step described
in ADR 0087 §Atomicity. Phase 1 only writes the new-gen rows; the
async sweep of old-gen rows lands in Phase 4 (BT-2300).
""".
-spec bump_gen(class_name()) -> gen().
bump_gen(Class) ->
    NewGen =
        case ets:lookup(?CLASS_GEN_TABLE, Class) of
            [] -> 1;
            [{_, Current}] -> Current + 1
        end,
    true = ets:insert(?CLASS_GEN_TABLE, {Class, NewGen}),
    NewGen.

%%====================================================================
%% Internal: write helpers
%%====================================================================

-doc """
Insert all rows for a class's method_xref list under the given generation.
Each entry contributes one row to `methods`, plus one row per send to
`senders`, plus one row per reference to `references`.
""".
-spec insert_method_xref_rows(class_name(), [method_xref_entry()], gen()) -> ok.
insert_method_xref_rows(Class, MethodXref, Gen) ->
    lists:foreach(
        fun(Entry) -> insert_one_method(Class, Entry, Gen) end,
        MethodXref
    ),
    ok.

-spec insert_one_method(class_name(), method_xref_entry(), gen()) -> ok.
insert_one_method(Class, Entry, Gen) ->
    ClassSide = maps:get(class_side, Entry, false),
    Selector = maps:get(selector, Entry),
    Line = maps:get(line, Entry),
    SourceStatus = maps:get(source_status, Entry, indexed),
    Provenance = maps:get(provenance, Entry, class_body),

    MethodInfo = #{
        owner => Class,
        line => Line,
        source_status => SourceStatus,
        provenance => Provenance,
        gen => Gen
    },
    true = ets:insert(?METHODS_TABLE, {{Class, ClassSide, Selector}, MethodInfo}),

    Sends = maps:get(sends, Entry, []),
    lists:foreach(
        fun(Send) ->
            SendSelector = maps:get(selector, Send),
            Site = #{
                owner => Class,
                class_side => ClassSide,
                method => Selector,
                line => maps:get(line, Send, Line),
                recv_kind => maps:get(recv_kind, Send, other),
                gen => Gen
            },
            true = ets:insert(?SENDERS_TABLE, {SendSelector, Site})
        end,
        Sends
    ),

    References = maps:get(references, Entry, []),
    lists:foreach(
        fun(Ref) ->
            RefClass = maps:get(class, Ref),
            Site = #{
                owner => Class,
                class_side => ClassSide,
                method => Selector,
                line => maps:get(line, Ref, Line),
                gen => Gen
            },
            true = ets:insert(?REFERENCES_TABLE, {RefClass, Site})
        end,
        References
    ),
    ok.

-doc """
Delete all rows belonging to a class from every table, including the
class's generation counter. Idempotent.

`senders` and `references` are keyed by selector / referenced class, so
the class-owner predicate lives inside the value map. ETS match_spec
maps support partial-key pattern matching (`#{owner => '$1'}`), so we
use `select_delete` with a guard on the owner field rather than the
non-existent literal-map `match_delete` form.
""".
-spec do_purge_class(class_name()) -> ok.
do_purge_class(Class) ->
    %% Methods are keyed by {Class, ClassSide, Selector} — direct key match.
    true = ets:match_delete(?METHODS_TABLE, {{Class, '_', '_'}, '_'}),
    %% Senders / references — match on the value map's owner field.
    _ = ets:select_delete(
        ?SENDERS_TABLE,
        [{{'_', #{owner => '$1'}}, [{'=:=', '$1', {const, Class}}], [true]}]
    ),
    _ = ets:select_delete(
        ?REFERENCES_TABLE,
        [{{'_', #{owner => '$1'}}, [{'=:=', '$1', {const, Class}}], [true]}]
    ),
    true = ets:delete(?CLASS_GEN_TABLE, Class),
    ok.

-doc """
Delete rows for one specific method (Class, ClassSide, Selector).
Senders / references entries for the method are scrubbed by matching
the value map's owner + class_side + method fields via `select_delete`.
""".
-spec delete_method_rows(class_name(), class_side(), selector()) -> ok.
delete_method_rows(Class, ClassSide, Selector) ->
    true = ets:match_delete(?METHODS_TABLE, {{Class, ClassSide, Selector}, '_'}),
    MatchSpec = [
        {
            {'_', #{owner => '$1', class_side => '$2', method => '$3'}},
            [
                {'=:=', '$1', {const, Class}},
                {'=:=', '$2', {const, ClassSide}},
                {'=:=', '$3', {const, Selector}}
            ],
            [true]
        }
    ],
    _ = ets:select_delete(?SENDERS_TABLE, MatchSpec),
    _ = ets:select_delete(?REFERENCES_TABLE, MatchSpec),
    ok.
