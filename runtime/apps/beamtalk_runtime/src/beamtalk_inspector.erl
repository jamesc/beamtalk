%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime Context

-module(beamtalk_inspector).

-moduledoc """
Runtime shim backing the `Inspector` / `InspectorField` stdlib classes (ADR 0095).

An `Inspector` is an immutable cursor over one object. This module mints the
cursor handle, classifies the subject `kind`, derives its drillable
`InspectorField` records, drills (`at:`), and re-captures snapshots (`refresh`).
It is the runtime half of the design; the navigation API and rendering live in
`stdlib/src/Inspector.bt`.

## The cursor handle

`on/1` mints a tagged map:

```erlang
#{
  '$beamtalk_class' => 'Inspector',
  kind     => value | actor | collection | foreign,
  pid      => pid() | nil,       %% the live actor/foreign pid, for refresh (nil for values)
  subject  => term(),            %% the value itself, or the captured snapshot (actors)
  page     => non_neg_integer(), %% 0-based window index (collections only; 0 elsewhere)
  parent   => inspector() | nil, %% the cursor drilled from (nil at root)
  path     => [term()]           %% breadcrumb of drilled names from root
}
```

The handle is immutable: `at:`, `page:`, and `refresh` return *new* handles.

## Kind & state capture (ADR 0095 §3–§4, §6)

- `#value` — a tagged `Value` map. Fields are its user `field:` slots in ADR-0094
  sort order, read purely (no process contact).
- `#actor` — a live Beamtalk actor pid. State is captured **at `on:`** via the
  shared timeout-guarded `sys:get_state`
  (`beamtalk_process_navigation:guarded_state/1`); the frozen snapshot is the
  cursor's `subject`, reused for all drilling and reset only by `refresh`. A
  busy/dead/non-`sys` actor degrades to a single
  `InspectorField name: #status value: #unavailable` — never a crash.
- `#collection` — a `List`/`Array`/`Set`/`Dictionary`/`Bag`. `fields` returns a
  bounded **window** (default page size 50, ADR 0095 §6); `sizeOf` is the cheap
  full count (not a walk); `page/2` returns a cursor on the next window; `at:`
  indexes directly. Dictionary/Bag associations render with `kind => association`.
- `#foreign` — a non-Beamtalk OTP process (`Pid`). `fields` are best-effort
  `process_info` data plus a guarded `sys:get_state` when the process is
  `sys`-compliant, tagged `kind => processInfo`; never raises.

Value `evaluate:` compiles and evaluates the expression with `self` bound to the
value (delegated to the workspace eval pipeline via `erlang:apply`, no
compile-time dependency); actor evaluate-in-context is a deferred follow-up
(ADR 0095 §7) and returns `actor_eval_unsupported`.
""".

%% Selector→function mapping (beamtalk_erlang_proxy): a single-keyword selector
%% strips its trailing colon (`on:` → `on`, `fieldsOf:` → `fieldsOf`); a
%% multi-keyword selector uses its *first* keyword as the function name with all
%% arguments positional (`inspector:at:` → `inspector/2`, `printString:depth:` →
%% `printString/2`).
-export([
    on/1,
    on/2,
    subjectOf/1,
    kindOf/1,
    fieldsOf/1,
    sizeOf/1,
    inspector/2,
    page/2,
    evaluate/2,
    parentOf/1,
    pathOf/1,
    refresh/1,
    printString/2,
    asDictionaries/1,
    asDictionary/1
]).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% The `Inspector` cursor handle. The `'$beamtalk_class'` tag lets the spec
%% reader map FFI returns of this shape to the Beamtalk `Inspector` type.
-type inspector() :: #{
    '$beamtalk_class' := 'Inspector',
    kind := value | actor | collection | foreign,
    pid := pid() | nil,
    available := boolean(),
    subject := term(),
    page := non_neg_integer(),
    parent := inspector() | nil,
    path := [term()]
}.

%% Default collection window size (ADR 0095 §6 — matches ADR 0094's width cap).
-define(PAGE_SIZE, 50).

%% An `InspectorField` Value record. The `'$beamtalk_class'` tag maps it to the
%% Beamtalk `InspectorField` type for `fieldsOf/1`'s `List(InspectorField)`.
-type field_map() :: #{
    '$beamtalk_class' := 'InspectorField',
    name := term(),
    label := binary(),
    value := term(),
    kind := atom(),
    drillable := boolean()
}.

%%====================================================================
%% Construction
%%====================================================================

-doc """
Mint an `Inspector` cursor on `Subject` (`Inspector on:` / `anObject inspect`).

Classifies the subject kind and, for an actor, captures its timeout-guarded
state snapshot. The minted cursor is a root (`parent => nil`, `path => []`).
""".
-spec on(term()) -> inspector().
on(Subject) ->
    root_cursor(Subject, []).

-doc """
Mint an `Inspector` cursor on `Subject`, seeding an actor snapshot from a state
map the caller *already holds* (`Inspector on:` dispatched from inside the
actor).

This is the self-inspection-safe entry point used by `beamtalk_object_ops`'s
`inspect` dispatch: when `anActor inspect` runs, it executes **inside the
actor's own `handle_call`**, so a `sys:get_state(self())` would deadlock and
time out (the process cannot service the system message while busy in the call).
`KnownState` is the live state map the dispatch already has in hand, so we seed
the `#actor` cursor directly — no self-`sys:get_state`.

It only short-circuits when `Subject` resolves to a **live Beamtalk actor pid**
*and* `KnownState` is a non-empty tagged map (a real actor-instance state). Every
other shape — a value, a collection, a class object (`KnownState` is `#{}`), or a
*different* process — falls back to `on/1`, whose `sys:get_state` is safe because
it targets another process, not the caller.
""".
-spec on(term(), term()) -> inspector().
on(Subject, KnownState) when is_map(KnownState), map_size(KnownState) > 0 ->
    case actor_pid(Subject) of
        {ok, Pid} when Pid =:= self() ->
            %% Self-inspection only: seed from the state the caller (the actor's
            %% own handle_call) already holds, since `sys:get_state(self())`
            %% would deadlock. A *different* pid must NOT be seeded with this
            %% caller's state — it falls back to `on/1`'s guarded snapshot.
            case beamtalk_actor:is_beamtalk_actor(Pid) of
                true -> actor_cursor_from_state(Pid, KnownState, nil, []);
                false -> on(Subject)
            end;
        _ ->
            on(Subject)
    end;
on(Subject, _KnownState) ->
    on(Subject).

%% Mint an `#actor` cursor seeded from a state map the caller already holds (no
%% `sys:get_state` — used for actor self-inspection, which runs inside the
%% actor's process where a self-call would deadlock). Mirrors `actor_cursor/3`
%% but with `available => true` and the given snapshot. `refresh` later re-issues
%% the guarded `sys:get_state` (safe, since refresh is made by an external holder
%% of the cursor, not from inside the actor's call).
-spec actor_cursor_from_state(pid(), map(), inspector() | nil, [term()]) -> inspector().
actor_cursor_from_state(Pid, State, Parent, Path) ->
    #{
        '$beamtalk_class' => 'Inspector',
        kind => actor,
        pid => Pid,
        available => true,
        subject => State,
        page => 0,
        parent => Parent,
        path => Path
    }.

%% Build a root cursor (no parent) for a subject, classifying kind and capturing
%% actor state. `Path` is the breadcrumb a drilled child inherits.
-spec root_cursor(term(), [term()]) -> inspector().
root_cursor(Subject, Path) ->
    cursor(Subject, nil, Path).

%% Build a cursor with an explicit parent, classifying the subject's kind
%% (ADR 0095 §3). An actor/foreign subject arrives either as a bare pid or — the
%% usual Beamtalk handle — a `#beamtalk_object{}` wrapping the pid (resolving a
%% `{registered, Name}` ref to the live pid); a Beamtalk actor pid is `#actor`, a
%% genuinely foreign OTP pid is `#foreign`. A `List`/`Array`/`Set`/`Dictionary`/
%% `Bag` is `#collection`. Everything else (including a tagged `Value`) is
%% `#value`.
-spec cursor(term(), inspector() | nil, [term()]) -> inspector().
cursor(#beamtalk_object{pid = Pid} = Obj, Parent, Path) when is_pid(Pid) ->
    %% A class object (e.g. `Counter`) is a `#beamtalk_object{}` whose `pid` field
    %% holds its *class registry* gen_server — a live but unrelated pid. Without
    %% this guard it would classify as `#foreign` and surface the registry
    %% process's `process_info`, which is surprising: inspecting a class should
    %% view the class, not its backing process. Treat it as a `#value` cursor over
    %% the class object record (no drillable slots — a `#beamtalk_object{}` is not
    %% a tagged map — a correct, non-leaky `Inspector(Counter class)`). An actor
    %% *instance* handle (not a class object) falls through to `process_cursor`.
    case beamtalk_class_registry:is_class_object(Obj) of
        true -> value_cursor(Obj, Parent, Path);
        false -> process_cursor(Pid, Parent, Path)
    end;
cursor(#beamtalk_object{pid = {registered, Name}}, Parent, Path) ->
    case erlang:whereis(Name) of
        Pid when is_pid(Pid) -> process_cursor(Pid, Parent, Path);
        _ -> value_cursor(unavailable, Parent, Path)
    end;
cursor(Subject, Parent, Path) when is_pid(Subject) ->
    process_cursor(Subject, Parent, Path);
cursor(Subject, Parent, Path) ->
    case is_collection(Subject) of
        true -> collection_cursor(Subject, 0, Parent, Path);
        false -> value_cursor(Subject, Parent, Path)
    end.

%% Classify a live pid: a Beamtalk actor is `#actor` (guarded snapshot), any other
%% *local* OTP process is `#foreign` (best-effort `process_info`, never snapshotted
%% at construction — read lazily by `fieldsOf`). A pid on **another node** cannot be
%% introspected locally — `is_beamtalk_actor/1` (process dictionary),
%% `is_process_alive/1`, and `process_info/2` all raise `badarg` for a remote pid —
%% so it degrades to an unavailable `#foreign` cursor rather than crashing (BT-2508).
-spec process_cursor(pid(), inspector() | nil, [term()]) -> inspector().
process_cursor(Pid, Parent, Path) when node(Pid) =/= node() ->
    remote_cursor(Pid, Parent, Path);
process_cursor(Pid, Parent, Path) ->
    case beamtalk_actor:is_beamtalk_actor(Pid) of
        true -> actor_cursor(Pid, Parent, Path);
        false -> foreign_cursor(Pid, Parent, Path)
    end.

%% Mint an unavailable `#foreign` cursor over a remote-node pid (BT-2508). No local
%% introspection BIF (`process_info`/`is_process_alive`) accepts a remote pid, so
%% `available` is fixed `false` — `fieldsOf` yields the single `#status =>
%% #unavailable` diagnostic and `header_line` shows the pid, never a crash. Mirrors
%% `foreign_cursor/3` but skips the `process_alive/1` probe.
-spec remote_cursor(pid(), inspector() | nil, [term()]) -> inspector().
remote_cursor(Pid, Parent, Path) ->
    #{
        '$beamtalk_class' => 'Inspector',
        kind => foreign,
        pid => Pid,
        available => false,
        subject => Pid,
        page => 0,
        parent => Parent,
        path => Path
    }.

%% Mint an `#actor` cursor over a live pid: capture the guarded snapshot now
%% (ADR 0095 §4). The pid is retained for refresh. `available` records whether
%% the guarded read succeeded — a *distinct* flag, never a sentinel value in
%% `subject`, so an actor whose real state happens to be the atom `unavailable`
%% is not mistaken for a failed snapshot (the spaces never overlap).
-spec actor_cursor(pid(), inspector() | nil, [term()]) -> inspector().
actor_cursor(Pid, Parent, Path) ->
    {Available, Snapshot} =
        case beamtalk_process_navigation:guarded_state(Pid) of
            {ok, State} -> {true, State};
            unavailable -> {false, nil}
        end,
    #{
        '$beamtalk_class' => 'Inspector',
        kind => actor,
        pid => Pid,
        available => Available,
        subject => Snapshot,
        page => 0,
        parent => Parent,
        path => Path
    }.

%% Mint a `#foreign` cursor over a non-Beamtalk OTP pid. No state is captured at
%% construction (unlike `#actor`); `fieldsOf` reads `process_info` and a guarded
%% `sys:get_state` lazily, best-effort. `available` mirrors process liveness so a
%% dead pid degrades to the single `#status => #unavailable` diagnostic, never a
%% crash (ADR 0095 §3–§4).
-spec foreign_cursor(pid(), inspector() | nil, [term()]) -> inspector().
foreign_cursor(Pid, Parent, Path) ->
    #{
        '$beamtalk_class' => 'Inspector',
        kind => foreign,
        pid => Pid,
        available => process_alive(Pid),
        subject => Pid,
        page => 0,
        parent => Parent,
        path => Path
    }.

%% Mint a `#collection` cursor windowed at page `Page` (0-based). The subject is
%% the live collection term (list/tagged map); `fieldsOf` materialises only the
%% current window (ADR 0095 §6).
-spec collection_cursor(term(), non_neg_integer(), inspector() | nil, [term()]) -> inspector().
collection_cursor(Subject, Page, Parent, Path) ->
    #{
        '$beamtalk_class' => 'Inspector',
        kind => collection,
        pid => nil,
        available => true,
        subject => Subject,
        page => Page,
        parent => Parent,
        path => Path
    }.

%% Mint a `#value` cursor: pure structural subject, no process contact (always
%% available).
-spec value_cursor(term(), inspector() | nil, [term()]) -> inspector().
value_cursor(Subject, Parent, Path) ->
    #{
        '$beamtalk_class' => 'Inspector',
        kind => value,
        pid => nil,
        available => true,
        subject => Subject,
        page => 0,
        parent => Parent,
        path => Path
    }.

%% Liveness for a pid. Callers (`foreign_cursor`) only ever pass a `pid()`.
-spec process_alive(pid()) -> boolean().
process_alive(Pid) -> erlang:is_process_alive(Pid).

%%====================================================================
%% Identity accessors
%%====================================================================

-spec subjectOf(inspector()) -> term().
subjectOf(#{subject := Subject}) -> Subject.

-spec kindOf(inspector()) -> atom().
kindOf(#{kind := Kind}) -> Kind.

-spec parentOf(inspector()) -> inspector() | nil.
parentOf(#{parent := Parent}) -> Parent.

-spec pathOf(inspector()) -> [term()].
pathOf(#{path := Path}) -> Path.

%%====================================================================
%% Field derivation (ADR 0095 §3)
%%====================================================================

-doc """
Derive the drillable `InspectorField` records for a cursor.

- `#value` → its user `field:` slots in ADR-0094 sort order.
- `#actor` → its state slots from the frozen snapshot, or the single
  `#status => #unavailable` diagnostic when the snapshot could not be captured.
- `#collection` → the current window of elements/associations (page size 50,
  ADR 0095 §6); the cursor's `page` selects the window.
- `#foreign` → best-effort `process_info` data plus a guarded `sys:get_state`,
  tagged `#processInfo`; the single `#status => #unavailable` diagnostic for a
  dead process. Never raises.
""".
-spec fieldsOf(inspector()) -> [field_map()].
fieldsOf(#{kind := actor, available := false}) ->
    [unavailable_field()];
fieldsOf(#{kind := foreign, available := false}) ->
    [unavailable_field()];
fieldsOf(#{kind := foreign, pid := Pid}) ->
    foreign_fields(Pid);
fieldsOf(#{kind := collection, subject := Subject, page := Page}) ->
    collection_fields(Subject, Page);
fieldsOf(#{subject := Subject}) ->
    case beamtalk_tagged_map:is_tagged(Subject) of
        true -> slot_fields(Subject);
        false -> []
    end.

-doc """
The cheap full element count of a `#collection` cursor (ADR 0095 §6) — the
collection's `size`, not a window walk. `0` for any non-collection cursor (the
field count there is `length(fieldsOf/1)`, already cheap).
""".
-spec sizeOf(inspector()) -> non_neg_integer().
sizeOf(#{kind := collection, subject := Subject}) ->
    collection_size(Subject);
sizeOf(_) ->
    0.

%% Build a sorted list of `InspectorField` records from a tagged map's user
%% slots (ADR-0094 sort order — `user_field_keys/1` is unsorted, so we sort).
-spec slot_fields(map()) -> [field_map()].
slot_fields(State) ->
    Keys = lists:sort(beamtalk_tagged_map:user_field_keys(State)),
    [field_record(K, maps:get(K, State, nil)) || K <- Keys].

%% A `#slot` InspectorField for one field key/value pair.
-spec field_record(term(), term()) -> field_map().
field_record(Key, Value) ->
    inspector_field(Key, label_for(Key), Value, slot, is_drillable(Value)).

%% The diagnostic field for an actor whose state could not be captured
%% (busy/dead/non-`sys`): `InspectorField name: #status value: #unavailable`.
-spec unavailable_field() -> field_map().
unavailable_field() ->
    inspector_field(status, <<"status">>, unavailable, slot, false).

%% Mint an `InspectorField` Value record (tagged map). Field order mirrors the
%% stdlib `field:` declarations: name, label, value, kind, drillable.
-spec inspector_field(term(), binary(), term(), atom(), boolean()) -> field_map().
inspector_field(Name, Label, Value, Kind, Drillable) ->
    #{
        '$beamtalk_class' => 'InspectorField',
        name => Name,
        label => Label,
        value => Value,
        kind => Kind,
        drillable => Drillable
    }.

%% A field is drillable when its value is a tagged Beamtalk object (a Value with
%% slots, an actor/foreign pid, or a collection) — i.e. there is something to
%% drill into. Bare scalars (numbers, atoms/symbols, booleans, nil, strings) are
%% leaves. A non-empty bare list is a `#collection`, so it is drillable; the empty
%% list is `#()` (a leaf with no elements).
-spec is_drillable(term()) -> boolean().
is_drillable([]) ->
    false;
is_drillable(Value) ->
    actor_pid(Value) =/= not_actor orelse
        beamtalk_tagged_map:is_tagged(Value) orelse
        is_collection(Value).

%% Extract the live pid from an actor subject — a bare pid or a
%% `#beamtalk_object{}` handle (resolving a `{registered, Name}` ref). Returns
%% `not_actor` for anything that is not a live actor reference.
-spec actor_pid(term()) -> {ok, pid()} | not_actor.
actor_pid(Pid) when is_pid(Pid) ->
    {ok, Pid};
actor_pid(#beamtalk_object{pid = Pid}) when is_pid(Pid) ->
    {ok, Pid};
actor_pid(#beamtalk_object{pid = {registered, Name}}) ->
    case erlang:whereis(Name) of
        Pid when is_pid(Pid) -> {ok, Pid};
        _ -> not_actor
    end;
actor_pid(_) ->
    not_actor.

%% The display label for a slot key — the field name as a binary.
-spec label_for(term()) -> binary().
label_for(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
label_for(Key) when is_binary(Key) -> Key;
label_for(Key) -> iolist_to_binary(io_lib:format("~p", [Key])).

%%====================================================================
%% Collection field derivation & windowing (ADR 0095 §6)
%%====================================================================

%% True for a `List`/`Array`/`Set`/`Dictionary`/`Bag` subject (the kinds that get
%% the `#collection` cursor). A bare list, a plain (untagged) map, and the
%% collection-tagged maps all qualify; a tagged `Value`/`Actor`/exception map does
%% not.
-spec is_collection(term()) -> boolean().
is_collection([]) ->
    %% The empty list `#()` is a leaf (no elements to navigate), consistent with
    %% `is_drillable([])` — direct `Inspector on: #()` and a drilled `#()` field
    %% agree it is not a collection (BT-2509).
    false;
is_collection(Subject) when is_list(Subject) ->
    %% Only *proper* (nil-terminated) lists are collections. Improper lists
    %% (e.g. `[1|2]`, routine in foreign OTP process state) would crash the
    %% windowing paths — `length/1`, `lists:sublist/3`, `lists:nth/2` — so they
    %% degrade to `#value` instead (ADR 0095 "never raises").
    is_proper_list(Subject);
is_collection(Subject) when is_map(Subject) ->
    lists:member(beamtalk_tagged_map:class_of(Subject, 'Dictionary'), [
        'List', 'Array', 'Set', 'Dictionary', 'Bag'
    ]);
is_collection(_) ->
    false.

%% True only for a proper (nil-terminated) list; `false` for an improper list.
%% Unlike the `is_list/1` guard, this rejects `[H | T]` where `T` is not a list,
%% which would otherwise crash list-length/slice operations.
-spec is_proper_list(term()) -> boolean().
is_proper_list([]) -> true;
is_proper_list([_ | T]) -> is_proper_list(T);
is_proper_list(_) -> false.

%% The cheap full element count of a collection — never a window walk. Lists use
%% `length/1`; Array/Set delegate to their stdlib backers; a Dictionary is its
%% user-key count; a Bag is the sum of its counts.
-spec collection_size(term()) -> non_neg_integer().
collection_size(Subject) when is_list(Subject) ->
    length(Subject);
collection_size(Subject) when is_map(Subject) ->
    case beamtalk_tagged_map:class_of(Subject, 'Dictionary') of
        'Array' -> array:size(array_data(Subject));
        'Set' -> length(set_elements(Subject));
        'Bag' -> bag_size(Subject);
        _ -> dictionary_size(Subject)
    end.

%% The user-key count of a Dictionary tagged map — `maps:size/1` minus the
%% `'$beamtalk_class'` tag (if present), avoiding the full `maps:to_list/1` + sort
%% `dictionary_pairs/1` does. `size` is read on *every* render via `header_line`,
%% so it must stay cheap even for a large Dictionary (BT-2507).
-spec dictionary_size(map()) -> non_neg_integer().
dictionary_size(Map) ->
    case maps:is_key('$beamtalk_class', Map) of
        true -> maps:size(Map) - 1;
        false -> maps:size(Map)
    end.

%% The current window of `InspectorField` records for a collection at page `Page`
%% (0-based). Ordered collections (List/Array/Set) emit `#element` fields keyed by
%% their absolute 1-based index; keyed collections (Dictionary/Bag) emit
%% `#association` fields keyed by the entry key. Only the window is materialised.
-spec collection_fields(term(), non_neg_integer()) -> [field_map()].
collection_fields(Subject, Page) ->
    case keyed_collection_pairs(Subject) of
        {ok, Pairs} ->
            Window = window(Pairs, Page),
            [association_field(K, V) || {K, V} <- Window];
        not_keyed ->
            Offset = Page * ?PAGE_SIZE,
            Window = ordered_window(Subject, Page),
            [element_field(Offset + I, E) || {I, E} <- enumerate(Window)]
    end.

%% The page-`Page` window of an ordered collection's elements, materialising only
%% the window. For an `Array` this is `array:get/2` over the window's index range
%% — no full `array:to_list/1` per `fields` call. List/Set slice the in-memory
%% list (already cheap; no conversion) (BT-2507).
-spec ordered_window(term(), non_neg_integer()) -> [term()].
ordered_window(Subject, Page) when is_map(Subject) ->
    case beamtalk_tagged_map:class_of(Subject, 'Dictionary') of
        'Array' -> array_window(array_data(Subject), Page);
        _ -> window(ordered_elements(Subject), Page)
    end;
ordered_window(Subject, Page) ->
    window(ordered_elements(Subject), Page).

%% The page-`Page` window of an `array` via `array:get/2` over its index range —
%% at most `?PAGE_SIZE` elements, never the whole array. Indices are 0-based; an
%% empty range (past the end) yields `[]`.
-spec array_window(array:array(), non_neg_integer()) -> [term()].
array_window(Arr, Page) ->
    Start = Page * ?PAGE_SIZE,
    End = min(Start + ?PAGE_SIZE, array:size(Arr)),
    [array:get(I, Arr) || I <- lists:seq(Start, End - 1)].

%% The associations of a keyed collection (Dictionary key→value, Bag element→
%% count) as a `{Key, Value}` list, or `not_keyed` for an ordered collection.
-spec keyed_collection_pairs(term()) -> {ok, [{term(), term()}]} | not_keyed.
keyed_collection_pairs(Subject) when is_map(Subject) ->
    case beamtalk_tagged_map:class_of(Subject, 'Dictionary') of
        'Bag' -> {ok, bag_counts_pairs(Subject)};
        Class when Class =:= 'Array'; Class =:= 'Set' -> not_keyed;
        _ -> {ok, dictionary_pairs(Subject)}
    end;
keyed_collection_pairs(_) ->
    not_keyed.

%% The elements of an ordered collection (List/Array/Set) as a plain list, in
%% iteration order.
-spec ordered_elements(term()) -> [term()].
ordered_elements(Subject) when is_list(Subject) ->
    Subject;
ordered_elements(Subject) when is_map(Subject) ->
    case beamtalk_tagged_map:class_of(Subject, 'Dictionary') of
        'Array' -> array:to_list(array_data(Subject));
        'Set' -> set_elements(Subject);
        _ -> []
    end.

%% The `array`-backing of an `Array` tagged map. A forged/malformed tagged map
%% (e.g. `#{'$beamtalk_class' => 'Array', data => 42}`) whose `data` is missing or
%% not a real `array` must not crash `array:size/1`/`array:to_list/1` — degrade to
%% an empty array (BT-2509). `array:is_array/1` is total (false for any non-array).
-spec array_data(map()) -> array:array().
array_data(Subject) ->
    Data = maps:get(data, Subject, array:new()),
    case array:is_array(Data) of
        true -> Data;
        false -> array:new()
    end.

%% The `ordsets`-backed element list of a `Set` — `[]` for a forged map whose
%% `elements` field is not a list (must not crash `length/1`/`lists:nth/2`).
-spec set_elements(map()) -> [term()].
set_elements(Subject) ->
    case maps:get(elements, Subject, []) of
        Elements when is_list(Elements) -> Elements;
        _ -> []
    end.

%% A `Bag`'s total size — the sum of its element counts. A forged map whose
%% `counts` field is not a map degrades to `0` (`dictionary_pairs/1` requires a
%% map).
-spec bag_size(map()) -> non_neg_integer().
bag_size(Subject) ->
    lists:sum([C || {_K, C} <- bag_counts_pairs(Subject), is_integer(C)]).

%% The `counts` map of a `Bag` (element → count), tolerating a forged non-map
%% `counts` field by degrading to `#{}`.
-spec bag_counts(map()) -> map().
bag_counts(Subject) ->
    case maps:get(counts, Subject, #{}) of
        Counts when is_map(Counts) -> Counts;
        _ -> #{}
    end.

%% The `{Element, Count}` pairs of a `Bag`, tolerating a forged non-map `counts`.
-spec bag_counts_pairs(map()) -> [{term(), term()}].
bag_counts_pairs(Subject) ->
    dictionary_pairs(bag_counts(Subject)).

%% The user key→value pairs of a (tagged or plain) Dictionary map, in a stable
%% sorted order so windows are deterministic. The `'$beamtalk_class'` tag (if any)
%% is excluded.
-spec dictionary_pairs(map()) -> [{term(), term()}].
dictionary_pairs(Map) when is_map(Map) ->
    lists:sort(maps:to_list(maps:remove('$beamtalk_class', Map))).

%% The page-`Page` window (a `?PAGE_SIZE`-element slice) of `Items` — empty past
%% the end. `Page` is 0-based; `lists:sublist/3` is 1-based.
-spec window([term()], non_neg_integer()) -> [term()].
window(Items, Page) ->
    lists:sublist(Items, (Page * ?PAGE_SIZE) + 1, ?PAGE_SIZE).

%% Pair each item with its 1-based position within the given list slice.
-spec enumerate([term()]) -> [{pos_integer(), term()}].
enumerate(Items) ->
    lists:zip(lists:seq(1, length(Items)), Items).

%% An `#element` InspectorField for an ordered-collection element at 1-based
%% absolute index `Index` (the navigation key for `at:`), labelled `[Index]`.
-spec element_field(pos_integer(), term()) -> field_map().
element_field(Index, Value) ->
    Label = iolist_to_binary([<<"[">>, integer_to_binary(Index), <<"]">>]),
    inspector_field(Index, Label, Value, element, is_drillable(Value)).

%% An `#association` InspectorField for a keyed-collection entry (the key is the
%% navigation key for `at:`), labelled `Key →`.
-spec association_field(term(), term()) -> field_map().
association_field(Key, Value) ->
    Label = iolist_to_binary([label_for(Key), <<" →"/utf8>>]),
    inspector_field(Key, Label, Value, association, is_drillable(Value)).

%% Direct index into an ordered collection at 1-based `Index` (ADR 0095 §6 — no
%% full traversal for List/Array). Returns `{ok, Element}` or `out_of_range`.
-spec element_at(term(), integer()) -> {ok, term()} | out_of_range.
element_at(Subject, Index) when is_integer(Index), Index >= 1 ->
    case Subject of
        _ when is_list(Subject) ->
            case Index =< length(Subject) of
                true -> {ok, lists:nth(Index, Subject)};
                false -> out_of_range
            end;
        _ when is_map(Subject) ->
            case beamtalk_tagged_map:class_of(Subject, 'Dictionary') of
                'Array' ->
                    Arr = array_data(Subject),
                    case Index =< array:size(Arr) of
                        true -> {ok, array:get(Index - 1, Arr)};
                        false -> out_of_range
                    end;
                'Set' ->
                    Elements = set_elements(Subject),
                    case Index =< length(Elements) of
                        true -> {ok, lists:nth(Index, Elements)};
                        false -> out_of_range
                    end;
                _ ->
                    out_of_range
            end;
        _ ->
            out_of_range
    end;
element_at(_Subject, _Index) ->
    out_of_range.

%%====================================================================
%% Foreign process field derivation (ADR 0095 §3–§4)
%%====================================================================

%% The `process_info` items surfaced for a `#foreign` process, in a stable order
%% (ADR 0095 §4 — the `:observer`/LiveDashboard primitive set).
-define(FOREIGN_INFO_KEYS, [
    registered_name,
    current_function,
    initial_call,
    message_queue_len,
    status,
    memory,
    reductions
]).

%% Best-effort `#processInfo` fields for a foreign OTP pid: each available
%% `process_info` datum, plus a `state` field from a guarded `sys:get_state` when
%% the process is `sys`-compliant. Never raises — a process that dies mid-read
%% yields whatever was captured (possibly the single `#status => #unavailable`
%% diagnostic via the `available => false` guard in `fieldsOf`).
-spec foreign_fields(pid()) -> [field_map()].
foreign_fields(Pid) ->
    InfoFields =
        case erlang:process_info(Pid, ?FOREIGN_INFO_KEYS) of
            Items when is_list(Items) ->
                [process_info_field(K, V) || {K, V} <- Items, V =/= []];
            undefined ->
                [unavailable_field()]
        end,
    InfoFields ++ foreign_state_fields(Pid).

%% A guarded `sys:get_state` of a foreign process as a single drillable `state`
%% `#processInfo` field — or no field at all when the process is not
%% `sys`-compliant (best-effort, never a crash).
-spec foreign_state_fields(pid()) -> [field_map()].
foreign_state_fields(Pid) ->
    case beamtalk_process_navigation:guarded_state(Pid) of
        {ok, State} ->
            [inspector_field(state, <<"state">>, State, processInfo, is_drillable(State))];
        unavailable ->
            []
    end.

%% A `#processInfo` InspectorField for one `process_info` key/value datum.
-spec process_info_field(atom(), term()) -> field_map().
process_info_field(Key, Value) ->
    inspector_field(Key, label_for(Key), Value, processInfo, is_drillable(Value)).

%%====================================================================
%% Navigation (ADR 0095 §1)
%%====================================================================

-doc """
Drill into the field named `Name` (the `Inspector >> at:` shim).

Returns the raw `{ok, ChildCursor} | {error, #beamtalk_error{}}` tuple — the FFI
boundary (`beamtalk_erlang_proxy:coerce_result/1`) lifts it to a Beamtalk
`Result`, so the stdlib signature is `Result(Inspector, Error)` (the same shape
`beamtalk_process_navigation:from/1` uses). The child cursor's `parent` is
`Cursor` and its `path` extends this one's by `Name`; a miss yields a
`no_such_field` error.

For a `#collection`, `at:` resolves `Name` against the whole collection, not the
current window (ADR 0095 §6): an integer indexes an ordered collection directly
(no full traversal for List/Array), and a key looks up an association — so
`big at: 73` reaches the 73rd element even from page 0.
""".
-spec inspector(inspector(), term()) -> {ok, inspector()} | {error, #beamtalk_error{}}.
inspector(#{kind := collection, subject := Subject} = Cursor, Name) ->
    case collection_value_at(Subject, Name) of
        {ok, Value} -> drill_to(Cursor, Name, Value);
        error -> {error, no_such_field_error(Name)}
    end;
inspector(Cursor, Name) ->
    case find_field(Name, fieldsOf(Cursor)) of
        {ok, Value} -> drill_to(Cursor, Name, Value);
        error -> {error, no_such_field_error(Name)}
    end.

%% Build the child cursor for a successful drill: classify `Value`, parent it to
%% `Cursor`, and extend the path by the navigation `Name`.
-spec drill_to(inspector(), term(), term()) -> {ok, inspector()}.
drill_to(Cursor, Name, Value) ->
    ChildPath = pathOf(Cursor) ++ [Name],
    {ok, cursor(Value, Cursor, ChildPath)}.

%% Resolve a collection navigation key to its value across the *whole* collection
%% (not just the current window). An **ordered** collection (List/Array/Set) takes
%% an integer index directly (O(1) for Array). A **keyed** collection
%% (Dictionary/Bag) looks the key up by `maps:find/2` — O(log n), exact-key
%% semantics matching `Dictionary >> at:` (`beamtalk_map:at:`), never the
%% `'$beamtalk_class'` tag (BT-2507; was an O(n) `lists:keyfind/3` over the
%% materialised pairs).
-spec collection_value_at(term(), term()) -> {ok, term()} | error.
collection_value_at(Subject, Key) when is_map(Subject) ->
    case beamtalk_tagged_map:class_of(Subject, 'Dictionary') of
        'Bag' -> keyed_value_at(Key, bag_counts(Subject));
        Class when Class =:= 'Array'; Class =:= 'Set' -> ordered_value_at(Subject, Key);
        _ -> keyed_value_at(Key, Subject)
    end;
collection_value_at(Subject, Key) ->
    ordered_value_at(Subject, Key).

%% O(log n) exact-key lookup in a keyed collection's backing map. Never resolves
%% the `'$beamtalk_class'` tag (it is not a user association key).
-spec keyed_value_at(term(), map()) -> {ok, term()} | error.
keyed_value_at('$beamtalk_class', _Map) ->
    error;
keyed_value_at(Key, Map) ->
    maps:find(Key, Map).

%% Direct positional lookup in an ordered collection (List/Array/Set) — `error`
%% for a non-integer key or an out-of-range index.
-spec ordered_value_at(term(), term()) -> {ok, term()} | error.
ordered_value_at(Subject, Key) when is_integer(Key) ->
    case element_at(Subject, Key) of
        {ok, Value} -> {ok, Value};
        out_of_range -> error
    end;
ordered_value_at(_Subject, _Key) ->
    error.

%% Look up a field's value by its navigation name in a derived field list.
-spec find_field(term(), [field_map()]) -> {ok, term()} | error.
find_field(_Name, []) ->
    error;
find_field(Name, [#{name := Name, value := Value} | _]) ->
    {ok, Value};
find_field(Name, [_ | Rest]) ->
    find_field(Name, Rest).

-doc """
Return a new cursor on the `PageNum`-th window of a `#collection` (1-based, the
`Inspector >> page:` shim, ADR 0095 §6).

`page: 1` is the first window (the same elements `fields` shows on a fresh
cursor); `page: 2` is the next `?PAGE_SIZE`-element window, and so on. A page past
the end yields a cursor whose `fields` is empty (never an error). The new cursor
preserves `parent` and `path`; the original is unchanged (immutable). Sending
`page:` to a non-collection cursor is a `not_a_collection` error.
""".
-spec page(inspector(), term()) -> {ok, inspector()} | {error, #beamtalk_error{}}.
page(#{kind := collection, subject := Subject, parent := Parent, path := Path}, PageNum) when
    is_integer(PageNum), PageNum >= 1
->
    {ok, collection_cursor(Subject, PageNum - 1, Parent, Path)};
page(#{kind := collection}, _PageNum) ->
    {error,
        beamtalk_error:new(
            invalid_page,
            'Inspector',
            'page:',
            <<"page: expects a positive integer (1-based window)">>
        )};
page(_Cursor, _PageNum) ->
    {error,
        beamtalk_error:new(
            not_a_collection,
            'Inspector',
            'page:',
            <<"page: is only valid on a #collection cursor">>
        )}.

-doc """
Return a cursor on a freshly-captured snapshot of the same subject (`refresh`).

For a `#value` the subject is structurally identical; for an `#actor` the shared
guarded `sys:get_state` is re-issued, picking up state change since the previous
snapshot; a `#foreign` cursor re-reads `process_info` lazily on the next
`fields`. A `#collection` re-windows the *same* subject at its current page (the
immutable term cannot move on its own). The new cursor preserves `parent` and
`path`; the original is unchanged (immutable).
""".
-spec refresh(inspector()) -> inspector().
refresh(#{kind := actor, pid := Pid, parent := Parent, path := Path}) ->
    %% Re-snapshot as an `#actor`, not via `cursor/3` (→ `process_cursor`): a now
    %% dead actor would re-classify as `#foreign` and lose the actor kind. A dead
    %% actor stays `#actor` with `available => false` (BT-2509).
    actor_cursor(Pid, Parent, Path);
refresh(#{kind := foreign, pid := Pid, parent := Parent, path := Path}) ->
    cursor(Pid, Parent, Path);
refresh(#{kind := collection, subject := Subject, page := Page, parent := Parent, path := Path}) ->
    collection_cursor(Subject, Page, Parent, Path);
refresh(#{subject := Subject, parent := Parent, path := Path}) ->
    cursor(Subject, Parent, Path).

%%====================================================================
%% Evaluate-in-context (ADR 0095 §1, §7)
%%====================================================================

-doc """
Evaluate the Beamtalk expression `Src` against the cursor's subject (`Inspector
>> evaluate:`).

For a `#value` (or `#collection`) cursor, `Src` is compiled and evaluated with
`self` bound to the inspected value — a pure compiled dispatch (values are real
Beamtalk objects, ADR 0095 §7). For an `#actor` (or `#foreign`) cursor it returns
an `actor_eval_unsupported` error: actor evaluate-in-context cannot bind `self` to
a frozen snapshot and is a deferred follow-up (ADR 0095 §7) — drill with `at:`
instead.

Returns the raw `{ok, Value} | {error, #beamtalk_error{}}` tuple; the FFI boundary
lifts it to a Beamtalk `Result(Object, Error)`.
""".
-spec evaluate(inspector(), binary() | string()) ->
    {ok, term()} | {error, #beamtalk_error{}}.
evaluate(#{kind := Kind}, _Src) when Kind =:= actor; Kind =:= foreign ->
    {error, actor_eval_unsupported_error()};
evaluate(#{subject := Subject}, Src) ->
    eval_value(Subject, to_source(Src)).

%% Compile and evaluate `Source` (a string) with `self` bound to `Self`,
%% delegating to the workspace eval pipeline via `erlang:apply` to keep
%% `beamtalk_runtime` free of a compile-time dependency on `beamtalk_workspace`
%% (the same indirection `beamtalk_behaviour_intrinsics` uses for live patches).
%% A workspace that is not running, or an internal failure, degrades to an
%% `eval_unavailable` error — never a crash.
-spec eval_value(term(), string()) -> {ok, term()} | {error, #beamtalk_error{}}.
eval_value(Self, Source) ->
    try erlang:apply(beamtalk_repl_eval, eval_with_self, [Self, Source]) of
        {ok, Value} -> {ok, Value};
        {error, #beamtalk_error{} = Err} -> {error, Err};
        {error, Reason} -> {error, eval_failed_error(Reason)}
    catch
        error:undef ->
            {error, eval_unavailable_error()};
        Class:Reason ->
            ?LOG_DEBUG("inspector value evaluate failed", #{
                error_class => Class,
                reason => Reason,
                domain => [beamtalk, runtime]
            }),
            {error, eval_failed_error(Reason)}
    end.

%% Normalise an evaluate: argument (a Beamtalk String is a binary) to a string.
-spec to_source(binary() | string()) -> string().
to_source(Src) when is_binary(Src) -> unicode:characters_to_list(Src);
to_source(Src) when is_list(Src) -> Src.

%%====================================================================
%% Rendering (ADR 0095 §7)
%%====================================================================

-doc """
Render the cursor as an indented text tree to `Depth` levels.

Depth 1 is the header line plus the immediate fields; deeper levels expand each
drillable field's own fields recursively. A **pid-keyed seen-set** guards actor
descents: revisiting an already-expanded actor pid emits a back-reference marker
(`↩ Actor(...)`) instead of recursing (ADR 0095 Cycle handling). Pure value
sub-trees cannot cycle (ADR 0042) and are bounded by `Depth`.
""".
-spec printString(inspector(), integer()) -> binary().
printString(Cursor, Depth) ->
    iolist_to_binary(render(Cursor, Depth, 0, #{})).

%% Render a cursor's tree. `Indent` is the nesting level (2 spaces each); `Seen`
%% is the set of actor pids on the **current ancestor path** (the cycle guard).
%% `Seen` is passed *down* into children but never propagated back to siblings —
%% so it tracks true ancestry (a cycle) and not mere prior visitation (a shared
%% diamond reference, which must still expand, not back-reference).
-spec render(inspector(), integer(), non_neg_integer(), map()) -> iolist().
render(Cursor, Depth, Indent, Seen0) ->
    Header = header_line(Cursor),
    %% Add this cursor's pid to the ancestor set for the descent below it.
    Seen = mark_seen(Cursor, Seen0),
    case Depth =< 0 of
        true ->
            [pad(Indent), Header];
        false ->
            FieldLines = [
                render_field(F, Depth, Indent + 1, Seen)
             || F <- fieldsOf(Cursor)
            ],
            [pad(Indent), Header, FieldLines]
    end.

%% Add the cursor's actor pid (if any) to the ancestor seen-set.
-spec mark_seen(inspector(), map()) -> map().
mark_seen(#{kind := actor, pid := Pid}, Seen) when is_pid(Pid) ->
    Seen#{Pid => true};
mark_seen(_, Seen) ->
    Seen.

%% Render one field line, expanding drillable children when depth remains. A
%% drilled value that is an actor pid already on the ancestor path yields a
%% back-reference marker rather than recursing (the pid-keyed cycle guard).
-spec render_field(map(), integer(), non_neg_integer(), map()) -> iolist().
render_field(#{label := Label, value := Value, drillable := true}, Depth, Indent, Seen) when
    Depth > 1
->
    case actor_pid(Value) of
        {ok, Pid} ->
            case maps:is_key(Pid, Seen) of
                true ->
                    [$\n, pad(Indent), Label, <<": ">>, back_reference(Pid)];
                false ->
                    expand_field(Label, Value, Depth, Indent, Seen)
            end;
        not_actor ->
            expand_field(Label, Value, Depth, Indent, Seen)
    end;
render_field(#{label := Label, value := Value}, _Depth, Indent, _Seen) ->
    scalar_field_line(Label, Value, Indent).

%% Expand a drillable field into its own (depth-1) sub-tree, or fall back to a
%% scalar line for a non-navigable leaf. The ancestor `Seen` set is passed down
%% unchanged; the child adds itself for its own descent only.
-spec expand_field(binary(), term(), integer(), non_neg_integer(), map()) -> iolist().
expand_field(Label, Value, Depth, Indent, Seen) ->
    case beamtalk_tagged_map:is_tagged(Value) orelse actor_pid(Value) =/= not_actor of
        true ->
            Child = root_cursor(Value, []),
            ChildIo = render(Child, Depth - 1, 0, Seen),
            [
                $\n,
                pad(Indent),
                Label,
                <<": ">>,
                string:trim(iolist_to_binary(ChildIo), leading)
            ];
        false ->
            scalar_field_line(Label, Value, Indent)
    end.

%% The cycle back-reference marker for a revisited actor pid, e.g.
%% `↩ Actor(Counter, <0.123.0>)` — informative class + pid, never re-expanded.
-spec back_reference(pid()) -> iolist().
back_reference(Pid) ->
    [
        <<"↩ Actor("/utf8>>,
        actor_class_label(Pid),
        <<", ">>,
        beamtalk_opaque_ops:pid_to_string(Pid),
        <<")">>
    ].

-spec scalar_field_line(binary(), term(), non_neg_integer()) -> iolist().
scalar_field_line(Label, Value, Indent) ->
    [$\n, pad(Indent), Label, <<": ">>, value_string(Value)].

%% The header line for a cursor, e.g. `Inspector(Point)` or
%% `Inspector(Counter)` / `Inspector(Integer = 3)` for a scalar leaf.
%% A `#collection` names its class and full size (`Inspector(List, 100000)`); a
%% `#foreign` names its class and pid (`Inspector(gen_server, <0.42.0>)`).
-spec header_line(inspector()) -> iolist().
header_line(#{kind := actor, available := false, pid := Pid}) ->
    [<<"Inspector(">>, actor_class_label(Pid), <<" = #unavailable)">>];
header_line(#{kind := actor, pid := Pid}) ->
    [<<"Inspector(">>, actor_class_label(Pid), <<")">>];
header_line(#{kind := foreign, available := false, pid := Pid}) ->
    [<<"Inspector(">>, value_string(Pid), <<" = #unavailable)">>];
header_line(#{kind := foreign, pid := Pid}) ->
    [<<"Inspector(">>, foreign_class_label(Pid), <<", ">>, value_string(Pid), <<")">>];
header_line(#{kind := collection, subject := Subject} = Cursor) ->
    ClassName = beamtalk_primitive:class_of(Subject),
    Size = integer_to_binary(sizeOf(Cursor)),
    [<<"Inspector(">>, atom_to_binary(ClassName, utf8), <<", ">>, Size, <<")">>];
header_line(#{subject := Subject}) ->
    case beamtalk_tagged_map:is_tagged(Subject) of
        true ->
            ClassName = beamtalk_tagged_map:class_of(Subject, 'Object'),
            [<<"Inspector(">>, atom_to_binary(ClassName, utf8), <<")">>];
        false ->
            [<<"Inspector(">>, value_string(Subject), <<")">>]
    end.

%% A best-effort label for a foreign process — its `current_function`'s module,
%% else `process`.
-spec foreign_class_label(pid()) -> binary().
foreign_class_label(Pid) ->
    case erlang:process_info(Pid, current_function) of
        {current_function, {Mod, _F, _A}} when is_atom(Mod) -> atom_to_binary(Mod, utf8);
        _ -> <<"process">>
    end.

%% A best-effort class label for an actor pid (the behaviour class name, or the
%% pid string if unknown).
-spec actor_class_label(pid()) -> binary().
actor_class_label(Pid) when is_pid(Pid) ->
    case actor_class(Pid) of
        nil -> value_string(Pid);
        Class -> atom_to_binary(Class, utf8)
    end.

%% The actor's behaviour class atom from its process dictionary, or nil.
-spec actor_class(pid()) -> atom() | nil.
actor_class(Pid) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, Dict} when is_list(Dict) ->
            case lists:keyfind('$beamtalk_actor', 1, Dict) of
                {'$beamtalk_actor', Class} when is_atom(Class) -> Class;
                _ -> nil
            end;
        _ ->
            nil
    end.

%% Indent padding: two spaces per level.
-spec pad(non_neg_integer()) -> binary().
pad(N) ->
    list_to_binary(lists:duplicate(N * 2, $\s)).

%% A short display string for a leaf value (re-using the primitive renderer).
%% The primitive renderer assumes well-formed terms and can raise on pathological
%% input (e.g. an improper list reachable from foreign OTP state), so fall back to
%% a raw `~p` rendering to uphold the inspector's "never raises" contract.
-spec value_string(term()) -> binary().
value_string(Value) ->
    try
        beamtalk_primitive:print_string(Value)
    catch
        _:_ -> iolist_to_binary(io_lib:format("~p", [Value]))
    end.

%%====================================================================
%% Wire form — asDictionaries / asDictionary (ADR 0095 §7)
%%====================================================================

-doc """
Serialise the cursor's drillable fields to the cross-surface wire form
(`Inspector >> asDictionaries`, ADR 0095 §7).

Returns one `Dictionary` per `InspectorField`, each carrying its named typed
fields: `#name` (the navigation key for `at:` — an atom/integer/key), `#label`
(display label), `#value` (a JSON-stable display string, the same one-line
rendering the text tree uses — a browser drills via `at:` rather than embedding
the raw term), `#kind` (`#slot`/`#element`/`#association`/`#processInfo`), and
`#drillable`. This is the *exact* pattern `SupervisionTree asDictionaries`
(ADR 0092) uses, so the schema is pinned by the typed records. The cursor-level
envelope (`kind`/`path`/`childCount`/page) is carried by `asDictionary/1`.
""".
-spec asDictionaries(inspector()) -> [map()].
asDictionaries(Cursor) ->
    [field_dictionary(F) || F <- fieldsOf(Cursor)].

-doc """
Serialise the whole cursor to a single wire-form `Dictionary`
(`Inspector >> asDictionary`, ADR 0095 §7).

Carries the cursor-level envelope the MCP/browser surfaces need to render a
navigable node and fetch subsequent windows lazily: `#kind` (the subject kind),
`#path` (the breadcrumb of drilled navigation keys from the root), `#childCount`
(the cheap full element count for a `#collection`, else the field count),
`#page` (the 1-based window index — `1` for non-collections), and `#fields`
(the `asDictionaries/1` window of field records).
""".
-spec asDictionary(inspector()) -> map().
asDictionary(#{kind := Kind, page := Page, path := Path} = Cursor) ->
    %% Derive `fields` once and reuse it for both the wire records and the
    %% non-collection `childCount` — for a `#foreign` cursor `fieldsOf/1` does a
    %% `process_info` + a guarded `sys:get_state`, so computing it twice would
    %% double that (potentially blocking) work.
    Fields = fieldsOf(Cursor),
    #{
        kind => Kind,
        path => Path,
        childCount => child_count(Cursor, Fields),
        page => Page + 1,
        fields => [field_dictionary(F) || F <- Fields]
    }.

%% The full child count for the envelope: the cheap collection size for a
%% `#collection` (not a window walk), else the number of already-derived fields.
-spec child_count(inspector(), [field_map()]) -> non_neg_integer().
child_count(#{kind := collection} = Cursor, _Fields) ->
    sizeOf(Cursor);
child_count(_Cursor, Fields) ->
    length(Fields).

%% One field record as a wire-form dictionary. `value` is rendered to a
%% JSON-stable display string (the same one the text tree shows); `name` stays a
%% raw navigation key (atom/integer/term) so the surface can pass it back to
%% `at:`.
-spec field_dictionary(field_map()) -> map().
field_dictionary(#{
    name := Name, label := Label, value := Value, kind := Kind, drillable := Drillable
}) ->
    #{
        name => Name,
        label => Label,
        value => value_string(Value),
        kind => Kind,
        drillable => Drillable
    }.

%%====================================================================
%% Errors
%%====================================================================

-spec no_such_field_error(term()) -> #beamtalk_error{}.
no_such_field_error(Name) ->
    beamtalk_error:new(
        no_such_field,
        'Inspector',
        'at:',
        iolist_to_binary([<<"no field named ">>, label_for(Name)])
    ).

%% The deferred-feature error for `evaluate:` against an actor/foreign subject
%% (ADR 0095 §7): actor evaluate-in-context cannot bind `self` to a frozen
%% snapshot and is a follow-up; drill with `at:` instead.
-spec actor_eval_unsupported_error() -> #beamtalk_error{}.
actor_eval_unsupported_error() ->
    beamtalk_error:new(
        actor_eval_unsupported,
        'Inspector',
        'evaluate:',
        <<"evaluate: runs against values; actor evaluate-in-context is not in v1 — drill with at:">>
    ).

%% A value `evaluate:` whose expression failed to compile or evaluate.
-spec eval_failed_error(term()) -> #beamtalk_error{}.
eval_failed_error(Reason) ->
    beamtalk_error:new(
        eval_failed,
        'Inspector',
        'evaluate:',
        iolist_to_binary(io_lib:format("evaluate: failed: ~tp", [Reason]))
    ).

%% The workspace eval pipeline is not running (e.g. a bare runtime), so value
%% `evaluate:` cannot compile the expression.
-spec eval_unavailable_error() -> #beamtalk_error{}.
eval_unavailable_error() ->
    beamtalk_error:new(
        eval_unavailable,
        'Inspector',
        'evaluate:',
        <<"evaluate: requires a running workspace eval pipeline">>
    ).
