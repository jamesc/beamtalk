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
  kind    => value | actor,    %% #value (structural) | #actor (snapshot)
  pid     => pid() | nil,       %% the live actor pid, for refresh (nil for values)
  subject => term(),            %% the value itself, or the captured snapshot (actors)
  parent  => inspector() | nil, %% the cursor drilled from (nil at root)
  path    => [term()]           %% breadcrumb of drilled names from root
}
```

The handle is immutable: `at:` and `refresh` return *new* handles.

## Kind & state capture (ADR 0095 §3–§4)

- `#value` — a tagged `Value` map. Fields are its user `field:` slots in ADR-0094
  sort order, read purely (no process contact).
- `#actor` — a live pid. State is captured **at `on:`** via the shared
  timeout-guarded `sys:get_state` (`beamtalk_process_navigation:guarded_state/1`);
  the frozen snapshot is the cursor's `subject`, reused for all drilling and
  reset only by `refresh`. A busy/dead/non-`sys` actor degrades to a single
  `InspectorField name: #status value: #unavailable` — never a crash.

`#collection` / `#foreign` kinds, value `evaluate:`, windowing, and the pid-keyed
cycle guard for deep actor graphs are later phases (ADR 0095 §6–§7).
""".

%% Selector→function mapping (beamtalk_erlang_proxy): a single-keyword selector
%% strips its trailing colon (`on:` → `on`, `fieldsOf:` → `fieldsOf`); a
%% multi-keyword selector uses its *first* keyword as the function name with all
%% arguments positional (`inspector:at:` → `inspector/2`, `printString:depth:` →
%% `printString/2`).
-export([
    on/1,
    subjectOf/1,
    kindOf/1,
    fieldsOf/1,
    inspector/2,
    parentOf/1,
    pathOf/1,
    refresh/1,
    printString/2
]).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% The `Inspector` cursor handle. The `'$beamtalk_class'` tag lets the spec
%% reader map FFI returns of this shape to the Beamtalk `Inspector` type.
-type inspector() :: #{
    '$beamtalk_class' := 'Inspector',
    kind := value | actor,
    pid := pid() | nil,
    available := boolean(),
    subject := term(),
    parent := inspector() | nil,
    path := [term()]
}.

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

%% Build a root cursor (no parent) for a subject, classifying kind and capturing
%% actor state. `Path` is the breadcrumb a drilled child inherits.
-spec root_cursor(term(), [term()]) -> inspector().
root_cursor(Subject, Path) ->
    cursor(Subject, nil, Path).

%% Build a cursor with an explicit parent. An actor subject arrives either as a
%% bare pid or — the usual Beamtalk handle — a `#beamtalk_object{}` wrapping the
%% pid (resolving a `{registered, Name}` ref to the live pid). Everything else is
%% a `#value`.
-spec cursor(term(), inspector() | nil, [term()]) -> inspector().
cursor(#beamtalk_object{pid = Pid}, Parent, Path) when is_pid(Pid) ->
    actor_cursor(Pid, Parent, Path);
cursor(#beamtalk_object{pid = {registered, Name}}, Parent, Path) ->
    case erlang:whereis(Name) of
        Pid when is_pid(Pid) -> actor_cursor(Pid, Parent, Path);
        _ -> value_cursor(unavailable, Parent, Path)
    end;
cursor(Subject, Parent, Path) when is_pid(Subject) ->
    actor_cursor(Subject, Parent, Path);
cursor(Subject, Parent, Path) ->
    value_cursor(Subject, Parent, Path).

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
        parent => Parent,
        path => Path
    }.

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

`#value` → its user `field:` slots in ADR-0094 sort order. `#actor` → its state
slots from the frozen snapshot, or the single `#status => #unavailable`
diagnostic field when the snapshot could not be captured.
""".
-spec fieldsOf(inspector()) -> [field_map()].
fieldsOf(#{kind := actor, available := false}) ->
    [unavailable_field()];
fieldsOf(#{subject := Subject}) ->
    case beamtalk_tagged_map:is_tagged(Subject) of
        true -> slot_fields(Subject);
        false -> []
    end.

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
%% slots, or an actor pid) — i.e. there is something to drill into. Bare scalars
%% (numbers, atoms/symbols, booleans, nil, strings) are leaves.
-spec is_drillable(term()) -> boolean().
is_drillable(Value) ->
    actor_pid(Value) =/= not_actor orelse beamtalk_tagged_map:is_tagged(Value).

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
""".
-spec inspector(inspector(), term()) -> {ok, inspector()} | {error, #beamtalk_error{}}.
inspector(Cursor, Name) ->
    Fields = fieldsOf(Cursor),
    case find_field(Name, Fields) of
        {ok, Value} ->
            ChildPath = pathOf(Cursor) ++ [Name],
            Child = cursor(Value, Cursor, ChildPath),
            {ok, Child};
        error ->
            {error, no_such_field_error(Name)}
    end.

%% Look up a field's value by its navigation name in a derived field list.
-spec find_field(term(), [field_map()]) -> {ok, term()} | error.
find_field(_Name, []) ->
    error;
find_field(Name, [#{name := Name, value := Value} | _]) ->
    {ok, Value};
find_field(Name, [_ | Rest]) ->
    find_field(Name, Rest).

-doc """
Return a cursor on a freshly-captured snapshot of the same subject (`refresh`).

For a `#value` the subject is structurally identical; for an `#actor` the shared
guarded `sys:get_state` is re-issued, picking up state change since the previous
snapshot. The new cursor preserves `parent` and `path`; the original is
unchanged (immutable).
""".
-spec refresh(inspector()) -> inspector().
refresh(#{kind := actor, pid := Pid, parent := Parent, path := Path}) ->
    cursor(Pid, Parent, Path);
refresh(#{subject := Subject, parent := Parent, path := Path}) ->
    cursor(Subject, Parent, Path).

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
-spec header_line(inspector()) -> iolist().
header_line(#{kind := actor, available := false, pid := Pid}) ->
    [<<"Inspector(">>, actor_class_label(Pid), <<" = #unavailable)">>];
header_line(#{kind := actor, pid := Pid}) ->
    [<<"Inspector(">>, actor_class_label(Pid), <<")">>];
header_line(#{subject := Subject}) ->
    case beamtalk_tagged_map:is_tagged(Subject) of
        true ->
            ClassName = beamtalk_tagged_map:class_of(Subject, 'Object'),
            [<<"Inspector(">>, atom_to_binary(ClassName, utf8), <<")">>];
        false ->
            [<<"Inspector(">>, value_string(Subject), <<")">>]
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
-spec value_string(term()) -> binary().
value_string(Value) ->
    beamtalk_primitive:print_string(Value).

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
