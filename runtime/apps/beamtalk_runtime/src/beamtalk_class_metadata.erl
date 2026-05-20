%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_metadata).

%%% **DDD Context:** Object System Context

-moduledoc """
ETS table owner for the unified `beamtalk_class_metadata` table.

Holds the class-lifetime metadata that used to live in three separate
class-keyed ETS tables (`beamtalk_class_module`, `beamtalk_class_methods`,
`beamtalk_class_hierarchy`). They were consolidated (BT-2222) because they
shared a key (`ClassName`), a concurrency profile (`set` +
`{read_concurrency, true}`), a single writer (the class gen_server), and the
same three lifecycle points (`init`, `update_class`, `terminate`) — and the
`Module` field was duplicated across two of them.

## Row shape

Each row is a `#class_metadata{}` record keyed by `name`:

* `module`     — defining BEAM module, or `undefined` if unset
* `selectors`  — local class-method selectors, or `undefined` if unset
* `superclass` — superclass atom, `none` for a root class, or `undefined` if unset

`undefined` is the "field never written" sentinel and is distinct from `none`
(an explicitly root class). The typed lookups treat `undefined` as `not_found`,
so a row that carries only one field behaves exactly like the old per-table
absence of the others.

## Table properties

* Type: `set`, `{keypos, #class_metadata.name}` — one row per class name.
* Access: `public`, `named_table` — any process reads; written only by the
  class gen_server via `beamtalk_object_class`.
* `{read_concurrency, true}` — the hierarchy/module/methods reads are on hot
  dispatch and supervisor-init paths.
* Heir (BT-1888): created at app start by `beamtalk_runtime_app:start/2` (the
  long-lived app-master process owns it), then `start/2` re-runs `ensure_*` once
  the supervisor is alive so `beamtalk_runtime_sup` is set as `{heir, ...}`. The
  table therefore survives both a class-process crash and an app-master swap.

## Concurrency

`new/0` uses try/catch around `ets:new/2` for the TOCTOU race between
`ets:info/1` and `ets:new/2`. All read/write ops guard against `badarg` from
a table deleted between an existence check and the op (teardown/shutdown).
""".

-include("beamtalk.hrl").

-export([
    new/0,
    insert/4,
    delete/1,
    lookup_module/1,
    lookup_methods/1,
    lookup_superclass/1,
    match_subclasses/1,
    foldl/2,
    all_builtins/0
]).

-define(TABLE, beamtalk_class_metadata).

-type class_name() :: atom().
-type superclass() :: class_name() | none.
-type selector() :: atom().

-record(class_metadata, {
    name :: class_name(),
    module :: module() | undefined,
    selectors :: [selector()] | undefined,
    superclass :: superclass() | undefined
}).

%%====================================================================
%% Table lifecycle
%%====================================================================

-doc """
Ensure the unified class metadata ETS table exists (idempotent).

Creates the table on first call; subsequent calls retroactively set the heir
once the supervisor is alive (BT-1888). Safe to call concurrently — a try/catch
handles the race between `ets:info/1` and `ets:new/2`.
""".
-spec new() -> ok.
new() ->
    case ets:info(?TABLE) of
        undefined ->
            try
                ets:new(
                    ?TABLE,
                    [
                        set,
                        public,
                        named_table,
                        {keypos, #class_metadata.name},
                        {read_concurrency, true}
                        | beamtalk_class_registry:heir_option()
                    ]
                ),
                ok
            catch
                error:badarg -> ok
            end;
        _ ->
            beamtalk_class_registry:maybe_set_heir(?TABLE),
            ok
    end.

%%====================================================================
%% Writes
%%====================================================================

-doc """
Insert or overwrite the **entire** metadata row for a class.

This is a full-row write, not a per-field merge: every call replaces all of
`module`, `selectors`, and `superclass`. The class gen_server always supplies
all three together (`init`/`update_class`), so the write is atomic for readers.
Do NOT call this to update a single field — the others would be wiped. Pass
`undefined` only to leave a field genuinely unset (used by tests that exercise
one field in isolation).
""".
-spec insert(
    class_name(), module() | undefined, [selector()] | undefined, superclass() | undefined
) ->
    ok.
insert(Name, Module, Selectors, Superclass) ->
    Row = #class_metadata{
        name = Name, module = Module, selectors = Selectors, superclass = Superclass
    },
    new(),
    try
        ets:insert(?TABLE, Row),
        ok
    catch
        error:badarg ->
            %% Table vanished between new/0 and ets:insert/2 — recreate and retry once.
            new(),
            ets:insert(?TABLE, Row),
            ok
    end.

-doc """
Remove a class's metadata row.

Called from the class gen_server `terminate/2`. Safe if the row — or the table —
does not exist.
""".
-spec delete(class_name()) -> ok.
delete(Name) ->
    try
        ets:delete(?TABLE, Name),
        ok
    catch
        error:badarg -> ok
    end.

%%====================================================================
%% Reads
%%====================================================================

-doc "Look up a class's module name. `undefined`/missing yields `not_found`.".
-spec lookup_module(class_name()) -> {ok, module()} | not_found.
lookup_module(Name) ->
    case field(Name, #class_metadata.module) of
        undefined -> not_found;
        Module -> {ok, Module}
    end.

-doc """
Look up a class's module and local class-method selectors.

Returns `{ok, Module, Selectors}` only when the selectors field has been written
(an empty list is valid); otherwise `not_found`.
""".
-spec lookup_methods(class_name()) -> {ok, module(), [selector()]} | not_found.
lookup_methods(Name) ->
    case row(Name) of
        {ok, #class_metadata{selectors = undefined}} -> not_found;
        {ok, #class_metadata{module = Module, selectors = Selectors}} -> {ok, Module, Selectors};
        not_found -> not_found
    end.

-doc """
Look up a class's superclass.

Returns `{ok, none}` for a root class and `{ok, Super}` otherwise; `not_found`
when the superclass field has not been written.
""".
-spec lookup_superclass(class_name()) -> {ok, superclass()} | not_found.
lookup_superclass(Name) ->
    case field(Name, #class_metadata.superclass) of
        undefined -> not_found;
        Super -> {ok, Super}
    end.

-doc """
Return all class names whose direct superclass is `Name`.

Returns `[]` if the table does not exist.
""".
-spec match_subclasses(class_name()) -> [class_name()].
match_subclasses(Name) ->
    case ets:info(?TABLE) of
        undefined ->
            [];
        _ ->
            try
                %% Build the match pattern from record field indices rather than a
                %% literal tuple, so it stays correct if the record's fields are
                %% reordered or extended. (Record syntax `#class_metadata{_ = '_'}`
                %% can't be used here — it assigns the atom '_' to typed fields and
                %% trips dialyzer.) Bind the name column, match the superclass column.
                Pattern = erlang:make_tuple(
                    record_info(size, class_metadata),
                    '_',
                    [
                        {1, class_metadata},
                        {#class_metadata.name, '$1'},
                        {#class_metadata.superclass, Name}
                    ]
                ),
                Matches = ets:match(?TABLE, Pattern),
                [N || [N] <- Matches]
            catch
                error:badarg -> []
            end
    end.

-doc """
Fold over `{ClassName, Superclass}` pairs for every row with a known superclass.

Rows whose superclass field is unset (`undefined`) are skipped, preserving the
behaviour of the former hierarchy table, which only contained rows that had a
superclass written. Returns `Acc0` if the table does not exist.
""".
-spec foldl(fun(({class_name(), superclass()}, Acc) -> Acc), Acc) -> Acc.
foldl(Fun, Acc0) ->
    case ets:info(?TABLE) of
        undefined ->
            Acc0;
        _ ->
            try
                ets:foldl(
                    fun
                        (#class_metadata{superclass = undefined}, Acc) -> Acc;
                        (#class_metadata{name = N, superclass = S}, Acc) -> Fun({N, S}, Acc)
                    end,
                    Acc0,
                    ?TABLE
                )
            catch
                error:badarg -> Acc0
            end
    end.

-doc """
Return all stdlib builtin class names known to the Rust compiler.

These classes are already in `ClassHierarchy::with_builtins()` on the Rust side
with richer data than `__beamtalk_meta/0` provides. Crash recovery skips them so
the Rust compiler's richer definitions take precedence.

Keep this list in sync with `generated_builtins.rs::is_generated_builtin_class`
and the `Future` runtime-only class.
""".
-spec all_builtins() -> [atom()].
all_builtins() ->
    [
        'Actor',
        'Array',
        'BEAMError',
        'BeamtalkInterface',
        'Behaviour',
        'Block',
        'Boolean',
        'Character',
        'Class',
        'ClassBuilder',
        'Collection',
        'CompiledMethod',
        'DateTime',
        'Dictionary',
        'Erlang',
        'ErlangModule',
        'Error',
        'Exception',
        'ExitError',
        'False',
        'File',
        'FileHandle',
        'Float',
        'Future',
        'InstantiationError',
        'Integer',
        'JSON',
        'List',
        'Metaclass',
        'Number',
        'Object',
        'Pid',
        'Port',
        'ProtoObject',
        'Random',
        'Reference',
        'Regex',
        'RuntimeError',
        'Set',
        'StackFrame',
        'Stream',
        'String',
        'Subprocess',
        'Symbol',
        'System',
        'TestCase',
        'TestResult',
        'TestRunner',
        'ThrowError',
        'Timer',
        'TranscriptStream',
        'True',
        'Tuple',
        'TypeError',
        'UndefinedObject',
        'Value',
        'WorkspaceInterface'
    ].

%%====================================================================
%% Internal
%%====================================================================

%% Fetch a single field via ets:lookup_element/4 (OTP 26+), which copies only
%% that element instead of the whole row — measurably faster than ets:lookup/2
%% on the hierarchy-walk hot path (inherits_from/2). The `undefined` default
%% collapses the three not_found cases (table absent, key absent, field unset)
%% into one return value, since none of them is a valid written field value.
-spec field(class_name(), pos_integer()) -> term().
field(Name, Pos) ->
    case ets:info(?TABLE) of
        undefined ->
            undefined;
        _ ->
            try
                ets:lookup_element(?TABLE, Name, Pos, undefined)
            catch
                error:badarg -> undefined
            end
    end.

-spec row(class_name()) -> {ok, #class_metadata{}} | not_found.
row(Name) ->
    case ets:info(?TABLE) of
        undefined ->
            not_found;
        _ ->
            try ets:lookup(?TABLE, Name) of
                [Row] -> {ok, Row};
                [] -> not_found
            catch
                error:badarg -> not_found
            end
    end.
