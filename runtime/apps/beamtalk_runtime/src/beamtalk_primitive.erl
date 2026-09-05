%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_primitive).

%%% **DDD Context:** Object System Context

-moduledoc """
Primitive type dispatch and reflection.

Provides uniform dispatch and class identity for primitive types (integers,
strings, etc.) and tagged maps. Enables reflection like `42 class` → `'Integer'`.

See also: docs/internal/design-self-as-object.md Section 3.3
""".
-export([
    class_of/1,
    class_of_object/1,
    class_of_object_by_name/1,
    send/3,
    responds_to/2,
    class_responds_to/2,
    class_name_to_module/1,
    class_name_from_tag/1,
    print_string/1,
    display_string/1,
    process_label/1,
    pid_label/1,
    block_label/1,
    is_object/1,
    is_utf8/1
]).

-include("beamtalk.hrl").

%% Compiled stdlib modules are generated from Core Erlang, not .erl source.
% elp:fixme W0048 intentional suppression for dynamic dispatch
-dialyzer({nowarn_function, [send/3, responds_to/2]}).

%%% ============================================================================
%%% Public API
%%% ============================================================================

-doc """
True if `X` is a Beamtalk object whose comparison must route through message
dispatch rather than a bare Erlang term-order BIF (BT-2710).

Erlang's `<`/`=<`/`>`/`>=` define a *total order over every term* and never
raise, so a user value-type compared with a bare BIF would silently compare by
term-order instead of dispatching to its overloaded operator. The comparison
codegen guard therefore discriminates on "is this a Beamtalk object?" — not
"is this a number" (the arithmetic guard's `is_number`, which is safe only
because non-numbers there `badarith`).

Returns `true` for value-type / tagged-collection instances (maps carrying the
`'$beamtalk_class'` key), live actor and class objects (`#beamtalk_object{}`),
and actor process refs (pids). Primitives — numbers, atoms, binaries
(strings), characters (integers), lists, and plain (untagged) maps — return
`false`, so the fast path keeps Erlang's total term-order for them.

Limitation (single-node assumption): the pid case delegates to
`beamtalk_actor:is_beamtalk_actor/1`, which reads `process_info(Pid,
dictionary)` and so returns `false` for a **remote** actor pid (another node)
just as it does for a dead one. A comparison of two remote actor refs would
therefore take the bare term-order path rather than dispatching. This is
correct for single-node deployments (actor refs are always local); distributed
comparison of actor references is not a supported use case. Tagged value-type
maps and `#beamtalk_object{}` refs are node-agnostic and unaffected.
""".
-spec is_object(term()) -> boolean().
is_object(X) when is_map(X) ->
    is_map_key('$beamtalk_class', X);
is_object(#beamtalk_object{}) ->
    true;
is_object(X) when is_pid(X) ->
    beamtalk_actor:is_beamtalk_actor(X);
is_object(_) ->
    %% Non-pid primitives (numbers, atoms, lists, plain maps, …) are never
    %% objects — return via a local pattern match rather than paying a
    %% cross-module call to is_beamtalk_actor/1 (which only guards `is_pid`).
    false.

-doc """
Determine the Beamtalk class of any value.

Bare binaries (BT-2999): `String` is a subclass of `Binary` and both share the
single BEAM `binary()` representation, so a raw binary carries no runtime tag
saying which one it is. Valid UTF-8 answers `String` (the ambiguous but
overwhelmingly common case); a binary that is *not* valid UTF-8 cannot be a
`String` at all, so it answers `Binary`.
""".
-spec class_of(term()) -> atom().
class_of(X) when is_integer(X) -> 'Integer';
class_of(X) when is_float(X) -> 'Float';
class_of(X) when is_binary(X) ->
    case is_utf8(X) of
        true -> 'String';
        false -> 'Binary'
    end;
class_of(true) ->
    'True';
class_of(false) ->
    'False';
class_of(nil) ->
    'UndefinedObject';
class_of(X) when is_function(X) -> 'Block';
class_of(X) when is_atom(X) -> 'Symbol';
class_of(X) when is_list(X) -> 'List';
class_of(X) when is_map(X) ->
    beamtalk_tagged_map:class_of(X, 'Dictionary');
class_of({beamtalk_future, _} = Future) ->
    %% BT-840: Auto-await tagged futures before type inspection.
    class_of(beamtalk_future:await(Future));
class_of(X) when is_tuple(X), tuple_size(X) >= 2, element(1, X) =:= beamtalk_object ->
    % Extract class field from #beamtalk_object{}
    element(2, X);
class_of(X) when is_tuple(X), tuple_size(X) =:= 4, element(1, X) =:= beamtalk_supervisor ->
    % Extract class name from {beamtalk_supervisor, ClassName, Module, Pid}
    element(2, X);
class_of(X) when is_tuple(X) -> 'Tuple';
class_of(X) when is_pid(X) -> 'Pid';
class_of(X) when is_port(X) -> 'Port';
class_of(X) when is_reference(X) -> 'Reference';
class_of(_) ->
    'Object'.

-doc """
Return the class of any value as a first-class class object (BT-412).

ADR 0036: For class objects (tagged with " class" suffix) returns a real
metaclass object instead of the sentinel atom 'Metaclass'. For metaclass
objects (tagged with 'Metaclass'), returns the same struct (idempotent) to
enable the self-grounding invariant: `Metaclass class class == Metaclass class`.
""".
-spec class_of_object(term()) -> #beamtalk_object{} | atom().
class_of_object({beamtalk_future, _} = Future) ->
    %% BT-840: Auto-await tagged futures before class object inspection.
    class_of_object(beamtalk_future:await(Future));
class_of_object(#beamtalk_object{class = 'Metaclass', class_mod = ClassMod, pid = Pid}) ->
    %% ADR 0036 self-grounding: class of a metaclass object is itself (idempotent).
    %% This ensures `Metaclass class class == Metaclass class` holds via structural
    %% equality: both produce #beamtalk_object{class='Metaclass', pid=MetaclassPid}.
    #beamtalk_object{class = 'Metaclass', class_mod = ClassMod, pid = Pid};
class_of_object(#beamtalk_object{class = ClassName, pid = Pid}) ->
    %% ADR 0036: class of a class object → real metaclass object (wraps same pid).
    case beamtalk_class_registry:is_class_name(ClassName) of
        true ->
            #beamtalk_object{class = 'Metaclass', class_mod = beamtalk_metaclass_bt, pid = Pid};
        false ->
            class_of_object_inner(ClassName)
    end;
class_of_object(X) ->
    ClassName = class_of(X),
    class_of_object_inner(ClassName).

-doc "Helper to construct class object from class name.".
class_of_object_inner(ClassName) ->
    class_of_object_by_name(ClassName).

-doc """
Return a class object given a class name atom (BT-412).

BT-3052: resolves the module via the `beamtalk_class_metadata` ETS table
(populated unconditionally alongside the class's own identity at
init/reload, same precedent as BT-3047's `resolve_module_or_raise/2`)
rather than `beamtalk_object_class:module_name/1`'s `gen_server:call(Pid,
module_name)`. `class_of_object`/`class` is reachable from *any* value —
including one constructed via `self new` inside a block that ADR 0109
runs in a different class's process. If that different class was itself
synchronously invoked by `ClassName`'s own process (e.g. `ClassName`
called `otherClass someBlockTakingMethod: [...]` and the block does `self
new class`), a `gen_server:call` back to `ClassName`'s pid here deadlocks:
`ClassName`'s process is blocked waiting on the very process now trying to
call it back. This is a different shape from BT-893's guarded case
(`ClassPid =:= self()`, a *direct* self-call) — here the two pids differ,
so no existing check catches it. The metadata-table read sidesteps the
problem entirely: no message send, so no cycle to deadlock on. Falls back
to the gen_server call only for the practically-unreachable case where a
class object exists in the registry but hasn't (yet) written its metadata
row — accepting the old deadlock exposure there rather than raising, since
that miss should not happen for a class that can already be instantiated.
""".
-spec class_of_object_by_name(atom()) -> tuple() | atom().
class_of_object_by_name(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            ClassName;
        Pid when is_pid(Pid) ->
            ModuleName =
                case beamtalk_class_metadata:lookup_module(ClassName) of
                    {ok, Module} -> Module;
                    not_found -> beamtalk_object_class:module_name(Pid)
                end,
            ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
            {beamtalk_object, ClassTag, ModuleName, Pid}
    end.

-doc """
Return the printString representation of any value.

Strings are quoted (developer representation), symbols use the `#` prefix.
""".
-spec print_string(term()) -> binary().
print_string(X) when is_integer(X) -> erlang:integer_to_binary(X);
print_string(X) when is_float(X) -> erlang:float_to_binary(X, [short]);
print_string(X) when is_binary(X) ->
    case is_utf8(X) of
        true ->
            Escaped = binary:replace(X, <<"\"">>, <<"\"\"">>, [global]),
            iolist_to_binary([$", Escaped, $"]);
        false ->
            %% BT-2999: bytes that aren't valid UTF-8 can't be a String, and
            %% embedding them raw produces an invalid-UTF-8 result that later
            %% blows up anything expecting text (json:encode/1, logger, …).
            %% Render them the way `Binary printString` does instead.
            binary_hex_print_string(X)
    end;
print_string(true) ->
    <<"true">>;
print_string(false) ->
    <<"false">>;
print_string(nil) ->
    <<"nil">>;
print_string(X) when is_atom(X) ->
    iolist_to_binary([<<"#">>, erlang:atom_to_binary(X, utf8)]);
print_string(X) when is_list(X) ->
    iolist_to_binary([<<"#(">>, lists:join(<<", ">>, [print_string(E) || E <- X]), <<")">>]);
print_string({beamtalk_future, _} = Future) ->
    %% BT-840: Auto-await tagged futures before string conversion.
    print_string(beamtalk_future:await(Future));
print_string(#beamtalk_object{class = 'Metaclass', pid = Pid}) ->
    %% ADR 0036: Metaclass objects display as "ClassName class" (e.g. "Integer class").
    ClassName = beamtalk_object_class:class_name(Pid),
    iolist_to_binary([atom_to_binary(ClassName, utf8), <<" class">>]);
print_string(#beamtalk_object{class = ClassName} = Obj) ->
    case beamtalk_class_registry:is_class_name(ClassName) of
        true ->
            %% Class object — bare class name (ADR 0094).
            beamtalk_class_registry:class_display_name(ClassName);
        false ->
            %% ADR 0094: live actor instances render kind-headed and positional,
            %% e.g. `Actor(Counter, 0.123.0)`. Derived directly from the tuple.
            process_label(Obj)
    end;
print_string(X) when is_map(X) -> print_string_map(X);
print_string(#beamtalk_error{} = Error) ->
    iolist_to_binary(beamtalk_error:format(Error));
print_string({beamtalk_supervisor, _, _, _} = Sup) ->
    %% BT-3082: supervisors have no #beamtalk_object{} wrapper, so without this
    %% clause they fell into the generic is_tuple/1 clause below and printed as
    %% a raw Erlang tuple instead of the ADR 0094 kind-headed label — e.g. when
    %% a supervisor appears nested inside a collection being printed (printString
    %% dispatch on the supervisor itself is handled separately, in
    %% beamtalk_dispatch:invoke_method/6).
    process_label(Sup);
print_string(X) when is_function(X) ->
    %% BT-3082: without this clause, a Block nested inside a collection (whose
    %% elements are printed via direct recursion, not message dispatch — see
    %% print_string/1's is_list/1 and print_string_map/1's 'Array' clauses)
    %% fell into the ~p catch-all and rendered as a raw `#Fun<...>`, diverging
    %% from block_label/1's `Block/N` convention.
    block_label(X);
print_string(X) when is_tuple(X) ->
    Elements = tuple_to_list(X),
    iolist_to_binary([<<"{">>, lists:join(<<", ">>, [print_string(E) || E <- Elements]), <<"}">>]);
print_string(X) when is_pid(X) -> beamtalk_opaque_ops:pid_to_string(X);
print_string(X) when is_port(X) -> beamtalk_opaque_ops:port_to_string(X);
print_string(X) when is_reference(X) -> beamtalk_opaque_ops:ref_to_string(X);
print_string(X) ->
    iolist_to_binary(io_lib:format("~p", [X])).

-doc "Format tagged maps for display.".
-spec print_string_map(map()) -> binary().
print_string_map(X) ->
    case beamtalk_tagged_map:class_of(X) of
        'Set' ->
            ElemStrs = [print_string(E) || E <- maps:get(elements, X, [])],
            iolist_to_binary([<<"Set(">>, lists:join(<<", ">>, ElemStrs), <<")">>]);
        'Array' ->
            Elements = beamtalk_tagged_map:array_to_list(X),
            Parts = [print_string(E) || E <- Elements],
            iolist_to_binary(["#[", lists:join(<<", ">>, Parts), "]"]);
        'Stream' ->
            maps:get(description, X);
        'CompiledMethod' ->
            beamtalk_compiled_method_ops:dispatch('printString', [], X);
        'ErlangModule' ->
            beamtalk_erlang_proxy:dispatch('printString', [], X);
        undefined ->
            PlainMap = maps:remove('$beamtalk_class', X),
            Pairs = maps:fold(
                fun(K, V, Acc) ->
                    KeyStr = print_string(K),
                    ValStr = print_string(V),
                    [iolist_to_binary([KeyStr, <<" => ">>, ValStr]) | Acc]
                end,
                [],
                PlainMap
            ),
            SortedPairs = lists:sort(Pairs),
            iolist_to_binary([<<"#{">>, lists:join(<<", ">>, SortedPairs), <<"}">>]);
        Class ->
            case beamtalk_exception_handler:is_exception_class(Class) of
                true ->
                    beamtalk_exception_handler:dispatch('printString', [], X);
                false ->
                    %% Dispatch printString to the value class so user-defined
                    %% printString methods are honoured (e.g. Package>>printString).
                    send_map(X, 'printString', [])
            end
    end.

-doc """
Return the displayString representation of any value.

Strings are returned as-is (no surrounding quotes), symbols without
the `#` prefix. This is the user-facing representation suitable for
`Transcript show:` and string interpolation.
""".
-spec display_string(term()) -> binary().
display_string(X) when is_integer(X) -> erlang:integer_to_binary(X);
display_string(X) when is_float(X) -> erlang:float_to_binary(X, [short]);
display_string(X) when is_binary(X) ->
    case unicode:characters_to_binary(X) of
        Utf8 when is_binary(Utf8) -> Utf8;
        _ -> iolist_to_binary(io_lib:format("~p", [X]))
    end;
display_string(true) ->
    <<"true">>;
display_string(false) ->
    <<"false">>;
display_string(nil) ->
    <<"nil">>;
display_string(X) when is_atom(X) -> erlang:atom_to_binary(X, utf8);
display_string(X) when is_list(X) ->
    iolist_to_binary([<<"#(">>, lists:join(<<", ">>, [display_string(E) || E <- X]), <<")">>]);
display_string({beamtalk_future, _} = Future) ->
    display_string(beamtalk_future:await(Future));
display_string(#beamtalk_object{class = 'Metaclass', pid = Pid}) ->
    ClassName = beamtalk_object_class:class_name(Pid),
    iolist_to_binary([atom_to_binary(ClassName, utf8), <<" class">>]);
display_string(#beamtalk_object{class = ClassName} = Obj) ->
    case beamtalk_class_registry:is_class_name(ClassName) of
        true ->
            beamtalk_class_registry:class_display_name(ClassName);
        false ->
            %% ADR 0094: displayString for actors delegates to printString.
            process_label(Obj)
    end;
display_string(X) when is_map(X) ->
    beamtalk_tagged_map:format_for_display(X);
display_string({beamtalk_supervisor, _, _, _} = Sup) ->
    %% BT-3082: see the matching print_string/1 clause.
    process_label(Sup);
display_string(X) when is_function(X) ->
    %% BT-3082: see the matching print_string/1 clause.
    block_label(X);
display_string(X) when is_tuple(X) ->
    %% BT-3082: this clause was missing entirely (unlike print_string/1's),
    %% so a plain tuple fell into the ~p catch-all below instead of recursing
    %% with display_string/1 (no quotes on nested strings, matching the rest
    %% of this function's contract).
    Elements = tuple_to_list(X),
    iolist_to_binary([<<"{">>, lists:join(<<", ">>, [display_string(E) || E <- Elements]), <<"}">>]);
display_string(X) when is_pid(X) ->
    %% BT-3082: this clause was missing entirely (unlike print_string/1's),
    %% so a raw pid fell into the ~p catch-all below and rendered as the bare
    %% Erlang `<0.123.0>` instead of `#Pid<0.123.0>`.
    beamtalk_opaque_ops:pid_to_string(X);
display_string(X) when is_port(X) -> beamtalk_opaque_ops:port_to_string(X);
display_string(X) when is_reference(X) -> beamtalk_opaque_ops:ref_to_string(X);
display_string(X) ->
    iolist_to_binary(io_lib:format("~p", [X])).

-doc """
Render the kind-headed, positional label for a live process (ADR 0094).

Returns `Actor(ClassName, pid)` for actor instances and
`Supervisor(ClassName, pid)` / `DynamicSupervisor(ClassName, pid)` for
supervisors, with the kind head determined by ancestry. The label is derived
**directly from the tuple** — no message is sent to the process — so it is
safe to use as the timeout/dead-process fallback for a wedged actor.

The kind words `Actor`, `Supervisor`, and `DynamicSupervisor` are reserved and
must not be shadowed by user `Value` classes (validation tracked as follow-up).
""".
-spec process_label(#beamtalk_object{} | tuple()) -> binary().
process_label(#beamtalk_object{class = ClassName, pid = Identity}) ->
    iolist_to_binary([
        <<"Actor(">>, atom_to_binary(ClassName, utf8), <<", ">>, identity_inner(Identity), <<")">>
    ]);
process_label({beamtalk_supervisor, ClassName, _Module, Pid}) ->
    Head =
        try beamtalk_class_registry:inherits_from(ClassName, 'DynamicSupervisor') of
            true -> <<"DynamicSupervisor(">>;
            _ -> <<"Supervisor(">>
        catch
            _:_ -> <<"Supervisor(">>
        end,
    iolist_to_binary([
        Head, atom_to_binary(ClassName, utf8), <<", ">>, identity_inner(Pid), <<")">>
    ]).

-doc """
Format a process identity slot as the inner `X.Y.Z` form (no `#Pid<...>`).

Handles raw pids and ADR 0079 name-resolving proxies (`{registered, Name}`),
falling back defensively so the formatter never crashes on a malformed slot.
""".
-spec identity_inner(term()) -> binary().
identity_inner(Pid) when is_pid(Pid) ->
    List = erlang:pid_to_list(Pid),
    %% erlang:pid_to_list/1 returns "<X.Y.Z>"; strip the angle brackets.
    list_to_binary(lists:sublist(List, 2, length(List) - 2));
identity_inner({registered, Name}) when is_atom(Name) ->
    iolist_to_binary([<<"registered, ">>, atom_to_binary(Name, utf8)]);
identity_inner(Other) ->
    iolist_to_binary(io_lib:format("~tp", [Other])).

-doc """
`Block/N` label for a bare fun, `N` being its arity (BT-3082).

The single canonical algorithm shared by `print_string/1`/`display_string/1`
(for a Block nested inside a collection, printed via direct recursion
rather than message dispatch), the REPL wire encoder
(`beamtalk_repl_json:term_to_json/1`), and the stdlib-test result formatter
(`beamtalk_stdlib_test:format_result/1`) — previously four independent
copies of the same `erlang:fun_info/2` + format logic.
""".
-spec block_label(function()) -> binary().
block_label(Fun) when is_function(Fun) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    iolist_to_binary([<<"Block/">>, integer_to_binary(Arity)]).

-doc """
Liveness-probed label for a bare pid: `#Actor<X.Y.Z>` for a live process,
`#Dead<X.Y.Z>` for a dead/unreachable one, or the matching `#Future<...>`
tag when the pid is (or was) executing `beamtalk_future` code (BT-3082).

This is the single canonical algorithm shared by the REPL wire encoder
(`beamtalk_repl_json:term_to_json/1`, via the `beamtalk_runtime_api` facade)
and the stdlib-test result formatter (`beamtalk_stdlib_test:format_result/1`)
for rendering a raw, unwrapped pid — previously duplicated between the two
with drifted liveness handling: the test-runner copy unconditionally
rendered `#Actor<...>`, so a dead pid was reported as alive.

Deliberately distinct from `print_string/1`'s `#Pid<...>` rendering for
`Pid`-class values (ADR-documented in `stdlib/src/pid.bt`, liveness-agnostic
by design) — this is wire/test display only, layered on top of the same
underlying pid, not a replacement for it.
""".
-spec pid_label(pid()) -> binary().
pid_label(Pid) ->
    case is_pid_alive_safe(Pid) of
        true ->
            case process_info(Pid, current_function) of
                {current_function, {beamtalk_future, pending, _}} ->
                    <<"#Future<pending>">>;
                {current_function, {beamtalk_future, resolved, _}} ->
                    <<"#Future<resolved>">>;
                {current_function, {beamtalk_future, rejected, _}} ->
                    <<"#Future<rejected>">>;
                undefined ->
                    %% Race: died between the liveness check above and here.
                    dead_pid_label(Pid);
                _ ->
                    iolist_to_binary([<<"#Actor<">>, identity_inner(Pid), <<">">>])
            end;
        false ->
            dead_pid_label(Pid)
    end.

-doc "Render the `#Dead<X.Y.Z>` label shared by both pid_label/1 branches.".
-spec dead_pid_label(pid()) -> binary().
dead_pid_label(Pid) ->
    iolist_to_binary([<<"#Dead<">>, identity_inner(Pid), <<">">>]).

-doc "Liveness probe that treats a remote/unreachable pid as not alive (never raises).".
-spec is_pid_alive_safe(pid()) -> boolean().
is_pid_alive_safe(Pid) ->
    try
        is_process_alive(Pid)
    catch
        _:_ -> false
    end.

-doc "Send a message to any value (actor or primitive).".
-spec send(term(), atom(), list()) -> term().
send({beamtalk_future, _} = Future, Selector, Args) ->
    %% BT-840: Auto-await tagged futures before dispatching.
    send(beamtalk_future:await(Future), Selector, Args);
send(#beamtalk_object{class = 'Metaclass', pid = Pid} = Self, Selector, Args) ->
    %% ADR 0036: Route metaclass objects through the Metaclass dispatch chain.
    %% Must be matched before the generic #beamtalk_object{} clause below.
    beamtalk_class_dispatch:metaclass_send(Pid, Selector, Args, Self);
send(#beamtalk_object{pid = Pid}, Selector, Args) ->
    gen_server:call(Pid, {Selector, Args});
send(X, Selector, Args) when is_tuple(X) ->
    %% Handle tuples that might be beamtalk_objects not matching the record pattern
    case tuple_size(X) >= 2 andalso element(1, X) =:= beamtalk_object of
        true ->
            Pid = element(4, X),
            gen_server:call(Pid, {Selector, Args});
        false ->
            dispatch_via_module(X, Selector, Args)
    end;
send(X, Selector, Args) when is_map(X) ->
    send_map(X, Selector, Args);
send(X, Selector, Args) when is_pid(X) ->
    send_pid(X, Selector, Args);
send(X, Selector, Args) ->
    %% All other primitives: route through module_for_value/1
    dispatch_via_module(X, Selector, Args).

-doc """
Tagged map dispatch — routes through module_for_value/1 for all
tagged-map types, falling back to send_tagged_map_fallback/4 if unregistered.
""".
-spec send_map(map(), atom(), list()) -> term().
send_map(X, Selector, Args) ->
    case module_for_value(X) of
        undefined ->
            Class = beamtalk_tagged_map:class_of(X),
            send_tagged_map_fallback(X, Class, Selector, Args);
        Mod ->
            Mod:dispatch(Selector, Args, X)
    end.

-doc """
Dispatch messages to a pid value.

Routes Future-specific selectors (BT-813) directly to beamtalk_future.
All other selectors fall through to the Pid stdlib module.
""".
-spec send_pid(pid(), atom(), list()) -> term().
send_pid(X, await, _Args) ->
    beamtalk_future:await(X);
send_pid(X, awaitForever, _Args) ->
    beamtalk_future:await_forever(X);
send_pid(X, 'await:', [#{'$beamtalk_class' := 'Duration', millis := Ms}]) ->
    %% BT-2969: `Future await:` accepts a Duration as well as Integer ms.
    %% Matched structurally — beamtalk_runtime cannot call into
    %% beamtalk_stdlib (beamtalk_duration), dependencies flow down only.
    beamtalk_future:await(X, Ms);
send_pid(X, 'await:', [Timeout]) ->
    beamtalk_future:await(X, Timeout);
send_pid(X, 'whenResolved:', [Block]) ->
    beamtalk_future:when_resolved(X, Block);
send_pid(X, 'whenRejected:', [Block]) ->
    beamtalk_future:when_rejected(X, Block);
send_pid(X, Selector, Args) ->
    dispatch_via_module(X, Selector, Args).

-doc """
True if the selector belongs to the Future protocol.

Used by both send_pid/3 and responds_to/2 to recognise Future-specific
messages on bare pids.
""".
-spec is_future_selector(atom()) -> boolean().
is_future_selector(await) -> true;
is_future_selector(awaitForever) -> true;
is_future_selector('await:') -> true;
is_future_selector('whenResolved:') -> true;
is_future_selector('whenRejected:') -> true;
is_future_selector(_) -> false.

-doc "Fallback for tagged maps without a compiled stdlib module.".
-spec send_tagged_map_fallback(map(), atom(), atom(), list()) -> term().
send_tagged_map_fallback(X, Class, Selector, Args) ->
    case beamtalk_exception_handler:is_exception_class(Class) of
        true ->
            beamtalk_exception_handler:dispatch(Selector, Args, X);
        false ->
            value_type_send(X, Class, Selector, Args)
    end.

-doc """
Check if a value responds to a given selector.

ADR 0032 Phase 3: For actor instances (#beamtalk_object{}), delegate to
beamtalk_dispatch:responds_to/2 which walks the full class hierarchy.
The previous approach (Mod:has_method/1) only checked locally-defined methods,
causing inherited methods (e.g. 'class' from ProtoObject) to return false.
""".
-spec responds_to(term(), atom()) -> boolean().
responds_to({beamtalk_future, _} = Future, Selector) ->
    %% BT-840: Auto-await tagged futures before protocol checking.
    responds_to(beamtalk_future:await(Future), Selector);
responds_to(#beamtalk_object{class = Tag} = Obj, Selector) ->
    %% BT-776/BT-3200: Class objects (e.g., Counter as a value) are instances
    %% of 'Class', so fall back to the Class -> Behaviour -> Object hierarchy
    %% for generic protocol — but first check the class's OWN class-side
    %% methods/extensions (class_understands_class_selector/2), since those
    %% are not reachable via the generic 'Class' walk (the metaclass tag is
    %% virtual, per BT-776's own doc comment on class_object_tag/1).
    case beamtalk_class_registry:is_class_object(Obj) of
        true ->
            class_responds_to(class_name_from_tag(Tag), Selector);
        false ->
            ClassName = class_name_from_tag(Tag),
            beamtalk_dispatch:responds_to(Selector, ClassName)
    end;
responds_to(X, Selector) when is_tuple(X) ->
    %% Handle tuples that might be beamtalk_objects not matching the record pattern
    case tuple_size(X) >= 4 andalso element(1, X) =:= beamtalk_object of
        true ->
            %% BT-776/BT-3200: Class objects — see the record-pattern clause above.
            case beamtalk_class_registry:is_class_object(X) of
                true ->
                    Tag = element(2, X),
                    class_responds_to(class_name_from_tag(Tag), Selector);
                false ->
                    Tag = element(2, X),
                    ClassName = class_name_from_tag(Tag),
                    beamtalk_dispatch:responds_to(Selector, ClassName)
            end;
        false ->
            responds_via_module(X, Selector)
    end;
responds_to(X, Selector) when is_map(X) ->
    responds_to_map(X, Selector);
responds_to(X, Selector) when is_pid(X) ->
    %% BT-813: Future-specific selectors are handled by send_pid/3.
    is_future_selector(Selector) orelse responds_via_module(X, Selector);
responds_to(X, Selector) ->
    %% All other primitives: route through module_for_value/1
    responds_via_module(X, Selector).

-doc """
BT-3200: `respondsTo:` on a class-object receiver — checks `ClassName`'s own
class-side methods/extensions before falling back to the generic
`Class`/`Behaviour`/`Object` protocol.

`respondsTo:` is a sealed compiler intrinsic (`object.bt`), so every call
site compiles directly to `beamtalk_primitive:responds_to/2` — it never
reaches `beamtalk_dispatch:lookup/5` or a compiled class's own `dispatch/4`.
Before this, a class object's `respondsTo:` always answered against the
generic `'Class'` hierarchy only (BT-776), so `SomeClass respondsTo: #foo`
for a class method or class-side extension `SomeClass` itself defines
answered `false` even though `SomeClass foo` (or `self foo` from another of
its class methods) would actually work.
""".
-spec class_responds_to(atom(), atom()) -> boolean().
class_responds_to(ClassName, Selector) ->
    case beamtalk_class_dispatch:class_understands_class_selector(ClassName, Selector) of
        true -> true;
        false -> beamtalk_dispatch:responds_to(Selector, 'Class')
    end.

-doc """
Tagged map responds_to — routes through module_for_value/1 for all
tagged-map types, falling back to exception/value-type checks if unregistered.
""".
-spec responds_to_map(map(), atom()) -> boolean().
responds_to_map(X, Selector) ->
    case module_for_value(X) of
        undefined ->
            Class = beamtalk_tagged_map:class_of(X),
            case beamtalk_exception_handler:is_exception_class(Class) of
                true -> beamtalk_exception_handler:has_method(Selector);
                false -> value_type_responds_to(Class, Selector)
            end;
        Mod ->
            Mod:has_method(Selector)
    end.

-doc "Dispatch a value using its mapped stdlib module.".
-spec dispatch_via_module(term(), atom(), list()) -> term().
dispatch_via_module(X, Selector, Args) ->
    case module_for_value(X, Selector) of
        undefined ->
            beamtalk_error:raise(
                beamtalk_error:new(
                    does_not_understand,
                    class_of(X),
                    Selector,
                    <<"Primitive type does not support this message">>
                )
            );
        Mod ->
            Mod:dispatch(Selector, Args, X)
    end.

-doc "Check method support using the mapped stdlib module.".
-spec responds_via_module(term(), atom()) -> boolean().
responds_via_module(X, Selector) ->
    case module_for_value(X, Selector) of
        undefined -> false;
        Mod -> Mod:has_method(Selector)
    end.

-doc """
Map a runtime value to its stdlib dispatch module, selector-aware (BT-3033).

Overload of `module_for_value/1` for the dynamic-dispatch call sites that
already have the selector in hand (`dispatch_via_module/3`,
`responds_via_module/2`). For a bare binary whose selector is in
`is_string_binary_shared_selector/1` *and* has no `String` extension
registered (ADR 0066), skips the O(byte_size) `is_utf8/1` validating scan
entirely — String and Binary agree on that selector's behaviour regardless
of which one the value "really" is, so either module answers identically.
Falls back to `module_for_value/1` (and its UTF-8 scan) for every other
selector, and for a shared selector that a `String` extension has
overridden — an unconditional fast path would let a String-only extension
fire on a genuinely non-UTF-8 receiver that `class_of/1` still reports as
`Binary` (caught in review).

Routes straight to `'bt@stdlib@binary'` rather than `'bt@stdlib@string'`
(BT-3049): all 9 shared selectors are locally defined on `Binary`, and ADR
0066 extensions cannot override a class-body-defined method, so `Binary`
itself can never have a competing extension for one of them — only
`String`'s extension registry needs checking (done above), and going
straight to `'bt@stdlib@binary'` skips the redundant hop through
`'bt@stdlib@string':dispatch/3`, which would otherwise re-check the
extension registry itself before delegating to `'bt@stdlib@binary'` anyway.
""".
-spec module_for_value(term(), atom()) -> atom() | undefined.
module_for_value(X, Selector) when is_binary(X) ->
    case
        is_string_binary_shared_selector(Selector) andalso
            not beamtalk_extensions:has('String', Selector)
    of
        true -> 'bt@stdlib@binary';
        false -> module_for_value(X)
    end;
module_for_value(X, _Selector) ->
    module_for_value(X).

-doc """
True for selectors where `String` and `Binary` behave identically (BT-3033).

Derived from `binary.bt`'s own instance methods that `string.bt` does *not*
redefine — per ADR 0086's method override table, these are the byte-level
primitives String inherits unchanged (`byteAt:`, `byteSize`, `part:size:`,
`concat:`, `toBytes`, `asStringUnchecked`, `asBase64`, `asBase64Url`,
`asHex`). Every other Binary-defined selector (`size`, `at:`, `do:`,
`printString`, `asString`) is overridden by `String` with grapheme-aware
behaviour, and every String-only selector (`uppercase`, `split:`, …) simply
doesn't exist on `Binary` — both cases need the real `is_utf8/1` answer to
dispatch (or reject) correctly.

Kept in sync with the two source files by
`build_stdlib.rs`'s `test_binary_string_shared_selectors_stay_in_sync`,
which fails the build if either file's method list drifts from this set.

Soundness depends on a codegen invariant, not just name-matching: a selector
`string.bt` doesn't locally define is never duplicated into
`'bt@stdlib@string'`'s compiled `dispatch/3`/`has_method/1` — it's compiled
as a runtime delegation to `'bt@stdlib@binary'`'s implementation instead. So
routing one of these selectors through `'bt@stdlib@string'` would call the
exact same code as routing it through `'bt@stdlib@binary'` directly — which
is what `module_for_value/2` does (BT-3049), skipping that redundant
delegation hop — for any binary, valid UTF-8 or not. The sync test only
needs to track selector *names* because the two modules already share the
method *body*. If that delegation model ever changes (e.g. inherited
primitives get inlined/duplicated per subclass for a future perf win), this
list's safety would need re-deriving from semantics again, not just names.

This function alone is *not* sufficient to pick the fast path, only a
necessary precondition: since these selectors are locally defined on
`Binary`, ADR 0066 forbids a competing `Binary` extension for any of them,
but a `String` extension is legal (they're absent from `string.bt`) and
would make the selector no longer "identical either way" — it would run
String-only logic against a receiver `class_of/1` still reports as
`Binary`. Callers must additionally check `beamtalk_extensions:has/2` for a
`String` override (see `module_for_value/2`, the only caller — call this
function directly elsewhere only if you replicate that check too).
""".
-spec is_string_binary_shared_selector(atom()) -> boolean().
is_string_binary_shared_selector('byteAt:') -> true;
is_string_binary_shared_selector(byteSize) -> true;
is_string_binary_shared_selector('part:size:') -> true;
is_string_binary_shared_selector('concat:') -> true;
is_string_binary_shared_selector(toBytes) -> true;
is_string_binary_shared_selector(asStringUnchecked) -> true;
is_string_binary_shared_selector(asBase64) -> true;
is_string_binary_shared_selector(asBase64Url) -> true;
is_string_binary_shared_selector(asHex) -> true;
is_string_binary_shared_selector(_) -> false.

-doc "Map a runtime value to its stdlib dispatch module.".
-spec module_for_value(term()) -> atom() | undefined.
module_for_value(X) when is_integer(X) -> 'bt@stdlib@integer';
module_for_value(X) when is_binary(X) ->
    %% BT-2999: keep dynamic dispatch consistent with class_of/1 — a binary
    %% that isn't valid UTF-8 is a Binary, so grapheme-aware String methods
    %% (`size`, `at:`, `do:`, …) would only fail on it. BT-3033: callers that
    %% already know the selector should prefer module_for_value/2, which
    %% skips this scan for selectors where String and Binary agree.
    case is_utf8(X) of
        true -> 'bt@stdlib@string';
        false -> 'bt@stdlib@binary'
    end;
module_for_value(true) ->
    'bt@stdlib@true';
module_for_value(false) ->
    'bt@stdlib@false';
module_for_value(nil) ->
    'bt@stdlib@undefined_object';
module_for_value(X) when is_atom(X) -> 'bt@stdlib@symbol';
module_for_value(X) when is_function(X) -> 'bt@stdlib@block';
module_for_value(X) when is_tuple(X) -> 'bt@stdlib@tuple';
module_for_value(X) when is_float(X) -> 'bt@stdlib@float';
module_for_value(X) when is_list(X) -> 'bt@stdlib@list';
module_for_value(X) when is_pid(X) -> 'bt@stdlib@pid';
module_for_value(X) when is_port(X) -> 'bt@stdlib@port';
module_for_value(X) when is_reference(X) -> 'bt@stdlib@reference';
module_for_value(X) when is_map(X) ->
    %% BT-3081: only the two genuine exceptions are hardcoded — 'ErlangModule'
    %% dispatches to a hand-written native proxy module, not a compiled `.bt`
    %% class, and an untagged map (no '$beamtalk_class') is Dictionary by
    %% convention, not by class name. Every other tagged-map class name is
    %% derivable from the `ClassName ⇄ bt@stdlib@snake_case` convention
    %% (BT-2999 comment on the old hand-written ~35-entry table this replaced).
    case beamtalk_tagged_map:class_of(X) of
        'ErlangModule' ->
            beamtalk_erlang_proxy;
        undefined ->
            'bt@stdlib@dictionary';
        Other ->
            case beamtalk_exception_handler:is_exception_class(Other) of
                true -> 'bt@stdlib@exception';
                false -> stdlib_module_for_tagged_class(Other)
            end
    end;
module_for_value(_) ->
    undefined.

-doc """
Derive a tagged-map class's stdlib dispatch module from its class name
(BT-3081), verifying the module actually exists rather than trusting the
naming convention blindly — a class name that doesn't correspond to a
loaded `bt@stdlib@…` module (e.g. a typo, or a class removed from stdlib)
falls back to `undefined`, matching `module_for_value/1`'s prior behaviour
for anything outside its hand-written table.
""".
-spec stdlib_module_for_tagged_class(atom()) -> atom() | undefined.
stdlib_module_for_tagged_class(ClassName) ->
    Candidate = beamtalk_module_name:to_stdlib_module_atom(ClassName),
    case code:ensure_loaded(Candidate) of
        {module, _} -> Candidate;
        {error, _} -> undefined
    end.

-doc "Send a message to a value type instance (BT-354).".
-spec value_type_send(map(), atom(), atom(), list()) -> term().
value_type_send(Self, Class, Selector, Args) ->
    case is_ivar_method(Selector) of
        {true, Hint} ->
            beamtalk_error:raise(beamtalk_error:new(immutable_value, Class, Selector, Hint));
        false ->
            ok
    end,
    Module = class_name_to_module(Class),
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, dispatch, 3) of
        true ->
            Module:dispatch(Selector, Args, Self);
        false ->
            Arity = length(Args) + 1,
            case erlang:function_exported(Module, Selector, Arity) of
                true ->
                    erlang:apply(Module, Selector, [Self | Args]);
                false ->
                    %% BT-2275: module-less (builder-built) class — dispatch
                    %% fun-backed instance methods stored in the class gen_server.
                    %% These follow the value-type calling convention
                    %% `fun(Self, Arg1..ArgN) -> Result`, identical to the
                    %% compiled `Module:Selector(Self, Args)` form above, so
                    %% behaviour is unchanged once the class is flushed.
                    case runtime_instance_method(Class, Selector) of
                        {ok, Fun} ->
                            erlang:apply(Fun, [Self | Args]);
                        none ->
                            case beamtalk_object_ops:try_dispatch(Selector, Args, Self) of
                                {ok, Result} ->
                                    Result;
                                false ->
                                    beamtalk_error:raise(
                                        beamtalk_error:new(
                                            does_not_understand,
                                            Class,
                                            Selector,
                                            <<"Value type does not understand this message">>
                                        )
                                    )
                            end
                    end
            end
    end.

-doc """
Look up a fun-backed instance method on a module-less class (BT-2275).

Builder-built classes register instance methods as funs in the class
gen_server's `instance_methods` map rather than as compiled module functions.
Resolves the selector against the class — and its superclass chain, via
`beamtalk_object_class:method/2` — returning the stored block fun if present.
Returns `none` when the class is unregistered or the selector is not a runtime
fun (so callers fall through to the Object protocol / does_not_understand).

The common (external) path goes through `beamtalk_object_class:method/2`, which
issues a `gen_server:call`. That is unsafe when dispatch runs *inside* the class
gen_server itself — e.g. a fun-backed class method does `Inst := self new`
followed by `Inst someMethod`, all while the class process is mid-`handle_call`.
Calling `self()` there would deadlock.

BT-2277: rather than reporting `none` in that case (which made self-dispatch
silently diverge from external dispatch, a pre/post-flush hazard), the
`Pid =:= self()` branch resolves deadlock-free. It consults the local
instance-method cache kept in the process dictionary by
`beamtalk_object_class` (`beamtalk_class_instance_methods`), then walks the
superclass chain — superclasses are *other* processes, so the normal
`gen_server:call` resolution is safe for them.
""".
-spec runtime_instance_method(atom(), atom()) -> {ok, fun()} | none.
runtime_instance_method(Class, Selector) ->
    case beamtalk_class_registry:whereis_class(Class) of
        Pid when is_pid(Pid), Pid =/= self() ->
            method_fun_from_resolved(beamtalk_object_class:method(Pid, Selector));
        Pid when is_pid(Pid) ->
            %% Dispatch is running inside this class gen_server: resolve without a
            %% deadlocking call to self().
            runtime_instance_method_self(Selector);
        _ ->
            none
    end.

-doc """
Deadlock-free resolution of a fun-backed instance method from inside the class
gen_server (BT-2277).

Reads the local class's instance methods from the process-dictionary cache
seeded by `beamtalk_object_class:init/1` (and kept current by `put_method` and
hot reload). On a miss, walks the superclass chain — those are distinct
processes, so the ordinary `gen_server:call`-based resolution is safe.
""".
-spec runtime_instance_method_self(atom()) -> {ok, fun()} | none.
runtime_instance_method_self(Selector) ->
    LocalMethods =
        case get(beamtalk_class_instance_methods) of
            M when is_map(M) -> M;
            _ -> #{}
        end,
    case maps:find(Selector, LocalMethods) of
        {ok, #{block := Fun}} when is_function(Fun) ->
            {ok, Fun};
        _ ->
            runtime_instance_method_in_super(Selector)
    end.

-doc "Resolve a fun-backed instance method on the superclass chain (BT-2277).".
-spec runtime_instance_method_in_super(atom()) -> {ok, fun()} | none.
runtime_instance_method_in_super(Selector) ->
    case get(beamtalk_class_superclass) of
        Super when is_atom(Super), Super =/= none, Super =/= nil ->
            case beamtalk_class_registry:whereis_class(Super) of
                SuperPid when is_pid(SuperPid), SuperPid =/= self() ->
                    method_fun_from_resolved(
                        beamtalk_object_class:method(SuperPid, Selector)
                    );
                _ ->
                    none
            end;
        _ ->
            none
    end.

-doc "Extract a block fun from a resolved method map, or `none`.".
-spec method_fun_from_resolved(term()) -> {ok, fun()} | none.
method_fun_from_resolved(#{'__method_info__' := #{block := Fun}}) when is_function(Fun) ->
    {ok, Fun};
method_fun_from_resolved(_) ->
    none.

-doc """
Check if a selector is a mutation method on a value type (BT-359, BT-924).

`fieldAt:put:` is blocked — value types are immutable, and the `with*:` methods
return new instances rather than mutating in place.

`fieldAt:` is intentionally NOT blocked here: user-defined value objects store
their slots in the underlying map and support read-only reflection (BT-924).
""".
-spec is_ivar_method(atom()) -> {true, binary()} | false.
is_ivar_method('fieldAt:put:') ->
    {true, <<"Cannot modify slot on value type; use withSlot: to create a new instance">>};
is_ivar_method(_) ->
    false.

-doc "Check if a value type responds to a selector (BT-354).".
-spec value_type_responds_to(atom(), atom()) -> boolean().
value_type_responds_to(Class, Selector) ->
    Module = class_name_to_module(Class),
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, has_method, 1) of
        true ->
            Module:has_method(Selector);
        false ->
            Exports =
                case erlang:function_exported(Module, module_info, 1) of
                    true -> Module:module_info(exports);
                    false -> []
                end,
            lists:any(fun({Name, _Arity}) -> Name =:= Selector end, Exports) orelse
                %% BT-2275: module-less classes report their fun-backed instance
                %% methods (and inherited ones) as understood.
                runtime_instance_method(Class, Selector) =/= none orelse
                beamtalk_object_ops:has_method(Selector)
    end.

-doc """
Convert a CamelCase class name atom to a module name atom (ADR 0016).

First tries the static naming convention (bt@{snake_case}).
If that module is not loaded, falls back to the class registry to
resolve package-qualified module names (e.g. bt@{package}@{snake_case}).
BT-760: This fallback enables `beamtalk test` to dispatch on package classes.
""".
-spec class_name_to_module(atom()) -> atom().
class_name_to_module(Class) when is_atom(Class) ->
    StaticModule = static_class_module_name(Class),
    case code:is_loaded(StaticModule) of
        {file, _} ->
            StaticModule;
        false ->
            %% Module not yet loaded — try loading it
            case code:ensure_loaded(StaticModule) of
                {module, _} ->
                    StaticModule;
                {error, _} ->
                    %% BT-760: Fall back to class registry for package-qualified modules
                    case beamtalk_class_registry:whereis_class(Class) of
                        undefined -> StaticModule;
                        ClassPid -> beamtalk_object_class:module_name_safe(ClassPid)
                    end
            end
    end.

-doc """
Static module name from class name (bt@{snake_case}).

BT-3081: delegates the CamelCase→snake_case conversion to
`beamtalk_module_name:to_module_atom/1`, the single Erlang-side authority
for the `ClassName ⇄ bt@[pkg@]snake_case` convention (ADR 0016).
""".
-spec static_class_module_name(atom()) -> atom().
static_class_module_name(Class) ->
    beamtalk_module_name:to_module_atom(Class).

-doc """
Whether a binary holds valid UTF-8 text (BT-2999).

`String` and `Binary` share the single BEAM `binary()` representation, so this
is the only signal available at runtime: invalid UTF-8 definitively rules out
`String`, while valid UTF-8 leaves the two indistinguishable and is treated as
`String`.

Cost is a single O(byte_size) validating scan with no copy — `characters_to_binary/3`
hands back the *same* binary when it is already valid UTF-8, and bails at the
first bad byte when it is not. Measured on OTP 28: ~32 ns for a short string,
~415 ns for 1 KB, ~0.4 ns/byte thereafter.

It runs only on the **dynamic**-dispatch path; statically typed sends compile
straight to BIFs and never reach here. That matters for hot loops: repeatedly
sending to the *same* multi-megabyte binary through dynamic dispatch re-scans
it every send (~0.4 ms per send at 1 MB), so an N-iteration loop is O(N x size).
Annotate such a receiver `:: Binary` or `:: String` to compile the sends
statically and skip this entirely. Note the grapheme-aware String selectors
(`size`, `at:`, `do:`) were already O(size) per call before this check existed.

BT-3033: for the O(1) byte-level selectors that gain a per-send scan
(`byteSize`, `byteAt:`, `part:size:`, `concat:`, `toBytes`,
`asStringUnchecked`, `asBase64`, `asBase64Url`, `asHex`), `send/3` and
`responds_to/2` skip this scan entirely via
`module_for_value/2`/`is_string_binary_shared_selector/1` — those selectors
behave identically whether the receiver "really" is a String or a Binary, so
there is no need to tell them apart. Every other selector still pays the
scan here.
""".
-spec is_utf8(binary()) -> boolean().
is_utf8(Bin) when is_binary(Bin) ->
    is_binary(unicode:characters_to_binary(Bin, utf8, utf8)).

-doc """
Render a non-UTF-8 binary the way `Binary printString` does — `<<AB CD>>`.

Kept here rather than delegating to `beamtalk_binary:print_string/1` because
beamtalk_runtime must not depend on beamtalk_stdlib.
""".
-spec binary_hex_print_string(binary()) -> binary().
binary_hex_print_string(Bin) ->
    Hex = binary:encode_hex(Bin, uppercase),
    Spaced = lists:join(<<" ">>, [Pair || <<Pair:2/binary>> <= Hex]),
    iolist_to_binary([<<"<<">>, Spaced, <<">>">>]).

-doc """
Extract the class name atom from a class tag or class object tag.

Handles both plain instance tags (e.g. 'Counter') and class object tags
(e.g. 'Counter class') — in both cases returns the class name atom 'Counter'.
Used by responds_to/2 to delegate to beamtalk_dispatch:responds_to/2, and
(BT-3047 / ADR 0109 amendment) called directly from generated Core Erlang to
untag `element(2, ClassSelf)` at the inherited-self-dispatch and instantiation-
intrinsic sites in `dispatch_codegen.rs`/`mod.rs` — hence the export.
""".
-spec class_name_from_tag(atom()) -> atom() | undefined.
class_name_from_tag(Tag) ->
    Bin = beamtalk_class_registry:class_display_name(Tag),
    try
        binary_to_existing_atom(Bin, utf8)
    catch
        error:badarg -> undefined
    end.
