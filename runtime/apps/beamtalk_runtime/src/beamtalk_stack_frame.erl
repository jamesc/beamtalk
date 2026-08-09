%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_stack_frame).

%%% **DDD Context:** Object System Context

-moduledoc """
StackFrame wrapper for post-exception introspection (BT-107).

Converts Erlang stacktrace entries into first-class Beamtalk StackFrame
value objects (tagged maps). Erlang stacktrace entries have the form:
  {Module, Function, ArityOrArgs, Location}
where Location is [{file, File}, {line, Line}] or [].

StackFrame objects are value types (tagged maps) with fields:
```
#{
  '$beamtalk_class' => 'StackFrame',
  module => atom(),          % Erlang module name
  function => atom(),        % Erlang function name
  arity => non_neg_integer(),% Function arity
  file => binary() | nil,    % Source file path
  line => non_neg_integer() | nil, % Source line number
  class_name => atom() | nil % Beamtalk class name (if mappable)
}
```
""".

-export([
    wrap/1,
    dispatch/3,
    has_method/1,
    module_to_class/1
]).

-doc """
Convert an Erlang stacktrace (list of tuples) to a list of StackFrame objects.

BT-3081: resolves the module→class registry map once for the whole
stacktrace rather than once per frame. `beamtalk_class_registry:module_to_class_map/0`
is a plain ETS fold (`beamtalk_class_metadata:foldl_modules/2`, a
`read_concurrency: true` table — no process round trips); paying that scan
once per exception instead of once per frame still keeps a deep stacktrace
on a many-classes system from turning into O(frames × classes) ETS scans.

Earlier versions of this function (and `module_to_class_map/0`) built this
map via a live class-registry scan with a `gen_server:call` per registered
class (`beamtalk_class_registry:live_class_entries/0`). Since `wrap/1` runs
on every Beamtalk exception via `beamtalk_exception_handler.erl`, a single
momentarily-slow or call-cycle-blocked class process could stall or deadlock
unrelated exception handling system-wide — fixed by moving to the ETS-backed
fold, which needs no live process at all.
""".
-spec wrap(list()) -> list().
wrap(Stacktrace) when is_list(Stacktrace) ->
    ModuleToClass = beamtalk_class_registry:module_to_class_map(),
    [wrap_frame(Frame, ModuleToClass) || Frame <- Stacktrace];
wrap(_) ->
    [].

-doc "Convert a single Erlang stacktrace entry to a StackFrame tagged map.".
-spec wrap_frame(tuple(), #{atom() => atom()}) -> map().
wrap_frame({Module, Function, ArityOrArgs, Location}, ModuleToClass) ->
    Arity =
        case is_list(ArityOrArgs) of
            true -> length(ArityOrArgs);
            false -> ArityOrArgs
        end,
    File =
        case proplists:get_value(file, Location) of
            undefined -> nil;
            F -> list_to_binary(F)
        end,
    Line =
        case proplists:get_value(line, Location) of
            undefined -> nil;
            L -> L
        end,
    ClassName = resolve_class_name(Module, ModuleToClass),
    #{
        '$beamtalk_class' => 'StackFrame',
        module => Module,
        function => Function,
        arity => Arity,
        file => File,
        line => Line,
        class_name => ClassName
    };
wrap_frame(_Other, _ModuleToClass) ->
    #{
        '$beamtalk_class' => 'StackFrame',
        module => undefined,
        function => undefined,
        arity => 0,
        file => nil,
        line => nil,
        class_name => nil
    }.

-doc """
Resolve `Module` to a class name using a pre-built module→class map (from
`beamtalk_class_registry:module_to_class_map/0`), falling back to the string
heuristic — the batched counterpart to `module_to_class/1`'s single-module
registry lookup, used by `wrap/1` to avoid re-scanning the class registry
per frame (BT-3081).
""".
-spec resolve_class_name(atom(), #{atom() => atom()}) -> atom() | 'nil'.
resolve_class_name(Module, ModuleToClass) when is_atom(Module) ->
    case maps:find(Module, ModuleToClass) of
        {ok, ClassName} ->
            ClassName;
        error ->
            module_to_class_heuristic(Module)
    end;
resolve_class_name(_, _ModuleToClass) ->
    nil.

-doc """
Map Erlang module name to Beamtalk class name.

BT-3081: Resolves via the class metadata table first
(`beamtalk_class_registry:class_name_for_module/1` — no live process needed),
which is authoritative — it reads each class's actual registered name rather
than guessing one from its module name, so it can't mis-capitalize
acronym-cased classes the way the string heuristic below provably does
(`bt@stdlib@beamerror` → `'BEAMError'` via the metadata table, vs. the
heuristic's lossy `'Beamerror'`). Falls back to the heuristic only when the
module isn't a registered class at all — no class was ever registered for
it, or the metadata table doesn't exist yet (early boot or unit tests), or
it's an Erlang module that was never a Beamtalk class. Unlike an earlier
version of this lookup, resolution does not depend on the class's gen_server
process currently being alive — the metadata table is written at
registration time and persists independently of process liveness.

`class_name_for_module/1` rebuilds the whole module→class map (a full ETS
scan via `module_to_class_map/0`) on every call — it is not an O(1) lookup.
Resolving a single module here is fine, but a caller resolving many modules
should build the map once with `module_to_class_map/0` and look each one up
in the result, exactly as `wrap/1` below does for a stacktrace.

Compiled module naming conventions the heuristic fallback handles:
  - 'counter' → 'Counter' (user classes)
  - 'bt@stdlib@integer' → 'Integer' (stdlib classes)
  - 'bt@integer' → 'Integer' (stdlib alt format)
  - 'beamtalk_integer' → 'Integer' (runtime primitives)
  - Other modules → nil (not a Beamtalk class)
""".
-spec module_to_class(atom()) -> atom() | 'nil'.
module_to_class(Module) when is_atom(Module) ->
    case beamtalk_class_registry:class_name_for_module(Module) of
        {ok, ClassName} ->
            ClassName;
        not_found ->
            module_to_class_heuristic(Module)
    end;
module_to_class(_) ->
    nil.

-doc """
Lossy snake_case→CamelCase fallback for `module_to_class/1`, used only when
the module isn't found in the class metadata table — see that function's doc
for why this alone is not authoritative (BT-3081).
""".
-spec module_to_class_heuristic(atom()) -> atom() | 'nil'.
module_to_class_heuristic(Module) ->
    ModStr = atom_to_list(Module),
    case ModStr of
        "bt@" ++ Rest ->
            %% bt@stdlib@integer, bt@exdura@workflow_engine, bt@counter
            %% Take the segment after the last '@' for the class name.
            Parts = string:split(Rest, "@", all),
            beamtalk_module_name:snake_to_class(lists:last(Parts));
        "beamtalk_" ++ Rest ->
            %% Runtime primitive modules like beamtalk_integer, beamtalk_string
            beamtalk_module_name:snake_to_class(Rest);
        _ ->
            %% Could be a user class compiled as snake_case module that just
            %% isn't registered under this exact module (already ruled out
            %% by the registry lookup above) — only trust the guess if a
            %% class by that guessed name is registered at all.
            ClassName = beamtalk_module_name:snake_to_class(ModStr),
            case ClassName of
                nil ->
                    nil;
                _ ->
                    case beamtalk_class_registry:whereis_class(ClassName) of
                        undefined -> nil;
                        _ -> ClassName
                    end
            end
    end.

-doc "Dispatch a message to a StackFrame object.".
-spec dispatch(atom(), list(), map()) -> term().
dispatch('class', [], _Frame) ->
    'StackFrame';
dispatch('method', [], #{function := Function}) ->
    Function;
dispatch('receiverClass', [], #{class_name := ClassName}) ->
    case ClassName of
        nil -> nil;
        _ -> ClassName
    end;
dispatch('arguments', [], #{arity := Arity}) ->
    Arity;
dispatch('sourceLocation', [], #{file := File, line := Line}) ->
    case {File, Line} of
        {nil, _} ->
            nil;
        {_, nil} ->
            nil;
        _ ->
            LineBin = integer_to_binary(Line),
            <<File/binary, ":", LineBin/binary>>
    end;
dispatch('moduleName', [], #{module := Module}) ->
    case Module of
        undefined -> nil;
        _ -> atom_to_binary(Module, utf8)
    end;
dispatch('line', [], #{line := Line}) ->
    Line;
dispatch('file', [], #{file := File}) ->
    File;
dispatch('printString', [], Frame) ->
    format_frame(Frame).

-doc "Check if StackFrame responds to a selector.".
-spec has_method(atom()) -> boolean().
has_method('class') -> true;
has_method('method') -> true;
has_method('receiverClass') -> true;
has_method('arguments') -> true;
has_method('sourceLocation') -> true;
has_method('moduleName') -> true;
has_method('line') -> true;
has_method('file') -> true;
has_method('printString') -> true;
has_method(_) -> false.

-doc "Format a StackFrame as a human-readable string.".
-spec format_frame(map()) -> binary().
format_frame(#{
    class_name := ClassName,
    function := Function,
    arity := Arity,
    file := File,
    line := Line
}) ->
    ClassPart =
        case ClassName of
            nil -> <<"?">>;
            _ -> atom_to_binary(ClassName, utf8)
        end,
    FunPart =
        case Function of
            undefined -> <<"?">>;
            _ -> atom_to_binary(Function, utf8)
        end,
    ArityBin = integer_to_binary(Arity),
    LocationPart =
        case {File, Line} of
            {nil, _} ->
                <<>>;
            {_, nil} ->
                <<" (", File/binary, ")">>;
            _ ->
                LineBin = integer_to_binary(Line),
                <<" (", File/binary, ":", LineBin/binary, ")">>
        end,
    <<ClassPart/binary, ">>", FunPart/binary, "/", ArityBin/binary, LocationPart/binary>>.
