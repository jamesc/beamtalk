%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_stack_frame).

%%% **DDD Context:** Object System Context

-moduledoc """
StackFrame wrapper for post-exception introspection.

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
    module_to_class/1,
    wrap_frame/1,
    format_frame/1
]).

-doc """
Convert an Erlang stacktrace (list of tuples) to a list of StackFrame objects.

Resolves the module→class registry map once for the whole
stacktrace rather than once per frame. `beamtalk_class_registry:module_to_class_map/0`
is a plain ETS fold (`beamtalk_class_metadata:foldl_modules/2`, a
`read_concurrency: true` table — no process round trips); paying that scan
once per exception instead of once per frame still keeps a deep stacktrace
on a many-classes system from turning into O(frames × classes) ETS scans.

Earlier versions of this function (and `module_to_class_map/0`) built this
map via a live class-registry scan with a `gen_server:call` per registered
class (`beamtalk_class_registry:live_class_entries/0`). Since `wrap/1` then ran
on every Beamtalk exception via `beamtalk_exception_handler.erl`, a single
momentarily-slow or call-cycle-blocked class process could stall or deadlock
unrelated exception handling system-wide — fixed by moving to the ETS-backed
fold, which needs no live process at all.

Since BT-4115 a catch no longer calls this: `beamtalk_exception_handler`
stores the raw stacktrace (arguments reduced to arities) and calls `wrap/1`
only when something reads the frames (`beamtalk_exception_handler:frames/1`,
behind `e stackTrace`). So class names are resolved at read time.
""".
-spec wrap(list()) -> list().
wrap(Stacktrace) when is_list(Stacktrace) ->
    ModuleToClass = beamtalk_class_registry:module_to_class_map(),
    [wrap_frame(Frame, ModuleToClass) || Frame <- Stacktrace];
wrap(_) ->
    [].

-doc """
Convert a single raw Erlang stacktrace entry to a StackFrame tagged map,
resolving its class with a single registry lookup. Used by paths that render
one frame at a time (test failure output) so they share `format_frame/1`.
""".
-spec wrap_frame(term()) -> map().
wrap_frame({Module, _, _, _} = Frame) when is_atom(Module) ->
    wrap_frame(Frame, #{Module => module_to_class(Module)});
wrap_frame(Other) ->
    wrap_frame(Other, #{}).

-doc "Convert a single Erlang stacktrace entry to a StackFrame tagged map.".
-spec wrap_frame(term(), #{atom() => atom()}) -> map().
wrap_frame({Module, Function, ArityOrArgs, Location}, ModuleToClass) ->
    Arity =
        case is_list(ArityOrArgs) of
            true -> length(ArityOrArgs);
            false -> ArityOrArgs
        end,
    File =
        case proplists:get_value(file, Location) of
            undefined ->
                nil;
            F ->
                %% Shared byte-aware conversion; nil keeps `String | Nil` (BT-4370).
                beamtalk_reflection:path_to_utf8(F)
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
per frame.
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

Resolves via the class metadata table first
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
for why this alone is not authoritative.
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
            %% BT-4170: text answers are tagged `String` values.
            beamtalk_primitive:string_from_binary(<<File/binary, ":", LineBin/binary>>)
    end;
dispatch('moduleName', [], #{module := Module}) ->
    case Module of
        undefined -> nil;
        _ -> beamtalk_primitive:string_from_binary(atom_to_binary(Module, utf8))
    end;
dispatch('line', [], #{line := Line}) ->
    Line;
dispatch('file', [], #{file := File}) when is_binary(File) ->
    beamtalk_primitive:string_from_binary(File);
dispatch('file', [], #{file := File}) ->
    File;
dispatch('printString', [], Frame) ->
    beamtalk_primitive:string_from_binary(format_frame(Frame)).

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

-doc """
Format a StackFrame as a human-readable string. This is the single renderer
for every trace display: `Exception>>stackTrace` (`printString`) and test
failure output (`beamtalk_test_case:format_stacktrace/1`).

Shape: `Class>>selector [block] (file:line)`.
  - Class: the Beamtalk class, else the module name, else `?`.
  - Selector: compiler-lifted funs (`'-sel/N-fun-M-'`, `-anonymous-`, loops
    `'-sel/N-name/K-M-'`) render as `sel [block]`. Arity and fun index are
    never shown; raw Erlang `module:function/arity` is never emitted.
  - Location: the frame's file, else the module's `beamtalk_source`
    attribute. Stdlib modules (`bt@stdlib@*`) show the basename only; other
    absolute paths are made relative to CWD (basename if outside it). The
    line is omitted when unknown or 0; no file means no location part.
""".
-spec format_frame(map()) -> binary().
format_frame(#{module := Module, function := Function} = Frame) ->
    ClassPart =
        case maps:get(class_name, Frame, nil) of
            nil when is_atom(Module), Module =/= undefined -> atom_to_binary(Module, utf8);
            nil -> <<"?">>;
            ClassName -> atom_to_binary(ClassName, utf8)
        end,
    FunPart =
        case Function of
            undefined -> <<"?">>;
            _ -> unicode:characters_to_binary(format_fun_name(Function))
        end,
    Line =
        case maps:get(line, Frame, nil) of
            L when is_integer(L), L > 0 -> L;
            _ -> nil
        end,
    LocationPart =
        case frame_file(Module, maps:get(file, Frame, nil)) of
            nil ->
                <<>>;
            File ->
                Shown = unicode:characters_to_binary(format_file_path(Module, File)),
                case Line of
                    nil -> <<" (", Shown/binary, ")">>;
                    _ -> <<" (", Shown/binary, ":", (integer_to_binary(Line))/binary, ")">>
                end
        end,
    <<ClassPart/binary, ">>", FunPart/binary, LocationPart/binary>>.

-doc """
Source file for a frame: its own `file` when present, otherwise the module's
`beamtalk_source` attribute (`nil` when neither is known).
""".
-spec frame_file(atom(), binary() | nil) -> string() | nil.
frame_file(_Module, File) when is_binary(File), File =/= <<>> ->
    unicode:characters_to_list(File);
frame_file(Module, _) when is_atom(Module), Module =/= undefined ->
    case beamtalk_reflection:source_file_from_module(Module) of
        nil -> nil;
        Path -> unicode:characters_to_list(Path)
    end;
frame_file(_, _) ->
    nil.

-spec format_file_path(atom(), string()) -> string().
format_file_path(Module, File) ->
    case lists:prefix("bt@stdlib@", atom_to_list(Module)) of
        true -> filename:basename(File);
        false -> make_relative(File)
    end.

-spec make_relative(string()) -> string().
make_relative(File) ->
    case filename:pathtype(File) of
        relative ->
            File;
        _ ->
            case file:get_cwd() of
                {ok, Cwd} ->
                    CwdComps = filename:split(Cwd),
                    FileComps = filename:split(File),
                    case lists:prefix(CwdComps, FileComps) of
                        true -> filename:join(lists:nthtail(length(CwdComps), FileComps));
                        false -> filename:basename(File)
                    end;
                _ ->
                    filename:basename(File)
            end
    end.

-spec format_fun_name(atom()) -> string().
format_fun_name(Fun) ->
    FunStr = atom_to_list(Fun),
    %% Lazy match keeps selectors that themselves contain '/' (`/`, `//`) intact.
    case re:run(FunStr, "^-(.+?)/[0-9]+-", [{capture, all_but_first, list}, unicode]) of
        {match, [BaseName]} -> BaseName ++ " [block]";
        nomatch -> FunStr
    end.
