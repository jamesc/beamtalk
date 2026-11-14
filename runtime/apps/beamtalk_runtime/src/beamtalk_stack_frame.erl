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
  class_name => atom() | nil, % Beamtalk class name (if the runtime class table knows the module)
  package => binary() | nil   % package qualifier, set only when two loaded class modules share the name
}
```

## The shared renderer (ADR 0139, "hide what the compiler made up")

This module is the one place a frame or a bare MFA is turned into text. Every
human-facing surface (`Exception>>stackTrace`, test failure output, JSON logs,
the workspace log stream, process stats) calls these; none keeps its own.

| Entry point | Answers |
|---|---|
| `format_frame/1` | one frame: `Counter>>increment (counter.bt:12)` |
| `render_mfa/1` | a bare `{M, F, A}`, same notation, no location |
| `raw_mfa/1` | a bare `{M, F, A}` as `module:function/arity`, always raw (machine-readable log fields) |
| `frame_parts/1` | the structured pieces `format_frame/1` is built from |

Frame table (the first column is what the frame is, not what it is called):

| Frame | Rendered as |
|---|---|
| Instance method | `Counter>>increment (counter.bt:12)` |
| Class-side method | `Counter class>>new (counter.bt:3)` (no `class_` prefix) |
| Block | `[] in Counter>>increment (counter.bt:14)` |
| Erlang code (any module that is not a Beamtalk class) | `lists:foldl/3 (lists.erl:2471)` |
| A `bt@...` module the class table cannot resolve | `bt@user@Hpack>>class_encode:headers:table:`: the raw module, nothing guessed |

Names resolve through the runtime class table
(`beamtalk_class_registry:module_to_class_map/0`); there is no string-guessing
fallback. A class name is package-qualified (`json@Parser`) only when two
loaded class modules share it. Which frames are listed is not decided here.
""".

-export([
    wrap/1,
    dispatch/3,
    has_method/1,
    module_to_class/1,
    wrap_frame/1,
    format_frame/1,
    render_mfa/1,
    raw_mfa/1,
    frame_parts/1,
    frame_source_path/1,
    lookup_method_line/2
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
    Shared = shared_class_segments(Stacktrace, ModuleToClass),
    [wrap_frame(Frame, ModuleToClass, Shared) || Frame <- Stacktrace];
wrap(_) ->
    [].

-doc """
Convert a single raw Erlang stacktrace entry to a StackFrame tagged map,
resolving its class with a single registry lookup. Used by paths that render
one frame at a time (test failure output) so they share `format_frame/1`.
""".
-spec wrap_frame(term()) -> map().
wrap_frame(Frame) ->
    ModuleToClass = beamtalk_class_registry:module_to_class_map(),
    wrap_frame(Frame, ModuleToClass, shared_class_segments([Frame], ModuleToClass)).

-spec wrap_frame(term(), #{atom() => atom()}, #{binary() => true}) -> map().
wrap_frame({Module, Function, ArityOrArgs, Location}, ModuleToClass, Shared) ->
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
        class_name => ClassName,
        package => package_qualifier(Module, ClassName, Shared)
    };
wrap_frame(_Other, _ModuleToClass, _Shared) ->
    #{
        '$beamtalk_class' => 'StackFrame',
        module => undefined,
        function => undefined,
        arity => 0,
        file => nil,
        line => nil,
        class_name => nil,
        package => nil
    }.

-doc """
Resolve `Module` to a class name using a pre-built module→class map (from
`beamtalk_class_registry:module_to_class_map/0`) — the batched counterpart to
`module_to_class/1`'s single-module registry lookup, used by `wrap/1` to avoid
re-scanning the class registry per frame. A miss is `nil`; nothing is guessed.
""".
-spec resolve_class_name(atom(), #{atom() => atom()}) -> atom() | 'nil'.
resolve_class_name(Module, ModuleToClass) when is_atom(Module) ->
    maps:get(Module, ModuleToClass, nil);
resolve_class_name(_, _ModuleToClass) ->
    nil.

-doc """
The class-name segments (the last `@`-separated segment of a `bt@...` module)
carried by more than one loaded class module, as `#{Segment => true}`. Empty
unless some frame resolved to a class in a package, so a trace of plain
Erlang or unpackaged frames never scans the loaded modules.
""".
-spec shared_class_segments(list(), #{atom() => atom()}) -> #{binary() => true}.
shared_class_segments(Frames, ModuleToClass) ->
    Packaged = lists:any(
        fun
            ({Module, _, _, _}) when is_atom(Module) ->
                maps:is_key(Module, ModuleToClass) andalso
                    beamtalk_module_name:package_of(Module) =/= undefined;
            (_) ->
                false
        end,
        Frames
    ),
    case Packaged of
        false ->
            #{};
        true ->
            Counts = lists:foldl(
                fun(Loaded, Acc) ->
                    case class_segment(Loaded) of
                        undefined ->
                            Acc;
                        Seg ->
                            %% `erlang:loaded/0` also lists modules with only old
                            %% code (a removed class awaiting a purge); they are
                            %% not loaded class modules.
                            case erlang:module_loaded(Loaded) of
                                true -> Acc#{Seg => maps:get(Seg, Acc, 0) + 1};
                                false -> Acc
                            end
                    end
                end,
                #{},
                %% The BIF, not code:all_loaded/0: no code_server round trip while
                %% an exception is being read or a log line formatted.
                erlang:loaded()
            ),
            maps:filter(fun(_, N) -> N > 1 end, Counts)
    end.

%% Everything between `bt@` and the class segment (`bt@json@a@parser` is
%% `json@a`), so nested packages with the same class name stay distinguishable.
-spec package_path(atom()) -> binary().
package_path(Module) ->
    [<<"bt">> | Rest] = binary:split(atom_to_binary(Module, utf8), <<"@">>, [global]),
    iolist_to_binary(lists:join(<<"@">>, lists:droplast(Rest))).

-spec class_segment(atom()) -> binary() | undefined.
class_segment(Module) ->
    %% The prefix test first: most loaded modules are OTP and runtime ones,
    %% and this runs over all of them.
    case atom_to_binary(Module, utf8) of
        <<"bt@", _/binary>> = Name ->
            case binary:split(Name, <<"@">>, [global]) of
                [_, _ | _] = Parts -> lists:last(Parts);
                _ -> undefined
            end;
        _ ->
            undefined
    end.

%% The package qualifier for a resolved class, only when another loaded class
%% module shares its name (ADR 0139 decision 2).
-spec package_qualifier(term(), atom() | nil, #{binary() => true}) -> binary() | nil.
package_qualifier(Module, ClassName, Shared) when
    is_atom(Module), ClassName =/= nil, map_size(Shared) > 0
->
    case {beamtalk_module_name:package_of(Module), class_segment(Module)} of
        {Pkg, Seg} when Pkg =/= undefined, Seg =/= undefined ->
            case maps:is_key(Seg, Shared) of
                true -> package_path(Module);
                false -> nil
            end;
        _ ->
            nil
    end;
package_qualifier(_Module, _ClassName, _Shared) ->
    nil.

-doc """
Map an Erlang module name to its Beamtalk class name through the runtime class
table (`beamtalk_class_registry:class_name_for_module/1`, ETS only, no live
process needed). Authoritative: it reads each class's registered name, so
acronym-cased classes (`bt@stdlib@beamerror` is `'BEAMError'`) resolve
correctly.

A module the table does not know answers `nil`: an Erlang module that was never
a Beamtalk class, a `bt@...` module whose class is not loaded, or a table that
does not exist yet (early boot). The renderer never guesses a class name from
the module name (ADR 0139 decision 2).

`class_name_for_module/1` scans the whole table on every call. A caller
resolving many modules should build the map once with
`beamtalk_class_registry:module_to_class_map/0`, as `wrap/1` does.
""".
-spec module_to_class(atom()) -> atom() | 'nil'.
module_to_class(Module) when is_atom(Module) ->
    case beamtalk_class_registry:class_name_for_module(Module) of
        {ok, ClassName} -> ClassName;
        not_found -> nil
    end;
module_to_class(_) ->
    nil.

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
for every trace display: `Exception>>stackTrace` (`printString`), test
failure output (`beamtalk_test_case:format_stacktrace/1`) and JSON log
stacktraces (`beamtalk_json_formatter`).

See the module doc for the frame table. In short: `Class>>selector`,
`Class class>>selector`, `[] in Class>>selector` for a block, and
`module:function/arity` for Erlang code. The location is the frame's file,
else the module's `beamtalk_source` attribute. Stdlib modules (`bt@stdlib@*`)
show the basename only; other absolute paths are made relative to CWD
(basename if outside it). The line is omitted when unknown or 0; no file means
no location part.
""".
-spec format_frame(map()) -> binary().
format_frame(Frame) ->
    #{file := File, line := Line} = Parts = frame_parts(Frame),
    LocationPart =
        case File of
            nil ->
                <<>>;
            _ ->
                case Line of
                    nil -> <<" (", File/binary, ")">>;
                    _ -> <<" (", File/binary, ":", (integer_to_binary(Line))/binary, ")">>
                end
        end,
    <<(head(Parts))/binary, LocationPart/binary>>.

-doc """
Render a bare `{Module, Function, Arity}` for a human, in the same notation as
`format_frame/1` and resolved through the same class table: a Beamtalk class
method is `Class>>selector` (`Class class>>selector` class-side), any other
Erlang function is `module:function/arity`. No location. Used where only an
MFA is known (a process's current function, an error message naming a call).
Machine-readable fields use `raw_mfa/1` instead.
""".
-spec render_mfa({atom(), atom(), arity()}) -> binary().
render_mfa({Module, Function, Arity} = _Mfa) when
    is_atom(Module), is_atom(Function), is_integer(Arity)
->
    head(frame_parts(wrap_frame({Module, Function, Arity, []}))).

-doc """
A bare `{Module, Function, Arity}` as the raw `module:function/arity` binary.
The one machine-readable MFA formatter (log `mfa` fields, process stats): it
never resolves a class or hides a compiler-made name.
""".
-spec raw_mfa({atom(), atom(), arity()}) -> binary().
raw_mfa({Module, Function, Arity}) when is_atom(Module), is_atom(Function), is_integer(Arity) ->
    beamtalk_error:chars_to_binary(io_lib:format("~ts:~ts/~B", [Module, Function, Arity])).

%% The location-free text of a frame, built from `frame_parts/1`.
-spec head(map()) -> binary().
head(#{kind := erlang, class := Module, selector := Function, arity := Arity}) ->
    <<Module/binary, ":", Function/binary, "/", (integer_to_binary(Arity))/binary>>;
head(#{class := Class, selector := Selector, block := IsBlock, class_side := ClassSide}) ->
    Owner =
        case ClassSide of
            true -> <<Class/binary, " class">>;
            false -> Class
        end,
    Prefix =
        case IsBlock of
            true -> <<"[] in ">>;
            false -> <<>>
        end,
    <<Prefix/binary, Owner/binary, ">>", Selector/binary>>.

-doc """
The parts of a frame, as displayed by `format_frame/1` (which is built on
this, so the text and structured forms cannot drift; BT-4355):
`kind` (`beamtalk` for a resolved class, `unresolved` for a `bt@...` module
the class table cannot resolve, `erlang` for any other module, `unknown` when
the frame has no module), `class` (the class, package-qualified only when two
loaded class modules share the name; else the raw module name; `?` when unknown),
`selector` (the user-visible selector, without the `class_` prefix and the
block marker), `class_side`, `block` (true for compiler-lifted funs), `file`
(the display path, `nil` when unknown) and `line` (`nil` when unknown or 0).
An `erlang` frame also carries `arity`, and its `class` is the module and
`selector` the function, exactly as recorded.
""".
-spec frame_parts(map()) ->
    #{
        kind := beamtalk | unresolved | erlang | unknown,
        class := binary(),
        selector := binary(),
        class_side := boolean(),
        block := boolean(),
        file := binary() | nil,
        line := pos_integer() | nil,
        arity => non_neg_integer()
    }.
frame_parts(#{module := Module, function := Function} = Frame) ->
    ClassName = maps:get(class_name, Frame, nil),
    Kind = frame_kind(Module, ClassName),
    Line =
        case maps:get(line, Frame, nil) of
            L when is_integer(L), L > 0 -> L;
            _ -> nil
        end,
    File =
        case frame_file(Module, maps:get(file, Frame, nil)) of
            nil -> nil;
            Path -> unicode:characters_to_binary(format_file_path(Module, Path))
        end,
    Base = #{kind => Kind, file => File, line => Line},
    case Kind of
        erlang ->
            Base#{
                class => atom_to_binary(Module, utf8),
                selector => atom_to_binary(Function, utf8),
                class_side => false,
                block => false,
                arity => maps:get(arity, Frame, 0)
            };
        _ ->
            {Name, IsBlock} =
                case Function of
                    undefined -> {<<"?">>, false};
                    _ -> fun_name_parts(Function)
                end,
            {Selector, ClassSide} =
                case Kind of
                    beamtalk -> strip_class_side(Name);
                    _ -> {Name, false}
                end,
            Base#{
                class => class_part(Kind, Module, ClassName, maps:get(package, Frame, nil)),
                selector => actor_selector(Kind, Module, Selector, Line, maps:get(file, Frame, nil)),
                class_side => ClassSide,
                block => IsBlock
            }
    end.

%% Actor methods are clauses of one `dispatch/4`, so the MFA of a frame inside
%% one reads `dispatch`. The compiler emits `__beamtalk_method_lines/0`, a list
%% of `{File, Rows}` where `Rows` is a tuple of `{StartLine, Selector}` sorted by
%% line, one entry per source file the module's methods come from (a
%% `uses:`-flattened method's lines are in its protocol's file). The method is
%% the row of the frame's own file with the greatest StartLine =< the frame's
%% line. No line, no file, a file the table does not list, no table (older
%% `.beam`) or a line before the first method leaves `dispatch` as it was.
-spec actor_selector(
    beamtalk | unresolved | unknown, atom(), binary(), pos_integer() | nil, binary() | nil
) -> binary().
actor_selector(beamtalk, Module, <<"dispatch">>, Line, File) when
    is_integer(Line), is_binary(File)
->
    case lists:keyfind(File, 1, method_lines(Module)) of
        {File, Rows} ->
            case lookup_method_line(Rows, Line) of
                {ok, Selector} -> atom_to_binary(Selector, utf8);
                not_found -> <<"dispatch">>
            end;
        false ->
            <<"dispatch">>
    end;
actor_selector(_Kind, _Module, Selector, _Line, _File) ->
    Selector.

-spec method_lines(atom()) -> [{binary(), tuple()}].
method_lines(Module) ->
    case erlang:function_exported(Module, '__beamtalk_method_lines', 0) of
        true -> Module:'__beamtalk_method_lines'();
        false -> []
    end.

-doc """
Find the method containing `Line` in a `__beamtalk_method_lines/0` table: the
entry with the greatest `StartLine =< Line`, by binary search over the tuple
(`element/2` is O(1) and copies nothing).
""".
-spec lookup_method_line(tuple(), integer()) -> {ok, atom()} | not_found.
lookup_method_line(Table, Line) ->
    search_method_lines(Table, Line, 1, tuple_size(Table), not_found).

-spec search_method_lines(tuple(), integer(), pos_integer(), non_neg_integer(), term()) ->
    {ok, atom()} | not_found.
search_method_lines(_Table, _Line, Lo, Hi, Best) when Lo > Hi ->
    Best;
search_method_lines(Table, Line, Lo, Hi, Best) ->
    Mid = (Lo + Hi) div 2,
    {Start, Selector} = element(Mid, Table),
    case Start =< Line of
        true -> search_method_lines(Table, Line, Mid + 1, Hi, {ok, Selector});
        false -> search_method_lines(Table, Line, Lo, Mid - 1, Best)
    end.

-spec frame_kind(term(), atom() | nil) -> beamtalk | unresolved | erlang | unknown.
frame_kind(Module, _) when Module =:= undefined; not is_atom(Module) -> unknown;
frame_kind(_Module, ClassName) when ClassName =/= nil -> beamtalk;
frame_kind(Module, nil) ->
    case beamtalk_module_name:is_compiled_beamtalk_module(Module) of
        true -> unresolved;
        false -> erlang
    end.

-spec class_part(beamtalk | unresolved | unknown, atom(), atom() | nil, binary() | nil) -> binary().
class_part(beamtalk, _Module, ClassName, nil) ->
    atom_to_binary(ClassName, utf8);
class_part(beamtalk, _Module, ClassName, Package) ->
    <<Package/binary, "@", (atom_to_binary(ClassName, utf8))/binary>>;
class_part(unresolved, Module, _ClassName, _Package) ->
    atom_to_binary(Module, utf8);
class_part(unknown, _Module, _ClassName, _Package) ->
    <<"?">>.

%% A class-side method is compiled as `class_<selector>`; the prefix is the
%% compiler's, so it is dropped once the owning class is known.
-spec strip_class_side(binary()) -> {binary(), boolean()}.
strip_class_side(<<"class_", Selector/binary>>) when Selector =/= <<>> -> {Selector, true};
strip_class_side(Name) -> {Name, false}.

-doc """
Source file for a frame as an unabbreviated path (not the display form that
`frame_parts/1` shortens), so the file can be opened to show a snippet
(BT-4358): the frame's own `file`, else the module's `beamtalk_source`
attribute; `nil` when neither is known.
""".
-spec frame_source_path(map()) -> binary() | nil.
frame_source_path(#{module := Module} = Frame) ->
    case frame_file(Module, maps:get(file, Frame, nil)) of
        nil -> nil;
        Path -> unicode:characters_to_binary(Path)
    end;
frame_source_path(_) ->
    nil.

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

-spec fun_name_parts(atom()) -> {binary(), boolean()}.
fun_name_parts(Fun) ->
    FunStr = atom_to_list(Fun),
    %% Lazy match keeps selectors that themselves contain '/' (`/`, `//`) intact.
    case re:run(FunStr, "^-(.+?)/[0-9]+-", [{capture, all_but_first, list}, unicode]) of
        {match, [BaseName]} -> {unicode:characters_to_binary(BaseName), true};
        nomatch -> {unicode:characters_to_binary(FunStr), false}
    end.
