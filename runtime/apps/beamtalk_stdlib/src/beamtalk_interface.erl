%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_interface).

%%% **DDD Context:** Object System Context

-moduledoc """
Method implementations for the BeamtalkInterface sealed Object.

Implements methods for the BeamtalkInterface class. BeamtalkInterface is
a `sealed Object subclass:` (value type, no gen_server process). Methods
are called via Erlang FFI from the compiled Beamtalk module using the
ErlangModule proxy pattern: `(Erlang beamtalk_interface) fn: arg`.

The `dispatch/3` function is used by EUnit tests and runtime bootstrap.

All methods are stateless reads from the class registry; no process
dictionary or ETS state is required.

## Methods

| Selector          | Description                                       |
|-------------------|---------------------------------------------------|
| `allClasses'      | List of all registered classes (class objects)     |
| `classNamed:'     | Class object reference by name, or nil            |
| `globals'         | Class registry snapshot as a map                  |
| `help:'           | Formatted class documentation                     |
| `help:selector:'  | Formatted method documentation                    |
| `erlangHelp:'     | Formatted Erlang module documentation              |
| `erlangHelp:selector:' | Formatted Erlang function documentation       |
| `version'         | Beamtalk runtime version string                   |
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([dispatch/3]).
%% Direct exports for Erlang FFI calls from sealed Object BeamtalkInterface
-export([
    allClasses/0,
    classNamed/1,
    findClass/1,
    globals/0,
    help/1, help/2,
    erlangHelp/1, erlangHelp/2,
    version/0,
    findSendersIn/2,
    allSendsIn/1,
    findReferencesToIn/2,
    findFieldReadersIn/2,
    findFieldWritersIn/2,
    ffiSitesIn/4
]).

-ifdef(TEST).
%% Expose internal helper for EUnit testing (BT-2219).
-export([log_compiler_diagnostics/2]).
-endif.

%%% ============================================================================
%%% dispatch/3 — called from compiled bt@stdlib@beamtalk_interface for @primitives
%%% ============================================================================

-doc """
Dispatch a primitive method call for BeamtalkInterface.

Called by the compiled `bt@stdlib@beamtalk_interface:dispatch/3`.
""".
-spec dispatch(atom(), list(), term()) -> term().
dispatch(allClasses, [], _Self) ->
    allClasses();
dispatch('classNamed:', [ClassName], _Self) ->
    handle_class_named(ClassName);
dispatch(globals, [], _Self) ->
    handle_globals();
dispatch('help:', [ClassArg], _Self) ->
    case handle_help(ClassArg) of
        {error, Err} -> beamtalk_error:raise(Err);
        Result -> Result
    end;
dispatch('help:selector:', [ClassArg, SelectorArg], _Self) ->
    case handle_help_selector(ClassArg, SelectorArg) of
        {error, Err} -> beamtalk_error:raise(Err);
        Result -> Result
    end;
dispatch('erlangHelp:', [ModuleArg], _Self) ->
    handle_erlang_help(ModuleArg);
dispatch('erlangHelp:selector:', [ModuleArg, SelectorArg], _Self) ->
    handle_erlang_help(ModuleArg, SelectorArg);
dispatch(version, [], _Self) ->
    case application:get_key(beamtalk_runtime, vsn) of
        {ok, Vsn} -> list_to_binary(Vsn);
        _ -> <<"unknown">>
    end;
dispatch(Selector, _Args, _Self) ->
    Err0 = beamtalk_error:new(does_not_understand, 'BeamtalkInterface'),
    Err1 = beamtalk_error:with_selector(Err0, Selector),
    beamtalk_error:raise(Err1).

%%% ============================================================================
%%% Direct exports for Erlang FFI (called via ErlangModule proxy from sealed Object)
%%% ============================================================================

-doc """
Return list of all registered classes as class objects.
Called via `(Erlang beamtalk_interface) allClasses`.
""".
-spec allClasses() -> [beamtalk_object()].
allClasses() ->
    [
        begin
            ClassTag = beamtalk_class_registry:class_object_tag(Name),
            {beamtalk_object, ClassTag, Mod, Pid}
        end
     || {Name, Mod, Pid} <- beamtalk_class_registry:live_class_entries()
    ].

-doc """
Look up a class by name (atom or binary).
Called via `(Erlang beamtalk_interface) classNamed: className`.
""".
-spec classNamed(binary() | atom() | term()) -> tuple() | 'nil'.
classNamed(ClassName) ->
    case handle_class_named(ClassName) of
        {error, Err} -> beamtalk_error:raise(Err);
        Result -> Result
    end.

-doc """
Look up a class by name (atom or binary). Called via findClass: FFI.
Alias for classNamed/1 — used because classNamed: selector triggers compile-time validation.
""".
-spec findClass(binary() | atom() | term()) -> tuple() | 'nil'.
findClass(ClassName) ->
    case handle_class_named(ClassName) of
        {error, Err} -> beamtalk_error:raise(Err);
        Result -> Result
    end.

-doc """
Return class registry snapshot as a map from class name to class object.
Called via `(Erlang beamtalk_interface) globals`.
""".
-spec globals() -> map().
globals() ->
    handle_globals().

-doc """
Format class documentation (help: aClass).
Called via `(Erlang beamtalk_interface) help: aClass`.
""".
-spec help(term()) -> binary().
help(ClassArg) ->
    case handle_help(ClassArg) of
        {error, Err} -> beamtalk_error:raise(Err);
        Result -> Result
    end.

-doc """
Format method documentation (help: aClass selector: aSelector).
Called via `(Erlang beamtalk_interface) help: aClass selector: aSelector`.
""".
-spec help(term(), Selector :: atom()) -> binary().
help(ClassArg, SelectorArg) ->
    case handle_help_selector(ClassArg, SelectorArg) of
        {error, Err} -> beamtalk_error:raise(Err);
        Result -> Result
    end.

-doc """
Format Erlang module documentation (erlangHelp: moduleName).
Called via `(Erlang beamtalk_interface) erlangHelp: "lists"`.
""".
-spec erlangHelp(binary()) -> binary().
erlangHelp(ModuleArg) ->
    handle_erlang_help(ModuleArg).

-doc """
Format Erlang function documentation (erlangHelp: moduleName selector: #fn).
Called via `(Erlang beamtalk_interface) erlangHelp: "lists" selector: #reverse`.
""".
-spec erlangHelp(ModuleName :: binary(), Selector :: atom() | binary()) -> binary().
erlangHelp(ModuleArg, SelectorArg) ->
    handle_erlang_help(ModuleArg, SelectorArg).

-doc """
Return the Beamtalk runtime version string.
Called via `(Erlang beamtalk_interface) version`.
""".
-spec version() -> binary().
version() ->
    case application:get_key(beamtalk_runtime, vsn) of
        {ok, Vsn} -> list_to_binary(Vsn);
        _ -> <<"unknown">>
    end.

-doc """
Find call sites of a selector within a single method's source (BT-2190).

Backs `SystemNavigation sendersOf:`. Delegates to `beamtalk_compiler:find_senders_in_source/2`,
which parses the source via the OTP-port compiler and walks the AST for
matching `MessageSend` / `Cascade` nodes.

Called via `(Erlang beamtalk_interface) findSendersIn: source selector: aSelector`.
The selector argument is normalised to a binary before delegation so that a
Symbol from Beamtalk and a String both work. Returns a list of 1-based line
numbers (relative to the supplied source); returns `[]` if no senders are
found or the source cannot be parsed.
""".
-spec findSendersIn(binary(), atom() | binary()) -> [pos_integer()].
findSendersIn(Source, Selector) when
    is_binary(Source), (is_atom(Selector) orelse is_binary(Selector))
->
    SelectorBin =
        case Selector of
            A when is_atom(A) -> atom_to_binary(A, utf8);
            B when is_binary(B) -> B
        end,
    case beamtalk_compiler:find_senders_in_source(Source, SelectorBin) of
        {ok, Lines} ->
            Lines;
        {error, Diagnostics} ->
            %% Log the diagnostics before returning []. We still return [] because
            %% the per-method fault-tolerance contract is unchanged: a transient port
            %% failure on one method's source must not abort the whole `sendersOf:`
            %% iteration. The log exists for systemic-failure visibility — if the
            %% compiler port is wedged, every call will emit a warning (or an
            %% error for port-unavailability) and operators can see the pattern
            %% in the log rather than getting silently empty results.
            log_compiler_diagnostics(Diagnostics, 'findSendersIn:selector:'),
            []
    end;
findSendersIn(_Source, _Selector) ->
    Err0 = beamtalk_error:new(type_error, 'BeamtalkInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'findSendersIn:selector:'),
    Err2 = beamtalk_error:with_message(
        Err1,
        <<
            "findSendersIn:selector: expects a binary source and an atom or "
            "binary selector"
        >>
    ),
    beamtalk_error:raise(Err2).

-doc """
Find every message send within a single method's source (BT-2206).

Backs `SystemNavigation unimplementedSelectors`. Single-pass companion to
`findSendersIn/2`: delegates to `beamtalk_compiler:find_all_sends_in_source/1`,
which parses the source via the OTP-port compiler and walks the AST collecting
EVERY `MessageSend` / `Cascade` send (selector, line, receiver kind) rather than
filtering by one selector.

Called via `(Erlang beamtalk_interface) allSendsIn: source`. Each compiler-side
send map `#{selector := Bin, line := Line, recv := Recv}` is converted to a
3-TUPLE `{Selector, Line, Recv}` where `Selector` is the selector atom and
`Recv` is `self`, `super`, `erlang_ffi`, or `other`. `binary_to_atom/2` (not
`binary_to_existing_atom`) is used because a send may reference a selector atom
that does not yet exist anywhere in the loaded image — that is precisely the
typo case the caller is hunting for. Returning 3-tuples mirrors how
`beamtalk_extensions list:` / `listAllWithSource` already feed BT data accessed
via `entry at: N`.

Returns a list of 3-tuples; returns `[]` if the source has no sends, cannot be
parsed, or the compiler port is unavailable. The degrade-on-error behaviour
mirrors `findSendersIn/2`: a transient port failure on one method's source must
not abort the whole `unimplementedSelectors` walk.
""".
-spec allSendsIn(binary()) -> [{atom(), pos_integer(), atom()}].
allSendsIn(Source) when is_binary(Source) ->
    case beamtalk_compiler:find_all_sends_in_source(Source) of
        {ok, Sends} ->
            [
                {binary_to_atom(Sel, utf8), Line, Recv}
             || #{selector := Sel, line := Line, recv := Recv} <- Sends
            ];
        {error, _Diagnostics} ->
            %% Compiler port unavailable — degrade to "no sends found" rather
            %% than crashing the caller. Iteration in `unimplementedSelectors`
            %% should not be aborted by a transient port failure on one
            %% method's source.
            []
    end;
allSendsIn(_Source) ->
    Err0 = beamtalk_error:new(type_error, 'BeamtalkInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'allSendsIn:'),
    Err2 = beamtalk_error:with_message(
        Err1,
        <<"allSendsIn: expects a binary source">>
    ),
    beamtalk_error:raise(Err2).

-doc """
Find references to a class within a single method's source (BT-2203).

Backs `SystemNavigation referencesTo:'. Delegates to
`beamtalk_compiler:find_references_to_in_source/2', which parses the source
via the OTP-port compiler and walks the AST for matching `ClassReference'
nodes (plus class names in type annotations).

Called via `(Erlang beamtalk_interface) findReferencesToIn: source class: aClassName'.
The class-name argument is normalised to a binary before delegation so that a
Symbol from Beamtalk and a String both work. Returns a list of 1-based line
numbers (relative to the supplied source); returns `[]' if no references are
found or the source cannot be parsed.
""".
-spec findReferencesToIn(binary(), atom() | binary()) -> [pos_integer()].
findReferencesToIn(Source, ClassName) when
    is_binary(Source), (is_atom(ClassName) orelse is_binary(ClassName))
->
    ClassNameBin =
        case ClassName of
            A when is_atom(A) -> atom_to_binary(A, utf8);
            B when is_binary(B) -> B
        end,
    case beamtalk_compiler:find_references_to_in_source(Source, ClassNameBin) of
        {ok, Lines} ->
            Lines;
        {error, Diagnostics} ->
            %% Log the diagnostics before returning []. We still return [] because
            %% the per-method fault-tolerance contract is unchanged: a transient port
            %% failure on one method's source must not abort the whole `referencesTo:'
            %% iteration. The log exists for systemic-failure visibility — if the
            %% compiler port is wedged, every call will emit a warning (or an
            %% error for port-unavailability) and operators can see the pattern
            %% in the log rather than getting silently empty results.
            log_compiler_diagnostics(Diagnostics, 'findReferencesToIn:class:'),
            []
    end;
findReferencesToIn(_Source, _ClassName) ->
    Err0 = beamtalk_error:new(type_error, 'BeamtalkInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'findReferencesToIn:class:'),
    Err2 = beamtalk_error:with_message(
        Err1,
        <<
            "findReferencesToIn:class: expects a binary source and an atom or "
            "binary class name"
        >>
    ),
    beamtalk_error:raise(Err2).

-doc """
Find reads of an field within a single method's source (BT-2208).

Backs `SystemNavigation fieldReadersOf:in:`. Delegates to
`beamtalk_compiler:find_field_readers_in_source/2`, which parses the source
via the OTP-port compiler and walks the AST for `FieldAccess` nodes (`self.x`)
whose field name matches the slot and that are NOT an assignment target.

Called via `(Erlang beamtalk_interface) findFieldReadersIn: source field: aName`.
The slot-name argument is normalised to a binary before delegation so that a
Symbol from Beamtalk and a String both work. Returns a list of 1-based line
numbers (relative to the supplied source); returns `[]` if no reads are found
or the source cannot be parsed.
""".
-spec findFieldReadersIn(binary(), atom() | binary()) -> [pos_integer()].
findFieldReadersIn(Source, Field) ->
    find_field_access(Source, Field, readers, 'findFieldReadersIn:field:').

-doc """
Find writes of an field within a single method's source (BT-2208).

Backs `SystemNavigation fieldWritersOf:in:`. Delegates to
`beamtalk_compiler:find_field_writers_in_source/2`, which parses the source
via the OTP-port compiler and walks the AST for assignment targets (`self.x :=
...`) whose field name matches the slot.

Called via `(Erlang beamtalk_interface) findFieldWritersIn: source field: aName`.
The slot-name argument is normalised to a binary before delegation so that a
Symbol from Beamtalk and a String both work. Returns a list of 1-based line
numbers (relative to the supplied source); returns `[]` if no writes are found
or the source cannot be parsed.
""".
-spec findFieldWritersIn(binary(), atom() | binary()) -> [pos_integer()].
findFieldWritersIn(Source, Field) ->
    find_field_access(Source, Field, writers, 'findFieldWritersIn:field:').

-doc """
Shared driver for the field reader/writer FFI calls (BT-2208).

`Kind` selects the underlying compiler query (`readers' or `writers'); both
take a binary source and an field name and return a list of
1-based line numbers, so they share the validation and fault-tolerance path.
Mirrors `findSendersIn/2`: a transient port failure on one method's source is
logged and degraded to `[]` rather than aborting the caller's whole iteration.
""".
-spec find_field_access(binary(), atom() | binary(), readers | writers, atom()) ->
    [pos_integer()].
find_field_access(Source, Field, Kind, Selector) when
    is_binary(Source), (is_atom(Field) orelse is_binary(Field))
->
    IVarBin =
        case Field of
            A when is_atom(A) -> atom_to_binary(A, utf8);
            B when is_binary(B) -> B
        end,
    Result =
        case Kind of
            readers -> beamtalk_compiler:find_field_readers_in_source(Source, IVarBin);
            writers -> beamtalk_compiler:find_field_writers_in_source(Source, IVarBin)
        end,
    case Result of
        {ok, Lines} ->
            Lines;
        {error, Diagnostics} ->
            %% Degrade to [] on a per-method basis (same contract as
            %% findSendersIn/2): a transient port failure on one method's
            %% source must not abort the whole fieldReadersOf:/fieldWritersOf:
            %% walk. The log gives systemic-failure visibility.
            log_compiler_diagnostics(Diagnostics, Selector),
            []
    end;
find_field_access(_Source, _IVar, _Kind, Selector) ->
    Err0 = beamtalk_error:new(type_error, 'BeamtalkInterface'),
    Err1 = beamtalk_error:with_selector(Err0, Selector),
    Err2 = beamtalk_error:with_message(
        Err1,
        <<
            "expects a binary source and an atom or binary "
            "field name"
        >>
    ),
    beamtalk_error:raise(Err2).

-doc """
Find Erlang FFI call sites within a single method's source (BT-2211).

Backs `SystemNavigation ffiSitesFor:`. Delegates to
`beamtalk_compiler:find_ffi_sites_in_source/4`, which parses the source via the
OTP-port compiler and walks the AST for `MessageSend` nodes that resolve through
the `Erlang` FFI bridge to the named function.

Called via
`(Erlang beamtalk_interface) ffiSitesIn: source module: m function: f arity: a`.
`Module` and `Function` are normalised to binaries so a Symbol from Beamtalk
and a String both work. `Arity` is a non-negative integer to constrain the match
to that argument count, or the atom `any` (the conventional Beamtalk encoding of
"any arity"). Returns a list of 1-based line numbers (relative to the supplied
source); returns `[]` if no sites are found or the source cannot be parsed.

Mirrors `findSendersIn/2`: a transient port failure on one method's source is
logged and degraded to `[]` rather than aborting the caller's whole iteration.
""".
-spec ffiSitesIn(binary(), atom() | binary(), atom() | binary(), non_neg_integer() | any) ->
    [pos_integer()].
ffiSitesIn(Source, Module, Function, Arity) when
    is_binary(Source),
    (is_atom(Module) orelse is_binary(Module)),
    (is_atom(Function) orelse is_binary(Function)),
    (Arity =:= any orelse (is_integer(Arity) andalso Arity >= 0))
->
    ModuleBin = to_binary(Module),
    FunctionBin = to_binary(Function),
    case beamtalk_compiler:find_ffi_sites_in_source(Source, ModuleBin, FunctionBin, Arity) of
        {ok, Lines} ->
            Lines;
        {error, Diagnostics} ->
            %% Degrade to [] on a per-method basis (same contract as
            %% findSendersIn/2): a transient port failure on one method's source
            %% must not abort the whole ffiSitesFor: walk. The log gives
            %% systemic-failure visibility.
            log_compiler_diagnostics(Diagnostics, 'ffiSitesIn:module:function:arity:'),
            []
    end;
ffiSitesIn(_Source, _Module, _Function, _Arity) ->
    Err0 = beamtalk_error:new(type_error, 'BeamtalkInterface'),
    Err1 = beamtalk_error:with_selector(Err0, 'ffiSitesIn:module:function:arity:'),
    Err2 = beamtalk_error:with_message(
        Err1,
        <<
            "ffiSitesIn:module:function:arity: expects a binary source, atom or "
            "binary module and function, and a non-negative integer arity or the "
            "atom any"
        >>
    ),
    beamtalk_error:raise(Err2).

%% Normalise an atom-or-binary identifier to a binary.
-spec to_binary(atom() | binary()) -> binary().
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) -> B.

%%% ============================================================================
%%% Internal method implementations
%%% ============================================================================

-doc """
Log compiler-port diagnostics at the appropriate OTP logger level.

Called when `beamtalk_compiler' returns `{error, Diagnostics}' from a source-
analysis call (find_senders_in_source, find_references_to_in_source, etc.).

Uses `?LOG_ERROR' if the diagnostics indicate port unavailability (messages
containing "not available" or "timed out"), `?LOG_WARNING' otherwise (e.g. a
parse failure on a specific method's source).

Includes `domain => [beamtalk, stdlib]' metadata on every log call per project
convention (CLAUDE.md).
""".
-spec log_compiler_diagnostics([map()], atom()) -> ok.
log_compiler_diagnostics(Diagnostics, Selector) ->
    IsPortUnavailable = lists:any(
        fun(D) ->
            Msg = maps:get(message, D, <<>>),
            %% Lowercase before matching so capitalised variants (e.g. "Timed out")
            %% are still classified as port-unavailability (error, not warning).
            MsgLc =
                case is_binary(Msg) of
                    true -> string:lowercase(Msg);
                    false -> <<>>
                end,
            binary:match(MsgLc, <<"not available">>) =/= nomatch orelse
                binary:match(MsgLc, <<"timed out">>) =/= nomatch
        end,
        Diagnostics
    ),
    %% Include a short summary in the message body itself: in text log mode the
    %% handler template renders only `msg`, so the full `diagnostics` payload in
    %% metadata is invisible there (it only shows up in structured/JSON logs).
    Summary = summarise_diagnostics(Diagnostics),
    Meta = #{selector => Selector, diagnostics => Diagnostics, domain => [beamtalk, stdlib]},
    case IsPortUnavailable of
        true ->
            ?LOG_ERROR(
                "Compiler port unavailable in ~p: ~ts — returning [] (per-method fault-tolerance preserved)",
                [Selector, Summary],
                Meta
            );
        false ->
            ?LOG_WARNING(
                "Compiler port returned diagnostics in ~p: ~ts — returning [] (per-method fault-tolerance preserved)",
                [Selector, Summary],
                Meta
            )
    end.

-doc "Build a short human-readable summary of compiler diagnostics for a log message body.".
-spec summarise_diagnostics([map()]) -> binary().
summarise_diagnostics([]) ->
    <<"(no diagnostics)">>;
summarise_diagnostics([First | Rest]) ->
    MsgBin = diagnostic_message(First),
    case Rest of
        [] -> MsgBin;
        _ -> iolist_to_binary([MsgBin, " (+", integer_to_binary(length(Rest)), " more)"])
    end.

-doc "Extract a diagnostic's `message` as a binary, falling back to a printed term.".
-spec diagnostic_message(map()) -> binary().
diagnostic_message(D) ->
    case maps:get(message, D, <<"(no message)">>) of
        Msg when is_binary(Msg) -> Msg;
        Other -> iolist_to_binary(io_lib:format("~p", [Other]))
    end.

-doc "Format Erlang module help via beamtalk_erlang_help (dynamic call).".
-spec handle_erlang_help(binary()) -> binary().
handle_erlang_help(ModuleBin) when is_binary(ModuleBin) ->
    try binary_to_existing_atom(ModuleBin, utf8) of
        Module ->
            case beamtalk_erlang_help:format_module_help(Module) of
                {ok, Text} ->
                    Text;
                {error, not_found} ->
                    Err = beamtalk_error:new(not_found, 'BeamtalkInterface'),
                    Err1 = beamtalk_error:with_message(
                        Err,
                        iolist_to_binary([<<"Erlang module '">>, ModuleBin, <<"' not found">>])
                    ),
                    Err2 = beamtalk_error:with_hint(
                        Err1,
                        <<"Check the module name and ensure it is available on the code path.">>
                    ),
                    beamtalk_error:raise(Err2)
            end
    catch
        error:badarg ->
            Err = beamtalk_error:new(not_found, 'BeamtalkInterface'),
            Err1 = beamtalk_error:with_message(
                Err,
                iolist_to_binary([<<"Erlang module '">>, ModuleBin, <<"' not found">>])
            ),
            Err2 = beamtalk_error:with_hint(
                Err1,
                <<"Check the module name and ensure it is available on the code path.">>
            ),
            beamtalk_error:raise(Err2)
    end;
handle_erlang_help(_ModuleArg) ->
    Err = beamtalk_error:new(type_error, 'BeamtalkInterface'),
    Err1 = beamtalk_error:with_selector(Err, 'erlangHelp:'),
    Err2 = beamtalk_error:with_message(
        Err1, <<"erlangHelp: expects a binary module name">>
    ),
    beamtalk_error:raise(Err2).

-doc "Format Erlang function help via beamtalk_erlang_help (dynamic call).".
-spec handle_erlang_help(binary(), atom() | binary()) -> binary().
handle_erlang_help(ModuleBin, SelectorArg) when
    is_binary(ModuleBin), (is_atom(SelectorArg) orelse is_binary(SelectorArg))
->
    FunctionBin =
        case SelectorArg of
            A when is_atom(A) -> atom_to_binary(A, utf8);
            B when is_binary(B) -> B
        end,
    try binary_to_existing_atom(ModuleBin, utf8) of
        Module ->
            case beamtalk_erlang_help:format_function_help(Module, FunctionBin) of
                {ok, Text} ->
                    Text;
                {error, not_found} ->
                    Err = beamtalk_error:new(not_found, 'BeamtalkInterface'),
                    Err1 = beamtalk_error:with_message(
                        Err,
                        iolist_to_binary([ModuleBin, <<":">>, FunctionBin, <<" not found">>])
                    ),
                    Err2 = beamtalk_error:with_hint(
                        Err1,
                        iolist_to_binary([
                            <<"Use Beamtalk erlangHelp: \"">>,
                            ModuleBin,
                            <<"\" to see available functions.">>
                        ])
                    ),
                    beamtalk_error:raise(Err2)
            end
    catch
        error:badarg ->
            Err = beamtalk_error:new(not_found, 'BeamtalkInterface'),
            Err1 = beamtalk_error:with_message(
                Err,
                iolist_to_binary([<<"Erlang module '">>, ModuleBin, <<"' not found">>])
            ),
            Err2 = beamtalk_error:with_hint(
                Err1,
                <<"Check the module name and ensure it is available on the code path.">>
            ),
            beamtalk_error:raise(Err2)
    end;
handle_erlang_help(_ModuleArg, _SelectorArg) ->
    Err = beamtalk_error:new(type_error, 'BeamtalkInterface'),
    Err1 = beamtalk_error:with_selector(Err, 'erlangHelp:selector:'),
    Err2 = beamtalk_error:with_message(
        Err1, <<"erlangHelp:selector: expects a binary module name and atom/binary function name">>
    ),
    beamtalk_error:raise(Err2).

-doc "Look up a class by name and return a wrapped class object or nil.".
-spec handle_class_named(binary() | atom() | term()) ->
    beamtalk_object() | 'nil' | {error, #beamtalk_error{}}.
handle_class_named(ClassName) when is_binary(ClassName) ->
    try
        ClassAtom = binary_to_existing_atom(ClassName, utf8),
        handle_class_named(ClassAtom)
    catch
        %% The full tag atom is not interned. It may still be a metaclass tag
        %% (`<<"Foo class">>`) whose base class is live — decode on the binary
        %% so resolution does not depend on the tag atom's interning history.
        error:badarg ->
            resolve_metaclass_tag(ClassName)
    end;
handle_class_named(ClassName) when is_atom(ClassName) ->
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined ->
            resolve_metaclass_tag(ClassName);
        Pid when is_pid(Pid) ->
            ModuleName = beamtalk_object_class:module_name(Pid),
            ClassTag = beamtalk_class_registry:class_object_tag(ClassName),
            {beamtalk_object, ClassTag, ModuleName, Pid}
    end;
handle_class_named(_ClassName) ->
    Error0 = beamtalk_error:new(type_error, 'BeamtalkInterface'),
    Error1 = beamtalk_error:with_selector(Error0, 'classNamed:'),
    Error2 = beamtalk_error:with_hint(
        Error1, <<"classNamed: expects an atom or binary class name">>
    ),
    {error, Error2}.

-doc """
Resolve a metaclass display tag (`'Foo class'`) to the `Foo class` metaclass
object, or nil when the tag is not a metaclass tag or names no live class.

BT-2223: The extension registry keys class-side extensions under the metaclass
display tag (`className ++ " class"`), produced by
`beamtalk_class_registry:class_object_tag/1`. `whereis_class/1` has no entry
for that tag, so `findClass:`/`classNamed:` would otherwise return nil for a
class-side tag. We decode the tag with the same runtime authority that encodes
it (`beamtalk_class_registry:is_class_name/1` + `class_display_name/1`),
resolve the base class, and return its metaclass object — structurally
identical to `beamtalk_behaviour_intrinsics:classClass/1` (ADR 0036) so that
`==` against `Foo class` holds. This keeps the encode/decode of the metaclass
tag convention in one place rather than reverse-engineered with string surgery
in the stdlib.

Accepts an atom or binary tag. Decoding the suffix off the binary means only
the base class atom must be interned (always true for a live class), so binary
metaclass tags resolve regardless of whether the full `'Foo class'` atom was
ever created — and without minting new atoms from caller input.
""".
-spec resolve_metaclass_tag(atom() | binary()) -> beamtalk_object() | 'nil'.
resolve_metaclass_tag(Tag) ->
    case beamtalk_class_registry:is_class_name(Tag) of
        false ->
            nil;
        true ->
            BaseName = beamtalk_class_registry:class_display_name(Tag),
            try binary_to_existing_atom(BaseName, utf8) of
                BaseClass ->
                    case beamtalk_class_registry:whereis_class(BaseClass) of
                        undefined ->
                            nil;
                        Pid when is_pid(Pid) ->
                            #beamtalk_object{
                                class = 'Metaclass',
                                class_mod = beamtalk_metaclass_bt,
                                pid = Pid
                            }
                    end
            catch
                error:badarg -> nil
            end
    end.

-doc "Get workspace global bindings as a map from class name to class object.".
-spec handle_globals() -> map().
handle_globals() ->
    lists:foldl(
        fun({Name, ModuleName, Pid}, Acc) ->
            ClassTag = beamtalk_class_registry:class_object_tag(Name),
            ClassObj = {beamtalk_object, ClassTag, ModuleName, Pid},
            Acc#{Name => ClassObj}
        end,
        #{},
        beamtalk_class_registry:live_class_entries()
    ).

-doc "Format class documentation for help:.".
-spec handle_help(term()) -> binary() | {error, #beamtalk_error{}}.
handle_help(ClassArg) ->
    case resolve_class_name(ClassArg) of
        {error, Err} ->
            {error, Err};
        {ok, ClassName} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    {error, make_class_not_found_error(ClassName)};
                ClassPid ->
                    try
                        format_class_help(ClassName, ClassPid)
                    catch
                        exit:{noproc, _} -> {error, make_class_not_found_error(ClassName)};
                        exit:{timeout, _} -> {error, make_class_not_found_error(ClassName)}
                    end
            end
    end.

-doc """
Format method documentation for help:selector:.

BT-2091: Walks both the instance-side and class-side method tables, mirroring
the deprecated `docs` op's behaviour so `:help ClassName <classSideMethod>`
keeps working after that op was removed. The lookup order is:

  1. Metaclass — answered from the hardcoded metaclass method dictionary.
  2. Instance-side method (via beamtalk_method_resolver:resolve/2).
  3. Class-side method (via {class_method, Selector} on the class object).
  4. Class protocol fallback (resolve/2 against the `Class` class).

If none match, returns method_not_found.
""".
-spec handle_help_selector(term(), atom()) -> binary() | {error, #beamtalk_error{}}.
handle_help_selector(ClassArg, SelectorArg) ->
    case resolve_class_name(ClassArg) of
        {error, Err} ->
            {error, Err};
        {ok, 'Metaclass'} ->
            %% Normalise via ensure_atom/1 so a binary selector becomes an
            %% atom before the not_found error path interpolates it.
            case ensure_atom(SelectorArg) of
                {error, Err} ->
                    {error, Err};
                SelectorAtom ->
                    handle_metaclass_help_selector(SelectorAtom)
            end;
        {ok, ClassName} ->
            case beamtalk_class_registry:whereis_class(ClassName) of
                undefined ->
                    {error, make_class_not_found_error(ClassName)};
                ClassPid ->
                    case ensure_atom(SelectorArg) of
                        {error, Err} ->
                            {error, Err};
                        SelectorAtom ->
                            try
                                resolve_help_method(ClassName, ClassPid, SelectorAtom)
                            catch
                                exit:{noproc, _} ->
                                    {error, make_class_not_found_error(ClassName)};
                                exit:{timeout, _} ->
                                    {error, make_class_not_found_error(ClassName)}
                            end
                    end
            end
    end.

%% BT-2091: Walk instance side first, then class side, then Class protocol.
-spec resolve_help_method(atom(), pid(), atom()) -> binary() | {error, #beamtalk_error{}}.
resolve_help_method(ClassName, ClassPid, SelectorAtom) ->
    case beamtalk_method_resolver:resolve(ClassPid, SelectorAtom) of
        MethodObj when is_map(MethodObj) ->
            DefiningClass = find_defining_class(ClassPid, SelectorAtom),
            format_method_help(ClassName, SelectorAtom, DefiningClass, MethodObj);
        nil ->
            case gen_server:call(ClassPid, {class_method, SelectorAtom}, 5000) of
                ClassMethodObj when is_map(ClassMethodObj) ->
                    DefiningClass = find_defining_class_method(ClassPid, SelectorAtom),
                    format_method_help(
                        ClassName, SelectorAtom, DefiningClass, ClassMethodObj
                    );
                nil ->
                    %% Final fallback: methods inherited from the `Class` class
                    %% (the class object's protocol — e.g. `name`, `superclass`).
                    case beamtalk_class_registry:whereis_class('Class') of
                        undefined ->
                            {error, make_method_not_found_error(ClassName, SelectorAtom)};
                        _ ->
                            case beamtalk_method_resolver:resolve('Class', SelectorAtom) of
                                ProtoObj when is_map(ProtoObj) ->
                                    format_method_help(
                                        ClassName, SelectorAtom, 'Class', ProtoObj
                                    );
                                nil ->
                                    {error, make_method_not_found_error(ClassName, SelectorAtom)}
                            end
                    end
            end
    end.

%% BT-2091: Walk the class hierarchy looking at *class-side* method tables
%% so the help header attributes the method to the class that actually
%% defines it (mirrors find_defining_class/3 for the instance side).
-spec find_defining_class_method(pid(), atom()) -> atom().
find_defining_class_method(ClassPid, Selector) ->
    find_defining_class_method(ClassPid, Selector, 0).

-spec find_defining_class_method(pid(), atom(), non_neg_integer()) -> atom().
find_defining_class_method(ClassPid, _Selector, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    beamtalk_object_class:class_name(ClassPid);
find_defining_class_method(ClassPid, Selector, Depth) ->
    Name = beamtalk_object_class:class_name(ClassPid),
    LocalClassMethods = gen_server:call(ClassPid, get_local_class_methods, 5000),
    case maps:is_key(Selector, LocalClassMethods) of
        true ->
            Name;
        false ->
            case gen_server:call(ClassPid, superclass, 5000) of
                none ->
                    Name;
                SuperName ->
                    case beamtalk_class_registry:whereis_class(SuperName) of
                        undefined -> Name;
                        SuperPid -> find_defining_class_method(SuperPid, Selector, Depth + 1)
                    end
            end
    end.

%% BT-2091: Hardcoded Metaclass method docs (mirrors beamtalk_repl_docs').
%% Caller must normalise SelectorArg via ensure_atom/1 first so a binary
%% selector cannot reach this path; that keeps make_method_not_found_error
%% unable to crash on a binary input.
-spec handle_metaclass_help_selector(atom()) ->
    binary() | {error, #beamtalk_error{}}.
handle_metaclass_help_selector(SelectorAtom) ->
    SelectorBin = atom_to_binary(SelectorAtom, utf8),
    case metaclass_method_doc(SelectorBin) of
        {ok, Doc} ->
            iolist_to_binary([
                <<"== Metaclass >> ">>,
                SelectorBin,
                <<" ==">>,
                <<"\n  ">>,
                SelectorBin,
                <<"\n\n">>,
                Doc
            ]);
        not_found ->
            {error, make_method_not_found_error('Metaclass', SelectorAtom)}
    end.

-spec metaclass_method_doc(binary()) -> {ok, binary()} | not_found.
metaclass_method_doc(<<"new">>) ->
    {ok, <<"Create a new instance of the class.">>};
metaclass_method_doc(<<"spawn">>) ->
    {ok, <<"Create a new actor instance. Returns an actor reference.">>};
metaclass_method_doc(<<"spawnWith:">>) ->
    {ok, <<"Create a new actor with initial state from a Dictionary.">>};
metaclass_method_doc(_) ->
    not_found.

-doc "Resolve a class argument to an atom class name.".
-spec resolve_class_name(term()) -> {ok, atom()} | {error, #beamtalk_error{}}.
resolve_class_name(#beamtalk_object{pid = ClassPid}) when is_pid(ClassPid) ->
    try
        Name = beamtalk_object_class:class_name(ClassPid),
        {ok, Name}
    catch
        exit:{noproc, _} ->
            Error0 = beamtalk_error:new(class_not_found, 'BeamtalkInterface'),
            {error, beamtalk_error:with_message(Error0, <<"Class process no longer alive">>)};
        exit:{timeout, _} ->
            Error0 = beamtalk_error:new(class_not_found, 'BeamtalkInterface'),
            {error, beamtalk_error:with_message(Error0, <<"Class process not responding">>)}
    end;
resolve_class_name(Name) when is_atom(Name) ->
    {ok, Name};
resolve_class_name(Name) when is_binary(Name) ->
    try
        {ok, binary_to_existing_atom(Name, utf8)}
    catch
        error:badarg ->
            {error, make_class_not_found_error(Name)}
    end;
resolve_class_name(_Other) ->
    Error0 = beamtalk_error:new(type_error, 'BeamtalkInterface'),
    {error, beamtalk_error:with_message(Error0, <<"Expected a class or symbol argument">>)}.

-doc "Ensure a selector argument is an existing atom.".
-spec ensure_atom(atom() | binary()) -> atom() | {error, #beamtalk_error{}}.
ensure_atom(A) when is_atom(A) -> A;
ensure_atom(B) when is_binary(B) ->
    try
        binary_to_existing_atom(B, utf8)
    catch
        error:badarg ->
            Err0 = beamtalk_error:new(type_error, 'BeamtalkInterface'),
            Err1 = beamtalk_error:with_selector(Err0, 'help:selector:'),
            {error,
                beamtalk_error:with_message(
                    Err1,
                    iolist_to_binary([<<"Unknown selector: ">>, B])
                )}
    end.

-doc "Format class-level help output.".
-spec format_class_help(atom(), pid()) -> binary().
format_class_help(ClassName, ClassPid) ->
    Superclass = gen_server:call(ClassPid, superclass, 5000),
    IsSealed = gen_server:call(ClassPid, is_sealed, 5000),
    IsAbstract = gen_server:call(ClassPid, is_abstract, 5000),
    ModuleDoc =
        case gen_server:call(ClassPid, get_doc, 5000) of
            none -> none;
            Doc when is_binary(Doc) -> Doc
        end,

    Flattened = collect_flattened_methods(ClassName, ClassPid),

    {Own, Inherited} = maps:fold(
        fun(Selector, {DefClass, MethodInfo}, {OwnAcc, InhAcc}) ->
            case DefClass of
                ClassName ->
                    MethodSealed = maps:get(is_sealed, MethodInfo, false),
                    {[{Selector, MethodSealed} | OwnAcc], InhAcc};
                _ ->
                    {OwnAcc, [{Selector, DefClass} | InhAcc]}
            end
        end,
        {[], []},
        Flattened
    ),

    OwnSelectors = lists:sort([S || {S, _} <- Own]),
    SealedMap = maps:from_list(Own),
    OwnDocs = lists:map(
        fun(Sel) ->
            {Sig, _Doc} = get_method_sig(ClassPid, Sel),
            IsSealedMethod = maps:get(Sel, SealedMap, false),
            {Sel, Sig, IsSealedMethod}
        end,
        OwnSelectors
    ),

    InheritedGrouped = group_by_class(lists:sort(Inherited)),

    NameBin = atom_to_binary(ClassName, utf8),
    Header =
        case Superclass of
            none ->
                iolist_to_binary([<<"== ">>, NameBin, <<" ==">>]);
            Super ->
                iolist_to_binary([
                    <<"== ">>, NameBin, <<" < ">>, atom_to_binary(Super, utf8), <<" ==">>
                ])
        end,

    ModifierPart =
        case {IsSealed, IsAbstract} of
            {true, true} -> <<"\n[sealed] [abstract]">>;
            {true, false} -> <<"\n[sealed]">>;
            {false, true} -> <<"\n[abstract]">>;
            {false, false} -> <<>>
        end,

    DocPart =
        case ModuleDoc of
            none -> <<>>;
            Text -> iolist_to_binary([<<"\n">>, Text])
        end,

    OwnMethodsPart =
        case OwnDocs of
            [] ->
                <<>>;
            _ ->
                Lines = lists:map(
                    fun
                        ({_Sel, Sig, true}) ->
                            iolist_to_binary([<<"  ">>, Sig, <<" [sealed]">>]);
                        ({_Sel, Sig, false}) ->
                            iolist_to_binary([<<"  ">>, Sig])
                    end,
                    OwnDocs
                ),
                iolist_to_binary([<<"\nInstance methods:\n">>, lists:join(<<"\n">>, Lines)])
        end,

    InheritedParts = lists:map(
        fun({FromClass, Selectors}) ->
            Count = length(Selectors),
            Summary =
                case Count =< 5 of
                    true ->
                        lists:join(<<", ">>, [atom_to_binary(S, utf8) || S <- Selectors]);
                    false ->
                        {First3, _} = lists:split(3, Selectors),
                        Remaining = Count - 3,
                        iolist_to_binary([
                            lists:join(<<", ">>, [atom_to_binary(S, utf8) || S <- First3]),
                            <<", ... (">>,
                            integer_to_binary(Remaining),
                            <<" more)">>
                        ])
                end,
            iolist_to_binary([
                <<"\nInherited from ">>,
                atom_to_binary(FromClass, utf8),
                <<" (">>,
                integer_to_binary(Count),
                <<" methods): ">>,
                Summary
            ])
        end,
        InheritedGrouped
    ),

    HintPart = <<"\nUse Beamtalk help: ClassName selector: #method for method details.">>,

    AllParts = [Header, ModifierPart, DocPart, OwnMethodsPart | InheritedParts] ++ [HintPart],
    iolist_to_binary(
        lists:filter(
            fun
                (<<>>) -> false;
                (_) -> true
            end,
            lists:flatten(AllParts)
        )
    ).

-doc "Format method-level help output.".
-spec format_method_help(atom(), atom(), atom(), map()) -> binary().
format_method_help(ClassName, SelectorAtom, DefiningClass, MethodObj) ->
    SelectorBin = atom_to_binary(SelectorAtom, utf8),
    NameBin = atom_to_binary(ClassName, utf8),

    Header = iolist_to_binary([<<"== ">>, NameBin, <<" >> ">>, SelectorBin, <<" ==">>]),

    IsSealed =
        case maps:get('__method_info__', MethodObj, #{}) of
            MethodInfo when is_map(MethodInfo) ->
                maps:get(is_sealed, MethodInfo, false);
            _ ->
                false
        end,

    SealedLine =
        case IsSealed of
            true -> <<"\n[sealed]">>;
            false -> <<>>
        end,

    InheritedPart =
        case DefiningClass of
            ClassName ->
                <<>>;
            _ ->
                iolist_to_binary([
                    <<"\n(inherited from ">>, atom_to_binary(DefiningClass, utf8), <<")">>
                ])
        end,

    Signature =
        case maps:get('__signature__', MethodObj, nil) of
            nil -> SelectorBin;
            SigBin when is_binary(SigBin) -> SigBin
        end,

    SignatureLine = iolist_to_binary([<<"\n  ">>, Signature]),

    DocPart =
        case maps:get('__doc__', MethodObj, nil) of
            nil -> <<>>;
            DocBin when is_binary(DocBin) -> iolist_to_binary([<<"\n\n">>, DocBin])
        end,

    iolist_to_binary([Header, SealedLine, InheritedPart, SignatureLine, DocPart]).

-doc "Get method signature from a class pid.".
-spec get_method_sig(pid(), atom()) -> {binary(), binary() | none}.
get_method_sig(ClassPid, Selector) ->
    case gen_server:call(ClassPid, {method, Selector}, 5000) of
        nil ->
            {atom_to_binary(Selector, utf8), none};
        MethodObj when is_map(MethodObj) ->
            Doc =
                case maps:get('__doc__', MethodObj, nil) of
                    nil -> none;
                    D when is_binary(D) -> D
                end,
            {atom_to_binary(Selector, utf8), Doc}
    end.

-doc "Walk the class hierarchy to collect flattened method map.".
-spec collect_flattened_methods(atom(), pid()) -> map().
collect_flattened_methods(ClassName, ClassPid) ->
    collect_flattened_methods(ClassName, ClassPid, 0).

-spec collect_flattened_methods(atom(), pid(), non_neg_integer()) -> map().
collect_flattened_methods(_ClassName, _ClassPid, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    #{};
collect_flattened_methods(ClassName, ClassPid, Depth) ->
    {ok, LocalMethods} = gen_server:call(ClassPid, get_instance_methods, 5000),
    LocalFlat = maps:map(fun(_Sel, Info) -> {ClassName, Info} end, LocalMethods),
    Superclass = gen_server:call(ClassPid, superclass, 5000),
    SuperFlat = collect_chain_methods(Superclass, Depth + 1),
    maps:merge(SuperFlat, LocalFlat).

-spec collect_chain_methods(atom() | none, non_neg_integer()) -> map().
collect_chain_methods(none, _Depth) ->
    #{};
collect_chain_methods(SuperName, Depth) ->
    case beamtalk_class_registry:whereis_class(SuperName) of
        undefined -> #{};
        SuperPid -> collect_flattened_methods(SuperName, SuperPid, Depth)
    end.

-doc "Find which class in the hierarchy defines a selector.".
-spec find_defining_class(pid(), atom()) -> atom().
find_defining_class(ClassPid, Selector) ->
    find_defining_class(ClassPid, Selector, 0).

-spec find_defining_class(pid(), atom(), non_neg_integer()) -> atom().
find_defining_class(ClassPid, _Selector, Depth) when Depth > ?MAX_HIERARCHY_DEPTH ->
    gen_server:call(ClassPid, class_name, 5000);
find_defining_class(ClassPid, Selector, Depth) ->
    ClassName = gen_server:call(ClassPid, class_name, 5000),
    case gen_server:call(ClassPid, {method, Selector}, 5000) of
        nil ->
            case gen_server:call(ClassPid, superclass, 5000) of
                none ->
                    ClassName;
                Super ->
                    case beamtalk_class_registry:whereis_class(Super) of
                        undefined -> ClassName;
                        SuperPid -> find_defining_class(SuperPid, Selector, Depth + 1)
                    end
            end;
        _MethodInfo ->
            ClassName
    end.

-doc "Group inherited methods by defining class.".
-spec group_by_class([{atom(), atom()}]) -> [{atom(), [atom()]}].
group_by_class(Methods) ->
    Grouped = lists:foldl(
        fun({Selector, DefClass}, Acc) ->
            Existing = maps:get(DefClass, Acc, []),
            Acc#{DefClass => [Selector | Existing]}
        end,
        #{},
        Methods
    ),
    lists:sort(
        maps:fold(
            fun(Class, Selectors, Acc) ->
                [{Class, lists:sort(Selectors)} | Acc]
            end,
            [],
            Grouped
        )
    ).

-doc "Build a structured error for a class not found.".
-spec make_class_not_found_error(atom() | binary()) -> #beamtalk_error{}.
make_class_not_found_error(ClassName) ->
    NameBin =
        case ClassName of
            A when is_atom(A) -> atom_to_binary(A, utf8);
            B when is_binary(B) -> B
        end,
    Err0 = beamtalk_error:new(class_not_found, 'BeamtalkInterface'),
    Err1 = beamtalk_error:with_message(
        Err0,
        iolist_to_binary([<<"Class '">>, NameBin, <<"' not found.">>])
    ),
    beamtalk_error:with_hint(
        Err1,
        <<"Use Beamtalk allClasses for available classes.">>
    ).

-doc "Build a structured error for a method not found.".
-spec make_method_not_found_error(atom(), atom()) -> #beamtalk_error{}.
make_method_not_found_error(ClassName, Selector) ->
    NameBin = atom_to_binary(ClassName, utf8),
    SelBin = atom_to_binary(Selector, utf8),
    Err0 = beamtalk_error:new(does_not_understand, ClassName),
    Err1 = beamtalk_error:with_selector(Err0, Selector),
    Err2 = beamtalk_error:with_message(
        Err1,
        iolist_to_binary([NameBin, <<" does not understand ">>, SelBin])
    ),
    beamtalk_error:with_hint(
        Err2,
        iolist_to_binary([<<"Use Beamtalk help: ">>, NameBin, <<" to see available methods.">>])
    ).
