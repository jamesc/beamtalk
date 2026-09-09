%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_errors).

%%% **DDD Context:** REPL Session Context

-moduledoc """
Error utilities for the Beamtalk REPL.

Provides helpers for wrapping raw error terms into structured
#beamtalk_error{} records, safe atom conversion, and name formatting.
Used by protocol handlers and op modules.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% BT-3084: {Tag, Arity} pairs recognized by ensure_structured_error/1's
%% specific clauses (excluding the map/#beamtalk_error{}/eval_error wrapper
%% shapes handled by earlier, more specific clauses). Shared by
%% is_known_error_reason/1 so a `{eval_error, Class, Reason}` whose Reason is
%% itself one of these keeps its specific structured kind instead of
%% collapsing into the generic "Evaluation error: Class:Reason" wrapper.
%%
%% Matched as {Tag, Arity} rather than bare Tag: every tag here happens to map
%% to exactly one arity today, but a same-named tag at a different arity
%% elsewhere in the codebase (e.g. a hypothetical unrelated 2-arity
%% `{method_not_found, Reason}`) must NOT be misidentified as "known" just
%% because the atom matches — that would silently route it through
%% is_known_error_reason into ensure_structured_error/1's generic catch-all
%% clause (a degraded `~p` dump), dropping the eval_error `Class:` context,
%% instead of the intended generic "Evaluation error: Class:Reason" wrapper.
-define(KNOWN_ERROR_TUPLE_TAGS, [
    {compile_error, 2},
    {undefined_variable, 2},
    {file_not_found, 2},
    {read_error, 2},
    {load_error, 2},
    {registration_error, 2},
    {parse_error, 2},
    {invalid_request, 2},
    {module_not_found, 2},
    {invalid_module_name, 2},
    {actors_exist, 3},
    {class_not_found, 2},
    {method_not_found, 3},
    {unknown_op, 2},
    {inspect_failed, 2},
    {actor_not_alive, 2},
    {no_source_file, 2},
    {module_not_loaded, 2},
    {missing_module_name, 2},
    {session_creation_failed, 2}
]).

-export([
    make/3,
    make/4,
    ensure_structured_error/1,
    ensure_structured_error/2,
    format_name/1,
    normalize_diagnostic/1,
    safe_to_existing_atom/1
]).

%%% Public API

-doc """
Build a structured #beamtalk_error{} from a kind, source class, and message.

Collapses the repeated `new` -> `with_message` chain used across the REPL
op modules.
""".
-spec make(atom(), atom(), term()) -> #beamtalk_error{}.
make(Kind, Source, Message) ->
    beamtalk_error:with_message(beamtalk_error:new(Kind, Source), Message).

-doc """
Build a structured #beamtalk_error{} from a kind, source class, message, and hint.

Collapses the repeated `new` -> `with_message` -> `with_hint` chain used across
the REPL op modules. A hint of `undefined` is dropped by `beamtalk_error:with_hint/2`.
""".
-spec make(atom(), atom(), term(), term()) -> #beamtalk_error{}.
make(Kind, Source, Message, Hint) ->
    beamtalk_error:with_hint(make(Kind, Source, Message), Hint).

-doc """
Safely convert a binary to an existing atom, returning error instead of creating new atoms.
""".
-spec safe_to_existing_atom(binary()) -> {ok, atom()} | {error, badarg}.
safe_to_existing_atom(<<>>) ->
    {error, badarg};
safe_to_existing_atom(Bin) when is_binary(Bin) ->
    try binary_to_existing_atom(Bin, utf8) of
        Atom -> {ok, Atom}
    catch
        error:badarg -> {error, badarg}
    end;
safe_to_existing_atom(_) ->
    {error, badarg}.

-doc """
Ensure an error reason is a structured #beamtalk_error{} record.
If already structured (or a wrapped exception), passes through unchanged.

BT-3084: this is the single canonical dispatch table for the REPL's raw
error-tuple vocabulary — compile/eval failures, undefined variables, file and
module I/O, class/method lookups, actor ops, and session/request errors.
`beamtalk_repl_json:format_error_message/1` derives from this table rather
than maintaining a second one, so a tuple handled here is guaranteed to also
render correctly there. Otherwise wraps the raw term in an internal_error.
""".
-spec ensure_structured_error(term()) -> #beamtalk_error{}.
ensure_structured_error(#beamtalk_error{} = Err) ->
    Err;
ensure_structured_error(#{'$beamtalk_class' := _, error := #beamtalk_error{} = Err}) ->
    Err;
ensure_structured_error(
    {eval_error, _Class, #{'$beamtalk_class' := _, error := #beamtalk_error{} = Err}}
) ->
    Err;
ensure_structured_error({eval_error, _Class, #beamtalk_error{} = Err}) ->
    Err;
ensure_structured_error({eval_error, Class, Reason}) ->
    case is_known_error_reason(Reason) of
        true ->
            %% Reason is itself one of the recognized raw-error-tuple shapes
            %% (or empty_expression/timeout) — delegate so it keeps its
            %% specific structured kind/message instead of flattening into
            %% the generic wrapper below.
            ensure_structured_error(Reason);
        false ->
            %% BT-3084: opaque/unrecognized Reason — preserve the exception
            %% class in the message. Previously this clause dropped `Class`
            %% entirely, which diverged from beamtalk_repl_json's separate
            %% "Evaluation error: Class:Reason" wording for the same shape;
            %% unify on that wording here so both callers agree.
            make(
                internal_error,
                'REPL',
                iolist_to_binary([
                    <<"Evaluation error: ">>,
                    atom_to_binary(Class, utf8),
                    <<":">>,
                    format_name(Reason)
                ])
            )
    end;
ensure_structured_error({compile_error, [#{message := Msg} = Diag | _]}) ->
    %% BT-1235: structured diagnostic list — extract message and hint from first diagnostic
    % elp:fixme W0032 maps:find with complex branch logic
    case maps:find(hint, Diag) of
        {ok, Hint} when is_binary(Hint) -> make(compile_error, 'Compiler', Msg, Hint);
        _ -> make(compile_error, 'Compiler', Msg)
    end;
ensure_structured_error({compile_error, Msg}) when is_binary(Msg) ->
    make(compile_error, 'Compiler', Msg);
ensure_structured_error({compile_error, Msg}) when is_list(Msg) ->
    MsgBin =
        try
            list_to_binary(Msg)
        catch
            error:badarg -> iolist_to_binary(io_lib:format("~p", [Msg]))
        end,
    make(compile_error, 'Compiler', MsgBin);
ensure_structured_error({compile_error, Reason}) ->
    make(compile_error, 'Compiler', iolist_to_binary([<<"Compile error: ">>, format_name(Reason)]));
ensure_structured_error({undefined_variable, Name}) ->
    make(
        undefined_variable,
        'REPL',
        iolist_to_binary([<<"Undefined variable: ">>, format_name(Name)])
    );
ensure_structured_error({file_not_found, Path}) ->
    make(file_not_found, 'File', iolist_to_binary([<<"File not found: ">>, format_name(Path)]));
ensure_structured_error({read_error, Reason}) ->
    make(io_error, 'File', iolist_to_binary([<<"Failed to read file: ">>, format_name(Reason)]));
ensure_structured_error({load_error, Reason}) ->
    make(
        io_error,
        'File',
        iolist_to_binary([<<"Failed to load bytecode: ">>, format_name(Reason)])
    );
ensure_structured_error({registration_error, {ModuleName, Reason}}) ->
    make(
        registration_error,
        'Runtime',
        iolist_to_binary(
            io_lib:format("Class registration failed for ~s: ~p", [ModuleName, Reason])
        )
    );
ensure_structured_error({registration_error, Reason}) ->
    make(
        registration_error,
        'Runtime',
        iolist_to_binary([<<"Class registration failed: ">>, format_name(Reason)])
    );
ensure_structured_error({parse_error, Details}) ->
    make(compile_error, 'Compiler', iolist_to_binary([<<"Parse error: ">>, format_name(Details)]));
ensure_structured_error({invalid_request, Reason}) ->
    make(internal_error, 'REPL', iolist_to_binary([<<"Invalid request: ">>, format_name(Reason)]));
%% BT-3084: the remaining clauses below were previously only handled by
%% beamtalk_repl_json:format_error_message/1's separate dispatch table —
%% absent here, they fell through to the generic `~p` wrapper just below
%% (silently dropping the vocabulary, e.g. `{registration_error, ...}` was
%% the reverse case: handled here but absent from the JSON table). Folding
%% them into this one canonical table closes both gaps.
ensure_structured_error({module_not_found, ModuleName}) ->
    make(
        module_not_found,
        'Module',
        iolist_to_binary([<<"Module not loaded: ">>, format_name(ModuleName)])
    );
ensure_structured_error({invalid_module_name, ModuleName}) ->
    make(
        invalid_module_name,
        'Module',
        iolist_to_binary([<<"Invalid module name: ">>, format_name(ModuleName)])
    );
ensure_structured_error({actors_exist, ModuleName, Count}) ->
    ActorWord =
        case Count of
            1 -> <<"actor">>;
            _ -> <<"actors">>
        end,
    make(
        actors_exist,
        'Module',
        iolist_to_binary([
            <<"Cannot unload ">>,
            format_name(ModuleName),
            <<": ">>,
            integer_to_binary(Count),
            <<" ">>,
            ActorWord,
            <<" still running. Kill them first with :kill">>
        ])
    );
ensure_structured_error({class_not_found, ClassName}) ->
    make(
        class_not_found,
        'REPL',
        iolist_to_binary([
            <<"Unknown class: ">>,
            format_name(ClassName),
            <<". Use Workspace classes to see loaded classes.">>
        ])
    );
ensure_structured_error({method_not_found, ClassName, Selector}) ->
    %% BT-3084: canonical DNU message — call beamtalk_error:generate_message/3
    %% (via with_selector/2 when Selector is an atom) instead of hand-rolling
    %% the "does not understand" text a third time.
    Err0 = beamtalk_error:new(does_not_understand, ClassName),
    Err1 =
        case Selector of
            S when is_atom(S) ->
                beamtalk_error:with_selector(Err0, S);
            _ ->
                %% Selector arrived as a binary (e.g. no existing atom for it,
                %% so the caller couldn't safely mint one) — still render via
                %% generate_message/3 rather than reimplementing the quoting.
                beamtalk_error:with_message(
                    Err0,
                    beamtalk_error:generate_message(does_not_understand, ClassName, Selector)
                )
        end,
    beamtalk_error:with_hint(
        Err1,
        iolist_to_binary([
            <<"Use :help ">>, format_name(ClassName), <<" to see available methods.">>
        ])
    );
ensure_structured_error({unknown_op, Op}) ->
    make(unknown_op, 'REPL', iolist_to_binary([<<"Unknown operation: ">>, format_name(Op)]));
ensure_structured_error({inspect_failed, PidStr}) ->
    make(
        inspect_failed,
        'Actor',
        iolist_to_binary([<<"Failed to inspect actor: ">>, format_name(PidStr)])
    );
ensure_structured_error({actor_not_alive, PidStr}) ->
    make(
        actor_not_alive,
        'Actor',
        iolist_to_binary([<<"Actor is not alive: ">>, format_name(PidStr)])
    );
ensure_structured_error({no_source_file, Module}) ->
    make(
        no_source_file,
        'Module',
        iolist_to_binary([
            <<"No source file recorded for module: ">>,
            format_name(Module),
            <<". Try :load <path> to load it first.">>
        ])
    );
ensure_structured_error({module_not_loaded, Module}) ->
    make(
        module_not_loaded,
        'Module',
        iolist_to_binary([
            <<"Module not loaded: ">>,
            format_name(Module),
            <<". Use :load <path> to load it first.">>
        ])
    );
ensure_structured_error({missing_module_name, reload}) ->
    make(
        missing_module_name,
        'REPL',
        <<"Usage: :reload <ModuleName> or :reload (to reload last file)">>
    );
ensure_structured_error({session_creation_failed, Reason}) ->
    make(
        session_creation_failed,
        'REPL',
        iolist_to_binary([<<"Failed to create session: ">>, format_name(Reason)])
    );
ensure_structured_error(empty_expression) ->
    make(empty_expression, 'REPL', <<"Empty expression">>);
ensure_structured_error(timeout) ->
    make(timeout, 'REPL', <<"Request timed out">>);
ensure_structured_error(Reason) ->
    make(internal_error, 'REPL', iolist_to_binary(io_lib:format("~p", [Reason]))).

-doc """
Ensure an error reason is structured, with exception class context.
Delegates known tuple patterns to ensure_structured_error/1 to preserve
specific error kinds, only falling back to generic wrapper for unknown terms.
""".
-spec ensure_structured_error(term(), atom()) -> #beamtalk_error{}.
ensure_structured_error(#beamtalk_error{} = Err, _Class) ->
    Err;
ensure_structured_error(#{'$beamtalk_class' := _, error := #beamtalk_error{} = Err}, _Class) ->
    Err;
ensure_structured_error({compile_error, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({eval_error, _, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({undefined_variable, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({file_not_found, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({read_error, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({load_error, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({parse_error, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({invalid_request, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({registration_error, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({module_not_found, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({invalid_module_name, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({actors_exist, _, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({class_not_found, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({method_not_found, _, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({unknown_op, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({inspect_failed, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({actor_not_alive, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({no_source_file, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({module_not_loaded, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({missing_module_name, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error({session_creation_failed, _} = Reason, _Class) ->
    ensure_structured_error(Reason);
ensure_structured_error(Reason, Class) ->
    make(
        internal_error,
        'REPL',
        iolist_to_binary([
            atom_to_binary(Class, utf8),
            <<": ">>,
            io_lib:format("~p", [Reason])
        ])
    ).

-doc """
Is this term one of the raw-error-tuple shapes ensure_structured_error/1
recognizes (or the bare `empty_expression`/`timeout` atoms)? Used to decide
whether a `{eval_error, Class, Reason}`'s Reason should delegate to /1
(keeping its specific kind) or fall back to the generic
"Evaluation error: Class:Reason" wrapper.
""".
-spec is_known_error_reason(term()) -> boolean().
is_known_error_reason(empty_expression) ->
    true;
is_known_error_reason(timeout) ->
    true;
is_known_error_reason(Reason) when is_tuple(Reason), tuple_size(Reason) > 0 ->
    lists:member({element(1, Reason), tuple_size(Reason)}, ?KNOWN_ERROR_TUPLE_TAGS);
is_known_error_reason(_) ->
    false.

%% BT-3090: delegates to the canonical `beamtalk_text:to_binary/1` — was a
%% byte-identical copy of `beamtalk_repl_protocol:to_binary/1`.
-doc "Format a name for error messages.".
-spec format_name(term()) -> binary().
format_name(Name) -> beamtalk_text:to_binary(Name).

-doc """
Normalise a compiler diagnostic term to a plain map with guaranteed keys.

Single authority for the 3-clause dispatch shared by `format_diagnostic_text/1`
(beamtalk_repl_compiler) and `diagnostic_to_error_map/2` (beamtalk_repl_ops_load):
both were re-implementing the same `is_map`/`is_binary`/fallback pattern, so if
the compiler changes the diagnostic shape (e.g. renames `line` to `line_number`)
both files would need parallel edits with no mechanical link.

Always returns `message`. Includes `line` (integer) and `hint` (binary) only when
present in the source map.
""".
-spec normalize_diagnostic(term()) ->
    #{message := binary(), line => integer(), hint => binary()}.
normalize_diagnostic(D) when is_map(D) ->
    Msg = maps:get(message, D, <<"Unknown error">>),
    Base = #{message => Msg},
    % elp:fixme W0032 maps:find with complex branch logic
    Base1 =
        case maps:find(line, D) of
            {ok, Line} when is_integer(Line) -> Base#{line => Line};
            _ -> Base
        end,
    % elp:fixme W0032 maps:find with complex branch logic
    case maps:find(hint, D) of
        {ok, Hint} when is_binary(Hint) -> Base1#{hint => Hint};
        _ -> Base1
    end;
normalize_diagnostic(D) when is_binary(D) ->
    #{message => D};
normalize_diagnostic(D) ->
    #{message => iolist_to_binary(io_lib:format("~p", [D]))}.
