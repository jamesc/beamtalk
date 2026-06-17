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

-export([
    make/3,
    make/4,
    ensure_structured_error/1,
    ensure_structured_error/2,
    format_name/1,
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
Handles known bare tuple error patterns from the compile pipeline.
Otherwise wraps the raw term in an internal_error.
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
ensure_structured_error({eval_error, _Class, Reason}) ->
    %% Delegate to /1 for known tuple patterns; fall back to generic wrapper.
    ensure_structured_error(Reason);
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

-doc "Format a name for error messages.".
-spec format_name(term()) -> binary().
format_name(Name) when is_atom(Name) -> atom_to_binary(Name, utf8);
format_name(Name) when is_binary(Name) -> Name;
format_name(Name) when is_list(Name) -> list_to_binary(Name);
format_name(Name) -> iolist_to_binary(io_lib:format("~p", [Name])).
