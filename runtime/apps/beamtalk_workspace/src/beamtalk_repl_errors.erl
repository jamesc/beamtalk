%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Error utilities for the Beamtalk REPL.
%%%
%%% **DDD Context:** REPL
%%%
%%% Provides helpers for wrapping raw error terms into structured
%%% #beamtalk_error{} records, safe atom conversion, and name formatting.
%%% Used by protocol handlers and op modules.

-module(beamtalk_repl_errors).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([
    ensure_structured_error/1,
    ensure_structured_error/2,
    format_name/1,
    safe_to_existing_atom/1
]).

%%% Public API

%% @doc Safely convert a binary to an existing atom, returning error instead of creating new atoms.
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

%% @doc Ensure an error reason is a structured #beamtalk_error{} record.
%% If already structured (or a wrapped exception), passes through unchanged.
%% Handles known bare tuple error patterns from the compile pipeline.
%% Otherwise wraps the raw term in an internal_error.
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
ensure_structured_error({compile_error, Msg}) when is_binary(Msg) ->
    Err0 = beamtalk_error:new(compile_error, 'Compiler'),
    beamtalk_error:with_message(Err0, Msg);
ensure_structured_error({compile_error, Msg}) when is_list(Msg) ->
    Err0 = beamtalk_error:new(compile_error, 'Compiler'),
    beamtalk_error:with_message(Err0, list_to_binary(Msg));
ensure_structured_error({compile_error, Reason}) ->
    Err0 = beamtalk_error:new(compile_error, 'Compiler'),
    beamtalk_error:with_message(
        Err0,
        iolist_to_binary([<<"Compile error: ">>, format_name(Reason)])
    );
ensure_structured_error({undefined_variable, Name}) ->
    Err0 = beamtalk_error:new(undefined_variable, 'REPL'),
    beamtalk_error:with_message(
        Err0,
        iolist_to_binary([<<"Undefined variable: ">>, format_name(Name)])
    );
ensure_structured_error({file_not_found, Path}) ->
    Err0 = beamtalk_error:new(file_not_found, 'File'),
    beamtalk_error:with_message(
        Err0,
        iolist_to_binary([<<"File not found: ">>, format_name(Path)])
    );
ensure_structured_error({read_error, Reason}) ->
    Err0 = beamtalk_error:new(io_error, 'File'),
    beamtalk_error:with_message(
        Err0,
        iolist_to_binary([<<"Failed to read file: ">>, format_name(Reason)])
    );
ensure_structured_error({load_error, Reason}) ->
    Err0 = beamtalk_error:new(io_error, 'File'),
    beamtalk_error:with_message(
        Err0,
        iolist_to_binary([<<"Failed to load bytecode: ">>, format_name(Reason)])
    );
ensure_structured_error({parse_error, Details}) ->
    Err0 = beamtalk_error:new(compile_error, 'Compiler'),
    beamtalk_error:with_message(
        Err0,
        iolist_to_binary([<<"Parse error: ">>, format_name(Details)])
    );
ensure_structured_error({invalid_request, Reason}) ->
    Err0 = beamtalk_error:new(internal_error, 'REPL'),
    beamtalk_error:with_message(
        Err0,
        iolist_to_binary([<<"Invalid request: ">>, format_name(Reason)])
    );
ensure_structured_error(empty_expression) ->
    Err0 = beamtalk_error:new(empty_expression, 'REPL'),
    beamtalk_error:with_message(Err0, <<"Empty expression">>);
ensure_structured_error(timeout) ->
    Err0 = beamtalk_error:new(timeout, 'REPL'),
    beamtalk_error:with_message(Err0, <<"Request timed out">>);
ensure_structured_error(Reason) ->
    Err0 = beamtalk_error:new(internal_error, 'REPL'),
    beamtalk_error:with_message(
        Err0,
        iolist_to_binary(io_lib:format("~p", [Reason]))
    ).

%% @doc Ensure an error reason is structured, with exception class context.
%% Delegates known tuple patterns to ensure_structured_error/1 to preserve
%% specific error kinds, only falling back to generic wrapper for unknown terms.
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
ensure_structured_error(Reason, Class) ->
    Err0 = beamtalk_error:new(internal_error, 'REPL'),
    beamtalk_error:with_message(
        Err0,
        iolist_to_binary([
            atom_to_binary(Class, utf8),
            <<": ">>,
            io_lib:format("~p", [Reason])
        ])
    ).

%% @doc Format a name for error messages.
-spec format_name(term()) -> binary().
format_name(Name) when is_atom(Name) -> atom_to_binary(Name, utf8);
format_name(Name) when is_binary(Name) -> Name;
format_name(Name) when is_list(Name) -> list_to_binary(Name);
format_name(Name) -> iolist_to_binary(io_lib:format("~p", [Name])).
