%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc StackFrame wrapper for post-exception introspection (BT-107).
%%%
%%% Converts Erlang stacktrace entries into first-class Beamtalk StackFrame
%%% value objects (tagged maps). Erlang stacktrace entries have the form:
%%%   {Module, Function, ArityOrArgs, Location}
%%% where Location is [{file, File}, {line, Line}] or [].
%%%
%%% **DDD Context:** Runtime — Error Handling
%%%
%%% StackFrame objects are value types (tagged maps) with fields:
%%% ```
%%% #{
%%%   '$beamtalk_class' => 'StackFrame',
%%%   module => atom(),          % Erlang module name
%%%   function => atom(),        % Erlang function name
%%%   arity => non_neg_integer(),% Function arity
%%%   file => binary() | nil,    % Source file path
%%%   line => non_neg_integer() | nil, % Source line number
%%%   class_name => atom() | nil % Beamtalk class name (if mappable)
%%% }
%%% ```

-module(beamtalk_stack_frame).

-export([
    wrap/1,
    dispatch/3,
    has_method/1
]).

%% @doc Convert an Erlang stacktrace (list of tuples) to a list of StackFrame objects.
-spec wrap(list()) -> list().
wrap(Stacktrace) when is_list(Stacktrace) ->
    [wrap_frame(Frame) || Frame <- Stacktrace];
wrap(_) ->
    [].

%% @private Convert a single Erlang stacktrace entry to a StackFrame tagged map.
-spec wrap_frame(tuple()) -> map().
wrap_frame({Module, Function, ArityOrArgs, Location}) ->
    Arity = case is_list(ArityOrArgs) of
        true -> length(ArityOrArgs);
        false -> ArityOrArgs
    end,
    File = case proplists:get_value(file, Location) of
        undefined -> nil;
        F -> list_to_binary(F)
    end,
    Line = case proplists:get_value(line, Location) of
        undefined -> nil;
        L -> L
    end,
    ClassName = module_to_class(Module),
    #{
        '$beamtalk_class' => 'StackFrame',
        module => Module,
        function => Function,
        arity => Arity,
        file => File,
        line => Line,
        class_name => ClassName
    };
wrap_frame(_Other) ->
    #{
        '$beamtalk_class' => 'StackFrame',
        module => undefined,
        function => undefined,
        arity => 0,
        file => nil,
        line => nil,
        class_name => nil
    }.

%% @doc Map Erlang module name to Beamtalk class name.
%%
%% Handles compiled module naming conventions:
%%   - 'counter' → 'Counter' (user classes)
%%   - 'bt@stdlib@integer' → 'Integer' (stdlib classes)
%%   - 'bt@integer' → 'Integer' (stdlib alt format)
%%   - 'beamtalk_integer' → 'Integer' (runtime primitives)
%%   - Other modules → nil (not a Beamtalk class)
-spec module_to_class(atom()) -> atom() | nil.
module_to_class(Module) when is_atom(Module) ->
    ModStr = atom_to_list(Module),
    case ModStr of
        "bt@stdlib@" ++ Rest ->
            snake_to_class(Rest);
        "bt@" ++ Rest ->
            snake_to_class(Rest);
        "beamtalk_" ++ Rest ->
            %% Runtime primitive modules like beamtalk_integer, beamtalk_string
            snake_to_class(Rest);
        _ ->
            %% Could be a user class compiled as snake_case module
            %% Try to look it up in the class registry
            ClassName = snake_to_class(ModStr),
            case beamtalk_class_registry:is_class_name(ClassName) of
                true -> ClassName;
                false -> nil
            end
    end;
module_to_class(_) ->
    nil.

%% @private Convert snake_case module name to CamelCase class name atom.
%% Uses list_to_existing_atom to avoid atom table growth from arbitrary module names.
-spec snake_to_class(string()) -> atom() | nil.
snake_to_class(Snake) ->
    Words = string:split(Snake, "_", all),
    Capitalized = [capitalize(W) || W <- Words],
    try list_to_existing_atom(lists:flatten(Capitalized))
    catch error:badarg -> nil
    end.

%% @private Capitalize first letter of a string.
-spec capitalize(string()) -> string().
capitalize([]) -> [];
capitalize([H | T]) when H >= $a, H =< $z -> [H - 32 | T];
capitalize(Str) -> Str.

%% @doc Dispatch a message to a StackFrame object.
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
        {nil, _} -> nil;
        {_, nil} -> nil;
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
    format_frame(Frame);
dispatch('describe', [], Frame) ->
    format_frame(Frame);
dispatch(Selector, _Args, _Frame) ->
    Error0 = beamtalk_error:new(does_not_understand, 'StackFrame'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    beamtalk_error:raise(Error1).

%% @doc Check if StackFrame responds to a selector.
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
has_method('describe') -> true;
has_method(_) -> false.

%% @private Format a StackFrame as a human-readable string.
-spec format_frame(map()) -> binary().
format_frame(#{class_name := ClassName, function := Function, arity := Arity,
               file := File, line := Line}) ->
    ClassPart = case ClassName of
        nil -> <<"?">>;
        _ -> atom_to_binary(ClassName, utf8)
    end,
    FunPart = case Function of
        undefined -> <<"?">>;
        _ -> atom_to_binary(Function, utf8)
    end,
    ArityBin = integer_to_binary(Arity),
    LocationPart = case {File, Line} of
        {nil, _} -> <<>>;
        {_, nil} -> <<" (", File/binary, ")">>;
        _ ->
            LineBin = integer_to_binary(Line),
            <<" (", File/binary, ":", LineBin/binary, ")">>
    end,
    <<ClassPart/binary, ">>", FunPart/binary, "/", ArityBin/binary, LocationPart/binary>>.
