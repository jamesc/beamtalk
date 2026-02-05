%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Standard library initialization for Beamtalk.
%%%
%%% This module registers all primitive classes (Integer, String, Boolean, etc.)
%%% and the Beamtalk global class in the class registry at runtime startup.
%%%
%%% ## Classes Registered
%%%
%%% | Class | Superclass | Dispatch Module |
%%% |-------|-----------|-----------------|
%%% | Integer | Object | beamtalk_integer |
%%% | String | Object | beamtalk_string |
%%% | Boolean | Object | beamtalk_boolean |
%%% | UndefinedObject | Object | beamtalk_nil |
%%% | Block | Object | beamtalk_block |
%%% | Tuple | Object | beamtalk_tuple |
%%% | Beamtalk | Object | beamtalk_stdlib |
%%%
%%% ## Usage
%%%
%%% Called automatically during runtime startup:
%%% ```erlang
%%% beamtalk_stdlib:init()
%%% ```
%%%
%%% After initialization, all classes appear in `Beamtalk allClasses`.
-module(beamtalk_stdlib).

-export([start_link/0, init/0, init/1]).

%% Beamtalk class dispatch - implements class methods for the Beamtalk global
-export([dispatch/3, has_method/1]).

%% @doc Start the stdlib initializer as a supervised process.
%%
%% This is called by the supervisor after beamtalk_bootstrap completes.
%% Uses proc_lib for proper OTP integration.
-spec start_link() -> {ok, pid()}.
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

%% @doc Initialize stdlib for manual/testing use.
%%
%% Call this after bootstrap is complete to register primitive classes.
%% For production use, prefer start_link/0 via supervisor.
-spec init() -> ok.
init() ->
    do_init(),
    ok.

%% @doc Initialize the standard library by registering all primitive classes.
%%
%% This function should be called after beamtalk_bootstrap has completed,
%% ensuring that ProtoObject, Object, and Actor classes exist.
%%
%% Returns ok on success, or logs warnings for any failed registrations.
-spec init(pid()) -> no_return().
init(Parent) ->
    do_init(),
    %% Signal parent that we're ready
    proc_lib:init_ack(Parent, {ok, self()}),
    %% Enter idle loop - this process stays alive but doesn't do anything
    stdlib_loop().

%% @private
%% Shared initialization logic
-spec do_init() -> ok.
do_init() ->
    io:format("beamtalk_stdlib: Registering primitive classes...~n"),
    Results = [
        register_integer_class(),
        register_string_class(),
        register_boolean_class(),
        register_nil_class(),
        register_block_class(),
        register_tuple_class(),
        register_beamtalk_class()
    ],
    %% Log any failures but don't crash
    lists:foreach(fun
        ({ok, ClassName}) -> 
            io:format("  Registered ~p~n", [ClassName]);
        ({error, ClassName, Reason}) -> 
            io:format("  WARNING: Failed to register ~p: ~p~n", [ClassName, Reason])
    end, Results),
    ok.

%% @private
-spec stdlib_loop() -> no_return().
stdlib_loop() ->
    receive
        _Any -> stdlib_loop()
    end.

%%% ============================================================================
%%% Integer Class
%%% ============================================================================

-spec register_integer_class() -> {ok, atom()} | {error, atom(), term()}.
register_integer_class() ->
    ClassInfo = #{
        name => 'Integer',
        module => beamtalk_integer,
        superclass => 'Object',
        instance_methods => #{
            '+' => #{arity => 1},
            '-' => #{arity => 1},
            '*' => #{arity => 1},
            '/' => #{arity => 1},
            '=' => #{arity => 1},
            '~=' => #{arity => 1},
            '<' => #{arity => 1},
            '>' => #{arity => 1},
            '<=' => #{arity => 1},
            '>=' => #{arity => 1},
            class => #{arity => 0},
            'respondsTo:' => #{arity => 1},
            asString => #{arity => 0},
            abs => #{arity => 0},
            negated => #{arity => 0}
        },
        class_methods => #{},
        instance_variables => []
    },
    register_class('Integer', ClassInfo).

%%% ============================================================================
%%% String Class
%%% ============================================================================

-spec register_string_class() -> {ok, atom()} | {error, atom(), term()}.
register_string_class() ->
    ClassInfo = #{
        name => 'String',
        module => beamtalk_string,
        superclass => 'Object',
        instance_methods => #{
            class => #{arity => 0},
            'respondsTo:' => #{arity => 1},
            size => #{arity => 0},
            length => #{arity => 0},
            isEmpty => #{arity => 0},
            uppercase => #{arity => 0},
            lowercase => #{arity => 0},
            trim => #{arity => 0},
            '++' => #{arity => 1},
            'concat:' => #{arity => 1},
            'at:' => #{arity => 1},
            'includes:' => #{arity => 1},
            'startsWith:' => #{arity => 1},
            'endsWith:' => #{arity => 1},
            'indexOf:' => #{arity => 1},
            'replace:with:' => #{arity => 2},
            'substring:to:' => #{arity => 2},
            'split:' => #{arity => 1},
            asInteger => #{arity => 0}
        },
        class_methods => #{},
        instance_variables => []
    },
    register_class('String', ClassInfo).

%%% ============================================================================
%%% Boolean Class
%%% ============================================================================

-spec register_boolean_class() -> {ok, atom()} | {error, atom(), term()}.
register_boolean_class() ->
    ClassInfo = #{
        name => 'Boolean',
        module => beamtalk_boolean,
        superclass => 'Object',
        instance_methods => #{
            class => #{arity => 0},
            'respondsTo:' => #{arity => 1},
            'ifTrue:' => #{arity => 1},
            'ifFalse:' => #{arity => 1},
            'ifTrue:ifFalse:' => #{arity => 2},
            'not' => #{arity => 0},
            'and:' => #{arity => 1},
            'or:' => #{arity => 1},
            asString => #{arity => 0}
        },
        class_methods => #{},
        instance_variables => []
    },
    register_class('Boolean', ClassInfo).

%%% ============================================================================
%%% UndefinedObject (Nil) Class
%%% ============================================================================

-spec register_nil_class() -> {ok, atom()} | {error, atom(), term()}.
register_nil_class() ->
    ClassInfo = #{
        name => 'UndefinedObject',
        module => beamtalk_nil,
        superclass => 'Object',
        instance_methods => #{
            class => #{arity => 0},
            'respondsTo:' => #{arity => 1},
            isNil => #{arity => 0},
            'ifNil:' => #{arity => 1},
            'ifNotNil:' => #{arity => 1},
            'ifNil:ifNotNil:' => #{arity => 2},
            asString => #{arity => 0}
        },
        class_methods => #{},
        instance_variables => []
    },
    register_class('UndefinedObject', ClassInfo).

%%% ============================================================================
%%% Block Class
%%% ============================================================================

-spec register_block_class() -> {ok, atom()} | {error, atom(), term()}.
register_block_class() ->
    ClassInfo = #{
        name => 'Block',
        module => beamtalk_block,
        superclass => 'Object',
        instance_methods => #{
            class => #{arity => 0},
            'respondsTo:' => #{arity => 1},
            value => #{arity => 0},
            'value:' => #{arity => 1},
            'value:value:' => #{arity => 2},
            arity => #{arity => 0},
            asString => #{arity => 0}
        },
        class_methods => #{},
        instance_variables => []
    },
    register_class('Block', ClassInfo).

%%% ============================================================================
%%% Tuple Class
%%% ============================================================================

-spec register_tuple_class() -> {ok, atom()} | {error, atom(), term()}.
register_tuple_class() ->
    ClassInfo = #{
        name => 'Tuple',
        module => beamtalk_tuple,
        superclass => 'Object',
        instance_methods => #{
            class => #{arity => 0},
            'respondsTo:' => #{arity => 1},
            size => #{arity => 0},
            'at:' => #{arity => 1},
            isOk => #{arity => 0},
            isError => #{arity => 0},
            unwrap => #{arity => 0},
            'unwrapOr:' => #{arity => 1},
            'unwrapOrElse:' => #{arity => 1},
            asString => #{arity => 0}
        },
        class_methods => #{},
        instance_variables => []
    },
    register_class('Tuple', ClassInfo).

%%% ============================================================================
%%% Beamtalk Global Class (System Reflection)
%%% ============================================================================

-spec register_beamtalk_class() -> {ok, atom()} | {error, atom(), term()}.
register_beamtalk_class() ->
    ClassInfo = #{
        name => 'Beamtalk',
        module => beamtalk_stdlib,  %% Methods implemented in this module
        superclass => 'Object',
        instance_methods => #{},
        class_methods => #{
            allClasses => #{arity => 0},
            'classNamed:' => #{arity => 1},
            globals => #{arity => 0},
            version => #{arity => 0}
        },
        instance_variables => []
    },
    register_class('Beamtalk', ClassInfo).

%%% ============================================================================
%%% Helper Functions
%%% ============================================================================

%% @doc Register a class with the class registry.
%% Returns {ok, ClassName} on success, {error, ClassName, Reason} on failure.
-spec register_class(atom(), map()) -> {ok, atom()} | {error, atom(), term()}.
register_class(ClassName, ClassInfo) ->
    case beamtalk_object_class:whereis_class(ClassName) of
        undefined ->
            %% Class doesn't exist, create it
            case beamtalk_object_class:start_link(ClassName, ClassInfo) of
                {ok, _Pid} ->
                    {ok, ClassName};
                {error, Reason} ->
                    {error, ClassName, Reason}
            end;
        _Pid ->
            %% Class already registered (e.g., from bootstrap)
            {ok, ClassName}
    end.

%%% ============================================================================
%%% Beamtalk Class Method Dispatch
%%% ============================================================================

%% @doc Dispatch a class method on the Beamtalk global class.
%%
%% The Beamtalk class provides system reflection methods:
%% - allClasses: Returns list of all registered class names
%% - classNamed: Look up a class by name (returns class pid or nil)
%% - globals: Returns global namespace (placeholder - returns empty map)
%% - version: Returns Beamtalk version string
-spec dispatch(atom(), list(), term()) -> term().
dispatch(allClasses, [], _Receiver) ->
    %% Return list of all registered class names
    Pids = beamtalk_object_class:all_classes(),
    [beamtalk_object_class:class_name(Pid) || Pid <- Pids];

dispatch('classNamed:', [ClassName], _Receiver) when is_atom(ClassName) ->
    %% Look up a class by name, return pid or nil
    case beamtalk_object_class:whereis_class(ClassName) of
        undefined -> nil;
        Pid -> Pid
    end;

dispatch(globals, [], _Receiver) ->
    %% Placeholder - global namespace not yet implemented
    #{};

dispatch(version, [], _Receiver) ->
    %% Return Beamtalk version
    <<"0.1.0">>;

dispatch(Selector, Args, _Receiver) ->
    %% Unknown method
    error({does_not_understand, 'Beamtalk', Selector, length(Args)}).

%% @doc Check if the Beamtalk class responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method(allClasses) -> true;
has_method('classNamed:') -> true;
has_method(globals) -> true;
has_method(version) -> true;
has_method(_) -> false.
