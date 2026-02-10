%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Standard library initialization for Beamtalk.
%%%
%%% This module registers all primitive classes (Integer, String, Boolean, etc.)
%%% and the SystemDictionary/TranscriptStream classes in the class registry at runtime startup.
%%%
%%% ## Classes Registered
%%%
%%% | Class | Superclass | Dispatch Module |
%%% |-------|-----------|-----------------|
%%% | Number | Object | (abstract) |
%%% | Integer | Number | beamtalk_integer |
%%% | String | Object | beamtalk_string |
%%% | True | Object | beamtalk_true |
%%% | False | Object | beamtalk_false |
%%% | Float | Number | beamtalk_float |
%%% | UndefinedObject | Object | beamtalk_undefined_object |
%%% | Block | Object | beamtalk_block |
%%% | Tuple | Object | beamtalk_tuple |
%%% | SystemDictionary | Actor | beamtalk_system_dictionary |
%%% | TranscriptStream | Actor | beamtalk_transcript_stream |
%%% | File | Object | beamtalk_file |
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
    logger:info("Registering primitive classes"),
    PrimitiveResults = [
        register_number_class(),
        register_integer_class(),
        register_string_class(),
        register_true_class(),
        register_false_class(),
        register_nil_class(),
        register_block_class(),
        register_tuple_class(),
        register_float_class(),
        register_file_class()
    ],
    %% BT-411: Load compiled stdlib modules that register via on_load.
    %% Modules compiled from lib/*.bt (by build-stdlib) self-register through
    %% on_load → register_class. Since the beamtalk_stdlib OTP app isn't
    %% started as a dependency, we explicitly load them here.
    load_compiled_stdlib_modules(),
    %% Register classes for workspace globals (singletons like Beamtalk, Transcript).
    %% Each module exports class_info/0 as the single source of truth for its metadata.
    GlobalResults = register_workspace_globals(),
    Results = PrimitiveResults ++ GlobalResults,
    %% Log any failures but don't crash
    lists:foreach(fun
        ({ok, ClassName}) -> 
            logger:debug("Registered stdlib class", #{class => ClassName});
        ({error, ClassName, Reason}) -> 
            logger:warning("Failed to register stdlib class", #{class => ClassName, reason => Reason})
    end, Results),
    ok.

%% @private
-spec stdlib_loop() -> no_return().
stdlib_loop() ->
    receive
        _Any -> stdlib_loop()
    end.

%%% ============================================================================
%%% Number Class (BT-334) — Abstract numeric superclass
%%% ============================================================================

-spec register_number_class() -> {ok, atom()} | {error, atom(), term()}.
register_number_class() ->
    ClassInfo = #{
        name => 'Number',
        module => bt_stdlib_number,
        superclass => 'Object',
        is_abstract => true,
        instance_methods => #{
            isZero => #{arity => 0},
            isPositive => #{arity => 0},
            isNegative => #{arity => 0},
            sign => #{arity => 0},
            'between:and:' => #{arity => 2}
        },
        class_methods => #{},
        instance_variables => []
    },
    register_class('Number', ClassInfo).

%%% ============================================================================
%%% Integer Class
%%% ============================================================================

-spec register_integer_class() -> {ok, atom()} | {error, atom(), term()}.
register_integer_class() ->
    ClassInfo = #{
        name => 'Integer',
        module => beamtalk_integer,
        superclass => 'Number',
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
            asFloat => #{arity => 0},
            abs => #{arity => 0},
            negated => #{arity => 0},
            '%' => #{arity => 1},
            '**' => #{arity => 1},
            isEven => #{arity => 0},
            isOdd => #{arity => 0},
            'min:' => #{arity => 1},
            'max:' => #{arity => 1},
            'timesRepeat:' => #{arity => 1},
            'to:do:' => #{arity => 2},
            'to:by:do:' => #{arity => 3},
            describe => #{arity => 0}
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
%%% True Class
%%% ============================================================================

-spec register_true_class() -> {ok, atom()} | {error, atom(), term()}.
register_true_class() ->
    ClassInfo = #{
        name => 'True',
        module => beamtalk_true,
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
            describe => #{arity => 0},
            isTrue => #{arity => 0},
            isFalse => #{arity => 0},
            asString => #{arity => 0}
        },
        class_methods => #{},
        instance_variables => []
    },
    register_class('True', ClassInfo).

%%% ============================================================================
%%% False Class
%%% ============================================================================

-spec register_false_class() -> {ok, atom()} | {error, atom(), term()}.
register_false_class() ->
    ClassInfo = #{
        name => 'False',
        module => beamtalk_false,
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
            describe => #{arity => 0},
            isTrue => #{arity => 0},
            isFalse => #{arity => 0},
            asString => #{arity => 0}
        },
        class_methods => #{},
        instance_variables => []
    },
    register_class('False', ClassInfo).

%%% ============================================================================
%%% UndefinedObject Class
%%% ============================================================================

-spec register_nil_class() -> {ok, atom()} | {error, atom(), term()}.
register_nil_class() ->
    ClassInfo = #{
        name => 'UndefinedObject',
        module => beamtalk_undefined_object,
        superclass => 'Object',
        instance_methods => #{
            class => #{arity => 0},
            'respondsTo:' => #{arity => 1},
            isNil => #{arity => 0},
            notNil => #{arity => 0},
            'ifNil:' => #{arity => 1},
            'ifNotNil:' => #{arity => 1},
            'ifNil:ifNotNil:' => #{arity => 2},
            'ifNotNil:ifNil:' => #{arity => 2},
            copy => #{arity => 0},
            deepCopy => #{arity => 0},
            shallowCopy => #{arity => 0},
            describe => #{arity => 0},
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
%%% Float Class (BT-340)
%%% ============================================================================

-spec register_float_class() -> {ok, atom()} | {error, atom(), term()}.
register_float_class() ->
    ClassInfo = #{
        name => 'Float',
        module => beamtalk_float,
        superclass => 'Number',
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
            asInteger => #{arity => 0},
            abs => #{arity => 0},
            negated => #{arity => 0},
            'min:' => #{arity => 1},
            'max:' => #{arity => 1},
            rounded => #{arity => 0},
            ceiling => #{arity => 0},
            floor => #{arity => 0},
            truncated => #{arity => 0},
            isNaN => #{arity => 0},
            isInfinite => #{arity => 0},
            isZero => #{arity => 0},
            describe => #{arity => 0}
        },
        class_methods => #{},
        instance_variables => []
    },
    register_class('Float', ClassInfo).

%%% ============================================================================
%%% File Class (BT-336)
%%% ============================================================================

-spec register_file_class() -> {ok, atom()} | {error, atom(), term()}.
register_file_class() ->
    ClassInfo = #{
        name => 'File',
        module => beamtalk_file,
        superclass => 'Object',
        %% BT-411: File methods are instance methods (called via dispatch),
        %% not class methods. The original version had these swapped.
        instance_methods => #{
            'exists:' => #{arity => 1},
            'readAll:' => #{arity => 1},
            'writeAll:contents:' => #{arity => 2}
        },
        class_methods => #{},
        instance_variables => []
    },
    register_class('File', ClassInfo).

%%% ============================================================================
%%% Compiled Stdlib Module Loading (BT-411)
%%% ============================================================================

%% @doc Load compiled stdlib modules that register via on_load.
%%
%% All compiled stdlib modules (from lib/*.bt via build-stdlib) register
%% their class processes via on_load → register_class/0. Since the
%% beamtalk_stdlib OTP app isn't a dependency of beamtalk_runtime, we
%% explicitly load every compiled module here.
%%
%% For classes that also have hand-written register_*_class() above
%% (Integer, Float, etc.), the on_load register_class will harmlessly
%% get {already_started, _}.
-spec load_compiled_stdlib_modules() -> ok.
load_compiled_stdlib_modules() ->
    %% Primitive types (beamtalk_* prefix — replace hand-written dispatch modules)
    PrimitiveModules = [
        beamtalk_block,
        beamtalk_dictionary,
        beamtalk_false,
        beamtalk_float,
        beamtalk_integer,
        beamtalk_list,
        beamtalk_set,
        beamtalk_string,
        beamtalk_symbol,
        beamtalk_true,
        beamtalk_tuple,
        beamtalk_undefined_object
    ],
    %% Non-primitive types (bt_stdlib_* prefix)
    StdlibModules = [
        bt_stdlib_actor,
        bt_stdlib_association,
        bt_stdlib_error,
        bt_stdlib_exception,
        bt_stdlib_file,
        bt_stdlib_number,
        bt_stdlib_object,
        bt_stdlib_proto_object,
        bt_stdlib_system_dictionary,
        bt_stdlib_transcript_stream
    ],
    lists:foreach(fun(Mod) ->
        case code:ensure_loaded(Mod) of
            {module, Mod} ->
                logger:debug("Loaded compiled stdlib module", #{module => Mod});
            {error, Reason} ->
                logger:warning("Failed to load compiled stdlib module",
                               #{module => Mod, reason => Reason})
        end
    end, PrimitiveModules ++ StdlibModules).

%%% ============================================================================
%%% Helper Functions
%%% ============================================================================

%% Workspace global singleton modules whose classes need registering.
%% Each module exports class_info/0 as the single source of truth for its metadata.
%% To add a new workspace global: implement class_info/0 in the module and add it here.
-define(WORKSPACE_GLOBALS, [
    beamtalk_system_dictionary,
    beamtalk_transcript_stream
]).

%% @doc Register classes for all workspace global singletons.
-spec register_workspace_globals() -> [{ok, atom()} | {error, atom(), term()}].
register_workspace_globals() ->
    [begin
        ClassInfo = Mod:class_info(),
        ClassName = maps:get(name, ClassInfo),
        register_class(ClassName, ClassInfo)
     end || Mod <- ?WORKSPACE_GLOBALS].

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
    %% Look up a class by name, return wrapped class object or nil
    case beamtalk_object_class:whereis_class(ClassName) of
        undefined -> nil;
        Pid -> {beamtalk_object, ClassName, beamtalk_object_class, Pid}
    end;

dispatch(globals, [], _Receiver) ->
    %% Placeholder - global namespace not yet implemented
    #{};

dispatch(version, [], _Receiver) ->
    %% Return Beamtalk version
    <<"0.1.0">>;

dispatch(Selector, _Args, _Receiver) ->
    %% Unknown method
    Error0 = beamtalk_error:new(does_not_understand, 'Beamtalk'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(Error1, <<"Check spelling or use 'Beamtalk respondsTo:' to verify method exists">>),
    error(Error2).

%% @doc Check if the Beamtalk class responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method(allClasses) -> true;
has_method('classNamed:') -> true;
has_method(globals) -> true;
has_method(version) -> true;
has_method(_) -> false.
