%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Runtime Context

%%% @doc EUnit tests for beamtalk_logging_config module.
%%%
%%% Tests log-level management, debug-target toggling, and logger
%%% introspection.

-module(beamtalk_logging_config_tests).
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

%%====================================================================
%% Setup / teardown
%%====================================================================

%% Restore the log level to its original value after every test group.
logging_config_test_() ->
    {setup,
        fun() ->
            OrigLevel = beamtalk_logging_config:logLevel(),
            beamtalk_logging_config:ensure_table(),
            OrigLevel
        end,
        fun(OrigLevel) ->
            beamtalk_logging_config:disableAllDebug(),
            beamtalk_logging_config:logLevel(OrigLevel)
        end,
        [
            fun logLevel_returns_atom/0,
            fun logLevel_set_valid_changes_level/0,
            fun logLevel_set_invalid_returns_error/0,
            fun logLevel_set_non_atom_returns_error/0,
            fun logFormat_returns_text_by_default/0,
            fun logFormat_set_json_changes_format/0,
            fun logFormat_set_text_restores_default/0,
            fun logFormat_invalid_returns_error/0,
            fun logFormat_non_atom_returns_error/0,
            fun debugTargets_returns_expected_list/0,
            fun enableDebug_valid_symbol_succeeds/0,
            fun enableDebug_invalid_symbol_returns_error/0,
            fun disableDebug_removes_target/0,
            fun disableAllDebug_clears_all/0,
            fun loggerInfo_returns_binary/0,
            fun activeDebugTargets_tracks_multiple/0,
            fun enable_disable_cycle_removes_target/0,
            fun enableDebug_domain_target_succeeds/0,
            fun disableDebug_domain_target_removes/0,
            fun enableDebug_class_reference_succeeds/0,
            fun disableDebug_class_reference_removes/0,
            fun enableDebug_actor_instance_succeeds/0,
            fun disableDebug_actor_instance_removes/0,
            fun activeDebugTargets_includes_class_and_actor/0,
            fun enableDebug_invalid_type_returns_error/0,
            fun disableAllDebug_clears_class_and_actor/0,
            fun loggerInfo_includes_class_and_actor/0
        ]}.

%%====================================================================
%% Tests
%%====================================================================

logLevel_returns_atom() ->
    Level = beamtalk_logging_config:logLevel(),
    ?assert(is_atom(Level)).

logLevel_set_valid_changes_level() ->
    OrigLevel = beamtalk_logging_config:logLevel(),
    %% Set to debug
    ?assertEqual(nil, beamtalk_logging_config:logLevel(debug)),
    ?assertEqual(debug, beamtalk_logging_config:logLevel()),
    %% Set to warning
    ?assertEqual(nil, beamtalk_logging_config:logLevel(warning)),
    ?assertEqual(warning, beamtalk_logging_config:logLevel()),
    %% Restore
    beamtalk_logging_config:logLevel(OrigLevel).

logLevel_set_invalid_returns_error() ->
    Result = beamtalk_logging_config:logLevel(banana),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

logLevel_set_non_atom_returns_error() ->
    Result = beamtalk_logging_config:logLevel(<<"debug">>),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

%%====================================================================
%% Log format tests
%%====================================================================

logFormat_returns_text_by_default() ->
    %% Without a beamtalk_file_log handler, the default should be text.
    ?assertEqual(text, beamtalk_logging_config:logFormat()).

logFormat_set_json_changes_format() ->
    %% Install a temporary file handler so we can test format switching.
    install_test_file_handler(),
    ?assertEqual(nil, beamtalk_logging_config:logFormat(json)),
    ?assertEqual(json, beamtalk_logging_config:logFormat()),
    remove_test_file_handler().

logFormat_set_text_restores_default() ->
    install_test_file_handler(),
    beamtalk_logging_config:logFormat(json),
    ?assertEqual(json, beamtalk_logging_config:logFormat()),
    ?assertEqual(nil, beamtalk_logging_config:logFormat(text)),
    ?assertEqual(text, beamtalk_logging_config:logFormat()),
    remove_test_file_handler().

logFormat_invalid_returns_error() ->
    Result = beamtalk_logging_config:logFormat(yaml),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

logFormat_non_atom_returns_error() ->
    Result = beamtalk_logging_config:logFormat(<<"json">>),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

debugTargets_returns_expected_list() ->
    Targets = beamtalk_logging_config:debugTargets(),
    ?assert(is_list(Targets)),
    ?assert(lists:member(actor, Targets)),
    ?assert(lists:member(supervisor, Targets)),
    ?assert(lists:member(dispatch, Targets)),
    ?assert(lists:member(compiler, Targets)),
    ?assert(lists:member(workspace, Targets)),
    ?assert(lists:member(stdlib, Targets)),
    ?assert(lists:member(hotreload, Targets)),
    ?assert(lists:member(runtime, Targets)),
    ?assert(lists:member(user, Targets)).

enableDebug_valid_symbol_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(actor)),
    Active = beamtalk_logging_config:activeDebugTargets(),
    ?assert(lists:member(actor, Active)),
    beamtalk_logging_config:disableDebug(actor).

enableDebug_invalid_symbol_returns_error() ->
    Result = beamtalk_logging_config:enableDebug(nonexistent),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

disableDebug_removes_target() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(dispatch),
    ?assert(lists:member(dispatch, beamtalk_logging_config:activeDebugTargets())),
    ?assertEqual(nil, beamtalk_logging_config:disableDebug(dispatch)),
    ?assertNot(lists:member(dispatch, beamtalk_logging_config:activeDebugTargets())).

disableAllDebug_clears_all() ->
    beamtalk_logging_config:enableDebug(actor),
    beamtalk_logging_config:enableDebug(dispatch),
    ?assert(length(beamtalk_logging_config:activeDebugTargets()) >= 2),
    ?assertEqual(nil, beamtalk_logging_config:disableAllDebug()),
    ?assertEqual([], beamtalk_logging_config:activeDebugTargets()).

loggerInfo_returns_binary() ->
    Info = beamtalk_logging_config:loggerInfo(),
    ?assert(is_binary(Info)),
    ?assertNotEqual(<<>>, Info).

activeDebugTargets_tracks_multiple() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(actor),
    beamtalk_logging_config:enableDebug(stdlib),
    Active = beamtalk_logging_config:activeDebugTargets(),
    ?assert(lists:member(actor, Active)),
    ?assert(lists:member(stdlib, Active)),
    beamtalk_logging_config:disableAllDebug().

enable_disable_cycle_removes_target() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(hotreload),
    ?assert(lists:member(hotreload, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(hotreload),
    ?assertNot(lists:member(hotreload, beamtalk_logging_config:activeDebugTargets())).

enableDebug_domain_target_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(runtime)),
    ?assert(lists:member(runtime, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(runtime).

disableDebug_domain_target_removes() ->
    beamtalk_logging_config:disableAllDebug(),
    beamtalk_logging_config:enableDebug(user),
    ?assert(lists:member(user, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(user),
    ?assertNot(lists:member(user, beamtalk_logging_config:activeDebugTargets())).

%%====================================================================
%% Class debug tests
%%====================================================================

enableDebug_class_reference_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    %% Create a class reference (class object): class name ends with " class"
    ClassRef = #beamtalk_object{class = 'TestClass class', class_mod = test_class, pid = self()},
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(ClassRef)),
    Active = beamtalk_logging_config:activeDebugTargets(),
    ?assert(lists:member({class, 'TestClass'}, Active)),
    beamtalk_logging_config:disableAllDebug().

disableDebug_class_reference_removes() ->
    beamtalk_logging_config:disableAllDebug(),
    ClassRef = #beamtalk_object{class = 'TestClass class', class_mod = test_class, pid = self()},
    beamtalk_logging_config:enableDebug(ClassRef),
    ?assert(lists:member({class, 'TestClass'}, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(ClassRef),
    ?assertNot(lists:member({class, 'TestClass'}, beamtalk_logging_config:activeDebugTargets())).

%%====================================================================
%% Actor debug tests
%%====================================================================

enableDebug_actor_instance_succeeds() ->
    beamtalk_logging_config:disableAllDebug(),
    %% Spawn a simple process to act as an actor
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ActorRef = #beamtalk_object{class = 'Counter', class_mod = counter, pid = Pid},
    ?assertEqual(nil, beamtalk_logging_config:enableDebug(ActorRef)),
    Active = beamtalk_logging_config:activeDebugTargets(),
    ?assert(lists:member({actor, Pid, 'Counter'}, Active)),
    Pid ! stop,
    beamtalk_logging_config:disableAllDebug().

disableDebug_actor_instance_removes() ->
    beamtalk_logging_config:disableAllDebug(),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ActorRef = #beamtalk_object{class = 'Counter', class_mod = counter, pid = Pid},
    beamtalk_logging_config:enableDebug(ActorRef),
    ?assert(lists:member({actor, Pid, 'Counter'}, beamtalk_logging_config:activeDebugTargets())),
    beamtalk_logging_config:disableDebug(ActorRef),
    ?assertNot(lists:member({actor, Pid, 'Counter'}, beamtalk_logging_config:activeDebugTargets())),
    Pid ! stop.

%%====================================================================
%% Mixed target tests
%%====================================================================

activeDebugTargets_includes_class_and_actor() ->
    beamtalk_logging_config:disableAllDebug(),
    %% Enable a subsystem, a class, and an actor
    beamtalk_logging_config:enableDebug(actor),
    ClassRef = #beamtalk_object{class = 'MyClass class', class_mod = my_class, pid = self()},
    beamtalk_logging_config:enableDebug(ClassRef),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ActorRef = #beamtalk_object{class = 'Counter', class_mod = counter, pid = Pid},
    beamtalk_logging_config:enableDebug(ActorRef),
    Active = beamtalk_logging_config:activeDebugTargets(),
    ?assert(lists:member(actor, Active)),
    ?assert(lists:member({class, 'MyClass'}, Active)),
    ?assert(lists:member({actor, Pid, 'Counter'}, Active)),
    Pid ! stop,
    beamtalk_logging_config:disableAllDebug().

enableDebug_invalid_type_returns_error() ->
    Result = beamtalk_logging_config:enableDebug(42),
    ?assertMatch(#beamtalk_error{kind = type_error}, Result).

disableAllDebug_clears_class_and_actor() ->
    beamtalk_logging_config:disableAllDebug(),
    ClassRef = #beamtalk_object{class = 'Foo class', class_mod = foo, pid = self()},
    beamtalk_logging_config:enableDebug(ClassRef),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ActorRef = #beamtalk_object{class = 'Bar', class_mod = bar, pid = Pid},
    beamtalk_logging_config:enableDebug(ActorRef),
    beamtalk_logging_config:enableDebug(dispatch),
    ?assert(length(beamtalk_logging_config:activeDebugTargets()) >= 3),
    ?assertEqual(nil, beamtalk_logging_config:disableAllDebug()),
    ?assertEqual([], beamtalk_logging_config:activeDebugTargets()),
    Pid ! stop.

loggerInfo_includes_class_and_actor() ->
    beamtalk_logging_config:disableAllDebug(),
    ClassRef = #beamtalk_object{class = 'Widget class', class_mod = widget, pid = self()},
    beamtalk_logging_config:enableDebug(ClassRef),
    Pid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    ActorRef = #beamtalk_object{class = 'Counter', class_mod = counter, pid = Pid},
    beamtalk_logging_config:enableDebug(ActorRef),
    Info = beamtalk_logging_config:loggerInfo(),
    ?assert(is_binary(Info)),
    %% Should mention the class and actor in the output
    ?assertNotEqual(nomatch, binary:match(Info, <<"Widget">>)),
    ?assertNotEqual(nomatch, binary:match(Info, <<"Counter">>)),
    Pid ! stop,
    beamtalk_logging_config:disableAllDebug().

%%====================================================================
%% Test helpers
%%====================================================================

%% @doc Install a temporary beamtalk_file_log handler for format-switching tests.
install_test_file_handler() ->
    _ = logger:remove_handler(beamtalk_file_log),
    logger:add_handler(beamtalk_file_log, logger_std_h, #{
        config => #{type => standard_io},
        level => debug,
        formatter =>
            {logger_formatter, #{
                template => [time, " [", level, "] ", msg, "\n"],
                single_line => true
            }}
    }).

%% @doc Remove the temporary beamtalk_file_log handler.
remove_test_file_handler() ->
    _ = logger:remove_handler(beamtalk_file_log),
    ok.
