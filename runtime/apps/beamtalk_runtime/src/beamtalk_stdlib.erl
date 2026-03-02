%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Standard library initialization for Beamtalk.
%%%
%%% **DDD Context:** Standard Library
%%%
%%% This module loads all compiled stdlib modules (from stdlib/src/*.bt) at runtime
%%% startup. All class registrations are handled by compiled stdlib modules
%%% themselves — no hand-written registrations remain.
%%%
%%% ## Design: No Hand-Written Registrations
%%%
%%% All class registrations are handled by compiled stdlib modules themselves.
%%% Each compiled module has an on_load function that calls register_class/0,
%%% which creates the class process with the correct method maps generated
%%% from the .bt source files.
%%%
%%% This eliminates the previous pattern of hand-written register_*_class()
%%% functions which were a source of bugs (e.g., swapped maps, stale methods).
%%%
%%% ## Startup Order (BT-446)
%%%
%%% 1. beamtalk_bootstrap starts pg (process group for class registry)
%%% 2. This module reads class metadata from beamtalk_stdlib.app env
%%% 3. Topologically sorts by superclass dependency
%%% 4. Loads ALL modules in order (including ProtoObject, Object, Actor)
%%% 5. Each module's on_load → register_class/0 creates its class process
%%%
%%% ## Adding a New Stdlib Class
%%%
%%% Just add a .bt file to stdlib/src/ and run `beamtalk build-stdlib`.
%%% No Erlang code changes needed.
-module(beamtalk_stdlib).
-include_lib("kernel/include/logger.hrl").

-export([start_link/0, init/0, init/1]).

%% Beamtalk class dispatch - implements class methods for the Beamtalk global
-export([dispatch/3, has_method/1]).

%% Exported for testing
-ifdef(TEST).
-export([topo_sort/1]).
-endif.

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

%% @doc Initialize the standard library by loading compiled modules.
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
%% @doc Shared initialization logic for stdlib loading.
-spec do_init() -> ok.
do_init() ->
    ?LOG_INFO("Loading compiled stdlib modules"),
    %% Load compiled stdlib modules in dependency order.
    %% Each module's on_load → register_class/0 creates its class process
    %% with correct method maps generated from the .bt source.
    load_compiled_stdlib_modules(),
    ok.

%% @private
%% @doc Idle receive loop to keep the stdlib process alive.
-spec stdlib_loop() -> no_return().
stdlib_loop() ->
    receive
        _Any -> stdlib_loop()
    end.

%%% ============================================================================
%%% Compiled Stdlib Module Loading
%%% ============================================================================

%% @doc Load all compiled stdlib modules in dependency order.
%%
%% Reads class hierarchy from the beamtalk_stdlib app env (embedded in .app file
%% by build-stdlib), then loads modules in topological order (superclass before
%% subclass). Each module's on_load → register_class/0 creates its class process.
%%
%% BT-446: All classes are loaded from compiled stdlib, including ProtoObject,
%% Object, and Actor. The bootstrap no longer registers these classes.
-spec load_compiled_stdlib_modules() -> ok.
load_compiled_stdlib_modules() ->
    %% Ensure the beamtalk_stdlib app is loaded (not started — just metadata)
    _ = application:load(beamtalk_stdlib),
    case application:get_env(beamtalk_stdlib, classes) of
        {ok, ClassList} when is_list(ClassList) ->
            %% ClassList = [{Module, ClassName, SuperclassName}, ...]
            %% BT-446: Load ALL classes — bootstrap no longer registers any
            Sorted = topo_sort(ClassList),
            lists:foreach(
                fun({Mod, Class, _Super}) ->
                    case code:ensure_loaded(Mod) of
                        {module, Mod} ->
                            %% Module loaded. If on_load already ran but the class
                            %% process was killed (e.g., test teardown), re-register.
                            ensure_class_registered(Mod, Class),
                            ?LOG_DEBUG("Loaded stdlib module", #{module => Mod});
                        {error, Reason} ->
                            ?LOG_WARNING(
                                "Failed to load module ~s: ~p",
                                [format_bt_module(Mod), Reason]
                            )
                    end
                end,
                Sorted
            );
        undefined ->
            ?LOG_WARNING("No stdlib class metadata found, falling back to discovery"),
            EbinDir = find_stdlib_ebin(),
            discover_and_load_fallback(EbinDir)
    end.

%% @private
%% @doc Find the stdlib ebin directory (for fallback loading).
-spec find_stdlib_ebin() -> file:filename().
find_stdlib_ebin() ->
    case code:lib_dir(beamtalk_stdlib, ebin) of
        {error, _} -> "runtime/apps/beamtalk_stdlib/ebin";
        Dir -> Dir
    end.

%% @private
%% @doc Ensure a class is registered (or updated) after module loading.
%%
%% Always calls register_class/0, even if the class process is already alive.
%% This handles the case where a bootstrap stub re-registered the class with
%% incomplete methods (e.g., after a test teardown killed all class processes
%% and a subsequent test re-created the class via the hand-written stub rather
%% than the compiled module). The compiled register_class/0 handles already_started
%% via update_class/2, which is idempotent and restores the correct method set.
-spec ensure_class_registered(module(), atom()) -> ok.
ensure_class_registered(Mod, ClassName) ->
    try Mod:register_class() of
        ok ->
            ok;
        Other ->
            ?LOG_WARNING("Unexpected return from register_class", #{
                module => Mod,
                class => ClassName,
                result => Other
            }),
            ok
    catch
        error:undef ->
            %% Module doesn't export register_class/0
            ?LOG_WARNING("Module missing register_class/0", #{
                module => Mod,
                class => ClassName
            }),
            ok;
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Failed to register class", #{
                module => Mod,
                class => ClassName,
                error_class => Class,
                reason => Reason,
                stacktrace => Stacktrace
            }),
            ok
    end.

%% @private
%% @doc Topological sort by superclass dependency.
%%
%% Returns entries ordered so that each class's superclass appears before it.
-spec topo_sort([{module(), atom(), atom()}]) -> [{module(), atom(), atom()}].
topo_sort(Entries) ->
    ClassSet = sets:from_list([Class || {_, Class, _} <- Entries]),
    topo_sort_waves(Entries, ClassSet, sets:new(), []).

%% @private
%% @doc Iteratively emit classes whose superclass dependencies are satisfied.
-spec topo_sort_waves(
    [{module(), atom(), atom()}],
    sets:set(atom()),
    sets:set(atom()),
    [{module(), atom(), atom()}]
) -> [{module(), atom(), atom()}].
topo_sort_waves([], _ClassSet, _Emitted, Acc) ->
    lists:reverse(Acc);
topo_sort_waves(Remaining, ClassSet, Emitted, Acc) ->
    {Ready, Deferred} = lists:partition(
        fun({_Mod, _Class, Super}) ->
            %% Ready if superclass is external (bootstrap) or already emitted
            (not sets:is_element(Super, ClassSet)) orelse sets:is_element(Super, Emitted)
        end,
        Remaining
    ),
    case Ready of
        [] ->
            %% Circular dependency or missing superclass — emit remaining as-is
            ?LOG_WARNING(
                "Stdlib topo_sort: unresolvable dependencies",
                #{remaining => [C || {_, C, _} <- Deferred]}
            ),
            lists:reverse(Acc) ++ Deferred;
        _ ->
            NewEmitted = lists:foldl(
                fun({_, Class, _}, S) ->
                    sets:add_element(Class, S)
                end,
                Emitted,
                Ready
            ),
            topo_sort_waves(Deferred, ClassSet, NewEmitted, lists:reverse(Ready) ++ Acc)
    end.

%% @private
%% @doc Fallback: discover .beam files and load them all.
%%
%% Used when stdlib_classes.term is missing (e.g., development without build-stdlib).
-spec discover_and_load_fallback(file:filename()) -> ok.
discover_and_load_fallback(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            Modules = [
                list_to_atom(filename:rootname(F))
             || F <- Files, filename:extension(F) =:= ".beam"
            ],
            lists:foreach(
                fun(Mod) ->
                    case code:ensure_loaded(Mod) of
                        {module, Mod} ->
                            ?LOG_DEBUG("Loaded stdlib module (fallback)", #{module => Mod});
                        {error, Reason} ->
                            ?LOG_WARNING(
                                "Failed to load module ~s: ~p",
                                [format_bt_module(Mod), Reason]
                            )
                    end
                end,
                Modules
            );
        {error, Reason} ->
            ?LOG_WARNING(
                "Cannot list stdlib ebin directory",
                #{dir => Dir, reason => Reason}
            )
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
    Pids = beamtalk_class_registry:all_classes(),
    [beamtalk_object_class:class_name(Pid) || Pid <- Pids];
dispatch('classNamed:', [ClassName], _Receiver) when is_atom(ClassName) ->
    %% Look up a class by name, return wrapped class object or nil
    case beamtalk_class_registry:whereis_class(ClassName) of
        undefined -> nil;
        Pid -> {beamtalk_object, ClassName, beamtalk_object_class, Pid}
    end;
dispatch(globals, [], _Receiver) ->
    %% Placeholder - global namespace not yet implemented
    #{};
dispatch(version, [], _Receiver) ->
    %% Return Beamtalk version from OTP application metadata
    case application:get_key(beamtalk_runtime, vsn) of
        {ok, Vsn} -> list_to_binary(Vsn);
        _ -> <<"unknown">>
    end;
dispatch(Selector, _Args, _Receiver) ->
    %% Unknown method
    Error0 = beamtalk_error:new(does_not_understand, 'Beamtalk'),
    Error1 = beamtalk_error:with_selector(Error0, Selector),
    Error2 = beamtalk_error:with_hint(
        Error1, <<"Check spelling or use 'Beamtalk respondsTo:' to verify method exists">>
    ),
    beamtalk_error:raise(Error2).

%% @doc Format an Erlang module atom as a Beamtalk dotted path.
%% e.g. 'bt@sicp@scheme@lambda' -> "bt.sicp.scheme.lambda"
-spec format_bt_module(atom()) -> string().
format_bt_module(Mod) ->
    re:replace(atom_to_list(Mod), "@", ".", [global, {return, list}]).

%% @doc Check if the Beamtalk class responds to the given selector.
-spec has_method(atom()) -> boolean().
has_method(allClasses) -> true;
has_method('classNamed:') -> true;
has_method(globals) -> true;
has_method(version) -> true;
has_method(_) -> false.
