%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Application callback module for the Beamtalk runtime.
%%%
%%% **DDD Context:** Runtime
-module(beamtalk_runtime_app).
-behaviour(application).

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

-export([start/2, stop/1]).

%% @private
%% @doc Start the Beamtalk runtime application, initializing ETS tables and supervisor tree.
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Initialize extension registry ETS tables
    beamtalk_extensions:init(),

    %% BT-510: Create class hierarchy ETS table at app startup so it's owned
    %% by the application master (survives individual class process crashes).
    beamtalk_class_registry:ensure_hierarchy_table(),

    %% BT-737: Create collision warnings ETS table at app startup.
    beamtalk_class_registry:ensure_class_warnings_table(),

    %% Start the runtime supervisor tree (which starts beamtalk_bootstrap, beamtalk_stdlib,
    %% and beamtalk_object_instances; pg is conditionally started inside beamtalk_bootstrap:init/1)
    case beamtalk_runtime_sup:start_link() of
        {ok, _} = Ok ->
            %% ADR 0036 Phase 1 (BT-802): Post-bootstrap self-grounding assertion.
            %% Validates that Metaclass class class == Metaclass class holds
            %% after bootstrap. This is a soft assertion (logs on failure, does not crash).
            verify_metaclass_self_grounding(),
            Ok;
        Error ->
            Error
    end.

%% @private
%% @doc Verify the ADR 0036 metaclass self-grounding invariant after bootstrap.
%%
%% The invariant: `Metaclass class class == Metaclass class`
%% In Erlang terms: class_of_object(class_of_object(MetaclassRef)) =:= class_of_object(MetaclassRef)
%% This holds because class_of_object/1 is idempotent for 'Metaclass'-tagged objects.
-spec verify_metaclass_self_grounding() -> ok.
verify_metaclass_self_grounding() ->
    case beamtalk_class_registry:whereis_class('Metaclass') of
        undefined ->
            %% Metaclass not registered — bootstrap may not have run yet.
            ok;
        MetaclassPid ->
            Module = beamtalk_object_class:module_name(MetaclassPid),
            Tag = beamtalk_class_registry:class_object_tag('Metaclass'),
            %% MetaclassRef is the Metaclass class reference (tag='Metaclass class')
            MetaclassRef = #beamtalk_object{class = Tag, class_mod = Module, pid = MetaclassPid},
            %% MetaclassClass is "Metaclass class" (tag='Metaclass')
            MetaclassClass = beamtalk_primitive:class_of_object(MetaclassRef),
            %% MetaclassClassClass is "Metaclass class class" (must equal MetaclassClass)
            MetaclassClassClass = beamtalk_primitive:class_of_object(MetaclassClass),
            case MetaclassClass =:= MetaclassClassClass of
                true ->
                    ?LOG_INFO(
                        "ADR 0036: Metaclass self-grounding OK (Metaclass class class == Metaclass class)",
                        #{module => ?MODULE}
                    ),
                    ok;
                false ->
                    ?LOG_ERROR(
                        "ADR 0036: Metaclass self-grounding FAILED: Metaclass class class =/= Metaclass class",
                        #{
                            module => ?MODULE,
                            metaclass_class => MetaclassClass,
                            metaclass_class_class => MetaclassClassClass
                        }
                    ),
                    ok
            end
    end.

%% @private
%% @doc Stop the Beamtalk runtime application.
-spec stop(term()) -> ok.
stop(_State) ->
    ok.
