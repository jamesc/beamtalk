%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_bootstrap_stub).

%%% **DDD Context:** Object System Context

-moduledoc """
Shared boilerplate for the hand-coded bootstrap `_bt.erl` stub modules
(`beamtalk_class_bt`, `beamtalk_metaclass_bt`, `beamtalk_class_builder_bt`).

BT-2788: Each stub duplicated (a) a generic `does_not_understand` dispatch
tail and (b) a `register_class/0` body that starts the class process and,
on `{already_started, _}`, either refreshes it via `update_class/2` or does
nothing — plus matching `?LOG_INFO`/`?LOG_WARNING` calls. This module
factors both patterns out; the stubs keep their own module-specific
`dispatch/4` clauses and `ClassInfo` maps and delegate the shared tail.
""".

-include("beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([dnu/3, register/3]).

-doc """
Build the generic `does_not_understand` dispatch tail shared by every
bootstrap stub's `dispatch/4` fallthrough clause.

`ClassName` is the Beamtalk class the stub implements (e.g. `'Class'`,
`'Metaclass'`, `'ClassBuilder'`); `Selector` is the unrecognised message;
`State` is passed through unchanged.
""".
-spec dnu(atom(), atom(), map()) -> {error, #beamtalk_error{}, map()}.
dnu(ClassName, Selector, State) ->
    Error = beamtalk_error:new(does_not_understand, ClassName, Selector),
    {error, Error, State}.

-doc """
Register a bootstrap stub class, handling the shared start/already-started
dance and logging.

`Opts` is a map with:
  - `module`: the calling `_bt` module (used as `?LOG_INFO`/`?LOG_WARNING`
    metadata, matching what each stub logged before this refactor).
  - `registered_msg`: log message for a fresh registration.
  - `refresh_on_conflict`: whether to call `beamtalk_object_class:update_class/2`
    when the class is already registered. `beamtalk_class_bt` passes `false`
    because its `ClassInfo` intentionally carries an empty `instance_methods`
    map (the compiled stdlib module owns the real ones via its own
    `update_class` call) — refreshing here would clobber them. The
    `Metaclass`/`ClassBuilder` stubs pass `true` since their `ClassInfo` is
    authoritative and refreshing keeps metadata consistent across repeated
    bootstrap runs.
""".
-spec register(atom(), map(), #{
    module := module(),
    registered_msg := iodata(),
    refresh_on_conflict := boolean()
}) -> ok.
register(ClassName, ClassInfo, #{
    module := Module, registered_msg := RegisteredMsg, refresh_on_conflict := RefreshOnConflict
}) ->
    case beamtalk_object_class:start(ClassName, ClassInfo) of
        {ok, _Pid} ->
            ?LOG_INFO(RegisteredMsg, #{module => Module, domain => [beamtalk, runtime]}),
            ok;
        {error, {already_started, _}} ->
            %% Already registered (compiled stdlib loaded first, or bootstrap ran
            %% twice). See `refresh_on_conflict` doc above for why this differs
            %% per stub.
            case RefreshOnConflict of
                true -> beamtalk_object_class:update_class(ClassName, ClassInfo);
                false -> ok
            end,
            ok;
        {error, Reason} ->
            ?LOG_WARNING("Failed to register bootstrap stub class", #{
                class => ClassName,
                module => Module,
                reason => Reason,
                domain => [beamtalk, runtime]
            }),
            ok
    end.
