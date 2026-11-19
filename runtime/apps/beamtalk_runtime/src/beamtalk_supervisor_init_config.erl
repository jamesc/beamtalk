%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_supervisor_init_config).

%%% **DDD Context:** Runtime Context

-moduledoc """
Reading and validating a supervisor's configuration inside `static_init/3` and
`dynamic_init/3`: restart intensity, strategy, the config method result and
the child class entry.

Split out of `beamtalk_supervisor` (BT-5047); the entry points generated code and
recorded child MFAs call keep their module and name there.
""".

-include("beamtalk.hrl").
-include("beamtalk_supervisor_config.hrl").
-include("beamtalk_supervisor_capacity.hrl").
-include_lib("kernel/include/logger.hrl").
-include("beamtalk_supervisor_internal.hrl").
-export([
    begin_init/3,
    check_integer/4,
    check_strategy/2,
    child_class_entry/2,
    child_class_entry_with_kind/1,
    config_result/1,
    config_step/3,
    read_config/3,
    read_restart_intensity/2
]).

%% ADR 0135 §2: hint for a configuration method that messaged its own
%% supervisor (OTP `calling_self`) while it was still in `init/1`.
-define(CALLING_SELF_HINT, <<
    "configuration methods run inside the supervisor's init; use `self args` "
    "and return specs, do not send messages to `self`"
>>).

%% Read and check `maxRestarts` / `restartWindow` (non-negative / positive
%% Integers, as OTP requires) so a bad value fails with a clear hint.
-spec read_restart_intensity(tuple(), atom()) -> {non_neg_integer(), pos_integer()}.

read_restart_intensity(Self, ClassName) ->
    MaxR = read_config(Self, ClassName, maxRestarts),
    MaxT = read_config(Self, ClassName, restartWindow),
    _ = config_result(check_integer(ClassName, maxRestarts, MaxR, 0)),
    _ = config_result(check_integer(ClassName, restartWindow, MaxT, 1)),
    {MaxR, MaxT}.

-spec check_integer(atom(), atom(), term(), integer()) ->
    {ok, integer()} | {error, #beamtalk_error{}}.

check_integer(_ClassName, _Selector, Value, Min) when is_integer(Value), Value >= Min ->
    {ok, Value};
check_integer(ClassName, Selector, Value, Min) ->
    {error,
        beamtalk_error:bad_value(
            ClassName,
            Selector,
            "~ts must be an Integer >= ~b, got ~tp",
            [Selector, Min, Value],
            Value
        )}.

%% Check a static supervisor's `strategy` and translate it to OTP's atom.
-spec check_strategy(atom(), term()) ->
    {ok, one_for_one | one_for_all | rest_for_one} | {error, #beamtalk_error{}}.

check_strategy(ClassName, Value) ->
    case beamtalk_supervisor_spec:to_otp_strategy(Value) of
        Otp when Otp =:= one_for_one; Otp =:= one_for_all; Otp =:= rest_for_one ->
            {ok, Otp};
        _ ->
            {error,
                beamtalk_error:bad_value(
                    ClassName,
                    strategy,
                    "strategy must be #oneForOne, #oneForAll or #restForOne, got ~tp",
                    [Value],
                    Value
                )}
    end.

%% Shared prologue of `static_init/3` / `dynamic_init/3`: record the start
%% args for `self args` and the class for `named:`, and return this
%% supervisor's own handle. A top-level spawn-family start wraps its args in
%% `{?TOP_LEVEL_TAG, Args}` (ADR 0135 §1): unwrap them and join
%% `?INSTANCES_GROUP` before `start_link` returns, so the instance is listed by
%% process navigation for as long as it runs (`pg` drops it on exit).
-spec begin_init(module(), atom(), term()) -> tuple().

begin_init(Module, ClassName, {?TOP_LEVEL_TAG, Args}) ->
    ok = join_instances_group(),
    begin_init(Module, ClassName, Args);
begin_init(Module, ClassName, Args) ->
    put(?SUP_ARGS_KEY, Args),
    put(?SUP_CLASS_KEY, {ClassName, Module}),
    {beamtalk_supervisor, ClassName, Module, self()}.

%% Join the default-scope `pg` group of top-level instances (see
%% `instance_pids/0` for why the default scope). `pg` is started by
%% `beamtalk_bootstrap`; without it (a bare test node) the instance is simply
%% not listed.
-spec join_instances_group() -> ok.

join_instances_group() ->
    %% `init/1` can run again in the same process (a supervisor's
    %% `code_change` re-runs it with the stored args): join only once.
    try
        case lists:member(self(), pg:get_local_members(?INSTANCES_GROUP)) of
            true -> ok;
            false -> pg:join(?INSTANCES_GROUP, self())
        end
    catch
        error:badarg -> ok;
        exit:{noproc, _} -> ok
    end.

-doc """
Read one configuration setting by sending `Selector` to the supervisor being
started (`Self`, its own handle): ordinary instance dispatch, in this
process (ADR 0135 §2).

The send runs under `beamtalk_class_vars:with_snapshot/2` (ADR 0130 §5), as
the class-side configuration did. Any failure ends `init/1` via
`config_step/3`.
""".
-spec read_config(tuple(), atom(), atom()) -> term().
read_config(Self, ClassName, Selector) ->
    Module = element(3, Self),
    ClassSelf = beamtalk_supervisor_class_call:make_init_class_self(ClassName, Module),
    config_step(ClassName, Selector, fun() ->
        beamtalk_class_vars:with_snapshot(ClassSelf, fun() ->
            beamtalk_message_dispatch:send(Self, Selector, [])
        end)
    end).

-doc """
Run one step of supervisor configuration inside `init/1`; on failure, exit
`init/1` with a `#beamtalk_error{}` (ADR 0135 §2).

`supervise/2` and the spawn family report such an exit as `supervisor_start_failed`
carrying the error's hint (a parent reports a nested failure as its own
start failure). A configuration method that sent a message to its own
supervisor (OTP `calling_self`, raw or wrapped by dispatch) gets the
`?CALLING_SELF_HINT` instead.
""".
-spec config_step(atom(), atom(), fun(() -> T)) -> T.
config_step(ClassName, Selector, Fun) ->
    %% bt-catcher-audit: not-applicable-reraises - every clause exits init/1, so the
    %% supervisor process (and any class-variable region in it) ends here
    try
        Fun()
    catch
        Class:Reason:Stack ->
            exit(config_error(ClassName, Selector, Class, Reason, Stack))
    end.

-spec config_error(atom(), atom(), atom(), term(), list()) -> #beamtalk_error{}.

config_error(ClassName, Selector, Class, Reason, Stack) ->
    case beamtalk_exception_handler:contains_calling_self(Reason) of
        true ->
            Error0 = beamtalk_error:new(
                supervisor_start_failed, ClassName, Selector, ?CALLING_SELF_HINT
            ),
            beamtalk_error:with_details(Error0, #{reason => calling_self});
        false ->
            #{error := BtError} = beamtalk_exception_handler:ensure_wrapped(
                Class, Reason, Stack, #{selector => Selector, class => ClassName}
            ),
            BtError
    end.

%% Unwrap a `check_*` result inside `init/1`: the validator's error ends
%% `init/1` (reported as `supervisor_start_failed` with its hint).
-spec config_result({ok, T} | {error, #beamtalk_error{}}) -> T.

config_result({ok, Value}) -> Value;
config_result({error, #beamtalk_error{} = Error}) -> exit(Error).

%% Resolve a DynamicSupervisor's `childClass` value to the
%% `{ChildClassName, ChildModule}` entry stored for `pool_child_class/1`.
%% BT-4502: cache the child's spawn kind next to the entry so `wrap_child/4`
%% skips the `spawn_kind/1` superclass walk on every start. Only a definitive
%% kind is cached; `unknown` (class not loaded yet) keeps the 2-tuple entry, which
%% `pool_child_entry/1` still reads and `wrap_child/4` resolves on demand.
-spec child_class_entry_with_kind({atom(), module()}) ->
    {atom(), module()} | {atom(), module(), supervisor | actor | none}.

child_class_entry_with_kind({ChildClass, ChildModule} = Entry) ->
    case beamtalk_supervisor:spawn_kind(ChildClass) of
        unknown -> Entry;
        Kind -> {ChildClass, ChildModule, Kind}
    end.

-spec child_class_entry(atom(), term()) -> {atom(), module()}.

child_class_entry(ClassName, ChildClass) ->
    case is_tuple(ChildClass) andalso beamtalk_class_registry:is_class_object(ChildClass) of
        true ->
            {
                beamtalk_object_class_read:class_name_safe(element(4, ChildClass)),
                element(3, ChildClass)
            };
        false ->
            exit(
                beamtalk_error:bad_value(
                    ClassName,
                    childClass,
                    "childClass must return a class, got ~tp",
                    [ChildClass],
                    ChildClass
                )
            )
    end.
