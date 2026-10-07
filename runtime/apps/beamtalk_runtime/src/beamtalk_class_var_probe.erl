%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_class_var_probe).

%%% **DDD Context:** Object System Context

-moduledoc """
ADR 0130 Phase 0 census probe for class-variable accesses inside blocks.

Compiled code calls `report/6` only when the module was generated with
`BEAMTALK_CLASS_VAR_PROBE=1`; with the flag unset nothing references this
module and generated code is byte-identical. The call precedes every
class-variable read or write. Inside a non-inlined block (`in_block = true`)
the runtime cannot otherwise see the access (the block reads a lexically
captured map with no runtime call), so every such access is logged. At a
method's own level (`in_block = false`) an access made by the home class process
is the normal case and is not logged; only an access made away from home is,
which is how a stored closure's self-send reaches a class-variable write.

Each call emits one OTP logger event at `notice` level with
`domain => [beamtalk, probe]` and a report map:

| key          | meaning                                                          |
|--------------|------------------------------------------------------------------|
| `class`      | class whose method contains the block                            |
| `selector`   | class-side selector the block literal was written in             |
| `kind`       | `read` or `write`                                                |
| `in_block`   | access sits lexically inside a non-inlined block                 |
| `field`      | class-variable name                                              |
| `at_home`    | this process holds the class's variables (key presence, ADR 0130 §2) |
| `home_live`  | the home class process is inside a class-method invocation       |
| `process`    | `self()` rendered as text                                        |
| `home`       | the home pid rendered as text, or `none`                         |
| `shape`      | `home` (at home), `carried_sync` (home live but blocked), `abroad` |

`home_live` is read from the home process's stack: a frame in
`beamtalk_class_dispatch:invoke_class_method/7` or `invoke_class_extension/7`.
It is a probe-quality answer (a stack of depth > the system backtrace depth
may hide the frame; `report/6` raises `backtrace_depth` to 128), not an API.

Census sink: when the environment variable `BEAMTALK_CLASS_VAR_PROBE_LOG` names
a file, the first `report/6` call installs a `logger_std_h` handler
(`beamtalk_class_var_probe`) that appends only `[beamtalk, probe]` events to it,
one line each, and lowers the primary level to `notice` so they are not
dropped. To keep that from leaking `notice` events into the node's other
handlers, every other handler configured at a level below `warning` is raised to
`warning`. Without the variable, events go to whatever handlers the node already
has.

Global side effects (census runs only, never normal operation): the first
reported event sets `erlang:system_flag(backtrace_depth, 128)` for the whole VM
(once, the previous value is not restored) so `home_live` can see deep stacks.
A class-side `hasField:` test is reported as a `read` (field `_dynamic` when its
argument is not a literal Symbol).

The function never raises: a failing probe must not change program behaviour.
""".

-include_lib("kernel/include/logger.hrl").
-include("beamtalk.hrl").

-export([report/6]).

-define(SINK_HANDLER, beamtalk_class_var_probe).
-define(SETUP_KEY, {beamtalk_class_var_probe, setup}).

-doc "Report one class-variable access (see the module doc for what is logged).".
-spec report(term(), atom(), atom(), read | write, atom(), boolean()) -> ok.
report(ClassSelf, Class, Selector, Kind, Field, InBlock) ->
    try
        case at_home(ClassSelf) of
            true when not InBlock -> ok;
            _ -> do_report(ClassSelf, Class, Selector, Kind, Field, InBlock)
        end
    catch
        _:_ -> ok
    end.

-spec do_report(term(), atom(), atom(), read | write, atom(), boolean()) -> ok.
do_report(ClassSelf, Class, Selector, Kind, Field, InBlock) ->
    try
        ok = ensure_sink(),
        HomePid = home_pid(ClassSelf),
        Self = self(),
        AtHome = at_home(ClassSelf),
        HomeLive = AtHome orelse home_invocation_live(HomePid),
        Shape =
            case {AtHome, HomeLive} of
                {true, _} -> home;
                {false, true} -> carried_sync;
                {false, false} -> abroad
            end,
        ?LOG_NOTICE(
            #{
                class => Class,
                selector => Selector,
                kind => Kind,
                in_block => InBlock,
                field => Field,
                at_home => AtHome,
                home_live => HomeLive,
                shape => Shape,
                process => pid_text(Self),
                home => pid_text(HomePid)
            },
            #{domain => [beamtalk, probe]}
        ),
        ok
    catch
        _:_ -> ok
    end.

%%% Internal

-spec ensure_sink() -> ok.
ensure_sink() ->
    %% One-time probe setup. Deep class-method call chains need a deeper
    %% backtrace window to find the invoke frame on a blocked home process;
    %% the flag is VM-wide, so set it once, not on every event.
    %%
    %% BT-3723: the check-then-put must be atomic. Two processes reporting
    %% concurrently could both see `false` and both install the handler (the
    %% second `add_handler` fails) or race the quieting of other handlers, so
    %% the slow path runs under a node-local global lock and re-checks inside
    %% it. The flag is set only after setup finishes, so a concurrent reporter
    %% waits for the lock rather than logging before the sink exists.
    case persistent_term:get(?SETUP_KEY, false) of
        true ->
            ok;
        false ->
            _ = global:trans({?SETUP_KEY, self()}, fun setup_once/0, [node()], infinity),
            ok
    end.

-spec setup_once() -> ok.
setup_once() ->
    case persistent_term:get(?SETUP_KEY, false) of
        true ->
            ok;
        false ->
            _ = erlang:system_flag(backtrace_depth, 128),
            ok = ensure_sink_handler(),
            ok = persistent_term:put(?SETUP_KEY, true)
    end.

-spec ensure_sink_handler() -> ok.
ensure_sink_handler() ->
    case os:getenv("BEAMTALK_CLASS_VAR_PROBE_LOG") of
        false ->
            ok;
        File ->
            case logger:get_handler_config(?SINK_HANDLER) of
                {ok, _} ->
                    ok;
                _ ->
                    _ = logger:add_handler(?SINK_HANDLER, logger_std_h, #{
                        level => notice,
                        config => #{file => File},
                        filter_default => stop,
                        filters => [
                            {probe_domain, {
                                fun logger_filters:domain/2, {log, sub, [beamtalk, probe]}
                            }}
                        ],
                        formatter =>
                            {logger_formatter, #{single_line => true, template => [msg, "\n"]}}
                    }),
                    ok = quiet_other_handlers(),
                    case logger:get_primary_config() of
                        #{level := Level} when
                            Level =:= emergency;
                            Level =:= alert;
                            Level =:= critical;
                            Level =:= error;
                            Level =:= warning
                        ->
                            _ = logger:set_primary_config(level, notice);
                        _ ->
                            ok
                    end,
                    ok
            end
    end.

%% Lowering the primary level to `notice` would let notice events reach every
%% other handler; raise any handler more verbose than `warning` to `warning`.
-spec quiet_other_handlers() -> ok.
quiet_other_handlers() ->
    lists:foreach(
        fun
            (#{id := ?SINK_HANDLER}) ->
                ok;
            (#{id := Id, level := Level}) when
                Level =:= all; Level =:= debug; Level =:= info; Level =:= notice
            ->
                _ = logger:set_handler_config(Id, level, warning),
                ok;
            (_) ->
                ok
        end,
        logger:get_handler_config()
    ).

%% ADR 0130 §2: "at home" is key presence, not pid equality: this process holds
%% the class's variables under `{'$bt_class_vars', ClassTag}` (the live
%% invocation's, or a `with_snapshot/2` region's), the same test the access
%% helpers make. A class-process restart or a closure holding a stale pid does
%% not change the answer.
-spec at_home(term()) -> boolean().
at_home(#beamtalk_object{class = ClassTag}) when is_atom(ClassTag) ->
    erlang:get(beamtalk_class_vars:key_for_tag(ClassTag)) =/= undefined;
at_home(_) ->
    false.

-spec home_pid(term()) -> pid() | none.
home_pid(#beamtalk_object{pid = Pid}) when is_pid(Pid) -> Pid;
home_pid(_) -> none.

-spec home_invocation_live(pid() | none) -> boolean().
home_invocation_live(none) ->
    false;
home_invocation_live(Pid) when node(Pid) =/= node() ->
    %% BT-3723: `erlang:process_info/2` raises `badarg` on a pid of another
    %% node, which would drop the whole event. The census runs single-node; a
    %% remote home is reported as not live (`abroad`) rather than probed.
    false;
home_invocation_live(Pid) ->
    case erlang:process_info(Pid, current_stacktrace) of
        {current_stacktrace, Frames} ->
            lists:any(fun is_invoke_frame/1, Frames);
        _ ->
            false
    end.

-spec is_invoke_frame(term()) -> boolean().
is_invoke_frame({beamtalk_class_dispatch, invoke_class_method, _, _}) -> true;
is_invoke_frame({beamtalk_class_dispatch, invoke_class_extension, _, _}) -> true;
is_invoke_frame(_) -> false.

-spec pid_text(pid() | none) -> binary().
pid_text(none) -> <<"none">>;
pid_text(Pid) -> list_to_binary(pid_to_list(Pid)).
