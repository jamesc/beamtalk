%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_file_handle_registry_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
EUnit tests for `beamtalk_file_handle_registry` (BT-3020).

Covers the acceptance criteria:

* a handle whose owner dies is closed and removed from the registry;
* an unowned handle (registered with `Owner = undefined`, mirroring
  `open:mode:` called from compiled code with neither a session nor a calling
  actor) is listed but never reclaimed by any death;
* `unregister/1` removes a handle immediately, so a closed handle does not
  linger in `open_handles/0` and a later owner `'DOWN'` cannot double-close it;
* handles sharing an owner share a single monitor, torn down only once the
  last of them is gone;
* owner-death reclamation does not block the registry's mailbox (BT-3050) —
  an unrelated `register`/`open_handles` is still answered while one owner's
  `file:close/1` is stuck.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup / teardown
%%====================================================================

setup() ->
    case whereis(beamtalk_file_handle_registry) of
        undefined -> {ok, _} = beamtalk_file_handle_registry:start_link();
        _ -> ok
    end,
    ok.

teardown(_) ->
    ok.

-doc "A unique relative temp path — never /tmp, per testing-strategy.md.".
temp_path(Tag) ->
    Tag ++ "_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".tmp".

-doc "Open a real file and wrap it as a FileHandle — genuinely closeable.".
open_handle() ->
    TmpPath = temp_path("beamtalk_file_handle_registry_test"),
    {ok, Fd} = file:open(TmpPath, [write, binary]),
    Handle = beamtalk_file_handle:new(Fd, write, list_to_binary(TmpPath)),
    {Handle, TmpPath}.

delete_temp(TmpPath) ->
    file:delete(TmpPath),
    ok.

spawn_dummy() ->
    spawn(fun() ->
        receive
            stop -> ok
        end
    end).

stop_dummy(Pid) ->
    Pid ! stop,
    wait_dead(Pid).

wait_dead(Pid) ->
    case is_process_alive(Pid) of
        false ->
            ok;
        true ->
            timer:sleep(5),
            wait_dead(Pid)
    end.

%% A descriptor stand-in that never answers a close request. `file:close/1` on
%% a pid sends the request and then waits for either a `{file_reply, ...}` or
%% the descriptor process's death, so a handle wrapping this pid makes
%% reclamation hang until `release_wedged_fd/1`.
spawn_wedged_fd() ->
    spawn(fun() ->
        receive
            release -> ok
        end
    end).

release_wedged_fd(Fd) ->
    Fd ! release,
    wait_dead(Fd).

%% Block until the registry has processed all calls up to this point (calls
%% are handled in order, so a further sync call flushes prior ones — and
%% 'DOWN' messages are handled by the same serialised mailbox).
sync(_) ->
    _ = sys:get_state(beamtalk_file_handle_registry),
    ok.

%% Poll `Pred` until it holds, then return ok; assert (and so fail the test) if
%% it never does. Owner-death reclamation is only half-synchronous since
%% BT-3050: `sync/1` proves the registry has dropped its own bookkeeping, but
%% the descriptor is closed by a transient process shortly afterwards, so
%% "is the handle shut?" has to be polled rather than assumed.
wait_until(Pred) ->
    wait_until(Pred, 400).

wait_until(Pred, 0) ->
    ?assert(Pred());
wait_until(Pred, Attempts) ->
    case Pred() of
        true ->
            ok;
        false ->
            timer:sleep(5),
            wait_until(Pred, Attempts - 1)
    end.

closed(Handle) ->
    fun() -> not beamtalk_file_handle:is_open(Handle) end.

unlisted(Path) ->
    fun() -> not has_entry(Path, beamtalk_file_handle_registry:open_handles()) end.

has_entry(Path, Handles) ->
    lists:any(fun({P, _Mode, _Owner}) -> P =:= Path end, Handles).

%%====================================================================
%% Tests
%%====================================================================

owned_handle_closed_and_removed_on_owner_death_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            Owner = spawn_dummy(),
            {Handle, TmpPath} = open_handle(),
            PathBin = list_to_binary(TmpPath),
            ok = beamtalk_file_handle_registry:register(Handle, Owner),
            ?assert(has_entry(PathBin, beamtalk_file_handle_registry:open_handles())),
            ?assert(beamtalk_file_handle:is_open(Handle)),

            stop_dummy(Owner),
            ok = sync(ok),

            %% Bookkeeping is dropped synchronously with the 'DOWN'; the close
            %% itself lands a moment later, off the registry process (BT-3050).
            ?assertNot(has_entry(PathBin, beamtalk_file_handle_registry:open_handles())),
            ok = wait_until(closed(Handle)),
            delete_temp(TmpPath)
        end)
    end}.

registry_stays_responsive_while_owner_close_is_in_flight_test_() ->
    %% BT-3050: owner-death reclamation must not run `file:close/1` inside the
    %% registry's own handle_info. One owner's handle wraps a descriptor
    %% stand-in that never answers a close, so its reclamation is stuck for as
    %% long as the test wants it to be; meanwhile an unrelated register/
    %% open_handles must still be answered. Both assertions below use a bounded
    %% timeout, so the inline-close implementation this replaces fails them
    %% (gen_server:call exits on timeout) rather than merely running slowly.
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            Registry = whereis(beamtalk_file_handle_registry),
            Owner = spawn_dummy(),
            WedgedFd = spawn_wedged_fd(),
            WedgedPath = list_to_binary(temp_path("beamtalk_file_handle_registry_wedged")),
            Wedged = beamtalk_file_handle:new(WedgedFd, write, WedgedPath),
            ok = beamtalk_file_handle_registry:register(Wedged, Owner),

            stop_dummy(Owner),
            %% The registry drops its own bookkeeping synchronously, so the
            %% wedged handle disappearing proves the 'DOWN' was handled...
            ok = wait_until(unlisted(WedgedPath)),
            %% ...while its close is genuinely still in flight and stuck.
            ?assert(is_process_alive(WedgedFd)),

            {Other, OtherPath} = open_handle(),
            OtherPathBin = list_to_binary(OtherPath),
            ?assertEqual(ok, gen_server:call(Registry, {register, Other, undefined}, 1000)),
            ?assert(has_entry(OtherPathBin, gen_server:call(Registry, open_handles, 1000))),

            %% Releasing the stand-in lets the stuck close/1 return and the
            %% reclaimer finish on its own. (The handle already reads closed:
            %% close_handle/1 claims its atomics cell before calling
            %% file:close/1, which is what makes the close idempotent.)
            release_wedged_fd(WedgedFd),
            ?assertNot(beamtalk_file_handle:is_open(Wedged)),

            ok = beamtalk_file_handle:close_handle(Other),
            ok = beamtalk_file_handle_registry:unregister(Other),
            delete_temp(OtherPath)
        end)
    end}.

unowned_handle_listed_but_never_reclaimed_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            {Handle, TmpPath} = open_handle(),
            PathBin = list_to_binary(TmpPath),
            %% Mirrors `open:mode:` called from compiled code with no session
            %% and no calling actor: no owner to register, so nothing to
            %% monitor — the handle is listed but reclaimed by nothing.
            ok = beamtalk_file_handle_registry:register(Handle, undefined),
            ok = sync(ok),
            ?assert(has_entry(PathBin, beamtalk_file_handle_registry:open_handles())),
            ?assert(beamtalk_file_handle:is_open(Handle)),

            %% The process that happened to make the call (the "caller") is
            %% never itself the owner for an unowned registration — it is not
            %% monitored at all, so its death changes nothing.
            Caller = spawn_dummy(),
            stop_dummy(Caller),
            ok = sync(ok),
            ?assert(beamtalk_file_handle:is_open(Handle)),
            ?assert(has_entry(PathBin, beamtalk_file_handle_registry:open_handles())),

            beamtalk_file_handle:close_handle(Handle),
            ok = beamtalk_file_handle_registry:unregister(Handle),
            delete_temp(TmpPath)
        end)
    end}.

unregister_removes_immediately_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            Owner = spawn_dummy(),
            {Handle, TmpPath} = open_handle(),
            PathBin = list_to_binary(TmpPath),
            ok = beamtalk_file_handle_registry:register(Handle, Owner),
            ok = beamtalk_file_handle:close_handle(Handle),
            ok = beamtalk_file_handle_registry:unregister(Handle),
            ?assertNot(has_entry(PathBin, beamtalk_file_handle_registry:open_handles())),

            %% Owner death after an explicit close/unregister must find nothing
            %% left to close — close_handle/1 is already idempotent, but the
            %% registry must not even attempt it: the entry is gone.
            stop_dummy(Owner),
            ok = sync(ok),
            ?assertNot(has_entry(PathBin, beamtalk_file_handle_registry:open_handles())),
            delete_temp(TmpPath)
        end)
    end}.

shared_owner_monitor_survives_until_last_handle_gone_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            Owner = spawn_dummy(),
            {H1, P1} = open_handle(),
            {H2, P2} = open_handle(),
            ok = beamtalk_file_handle_registry:register(H1, Owner),
            ok = beamtalk_file_handle_registry:register(H2, Owner),

            %% Close and unregister only the first — the second must still be
            %% reclaimed when the shared owner dies (the owner's monitor must
            %% not have been torn down early).
            ok = beamtalk_file_handle:close_handle(H1),
            ok = beamtalk_file_handle_registry:unregister(H1),

            stop_dummy(Owner),
            ok = sync(ok),

            ok = wait_until(closed(H2)),
            Handles = beamtalk_file_handle_registry:open_handles(),
            ?assertNot(has_entry(list_to_binary(P1), Handles)),
            ?assertNot(has_entry(list_to_binary(P2), Handles)),
            delete_temp(P1),
            delete_temp(P2)
        end)
    end}.

no_server_calls_are_safe_test() ->
    %% Mirrors beamtalk_object_watch_tests:publish_no_server_is_safe_test/0:
    %% this server is supervised (one_for_one, permanent), so it may already
    %% be restarted by the time the assertions below run. Either way — freshly
    %% absent or freshly restarted-empty — every assertion holds.
    case whereis(beamtalk_file_handle_registry) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    {Handle, TmpPath} = open_handle(),
    ?assertEqual(ok, beamtalk_file_handle_registry:register(Handle, undefined)),
    ?assertEqual(ok, beamtalk_file_handle_registry:unregister(Handle)),
    beamtalk_file_handle:close_handle(Handle),
    delete_temp(TmpPath).

malformed_handle_calls_are_safe_test_() ->
    %% A handle missing the `state` cell (beamtalk_file_handle:new/3 is the
    %% only real constructor, so this cannot happen via the public FileHandle
    %% API) must never crash the registry — register/2 and unregister/1 guard
    %% on `is_map_key(state, Handle)` and no-op otherwise.
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            Malformed = #{'$beamtalk_class' => 'FileHandle', fd => not_a_real_fd},
            ?assertEqual(ok, beamtalk_file_handle_registry:register(Malformed, undefined)),
            ?assertEqual(ok, beamtalk_file_handle_registry:register(Malformed, self())),
            ?assertEqual(ok, beamtalk_file_handle_registry:unregister(Malformed)),
            %% The registry is still alive and answering normal calls.
            ?assertEqual(ok, sync(ok)),
            ?assertNot(has_entry(<<"anything">>, beamtalk_file_handle_registry:open_handles()))
        end)
    end}.

open_handles_no_server_is_safe_test() ->
    %% Complements no_server_calls_are_safe_test/0: that test covers register/2
    %% and unregister/1 when the server is absent; this one covers open_handles/0.
    %% After gen_server:stop the registered name is gone (stop is synchronous);
    %% open_handles/0 must return [] rather than raise.
    case whereis(beamtalk_file_handle_registry) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    ?assertEqual([], beamtalk_file_handle_registry:open_handles()).

unknown_call_returns_error_unknown_request_test_() ->
    %% handle_call/3 has a catch-all clause that replies {error, unknown_request}
    %% for any message not matching the three known selectors. Verify it without
    %% crashing the server.
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            Pid = whereis(beamtalk_file_handle_registry),
            ?assertMatch(
                {error, unknown_request},
                gen_server:call(Pid, totally_unknown_message)
            ),
            %% Registry is still alive after the unknown call.
            ?assertEqual(ok, sync(ok))
        end)
    end}.

unknown_cast_is_safe_test_() ->
    %% handle_cast/2 catch-all: an unexpected cast must not crash the server.
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            gen_server:cast(beamtalk_file_handle_registry, totally_unknown_cast),
            ?assertEqual(ok, sync(ok))
        end)
    end}.

unknown_handle_info_is_safe_test_() ->
    %% handle_info/2 catch-all: an unexpected message sent directly to the
    %% process must not crash the server.
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            beamtalk_file_handle_registry ! totally_unknown_info,
            ?assertEqual(ok, sync(ok))
        end)
    end}.

multiple_owners_monitor_demonitored_independently_test_() ->
    %% When two different owners each hold a handle, closing and unregistering
    %% one owner's handle must demonitor that owner WITHOUT removing the other
    %% owner's monitor.  This exercises the `(_Ref, _P) -> true` keep-branch
    %% inside `demonitor_owner/2`'s `maps:filter` — only reached when the
    %% monitors map contains entries for more than one distinct owner pid.
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        ?_test(begin
            Owner1 = spawn_dummy(),
            Owner2 = spawn_dummy(),
            {H1, P1} = open_handle(),
            {H2, P2} = open_handle(),
            ok = beamtalk_file_handle_registry:register(H1, Owner1),
            ok = beamtalk_file_handle_registry:register(H2, Owner2),

            %% Explicitly close and unregister H1 (not via Owner1's death).
            %% This must demonitor Owner1 while leaving Owner2's monitor intact.
            ok = beamtalk_file_handle:close_handle(H1),
            ok = beamtalk_file_handle_registry:unregister(H1),
            ok = sync(ok),

            %% H2 must still be reclaimed when Owner2 dies (its monitor was kept).
            stop_dummy(Owner2),
            ok = sync(ok),
            ok = wait_until(closed(H2)),
            ?assertNot(
                has_entry(
                    list_to_binary(P2),
                    beamtalk_file_handle_registry:open_handles()
                )
            ),

            %% Owner1 is still alive but holds no handles — let it exit cleanly.
            stop_dummy(Owner1),
            ok = sync(ok),
            ?assertNot(
                has_entry(
                    list_to_binary(P1),
                    beamtalk_file_handle_registry:open_handles()
                )
            ),
            delete_temp(P1),
            delete_temp(P2)
        end)
    end}.
