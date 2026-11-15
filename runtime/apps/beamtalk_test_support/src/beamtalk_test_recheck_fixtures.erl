%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_recheck_fixtures).

%%% **DDD Context:** REPL Context (test support)

-moduledoc """
Shared fixtures for the `beamtalk_workspace` recheck suites
(`beamtalk_recheck_tests`, `beamtalk_repl_loader_recheck_tests`,
`beamtalk_repl_loader_shape_recheck_tests`,
`beamtalk_repl_loader_leaf_change_recheck_tests`,
`beamtalk_repl_alias_change_recheck_tests`): the integration setup/teardown,
the `reload_check` announcement helper, and the `Dashboard`/`ListView`
dependent sources with their matching xref payloads.

Each suite names its own class family with a `Prefix` (`<<"ReCheck">>`,
`<<"LoaderReCheck">>`, …): the dependent is `<Prefix>Dashboard`, the changed
class it reads from is `<Prefix>Counter`. The prefixes stay per suite so that
two suites loading classes into the one EUnit VM never collide.
""".

-export([
    setup/1,
    teardown/1,
    subscribe_self_to_reload_check/0,
    receive_reload_check_announcement/0,
    dashboard_xref/1,
    dashboard_source/1,
    listview_xref/1,
    listview_counter_source/1,
    listview_list_source/1,
    dashboard_name_accessor_xref/1,
    dashboard_name_accessor_source/1
]).

-type opts() :: #{
    workspace_id := binary(),
    restart => [module()],
    clear_xref => boolean()
}.
-export_type([opts/0]).

%%====================================================================
%% Integration setup / teardown
%%====================================================================

-doc """
Boot the workspace for a recheck suite: `beamtalk_test_boot:workspace/1`
with the SystemAnnouncer bus ensured up (a `subscribe/2` made before the
bus's first announce would otherwise be silently skipped).

Subscribing is deliberately not done here — call
`subscribe_self_to_reload_check/0` from inside each test body: an EUnit
`{setup, …}` fixture's `Start` function is not guaranteed to run in the same
process as the test body it brackets, and a `self()`-keyed subscription is
only useful if subscriber and receiver are the same process.
""".
-spec setup(opts()) -> ok.
setup(Opts) ->
    beamtalk_test_boot:workspace(Opts#{announcements => true}).

-doc """
Tear down what `setup/1` started (`Opts` is the same map): unsubscribe the
calling process, stop the workspace meta and every restarted singleton, reset
the xref tables (or, with `clear_xref => false`, the compiler class caches),
and stop the compiler port.
""".
-spec teardown(opts()) -> ok.
teardown(Opts) ->
    beamtalk_repl_subscriptions:unsubscribe(reload_check, self()),
    beamtalk_test_proc:stop_registered(beamtalk_workspace_meta),
    [beamtalk_test_proc:stop_registered(Name) || Name <- maps:get(restart, Opts, [])],
    case maps:get(clear_xref, Opts, true) of
        true -> beamtalk_test_repl:clear_xref();
        false -> beamtalk_test_boot:clear_compiler_classes()
    end,
    _ = application:stop(beamtalk_compiler),
    ok.

%%====================================================================
%% Announcements
%%====================================================================

-doc """
Subscribe *this* process (the test body's own pid) to the `reload_check`
stream. Must be called from inside each `?_test(...)` body — see `setup/1`.
""".
-spec subscribe_self_to_reload_check() -> ok.
subscribe_self_to_reload_check() ->
    ok = beamtalk_repl_subscriptions:subscribe(reload_check, self()).

-doc """
Wait (up to 2 s) for a `'ReloadCheckCompleted'` announcement to reach this
process's mailbox and return its event. Announcements are async
(`beamtalk_announcements:system_announce/2` dispatches via the async veneer
path), so a test that triggers a recheck must not assert on the mailbox
immediately.
""".
-spec receive_reload_check_announcement() -> term().
receive_reload_check_announcement() ->
    receive
        {beamtalk_announcement, _SubRef, 'ReloadCheckCompleted', _Handler, Event} ->
            Event
    after 2000 ->
        error(timeout_waiting_for_reload_check_announcement)
    end.

%%====================================================================
%% Dependents: `<Prefix>Dashboard` / `<Prefix>ListView` reading `<Prefix>Counter`
%%====================================================================

-doc """
xref payload: `<Prefix>Dashboard>>refresh:` sends `size` to a
`<Prefix>Counter`-typed parameter — the real dependent.
""".
-spec dashboard_xref(binary()) -> [map()].
dashboard_xref(Prefix) ->
    send_site('refresh:', size, [counter(Prefix)]).

-spec dashboard_source(binary()) -> binary().
dashboard_source(Prefix) ->
    <<
        "Object subclass: ",
        Prefix/binary,
        "Dashboard\n"
        "  refresh: c :: ",
        Prefix/binary,
        "Counter -> Integer => (c size) + 1\n"
    >>.

-doc """
xref payload: `<Prefix>ListView>>render:` also sends `size`. `References` is
the classes the site references: `[]` for an unrelated same-selector site
(a `List`-typed parameter, see `listview_list_source/1`), or
`[counter_class]` for a second real dependent (see `listview_counter_source/1`).
""".
-spec listview_xref([atom()]) -> [map()].
listview_xref(References) ->
    send_site('render:', size, References).

-doc "A second real dependent of `size`: `render:` takes a `<Prefix>Counter`.".
-spec listview_counter_source(binary()) -> binary().
listview_counter_source(Prefix) ->
    <<
        "Object subclass: ",
        Prefix/binary,
        "ListView\n"
        "  render: c :: ",
        Prefix/binary,
        "Counter -> Integer => (c size) + 1\n"
    >>.

-doc "An unrelated same-selector site: `render:` takes a `List`.".
-spec listview_list_source(binary()) -> binary().
listview_list_source(Prefix) ->
    <<
        "Object subclass: ",
        Prefix/binary,
        "ListView\n"
        "  render: xs :: List -> Integer => (xs size) + 1\n"
    >>.

-doc """
xref payload: `<Prefix>Dashboard>>cleanup:` sends the removed accessor
selector `name` to a `<Prefix>Counter`-typed parameter.
""".
-spec dashboard_name_accessor_xref(binary()) -> [map()].
dashboard_name_accessor_xref(Prefix) ->
    send_site('cleanup:', name, [counter(Prefix)]).

-spec dashboard_name_accessor_source(binary()) -> binary().
dashboard_name_accessor_source(Prefix) ->
    <<
        "Object subclass: ",
        Prefix/binary,
        "Dashboard\n"
        "  cleanup: c :: ",
        Prefix/binary,
        "Counter => c name\n"
    >>.

%%====================================================================
%% Internal
%%====================================================================

-spec counter(binary()) -> atom().
counter(Prefix) ->
    binary_to_atom(<<Prefix/binary, "Counter">>).

%% One xref method entry: `Selector` (instance side, line 2) sending
%% `Sent` and referencing each class in `References`.
-spec send_site(atom(), atom(), [atom()]) -> [map()].
send_site(Selector, Sent, References) ->
    [
        #{
            class_side => false,
            selector => Selector,
            line => 2,
            sends => [#{selector => Sent, line => 2, recv_kind => other}],
            references => [#{class => Class, line => 2} || Class <- References],
            source_status => indexed,
            provenance => class_body
        }
    ].
