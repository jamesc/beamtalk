%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_state_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
The workspace-state ops (BT-5204): `flush`, `changes` and `info`.

Driven through `beamtalk_repl_ops:request/3`, the entry point LiveView and MCP
use, so this is the one runtime test of "flush the ChangeLog" including the
destructive-flush confirmation rules every surface now shares.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

state_ops_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(Ctx) -> {"flush writes a pending method edit", fun() -> flush_writes(Ctx) end} end,
        fun(Ctx) ->
            {"flush withholds a destructive entry unless confirm_destructive is true", fun() ->
                flush_destructive_gate(Ctx)
            end}
        end,
        fun(Ctx) ->
            {"flush refuses a confirm_destructive that is not a boolean", fun() ->
                flush_rejects_non_boolean_confirm(Ctx)
            end}
        end,
        fun(Ctx) ->
            {"unscoped flush with confirm_destructive applies Tier 2", fun() ->
                flush_unscoped_confirmed(Ctx)
            end}
        end,
        fun(Ctx) ->
            {"flush scoped by file and by kind", fun() -> flush_file_and_kind_scopes(Ctx) end}
        end,
        fun(_Ctx) ->
            {"flush scopes are mutually exclusive", fun flush_scopes_exclusive/0}
        end,
        fun(Ctx) ->
            {"flush refuses a mistyped scope instead of widening it", fun() ->
                flush_rejects_mistyped_scope(Ctx)
            end}
        end,
        fun(Ctx) ->
            {"flush refuses an empty-string scope", fun() -> flush_rejects_empty_scope(Ctx) end}
        end,
        fun(Ctx) ->
            {"flush scoped by selector, alone and with a class", fun() ->
                flush_selector_scope(Ctx)
            end}
        end,
        fun(Ctx) ->
            {"flush selector refuses file/kind and an empty selector", fun() ->
                flush_selector_refusals(Ctx)
            end}
        end,
        fun(_Ctx) ->
            {"flush with an unknown kind is a structured error", fun flush_unknown_kind/0}
        end,
        fun(Ctx) ->
            {"changes lists entries wire-shaped", fun() -> changes_entries(Ctx) end}
        end,
        fun(_Ctx) -> {"changes of an empty log is an empty list", fun changes_empty/0} end,
        fun(_Ctx) -> {"info answers version, node, workspace id, settings", fun info_shape/0} end,
        fun(_Ctx) -> {"info reads the autoflush setting", fun info_autoflush/0} end,
        fun(_Ctx) -> {"results encode as JSON at the edge", fun results_encode_as_json/0} end,
        fun(_Ctx) -> {"wire/1 projects runtime terms", fun wire_projection/0} end
    ]}.

setup() ->
    {WorkspaceId, Tmp, OldHome} = beamtalk_test_fs:changelog_workspace(),
    {ok, ClogPid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    ProjDir = filename:join(Tmp, "proj"),
    ok = filelib:ensure_path(filename:join(ProjDir, "src")),
    beamtalk_test_proc:stop_registered(beamtalk_workspace_meta),
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WorkspaceId,
        project_path => list_to_binary(ProjDir),
        created_at => erlang:system_time(second),
        mode => run
    }),
    #{
        clog => ClogPid,
        meta => MetaPid,
        workspace_id => WorkspaceId,
        tmp => Tmp,
        old_home => OldHome,
        proj_dir => ProjDir
    }.

cleanup(#{clog := ClogPid, meta := MetaPid, tmp := Tmp, old_home := OldHome}) ->
    beamtalk_test_proc:shutdown(MetaPid),
    beamtalk_test_proc:shutdown(ClogPid),
    beamtalk_test_fs:restore_home(OldHome),
    beamtalk_test_fs:rm_rf(Tmp),
    ok.

%%% Helpers

request(Op, Params) ->
    beamtalk_repl_ops:request(Op, Params, self()).

%% A pending patch of `Counter>>value` in a fresh file; answers the file path.
seed_patch(ProjDir, Name) ->
    File = filename:join([ProjDir, "src", Name]),
    Original = <<"Object subclass: Counter\n  value => 0\nend\n">>,
    ok = file:write_file(File, Original),
    {Start, Len} = binary:match(Original, <<"value => 0\n">>),
    {ok, _} = beamtalk_workspace_changelog:append(#{
        class => <<"Counter">>,
        selector => <<"value">>,
        kind => instance,
        source => <<"value => 42\n">>,
        prev_source => <<"value => 0\n">>,
        intent => durable,
        flushable => true,
        author => <<"sess-test">>,
        author_kind => human,
        source_file => list_to_binary(File),
        span => #{start => Start, 'end' => Start + Len}
    }),
    File.

%% A pending `remove-class` entry (Tier 2) for `Class` backed by a fresh file.
seed_remove_class(ProjDir, Class) ->
    File = filename:join([ProjDir, "src", binary_to_list(Class) ++ ".bt"]),
    Original = iolist_to_binary(["Object subclass: ", Class, "\n  foo => 1\nend\n"]),
    ok = file:write_file(File, Original),
    {ok, Seq} = beamtalk_workspace_changelog:append(#{
        class => Class,
        kind => 'remove-class',
        intent => durable,
        flushable => true,
        author => <<"sess-test">>,
        author_kind => human,
        source_file => list_to_binary(File),
        prev_source => Original
    }),
    {File, Seq}.

%%% flush

flush_writes(#{proj_dir := ProjDir}) ->
    File = seed_patch(ProjDir, "counter.bt"),
    {value, Summary} = request(<<"flush">>, #{}),
    ?assertEqual(1, maps:get(<<"flushed">>, Summary)),
    ?assertEqual([list_to_binary(File)], maps:get(<<"files">>, Summary)),
    ?assertEqual([], maps:get(<<"conflicts">>, Summary)),
    ?assertEqual([], maps:get(<<"skipped">>, Summary)),
    ?assertNot(maps:is_key(<<"$beamtalk_class">>, Summary)),
    {ok, Written} = file:read_file(File),
    ?assertMatch({_, _}, binary:match(Written, <<"value => 42">>)).

%% The confirmation rules: a Tier 2 entry is only applied by an explicit `true`.
flush_destructive_gate(#{proj_dir := ProjDir}) ->
    {File, Seq} = seed_remove_class(ProjDir, <<"Gone">>),
    %% Neither the default nor an explicit false applies it; it is reported.
    {value, S1} = request(<<"flush">>, #{}),
    ?assertEqual(0, maps:get(<<"flushed">>, S1)),
    ?assertMatch(
        [#{<<"reason">> := <<"destructive">>, <<"seq">> := Seq, <<"class">> := <<"Gone">>}],
        maps:get(<<"skipped">>, S1)
    ),
    {value, S2} = request(<<"flush">>, #{
        <<"class">> => <<"Gone">>, <<"confirm_destructive">> => false
    }),
    ?assertEqual(0, maps:get(<<"flushed">>, S2)),
    ?assert(filelib:is_regular(File)),
    %% A scoped confirmation for another class does not reach it.
    {value, S3} = request(<<"flush">>, #{
        <<"class">> => <<"Other">>, <<"confirm_destructive">> => true
    }),
    ?assertEqual(0, maps:get(<<"flushed">>, S3)),
    ?assert(filelib:is_regular(File)),
    %% Scoped and confirmed: applied.
    {value, S4} = request(<<"flush">>, #{
        <<"class">> => <<"Gone">>, <<"confirm_destructive">> => true
    }),
    ?assertEqual(1, maps:get(<<"flushed">>, S4)),
    ?assertEqual(1, maps:get(<<"removedClasses">>, S4)),
    ?assertNot(filelib:is_regular(File)).

flush_rejects_non_boolean_confirm(#{proj_dir := ProjDir}) ->
    {File, _Seq} = seed_remove_class(ProjDir, <<"Gone">>),
    %% A string "true" is not `true`: refused, nothing deleted.
    lists:foreach(
        fun(Bad) ->
            ?assertMatch(
                {error, #beamtalk_error{kind = invalid_argument}},
                request(<<"flush">>, #{<<"confirm_destructive">> => Bad})
            )
        end,
        [<<"true">>, 1, null]
    ),
    ?assert(filelib:is_regular(File)).

flush_unscoped_confirmed(#{proj_dir := ProjDir}) ->
    {File, _Seq} = seed_remove_class(ProjDir, <<"Gone">>),
    {value, Summary} = request(<<"flush">>, #{<<"confirm_destructive">> => true}),
    ?assertEqual(1, maps:get(<<"flushed">>, Summary)),
    ?assertNot(filelib:is_regular(File)).

flush_file_and_kind_scopes(#{proj_dir := ProjDir}) ->
    FileA = seed_patch(ProjDir, "a.bt"),
    {FileB, _} = seed_remove_class(ProjDir, <<"Gone">>),
    %% Scoped to another file: nothing flushed.
    {value, S1} = request(<<"flush">>, #{<<"file">> => <<"/nowhere/else.bt">>}),
    ?assertEqual(0, maps:get(<<"flushed">>, S1)),
    %% `kind` accepts the bare name and the `#'..'` Symbol spelling.
    {value, S2} = request(<<"flush">>, #{
        <<"kind">> => <<"#'remove-class'">>, <<"confirm_destructive">> => true
    }),
    ?assertEqual(1, maps:get(<<"flushed">>, S2)),
    ?assertNot(filelib:is_regular(FileB)),
    {value, S3} = request(<<"flush">>, #{<<"file">> => list_to_binary(FileA)}),
    ?assertEqual(1, maps:get(<<"flushed">>, S3)).

flush_scopes_exclusive() ->
    ?assertMatch(
        {error, #beamtalk_error{kind = invalid_argument}},
        request(<<"flush">>, #{<<"class">> => <<"Counter">>, <<"file">> => <<"a.bt">>})
    ),
    ?assertMatch(
        {error, #beamtalk_error{kind = invalid_argument}},
        request(<<"flush">>, #{<<"file">> => <<"a.bt">>, <<"kind">> => <<"new-class">>})
    ).

%% A wrongly typed scope must not widen a scoped flush into an unscoped one.
flush_rejects_mistyped_scope(#{proj_dir := ProjDir}) ->
    {File, _Seq} = seed_remove_class(ProjDir, <<"Gone">>),
    lists:foreach(
        fun(Key) ->
            lists:foreach(
                fun(Bad) ->
                    ?assertMatch(
                        {error, #beamtalk_error{kind = invalid_argument}},
                        request(<<"flush">>, #{Key => Bad, <<"confirm_destructive">> => true})
                    )
                end,
                [123, null, [<<"Gone">>], #{}]
            )
        end,
        [<<"class">>, <<"file">>, <<"kind">>]
    ),
    ?assert(filelib:is_regular(File)).

%% BT-5206: an empty-string scope used to read as "no scope" and flush
%% everything; it is refused, with nothing written.
flush_rejects_empty_scope(#{proj_dir := ProjDir}) ->
    {File, _Seq} = seed_remove_class(ProjDir, <<"GoneEmpty">>),
    lists:foreach(
        fun(Key) ->
            ?assertMatch(
                {error, #beamtalk_error{kind = invalid_argument}},
                request(<<"flush">>, #{Key => <<>>, <<"confirm_destructive">> => true})
            )
        end,
        [<<"class">>, <<"file">>, <<"kind">>]
    ),
    ?assert(filelib:is_regular(File)).

%% BT-5297: a selector scope flushes that method only; with a class it flushes
%% that class's method only.
flush_selector_scope(#{proj_dir := ProjDir}) ->
    File = seed_patch(ProjDir, "sel.bt"),
    %% Another selector, or the same selector in another class: nothing flushed.
    {value, S1} = request(<<"flush">>, #{<<"selector">> => <<"other">>}),
    ?assertEqual(0, maps:get(<<"flushed">>, S1)),
    {value, S2} = request(<<"flush">>, #{
        <<"selector">> => <<"value">>, <<"class">> => <<"Elsewhere">>
    }),
    ?assertEqual(0, maps:get(<<"flushed">>, S2)),
    {ok, Before} = file:read_file(File),
    ?assertNotEqual(nomatch, binary:match(Before, <<"value => 0">>)),
    %% `#value` is the same selector as `value`.
    {value, S3} = request(<<"flush">>, #{
        <<"selector">> => <<"#value">>, <<"class">> => <<"Counter">>
    }),
    ?assertEqual(1, maps:get(<<"flushed">>, S3)),
    {ok, After} = file:read_file(File),
    ?assertNotEqual(nomatch, binary:match(After, <<"value => 42">>)),
    %% Selector alone matches too.
    _ = seed_patch(ProjDir, "sel2.bt"),
    {value, S4} = request(<<"flush">>, #{<<"selector">> => <<"value">>}),
    ?assertEqual(1, maps:get(<<"flushed">>, S4)).

flush_selector_refusals(#{proj_dir := ProjDir}) ->
    {File, _} = seed_remove_class(ProjDir, <<"GoneSel">>),
    lists:foreach(
        fun(Params) ->
            ?assertMatch(
                {error, #beamtalk_error{kind = invalid_argument}},
                request(<<"flush">>, Params#{<<"confirm_destructive">> => true})
            )
        end,
        [
            #{<<"selector">> => <<"value">>, <<"file">> => <<"a.bt">>},
            #{<<"selector">> => <<"value">>, <<"kind">> => <<"new-class">>},
            #{<<"selector">> => <<>>},
            #{<<"selector">> => 42}
        ]
    ),
    ?assert(filelib:is_regular(File)).

flush_unknown_kind() ->
    ?assertMatch(
        {error, #beamtalk_error{kind = invalid_argument}},
        request(<<"flush">>, #{<<"kind">> => <<"no-such-kind-bt5204">>})
    ),
    %% An atom that exists but is no entry kind (a class name, `ok`, a selector
    %% such as `instance`) is not read as a class or selector scope.
    lists:foreach(
        fun(Name) ->
            ?assertMatch(
                {error, #beamtalk_error{kind = invalid_argument}},
                request(<<"flush">>, #{<<"kind">> => Name})
            )
        end,
        [<<"Object">>, <<"ok">>, <<"instance">>]
    ).

%%% changes

changes_entries(#{proj_dir := ProjDir}) ->
    File = seed_patch(ProjDir, "counter.bt"),
    {value, #{<<"entries">> := [Entry]}} = request(<<"changes">>, #{}),
    ?assertEqual(<<"Counter">>, maps:get(<<"className">>, Entry)),
    ?assertEqual(<<"value">>, maps:get(<<"selector">>, Entry)),
    ?assertEqual(<<"instance">>, maps:get(<<"kind">>, Entry)),
    ?assertEqual(<<"human">>, maps:get(<<"authorKind">>, Entry)),
    ?assertEqual(true, maps:get(<<"active">>, Entry)),
    ?assertEqual(true, maps:get(<<"flushable">>, Entry)),
    ?assertEqual(false, maps:get(<<"flushed">>, Entry)),
    ?assertEqual(false, maps:get(<<"shadowed">>, Entry)),
    ?assertEqual(list_to_binary(File), maps:get(<<"sourceFile">>, Entry)),
    %% `diff` is the net unified diff, or null when none is computable.
    ?assert(
        lists:member(maps:get(<<"diff">>, Entry), [null]) orelse
            is_binary(maps:get(<<"diff">>, Entry))
    ),
    %% An absent value is JSON null, and the internal class tag is not exposed.
    ?assertEqual(null, maps:get(<<"oldClass">>, Entry)),
    ?assertNot(maps:is_key(<<"$beamtalk_class">>, Entry)),
    %% A flushed entry stays listed, flagged, and is no longer active.
    {value, _} = request(<<"flush">>, #{}),
    {value, #{<<"entries">> := [After]}} = request(<<"changes">>, #{}),
    ?assertEqual(true, maps:get(<<"flushed">>, After)),
    ?assertEqual(false, maps:get(<<"active">>, After)).

changes_empty() ->
    ?assertEqual({value, #{<<"entries">> => []}}, request(<<"changes">>, #{})).

%%% info

info_shape() ->
    {value, Info} = request(<<"info">>, #{}),
    Version = maps:get(<<"version">>, Info),
    [?assert(is_binary(maps:get(K, Version))) || K <- version_keys()],
    ?assertEqual(
        wire_version(beamtalk_version:get()),
        Version
    ),
    ?assertEqual(atom_to_binary(node(), utf8), maps:get(<<"node">>, Info)),
    ?assert(is_binary(maps:get(<<"workspace_id">>, Info))),
    ?assertNotEqual(<<>>, maps:get(<<"workspace_id">>, Info)),
    ?assertEqual(#{<<"autoflush">> => false}, maps:get(<<"settings">>, Info)).

info_autoflush() ->
    ok = beamtalk_workspace_meta:set_setting(autoflush, true),
    {value, #{<<"settings">> := #{<<"autoflush">> := On}}} = request(<<"info">>, #{}),
    ?assertEqual(true, On),
    ok = beamtalk_workspace_meta:set_setting(autoflush, false),
    {value, #{<<"settings">> := #{<<"autoflush">> := Off}}} = request(<<"info">>, #{}),
    ?assertEqual(false, Off).

version_keys() ->
    [
        <<"runtime_version">>,
        <<"protocol_version">>,
        <<"otp_release">>,
        <<"erts_version">>
    ].

wire_version(Report) ->
    maps:from_list([{atom_to_binary(K, utf8), V} || {K, V} <- maps:to_list(Report)]).

%%% wire encoding

%% The WebSocket edge encodes the same terms the dist-attached callers get.
results_encode_as_json() ->
    lists:foreach(
        fun(Op) ->
            {value, V} = request(Op, #{}),
            ?assert(is_binary(iolist_to_binary(json:encode(V))))
        end,
        [<<"flush">>, <<"changes">>, <<"info">>]
    ).

wire_projection() ->
    Wire = fun beamtalk_repl_ops_state:wire/1,
    ?assertEqual(
        #{
            <<"a">> => null,
            <<"b">> => null,
            <<"c">> => true,
            <<"d">> => <<"sym">>,
            <<"e">> => [1, <<"x">>],
            <<"f">> => #{<<"g">> => 2}
        },
        Wire(#{
            ?BT_CLASS_TAG_KEY => 'Tagged',
            a => nil,
            b => undefined,
            c => true,
            d => sym,
            e => [1, <<"x">>],
            f => #{g => 2}
        })
    ),
    %% Anything with no wire form is text, never a crash.
    ?assert(is_binary(Wire({a, tuple}))).
