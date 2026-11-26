%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_edit_tests).

%%% **DDD Context:** REPL Session Context

-moduledoc """
The code-editing ops (BT-5203): `save-method`, `save-class`, `revert-method`,
`reload-file`.

These are driven through `beamtalk_repl_ops:request/3`, the entry point
LiveView and MCP use, so this is the one runtime test of "save this method":
validation, install, ChangeLog recording and the structured error all happen
here, once, for every surface that calls the op.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

edit_ops_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"save-method installs a method the live image runs", fun save_method_installs/0},
        {"save-method takes quotes, braces and backslashes as a value",
            fun save_method_special_characters/0},
        {"save-method stamps the ChangeLog entry with the given author",
            fun save_method_records_author/0},
        {"save-method on an unknown class is a structured error", fun save_method_unknown_class/0},
        {"save-method rejects a bad author_kind", fun save_method_bad_author_kind/0},
        {"each op names the missing param in a structured error", fun missing_params/0},
        {"revert-method answers what the revert chokepoint answers", fun revert_undoes_save/0},
        {"revert-method with nothing to revert is a structured error",
            fun revert_nothing_to_revert/0},
        {"save-class refuses a path outside the project", fun save_class_bad_path/0},
        {"reload-file reloads a file's classes", fun reload_file_reloads/0},
        {"save-class returns no warnings key when the ChangeLog append is clean",
            fun save_class_clean_has_no_warnings/0},
        {"revert-method rejects an unrecognised side", fun revert_bad_side/0},
        {"reload-file refuses absolute, escaping and non-.bt paths", fun reload_file_bad_paths/0},
        {"reload-file of a missing file is a structured error", fun reload_file_missing/0}
    ]}.

setup() ->
    beamtalk_test_boot:compiler(),
    {ok, _} = beamtalk_test_boot:start_app(beamtalk_runtime),
    ok = beamtalk_test_wait:class('Object'),
    {WorkspaceId, Tmp, OldHome} = beamtalk_test_fs:changelog_workspace(),
    {ok, ClogPid} = beamtalk_workspace_changelog:start_link(#{workspace_id => WorkspaceId}),
    beamtalk_test_proc:stop_registered(beamtalk_workspace_meta),
    {ok, MetaPid} = beamtalk_workspace_meta:start_link(#{
        workspace_id => WorkspaceId,
        project_path => list_to_binary(Tmp),
        created_at => erlang:system_time(second),
        mode => run
    }),
    case whereis(beamtalk_workspace_signature_store) of
        undefined -> {ok, _} = beamtalk_workspace_signature_store:start_link();
        _ -> ok
    end,
    #{clog => ClogPid, meta => MetaPid, tmp => Tmp, old_home => OldHome}.

cleanup(#{clog := ClogPid, meta := MetaPid, tmp := Tmp, old_home := OldHome}) ->
    _ = application:stop(beamtalk_compiler),
    beamtalk_test_proc:shutdown(MetaPid),
    beamtalk_test_proc:shutdown(ClogPid),
    beamtalk_test_proc:stop_registered(beamtalk_workspace_signature_store),
    beamtalk_test_fs:restore_home(OldHome),
    _ = file:del_dir_r(Tmp),
    ok.

%%% Helpers

request(Op, Params) ->
    beamtalk_repl_ops:request(Op, Params, self()).

%% Define `Name` (an `Object subclass:` with `v => 1`) and record its source, the
%% way `Workspace load:` does for a file-backed class.
define_class(Name) ->
    Source = iolist_to_binary(["Value subclass: ", Name, "\n  v => 1"]),
    {ok, _, _, _, _} = beamtalk_repl_eval:do_eval(
        binary_to_list(Source), beamtalk_repl_state:new(undefined, 0)
    ),
    ok = beamtalk_workspace_meta:set_class_source(Name, Source),
    ok.

%% The value `Name new v` answers right now.
current_v(Name) ->
    Code = binary_to_list(iolist_to_binary([Name, " new v"])),
    {ok, Value, _, _, _} = beamtalk_repl_eval:do_eval(Code, beamtalk_repl_state:new(undefined, 0)),
    Value.

%%% Tests

save_method_installs() ->
    define_class(<<"EditOpsInstall">>),
    ?assertEqual(1, current_v(<<"EditOpsInstall">>)),
    ?assertEqual(
        {value, #{<<"class">> => <<"EditOpsInstall">>, <<"selector">> => <<"v">>}},
        request(<<"save-method">>, #{
            <<"class">> => <<"EditOpsInstall">>,
            <<"selector">> => <<"v">>,
            <<"source">> => <<"2">>
        })
    ),
    ?assertEqual(2, current_v(<<"EditOpsInstall">>)).

save_method_special_characters() ->
    %% The body rides as a value, so nothing in it needs escaping: a quote, a
    %% brace and a backslash in a leading comment reach the compiler untouched.
    define_class(<<"EditOpsSpecial">>),
    Body = <<"// he said \"hi\" {x} \\ end\nv => 5">>,
    ?assertMatch(
        {value, #{<<"class">> := <<"EditOpsSpecial">>}},
        request(<<"save-method">>, #{
            <<"class">> => <<"EditOpsSpecial">>,
            <<"selector">> => <<"v">>,
            <<"source">> => Body
        })
    ),
    ?assertEqual(5, current_v(<<"EditOpsSpecial">>)).

save_method_records_author() ->
    %% Without author params the entry is a human REPL patch; with them it
    %% carries the caller's kind, the way LiveView / MCP identify themselves.
    define_class(<<"EditOpsAuthor">>),
    {value, _} = request(<<"save-method">>, #{
        <<"class">> => <<"EditOpsAuthor">>,
        <<"selector">> => <<"v">>,
        <<"source">> => <<"3">>,
        <<"author">> => <<"mcp">>,
        <<"author_kind">> => <<"agent">>
    }),
    {value, _} = request(<<"save-method">>, #{
        <<"class">> => <<"EditOpsAuthor">>,
        <<"selector">> => <<"v">>,
        <<"source">> => <<"4">>
    }),
    Kinds = [
        beamtalk_workspace_changelog:entry_author_kind(E)
     || E <- beamtalk_workspace_changelog:entries(),
        beamtalk_workspace_changelog:entry_class(E) =:= <<"EditOpsAuthor">>
    ],
    ?assertEqual([agent, human], lists:sort(Kinds)).

save_method_unknown_class() ->
    Result = request(<<"save-method">>, #{
        <<"class">> => <<"EditOpsNoSuchClass">>,
        <<"selector">> => <<"v">>,
        <<"source">> => <<"1">>
    }),
    ?assertMatch({error, #beamtalk_error{}}, Result).

save_method_bad_author_kind() ->
    define_class(<<"EditOpsKind">>),
    ?assertMatch(
        {error, #beamtalk_error{kind = invalid_argument}},
        request(<<"save-method">>, #{
            <<"class">> => <<"EditOpsKind">>,
            <<"selector">> => <<"v">>,
            <<"source">> => <<"3">>,
            <<"author_kind">> => <<"robot">>
        })
    ),
    %% Refused before any install: the method is unchanged.
    ?assertEqual(1, current_v(<<"EditOpsKind">>)).

missing_params() ->
    [
        ?assertMatch(
            {error, #beamtalk_error{kind = invalid_argument}}, request(Op, Params)
        )
     || {Op, Params} <- [
            {<<"save-method">>, #{<<"class">> => <<"C">>, <<"selector">> => <<"v">>}},
            {<<"save-method">>, #{
                <<"class">> => <<>>, <<"selector">> => <<"v">>, <<"source">> => <<"1">>
            }},
            {<<"save-class">>, #{<<"source">> => <<"Object subclass: Foo">>}},
            {<<"revert-method">>, #{<<"class">> => <<"C">>}},
            {<<"reload-file">>, #{}}
        ]
    ],
    {error, Err} = request(<<"save-class">>, #{<<"source">> => <<"Object subclass: Foo">>}),
    ?assertNotEqual(nomatch, binary:match(element(2, Err#beamtalk_error.message), <<"path">>)).

revert_undoes_save() ->
    %% The op reaches the revert chokepoint for the entry `save-method` just
    %% logged. A class defined from a string has no recoverable prior body in
    %% this harness, so the chokepoint answers `revert_not_possible`; the op must
    %% hand back that same structured error, side included. A successful revert
    %% needs a flushed project file and is covered by the repl-protocol e2e
    %% (`Workspace changes revert:`), which shares this chokepoint.
    define_class(<<"EditOpsRevert">>),
    {value, _} = request(<<"save-method">>, #{
        <<"class">> => <<"EditOpsRevert">>,
        <<"selector">> => <<"v">>,
        <<"source">> => <<"9">>
    }),
    Direct = beamtalk_workspace_interface_primitives:revert_method(
        <<"EditOpsRevert">>, <<"v">>, <<"instance">>
    ),
    ?assertMatch({error, #beamtalk_error{kind = revert_not_possible}}, Direct),
    ?assertEqual(
        Direct,
        request(<<"revert-method">>, #{
            <<"class">> => <<"EditOpsRevert">>,
            <<"selector">> => <<"v">>,
            <<"side">> => <<"instance">>
        })
    ).

revert_nothing_to_revert() ->
    ?assertMatch(
        {error, #beamtalk_error{}},
        request(<<"revert-method">>, #{
            <<"class">> => <<"EditOpsNeverSaved">>, <<"selector">> => <<"v">>
        })
    ).

save_class_bad_path() ->
    %% The loader's own validation answers: a path outside the project tree is
    %% refused with a structured error and nothing is created.
    Result = request(<<"save-class">>, #{
        <<"source">> => <<"Object subclass: EditOpsOutside">>,
        <<"path">> => <<"../outside/EditOpsOutside.bt">>
    }),
    ?assertMatch({error, #beamtalk_error{}}, Result).

reload_file_reloads() ->
    File = "edit_ops_reload_tmp.bt",
    ok = file:write_file(File, <<"Value subclass: EditOpsReloaded\n  v => 11\n">>),
    try
        ?assertEqual(
            {value, #{<<"classes">> => [<<"EditOpsReloaded">>]}},
            request(<<"reload-file">>, #{<<"path">> => list_to_binary(File)})
        ),
        ?assertEqual(11, current_v(<<"EditOpsReloaded">>))
    after
        _ = file:delete(File)
    end.

reload_file_missing() ->
    ?assertMatch(
        {error, #beamtalk_error{}},
        request(<<"reload-file">>, #{<<"path">> => <<"no/such/file.bt">>})
    ).

revert_bad_side() ->
    [
        ?assertMatch(
            {error, #beamtalk_error{kind = invalid_argument}},
            request(<<"revert-method">>, #{
                <<"class">> => <<"C">>, <<"selector">> => <<"v">>, <<"side">> => Side
            })
        )
     || Side <- [<<"Instance">>, <<>>, <<"meta">>]
    ].

reload_file_bad_paths() ->
    [
        ?assertMatch(
            {error, #beamtalk_error{kind = invalid_argument}},
            request(<<"reload-file">>, #{<<"path">> => Path})
        )
     || Path <- [<<"/etc/passwd">>, <<"../x.bt">>, <<"src/../../x.bt">>, <<"src/Counter.erl">>]
    ].

save_class_clean_has_no_warnings() ->
    %% The failure path (`{warning, _}` from the ChangeLog append) is queued by
    %% `note_changelog_result/1`; here we pin that the op collects around the call
    %% and leaves the process unmarked afterwards.
    _ = request(<<"save-class">>, #{
        <<"source">> => <<"Value subclass: EditOpsWarn">>,
        <<"path">> => <<"../outside/EditOpsWarn.bt">>
    }),
    ?assertEqual(undefined, erlang:get('$beamtalk_changelog_eval')),
    ok = beamtalk_refactor_support:begin_changelog_warnings(),
    ok = beamtalk_refactor_support:note_changelog_result({warning, <<"w">>}),
    ?assertEqual([<<"w">>], beamtalk_refactor_support:end_changelog_warnings()).
