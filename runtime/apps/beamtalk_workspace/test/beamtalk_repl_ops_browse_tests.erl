%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_browse_tests).
-compile(nowarn_deprecated_catch).

%%% **DDD Context:** REPL Session Context (System Browser bridge)

-moduledoc """
EUnit tests for `beamtalk_repl_ops_browse` (ADR 0096, BT-2488).

Covers:

* `describe_ops/0` shape — the four `browse-*` keys and their params.
* Validation error branches for all three parameterised ops (missing/empty
  class, unknown class, missing/bad `side`, missing `selector`).
* Success paths via `handle/4` against a live class object + a seeded
  `beamtalk_xref` fixture: `browse-classes` rows, `browse-protocols` protocol
  grouping with `line` / `source_status` / `origin`, `browse-method-source`
  image source + `disk_differs`, `browse-class-definition` header + state slots.
* The `{value, _}` term contract (success returns `{value, Map}`, encoded as a
  `status: [done]` JSON envelope).
* The ADR 0091 no-user-code guarantee: an instance method whose body would
  crash if *run* is enumerated/source-read without ever being invoked.
""".

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helpers
%%====================================================================

%% Construct a minimal protocol_msg record (mirrors nav tests).
make_msg() ->
    {protocol_msg, <<"browse">>, <<"t1">>, <<"s1">>, #{}}.

%% Decode a JSON binary and assert it represents an error response.
assert_error_response(Response) ->
    Decoded = json:decode(Response),
    ?assertMatch(#{<<"status">> := [<<"done">>, <<"error">>]}, Decoded),
    ?assert(is_binary(maps:get(<<"error">>, Decoded))),
    Decoded.

%% Decode a JSON binary and assert it is a successful `value`-tagged response,
%% returning the inner value.
decode_value(Response) ->
    Decoded = json:decode(Response),
    ?assertMatch(#{<<"status">> := [<<"done">>]}, Decoded),
    maps:get(<<"value">>, Decoded).

%%====================================================================
%% describe_ops/0
%%====================================================================

describe_ops_has_browse_keys_test() ->
    Ops = beamtalk_repl_ops_browse:describe_ops(),
    ?assert(maps:is_key(<<"browse-classes">>, Ops)),
    ?assert(maps:is_key(<<"browse-protocols">>, Ops)),
    ?assert(maps:is_key(<<"browse-method-source">>, Ops)),
    ?assert(maps:is_key(<<"browse-class-definition">>, Ops)),
    ?assert(maps:is_key(<<"browse-native-source">>, Ops)).

describe_ops_protocols_requires_class_and_side_test() ->
    #{<<"browse-protocols">> := Info} = beamtalk_repl_ops_browse:describe_ops(),
    Params = maps:get(<<"params">>, Info),
    ?assert(lists:member(<<"class">>, Params)),
    ?assert(lists:member(<<"side">>, Params)).

describe_ops_method_source_requires_class_side_selector_test() ->
    #{<<"browse-method-source">> := Info} = beamtalk_repl_ops_browse:describe_ops(),
    Params = maps:get(<<"params">>, Info),
    ?assert(lists:member(<<"class">>, Params)),
    ?assert(lists:member(<<"side">>, Params)),
    ?assert(lists:member(<<"selector">>, Params)).

%%====================================================================
%% browse-native-source — describe + pure helpers (BT-2578)
%%====================================================================

describe_ops_has_native_source_key_test() ->
    Ops = beamtalk_repl_ops_browse:describe_ops(),
    ?assert(maps:is_key(<<"browse-native-source">>, Ops)),
    #{<<"browse-native-source">> := Info} = Ops,
    ?assert(lists:member(<<"class">>, maps:get(<<"params">>, Info))).

%% The `handle_call` clause line-map: quoted-atom and bare-atom selectors are
%% captured with their 1-based line; generic clauses (uppercase var, catch-all)
%% are skipped. Mirrors the real `beamtalk_subprocess` clause shapes.
native_handle_call_clause_lines_test() ->
    Content = iolist_to_binary([
        "init(Config) -> {ok, #{}}.\n",
        "handle_call({Selector, Args, Ctx}, From, State) ->\n",
        "    handle_call({Selector, Args}, From, State);\n",
        "handle_call({'writeLine:', [Data]}, _From, State) ->\n",
        "    {reply, nil, State};\n",
        "handle_call({readLine, []}, From, State) ->\n",
        "    {noreply, register_waiter(stdout, From, State)};\n",
        "handle_call(Msg, _From, State) ->\n",
        "    {reply, {error, unknown}, State}.\n"
    ]),
    Clauses = beamtalk_repl_ops_browse:handle_call_clause_lines(Content),
    Pairs = [{maps:get(<<"selector">>, C), maps:get(<<"line">>, C)} || C <- Clauses],
    %% Only the two concrete selectors, at their real lines (1-based).
    ?assertEqual([{<<"writeLine:">>, 4}, {<<"readLine">>, 6}], Pairs).

native_clause_lines_null_content_is_empty_test() ->
    ?assertEqual([], beamtalk_repl_ops_browse:handle_call_clause_lines(null)).

native_clause_selector_skips_generic_clauses_test() ->
    ?assertEqual(
        none,
        beamtalk_repl_ops_browse:clause_selector(
            <<"handle_call({Selector, Args, Ctx}, From, State) ->">>
        )
    ),
    ?assertEqual(
        none, beamtalk_repl_ops_browse:clause_selector(<<"handle_call(Msg, _From, State) ->">>)
    ),
    ?assertEqual(
        {ok, <<"readLine">>},
        beamtalk_repl_ops_browse:clause_selector(<<"handle_call({readLine, []}, From, S) ->">>)
    ),
    %% An indented clause head still matches (anchored to optional leading
    %% whitespace).
    ?assertEqual(
        {ok, <<"readLine">>},
        beamtalk_repl_ops_browse:clause_selector(<<"    handle_call({readLine, []}, F, S) ->">>)
    ),
    %% A `handle_call` mention mid-line is NOT a clause head: a comment or an
    %% assignment must not produce a spurious clause row.
    ?assertEqual(
        none,
        beamtalk_repl_ops_browse:clause_selector(<<"%% see handle_call({readLine, []}, ...)">>)
    ),
    ?assertEqual(
        none,
        beamtalk_repl_ops_browse:clause_selector(<<"    R = handle_call({readLine, []}, F, S),">>)
    ).

%% BT-2582 conformance: every case in the shared corpus must resolve the same
%% selector here as the Rust LSP's `clause_selector` does. The corpus is the
%% single source of truth both implementations are pinned to; the Rust side
%% asserts the identical cases in
%% `definition_provider::tests::clause_selector_matches_shared_corpus`.
clause_selector_corpus_test() ->
    Cases = load_clause_corpus(),
    ?assert(length(Cases) > 0),
    lists:foreach(
        fun(Case) ->
            Line = maps:get(<<"line">>, Case),
            Expected =
                case maps:get(<<"selector">>, Case) of
                    null -> none;
                    Sel when is_binary(Sel) -> {ok, Sel}
                end,
            Why = maps:get(<<"why">>, Case, <<>>),
            ?assertEqual(
                Expected,
                beamtalk_repl_ops_browse:clause_selector(Line),
                {corpus_mismatch, Line, Why}
            )
        end,
        Cases
    ).

%% Load the shared selector-conformance corpus from the repo tree. Walks up from
%% the test CWD to the project root (the dir holding `Cargo.toml`), then reads
%% the fixture both surfaces share.
load_clause_corpus() ->
    Root = find_project_root(filename:absname("")),
    Path = filename:join([
        Root,
        "runtime",
        "apps",
        "beamtalk_workspace",
        "test",
        "fixtures",
        "handle_call_clause_corpus.json"
    ]),
    Bin =
        case file:read_file(Path) of
            {ok, B} -> B;
            {error, Reason} -> error({corpus_file_unreadable, Path, Reason})
        end,
    json:decode(Bin).

find_project_root("/") ->
    error(project_root_not_found);
find_project_root(Dir) ->
    case filelib:is_regular(filename:join(Dir, "Cargo.toml")) of
        true -> Dir;
        false -> find_project_root(filename:dirname(Dir))
    end.

%% native_delegate is keyed off the facade's `dispatch_<selector>` exports — the
%% compiler's own `is_self_delegate` decision (native_facade.rs), not a body-text
%% guess. The dispatch name embeds the selector verbatim (keyword colon included),
%% so an exact name match distinguishes a delegate from a same-prefixed method.
native_delegate_exported_marker_test() ->
    %% Mirrors the real `beamtalk_subprocess` facade exports.
    Exports = [
        {spawn, 0},
        {dispatch_readLine, 1},
        {'dispatch_writeLine:', 2},
        {'__beamtalk_meta', 0}
    ],
    ?assert(beamtalk_repl_ops_browse:delegate_exported(Exports, readLine)),
    ?assert(beamtalk_repl_ops_browse:delegate_exported(Exports, 'writeLine:')),
    %% A non-delegate selector (no dispatch export) is not a delegate.
    ?assertNot(beamtalk_repl_ops_browse:delegate_exported(Exports, 'open:args:')),
    %% Exact match, not prefix: `readLine` must not be matched by a `readLine:`
    %% query (or vice versa).
    ?assertNot(beamtalk_repl_ops_browse:delegate_exported(Exports, 'readLine:')),
    ?assertNot(beamtalk_repl_ops_browse:delegate_exported([], readLine)).

%%====================================================================
%% delegate_callers_of_native_module/1 (BT-2732)
%%====================================================================

%% self_delegate_selectors/1 recovers the `self delegate` selectors from a facade's
%% `dispatch_<selector>` exports (native_facade.rs) — keyword colon included — and
%% ignores every non-`dispatch_` export (`spawn`, `__beamtalk_meta`, module_info).
self_delegate_selectors_extracts_dispatch_exports_test() ->
    Exports = beamtalk_test_native_facade:module_info(exports),
    ?assertEqual(
        [increment, 'incrementBy:'],
        beamtalk_repl_ops_browse:self_delegate_selectors(Exports)
    ).

%% A bare exports list: the prefix must be an exact `dispatch_` head, and the
%% stripped remainder must already be an interned selector atom — an unknown one
%% is skipped rather than growing the atom table.
self_delegate_selectors_filters_non_dispatch_test() ->
    ?assertEqual([], beamtalk_repl_ops_browse:self_delegate_selectors([])),
    %% `dispatcher`/`dispatch` share a prefix but are not `dispatch_<x>`.
    ?assertEqual(
        [],
        beamtalk_repl_ops_browse:self_delegate_selectors([{dispatcher, 0}, {dispatch, 1}])
    ),
    %% `dispatch_` + an atom that is not interned as a selector → skipped.
    ?assertEqual(
        [],
        beamtalk_repl_ops_browse:self_delegate_selectors([
            {'dispatch_totallyUninternedSelectorXyz', 1}
        ])
    ).

delegate_callers_test_() ->
    {setup, fun delegate_setup/0, fun delegate_cleanup/1, fun delegate_tests/1}.

%% A live class whose module is the test native facade (backing module
%% `bt_test_native_backing`), with its two `self delegate` instance methods seeded
%% into xref so `delegate_row/2` resolves their header lines. Unique class name per
%% invocation, isolated cleanup — mirrors `browse_setup/0` (no global xref clears).
delegate_setup() ->
    XrefPid =
        case whereis(beamtalk_xref) of
            undefined ->
                case beamtalk_xref:start_link() of
                    {ok, P} -> P;
                    {error, {already_started, P}} -> P
                end;
            P ->
                P
        end,
    %% Load the facade so its `__beamtalk_meta/0` + `dispatch_<selector>` exports
    %% are visible to the reflection reads, regardless of test ordering.
    {module, beamtalk_test_native_facade} = code:ensure_loaded(beamtalk_test_native_facade),
    Uniq = erlang:integer_to_list(erlang:unique_integer([positive, monotonic])),
    ClassName = list_to_atom("DelegateCounter_" ++ Uniq),
    {Pid, Owned} = start_browse_class(ClassName, #{
        name => ClassName,
        %% The facade module carries `__beamtalk_meta/0` (backing_module) and the
        %% `dispatch_<selector>` exports — the two reflection reads the delegate
        %% lookup depends on.
        module => beamtalk_test_native_facade,
        superclass => none,
        fields => [],
        instance_methods => #{
            'increment' => #{block => fun(_, _) -> ok end, arity => 0},
            'incrementBy:' => #{block => fun(_, _, _) -> ok end, arity => 1}
        }
    }),
    %% Seed the delegate methods' header lines (and intern the `'incrementBy:'`
    %% selector so `list_to_existing_atom/1` in the lookup resolves it).
    ok = beamtalk_xref:register_class(ClassName, [
        method_row('increment', 61, indexed, class_body),
        method_row('incrementBy:', 71, indexed, class_body)
    ]),
    #{xref => XrefPid, class_name => ClassName, class => {ClassName, Pid, Owned}}.

delegate_cleanup(#{class := {Name, Pid, Owned}}) ->
    catch beamtalk_xref:purge_class(Name),
    case Owned of
        true -> catch gen_server:stop(Pid);
        false -> ok
    end,
    ok.

delegate_tests(#{class_name := ClassName}) ->
    [
        {"lists the class's self delegate methods, anchored at their header lines", fun() ->
            Rows = beamtalk_repl_ops_browse:delegate_callers_of_native_module(
                bt_test_native_backing
            ),
            MyRows = [R || R <- Rows, maps:get(owner, R) =:= ClassName],
            ?assertEqual(
                lists:sort([
                    #{owner => ClassName, class_side => false, method => increment, line => 61},
                    #{
                        owner => ClassName,
                        class_side => false,
                        method => 'incrementBy:',
                        line => 71
                    }
                ]),
                lists:sort(MyRows)
            )
        end},
        {"a module that backs no loaded class yields the empty state", fun() ->
            ?assertEqual(
                [],
                beamtalk_repl_ops_browse:delegate_callers_of_native_module(
                    bt_no_such_native_backing_module_xyz
                )
            )
        end},
        {"the `none` backing-module sentinel is guarded (never matches)", fun() ->
            %% `none` is `meta_backing_module/1`'s "no backing module" answer, so a
            %% `module=none` query must NOT match classes that report no backing.
            ?assertEqual([], beamtalk_repl_ops_browse:delegate_callers_of_native_module(none))
        end}
    ].

%%====================================================================
%% browse-native-modules — enumeration + filter + source-path (BT-2648)
%%====================================================================

describe_ops_has_native_modules_key_test() ->
    Ops = beamtalk_repl_ops_browse:describe_ops(),
    ?assert(maps:is_key(<<"browse-native-modules">>, Ops)),
    #{<<"browse-native-modules">> := Info} = Ops,
    %% No params (enumerates every loaded package).
    ?assertEqual([], maps:get(<<"params">>, Info)),
    %% browse-native-source advertises the BT-2648 `module` alternative key.
    #{<<"browse-native-source">> := NS} = Ops,
    ?assert(lists:member(<<"module">>, maps:get(<<"params">>, NS))).

%% The enumeration: under the EUnit harness the stdlib package is loaded, so
%% `browse-native-modules` returns the stdlib's hand-written native modules.
native_modules_enumerates_stdlib_test() ->
    ensure_stdlib(),
    Rows = native_modules(),
    ?assert(is_list(Rows)),
    ?assert(length(Rows) > 0),
    Modules = [maps:get(<<"module">>, R) || R <- Rows],
    %% A known stdlib backing module is present.
    ?assert(lists:member(<<"beamtalk_array">>, Modules)).

%% Filter rule: the auto-generated `bt@{pkg}@{class}` class facade modules are
%% NEVER surfaced here (they are already in browse-classes) — only hand-written
%% native `.erl` modules.
native_modules_excludes_bt_facades_test() ->
    ensure_stdlib(),
    Rows = native_modules(),
    BtFacades = [
        M
     || R <- Rows,
        M <- [maps:get(<<"module">>, R)],
        binary:part(M, 0, min(3, byte_size(M))) =:= <<"bt@">>
    ],
    ?assertEqual([], BtFacades).

%% Every row carries the documented fields, origin/package classification, and a
%% source path resolved from the module's compile info (openable when readable).
native_modules_row_shape_test() ->
    ensure_stdlib(),
    Rows = native_modules(),
    Row = find_module_row(Rows, <<"beamtalk_array">>),
    ?assertEqual(<<"stdlib">>, maps:get(<<"source_origin">>, Row)),
    ?assertEqual(<<"stdlib">>, maps:get(<<"package">>, Row)),
    %% Source path resolved from compile info → openable.
    ?assert(is_binary(maps:get(<<"source_file">>, Row))),
    ?assertEqual(true, maps:get(<<"openable">>, Row)).

%% Rows are sorted by module name (stable tree order).
native_modules_sorted_test() ->
    ensure_stdlib(),
    Rows = native_modules(),
    Modules = [maps:get(<<"module">>, R) || R <- Rows],
    ?assertEqual(lists:sort(Modules), Modules).

%% browse-native-source keyed by `module` (BT-2648): a standalone native module
%% (no backing class) returns its source read-only with `class = null`.
native_source_by_module_test() ->
    ensure_stdlib(),
    code:ensure_loaded(beamtalk_array),
    Value = decode_value(
        beamtalk_repl_ops_browse:handle(
            <<"browse-native-source">>,
            #{<<"module">> => <<"beamtalk_array">>},
            make_msg(),
            self()
        )
    ),
    ?assertEqual(null, maps:get(<<"class">>, Value)),
    ?assertEqual(<<"beamtalk_array">>, maps:get(<<"backing_module">>, Value)),
    ?assertEqual(<<"stdlib">>, maps:get(<<"source_origin">>, Value)),
    %% Read-only in every origin for a standalone module (no editing seam).
    ?assertEqual(false, maps:get(<<"editable">>, Value)),
    ?assert(is_binary(maps:get(<<"content">>, Value))).

%% A `module` that names no loaded module is a structured not-found error, not an
%% atom-table growth.
native_source_unknown_module_errors_test() ->
    Response = beamtalk_repl_ops_browse:handle(
        <<"browse-native-source">>,
        #{<<"module">> => <<"definitely_not_a_loaded_module_xyz">>},
        make_msg(),
        self()
    ),
    Decoded = assert_error_response(Response),
    ?assertNotEqual(
        nomatch,
        binary:match(maps:get(<<"error">>, Decoded), <<"not found">>)
    ).

%% Helper: enumerate via the term handler.
native_modules() ->
    {value, Rows} = beamtalk_repl_ops_browse:handle_term(
        <<"browse-native-modules">>, #{}, make_msg(), self()
    ),
    Rows.

find_module_row(Rows, Module) ->
    case lists:search(fun(R) -> maps:get(<<"module">>, R) =:= Module end, Rows) of
        {value, Row} -> Row;
        false -> error({module_row_not_found, Module})
    end.

%% Ensure the stdlib package app is loaded so enumeration has a package to walk.
ensure_stdlib() ->
    _ = application:load(beamtalk_stdlib),
    _ = application:ensure_all_started(beamtalk_stdlib),
    ok.

%%====================================================================
%% browse-type-aliases — enumeration + seeding-boundary exclusion (BT-2903)
%%====================================================================

describe_ops_has_type_aliases_key_test() ->
    Ops = beamtalk_repl_ops_browse:describe_ops(),
    ?assert(maps:is_key(<<"browse-type-aliases">>, Ops)),
    #{<<"browse-type-aliases">> := Info} = Ops,
    ?assertEqual([], maps:get(<<"params">>, Info)).

%% Sanity: with no `type_aliases` env set anywhere, the op still returns a
%% (possibly empty) list rather than crashing — most packages, including
%% stdlib today, declare no aliases.
type_aliases_returns_list_test() ->
    ensure_stdlib(),
    ?assert(is_list(type_aliases())).

%% A package's declared public alias round-trips through every AliasRow field
%% (ADR 0108: name, expansion, doc, source_file, internal).
type_aliases_row_shape_test() ->
    with_stdlib_aliases(
        [
            #{
                name => 'RestartStrategy',
                expansion => "#temporary | #transient | #permanent",
                doc => "Restart strategy for a supervised child.",
                source_file => "src/restart_strategy.bt",
                internal => false
            }
        ],
        fun() ->
            Row = find_alias_row(type_aliases(), <<"RestartStrategy">>),
            ?assertEqual(
                <<"#temporary | #transient | #permanent">>,
                maps:get(<<"expansion">>, Row)
            ),
            ?assertEqual(
                <<"Restart strategy for a supervised child.">>,
                maps:get(<<"doc">>, Row)
            ),
            ?assertEqual(
                <<"src/restart_strategy.bt">>, maps:get(<<"source_file">>, Row)
            ),
            ?assertEqual(false, maps:get(<<"internal">>, Row)),
            ?assertEqual(<<"stdlib">>, maps:get(<<"package">>, Row)),
            ?assertEqual(<<"stdlib">>, maps:get(<<"source_origin">>, Row))
        end
    ).

%% `doc => undefined` (no `///` comment) round-trips as JSON `null`.
type_aliases_missing_doc_is_null_test() ->
    with_stdlib_aliases(
        [
            #{
                name => 'TimeoutMs',
                expansion => "Integer",
                doc => undefined,
                source_file => "src/timeout.bt",
                internal => false
            }
        ],
        fun() ->
            Row = find_alias_row(type_aliases(), <<"TimeoutMs">>),
            ?assertEqual(null, maps:get(<<"doc">>, Row))
        end
    ).

%% Seeding-boundary exclusion (ADR 0108 Implementation, BT-2903): an internal
%% alias belonging to a package other than the current project is dropped
%% entirely — never returned as a row for any browsing session to filter.
%% Stands in for a dependency package the same way `beamtalk_stdlib` stands in
%% for "some other loaded package" throughout this suite (see
%% `with_stdlib_aliases`'s doc) — the current project is configured as
%% `myapp`, so stdlib (origin `<<"stdlib">>`, never `<<"project">>`) plays the
%% "not the current project" role.
type_aliases_internal_from_non_project_package_excluded_test() ->
    with_project_package(<<"myapp">>, fun() ->
        with_stdlib_aliases(
            [
                #{
                    name => 'InternalStdlibHelper',
                    expansion => "Integer",
                    doc => undefined,
                    source_file => "src/helper.bt",
                    internal => true
                },
                #{
                    name => 'PublicStdlibAlias',
                    expansion => "String",
                    doc => undefined,
                    source_file => "src/public.bt",
                    internal => false
                }
            ],
            fun() ->
                Names = [maps:get(<<"name">>, R) || R <- type_aliases()],
                ?assertNot(lists:member(<<"InternalStdlibHelper">>, Names)),
                ?assert(lists:member(<<"PublicStdlibAlias">>, Names))
            end
        )
    end).

%% Rows are sorted by name (stable tree order).
type_aliases_sorted_test() ->
    with_stdlib_aliases(
        [
            #{
                name => 'Zebra',
                expansion => "Integer",
                doc => undefined,
                source_file => "src/z.bt",
                internal => false
            },
            #{
                name => 'Alpha',
                expansion => "Integer",
                doc => undefined,
                source_file => "src/a.bt",
                internal => false
            }
        ],
        fun() ->
            OwnNames = [
                maps:get(<<"name">>, R)
             || R <- type_aliases(), maps:get(<<"package">>, R) =:= <<"stdlib">>
            ],
            ?assertEqual([<<"Alpha">>, <<"Zebra">>], OwnNames)
        end
    ).

%% Resilience: a malformed `type_aliases` env entry (e.g. a hand-edited or
%% toolchain-mismatched `.app` file, not a map) must not crash the whole
%% browse — `type_aliases_of_package/1` catches and skips, mirroring
%% `class_row/2`'s per-class isolation elsewhere in this module.
type_aliases_malformed_entry_does_not_crash_test() ->
    with_stdlib_aliases(
        [not_a_map],
        fun() ->
            Rows = type_aliases(),
            ?assert(is_list(Rows)),
            ?assertEqual([], [R || R <- Rows, maps:get(<<"package">>, R) =:= <<"stdlib">>])
        end
    ).

%% A package with zero classes and one or more `type` declarations is
%% discoverable via `beamtalk_package:all/0` and its aliases appear here
%% end to end (BT-2915) — unlike `with_stdlib_aliases`, this constructs a
%% real second OTP application (stdlib always carries `classes`, so it can't
%% exercise the types-only discovery path) with only a `type_aliases` env
%% key, no `classes` key at all.
type_aliases_surfaces_types_only_package_test() ->
    with_types_only_fixture_app(
        bt_fake_types_only_browse_pkg,
        [
            #{
                name => 'RestartStrategy',
                expansion => "#temporary | #transient | #permanent",
                doc => undefined,
                source_file => "src/restart_strategy.bt",
                internal => false
            }
        ],
        fun() ->
            Row = find_alias_row(type_aliases(), <<"RestartStrategy">>),
            ?assertEqual(
                <<"#temporary | #transient | #permanent">>,
                maps:get(<<"expansion">>, Row)
            ),
            ?assertEqual(
                <<"bt_fake_types_only_browse_pkg">>, maps:get(<<"package">>, Row)
            ),
            ?assertEqual(<<"dependency">>, maps:get(<<"source_origin">>, Row))
        end
    ).

%% alias_visible/2 — pure seeding-boundary decision (BT-2903).
alias_visible_internal_project_is_visible_test() ->
    ?assert(beamtalk_repl_ops_browse:alias_visible(#{internal => true}, <<"project">>)).

alias_visible_internal_non_project_is_hidden_test() ->
    ?assertNot(
        beamtalk_repl_ops_browse:alias_visible(#{internal => true}, <<"dependency">>)
    ),
    ?assertNot(beamtalk_repl_ops_browse:alias_visible(#{internal => true}, <<"stdlib">>)).

alias_visible_public_always_visible_test() ->
    ?assert(beamtalk_repl_ops_browse:alias_visible(#{internal => false}, <<"project">>)),
    ?assert(beamtalk_repl_ops_browse:alias_visible(#{internal => false}, <<"dependency">>)),
    ?assert(beamtalk_repl_ops_browse:alias_visible(#{internal => false}, <<"stdlib">>)).

%% alias_row/3 — field normalisation from the `.app`-file's atom/string shapes
%% (`file:consult`-parsed terms, never binaries — see `alias_field_binary/1`'s
%% doc).
alias_row_normalises_app_file_shapes_test() ->
    Entry = #{
        name => 'JsonKey',
        expansion => "String",
        doc => "A doc comment.",
        source_file => "src/json.bt",
        internal => false
    },
    Row = beamtalk_repl_ops_browse:alias_row(Entry, <<"my_app">>, <<"project">>),
    ?assertEqual(<<"JsonKey">>, maps:get(<<"name">>, Row)),
    ?assertEqual(<<"String">>, maps:get(<<"expansion">>, Row)),
    ?assertEqual(<<"A doc comment.">>, maps:get(<<"doc">>, Row)),
    ?assertEqual(<<"src/json.bt">>, maps:get(<<"source_file">>, Row)),
    ?assertEqual(false, maps:get(<<"internal">>, Row)),
    ?assertEqual(<<"my_app">>, maps:get(<<"package">>, Row)),
    ?assertEqual(<<"project">>, maps:get(<<"source_origin">>, Row)).

alias_row_undefined_doc_is_null_test() ->
    Entry = #{
        name => 'NoDoc',
        expansion => "Integer",
        doc => undefined,
        source_file => "src/nodoc.bt",
        internal => true
    },
    Row = beamtalk_repl_ops_browse:alias_row(Entry, <<"my_app">>, <<"project">>),
    ?assertEqual(null, maps:get(<<"doc">>, Row)).

%% Helper: enumerate via the term handler.
type_aliases() ->
    {value, Rows} = beamtalk_repl_ops_browse:handle_term(
        <<"browse-type-aliases">>, #{}, make_msg(), self()
    ),
    Rows.

find_alias_row(Rows, Name) ->
    case lists:search(fun(R) -> maps:get(<<"name">>, R) =:= Name end, Rows) of
        {value, Row} -> Row;
        false -> error({alias_row_not_found, Name})
    end.

%% Temporarily set `beamtalk_stdlib`'s `type_aliases` env to `Aliases`, run
%% `Fun`, then restore the original env (`undefined` → unset).
%%
%% Stands in for a real second fixture package for most tests in this
%% describe block: stdlib is the one app always loaded under the EUnit
%% harness (see `ensure_stdlib/0`), and `application:set_env/3` on it is the
%% minimal, precedent-consistent way to fixture a package's alias env for
%% enumeration tests without building a real `.app` file end-to-end
%% (`browse-native-modules`'s project/dependency classification is likewise
%% tested at the pure-function level, see `source_origin_of/2` tests below).
%% The types-only discovery path (BT-2915) can't be exercised this way,
%% since stdlib always carries a non-empty `classes` env — see
%% `with_types_only_fixture_app/3` below for the real second OTP application
%% that path needs.
with_stdlib_aliases(Aliases, Fun) ->
    ensure_stdlib(),
    Previous = application:get_env(beamtalk_stdlib, type_aliases),
    ok = application:set_env(beamtalk_stdlib, type_aliases, Aliases),
    try
        Fun()
    after
        case Previous of
            {ok, V} -> application:set_env(beamtalk_stdlib, type_aliases, V);
            undefined -> application:unset_env(beamtalk_stdlib, type_aliases)
        end
    end.

%% Load a throwaway OTP application declaring only `{type_aliases, Aliases}`
%% (no `classes` key at all), run `Fun`, then unload it (mirrors
%% `beamtalk_package_tests:load_fake_app/2`, the precedent for fixturing a
%% real second OTP application rather than piggybacking on stdlib's env).
with_types_only_fixture_app(App, Aliases, Fun) ->
    ensure_stdlib(),
    _ = application:unload(App),
    ok = application:load(
        {application, App, [
            {description, "fake types-only package"},
            {vsn, "1.0.0"},
            {env, [{type_aliases, Aliases}]}
        ]}
    ),
    try
        Fun()
    after
        _ = application:unload(App)
    end.

%%====================================================================
%% Validation error paths (no live class needed)
%%====================================================================

protocols_missing_class_is_error_test() ->
    Response = beamtalk_repl_ops_browse:handle(<<"browse-protocols">>, #{}, make_msg(), self()),
    Decoded = assert_error_response(Response),
    ?assertNotEqual(nomatch, binary:match(maps:get(<<"error">>, Decoded), <<"class">>)).

protocols_unknown_class_is_error_test() ->
    %% A class name that is not registered → not-found, no atom-table growth.
    Response = beamtalk_repl_ops_browse:handle(
        <<"browse-protocols">>,
        #{<<"class">> => <<"__NoSuchBrowseClass__">>, <<"side">> => <<"instance">>},
        make_msg(),
        self()
    ),
    Decoded = assert_error_response(Response),
    ?assertNotEqual(nomatch, binary:match(maps:get(<<"error">>, Decoded), <<"not found">>)).

method_source_missing_selector_is_error_test() ->
    %% Class resolves (registered in the fixture setup is not active here, so use
    %% the unknown-class path is avoided by asserting the message mentions a
    %% required field). Without a `class` the class check fires first.
    Response = beamtalk_repl_ops_browse:handle(
        <<"browse-method-source">>, #{}, make_msg(), self()
    ),
    assert_error_response(Response).

definition_empty_class_is_error_test() ->
    Response = beamtalk_repl_ops_browse:handle(
        <<"browse-class-definition">>, #{<<"class">> => <<>>}, make_msg(), self()
    ),
    assert_error_response(Response).

%%====================================================================
%% Success paths — live class object + seeded xref
%%====================================================================

browse_setup() ->
    %% Ensure the xref server is up. We never clear it globally: a unique class
    %% name per invocation (below) isolates this fixture from any other class /
    %% xref state, and cleanup purges only this class — so the fixture is
    %% order-independent and safe to run alongside a populated index (CodeRabbit
    %% BT-2506: no global xref-table clears, unique class per invocation).
    XrefPid =
        case whereis(beamtalk_xref) of
            undefined ->
                %% start-or-get: another fixture may win the race between the
                %% whereis/1 above and start_link/0 here (CodeRabbit BT-2506).
                case beamtalk_xref:start_link() of
                    {ok, P} ->
                        P;
                    {error, {already_started, P}} ->
                        P
                end;
            P ->
                P
        end,

    %% Unique class name + module per invocation. `erlang:unique_integer/1`
    %% (positive, monotonic) guarantees no collision with a previous run, a
    %% concurrent fixture, or a real class in the image — the names the System
    %% Browser asserts against are bound into the context and threaded through
    %% every test body, so nothing depends on a fixed `'BrowseCounter'` atom.
    Uniq = erlang:integer_to_list(erlang:unique_integer([positive, monotonic])),
    ClassName = list_to_atom("BrowseCounter_" ++ Uniq),
    ClassNameBin = atom_to_binary(ClassName, utf8),
    ModuleName = list_to_atom("bt@test@browse_counter_" ++ Uniq),

    %% A concrete class with two instance methods, a class-side method, fields
    %% with defaults, and a doc — the System Browser's canonical subject.
    {BrowsePid, BrowseOwned} = start_browse_class(ClassName, #{
        name => ClassName,
        module => ModuleName,
        superclass => none,
        doc => <<"The canonical live object.\nSecond line ignored.">>,
        fields => [value, step],
        field_defaults => #{value => 0, step => 1},
        instance_methods => #{
            %% A body that would crash if *invoked*; browse must never run it
            %% (ADR 0091 no-user-code guarantee).
            'increment' => #{block => fun(_, _) -> erlang:error(must_not_run) end, arity => 0},
            'value' => #{block => fun(_, _) -> erlang:error(must_not_run) end, arity => 0},
            %% BT-2735: a synthetic accessor that IS resolvable (registered with a
            %% signature + doc), so browse-protocols can enrich its hover row via
            %% `method_doc_signature_resolved/3`. Its xref row below is `synthetic`.
            'total' => #{block => fun(_, _) -> erlang:error(must_not_run) end, arity => 0}
        },
        method_source => #{
            'increment' => <<"increment =>\n  self.value := self.value + self.step">>,
            'value' => <<"value =>\n  ^ self.value">>
        },
        %% BT-2558: a signature for both methods and a `///` doc-comment on
        %% `increment` only — so browse-method-source can carry the rendered
        %% signature + doc, and `value` exercises the no-doc (null) path.
        method_signatures => #{
            'increment' => <<"increment -> Counter">>,
            'value' => <<"value -> Integer">>,
            'total' => <<"total -> Integer">>
        },
        method_docs => #{
            'increment' =>
                <<"Increment the counter by its step.\n\n## Examples\n```beamtalk\nc increment\n```">>,
            'total' => <<"The running total of the counter.\nDetail line ignored by the hover.">>
        },
        class_methods => #{
            'startingAt:' => #{block => fun(_, _, _) -> erlang:error(must_not_run) end, arity => 1}
        }
    }),

    %% Seed xref for the instance side so browse-protocols / -method-source get
    %% line + source_status + provenance from the index.
    ok = beamtalk_xref:register_class(ClassName, browse_xref()),

    #{
        xref => XrefPid,
        class_name => ClassNameBin,
        class => {ClassName, BrowsePid, BrowseOwned}
    }.

browse_cleanup(#{class := {Name, Pid, Owned}}) ->
    %% Purge only this fixture's class from the xref index — never a global
    %% `ets:delete_all_objects` that would wipe a concurrently-populated index
    %% (CodeRabbit BT-2506). `purge_class/1` is idempotent.
    catch beamtalk_xref:purge_class(Name),
    case Owned of
        true ->
            catch gen_server:stop(Pid);
        false ->
            ok
    end,
    ok.

start_browse_class(Name, Spec) ->
    case beamtalk_object_class:start(Name, Spec) of
        {ok, Pid} -> {Pid, true};
        {error, {already_started, Pid}} -> {Pid, false}
    end.

%% Instance-side xref rows. The first two are the source-backed selectors the
%% original browse tests assert on; the rest exercise the BT-2506 protocol
%% categorization decision (name heuristic, extension source fact, synthetic
%% accessor). Listing selectors as atom literals here interns them so the op's
%% `binary_to_existing_atom` resolution succeeds.
browse_xref() ->
    [
        method_row('increment', 68, indexed, class_body),
        method_row('value', 80, indexed, class_body),
        %% Name-heuristic buckets (no declared category, not extension):
        method_row('isEmpty', 90, indexed, class_body),
        method_row('asString', 92, indexed, class_body),
        method_row('printString', 94, indexed, class_body),
        method_row('setTo:', 96, indexed, class_body),
        %% Source fact: extension provenance → "extensions", outranks the name.
        method_row('describe', 98, indexed, extension),
        %% Synthetic compiler-generated accessor → "accessing" by construction.
        method_row('step', 100, synthetic, class_body),
        %% BT-2735: a synthetic accessor registered as a real method (signature +
        %% doc) so browse-protocols resolves its hover signature/doc.
        method_row('total', 110, synthetic, class_body),
        %% camelCase word-boundary guard: `address` must NOT match the `add`
        %% prefix (no uppercase/`:` boundary), so it stays "as yet unclassified".
        method_row('address', 102, indexed, class_body),
        %% Precedence overlap: synthetic AND extension. The synthetic source
        %% status (a compiler-generated accessor) is decided before the extension
        %% provenance, so this must bucket "accessing", not "extensions".
        method_row('syntheticExt', 104, synthetic, extension),
        %% print* prefix path (distinct from the exact-name `printString` match):
        %% `printDetails` must reach the prefix heuristic and land in "printing".
        method_row('printDetails', 106, indexed, class_body),
        %% BT-2622: a synthetic *instance-side* slot whose name collides with an
        %% actor constructor (`state: new :: Integer = 0` → synthetic accessor
        %% `new`). It must bucket "accessing" by `class_side`, NOT "instance
        %% creation" — the old selector-name convention would have misclassified
        %% it. `new:`/`spawn`/`spawn:` are exercised the same way, so all four
        %% colliding constructor names are covered on the instance side.
        method_row('new', 108, synthetic, class_body),
        method_row('new:', 108, synthetic, class_body),
        method_row('spawn', 108, synthetic, class_body),
        method_row('spawn:', 108, synthetic, class_body),
        %% BT-2614: compiler-injected synthetic class-side constructors. An actor's
        %% codegen emits `new`/`new:`/`spawn`/`spawn:` as sourceless exported
        %% functions; these rows are how the System Browser surfaces them (badged
        %% synthetic, bucketed "instance creation") so its method set matches
        %% runtime `aClass class allMethods` reflection. Listed as class-side
        %% (`class_side => true`) so they appear under `side = class`, not instance.
        class_method_row('new', 1, synthetic, class_body),
        class_method_row('new:', 1, synthetic, class_body),
        class_method_row('spawn', 1, synthetic, class_body),
        class_method_row('spawn:', 1, synthetic, class_body)
    ].

method_row(Selector, Line, SourceStatus, Provenance) ->
    method_xref_entry(false, Selector, Line, SourceStatus, Provenance).

%% BT-2614: class-side variant of `method_row/4` for synthetic constructors.
class_method_row(Selector, Line, SourceStatus, Provenance) ->
    method_xref_entry(true, Selector, Line, SourceStatus, Provenance).

method_xref_entry(ClassSide, Selector, Line, SourceStatus, Provenance) ->
    #{
        class_side => ClassSide,
        selector => Selector,
        line => Line,
        sends => [],
        references => [],
        source_status => SourceStatus,
        provenance => Provenance
    }.

browse_test_() ->
    {setup, fun browse_setup/0, fun browse_cleanup/1, fun browse_tests/1}.

browse_tests(#{class_name := Class}) ->
    [
        {"browse-classes includes the registered class with required fields", fun() ->
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(<<"browse-classes">>, #{}, make_msg(), self())
            ),
            ?assert(is_list(Value)),
            Row = find_class_row(Value, Class),
            ?assertNotEqual(undefined, Row),
            lists:foreach(
                fun(Key) -> ?assert(maps:is_key(Key, Row)) end,
                [
                    <<"name">>,
                    <<"superclass">>,
                    <<"category">>,
                    <<"comment">>,
                    <<"sealed">>,
                    <<"abstract">>,
                    <<"internal">>,
                    <<"source_file">>,
                    <<"origin">>,
                    <<"source_origin">>,
                    <<"is_test">>,
                    <<"is_protocol">>
                ]
            )
        end},
        {"browse-classes is_protocol is false for an ordinary class", fun() ->
            %% BT-2615: only protocol class objects (ADR 0068) carry is_protocol
            %% true — the browser groups those under a "Protocols" category.
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(<<"browse-classes">>, #{}, make_msg(), self())
            ),
            Row = find_class_row(Value, Class),
            ?assertEqual(false, maps:get(<<"is_protocol">>, Row))
        end},
        {"browse-classes is_test is false for a non-TestCase class", fun() ->
            %% BT-2557: is_test flags loaded TestCase subclasses so the browser
            %% can group them under a "Tests" category. The fixture class does not
            %% descend from TestCase, so the flag is a plain boolean false.
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(<<"browse-classes">>, #{}, make_msg(), self())
            ),
            Row = find_class_row(Value, Class),
            ?assertEqual(false, maps:get(<<"is_test">>, Row))
        end},
        {"browse-classes comment is the first line of the class doc", fun() ->
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(<<"browse-classes">>, #{}, make_msg(), self())
            ),
            Row = find_class_row(Value, Class),
            ?assertEqual(<<"The canonical live object.">>, maps:get(<<"comment">>, Row))
        end},
        {"browse-classes origin is runtime for a file-less class", fun() ->
            %% The fixture module has no backing .bt source attribute, so
            %% source_file is null → origin runtime (ADR 0096).
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(<<"browse-classes">>, #{}, make_msg(), self())
            ),
            Row = find_class_row(Value, Class),
            ?assertEqual(null, maps:get(<<"source_file">>, Row)),
            ?assertEqual(<<"runtime">>, maps:get(<<"origin">>, Row))
        end},
        {"browse-classes source_origin is a bare classification for all rows", fun() ->
            %% BT-2552/BT-2643: source_origin is the bare classification
            %% (project|dependency|stdlib) — the package name is NOT packed in.
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(<<"browse-classes">>, #{}, make_msg(), self())
            ),
            Row = find_class_row(Value, Class),
            SourceOrigin = maps:get(<<"source_origin">>, Row),
            ?assert(is_binary(SourceOrigin)),
            ?assert(lists:member(SourceOrigin, [<<"stdlib">>, <<"project">>, <<"dependency">>])),
            ?assertEqual(nomatch, binary:match(SourceOrigin, <<"dependency:">>))
        end},
        {"browse-classes carries a separate package field on every row", fun() ->
            %% BT-2643: package is orthogonal to source_origin and present
            %% (binary or null) on every class row.
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(<<"browse-classes">>, #{}, make_msg(), self())
            ),
            Row = find_class_row(Value, Class),
            ?assert(maps:is_key(<<"package">>, Row)),
            Package = maps:get(<<"package">>, Row),
            ?assert(is_binary(Package) orelse Package =:= null)
        end},
        {"browse-class-definition reports native=false for a plain class", fun() ->
            %% BT-2578: a fixture class with no native facade meta is not
            %% native-backed; backing_module is null.
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-class-definition">>,
                    #{<<"class">> => Class},
                    make_msg(),
                    self()
                )
            ),
            ?assertEqual(false, maps:get(<<"native">>, Value)),
            ?assertEqual(null, maps:get(<<"backing_module">>, Value))
        end},
        {"browse-class-definition carries reflected sealed/abstract modifiers", fun() ->
            %% BT-2605: op 4 surfaces the same sealed/abstract reflection op 1
            %% (browse-classes) carries, so the IDE editor header can badge them
            %% without parsing the synthesized definition skeleton. A plain fixture
            %% class is neither sealed nor abstract.
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-class-definition">>,
                    #{<<"class">> => Class},
                    make_msg(),
                    self()
                )
            ),
            ?assertEqual(false, maps:get(<<"sealed">>, Value)),
            ?assertEqual(false, maps:get(<<"abstract">>, Value)),
            %% BT-2629: a plain fixture class is not typed.
            ?assertEqual(false, maps:get(<<"typed">>, Value))
        end},
        {"browse-class-definition reports typed=true for a typed-declared class", fun() ->
            %% BT-2629: op 4 surfaces the already-emitted is_typed meta flag as a
            %% runtime-reflected boolean (mirroring sealed/abstract), so the IDE
            %% editor header can render the Typed badge without parsing the
            %% synthesized definition skeleton.
            Uniq = erlang:integer_to_list(erlang:unique_integer([positive, monotonic])),
            TypedName = list_to_atom("BrowseTyped_" ++ Uniq),
            TypedModule = list_to_atom("bt@test@browse_typed_" ++ Uniq),
            {TypedPid, Owned} = start_browse_class(TypedName, #{
                name => TypedName,
                module => TypedModule,
                superclass => none,
                is_typed => true
            }),
            try
                Value = decode_value(
                    beamtalk_repl_ops_browse:handle(
                        <<"browse-class-definition">>,
                        #{<<"class">> => atom_to_binary(TypedName, utf8)},
                        make_msg(),
                        self()
                    )
                ),
                ?assertEqual(true, maps:get(<<"typed">>, Value))
            after
                catch beamtalk_xref:purge_class(TypedName),
                case Owned of
                    true -> catch gen_server:stop(TypedPid);
                    false -> ok
                end
            end
        end},
        {"browse-class-definition reports is_protocol=false for an ordinary class", fun() ->
            %% BT-2639: op 4 surfaces is_protocol (runtime reflection, not a header
            %% string-sniff) so the def tab can gate the protocol action row. A
            %% plain fixture class is not a protocol.
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-class-definition">>,
                    #{<<"class">> => Class},
                    make_msg(),
                    self()
                )
            ),
            ?assertEqual(false, maps:get(<<"is_protocol">>, Value))
        end},
        {"browse-native-source errors for a non-native class", fun() ->
            %% BT-2578: the op only applies to native: classes (ADR 0056).
            Response = beamtalk_repl_ops_browse:handle(
                <<"browse-native-source">>,
                #{<<"class">> => Class},
                make_msg(),
                self()
            ),
            Decoded = assert_error_response(Response),
            ?assertNotEqual(
                nomatch,
                binary:match(maps:get(<<"error">>, Decoded), <<"not native-backed">>)
            )
        end},
        {"browse-protocols groups selectors with line and source_status", fun() ->
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-protocols">>,
                    #{<<"class">> => Class, <<"side">> => <<"instance">>},
                    make_msg(),
                    self()
                )
            ),
            ?assertEqual(Class, maps:get(<<"class">>, Value)),
            ?assertEqual(<<"instance">>, maps:get(<<"side">>, Value)),
            Protocols = maps:get(<<"protocols">>, Value),
            ?assert(is_list(Protocols)),
            Selectors = all_selector_rows(Protocols),
            Names = [maps:get(<<"selector">>, S) || S <- Selectors],
            ?assert(lists:member(<<"increment">>, Names)),
            ?assert(lists:member(<<"value">>, Names)),
            IncRow = find_selector_row(Selectors, <<"increment">>),
            ?assertEqual(68, maps:get(<<"line">>, IncRow)),
            ?assertEqual(<<"indexed">>, maps:get(<<"source_status">>, IncRow))
        end},
        {"browse-protocols enriches a synthetic row with signature + doc (BT-2735)", fun() ->
            %% A `synthetic` selector row carries the signature + doc resolved via
            %% the same hierarchy walk `:help` uses, so the method-list hover can
            %% show what the method is without a click.
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-protocols">>,
                    #{<<"class">> => Class, <<"side">> => <<"instance">>},
                    make_msg(),
                    self()
                )
            ),
            Selectors = all_selector_rows(maps:get(<<"protocols">>, Value)),
            TotalRow = find_selector_row(Selectors, <<"total">>),
            ?assertEqual(<<"synthetic">>, maps:get(<<"source_status">>, TotalRow)),
            ?assertEqual(<<"total -> Integer">>, maps:get(<<"signature">>, TotalRow)),
            Doc = maps:get(<<"doc">>, TotalRow),
            ?assert(is_binary(Doc)),
            ?assertNotEqual(nomatch, binary:match(Doc, <<"The running total">>)),
            %% Bounded cost (BT-2735): a hand-written `indexed` row is NOT resolved
            %% — no per-method CompiledMethod read on the browse hot path — so its
            %% signature/doc stay null and the hover falls back to the selector.
            IncRow = find_selector_row(Selectors, <<"increment">>),
            ?assertEqual(null, maps:get(<<"signature">>, IncRow)),
            ?assertEqual(null, maps:get(<<"doc">>, IncRow))
        end},
        {"browse-protocols categorizes selectors by real protocol buckets", fun() ->
            %% BT-2506: protocol_for_selector decides pragma → xref-extension
            %% source → name heuristic. With no declared category, the seed
            %% selectors land in meaningful buckets (not all "as yet
            %% unclassified"): name conventions for is*/as*/print*, the
            %% extension-provenance selector in "extensions", and a synthetic
            %% accessor in "accessing".
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-protocols">>,
                    #{<<"class">> => Class, <<"side">> => <<"instance">>},
                    make_msg(),
                    self()
                )
            ),
            Protocols = maps:get(<<"protocols">>, Value),
            ProtocolNames = [maps:get(<<"name">>, P) || P <- Protocols],
            %% More than one bucket now — the v1 single-bucket collapse is gone.
            ?assert(length(ProtocolNames) > 1),
            ?assertEqual(<<"testing">>, protocol_of(Protocols, <<"isEmpty">>)),
            ?assertEqual(<<"converting">>, protocol_of(Protocols, <<"asString">>)),
            ?assertEqual(<<"printing">>, protocol_of(Protocols, <<"printString">>)),
            ?assertEqual(<<"operations">>, protocol_of(Protocols, <<"setTo:">>)),
            %% Source fact (extension provenance) outranks the name heuristic.
            ?assertEqual(<<"extensions">>, protocol_of(Protocols, <<"describe">>)),
            %% Synthetic source_status → "accessing" by construction.
            ?assertEqual(<<"accessing">>, protocol_of(Protocols, <<"step">>)),
            %% Word-boundary discipline: `address` is not an `add` mutator.
            ?assertEqual(<<"as yet unclassified">>, protocol_of(Protocols, <<"address">>)),
            %% Precedence: synthetic status decided before extension provenance.
            ?assertEqual(<<"accessing">>, protocol_of(Protocols, <<"syntheticExt">>)),
            %% print* prefix path (not the exact-name `printString` match):
            %% `printDetails` must reach the prefix heuristic and land in "printing".
            ?assertEqual(<<"printing">>, protocol_of(Protocols, <<"printDetails">>)),
            %% BT-2622: instance-side synthetic slots whose names collide with the
            %% actor constructors must classify by `class_side` (accessing), NOT by
            %% the selector-name convention (which would say "instance creation").
            ?assertEqual(<<"accessing">>, protocol_of(Protocols, <<"new">>)),
            ?assertEqual(<<"accessing">>, protocol_of(Protocols, <<"new:">>)),
            ?assertEqual(<<"accessing">>, protocol_of(Protocols, <<"spawn">>)),
            ?assertEqual(<<"accessing">>, protocol_of(Protocols, <<"spawn:">>))
        end},
        {"browse-protocols surfaces injected synthetic class-side constructors", fun() ->
            %% BT-2614: the compiler-injected `new`/`new:`/`spawn`/`spawn:` an
            %% actor's codegen emits as sourceless class-side functions appear in
            %% the class-side protocol list, badged `synthetic` and bucketed under
            %% "instance creation" — so the browser's method set matches runtime
            %% `aClass class allMethods` reflection.
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-protocols">>,
                    #{<<"class">> => Class, <<"side">> => <<"class">>},
                    make_msg(),
                    self()
                )
            ),
            ?assertEqual(<<"class">>, maps:get(<<"side">>, Value)),
            Protocols = maps:get(<<"protocols">>, Value),
            Selectors = all_selector_rows(Protocols),
            Names = [maps:get(<<"selector">>, S) || S <- Selectors],
            lists:foreach(
                fun(Sel) ->
                    ?assert(lists:member(Sel, Names)),
                    Row = find_selector_row(Selectors, Sel),
                    %% Badged synthetic (read-only marker) and bucketed
                    %% "instance creation".
                    ?assertEqual(<<"synthetic">>, maps:get(<<"source_status">>, Row)),
                    ?assertEqual(
                        <<"instance creation">>, protocol_of(Protocols, Sel)
                    )
                end,
                [<<"new">>, <<"new:">>, <<"spawn">>, <<"spawn:">>]
            )
        end},
        {"browse-method-source returns null source for a synthetic class method", fun() ->
            %% BT-2614: a synthetic class-side constructor has no editable user
            %% source — `browse-method-source` returns `null` source (and null
            %% doc/signature) so the browser badges it read-only with no
            %% `[source]` jump, never surfacing an inherited body in its place.
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-method-source">>,
                    #{
                        <<"class">> => Class,
                        <<"side">> => <<"class">>,
                        <<"selector">> => <<"spawn">>
                    },
                    make_msg(),
                    self()
                )
            ),
            ?assertEqual(<<"spawn">>, maps:get(<<"selector">>, Value)),
            ?assertEqual(<<"synthetic">>, maps:get(<<"source_status">>, Value)),
            ?assertEqual(null, maps:get(<<"source">>, Value)),
            ?assertEqual(null, maps:get(<<"doc">>, Value)),
            ?assertEqual(null, maps:get(<<"signature">>, Value))
        end},
        {"browse-protocols rejects a bad side", fun() ->
            Response = beamtalk_repl_ops_browse:handle(
                <<"browse-protocols">>,
                #{<<"class">> => Class, <<"side">> => <<"klass">>},
                make_msg(),
                self()
            ),
            Decoded = assert_error_response(Response),
            ?assertNotEqual(nomatch, binary:match(maps:get(<<"error">>, Decoded), <<"side">>))
        end},
        {"browse-protocols missing side is an error", fun() ->
            Response = beamtalk_repl_ops_browse:handle(
                <<"browse-protocols">>,
                #{<<"class">> => Class},
                make_msg(),
                self()
            ),
            assert_error_response(Response)
        end},
        {"browse-method-source returns image source and source_status", fun() ->
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-method-source">>,
                    #{
                        <<"class">> => Class,
                        <<"side">> => <<"instance">>,
                        <<"selector">> => <<"increment">>
                    },
                    make_msg(),
                    self()
                )
            ),
            ?assertEqual(<<"increment">>, maps:get(<<"selector">>, Value)),
            ?assertEqual(<<"indexed">>, maps:get(<<"source_status">>, Value)),
            ?assertEqual(68, maps:get(<<"line">>, Value)),
            Src = maps:get(<<"source">>, Value),
            ?assert(is_binary(Src)),
            ?assertNotEqual(nomatch, binary:match(Src, <<"self.value">>))
        end},
        {"browse-method-source carries the doc-comment and signature (BT-2558)", fun() ->
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-method-source">>,
                    #{
                        <<"class">> => Class,
                        <<"side">> => <<"instance">>,
                        <<"selector">> => <<"increment">>
                    },
                    make_msg(),
                    self()
                )
            ),
            ?assertEqual(<<"increment -> Counter">>, maps:get(<<"signature">>, Value)),
            Doc = maps:get(<<"doc">>, Value),
            ?assert(is_binary(Doc)),
            ?assertNotEqual(nomatch, binary:match(Doc, <<"Increment the counter">>)),
            ?assertNotEqual(nomatch, binary:match(Doc, <<"## Examples">>))
        end},
        {"browse-method-source doc is null when the method has no doc-comment (BT-2558)", fun() ->
            %% `value` carries a signature but no `///` doc — doc is null, the
            %% signature still rides along.
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-method-source">>,
                    #{
                        <<"class">> => Class,
                        <<"side">> => <<"instance">>,
                        <<"selector">> => <<"value">>
                    },
                    make_msg(),
                    self()
                )
            ),
            ?assertEqual(null, maps:get(<<"doc">>, Value)),
            ?assertEqual(<<"value -> Integer">>, maps:get(<<"signature">>, Value))
        end},
        {"browse-method-source disk_differs is null with no static source", fun() ->
            %% No workspace_meta class source is stored in this isolated node, so
            %% there is nothing to diff against → null (ADR 0096).
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-method-source">>,
                    #{
                        <<"class">> => Class,
                        <<"side">> => <<"instance">>,
                        <<"selector">> => <<"increment">>
                    },
                    make_msg(),
                    self()
                )
            ),
            ?assertEqual(null, maps:get(<<"disk_differs">>, Value))
        end},
        {"browse-method-source for unknown selector yields null source", fun() ->
            %% A selector the class does not define → no xref row, no stored
            %% source: source null, status unindexed_runtime_fun. Whether the
            %% name is an existing atom or not, the op resolves to the same
            %% not-found result (an unknown name maps to a sentinel atom that
            %% also has no xref row / stored source).
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-method-source">>,
                    #{
                        <<"class">> => Class,
                        <<"side">> => <<"instance">>,
                        <<"selector">> => <<"unknownBrowseSelector">>
                    },
                    make_msg(),
                    self()
                )
            ),
            ?assertEqual(null, maps:get(<<"source">>, Value)),
            %% No CompiledMethod → no doc/signature either (BT-2558).
            ?assertEqual(null, maps:get(<<"doc">>, Value)),
            ?assertEqual(null, maps:get(<<"signature">>, Value)),
            ?assertEqual(<<"unindexed_runtime_fun">>, maps:get(<<"source_status">>, Value))
        end},
        {"browse-class-definition returns header, state slots and comment", fun() ->
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-class-definition">>,
                    #{<<"class">> => Class},
                    make_msg(),
                    self()
                )
            ),
            ?assertEqual(Class, maps:get(<<"class">>, Value)),
            State = maps:get(<<"state">>, Value),
            StateNames = [maps:get(<<"name">>, S) || S <- State],
            ?assert(lists:member(<<"value">>, StateNames)),
            ?assert(lists:member(<<"step">>, StateNames)),
            ?assertEqual(
                <<"The canonical live object.\nSecond line ignored.">>,
                maps:get(<<"comment">>, Value)
            ),
            %% File-less fixture: the skeleton is still synthesized from the
            %% loaded class' reflected super + state (BT-2570) — a null
            %% `source_file` only flips `origin` to runtime, never the definition.
            Definition = maps:get(<<"definition">>, Value),
            ?assert(is_binary(Definition)),
            ?assertNotEqual(<<>>, Definition),
            ?assertEqual(
                binary:match(Definition, <<" subclass: ", Class/binary>>) =/= nomatch,
                true
            ),
            ?assertEqual(<<"runtime">>, maps:get(<<"origin">>, Value))
        end},
        {"no-user-code: browse never invokes a method whose body would crash", fun() ->
            %% Every browse op runs against the fixture class, whose method bodies
            %% raise `must_not_run` if invoked. If any browse path sent the
            %% method to a value, these calls would surface that error. They all
            %% succeed (status [done]) → reflection only (ADR 0091 Decision 4).
            _ = decode_value(
                beamtalk_repl_ops_browse:handle(<<"browse-classes">>, #{}, make_msg(), self())
            ),
            _ = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-protocols">>,
                    #{<<"class">> => Class, <<"side">> => <<"instance">>},
                    make_msg(),
                    self()
                )
            ),
            _ = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-method-source">>,
                    #{
                        <<"class">> => Class,
                        <<"side">> => <<"instance">>,
                        <<"selector">> => <<"increment">>
                    },
                    make_msg(),
                    self()
                )
            ),
            _ = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-class-definition">>,
                    #{<<"class">> => Class},
                    make_msg(),
                    self()
                )
            ),
            ?assert(true)
        end}
    ].

%%====================================================================
%% Protocol browse tests (BT-2615)
%%====================================================================
%%
%% A protocol (e.g. Printable) is reified as a sealed abstract class object
%% (ADR 0068) dispatched by the shared `beamtalk_protocol_object` module. That
%% dispatch module carries no package and no on-disk source, so without special
%% handling every protocol would land in the browser's "(uncategorized)" bucket
%% badged "project". These tests pin the BT-2615 fixes: the `is_protocol` flag
%% (so the browser can group them under "Protocols"), origin resolution through
%% the protocol's *defining* module (so a stdlib protocol badges stdlib), and the
%% required-member rows (so a protocol's contract is visible instead of empty).

protocol_browse_test_() ->
    {setup, fun protocol_setup/0, fun protocol_cleanup/1, fun protocol_tests/1}.

protocol_setup() ->
    _ =
        case whereis(beamtalk_xref) of
            undefined ->
                case beamtalk_xref:start_link() of
                    {ok, _} -> ok;
                    {error, {already_started, _}} -> ok
                end;
            _ ->
                ok
        end,
    beamtalk_protocol_registry:init(),
    Uniq = erlang:integer_to_list(erlang:unique_integer([positive, monotonic])),
    ProtoName = list_to_atom("BrowseProto_" ++ Uniq),
    %% A stub protocol class object, dispatched (as the real ones are) by the
    %% shared beamtalk_protocol_object module — which has no package/source.
    {Pid, Owned} = start_browse_class(ProtoName, #{
        name => ProtoName,
        module => beamtalk_protocol_object,
        superclass => none,
        is_sealed => true,
        is_abstract => true,
        instance_methods => #{},
        fields => []
    }),
    %% Register the protocol with a stdlib-looking *defining* module so origin
    %% resolution can tell it apart from the dispatch module.
    DefiningModule = list_to_atom("bt@stdlib@browseproto_" ++ Uniq),
    ok = beamtalk_protocol_registry:register_protocol(#{
        name => ProtoName,
        module => DefiningModule,
        required_methods => [
            #{selector => 'asString', arity => 0},
            #{selector => 'printString', arity => 0}
        ],
        required_class_methods => [#{selector => 'fromString:', arity => 1}],
        type_params => [],
        extending => undefined
    }),
    #{
        proto_name => ProtoName,
        proto_bin => atom_to_binary(ProtoName, utf8),
        class => {ProtoName, Pid, Owned}
    }.

protocol_cleanup(#{proto_name := Name, class := {_, Pid, Owned}}) ->
    catch ets:delete(beamtalk_protocol_registry, Name),
    catch beamtalk_xref:purge_class(Name),
    case Owned of
        true -> catch gen_server:stop(Pid);
        false -> ok
    end,
    ok.

protocol_tests(#{proto_bin := Proto}) ->
    [
        {"browse-classes flags a protocol class object is_protocol=true", fun() ->
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(<<"browse-classes">>, #{}, make_msg(), self())
            ),
            Row = find_class_row(Value, Proto),
            ?assertNotEqual(undefined, Row),
            ?assertEqual(true, maps:get(<<"is_protocol">>, Row))
        end},
        {"browse-classes badges a stdlib protocol's source_origin as stdlib", fun() ->
            %% Origin resolves through the protocol's defining module, not the
            %% shared beamtalk_protocol_object dispatch module (which would read
            %% "project").
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(<<"browse-classes">>, #{}, make_msg(), self())
            ),
            Row = find_class_row(Value, Proto),
            ?assertEqual(<<"stdlib">>, maps:get(<<"source_origin">>, Row))
        end},
        {"browse-protocols surfaces a protocol's required instance members", fun() ->
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-protocols">>,
                    #{<<"class">> => Proto, <<"side">> => <<"instance">>},
                    make_msg(),
                    self()
                )
            ),
            Protocols = maps:get(<<"protocols">>, Value),
            ?assertEqual(<<"requirements">>, protocol_of(Protocols, <<"asString">>)),
            ?assertEqual(<<"requirements">>, protocol_of(Protocols, <<"printString">>))
        end},
        {"browse-protocols surfaces required class members on the class side", fun() ->
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(
                    <<"browse-protocols">>,
                    #{<<"class">> => Proto, <<"side">> => <<"class">>},
                    make_msg(),
                    self()
                )
            ),
            Protocols = maps:get(<<"protocols">>, Value),
            ?assertEqual(<<"requirements">>, protocol_of(Protocols, <<"fromString:">>))
        end}
    ].

%%====================================================================
%% source_origin (classification) / package (name) split — BT-2643
%%====================================================================

%% source_origin_of/2 returns the bare classification, never `dependency:<pkg>`.
source_origin_stdlib_module_test() ->
    %% A stdlib module classifies as stdlib regardless of its source file.
    ?assertEqual(
        <<"stdlib">>,
        beamtalk_repl_ops_browse:source_origin_of('bt@stdlib@OrderedCollection', null)
    ),
    ?assertEqual(
        <<"stdlib">>,
        beamtalk_repl_ops_browse:source_origin_of(
            'bt@stdlib@OrderedCollection', <<"stdlib/src/ordered_collection.bt">>
        )
    ).

source_origin_null_source_is_project_test() ->
    %% A non-stdlib module with no disk source (ClassBuilder / runtime class) is
    %% classified `project` — the bare classification, no package packing.
    ?assertEqual(
        <<"project">>,
        beamtalk_repl_ops_browse:source_origin_of('bt@myapp@Counter', null)
    ).

%% package_of/2 derives the package name for ALL origins, orthogonal to the
%% classification.
package_of_stdlib_test() ->
    %% Stdlib classes report the package "stdlib".
    ?assertEqual(
        <<"stdlib">>,
        beamtalk_repl_ops_browse:package_of('bt@stdlib@OrderedCollection', <<"stdlib">>)
    ).

package_of_project_package_module_test() ->
    %% A project class whose module atom encodes its package reports that package.
    ?assertEqual(
        <<"myapp">>,
        beamtalk_repl_ops_browse:package_of('bt@myapp@Counter', <<"project">>)
    ).

package_of_dependency_package_module_test() ->
    %% A dependency class reports its package from the module atom.
    ?assertEqual(
        <<"cowboy">>,
        beamtalk_repl_ops_browse:package_of('bt@cowboy@Listener', <<"dependency">>)
    ).

package_of_dependency_unknown_is_null_test() ->
    %% A dependency module that carries no package segment degrades to null
    %% (never the old "dependency:unknown" packing).
    ?assertEqual(
        null,
        beamtalk_repl_ops_browse:package_of(some_native_mod, <<"dependency">>)
    ).

package_of_project_no_segment_falls_back_test() ->
    %% A project class whose module atom has no package segment falls back to the
    %% workspace package name — null here since no workspace_meta is running.
    ?assertEqual(
        null,
        beamtalk_repl_ops_browse:package_of(plain_project_mod, <<"project">>)
    ).

package_of_module_extraction_test() ->
    ?assertEqual(<<"http">>, beamtalk_repl_ops_browse:package_of_module('bt@http@Server')),
    ?assertEqual(nil, beamtalk_repl_ops_browse:package_of_module(no_at_signs)).

%%====================================================================
%% Package-based source_origin classification — BT-2640
%%====================================================================

%% Without a running workspace_meta the project package is unknown, so a
%% packaged dependency-looking module degrades to the path-prefix fallback;
%% with a null source that lands on `project` (a wrong "project" badge is less
%% confusing than a wrong "dependency" badge).
source_origin_no_meta_packaged_null_source_is_project_test() ->
    ?assertEqual(
        <<"project">>,
        beamtalk_repl_ops_browse:source_origin_of('bt@cowboy@Listener', null)
    ).

%% No package segment + null source + no meta => project (path fallback).
source_origin_no_segment_null_source_is_project_test() ->
    ?assertEqual(
        <<"project">>,
        beamtalk_repl_ops_browse:source_origin_of(plain_mod, null)
    ).

%% Stdlib still wins first, even with a project package configured.
source_origin_with_meta_stdlib_wins_test() ->
    with_project_package(<<"myapp">>, fun() ->
        ?assertEqual(
            <<"stdlib">>,
            beamtalk_repl_ops_browse:source_origin_of('bt@stdlib@OrderedCollection', null)
        )
    end).

%% Package == project package => project, regardless of where the source file
%% resolves on disk (the primary signal is the package segment, not the path).
source_origin_with_meta_project_package_is_project_test() ->
    with_project_package(<<"myapp">>, fun() ->
        ?assertEqual(
            <<"project">>,
            beamtalk_repl_ops_browse:source_origin_of('bt@myapp@Counter', null)
        ),
        %% Even a source resolving under the project tree stays `project`.
        ?assertEqual(
            <<"project">>,
            beamtalk_repl_ops_browse:source_origin_of(
                'bt@myapp@Counter', <<"src/counter.bt">>
            )
        )
    end).

%% Package =/= project package => dependency, even when the dependency source
%% resolves under the project tree (the BT-2640 bug: a path-only check would
%% mislabel this `project`).
source_origin_with_meta_dependency_package_is_dependency_test() ->
    with_project_package(<<"myapp">>, fun() ->
        ?assertEqual(
            <<"dependency">>,
            beamtalk_repl_ops_browse:source_origin_of('bt@http@Server', null)
        ),
        %% Dep source nested under the project root still classifies as a dep.
        ?assertEqual(
            <<"dependency">>,
            beamtalk_repl_ops_browse:source_origin_of(
                'bt@http@Server', <<"_build/deps/http/src/server.bt">>
            )
        )
    end).

%% Missing-metadata robustness: meta running but with no `project_path` (so no
%% package detected) leaves the project package unknown, and a packaged module
%% falls back to the path check. With no project root to compare against, the
%% path check cannot prove a dependency, so it degrades to `project` rather than
%% dumping a wrong "dependency" badge.
source_origin_meta_without_project_path_falls_back_to_path_test() ->
    with_started_meta(#{}, fun() ->
        ?assertEqual(
            <<"project">>,
            beamtalk_repl_ops_browse:source_origin_of(
                'bt@http@Server', <<"/elsewhere/http/server.bt">>
            )
        ),
        ?assertEqual(
            <<"project">>,
            beamtalk_repl_ops_browse:source_origin_of('bt@http@Server', null)
        )
    end).

%% Start workspace_meta with a project_path pointing at a temp dir whose
%% beamtalk.toml declares package `Pkg`, run `Fun`, then tear meta down.
with_project_package(Pkg, Fun) ->
    Dir = make_project_dir(Pkg),
    try
        with_started_meta(#{project_path => Dir}, Fun)
    after
        _ = file:delete(filename:join(Dir, "beamtalk.toml")),
        _ = file:del_dir(Dir)
    end.

%% Start workspace_meta with the given extra init metadata (workspace_id,
%% created_at, repl=false are supplied), run `Fun`, then stop the server.
with_started_meta(Extra, Fun) ->
    %% Defensive: a stray server from another test would shadow ours.
    case whereis(beamtalk_workspace_meta) of
        undefined -> ok;
        Existing -> stop_meta(Existing)
    end,
    Base = #{
        workspace_id => <<"bt2640-test">>,
        created_at => erlang:system_time(second),
        repl => false
    },
    {ok, Pid} = beamtalk_workspace_meta:start_link(maps:merge(Base, Extra)),
    try
        Fun()
    after
        stop_meta(Pid)
    end.

stop_meta(Pid) when is_pid(Pid) ->
    Ref = erlang:monitor(process, Pid),
    unlink(Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 2000 ->
        erlang:demonitor(Ref, [flush]),
        ok
    end.

make_project_dir(Pkg) ->
    Uniq = erlang:integer_to_list(erlang:unique_integer([positive, monotonic])),
    Dir = filename:join(
        beamtalk_file:'tempDirectory'(), "bt2640_proj_" ++ Uniq
    ),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Toml = <<"[package]\nname = \"", Pkg/binary, "\"\n">>,
    ok = file:write_file(filename:join(Dir, "beamtalk.toml"), Toml),
    Dir.

%%====================================================================
%% Row helpers
%%====================================================================

find_class_row(Rows, Name) ->
    case [R || R <- Rows, maps:get(<<"name">>, R) =:= Name] of
        [Row | _] -> Row;
        [] -> undefined
    end.

all_selector_rows(Protocols) ->
    lists:flatten([maps:get(<<"selectors">>, P) || P <- Protocols]).

find_selector_row(Selectors, Name) ->
    case [S || S <- Selectors, maps:get(<<"selector">>, S) =:= Name] of
        [Row | _] -> Row;
        [] -> undefined
    end.

%% The name of the protocol bucket a given selector landed in, or `undefined`.
protocol_of(Protocols, Selector) ->
    case
        [
            maps:get(<<"name">>, P)
         || P <- Protocols,
            lists:any(
                fun(S) -> maps:get(<<"selector">>, S) =:= Selector end,
                maps:get(<<"selectors">>, P)
            )
        ]
    of
        [ProtocolName | _] -> ProtocolName;
        [] -> undefined
    end.
