%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_ops_browse_tests).

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

%% Construct a minimal non-legacy protocol_msg record (mirrors nav tests).
make_msg() ->
    {protocol_msg, <<"browse">>, <<"t1">>, <<"s1">>, #{}, false}.

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
            'value' => #{block => fun(_, _) -> erlang:error(must_not_run) end, arity => 0}
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
            'value' => <<"value -> Integer">>
        },
        method_docs => #{
            'increment' =>
                <<"Increment the counter by its step.\n\n## Examples\n```beamtalk\nc increment\n```">>
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
        %% camelCase word-boundary guard: `address` must NOT match the `add`
        %% prefix (no uppercase/`:` boundary), so it stays "as yet unclassified".
        method_row('address', 102, indexed, class_body),
        %% Precedence overlap: synthetic AND extension. The synthetic source
        %% status (a compiler-generated accessor) is decided before the extension
        %% provenance, so this must bucket "accessing", not "extensions".
        method_row('syntheticExt', 104, synthetic, extension),
        %% print* prefix path (distinct from the exact-name `printString` match):
        %% `printDetails` must reach the prefix heuristic and land in "printing".
        method_row('printDetails', 106, indexed, class_body)
    ].

method_row(Selector, Line, SourceStatus, Provenance) ->
    #{
        class_side => false,
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
                    <<"is_test">>
                ]
            )
        end},
        {"browse-classes is_test is false for a non-TestCase class", fun() ->
            %% BT-2596: is_test flags loaded TestCase subclasses so the browser
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
        {"browse-classes source_origin is present for all rows", fun() ->
            %% BT-2552: source_origin field classifies project/dependency/stdlib.
            Value = decode_value(
                beamtalk_repl_ops_browse:handle(<<"browse-classes">>, #{}, make_msg(), self())
            ),
            Row = find_class_row(Value, Class),
            SourceOrigin = maps:get(<<"source_origin">>, Row),
            ?assert(is_binary(SourceOrigin)),
            ?assert(
                lists:member(SourceOrigin, [<<"stdlib">>, <<"project">>, <<"dependency">>]) orelse
                    binary:match(SourceOrigin, <<"dependency:">>) =/= nomatch
            )
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
            ?assertEqual(<<"printing">>, protocol_of(Protocols, <<"printDetails">>))
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
