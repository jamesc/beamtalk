%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_repl_op_payload_examples).

%%% **DDD Context:** REPL Session Context

-moduledoc """
The Erlang half of the per-op wire payload fixture (BT-5049).

`examples/0` lists one `op_result()` term (or one `encode_error/5` call) per
example payload. `encode/1` turns an example into the exact JSON the server
puts on the wire, through the same `beamtalk_repl_ops:encode/2` seam the
WebSocket handler uses. `beamtalk_repl_op_table_tests` checks that output
against `runtime/apps/beamtalk_workspace/test/fixtures/repl_op_payloads.json`,
which the Rust `beamtalk-repl-protocol` tests and the VS Code extension tests
decode into their client types.

A payload is `{Id, Op, Term}`. `Id` is the key in the fixture (`<op>` or
`<op>/<variant>`), `Op` the REPL op that produces it.

After an intended wire change, the failure message of
`payload_examples_match_fixture_test/0` prints the new JSON for each stale
payload; paste it into the fixture.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([examples/0, encode/1, encode_all/0]).

-export_type([example/0]).

-type example() :: {Id :: binary(), Op :: binary(), Source :: term()}.

-define(MSG(Op), {protocol_msg, Op, <<"msg-1">>, <<"sess-1">>, #{}}).

-doc "Every example payload, in fixture order.".
-spec examples() -> [example()].
examples() ->
    Warning = #{message => <<"Unused variable: x">>, hint => <<"remove it or prefix with _">>},
    ClassNotFound = beamtalk_error:with_message(
        beamtalk_error:new(class_not_found, 'Counter'), <<"Class not found: Missing">>
    ),
    Frames = [
        #{
            <<"display">> => <<"Counter>>increment (counter.bt:12)">>,
            <<"module">> => <<"bt@counter">>,
            <<"function">> => <<"increment">>,
            <<"arity">> => 2,
            <<"file">> => <<"counter.bt">>,
            <<"line">> => 12,
            <<"hidden">> => false
        },
        #{
            <<"display">> => <<"gen_server:try_handle_call/4">>,
            <<"module">> => <<"gen_server">>,
            <<"function">> => <<"try_handle_call">>,
            <<"arity">> => 4,
            <<"file">> => null,
            <<"line">> => null,
            <<"hidden">> => true
        }
    ],
    MethodRow = fun(Name, Side, Line, Status, Sig, Doc) ->
        #{
            <<"name">> => Name,
            <<"selector">> => Name,
            <<"side">> => Side,
            <<"line">> => Line,
            <<"source_status">> => Status,
            <<"signature">> => Sig,
            <<"doc">> => Doc
        }
    end,
    SiteRow = #{
        <<"class">> => <<"Counter">>,
        <<"class_side">> => false,
        <<"method">> => <<"increment">>,
        <<"line">> => 12,
        <<"source_file">> => <<"/work/proj/src/counter.bt">>,
        <<"source_origin">> => <<"project">>
    },
    [
        {<<"eval">>, <<"eval">>, {ok, 3, <<>>, []}},
        {<<"eval/output_and_warnings">>, <<"eval">>,
            {ok, {beamtalk_string, <<"hi">>}, <<"hi\n">>, [Warning]}},
        {<<"eval/trace">>, <<"eval">>, {trace, [{<<"1 + 2">>, 3}, {<<"3 * 4">>, 12}], <<>>, []}},
        {<<"eval/script_exit">>, <<"eval">>, {script_exit, 3, <<"bye\n">>, []}},
        {<<"eval/error">>, <<"eval">>,
            {encode_error, <<"Error: boom">>, <<"out\n">>, [Warning], #{
                <<"line">> => 3, <<"hint">> => <<"check the receiver">>, <<"frames">> => Frames
            }}},
        {<<"get-traces/error">>, <<"get-traces">>, {error, ClassNotFound}},
        {<<"complete">>, <<"complete">>, {completions, [<<"Counter">>, <<"CounterTest">>]}},
        {<<"diagnostics">>, <<"diagnostics">>,
            {diagnostics, [
                #{
                    message => <<"Unexpected token">>,
                    severity => <<"error">>,
                    start => 4,
                    'end' => 9
                }
            ]}},
        {<<"show-codegen">>, <<"show-codegen">>,
            {codegen, <<"module 'bt@counter' []">>, [<<"a warning">>]}},
        {<<"erlang-help">>, <<"erlang-help">>, {docs, <<"lists:map/2\n\nApply a function.">>}},
        {<<"test">>, <<"test">>,
            {test_results, #{
                '$beamtalk_class' => 'TestResult',
                total => 3,
                passed => 1,
                failed => 1,
                skipped => 1,
                duration => 12,
                tests => [
                    #{name => testOk, status => pass, class => 'CounterTest'},
                    #{name => testBad, status => fail, error => <<"expected 1, got 2">>},
                    #{name => testLater, status => skip, reason => <<"not yet">>}
                ]
            }}},
        {<<"load-source">>, <<"load-source">>, {loaded, [#{name => "Counter"}], [Warning]}},
        {<<"load-project">>, <<"load-project">>,
            {load_project, [<<"Counter">>],
                [
                    #{
                        <<"path">> => <<"src/bad.bt">>,
                        <<"kind">> => <<"compile_error">>,
                        <<"message">> => <<"Unexpected token">>
                    }
                ],
                <<"Loaded 1 class, 1 failed">>, []}},
        {<<"sessions">>, <<"sessions">>, {sessions, [#{id => <<"sess-1">>}]}},
        {<<"close">>, <<"close">>, {status, ok}},
        {<<"health">>, <<"health">>, {health, <<"ws-1">>, <<"nonce-1">>}},
        {<<"actors">>, <<"actors">>,
            {actors, [
                #{
                    pid => list_to_pid("<0.123.0>"),
                    class => 'Counter',
                    module => 'bt@counter',
                    spawned_at => 1234567890,
                    system => false
                },
                #{
                    pid => list_to_pid("<0.5.0>"),
                    class => 'TranscriptStream',
                    module => beamtalk_transcript_stream,
                    spawned_at => null,
                    system => true
                }
            ]}},
        {<<"inspect">>, <<"inspect">>, {inspect, #{count => 3, name => <<"c">>}}},
        {<<"describe">>, <<"describe">>,
            {describe,
                #{
                    <<"eval">> => #{
                        <<"params">> => [<<"code">>], <<"optional">> => [<<"trace">>]
                    },
                    <<"health">> => #{<<"params">> => []}
                },
                #{<<"protocol">> => <<"2.0">>, <<"beamtalk">> => <<"0.0.0">>}}},
        {<<"methods">>, <<"methods">>,
            {methods,
                [
                    MethodRow(<<"increment">>, <<"instance">>, 12, <<"indexed">>, null, null),
                    MethodRow(
                        <<"new">>,
                        <<"class">>,
                        null,
                        <<"synthetic">>,
                        <<"new -> Counter">>,
                        <<"Make one.">>
                    )
                ],
                [
                    #{<<"name">> => <<"count">>, <<"line">> => 5},
                    #{<<"name">> => <<"step">>, <<"line">> => null}
                ]}},
        {<<"inherited-methods">>, <<"inherited-methods">>,
            {inherited_methods, [
                maps:put(
                    <<"defining_class">>,
                    <<"Object">>,
                    MethodRow(<<"printString">>, <<"instance">>, null, <<"indexed">>, null, null)
                )
            ]}},
        {<<"list-classes">>, <<"list-classes">>,
            {class_list, [
                #{
                    <<"name">> => <<"Counter">>,
                    <<"superclass">> => <<"Actor">>,
                    <<"doc">> => <<"A counter.">>,
                    <<"sealed">> => false,
                    <<"abstract">> => false,
                    <<"internal">> => false,
                    <<"source_file">> => <<"/work/proj/src/counter.bt">>,
                    <<"actor_count">> => 2,
                    <<"source_origin">> => <<"project">>
                },
                #{
                    <<"name">> => <<"Object">>,
                    <<"superclass">> => null,
                    <<"doc">> => null,
                    <<"sealed">> => false,
                    <<"abstract">> => true,
                    <<"internal">> => false,
                    <<"source_file">> => null,
                    <<"actor_count">> => 0,
                    <<"source_origin">> => <<"stdlib">>
                }
            ]}},
        {<<"nav-query">>, <<"nav-query">>, {value, #{<<"sites">> => [SiteRow]}}},
        {<<"nav-symbols">>, <<"nav-symbols">>,
            {value, #{
                <<"classes">> => [
                    #{
                        <<"name">> => <<"Counter">>,
                        <<"source_file">> => <<"/work/proj/src/counter.bt">>,
                        <<"source_origin">> => <<"project">>,
                        <<"line">> => 3,
                        <<"methods">> => [
                            #{
                                <<"selector">> => <<"increment">>,
                                <<"class_side">> => false,
                                <<"line">> => 12
                            }
                        ]
                    }
                ]
            }}},
        {<<"browse-classes">>, <<"browse-classes">>,
            {value, [
                #{
                    <<"name">> => <<"Counter">>,
                    <<"superclass">> => <<"Actor">>,
                    <<"category">> => <<"proj">>,
                    <<"comment">> => <<"A counter.">>,
                    <<"sealed">> => false,
                    <<"typed">> => true,
                    <<"abstract">> => false,
                    <<"internal">> => false,
                    <<"source_file">> => <<"/work/proj/src/counter.bt">>,
                    <<"origin">> => <<"project">>,
                    <<"source_origin">> => <<"project">>,
                    <<"package">> => <<"proj">>,
                    <<"is_test">> => false,
                    <<"is_protocol">> => false
                }
            ]}},
        {<<"browse-protocols">>, <<"browse-protocols">>,
            {value, #{
                <<"class">> => <<"Counter">>,
                <<"side">> => <<"instance">>,
                <<"protocols">> => [
                    #{
                        <<"name">> => <<"accessing">>,
                        <<"selectors">> => [
                            #{
                                <<"selector">> => <<"increment">>,
                                <<"line">> => 12,
                                <<"source_status">> => <<"indexed">>,
                                <<"origin">> => <<"project">>,
                                <<"source_origin">> => <<"project">>,
                                <<"package">> => <<"proj">>,
                                <<"signature">> => null,
                                <<"doc">> => null
                            }
                        ]
                    }
                ]
            }}},
        {<<"browse-type-aliases">>, <<"browse-type-aliases">>,
            {value, [
                #{
                    <<"name">> => <<"Count">>,
                    <<"expansion">> => <<"Integer">>,
                    <<"doc">> => null,
                    <<"source_file">> => <<"src/types.bt">>,
                    <<"internal">> => false,
                    <<"package">> => <<"proj">>,
                    <<"source_origin">> => <<"project">>
                }
            ]}},
        {<<"browse-alias-source">>, <<"browse-alias-source">>,
            {value, #{
                <<"name">> => <<"Count">>,
                <<"package">> => <<"proj">>,
                <<"source_file">> => <<"src/types.bt">>,
                <<"source_origin">> => <<"project">>,
                <<"editable">> => false,
                <<"content">> => <<"type Count = Integer\n">>
            }}},
        {<<"list-tests">>, <<"list-tests">>,
            {value, #{
                <<"classes">> => [
                    #{<<"class">> => <<"CounterTest">>, <<"selectors">> => [<<"testIncrement">>]}
                ]
            }}},
        {<<"load-tests">>, <<"load-tests">>,
            {value, #{
                <<"classes">> => [<<"CounterTest">>],
                <<"errors">> => [],
                <<"summary">> => <<"Loaded 1 class">>
            }}},
        {<<"reload-findings">>, <<"reload-findings">>,
            {value, #{
                <<"findings">> => [
                    beamtalk_repl_protocol:encode_reload_finding(#{
                        owner => <<"Caller">>,
                        changed_class => <<"Counter">>,
                        selector => <<"reset">>,
                        classification => removal,
                        severity => <<"warning">>,
                        message => <<"Counter>>reset was removed">>,
                        sites => [#{method => <<"run">>, line => 7}],
                        start => 10,
                        'end' => 15
                    })
                ]
            }}},
        %% BT-5203: the editing writes, each `{value, Map}` of scalars.
        {<<"save-method">>, <<"save-method">>,
            {value, #{<<"class">> => <<"Counter">>, <<"selector">> => <<"increment">>}}},
        {<<"save-class">>, <<"save-class">>, {value, #{<<"path">> => <<"src/Greeter.bt">>}}},
        {<<"revert-method">>, <<"revert-method">>,
            {value, #{<<"class">> => <<"Counter">>, <<"selector">> => <<"increment">>}}},
        {<<"reload-file">>, <<"reload-file">>,
            {value, #{<<"classes">> => [<<"Counter">>, <<"Greeter">>]}}},
        %% BT-5204: workspace state. Each `{value, Map}` is the wire projection
        %% (`beamtalk_repl_ops_state`): binary keys, `null` for absent.
        {<<"flush">>, <<"flush">>,
            {value, #{
                <<"flushed">> => 1,
                <<"files">> => [<<"src/counter.bt">>],
                <<"newClasses">> => 0,
                <<"removedClasses">> => 0,
                <<"skipped">> => [
                    #{
                        <<"seq">> => 2,
                        <<"class">> => <<"Gone">>,
                        <<"reason">> => <<"destructive">>
                    }
                ],
                <<"conflicts">> => []
            }}},
        {<<"changes">>, <<"changes">>,
            {value, #{
                <<"entries">> => [
                    #{
                        <<"seq">> => 1,
                        <<"className">> => <<"Counter">>,
                        <<"selector">> => <<"increment">>,
                        <<"kind">> => <<"instance">>,
                        <<"side">> => <<"instance">>,
                        <<"intent">> => <<"durable">>,
                        <<"flushable">> => true,
                        <<"authorKind">> => <<"human">>,
                        <<"sourceFile">> => <<"src/counter.bt">>,
                        <<"orphan">> => false,
                        <<"priorEpoch">> => false,
                        <<"flushed">> => false,
                        <<"active">> => true,
                        <<"shadowed">> => false,
                        <<"clean">> => false,
                        <<"diff">> => <<"-  increment => 1\n+  increment => 2\n">>,
                        <<"oldClass">> => null,
                        <<"oldSelector">> => null
                    }
                ]
            }}},
        {<<"info">>, <<"info">>,
            {value, #{
                <<"version">> => #{
                    <<"runtime_version">> => <<"0.6.0">>,
                    <<"protocol_version">> => <<"2.0">>,
                    <<"otp_release">> => <<"27">>,
                    <<"erts_version">> => <<"15.0">>
                },
                <<"node">> => <<"beamtalk_workspace_ab12@localhost">>,
                <<"workspace_id">> => <<"ws-ab12">>,
                <<"settings">> => #{<<"autoflush">> => false}
            }}}
    ].

-doc "The wire JSON (a binary) of one example.".
-spec encode(example()) -> binary().
encode({_Id, Op, {encode_error, Message, Output, Warnings, Metadata}}) ->
    beamtalk_repl_protocol:encode_error(
        Message, ?MSG(Op), fun(M) -> M end, Output, Warnings, Metadata
    );
encode({_Id, Op, Term}) ->
    beamtalk_repl_ops:encode(Term, ?MSG(Op)).

-doc "Every example's decoded wire JSON, keyed by example id.".
-spec encode_all() -> #{binary() => json:decode_value()}.
encode_all() ->
    maps:from_list([{Id, json:decode(encode(Ex))} || {Id, _Op, _} = Ex <- examples()]).
