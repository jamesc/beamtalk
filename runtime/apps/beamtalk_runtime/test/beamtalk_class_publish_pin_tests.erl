%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context

-module(beamtalk_class_publish_pin_tests).

-moduledoc """
Pins what `beamtalk_object_class` publishes about a class: the
`beamtalk_class_metadata` writes (`put_reflection`, `put_flags`,
`put_method_records`, `put_class_doc`) and the hot-patch notification to the
compiler server, in call order.

Captured for a value class, an actor class, and a class with class methods and
return types, at registration, after a hot reload (`update_class/2`) and after a
hot-patch (`put_method/4`). The expected traces were recorded from the code
before `publish_reflection/1` and `notify_hot_patch/1` moved into
`beamtalk_class_publish`, so they guard that move against behaviour change.
""".
-include_lib("eunit/include/eunit.hrl").

-define(WATCHED, [put_reflection, put_flags, put_method_records, put_class_doc]).

setup() ->
    case whereis(pg) of
        undefined -> {ok, _} = pg:start_link();
        _ -> ok
    end,
    beamtalk_class_registry:ensure_hierarchy_table(),
    beamtalk_class_registry:ensure_module_table(),
    beamtalk_class_registry:ensure_pid_table(),
    beamtalk_test_class_teardown:stop_pg_class_processes(),
    meck:new(beamtalk_class_metadata, [passthrough, no_link]),
    meck:new(beamtalk_compiler_server, [non_strict, no_link]),
    meck:expect(beamtalk_compiler_server, register_class, fun(_, _) -> ok end),
    %% put_method re-indexes xref from source; no compiler app in this test.
    meck:expect(beamtalk_compiler_server, find_all_sends_in_source, fun(_) -> {ok, []} end),
    ok.

teardown(_) ->
    meck:unload(beamtalk_compiler_server),
    meck:unload(beamtalk_class_metadata),
    beamtalk_test_class_teardown:stop_pg_class_processes(),
    ok.

%% Calls made for `Name` since the last reset, as `{Fun, Args}` in call order.
%% Funs are replaced by '$fun' so traces compare across runs.
trace(Name) ->
    Meta = [
        {F, norm(Args)}
     || {_Pid, {beamtalk_class_metadata, F, [Name | _] = Args}, _Ret} <- meck:history(
            beamtalk_class_metadata
        ),
        lists:member(F, ?WATCHED)
    ],
    Compiler = [
        {register_class, norm(Args)}
     || {_Pid, {beamtalk_compiler_server, register_class, [Name | _] = Args}, _Ret} <- meck:history(
            beamtalk_compiler_server
        )
    ],
    {Meta, Compiler}.

reset() ->
    meck:reset(beamtalk_class_metadata),
    meck:reset(beamtalk_compiler_server).

norm(F) when is_function(F) -> '$fun';
norm(M) when is_map(M) -> maps:map(fun(_, V) -> norm(V) end, M);
norm(L) when is_list(L) -> [norm(X) || X <- L];
norm(T) when is_tuple(T) -> list_to_tuple([norm(X) || X <- tuple_to_list(T)]);
norm(X) -> X.

value_info() ->
    #{
        name => 'PinPoint',
        module => pin_point,
        superclass => none,
        is_sealed => true,
        is_typed => true,
        fields => [x, y],
        doc => <<"A point.">>,
        shape_version => 2,
        instance_methods => #{x => #{arity => 0}, plus => #{arity => 1}},
        method_source => #{x => <<"x => self.x">>},
        method_signatures => #{plus => <<"plus: other">>},
        method_docs => #{x => <<"The x.">>}
    }.

actor_info() ->
    #{
        name => 'PinCounter',
        module => pin_counter,
        superclass => none,
        is_abstract => false,
        fields => [count],
        instance_methods => #{increment => #{arity => 0}, get => #{arity => 0}},
        method_return_types => #{get => 'Integer'}
    }.

class_side_info() ->
    #{
        name => 'PinFactory',
        module => pin_factory,
        superclass => none,
        is_internal => true,
        fields => [seed],
        instance_methods => #{make => #{arity => 0}},
        class_methods => #{create => #{arity => 1}, default => #{arity => 0}},
        class_method_source => #{default => <<"default => self new">>},
        class_method_signatures => #{create => <<"create: seed">>},
        class_method_docs => #{default => <<"The default.">>},
        method_return_types => #{make => {generic, 'List', ['Integer']}},
        class_method_return_types => #{create => 'PinFactory', default => 'PinFactory'}
    }.

%% Registration, hot reload, hot patch for one class; returns the three traces.
run(Name, Info, ReloadInfo) ->
    reset(),
    {ok, Pid} = beamtalk_object_class:start_link(Name, Info),
    Registered = trace(Name),
    reset(),
    {ok, _} = beamtalk_object_class:update_class(Name, ReloadInfo),
    Reloaded = trace(Name),
    reset(),
    ok = beamtalk_object_class:put_method(Pid, pinned, fun() -> 1 end, <<"pinned => 1">>),
    Patched = trace(Name),
    {Registered, Reloaded, Patched}.

pin_test_() ->
    {setup, fun setup/0, fun teardown/1, fun(_) ->
        [
            ?_test(begin
                Info = value_info(),
                R = run(
                    'PinPoint',
                    Info,
                    Info#{
                        instance_methods => #{x => #{arity => 0}},
                        fields => [x, y, z],
                        doc => <<"A point v2.">>
                    }
                ),
                dump(value, R),
                ?assertEqual(expected(value), R)
            end),
            ?_test(begin
                Info = actor_info(),
                R = run(
                    'PinCounter',
                    Info,
                    Info#{
                        instance_methods => #{
                            increment => #{arity => 0}, get => #{arity => 0}, reset => #{arity => 0}
                        },
                        method_return_types => #{get => 'Integer', reset => 'Nil'}
                    }
                ),
                dump(actor, R),
                ?assertEqual(expected(actor), R)
            end),
            ?_test(begin
                Info = class_side_info(),
                R = run(
                    'PinFactory',
                    Info,
                    Info#{
                        class_methods => #{create => #{arity => 1}},
                        class_method_return_types => #{create => 'PinFactory'}
                    }
                ),
                dump(class_side, R),
                ?assertEqual(expected(class_side), R)
            end)
        ]
    end}.

dump(Key, R) ->
    case os:getenv("PIN_DUMP") of
        false -> ok;
        Dir -> file:write_file(Dir ++ "/" ++ atom_to_list(Key), io_lib:format("~p.~n", [R]))
    end.

expected(Key) ->
    {ok, [T]} = file:consult(
        filename:join(code:lib_dir(beamtalk_runtime), "test/pin_" ++ atom_to_list(Key) ++ ".eterm")
    ),
    T.
