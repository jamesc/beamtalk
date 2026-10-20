%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Actor System Context

-module(beamtalk_bt3963_sup_fixture).
-moduledoc """
Stand-in for a compiled Beamtalk supervisor class module (ADR 0135 §2, BT-3963).

Mirrors what `supervisor_codegen.rs` generates: `start_link/0,1` and an
`init/1` that forwards the start args to `beamtalk_supervisor:static_init/3`
(or `dynamic_init/3`), plus the instance-side dispatch entry points
(`dispatch/4`, `has_method/1`, `has_method_local/1`) through which the runtime
reads each configuration setting from the supervisor being started.

The instance methods are supplied by the test: `configure/1` stores a map of
`Selector => Value | fun((Self) -> Value)` in a persistent term, and the class
is registered by the test with `module => ?MODULE`. `kind/1` picks the init
target for the two class names the tests use.
""".

-export([
    start_link/0,
    start_link/1,
    init/1,
    dispatch/4,
    has_method/1,
    has_method_local/1,
    configure/1,
    unconfigure/0
]).

-define(CONFIG, {?MODULE, config}).

%% The class names the tests register against this module.
-define(STATIC_CLASS, 'BT3963Sup').
-define(DYNAMIC_CLASS, 'BT3963Pool').

-doc "Set the instance methods (and the class kind) the fixture answers.".
-spec configure(#{kind := static | dynamic, atom() => term()}) -> ok.
configure(Config) ->
    persistent_term:put(?CONFIG, Config).

-doc "Remove the configuration stored by `configure/1`.".
-spec unconfigure() -> ok.
unconfigure() ->
    _ = persistent_term:erase(?CONFIG),
    ok.

config() ->
    persistent_term:get(?CONFIG, #{kind => static}).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, nil).

-spec start_link(term()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init(Args) ->
    case maps:get(kind, config()) of
        static -> beamtalk_supervisor:static_init(?MODULE, ?STATIC_CLASS, Args);
        dynamic -> beamtalk_supervisor:dynamic_init(?MODULE, ?DYNAMIC_CLASS, Args)
    end.

dispatch(Selector, _Args, Self, State) ->
    case maps:find(Selector, config()) of
        {ok, Fun} when is_function(Fun, 1) -> {reply, Fun(Self), State};
        {ok, Value} -> {reply, Value, State};
        error -> {error, beamtalk_error:new(does_not_understand, ?MODULE, Selector), State}
    end.

has_method(Selector) ->
    has_method_local(Selector).

has_method_local(kind) ->
    false;
has_method_local(Selector) ->
    maps:is_key(Selector, config()).
