%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_json_mapping).

%%% **DDD Context:** Object System Context

-moduledoc """
Native helpers behind the `JsonMapping` stdlib class (BT-2819, BT-5191).

| Function          | Description                                                  |
|-------------------|--------------------------------------------------------------|
| `cached/2`        | A class's mapping, built once per class until a class write  |
| `secretSlots/1`   | The `secretState` slots of an actor (inherited ones included) |
| `redacted/0`      | The value a redacted secret is emitted as                    |

`cached/2` memoises in `beamtalk_class_derived_cache` under
`{json_mapping, ClassName}`. That cache is flushed by every class write (load,
reload, redefinition, removal), so editing `class jsonMapping` at the REPL is
picked up on the next `asJson`. A raising build caches nothing.
""".

-include_lib("beamtalk_runtime/include/beamtalk.hrl").

-export([cached/2, secretSlots/1, redacted/0]).

-doc "The mapping for `ClassName`: the cached one, or `Build()` cached.".
-spec cached(atom(), fun(() -> term())) -> term().
cached(ClassName, Build) when is_atom(ClassName), is_function(Build, 0) ->
    beamtalk_class_derived_cache:fetch({json_mapping, ClassName}, fun() ->
        {cache, Build()}
    end).

-doc """
The state slots an actor's class (and its ancestors) declare in
`class secretState`, read from the compiled `'__beamtalk_secrets'/0` metadata
the actor's status redaction uses. Any other object has none.
""".
-spec secretSlots(term()) -> [atom()].
secretSlots(#beamtalk_object{class_mod = Module}) when is_atom(Module) ->
    case erlang:function_exported(Module, '__beamtalk_secrets', 0) of
        true ->
            {Slots, _Selectors} = Module:'__beamtalk_secrets'(),
            Slots;
        false ->
            []
    end;
secretSlots(_Other) ->
    [].

-doc "The value a redacted secret slot is emitted as (`\"[redacted]\"`).".
-spec redacted() -> beamtalk_string:t().
redacted() -> beamtalk_primitive:string_from_binary(beamtalk_actor_redaction:redacted()).
