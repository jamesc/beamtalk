%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_extensions).

%%% **DDD Context:** Runtime Context (test support)

-moduledoc """
Scoped extension-method fixture (BT-5038): register an extension on the
production `beamtalk_extensions` registry for the duration of one test body,
and always remove it afterwards.

Replaces the hand-written
`register(...), try ... after ets:delete(beamtalk_extensions, Key), invalidate() end`
scaffold. Removal goes through `beamtalk_extensions:unregister/2`, which also
clears the source row, the shadow flag, the conflict history and the derived
dispatch cache, so nothing leaks into the next test.
""".

-export([with_extension/4]).

-define(TEST_OWNER, test_owner).

-doc """
Ensure the extension registry exists, register `Selector` on `Class` with `Fun`
(owner `test_owner`), run `Body()`, then unregister it even if `Body` raises.
Returns what `Body()` returns.
""".
-spec with_extension(atom(), atom(), function(), fun(() -> T)) -> T when T :: term().
with_extension(Class, Selector, Fun, Body) when is_function(Body, 0) ->
    ok = beamtalk_extensions:init(),
    ok = beamtalk_extensions:register(Class, Selector, Fun, ?TEST_OWNER),
    try
        Body()
    after
        _ = beamtalk_extensions:unregister(Class, Selector)
    end.
