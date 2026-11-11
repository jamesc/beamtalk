%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_test_native_counter_facade).

%%% **DDD Context:** Object System Context

-moduledoc """
Test double for the facade module the compiler emits for a `native:` actor
(ADR 0056): it has no `init/1`, and its `__beamtalk_meta/0` names the backing
gen_server (`beamtalk_test_native_counter`). Used by
`beamtalk_native_actor_start_tests` (BT-4577).
""".

-export(['__beamtalk_meta'/0]).

'__beamtalk_meta'() ->
    #{
        native => true,
        backing_module => beamtalk_test_native_counter,
        class_var_abi => beamtalk_class_var_abi:abi()
    }.
