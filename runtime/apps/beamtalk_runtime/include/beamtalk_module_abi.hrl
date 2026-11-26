%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% The `module_abi` value compiled modules declare in `__beamtalk_meta/0`.
%% It must equal codegen's `MODULE_ABI` (`class_meta.rs`); the Rust test
%% `module_abi_matches_the_runtime_header` enforces that.
%%
%% History: 1-3 were the class-variable ABI generations of ADR 0130 / 0137 /
%% 0142. ADR 0153 removed class variables, deleting the runtime modules that
%% modules of those generations call, so the number moved to 4 under a new key:
%% a module that declares no `module_abi` is refused with a clear message.

-ifndef(BEAMTALK_MODULE_ABI_HRL).
-define(BEAMTALK_MODULE_ABI_HRL, true).

-define(BT_MODULE_ABI, 4).

-endif.
