%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%% The class-variable process-dictionary key shape (ADR 0130 §1, §2) used by
%% `beamtalk_class_vars` and the class-variable home. Compiled code no longer
%% emits class-variable access (ADR 0153 Phase 2), so this header is no longer
%% generated; ADR 0153 Phase 3 deletes it with the runtime modules.
%% `BT_CLASS_VAR_ABI` must equal codegen's `CLASS_VAR_ABI`
%% (`class_meta.rs`); the Rust test `class_var_abi_matches_the_runtime_header`
%% enforces that.

-ifndef(BEAMTALK_CLASS_VARS_KEYS_HRL).
-define(BEAMTALK_CLASS_VARS_KEYS_HRL, true).

%% The key of a class's variables; `ClassTag` is the metaclass tag
%% (`'Name class'`, `element(2, ClassSelf)`).
-define(BT_CLASS_VARS_KEY(ClassTag), {'$bt_class_vars', ClassTag}).

%% The process-dictionary key naming the live invocation's class key.
-define(BT_CLASS_VARS_HOME, '$bt_class_vars_home').

%% The read-only marker planted under a class key by `with_snapshot/2`.
-define(BT_CLASS_VARS_RO(ClassTag), {'$bt_class_vars_ro', ClassTag}).

%% The `class_var_abi` value compiled modules declare in `__beamtalk_meta/0`.
-define(BT_CLASS_VAR_ABI, 3).

-endif.
