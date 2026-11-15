%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% Shared by `beamtalk_repl_loader`, its split-out modules
%%% (`beamtalk_repl_loader_*`) and `beamtalk_workspace_callback_impl`.

%% ChangeLog entry construction lives in its own module (BT-4683).
-define(ENTRIES, beamtalk_repl_loader_changelog_entries).
