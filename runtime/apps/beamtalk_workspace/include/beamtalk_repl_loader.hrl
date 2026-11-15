%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% Shared by `beamtalk_repl_loader` and the modules split out of it
%%% (`beamtalk_repl_loader_activate`, `_support`, `_reload`, `_methods`; BT-4907).

%% ChangeLog entry construction lives in its own module (BT-4683).
-define(ENTRIES, beamtalk_repl_loader_changelog_entries).
