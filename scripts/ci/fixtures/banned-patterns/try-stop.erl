%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
    try gen_server:stop(Pid) catch _:_ -> ok end,
