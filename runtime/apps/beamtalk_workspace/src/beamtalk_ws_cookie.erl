%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_ws_cookie).

%%% **DDD Context:** REPL Session Context

-moduledoc """
The one place the console compares a client-supplied cookie to the node's.

A non-distributed node's cookie is the constant atom `nocookie`, a publicly
known string, so it is never accepted (BT-5066): the CLI starts the node
distributed under a random cookie, and a node without a real cookie refuses
every auth rather than authenticating with a constant.

EUnit nodes are non-distributed, so under `TEST` `nocookie` is accepted unless
the `allow_nocookie` app env of `beamtalk_workspace` is set to `false`. That
seam does not exist in release builds.
""".

-export([valid/1]).

-doc "True when `Provided` is the node's real cookie (constant-time compare).".
-spec valid(term()) -> boolean().
valid(Provided) when is_binary(Provided) ->
    Node = atom_to_binary(erlang:get_cookie(), utf8),
    cookie_usable(Node) andalso
        byte_size(Provided) =:= byte_size(Node) andalso
        crypto:hash_equals(Provided, Node);
valid(_) ->
    false.

-ifdef(TEST).
cookie_usable(<<"nocookie">>) -> application:get_env(beamtalk_workspace, allow_nocookie, true);
cookie_usable(_) -> true.
-else.
cookie_usable(<<"nocookie">>) -> false;
cookie_usable(_) -> true.
-endif.
