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

-export([valid/1, adopt_env_cookie/0, env_var/0]).

-define(COOKIE_ENV, "BEAMTALK_ERL_COOKIE").

-doc """
The environment variable the CLI and the release launcher hand the cookie
over in (BT-5126). Kept in one place so the Rust side's `COOKIE_ENV` has a
single Erlang counterpart for the conformance test to compare against.
""".
-spec env_var() -> string().
env_var() -> ?COOKIE_ENV.

-doc """
Adopt the cookie from `BEAMTALK_ERL_COOKIE` (BT-5126).

`-setcookie`, `ERL_AFLAGS` and `-args_file` all expand into the emulator's
argv, which any local user can read with `ps` or `/proc/<pid>/cmdline`. The
process environment is readable only by the owner, so the launcher puts the
real cookie there, starts the node with a random throwaway `-setcookie`
placeholder (so OTP does not create `~/.erlang.cookie`), and the node calls
this before anything authenticates. The variable is removed afterwards so it
is not inherited by child processes. Returns `none` when the variable is
unset or empty, or on a non-distributed node (no cookie to set).
""".
-spec adopt_env_cookie() -> adopted | none.
adopt_env_cookie() ->
    Result =
        case os:getenv(?COOKIE_ENV) of
            Cookie when is_list(Cookie), Cookie =/= "" ->
                case node() of
                    'nonode@nohost' ->
                        none;
                    Node ->
                        true = erlang:set_cookie(Node, list_to_atom(Cookie)),
                        adopted
                end;
            _ ->
                none
        end,
    _ = os:unsetenv(?COOKIE_ENV),
    Result.

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
