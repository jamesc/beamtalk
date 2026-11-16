%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_compiler_port_atoms_tests).

%%% **DDD Context:** Compilation Context

-moduledoc """
Guards for the atoms the Rust compiler port can put in a reply (BT-4938).

`beamtalk_compiler_port:port_roundtrip/6` decodes replies with
`binary_to_term(Bin, [safe])`, which refuses an atom the VM has not seen, so
a reply atom missing from the loaded code surfaces as "Compiler port response
is malformed" instead of the real answer. Both checks run in a fresh `peer`
node, because every other test in this app names these atoms in its own
source, which interns them in the test node and hides the problem:

* every atom in the shared fixture `compiler_port_reply_atoms.json` (the Rust
  half checks that each atom the port emits is listed there) exists once
  `beamtalk_compiler_port` is loaded. This is checked on the running VM
  (`list_to_existing_atom/1`), not on the beam's `atoms` chunk: an atom that
  only occurs inside a literal is not in that chunk but is interned at load;
* the specific path errors come back from `module_name_for_path/3` instead of
  "Compiler port response is malformed".
""".

-include_lib("eunit/include/eunit.hrl").

-define(PORT_MODULE, beamtalk_compiler_port).

reply_atoms_function_matches_fixture_test() ->
    ?assertEqual(
        lists:usort(load_reply_atoms_fixture()),
        lists:usort(?PORT_MODULE:reply_atoms())
    ).

fresh_node_test_() ->
    {setup, fun start_fresh_node/0, fun stop_fresh_node/1, fun(Peer) ->
        [
            {timeout, 60,
                {"every reply atom exists once the port module is loaded", fun() ->
                    reply_atoms_exist_in_fresh_node(Peer)
                end}},
            {timeout, 60,
                {"path errors keep their reason (BT-4938)", fun() ->
                    path_errors_decode_in_fresh_node(Peer)
                end}}
        ]
    end}.

start_fresh_node() ->
    {ok, Peer, _Node} = peer:start_link(#{
        name => peer:random_name("bt4938"),
        connection => standard_io,
        args => lists:append([["-pa", P] || P <- code:get_path()]),
        wait_boot => 60000
    }),
    Peer.

stop_fresh_node(Peer) ->
    peer:stop(Peer).

reply_atoms_exist_in_fresh_node(Peer) ->
    Names = [atom_to_list(A) || A <- load_reply_atoms_fixture()],
    ?assert(length(Names) > 0),
    {module, ?PORT_MODULE} = peer:call(Peer, code, ensure_loaded, [?PORT_MODULE]),
    Missing = [N || N <- Names, not is_existing_atom(Peer, N)],
    ?assertEqual([], Missing).

is_existing_atom(Peer, Name) ->
    try peer:call(Peer, erlang, list_to_existing_atom, [Name]) of
        A when is_atom(A) -> true
    catch
        exit:{exception, badarg, _} -> false;
        error:badarg -> false
    end.

%% The reproduction from BT-4938. The two reason atoms are not written in
%% anything the peer runs: it only receives strings and binaries.
path_errors_decode_in_fresh_node(Peer) ->
    {ok, _} = peer:call(Peer, application, ensure_all_started, [beamtalk_compiler]),
    Hyphen = peer:call(
        Peer, beamtalk_compiler, module_name_for_path, [<<"src/b-ad.bt">>, <<"pkg">>, src]
    ),
    Reserved = peer:call(
        Peer, beamtalk_compiler, module_name_for_path, [<<"test/x.bt">>, <<"pkg">>, src]
    ),
    ?assertMatch(
        {error, invalid_path_segment, <<"invalid path segment 'b-ad'", _/binary>>}, Hyphen
    ),
    ?assertMatch({error, reserved_test_directory, _}, Reserved).

load_reply_atoms_fixture() ->
    Names = beamtalk_test_corpus:load_json_fixture([
        "runtime",
        "apps",
        "beamtalk_compiler",
        "test",
        "fixtures",
        "compiler_port_reply_atoms.json"
    ]),
    [binary_to_atom(N) || N <- Names].
