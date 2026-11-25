%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_tls).

%%% **DDD Context:** Runtime Context

-moduledoc """
Whether Erlang distribution runs over TLS *with peer verification* (ADR 0126
§9 item 2 and its amendment, decision D2; BT-5233).

`-proto_dist inet_tls` alone encrypts the link but authenticates nothing: with
OTP's defaults a node accepts any client and trusts any server certificate.
Distribution counts as TLS here only when all three of these hold:

* the server side sets `{verify, verify_peer}`;
* the server side sets `{fail_if_no_peer_cert, true}`, so a client with no
  certificate is refused rather than let in unverified;
* the client side sets `{verify, verify_peer}`.

The settings come from where OTP's `inet_tls_dist` reads them: the
`-ssl_dist_optfile` file (`[{server, Opts}, {client, Opts}]`) and the
`-ssl_dist_opt server_<opt> <value> client_<opt> <value>` flags. OTP uses both
sources, so each setting must appear in at least one of them, and every place
that sets it must set the safe value; a conflicting value anywhere fails
closed.

`enabled/0` is the check `beamtalk_node:tls_distribution/0` makes before an
off-host connect. `check_optfile/1` is the release launcher's refusal to start
with `RELEASE_DISTRIBUTION=tls` and an optfile that misses one of the three
(`beamtalk_release_launcher:tls_preflight/1`). Both use `peer_verification/2`.
""".

-export([
    enabled/0,
    verified/3,
    peer_verification/2,
    read_optfile/1,
    check_optfile/1,
    format_error/1
]).

-type side() :: server | client.
-type setting() :: server_verify_peer | server_fail_if_no_peer_cert | client_verify_peer.
-type reason() ::
    {optfile_missing, file:filename_all()}
    | {optfile_unreadable, file:filename_all(), term()}
    | {optfile_malformed, file:filename_all(), term()}
    | {peer_verification, [setting(), ...]}.
-export_type([reason/0]).

%% Each required setting: its name, the side, the ssl option and the value.
-define(REQUIRED, [
    {server_verify_peer, server, verify, verify_peer},
    {server_fail_if_no_peer_cert, server, fail_if_no_peer_cert, true},
    {client_verify_peer, client, verify, verify_peer}
]).

-doc """
Whether this node runs TLS distribution with peer verification: a
`-proto_dist` ending in `_tls`, and the three settings above. The optfile's
options are read from the table OTP loaded them into at boot
(`ssl_dist_opts`) when distribution is up, else from the file itself; a file
that cannot be read counts as setting nothing.
""".
-spec enabled() -> boolean().
enabled() ->
    verified(arg_words(proto_dist), arg_words(ssl_dist_opt), optfile_opts()).

-doc """
The decision `enabled/0` makes, from explicit inputs: the `-proto_dist`
values, the `-ssl_dist_opt` words and the optfile's options.
""".
-spec verified([string()], [string()], [{side(), list()}]) -> boolean().
verified(ProtoDists, DistOptWords, FileOpts) ->
    lists:any(fun(P) -> lists:suffix("_tls", P) end, ProtoDists) andalso
        peer_verification(DistOptWords, FileOpts) =:= ok.

-doc """
`ok` if the `-ssl_dist_opt` words and the optfile's options together set all
three peer-verification settings, else the ones that are missing (or set to
anything else somewhere).
""".
-spec peer_verification([string()], [{side(), list()}]) ->
    ok | {error, {peer_verification, [setting(), ...]}}.
peer_verification(DistOptWords, FileOpts) ->
    Pairs = word_pairs(DistOptWords),
    case
        [
            Name
         || {Name, Side, Key, Want} <- ?REQUIRED, not settled(Side, Key, Want, Pairs, FileOpts)
        ]
    of
        [] -> ok;
        Missing -> {error, {peer_verification, Missing}}
    end.

-doc """
Read an `-ssl_dist_optfile` the way OTP's `ssl_dist_sup` does: one Erlang
expression (normally `[{server, Opts}, {client, Opts}].`) with no free
variables, evaluating to a list.
""".
-spec read_optfile(file:filename_all()) -> {ok, [{side(), list()}]} | {error, reason()}.
read_optfile(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            try consult(Bin) of
                Opts when is_list(Opts) -> {ok, Opts};
                Other -> {error, {optfile_malformed, Path, {not_a_list, Other}}}
            catch
                throw:Detail -> {error, {optfile_malformed, Path, Detail}};
                Class:Detail -> {error, {optfile_malformed, Path, {Class, Detail}}}
            end;
        {error, enoent} ->
            {error, {optfile_missing, Path}};
        {error, Posix} ->
            {error, {optfile_unreadable, Path, Posix}}
    end.

-doc """
`ok` if the optfile at `Path` exists, parses, and sets all three
peer-verification settings by itself.
""".
-spec check_optfile(file:filename_all()) -> ok | {error, reason()}.
check_optfile(Path) ->
    case read_optfile(Path) of
        {ok, Opts} -> peer_verification([], Opts);
        {error, _} = Error -> Error
    end.

-doc "An operator-facing explanation of a `reason()`, with the fix.".
-spec format_error(reason()) -> unicode:chardata().
format_error({optfile_missing, Path}) ->
    io_lib:format(
        "the TLS distribution option file '~ts' does not exist; "
        "set RELEASE_SSL_DIST_OPTFILE to the file that names this node's certificate, "
        "key and CA (see docs/development/deploying.md, \"Clustering across hosts\")",
        [Path]
    );
format_error({optfile_unreadable, Path, Posix}) ->
    io_lib:format("cannot read the TLS distribution option file '~ts': ~ts", [
        Path, file:format_error(Posix)
    ]);
format_error({optfile_malformed, Path, Detail}) ->
    io_lib:format(
        "the TLS distribution option file '~ts' is not one Erlang term "
        "[{server, [...]}, {client, [...]}]. (~0tp)",
        [Path, Detail]
    );
format_error({peer_verification, Missing}) ->
    [
        "the TLS distribution option file does not make both sides verify their peer; add ",
        lists:join("; ", [setting_fix(S) || S <- Missing])
    ].

%%% ============================================================================
%%% Internal
%%% ============================================================================

setting_fix(server_verify_peer) -> "{verify, verify_peer} to the server list";
setting_fix(server_fail_if_no_peer_cert) -> "{fail_if_no_peer_cert, true} to the server list";
setting_fix(client_verify_peer) -> "{verify, verify_peer} to the client list".

%% Every place that sets `Key` on `Side` must set `Want`, and at least one must.
settled(Side, Key, Want, Pairs, FileOpts) ->
    Flag = atom_to_list(Side) ++ "_" ++ atom_to_list(Key),
    FromWords = [V =:= atom_to_list(Want) || {F, V} <- Pairs, F =:= Flag],
    FromFile = [
        V =:= Want
     || {S, Opts} <- FileOpts, S =:= Side, is_list(Opts), {K, V} <- Opts, K =:= Key
    ],
    case FromWords ++ FromFile of
        [] -> false;
        Seen -> lists:all(fun(Ok) -> Ok end, Seen)
    end.

word_pairs([Flag, Value | Rest]) -> [{Flag, Value} | word_pairs(Rest)];
word_pairs(_) -> [].

arg_words(Flag) ->
    case init:get_argument(Flag) of
        {ok, Values} -> lists:append(Values);
        error -> []
    end.

optfile_opts() ->
    try
        ets:tab2list(ssl_dist_opts)
    catch
        error:badarg ->
            case init:get_argument(ssl_dist_optfile) of
                {ok, [[File]]} ->
                    case read_optfile(File) of
                        {ok, Opts} -> Opts;
                        {error, _} -> []
                    end;
                _ ->
                    []
            end
    end.

consult(Bin) ->
    Encoding =
        case epp:read_encoding_from_binary(Bin) of
            none -> latin1;
            Enc -> Enc
        end,
    String =
        case unicode:characters_to_list(Bin, Encoding) of
            S when is_list(S) -> S;
            _ -> throw(encoding_error)
        end,
    Tokens =
        case erl_scan:string(String) of
            {ok, Ts, _} -> Ts;
            {error, Info, Location} -> throw({scan_error, Info, Location})
        end,
    Expr =
        case erl_parse:parse_exprs(Tokens) of
            {ok, [E]} -> E;
            {ok, _} -> throw(not_one_expression);
            {error, Info2} -> throw({parse_error, Info2})
        end,
    {value, Value, Bindings} = erl_eval:expr(Expr, erl_eval:new_bindings()),
    case erl_eval:bindings(Bindings) of
        [] -> Value;
        Other -> throw({bindings, Other})
    end.
