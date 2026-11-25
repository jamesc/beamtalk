%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_dist_address).

%%% **DDD Context:** Runtime Context

-moduledoc """
The shared rule for reading a distribution address (BT-5127, BT-5185,
BT-5233), on the Erlang side.

The Rust CLI (`beamtalk_workspace::loopback_dist_for`), the LiveView front
(`BtAttach.Workspace.loopback_dist?/1`) and this module apply one rule,
pinned by `tests/conformance/loopback_dist_conformance.json`:

* an address entry is trimmed and any IPv6 `%zone` suffix is dropped, then
  parsed strictly (`inet:parse_strict_address/1`: four dotted decimal fields
  for IPv4, no shorthand, no leading zeros);
* it is loopback when it is in `127.0.0.0/8`, is `::1`, or is an
  IPv4-mapped IPv6 address whose IPv4 address is loopback;
* an `ERL_EPMD_ADDRESS`-style comma-separated list keeps distribution on
  loopback unless some entry parses as a non-loopback address. Unset, blank,
  loopback and unparseable values all keep it there, so a typo fails closed.

The release launcher's TLS opt-in (`RELEASE_DIST_INTERFACE`, ADR 0126
amendment D2) reads its interface with `parse/1`, the same entry rule.
""".

-export([parse/1, is_loopback/1, loopback_dist/1]).

-doc """
Parse one address entry: trim it, drop an IPv6 `%zone` suffix, then parse it
strictly. `error` for anything that is not an IP address literal (host names,
`localhost` included).
""".
-spec parse(string() | binary()) -> {ok, inet:ip_address()} | error.
parse(Entry) when is_binary(Entry) ->
    parse(unicode:characters_to_list(Entry));
parse(Entry0) when is_list(Entry0) ->
    Entry = string:trim(Entry0),
    [Address | _Zone] = string:split(Entry, "%"),
    case inet:parse_strict_address(Address) of
        {ok, Ip} -> {ok, Ip};
        {error, _} -> error
    end.

-doc """
Whether `Ip` is a loopback address: `127.0.0.0/8`, `::1`, or an IPv4-mapped
IPv6 address (`::ffff:a.b.c.d`) whose IPv4 address is loopback.
""".
-spec is_loopback(inet:ip_address()) -> boolean().
is_loopback({127, _, _, _}) -> true;
is_loopback({0, 0, 0, 0, 0, 0, 0, 1}) -> true;
is_loopback({0, 0, 0, 0, 0, 16#FFFF, Hi, _Lo}) -> Hi bsr 8 =:= 127;
is_loopback(_) -> false.

-doc """
Whether distribution stays on loopback, given an `ERL_EPMD_ADDRESS`-style
value (`undefined` or `false` when unset): `false` only when some
comma-separated entry parses as a non-loopback address.
""".
-spec loopback_dist(string() | binary() | undefined | false) -> boolean().
loopback_dist(undefined) ->
    true;
loopback_dist(false) ->
    true;
loopback_dist(Value) when is_binary(Value) ->
    loopback_dist(unicode:characters_to_list(Value));
loopback_dist(Value) when is_list(Value) ->
    not lists:any(
        fun(Entry) ->
            case parse(Entry) of
                {ok, Ip} -> not is_loopback(Ip);
                error -> false
            end
        end,
        string:split(Value, ",", all)
    ).
