%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Binary class implementation — serialization and byte operations.
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Binary provides class-side methods for serializing/deserializing Erlang
%%% terms and working with byte-level binary data.
%%%
%%% ## Methods
%%%
%%% | Selector        | Description                                      |
%%% |-----------------|--------------------------------------------------|
%%% | `serialize:`    | Term → binary (via `term_to_binary/1`)            |
%%% | `deserialize:`  | Binary → term (via `binary_to_term/2`, safe mode) |
%%% | `size:`         | Binary → byte size                               |
%%% | `fromIolist:`   | Iolist → binary                                  |

-module(beamtalk_binary).

-export(['serialize:'/1, 'deserialize:'/1, 'size:'/1, 'fromIolist:'/1]).
-export([serialize/1, deserialize/1, size/1, fromIolist/1]).

-include_lib("beamtalk_runtime/include/beamtalk.hrl").
-include_lib("kernel/include/logger.hrl").

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Serialize any term to a binary via `erlang:term_to_binary/1`.
-spec 'serialize:'(term()) -> binary().
'serialize:'(Term) ->
    erlang:term_to_binary(Term).

%% @doc Deserialize a binary back to the original term.
%%
%% Uses safe mode (`[safe]`) to prevent atom table exhaustion from untrusted
%% input. Atoms not already in the atom table will cause an error.
-spec 'deserialize:'(binary()) -> term().
'deserialize:'(Bin) when is_binary(Bin) ->
    try
        erlang:binary_to_term(Bin, [safe])
    catch
        error:badarg ->
            Error0 = beamtalk_error:new(type_error, 'Binary'),
            Error1 = beamtalk_error:with_selector(Error0, 'deserialize:'),
            Error2 = beamtalk_error:with_hint(Error1, <<"Binary data is invalid or contains unknown atoms">>),
            beamtalk_error:raise(Error2)
    end;
'deserialize:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'Binary'),
    Error1 = beamtalk_error:with_selector(Error0, 'deserialize:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be a Binary">>),
    beamtalk_error:raise(Error2).

%% @doc Return the byte size of a binary.
-spec 'size:'(binary()) -> non_neg_integer().
'size:'(Bin) when is_binary(Bin) ->
    erlang:byte_size(Bin);
'size:'(_) ->
    Error0 = beamtalk_error:new(type_error, 'Binary'),
    Error1 = beamtalk_error:with_selector(Error0, 'size:'),
    Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be a Binary">>),
    beamtalk_error:raise(Error2).

%% @doc Convert an iolist to a flat binary.
-spec 'fromIolist:'(iolist()) -> binary().
'fromIolist:'(Iolist) ->
    try
        erlang:iolist_to_binary(Iolist)
    catch
        error:badarg ->
            Error0 = beamtalk_error:new(type_error, 'Binary'),
            Error1 = beamtalk_error:with_selector(Error0, 'fromIolist:'),
            Error2 = beamtalk_error:with_hint(Error1, <<"Argument must be a valid iolist (list of binaries, integers 0-255, or nested iolists)">>),
            beamtalk_error:raise(Error2)
    end.

%%% ============================================================================
%%% FFI aliases — no-colon names for Erlang FFI dispatch
%%% ============================================================================

%% @doc FFI alias for serialize:/1.
-spec serialize(term()) -> binary().
serialize(X) -> 'serialize:'(X).

%% @doc FFI alias for deserialize:/1.
-spec deserialize(binary()) -> term().
deserialize(X) -> 'deserialize:'(X).

%% @doc FFI alias for size:/1.
-spec size(binary()) -> non_neg_integer().
size(X) -> 'size:'(X).

%% @doc FFI alias for fromIolist:/1.
-spec fromIolist(iolist()) -> binary().
fromIolist(X) -> 'fromIolist:'(X).
