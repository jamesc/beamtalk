%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% @doc Type definitions for the HTTPResponse Beamtalk value object (BT-1162).
%%%
%%% **DDD Context:** Object System Context
%%%
%%% Provides the canonical Dialyzer-visible type `t()` for HTTPResponse value
%%% objects returned by `beamtalk_http`. The runtime map structure is produced
%%% by the Beamtalk compiler from `state:` declarations in `stdlib/src/HTTPResponse.bt`
%%% and constructed by `'bt@stdlib@httpresponse':'class_status:headers:body:'/5`.
%%%
%%% This module is a type-only companion to `beamtalk_http` — it exports no
%%% functions. `'bt@stdlib@httpresponse'` is excluded from the Dialyzer PLT
%%% (generated BEAM abstract code cannot be analysed); this module mirrors its
%%% map structure so that `-spec` annotations in `beamtalk_http` can reference
%%% `beamtalk_http_response:t()` and benefit from Dialyzer validation.

-module(beamtalk_http_response).

-export_type([t/0]).

%% The map type for a Beamtalk HTTPResponse value object.
%%
%% Fields mirror the `state:` declarations in `stdlib/src/HTTPResponse.bt`:
%%   state: status  :: Integer = 0
%%   state: headers :: List    = #()
%%   state: body    :: String  = ""
-type t() :: #{
    '$beamtalk_class' := 'HTTPResponse',
    'status' := integer(),
    'headers' := list(),
    'body' := binary()
}.
