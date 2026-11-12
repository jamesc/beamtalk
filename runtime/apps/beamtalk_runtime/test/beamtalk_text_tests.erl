%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0
%%% **DDD Context:** Object System Context / Shared Kernel

-module(beamtalk_text_tests).

-moduledoc """
EUnit tests for `beamtalk_text:unwrap/1` and `unwrap_or_raise/4` (BT-4686).
""".
-include_lib("eunit/include/eunit.hrl").
-include("beamtalk.hrl").

unwrap_tagged_string_test() ->
    ?assertEqual({ok, <<"a">>}, beamtalk_text:unwrap({beamtalk_string, <<"a">>})).

unwrap_bare_binary_test() ->
    ?assertEqual({ok, <<"a">>}, beamtalk_text:unwrap(<<"a">>)).

unwrap_rejects_other_terms_test() ->
    ?assertEqual(error, beamtalk_text:unwrap(atom)),
    ?assertEqual(error, beamtalk_text:unwrap("list")),
    ?assertEqual(error, beamtalk_text:unwrap({beamtalk_string, not_binary})).

unwrap_or_raise_returns_bytes_test() ->
    ?assertEqual(
        <<"p">>, beamtalk_text:unwrap_or_raise({beamtalk_string, <<"p">>}, 'File', 'x:', <<"h">>)
    ),
    ?assertEqual(<<"p">>, beamtalk_text:unwrap_or_raise(<<"p">>, 'File', 'x:', <<"h">>)).

unwrap_or_raise_raises_type_error_with_hint_test() ->
    try
        beamtalk_text:unwrap_or_raise(42, 'File', 'x:', <<"Path must be a String">>),
        ?assert(false)
    catch
        error:#{'$beamtalk_class' := 'TypeError', error := Inner} ->
            ?assertEqual(type_error, Inner#beamtalk_error.kind),
            ?assertEqual('File', Inner#beamtalk_error.class),
            ?assertEqual('x:', Inner#beamtalk_error.selector),
            ?assertEqual(
                {beamtalk_string, <<"Path must be a String">>}, Inner#beamtalk_error.hint
            )
    end.

unwrap_or_raise_undefined_hint_leaves_hint_unset_test() ->
    try
        beamtalk_text:unwrap_or_raise(42, 'Subprocess', 'spawn:', undefined),
        ?assert(false)
    catch
        error:#{'$beamtalk_class' := 'TypeError', error := Inner} ->
            ?assertEqual(undefined, Inner#beamtalk_error.hint)
    end.
