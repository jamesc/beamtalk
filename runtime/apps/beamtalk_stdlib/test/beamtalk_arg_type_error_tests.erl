%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_arg_type_error_tests).

%%% **DDD Context:** Object System Context

-moduledoc """
Pins the `type_error` a wrong-typed text argument raises, per stdlib module
(BT-4902). These primitives share `beamtalk_text:unwrap_or_raise/4`; the
tests fix the kind, class, selector and hint/message each call site
reports, so a change to the shared helper cannot silently change them.
""".

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% The raised error's {Kind, Class, Selector, Hint-or-Message}. `hint` and
%% `message` are tagged Strings; the hint wins when set.
raised(Fun) ->
    try
        Fun(),
        no_error
    catch
        %% BT-4900: kept hand-written - value-returning helper (maps the error to a comparison tuple)
        error:#{error := #beamtalk_error{} = E} ->
            Text =
                case E#beamtalk_error.hint of
                    undefined -> E#beamtalk_error.message;
                    Hint -> Hint
                end,
            {E#beamtalk_error.kind, E#beamtalk_error.class, E#beamtalk_error.selector, Text}
    end.

type_error(Class, Selector, Text) ->
    {type_error, Class, Selector, {beamtalk_string, Text}}.

%%% ============================================================================
%%% Regex
%%% ============================================================================

regex_from_test() ->
    ?assertEqual(
        type_error('Regex', 'from:', <<"Pattern must be a String">>),
        raised(fun() -> beamtalk_regex:from(42) end)
    ),
    ?assertEqual(
        type_error('Regex', 'from:options:', <<"Pattern must be a String and options a List">>),
        raised(fun() -> beamtalk_regex:from({beamtalk_string, <<"a">>}, not_a_list) end)
    ),
    ?assertEqual(
        type_error('Regex', 'from:options:', <<"Pattern must be a String and options a List">>),
        raised(fun() -> beamtalk_regex:from(42, []) end)
    ).

regex_string_methods_test() ->
    Hint = <<"Receiver must be a String, argument a String or Regex">>,
    S = {beamtalk_string, <<"abc">>},
    ?assertEqual(
        type_error('String', 'matchesRegex:', Hint),
        raised(fun() -> beamtalk_regex:matches_regex(42, S) end)
    ),
    ?assertEqual(
        type_error('String', 'matchesRegex:', Hint),
        raised(fun() -> beamtalk_regex:matches_regex(S, 42) end)
    ),
    ?assertEqual(
        type_error('String', 'firstMatch:', Hint),
        raised(fun() -> beamtalk_regex:first_match(S, 42) end)
    ),
    ?assertEqual(
        type_error('String', 'allMatches:', Hint),
        raised(fun() -> beamtalk_regex:all_matches(42, S) end)
    ),
    ?assertEqual(
        type_error('String', 'splitRegex:', Hint),
        raised(fun() -> beamtalk_regex:split_regex(S, 42) end)
    ),
    ReplHint = <<"Arguments must be Strings or Regex">>,
    ?assertEqual(
        type_error('String', 'replaceRegex:with:', ReplHint),
        raised(fun() -> beamtalk_regex:replace_regex(S, S, 42) end)
    ),
    ?assertEqual(
        type_error('String', 'replaceAllRegex:with:', ReplHint),
        raised(fun() -> beamtalk_regex:replace_all_regex(42, S, S) end)
    ),
    ?assertEqual(
        type_error(
            'String',
            'matchesRegex:options:',
            <<"Receiver must be a String, pattern a String, options a List">>
        ),
        raised(fun() -> beamtalk_regex:matches_regex_options(S, S, not_a_list) end)
    ).

regex_bare_binaries_still_accepted_test() ->
    ?assert(beamtalk_regex:matches_regex(<<"abc">>, <<"b">>)),
    {ok, Re} = beamtalk_result:to_tagged_tuple(beamtalk_regex:from(<<"b+">>)),
    ?assertEqual(
        {beamtalk_string, <<"b">>}, beamtalk_regex:first_match({beamtalk_string, <<"abc">>}, Re)
    ),
    ?assertEqual(
        {beamtalk_string, <<"aXc">>},
        beamtalk_regex:replace_regex(<<"abc">>, Re, <<"X">>)
    ).

%%% ============================================================================
%%% Binary
%%% ============================================================================

binary_receiver_test() ->
    R = <<"Receiver must be a Binary">>,
    ?assertEqual(
        type_error('Binary', at, R), raised(fun() -> beamtalk_binary:at(not_bin, 1) end)
    ),
    ?assertEqual(
        type_error('Binary', byte_at, R), raised(fun() -> beamtalk_binary:byte_at(1, 1) end)
    ),
    ?assertEqual(
        type_error('Binary', part, R), raised(fun() -> beamtalk_binary:part(1, 0, 1) end)
    ),
    ?assertEqual(
        type_error('Binary', concat, R), raised(fun() -> beamtalk_binary:concat(1, <<>>) end)
    ),
    ?assertEqual(
        type_error('Binary', bitSize, R), raised(fun() -> beamtalk_binary:bitSize(1) end)
    ),
    ?assertEqual(
        type_error('Binary', to_bytes, R), raised(fun() -> beamtalk_binary:to_bytes(1) end)
    ),
    ?assertEqual(
        type_error('Binary', 'asBase64:', R), raised(fun() -> beamtalk_binary:asBase64(1) end)
    ),
    ?assertEqual(
        type_error('Binary', 'asBase64Url:', R),
        raised(fun() -> beamtalk_binary:asBase64Url(1) end)
    ),
    ?assertEqual(
        type_error('Binary', 'asHex:', R), raised(fun() -> beamtalk_binary:asHex(1) end)
    ),
    ?assertEqual(
        type_error('Binary', print_string, R),
        raised(fun() -> beamtalk_binary:print_string(1) end)
    ),
    ?assertEqual(
        type_error('Binary', deserialize_with_used, R),
        raised(fun() -> beamtalk_binary:deserialize_with_used(1) end)
    ),
    ?assertEqual(
        type_error('Binary', do, R), raised(fun() -> beamtalk_binary:do(1, fun(_) -> ok end) end)
    ).

binary_argument_test() ->
    ?assertEqual(
        type_error('Binary', at, <<"Index must be an Integer">>),
        raised(fun() -> beamtalk_binary:at(<<"a">>, x) end)
    ),
    ?assertEqual(
        type_error('Binary', byte_at, <<"Index must be an Integer">>),
        raised(fun() -> beamtalk_binary:byte_at(<<"a">>, x) end)
    ),
    ?assertEqual(
        type_error('Binary', part, <<"Position and length must be Integers">>),
        raised(fun() -> beamtalk_binary:part(<<"a">>, x, 1) end)
    ),
    ?assertEqual(
        type_error('Binary', do, <<"Argument must be a 1-argument block">>),
        raised(fun() -> beamtalk_binary:do(<<"a">>, not_a_fun) end)
    ),
    ?assertEqual(
        type_error('Binary', 'deserialize:', <<"Argument must be a Binary">>),
        raised(fun() -> beamtalk_binary:deserialize(1) end)
    ),
    ?assertEqual(
        type_error('Binary', 'includesSubstring:', <<"Argument must be a Binary">>),
        raised(fun() -> beamtalk_binary:includes_substring(<<"a">>, 1) end)
    ),
    ?assertEqual(
        type_error('Binary', 'indexOf:', <<"Argument must be a Binary">>),
        raised(fun() -> beamtalk_binary:index_of(<<"a">>, 1) end)
    ),
    ?assertEqual(
        type_error(
            'Binary',
            concat,
            <<"Argument must be a Binary; join a Bitstring from the Bitstring side (Bitstring>>concat:)">>
        ),
        raised(fun() -> beamtalk_binary:concat(<<"a">>, <<1:3>>) end)
    ).

binary_tagged_string_receiver_test() ->
    S = {beamtalk_string, <<"ab">>},
    ?assertEqual(97, beamtalk_binary:at(S, 1)),
    ?assertEqual(<<"abab">>, beamtalk_binary:concat(S, S)),
    ?assertEqual(16, beamtalk_binary:bitSize(S)),
    ?assertEqual(2, beamtalk_binary:index_of(S, <<"b">>)).

%%% ============================================================================
%%% File (multi-argument heads)
%%% ============================================================================

file_multi_argument_test() ->
    PathHint = <<"Path must be a String">>,
    S = {beamtalk_string, <<"p">>},
    ?assertEqual(
        type_error('File', 'writeAll:contents:', PathHint),
        raised(fun() -> beamtalk_file:writeAll(42, S) end)
    ),
    ?assertEqual(
        type_error('File', 'writeAll:contents:', <<"Contents must be a String">>),
        raised(fun() -> beamtalk_file:writeAll(S, 42) end)
    ),
    ?assertEqual(
        type_error('File', 'writeBinary:contents:', PathHint),
        raised(fun() -> beamtalk_file:writeBinary(42, <<>>) end)
    ),
    ?assertEqual(
        type_error('File', 'appendBinary:contents:', PathHint),
        raised(fun() -> beamtalk_file:appendBinary(42, <<>>) end)
    ),
    ?assertEqual(
        type_error('File', 'rename:to:', PathHint),
        raised(fun() -> beamtalk_file:rename(42, S) end)
    ),
    ?assertEqual(
        type_error('File', 'rename:to:', <<"Destination must be a String">>),
        raised(fun() -> beamtalk_file:rename(S, 42) end)
    ),
    ?assertEqual(
        type_error('File', 'open:do:', PathHint),
        raised(fun() -> beamtalk_file:'open:do:'(42, fun(_) -> ok end) end)
    ),
    ?assertEqual(
        type_error('File', 'open:do:', <<"Expected a Block with 1 argument">>),
        raised(fun() -> beamtalk_file:'open:do:'(S, not_a_block) end)
    ),
    ?assertEqual(
        type_error('File', 'open:mode:', PathHint),
        raised(fun() -> beamtalk_file:'open:mode:'(42, read) end)
    ),
    ?assertEqual(
        type_error('File', 'open:mode:', <<"Mode must be a Symbol">>),
        raised(fun() -> beamtalk_file:'open:mode:'(S, <<"read">>) end)
    ),
    ?assertEqual(
        type_error('File', 'open:mode:do:', PathHint),
        raised(fun() -> beamtalk_file:open(42, read, fun(_) -> ok end) end)
    ),
    ?assertEqual(
        type_error('File', 'open:mode:do:', <<"Mode must be a Symbol">>),
        raised(fun() -> beamtalk_file:open(S, <<"read">>, fun(_) -> ok end) end)
    ),
    ?assertEqual(
        type_error('File', 'open:mode:do:', <<"Expected a Block with 1 argument">>),
        raised(fun() -> beamtalk_file:open(S, read, not_a_block) end)
    ).

%%% ============================================================================
%%% DateTime, System, OS, Json, Uuid
%%% ============================================================================

datetime_test() ->
    ?assertEqual(
        type_error('DateTime', 'fromString:', <<"Argument must be a String">>),
        raised(fun() -> beamtalk_datetime:fromString(1) end)
    ),
    ?assertEqual(
        type_error('DateTime', 'parse:format:', <<"Both arguments must be Strings">>),
        raised(fun() -> beamtalk_datetime:parse(<<"a">>, 1) end)
    ),
    ?assertEqual(
        type_error('DateTime', 'parse:format:', <<"Both arguments must be Strings">>),
        raised(fun() -> beamtalk_datetime:parse(1, <<"a">>) end)
    ),
    Dt = beamtalk_datetime:fromString(<<"2026-07-25T14:30:00Z">>),
    ?assertEqual(
        type_error('DateTime', 'format:', <<"Argument must be a String pattern">>),
        raised(fun() -> beamtalk_datetime:format(Dt, 1) end)
    ).

system_test() ->
    ?assertEqual(
        type_error('System', 'getEnv:', <<"Argument must be a String">>),
        raised(fun() -> beamtalk_system:getEnv(1) end)
    ),
    ?assertEqual(
        type_error('System', 'getEnv:default:', <<"Name argument must be a String">>),
        raised(fun() -> beamtalk_system:getEnv(1, <<"d">>) end)
    ),
    ?assertEqual(
        type_error('System', 'getEnv:default:', <<"Default argument must be a String">>),
        raised(fun() -> beamtalk_system:getEnv(<<"N">>, 1) end)
    ),
    ?assertEqual(
        type_error('System', 'setEnv:value:', <<"Name argument must be a String">>),
        raised(fun() -> beamtalk_system:setEnv(1, <<"v">>) end)
    ),
    ?assertEqual(
        type_error('System', 'setEnv:value:', <<"Value argument must be a String">>),
        raised(fun() -> beamtalk_system:setEnv(<<"N">>, 1) end)
    ),
    ?assertEqual(
        type_error('System', 'unsetEnv:', <<"Argument must be a String">>),
        raised(fun() -> beamtalk_system:unsetEnv(1) end)
    ).

os_test() ->
    ?assertEqual(
        type_error('OS', 'run:', <<"Argument must be a String">>),
        raised(fun() -> beamtalk_os:run(1) end)
    ),
    ?assertEqual(
        type_error('OS', 'run:timeout:', <<"Argument must be a String">>),
        raised(fun() -> beamtalk_os:run(1, 10) end)
    ),
    ?assertEqual(
        type_error('OS', 'run:timeout:', <<"Timeout must be a positive Integer">>),
        raised(fun() -> beamtalk_os:run(<<"true">>, 0) end)
    ).

json_test() ->
    ?assertEqual(
        type_error('Json', 'parse:', <<"Argument must be a String">>),
        raised(fun() -> beamtalk_json:parse(1) end)
    ).

uuid_test() ->
    ?assertEqual(
        type_error('Uuid', 'fromString:', <<"Argument must be a String">>),
        raised(fun() -> beamtalk_uuid:fromString(1) end)
    ),
    ?assertEqual(
        type_error('Uuid', 'isValid:', <<"Argument must be a String">>),
        raised(fun() -> beamtalk_uuid:isValid(1) end)
    ).

%%% ============================================================================
%%% Beamtalk (interface) — message, no hint
%%% ============================================================================

interface_test() ->
    ?assertEqual(
        type_error(
            'Beamtalk',
            'allSendsIn:',
            <<"allSendsIn: expects a binary source">>
        ),
        raised(fun() -> beamtalk_interface:allSendsIn(1) end)
    ),
    ?assertEqual(
        type_error(
            'Beamtalk',
            'announceSitesIn:',
            <<"announceSitesIn: expects a binary source">>
        ),
        raised(fun() -> beamtalk_interface:announceSitesIn(1) end)
    ),
    ?assertEqual(
        type_error(
            'Beamtalk',
            'findSendersIn:selector:',
            <<
                "findSendersIn:selector: expects a binary source and an atom or "
                "binary selector"
            >>
        ),
        raised(fun() -> beamtalk_interface:findSendersIn(<<"x">>, 1) end)
    ),
    ?assertEqual(
        type_error(
            'Beamtalk',
            'findReferencesToIn:class:',
            <<
                "findReferencesToIn:class: expects a binary source and an atom or "
                "binary class name"
            >>
        ),
        raised(fun() -> beamtalk_interface:findReferencesToIn(1, 'Foo') end)
    ),
    ?assertEqual(
        type_error(
            'Beamtalk',
            'ffiSitesIn:module:function:arity:',
            <<
                "ffiSitesIn:module:function:arity: expects a binary source, atom or "
                "binary module and function, and a non-negative integer arity or the "
                "atom any"
            >>
        ),
        raised(fun() -> beamtalk_interface:ffiSitesIn(<<"x">>, lists, reverse, -1) end)
    ),
    ?assertEqual(
        type_error(
            'Beamtalk', 'erlangHelp:', <<"erlangHelp: expects a binary module name">>
        ),
        raised(fun() -> beamtalk_interface:erlangHelp(1) end)
    ),
    ?assertEqual(
        type_error(
            'Beamtalk',
            'erlangHelp:selector:',
            <<"erlangHelp:selector: expects a binary module name and atom/binary function name">>
        ),
        raised(fun() -> beamtalk_interface:erlangHelp(<<"lists">>, 1) end)
    ).
