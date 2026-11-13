%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Runtime Context (test support)
%%
%% Shared structured-error assertions (BT-4677). Include with
%%   -include_lib("beamtalk_test_support/include/beamtalk_test_assert.hrl").
%% (the includer also needs `beamtalk_runtime/include/beamtalk.hrl` for the
%% `#beamtalk_error{}` record and `eunit/include/eunit.hrl` for `?assertMatch`).

-ifndef(BEAMTALK_TEST_ASSERT_HRL).
-define(BEAMTALK_TEST_ASSERT_HRL, true).

%% Expr raises `#{error := #beamtalk_error{kind, class, selector}}`.
-define(assertBtError(Kind, Class, Selector, Expr),
    ?assertError(
        #{error := #beamtalk_error{kind = Kind, class = Class, selector = Selector}},
        Expr
    )
).

%% Shorter forms leave the unconstrained fields as wildcards.
-define(assertBtError(Kind, Class, Expr),
    ?assertBtError(Kind, Class, _, Expr)
).
-define(assertBtError(Kind, Expr),
    ?assertBtError(Kind, _, _, Expr)
).

%% As `?assertBtError/4`, and the raised map's `'$beamtalk_class'` (the exception
%% class tag, e.g. 'RuntimeError') must equal ExcClass. Use `_` for a wildcard field.
-define(assertBtErrorTagged(ExcClass, Kind, Class, Selector, Expr),
    ?assertError(
        #{
            '$beamtalk_class' := ExcClass,
            error := #beamtalk_error{kind = Kind, class = Class, selector = Selector}
        },
        Expr
    )
).

%% As `?assertBtError/4`, and the error's `details` must also match Details.
-define(assertBtErrorDetails(Kind, Class, Selector, Details, Expr),
    ?assertError(
        #{
            error := #beamtalk_error{
                kind = Kind, class = Class, selector = Selector, details = Details
            }
        },
        Expr
    )
).

%% Expr returns a Beamtalk `Result` object that is an error
%% (`isOk := false`) whose `errReason` is `#{error := #beamtalk_error{kind, class, selector}}`.
-define(assertBtResultError(Kind, Class, Selector, Expr),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                error := #beamtalk_error{kind = Kind, class = Class, selector = Selector}
            }
        },
        Expr
    )
).
-define(assertBtResultError(Kind, Class, Expr),
    ?assertBtResultError(Kind, Class, _, Expr)
).
-define(assertBtResultError(Kind, Expr),
    ?assertBtResultError(Kind, _, _, Expr)
).

%% Expr returns `{error, #beamtalk_error{kind, class, selector}}` (Result-wrapped).
-define(assertBtErrorResult(Kind, Class, Selector, Expr),
    ?assertMatch(
        {error, #beamtalk_error{kind = Kind, class = Class, selector = Selector}},
        Expr
    )
).

-endif.
