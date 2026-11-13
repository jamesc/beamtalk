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

%% Expr raises a wrapped Exception
%% `#{'$beamtalk_class' := _, error := #beamtalk_error{kind, class, selector}}`
%% (a bare `#{error := ...}` map does not match).
-define(assertBtError(Kind, Class, Selector, Expr),
    ?assertError(
        #{
            '$beamtalk_class' := _,
            error := #beamtalk_error{kind = Kind, class = Class, selector = Selector}
        },
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
            '$beamtalk_class' := _,
            error := #beamtalk_error{
                kind = Kind, class = Class, selector = Selector, details = Details
            }
        },
        Expr
    )
).

%% As `?assertBtError/4`, and the error's `hint` must also match Hint.
-define(assertBtErrorHint(Kind, Class, Selector, Hint, Expr),
    ?assertError(
        #{
            error := #beamtalk_error{
                kind = Kind, class = Class, selector = Selector, hint = Hint
            }
        },
        Expr
    )
).

%% Expr returns a Beamtalk `Result` object that is an error
%% (`isOk := false`) whose `errReason` is a wrapped Exception
%% `#{'$beamtalk_class' := _, error := #beamtalk_error{kind, class, selector}}`.
-define(assertBtResultObjError(Kind, Class, Selector, Expr),
    ?assertMatch(
        #{
            '$beamtalk_class' := 'Result',
            'isOk' := false,
            'errReason' := #{
                '$beamtalk_class' := _,
                error := #beamtalk_error{kind = Kind, class = Class, selector = Selector}
            }
        },
        Expr
    )
).
-define(assertBtResultObjError(Kind, Class, Expr),
    ?assertBtResultObjError(Kind, Class, _, Expr)
).
-define(assertBtResultObjError(Kind, Expr),
    ?assertBtResultObjError(Kind, _, _, Expr)
).

%% Expr evaluates to an already-raised/wrapped error map `#{error := #beamtalk_error{..}}`
%% (e.g. a caught exception reason or `ensure_wrapped/3` result).
-define(assertBtErrorValue(Kind, Class, Selector, Expr),
    ?assertMatch(
        #{error := #beamtalk_error{kind = Kind, class = Class, selector = Selector}},
        Expr
    )
).

%% Expr returns `{error, #{error := #beamtalk_error{..}}}` (error tuple around a wrapped map).
-define(assertBtErrorTuple(Kind, Class, Selector, Expr),
    ?assertMatch(
        {error, #{error := #beamtalk_error{kind = Kind, class = Class, selector = Selector}}},
        Expr
    )
).

%% Expr returns `{error, #beamtalk_error{kind, class, selector}}` (Result-wrapped).
-define(assertBtErrorResult(Kind, Class, Selector, Expr),
    ?assertMatch(
        {error, #beamtalk_error{kind = Kind, class = Class, selector = Selector}},
        Expr
    )
).

-endif.
