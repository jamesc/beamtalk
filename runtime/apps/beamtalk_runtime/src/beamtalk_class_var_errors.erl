%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

%%% **DDD Context:** Object System Context

-module(beamtalk_class_var_errors).
-moduledoc """
The structured errors class-variable accesses raise (ADR 0124 §1, ADR 0130 §5).

A leaf below both `beamtalk_class_vars` (the in-process accesses) and
`beamtalk_object_class` (the class gen_server's `get_class_var`), so each error
kind's message and hint have one source and neither module calls the other to
borrow it.
""".

-include("beamtalk.hrl").

-export([
    uninitialized_hint/2,
    uninitialized_error/2,
    raise_uninitialized/2,
    raise_nil_receiver/0,
    raise_unreachable/3,
    raise_no_snapshot/2,
    raise_read_only/2,
    raise_undeclared/2
]).

-doc "Hint of `uninitialized_state_error` for an unassigned `late` class variable.".
-spec uninitialized_hint(atom(), atom()) -> binary().
uninitialized_hint(Class, Name) ->
    iolist_to_binary(
        io_lib:format(
            "~s class variable '~s' is declared `late` and has not been assigned yet",
            [Class, Name]
        )
    ).

-doc """
Build (but do not raise) the `uninitialized_state_error` the class gen_server's
`get_class_var` replies with for an unassigned declared-`late` class variable
(selector `fieldAt:`). It only builds the value: that `handle_call` clause has
no `try` wrapper, so a raise there would crash the class gen_server instead of
answering the caller with `{error, Error}`. No declared-type metadata exists for
class variables (`__beamtalk_meta/0` carries no `class_field_types`), so the
hint names the variable without a `(:: Type)` suffix.
""".
-spec uninitialized_error(atom(), atom()) -> #beamtalk_error{}.
uninitialized_error(Class, Name) ->
    Error0 = beamtalk_error:new(uninitialized_state_error, Class, 'fieldAt:'),
    beamtalk_error:with_hint(Error0, uninitialized_hint(Class, Name)).

-doc """
Raise `uninitialized_state_error` for an in-process `late` read: same kind and
hint as `uninitialized_error/2`, but no selector (no `fieldAt:` was sent) and
the variable named in `details`.
""".
-spec raise_uninitialized(atom(), atom()) -> no_return().
raise_uninitialized(Class, Name) ->
    Error0 = beamtalk_error:new(uninitialized_state_error, Class),
    Error1 = beamtalk_error:with_details(Error0, #{class_variable => Name}),
    beamtalk_error:raise(beamtalk_error:with_hint(Error1, uninitialized_hint(Class, Name))).

-doc "Internal error: a class-variable access with a `nil` or non-class receiver.".
-spec raise_nil_receiver() -> no_return().
raise_nil_receiver() ->
    Error0 = beamtalk_error:new(internal_error, 'UndefinedObject'),
    Error = beamtalk_error:with_message(
        Error0, <<"class-variable access with a nil or non-class receiver">>
    ),
    beamtalk_error:raise(Error).

-doc """
ADR 0130 §5: `class_state_unreachable`. With `Name = undefined` it is the
class-level variant (no live class process is registered); otherwise the
variable cannot be read or written from this process.
""".
-spec raise_unreachable(atom(), atom() | undefined, read | write) -> no_return().
raise_unreachable(Class, undefined, _Mode) ->
    %% Name-less variant (`capture/2`, mirror reads without a variable name): a
    %% class-level condition, so the block-oriented hint of the named variant
    %% does not fit supervisor-init or `performLocally:` callers.
    Error0 = beamtalk_error:new(class_state_unreachable, Class),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary(
            io_lib:format("~s's class state cannot be reached: no live ~s class process", [
                Class, Class
            ])
        )
    ),
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            Error1,
            <<
                "No class process is registered for this class right now (it is not loaded, "
                "was removed, or is being restarted). Load the class or retry once it is running."
            >>
        )
    );
raise_unreachable(Class, Name, Mode) ->
    Verb =
        case Mode of
            read -> "read";
            write -> "written"
        end,
    Message = iolist_to_binary(
        io_lib:format("~s's class variable ~s cannot be ~s from this process", [Class, Name, Verb])
    ),
    Hint = iolist_to_binary(
        io_lib:format(
            "A block that writes ~s's class variables ran outside any ~s class "
            "method (it was passed to another class's class method or an actor, or "
            "stored or returned and run later outside ~s's own methods). A block "
            "can read ~s's class variables anywhere, as the values they had when the "
            "block was made, but can only write them from ~s's own method: return the "
            "value and assign it there.",
            [Class, Class, Class, Class, Class]
        )
    ),
    Error0 = beamtalk_error:new(class_state_unreachable, Class),
    Error1 = beamtalk_error:with_message(Error0, Message),
    Error2 = beamtalk_error:with_details(Error1, #{class_variable => Name}),
    beamtalk_error:raise(beamtalk_error:with_hint(Error2, Hint)).

-doc "`class_state_unreachable`: the class process is registered but has recorded no snapshot row yet.".
-spec raise_no_snapshot(atom(), atom() | undefined) -> no_return().
raise_no_snapshot(Class, Name) ->
    Error0 = beamtalk_error:new(class_state_unreachable, Class),
    Error1 = beamtalk_error:with_message(
        Error0,
        iolist_to_binary(
            io_lib:format("~s's class state cannot be reached: no snapshot recorded yet", [Class])
        )
    ),
    Error2 =
        case Name of
            undefined -> Error1;
            _ -> beamtalk_error:with_details(Error1, #{class_variable => Name})
        end,
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            Error2,
            <<
                "The class process was just (re)started and has not recorded its class "
                "variables yet; retry shortly."
            >>
        )
    ).

-doc "`class_state_read_only`: a write under `with_snapshot/2`'s read-only marker.".
-spec raise_read_only(atom(), atom()) -> no_return().
raise_read_only(Class, Name) ->
    Message = iolist_to_binary(
        io_lib:format("~s's class variable ~s is read-only here", [Class, Name])
    ),
    Hint = iolist_to_binary(
        io_lib:format(
            "This code runs against a read-only snapshot of ~s's class variables "
            "(supervisor definition, a class `initialize:` hook, or `performLocally:`). "
            "Assign the variable from one of ~s's own class methods, sent as a normal "
            "class-side message.",
            [Class, Class]
        )
    ),
    Error0 = beamtalk_error:new(class_state_read_only, Class),
    Error1 = beamtalk_error:with_message(Error0, Message),
    Error2 = beamtalk_error:with_details(Error1, #{class_variable => Name}),
    beamtalk_error:raise(beamtalk_error:with_hint(Error2, Hint)).

-doc "`undeclared_class_variable`: a read of a name the class does not declare.".
-spec raise_undeclared(atom(), atom()) -> no_return().
raise_undeclared(Class, Name) ->
    Message = iolist_to_binary(
        io_lib:format("~s has no class variable named ~s", [Class, Name])
    ),
    Error0 = beamtalk_error:new(undeclared_class_variable, Class),
    Error1 = beamtalk_error:with_message(Error0, Message),
    Error2 = beamtalk_error:with_details(Error1, #{class_variable => Name}),
    beamtalk_error:raise(
        beamtalk_error:with_hint(
            Error2, <<"Declare it with `classState:` on the class, or check the spelling.">>
        )
    ).
