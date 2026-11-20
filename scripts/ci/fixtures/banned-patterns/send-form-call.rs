// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0
// Fixture for the send-form-call lint (see self_test): each spelling below must be
// flagged, and `send_number_coercion` / comment lines must not be.
let d = form::call(
    "beamtalk_message_dispatch",
    "send",
    args,
);
let e = call(
    "beamtalk_message_dispatch",
    "send",
    args,
);
self.sequenced_runtime_call(
    "beamtalk_message_dispatch",
    "send",
    &[receiver, &arguments[0]],
    "Perf",
    |operands| operands,
);
self.class_side_error_guard(("beamtalk_message_dispatch", "send"), recv, msg);
let f = form::call("beamtalk_message_dispatch", "send", args);
let ok = form::call("beamtalk_message_dispatch", "send_number_coercion", args);
// form::call("beamtalk_message_dispatch", "send", args)
