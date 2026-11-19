%% Copyright 2026 James Casey
%% SPDX-License-Identifier: Apache-2.0

-module(beamtalk_actor_ctx).

%%% **DDD Context:** Actor System Context

-moduledoc """
Application-level trace context and causal trace IDs, split out of `beamtalk_actor` (BT-5047).

The set/get/clear API for the `'$beamtalk_trace_ctx'` process-dictionary
entry and the causal-ID reader used by `beamtalk_trace_store`. Building the
*propagated* context on every send (`get_propagated_ctx/0`) lives in
`beamtalk_actor_send`, where it is inlined into the send bodies; restoring it
(`restore_propagated_ctx/1`) stays in `beamtalk_actor`, because generated code
calls it on the hot path. `clear_stale_otel_ctx/0` reads the OTel context via
`beamtalk_actor_send:get_otel_ctx/0`.
""".

-include("beamtalk.hrl").
-export([
    clear_stale_otel_ctx/0,
    clear_trace_context/0,
    get_causal_ctx/0,
    get_trace_context/0,
    set_trace_context/1
]).

-doc """
Set application-level trace context key-value pairs.

Stores the provided map in the process dictionary under '$beamtalk_trace_ctx'.
Also merges the keys into OTP logger process metadata so that ?LOG_ERROR,
?LOG_INFO etc. automatically include fields like workflowId without extra work.

Example: set_trace_context(#{workflowId => <<"wf-123">>, activityId => <<"a-1">>})

Cost: ~20-30ns (one put/2 + one logger:update_process_metadata/1).
""".
-spec set_trace_context(map()) -> ok.
set_trace_context(Ctx) when is_map(Ctx) ->
    OldCtx = get_trace_context(),
    Merged = maps:merge(OldCtx, Ctx),
    put('$beamtalk_trace_ctx', Merged),
    logger:update_process_metadata(Merged),
    ok.

-doc """
Get the current application-level trace context.

Returns the map stored by set_trace_context/1, or #{} if none set.
Cost: ~10ns (one get/1).
""".
-spec get_trace_context() -> map().
get_trace_context() ->
    case get('$beamtalk_trace_ctx') of
        undefined -> #{};
        Ctx when is_map(Ctx) -> Ctx;
        _Other -> #{}
    end.

-doc """
Clear the application-level trace context and remove its keys from logger metadata.

Used when restoring an empty propagated trace context to prevent stale metadata
from leaking across unrelated messages in the same actor process.
""".
-spec clear_trace_context() -> ok.
clear_trace_context() ->
    OldCtx = get_trace_context(),
    erase('$beamtalk_trace_ctx'),
    %% Remove the trace context keys from logger metadata
    case logger:get_process_metadata() of
        undefined ->
            ok;
        LogMeta ->
            Cleaned = maps:without(maps:keys(OldCtx), LogMeta),
            logger:set_process_metadata(Cleaned)
    end,
    ok.

-doc """
Get the current causal trace context from the process dictionary.

Returns a map with trace_id, span_id, and parent_span_id keys when
causal tracing is active. Returns #{} when no causal context exists.
Called by beamtalk_trace_store:record_dispatch_stop/2 to merge causal IDs into trace event metadata.
""".
-spec get_causal_ctx() -> map().
get_causal_ctx() ->
    case get('$beamtalk_trace_id') of
        undefined ->
            #{};
        TraceId ->
            Base = #{trace_id => TraceId},
            WithSpan =
                case get('$beamtalk_span_id') of
                    undefined -> Base;
                    SpanId -> Base#{span_id => SpanId}
                end,
            case get('$beamtalk_parent_span_id') of
                undefined -> WithSpan;
                ParentSpanId -> WithSpan#{parent_span_id => ParentSpanId}
            end
    end.

-doc """
Detach a non-empty OTel context left by a previous message when the
incoming one carries none. Older senders always sent `otel => #{}` (which
attached the empty context); BT-4113 senders omit an empty context, so the
receiver clears instead. A no-op when OpenTelemetry is not loaded.
""".
-spec clear_stale_otel_ctx() -> ok.
clear_stale_otel_ctx() ->
    case beamtalk_actor_send:get_otel_ctx() of
        undefined ->
            ok;
        Ctx when Ctx =:= #{} ->
            ok;
        _Stale ->
            erlang:apply(otel_ctx, attach, [#{}]),
            ok
    end.
