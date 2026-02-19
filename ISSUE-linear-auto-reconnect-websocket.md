Title: Auto-reconnect and session-resume for WebSocket clients

Team: BT
Assignee: jamesc.000@gmail.com
Labels: agent-ready, Improvement, repl, M

Context:
The REPL websocket connection can close after idle and current fixes added reconnect/resume support to the synchronous CLI client only. The server already supports session resume via the "resume" field in the auth handshake, but async clients (crates/beamtalk-mcp and others) and other websocket users still need to adopt connect_with_resume and reconnect+retry semantics to provide a consistent UX.

Acceptance Criteria:
- [ ] Implement connect_with_resume and reconnect+retry-once semantics in crates/beamtalk-mcp/src/client.rs (tokio-tungstenite async client).
- [ ] Update other websocket clients (transcript viewer, interrupt socket, any direct tokio-tungstenite usages) to use the shared resume/reconnect helper.
- [ ] Ensure the CLI REPL uses resume when reconnecting (preserve current session id) and retries the last request once after resume.
- [ ] Add integration/e2e test(s) that simulate a dropped websocket and verify session resume restores bindings and workspace state.
- [ ] Add logging for reconnect attempts and failures.
- [ ] Document the new behavior in docs/ or AGENTS.md and add guidance on duplicate-eval risk and mitigation options.

Files to Modify:
- crates/beamtalk-mcp/src/client.rs
- crates/beamtalk-cli/src/commands/protocol.rs
- crates/beamtalk-cli/src/commands/repl/client.rs
- crates/beamtalk-cli/src/commands/repl/mod.rs
- runtime/apps/beamtalk_workspace/src/beamtalk_ws_handler.erl
- tests/e2e/ (new test fixtures to simulate disconnects)

Dependencies: None

References:
- Existing CLI reconnect work in crates/beamtalk-cli (connect_with_resume and repl reconnect flow)
- runtime/apps/beamtalk_workspace/src/beamtalk_ws_handler.erl (server-side resume support)

Notes:
- Behavior must be conservative: retry once to avoid infinite loops. Consider follow-up to implement backoff or server-side dedupe for idempotent eval.
