# Workspace Auto-Cleanup

## Overview

Beamtalk workspaces automatically clean up when abandoned to prevent accumulation of orphaned BEAM nodes. This is implemented via a self-monitoring system where each workspace node tracks its own activity and terminates after a configurable idle period.

## How It Works

### Architecture

Each workspace node includes `beamtalk_idle_monitor`, a gen_server that:

1. **Tracks activity** via `beamtalk_workspace_meta:update_activity()`
2. **Checks idle status** every 10 minutes
3. **Self-terminates** via `init:stop()` if idle > max_idle_seconds

### Activity Tracking

Activity is automatically tracked when:

- **REPL session connects** (`beamtalk_repl_server.erl` in session startup)
- **Actor is spawned** (`beamtalk_actor.erl` in register_spawned)
- **Code is hot-reloaded** (`beamtalk_repl_eval.erl` in do_load after module load)

Active sessions (even if idle in REPL) prevent auto-cleanup.

### Orphan Cleanup

When `beamtalk repl` starts, it checks for orphaned workspaces:

1. Read `~/.beamtalk/workspaces/{id}/node.info`
2. Check if node is actually running (`is_node_running()`)
3. If not running, clean up stale `node.info` file
4. Log cleanup action: `"Cleaning up orphaned workspace: {id}"`

## Configuration

### Default Behavior

```bash
beamtalk repl
# Auto-cleanup enabled, 4-hour idle timeout
```

### Disable Auto-Cleanup (Production)

```bash
beamtalk repl --persistent
# Workspace persists even when idle
```

### Custom Idle Timeout

```bash
# CLI flag (seconds)
beamtalk repl --timeout 7200  # 2 hours

# Environment variable (seconds)
export BEAMTALK_WORKSPACE_TIMEOUT=7200
beamtalk repl
```

**Priority:** `--timeout` flag > `BEAMTALK_WORKSPACE_TIMEOUT` env var > default (14400 = 4 hours)

## Testing

### Unit Tests

```bash
cd runtime
rebar3 eunit --module=beamtalk_idle_monitor_tests
rebar3 eunit --module=beamtalk_activity_tracking_tests
```

### Manual Testing

#### Test Activity Tracking

```bash
# Start workspace
beamtalk repl --timeout 60  # 1 minute idle timeout

# Activity 1: Spawn actor
> Actor spawn

# Activity 2: Load code  
> :load examples/counter.bt

# Disconnect the REPL session (Ctrl-D), then wait
# The idle monitor checks every 10 minutes, so the node
# will self-terminate within ~10 minutes after disconnecting
# (once idle time > 60s AND no active sessions)
```

#### Test Orphan Cleanup

```bash
# Terminal 1: Start workspace
beamtalk repl

# Terminal 2: Kill the BEAM node (simulate crash)
ps aux | grep beamtalk_workspace
kill -9 <PID>

# Terminal 1: Try to reconnect
beamtalk repl
# → Should detect orphaned workspace and clean up node.info
# → Should start new workspace node
```

#### Test Persistent Mode

```bash
beamtalk repl --persistent --timeout 10

# Wait 15 seconds with no activity
# → Node should stay alive (auto-cleanup disabled)
```

## Troubleshooting

### Workspace Not Auto-Cleaning

**Check idle monitor status:**

```erlang
> whereis(beamtalk_idle_monitor).
<0.123.0>  % Process is running

> {ok, Meta} = beamtalk_workspace_meta:get_metadata().
> maps:get(last_activity, Meta).
1738960000  % Timestamp in seconds
```

**Check if cleanup is enabled:**

```erlang
> sys:get_state(beamtalk_idle_monitor).
{state, true, 14400, #Ref<...>}
%        ^^^^ enabled flag
```

### Workspace Cleaned Up Too Early

**Possible causes:**

1. No active REPL sessions connected
2. Activity tracking not called (check call sites)
3. Idle timeout too short (check `--timeout` or env var)

**Debug activity tracking:**

```erlang
% Add to beamtalk_workspace_meta.erl temporarily:
handle_cast(update_activity, State) ->
    Now = erlang:system_time(second),
    io:format("Activity updated: ~p~n", [Now]),  % Debug output
    ...
```

### Orphaned Workspaces Not Cleaned Up

**Manual cleanup:**

```bash
# List orphaned workspaces
ls ~/.beamtalk/workspaces/

# For each workspace:
cd ~/.beamtalk/workspaces/<workspace_id>
cat node.info  # Get PID

# If PID is dead:
rm node.info
```

## Implementation Details

### File Changes

**Erlang runtime:**
- `beamtalk_repl_server.erl` - Session connect tracking
- `beamtalk_actor.erl` - Actor spawn tracking
- `beamtalk_repl_eval.erl` - Code reload tracking
- `beamtalk_workspace_sup.erl` - Config plumbing
- `beamtalk_idle_monitor.erl` - Self-monitoring (already existed)
- `beamtalk_workspace_meta.erl` - Activity storage (already existed)

**Rust CLI:**
- `main.rs` - CLI flags (`--persistent`, `--timeout`)
- `commands/repl/mod.rs` - Pass flags to workspace startup
- `commands/workspace/mod.rs` - Handle config, orphan cleanup

### Design Decisions

1. **Self-monitoring (no daemon):** Each node monitors itself, avoiding the need for an external cleanup daemon.
2. **Activity = any operation:** Session connect, actor spawn, or code reload all count as activity.
3. **Active session = no cleanup:** Even an idle REPL session prevents cleanup (preserves user's mental model).
4. **Graceful shutdown:** Uses `init:stop()` to let supervisor tree clean up actors before termination.

## References

- **ADR 0004:** Persistent Workspace Management
- **Issue:** BT-265 - Implement workspace auto-cleanup (idle timeout)
- **Tests:** `beamtalk_idle_monitor_tests.erl`, `beamtalk_activity_tracking_tests.erl`
