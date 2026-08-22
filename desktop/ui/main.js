// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// Picker frontend (ADR 0097, BT-2986) — vanilla JS, no build step, in
// keeping with the ADR's "thin shell" framing (the shell does very little:
// spawn -> probe -> window). Talks to the Rust side exclusively through
// `window.__TAURI__.core.invoke` (enabled via `withGlobalTauri` in
// tauri.conf.json) and listens for two events the backend emits:
// `attach-progress` (loading/probing state while attaching) and
// `connection-state-changed` (post-attach disconnected-state reflection,
// ADR 0097 Broker §3 / spike criterion (f)).

const { invoke } = window.__TAURI__.core;
const { listen } = window.__TAURI__.event;

const statusEl = document.getElementById("status");
const emptyStateEl = document.getElementById("empty-state");
const emptyStateMessageEl = document.getElementById("empty-state-message");
const cliFoundEl = document.getElementById("empty-state-cli-found");
const cliMissingEl = document.getElementById("empty-state-cli-missing");
const newWorkspaceNameEl = document.getElementById("new-workspace-name");
const createWorkspaceButton = document.getElementById(
  "create-workspace-button",
);
const workspaceListEl = document.getElementById("workspace-list");
const workspaceFiltersEl = document.getElementById("workspace-filters");
const workspaceFilterEmptyEl = document.getElementById(
  "workspace-filter-empty",
);
const filterButtons = [...document.querySelectorAll(".filter-button")];
const quitButton = document.getElementById("quit-button");

// workspaceId -> stage string ("spawning" | "probing"), while an attach is
// in flight.
const attachProgress = new Map();
// workspaceId -> a short human-readable disconnected/unreachable label, for
// attached workspaces the post-attach monitor has flagged as unhealthy.
const connectionBadges = new Map();

// "all" | "running" | "stopped" (BT-3230) — persists across refreshes
// (a periodic `runRefresh()` re-applies it to the freshly fetched list
// rather than resetting to "all") until the user picks a different one.
let currentFilter = "all";
// The last full (unfiltered) workspace list `render()` saw — filter button
// clicks re-render from this directly rather than re-invoking
// `list_workspaces`, so switching filters is instant and never races a
// concurrent backend refresh.
let lastWorkspaces = [];

// The last non-empty path segment, e.g. "/Users/james/source/beamtalk" ->
// "beamtalk". Splits on both "/" and "\\" since `project_path` comes from
// the picker's own host OS and this app targets Windows too (BT-2988);
// `null` for a missing path or one with no segments (e.g. "/" or "\\"),
// letting the caller fall back to something else rather than showing
// nothing.
function leafDirName(projectPath) {
  if (!projectPath) {
    return null;
  }
  const segments = projectPath.split(/[/\\]/).filter(Boolean);
  return segments.length > 0 ? segments[segments.length - 1] : null;
}

function matchesFilter(workspace) {
  if (currentFilter === "running") {
    return workspace.alive;
  }
  if (currentFilter === "stopped") {
    return !workspace.alive;
  }
  return true;
}

function setFilter(filter) {
  currentFilter = filter;
  for (const button of filterButtons) {
    button.classList.toggle("active", button.dataset.filter === filter);
  }
  renderWorkspaceListForCurrentFilter();
}

for (const button of filterButtons) {
  button.addEventListener("click", () => setFilter(button.dataset.filter));
}
setFilter(currentFilter);

// Coalescing window for `scheduleRefresh` (below) — chosen to smooth out a
// flapping connection or several concurrent attaches firing `attach-progress`/
// `connection-state-changed` several times a second, while still feeling
// immediate for a single user action.
const REFRESH_DEBOUNCE_MS = 150;
let refreshDebounceTimer = null;
// True while a `list_workspaces` invoke is in flight, so a refresh request
// that lands mid-invoke queues one more run afterwards instead of firing a
// second overlapping invoke.
let refreshInFlight = false;
// Set by any refresh request (direct call or event) that arrives while one
// is already in flight — consumed by that in-flight refresh's `finally` to
// run exactly one more time, so the latest state always wins without
// unbounded queuing.
let refreshPending = false;

// Coalesce refresh requests within `REFRESH_DEBOUNCE_MS` of each other into
// a single `runRefresh()` call. Bursty callers (the `attach-progress`/
// `connection-state-changed` event listeners, the periodic poll) go through
// this; a direct response to a single user action (attach/detach/create
// completing) calls `runRefresh()` itself instead, for immediate feedback —
// see `reconcileWorkspaceList`'s doc comment for why an uncoalesced flood of
// overlapping invokes was a problem worth fixing either way.
function scheduleRefresh() {
  if (refreshDebounceTimer !== null) {
    return;
  }
  refreshDebounceTimer = setTimeout(() => {
    refreshDebounceTimer = null;
    runRefresh();
  }, REFRESH_DEBOUNCE_MS);
}

async function runRefresh() {
  if (refreshInFlight) {
    refreshPending = true;
    return;
  }
  refreshInFlight = true;
  try {
    const view = await invoke("list_workspaces");
    render(view);
  } catch (err) {
    statusEl.hidden = false;
    statusEl.textContent = `Failed to list workspaces: ${err}`;
  } finally {
    refreshInFlight = false;
    if (refreshPending) {
      refreshPending = false;
      scheduleRefresh();
    }
  }
}

function render(view) {
  lastWorkspaces = view.workspaces;
  const hasWorkspaces = view.workspaces.length > 0;
  statusEl.hidden = true;
  emptyStateEl.hidden = hasWorkspaces;
  // Filter buttons are meaningless with nothing to filter — hide them along
  // with the (unrelated) "create a workspace" empty state.
  workspaceFiltersEl.hidden = !hasWorkspaces;

  if (!hasWorkspaces) {
    renderEmptyState(view.empty_state);
    // Nothing to reconcile against once the list is empty — clear any rows
    // left over from a prior non-empty render so the next transition back
    // to non-empty starts from a clean slate. Also clears a stale
    // "no workspaces match this filter" message left over from before the
    // last workspace disappeared entirely.
    workspaceListEl.hidden = true;
    workspaceListEl.replaceChildren();
    workspaceFilterEmptyEl.hidden = true;
    return;
  }

  renderWorkspaceListForCurrentFilter();
}

// Re-derive the visible row set from `lastWorkspaces` + `currentFilter` and
// reconcile the list against it. Called both by `render()` (a fresh
// backend fetch landed) and `setFilter()` (the user picked a different
// filter over already-fetched data) — kept as one function so those two
// triggers can't drift into different filtering logic.
function renderWorkspaceListForCurrentFilter() {
  if (lastWorkspaces.length === 0) {
    return;
  }
  const visible = lastWorkspaces.filter(matchesFilter);
  workspaceFilterEmptyEl.hidden = visible.length > 0;
  workspaceListEl.hidden = visible.length === 0;
  reconcileWorkspaceList(visible);
}

// Update `workspaceListEl` to match `workspaces` by reusing existing row
// elements (keyed by `li.dataset.workspaceId`) wherever possible, instead
// of `innerHTML = ""` + a full rebuild every refresh. Two problems that
// full teardown caused, both worth avoiding given how often a refresh can
// now fire (a flapping connection or several concurrent attaches, each
// driving its own `attach-progress`/`connection-state-changed` events):
//
// - A click can land on a button mid-replacement: `innerHTML = ""` detaches
//   every existing button from the document an instant before its
//   replacement is appended, so a click that lands in that window is lost
//   (dispatched to a node no longer in the DOM) rather than reaching the
//   freshly created button's handler.
// - Rebuilding every row's DOM nodes on every refresh is wasted work when,
//   as is by far the common case, only a badge or a button's text/disabled
//   state actually changed for one row out of many.
function reconcileWorkspaceList(workspaces) {
  const existingRows = new Map();
  for (const li of workspaceListEl.children) {
    existingRows.set(li.dataset.workspaceId, li);
  }

  let previousRow = null;
  for (const workspace of workspaces) {
    let li = existingRows.get(workspace.id);
    if (li) {
      updateWorkspaceRow(li, workspace);
      existingRows.delete(workspace.id);
    } else {
      li = renderWorkspaceRow(workspace);
    }

    const referenceNode = previousRow
      ? previousRow.nextSibling
      : workspaceListEl.firstChild;
    // `insertBefore` with a node already positioned right before
    // `referenceNode` is a documented no-op, so an already-correctly-placed
    // row's DOM position (and thus its subtree, including the button) is
    // left completely untouched.
    if (referenceNode !== li) {
      workspaceListEl.insertBefore(li, referenceNode);
    }
    previousRow = li;
  }

  // Anything left in `existingRows` is a row for a workspace no longer in
  // `workspaces` (e.g. it stopped being discoverable) — drop it.
  for (const staleRow of existingRows.values()) {
    staleRow.remove();
  }
}

function renderEmptyState(emptyState) {
  cliFoundEl.hidden = true;
  cliMissingEl.hidden = true;
  emptyStateMessageEl.textContent = "No workspaces yet.";

  if (emptyState.kind === "cli_found") {
    cliFoundEl.hidden = false;
  } else if (emptyState.kind === "cli_missing") {
    cliMissingEl.hidden = false;
  }
}

// Build a brand-new row's DOM structure (name, live badge, connection
// badge, path, action button — each a stable element `updateWorkspaceRow`
// can find again on a later refresh instead of recreating), then populate
// it via the same `updateWorkspaceRow` a reused row goes through, so the
// initial-render and update paths can't drift apart.
function renderWorkspaceRow(workspace) {
  const li = document.createElement("li");
  li.className = "workspace-row";
  li.dataset.workspaceId = workspace.id;

  const info = document.createElement("div");
  info.className = "workspace-info";

  const name = document.createElement("span");
  name.className = "workspace-name";
  info.appendChild(name);

  const id = document.createElement("span");
  id.className = "workspace-id";
  info.appendChild(id);

  const liveBadge = document.createElement("span");
  liveBadge.className = "workspace-live-badge";
  info.appendChild(liveBadge);

  const connBadge = document.createElement("span");
  connBadge.className = "badge badge-warning workspace-connection-badge";
  info.appendChild(connBadge);

  const path = document.createElement("span");
  path.className = "workspace-path";
  info.appendChild(path);

  li.appendChild(info);

  const button = document.createElement("button");
  button.className = "workspace-action-button";
  li.appendChild(button);

  updateWorkspaceRow(li, workspace);
  return li;
}

// Refresh an existing row's badges/text/button in place — no child
// elements are created, removed, or reordered here, so a row already
// showing (e.g.) an open dropdown or mid-click button is left otherwise
// undisturbed by an unrelated refresh.
function updateWorkspaceRow(li, workspace) {
  // BT-3230: the leaf directory name (e.g. ".../source/beamtalk" ->
  // "beamtalk") is the one thing a human actually recognizes at a glance —
  // promote it to the row's primary label, in place of the opaque id.
  // Falls back to the id when there's no project_path to take a leaf from
  // (a hand-edited/corrupted metadata.json — rare).
  const leaf = leafDirName(workspace.project_path);
  const name = li.querySelector(".workspace-name");
  name.textContent = leaf ?? workspace.id;

  const id = li.querySelector(".workspace-id");
  // Only shown alongside a promoted leaf name — when there's no path, the
  // primary label above already *is* the id, so repeating it here would be
  // redundant clutter instead of the disambiguation aid it's meant to be
  // (two different projects can share a leaf directory name).
  id.hidden = !leaf;
  id.textContent = leaf ? workspace.id : "";

  const liveBadge = li.querySelector(".workspace-live-badge");
  liveBadge.className = workspace.alive
    ? "badge badge-alive workspace-live-badge"
    : "badge badge-dead workspace-live-badge";
  liveBadge.textContent = workspace.alive ? "live" : "not running";

  const connBadge = li.querySelector(".workspace-connection-badge");
  const connectionLabel = connectionBadges.get(workspace.id);
  connBadge.hidden = !connectionLabel;
  connBadge.textContent = connectionLabel ?? "";

  const path = li.querySelector(".workspace-path");
  path.hidden = !workspace.project_path;
  path.textContent = workspace.project_path ?? "";

  updateActionButton(li.querySelector(".workspace-action-button"), workspace);
}

function updateActionButton(button, workspace) {
  if (workspace.attached) {
    button.textContent = "Detach";
    button.disabled = false;
    button.onclick = () => detach(workspace.id, button);
    return;
  }

  const progress = attachProgress.get(workspace.id);
  button.textContent = progress ? `${progress}…` : "Attach";
  button.disabled = Boolean(progress);
  button.onclick = () => attach(workspace.id, button);
}

async function attach(workspaceId, button) {
  button.disabled = true;
  button.textContent = "Attaching…";
  try {
    await invoke("attach", { workspaceId });
  } catch (err) {
    statusEl.hidden = false;
    statusEl.textContent = `Could not attach to '${workspaceId}': ${err}`;
  } finally {
    attachProgress.delete(workspaceId);
    // A direct response to this user action, not the flapping-event case
    // `scheduleRefresh`'s debounce exists for — refresh right away (still
    // single-flight-guarded by `runRefresh` against any concurrently
    // in-flight event-triggered refresh).
    await runRefresh();
  }
}

async function detach(workspaceId, button) {
  button.disabled = true;
  try {
    await invoke("detach", { workspaceId });
  } finally {
    connectionBadges.delete(workspaceId);
    await runRefresh();
  }
}

createWorkspaceButton.addEventListener("click", async () => {
  const workspaceId = newWorkspaceNameEl.value.trim();
  if (!workspaceId) {
    return;
  }
  createWorkspaceButton.disabled = true;
  try {
    await invoke("create_workspace", { workspaceId });
    newWorkspaceNameEl.value = "";
  } catch (err) {
    statusEl.hidden = false;
    statusEl.textContent = `Could not create workspace '${workspaceId}': ${err}`;
  } finally {
    createWorkspaceButton.disabled = false;
    await runRefresh();
  }
});

quitButton.addEventListener("click", () => {
  invoke("quit");
});

// These two events are the flapping/bursty case `scheduleRefresh`'s debounce
// exists for: a shaky connection or several concurrent attaches can each
// fire several times a second, and without coalescing, each one used to
// trigger its own overlapping `list_workspaces` invoke.
listen("attach-progress", (event) => {
  attachProgress.set(event.payload.workspace_id, event.payload.stage);
  scheduleRefresh();
});

listen("connection-state-changed", (event) => {
  const { workspace_id: workspaceId, state } = event.payload;
  if (state.kind === "connected") {
    connectionBadges.delete(workspaceId);
  } else if (state.kind === "disconnected") {
    connectionBadges.set(workspaceId, `disconnected (${state.reason})`);
  } else {
    connectionBadges.set(workspaceId, "unreachable");
  }
  scheduleRefresh();
});

runRefresh();
setInterval(scheduleRefresh, 3000);
