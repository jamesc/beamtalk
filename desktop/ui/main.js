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
const quitButton = document.getElementById("quit-button");

// workspaceId -> stage string ("spawning" | "probing"), while an attach is
// in flight.
const attachProgress = new Map();
// workspaceId -> a short human-readable disconnected/unreachable label, for
// attached workspaces the post-attach monitor has flagged as unhealthy.
const connectionBadges = new Map();

async function refresh() {
  try {
    const view = await invoke("list_workspaces");
    render(view);
  } catch (err) {
    statusEl.hidden = false;
    statusEl.textContent = `Failed to list workspaces: ${err}`;
  }
}

function render(view) {
  const hasWorkspaces = view.workspaces.length > 0;
  statusEl.hidden = true;
  emptyStateEl.hidden = hasWorkspaces;
  workspaceListEl.hidden = !hasWorkspaces;

  if (!hasWorkspaces) {
    renderEmptyState(view.empty_state);
    return;
  }

  workspaceListEl.innerHTML = "";
  for (const workspace of view.workspaces) {
    workspaceListEl.appendChild(renderWorkspaceRow(workspace));
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

function renderWorkspaceRow(workspace) {
  const li = document.createElement("li");
  li.className = "workspace-row";
  li.dataset.workspaceId = workspace.id;

  const info = document.createElement("div");
  info.className = "workspace-info";

  const name = document.createElement("span");
  name.className = "workspace-name";
  name.textContent = workspace.id;
  info.appendChild(name);

  const liveBadge = document.createElement("span");
  liveBadge.className = workspace.alive
    ? "badge badge-alive"
    : "badge badge-dead";
  liveBadge.textContent = workspace.alive ? "live" : "not running";
  info.appendChild(liveBadge);

  const connectionLabel = connectionBadges.get(workspace.id);
  if (connectionLabel) {
    const connBadge = document.createElement("span");
    connBadge.className = "badge badge-warning";
    connBadge.textContent = connectionLabel;
    info.appendChild(connBadge);
  }

  if (workspace.project_path) {
    const path = document.createElement("span");
    path.className = "workspace-path";
    path.textContent = workspace.project_path;
    info.appendChild(path);
  }

  li.appendChild(info);
  li.appendChild(renderActionButton(workspace));
  return li;
}

function renderActionButton(workspace) {
  const button = document.createElement("button");
  if (workspace.attached) {
    button.textContent = "Detach";
    button.onclick = () => detach(workspace.id, button);
    return button;
  }

  const progress = attachProgress.get(workspace.id);
  button.textContent = progress ? `${progress}…` : "Attach";
  button.disabled = Boolean(progress);
  button.onclick = () => attach(workspace.id, button);
  return button;
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
    await refresh();
  }
}

async function detach(workspaceId, button) {
  button.disabled = true;
  try {
    await invoke("detach", { workspaceId });
  } finally {
    connectionBadges.delete(workspaceId);
    await refresh();
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
    await refresh();
  }
});

quitButton.addEventListener("click", () => {
  invoke("quit");
});

listen("attach-progress", (event) => {
  attachProgress.set(event.payload.workspace_id, event.payload.stage);
  refresh();
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
  refresh();
});

refresh();
setInterval(refresh, 3000);
