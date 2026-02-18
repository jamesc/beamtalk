// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

// Beamtalk Workspace — WebSocket client, pane management, JSON protocol

(function() {
  'use strict';

  // DOM elements
  var statusEl = document.getElementById('status');
  var replOutput = document.getElementById('repl-output');
  var transcriptEl = document.getElementById('transcript');
  var transcriptEmpty = document.getElementById('transcript-empty');
  var inspectorEl = document.getElementById('inspector');
  var inspectorEmpty = document.getElementById('inspector-empty');
  var evalInput = document.getElementById('eval-input');
  var sendBtn = document.getElementById('send-btn');
  var inspectBtn = document.getElementById('inspect-btn');
  var connectBtn = document.getElementById('connect-btn');
  var cookieInput = document.getElementById('cookie-input');
  var authPanel = document.getElementById('auth-panel');
  var editorInput = document.getElementById('editor-input');
  var loadBtn = document.getElementById('load-btn');
  var editorStatus = document.getElementById('editor-status');
  var shortcutsOverlay = document.getElementById('shortcuts-overlay');
  var printBtn = document.getElementById('print-btn');

  var ws = null;
  var msgId = 0;
  // Track pending inspect requests to route responses to Inspector
  var pendingInspects = {};
  // Track pending load-source requests to route responses to Editor
  var pendingLoads = {};
  // Track pending completion requests
  var pendingComplete = {};
  // Track pending print-it requests to route result back inline
  var pendingPrintIt = {};

  // Command history
  var history = [];
  var historyIndex = -1;
  var historySaved = '';
  var MAX_HISTORY = 200;

  // Session reconnection
  var sessionId = null;

  function setStatus(text, color) {
    statusEl.textContent = text;
    statusEl.style.color = color || '#a6adc8';
  }

  function appendTo(el, text, cls) {
    var span = document.createElement('span');
    span.textContent = text;
    if (cls) span.style.color = cls;
    el.appendChild(span);
    el.scrollTop = el.scrollHeight;
  }

  function clearEl(el) {
    el.innerHTML = '';
  }

  // --- Inspector rendering ---

  function renderInspect(data) {
    clearEl(inspectorEl);
    if (inspectorEmpty) inspectorEmpty.remove();

    if (data.class) {
      var header = document.createElement('div');
      header.style.color = '#f9e2af';
      header.style.marginBottom = '8px';
      header.textContent = data.class + (data.pid ? ' (' + data.pid + ')' : '');
      inspectorEl.appendChild(header);
    }

    var fields = data.state || data.fields || data;
    if (typeof fields === 'object' && fields !== null) {
      var keys = Object.keys(fields);
      for (var i = 0; i < keys.length; i++) {
        var row = document.createElement('div');
        row.className = 'inspector-field';
        var nameSpan = document.createElement('span');
        nameSpan.className = 'inspector-field-name';
        nameSpan.textContent = keys[i] + ':';
        var valSpan = document.createElement('span');
        valSpan.className = 'inspector-field-value';
        valSpan.textContent = String(fields[keys[i]]);
        row.appendChild(nameSpan);
        row.appendChild(valSpan);
        inspectorEl.appendChild(row);
      }
    } else {
      appendTo(inspectorEl, String(fields), '#cdd6f4');
    }
  }

  // --- Session ID from URL ---

  function getSessionFromUrl() {
    var hash = location.hash.replace(/^#/, '');
    var params = new URLSearchParams(hash);
    return params.get('session') || null;
  }

  function setSessionInUrl(id) {
    location.hash = 'session=' + encodeURIComponent(id);
  }

  // --- WebSocket connection ---

  window.connect = function() {
    var cookie = cookieInput.value.trim();
    if (!cookie) { setStatus('Enter cookie first', '#f38ba8'); return; }

    connectBtn.disabled = true;
    setStatus('Connecting…', '#f9e2af');

    var proto = location.protocol === 'https:' ? 'wss:' : 'ws:';
    ws = new WebSocket(proto + '//' + location.host + '/ws');

    ws.onopen = function() {
      setStatus('Authenticating…', '#f9e2af');
      var authMsg = { type: 'auth', cookie: cookie };
      // Include session ID for resume if available
      var resumeId = getSessionFromUrl();
      if (resumeId) {
        authMsg.resume = resumeId;
      }
      ws.send(JSON.stringify(authMsg));
    };

    ws.onmessage = function(ev) {
      var msg;
      try { msg = JSON.parse(ev.data); } catch(e) { return; }

      // Auth flow
      if (msg.op === 'auth-required') {
        setStatus('Auth required', '#f9e2af');
        return;
      }
      if (msg.type === 'auth_ok') {
        setStatus('Authenticated', '#a6e3a1');
        // Save cookie to sessionStorage for reconnection (cleared on tab close)
        try { sessionStorage.setItem('beamtalk_cookie', cookieInput.value.trim()); } catch(e) {}
        return;
      }
      if (msg.type === 'auth_error') {
        setStatus('Auth failed: ' + msg.message, '#f38ba8');
        ws.close();
        return;
      }
      if (msg.op === 'session-started') {
        sessionId = msg.session;
        setSessionInUrl(sessionId);
        var display = sessionId ? sessionId.slice(0, 8) + '…' : 'unknown';
        setStatus('Connected (session ' + display + ')', '#a6e3a1');
        authPanel.style.display = 'none';
        evalInput.disabled = false;
        sendBtn.disabled = false;
        printBtn.disabled = false;
        inspectBtn.disabled = false;
        loadBtn.disabled = false;
        evalInput.focus();
        return;
      }

      // Push messages → Transcript pane
      if (msg.push === 'transcript') {
        if (transcriptEmpty) transcriptEmpty.remove();
        appendTo(transcriptEl, msg.text, '#cdd6f4');
        return;
      }

      // BT-696: Streaming stdout chunk during eval
      if (msg.out && !msg.status) {
        appendTo(replOutput, msg.out, '#cdd6f4');
        return;
      }

      // Route completion responses
      if (msg.id && pendingComplete[msg.id]) {
        var expectedPrefix = pendingComplete[msg.id];
        delete pendingComplete[msg.id];
        // Guard against stale completions: verify prefix still matches cursor text
        if (typeof expectedPrefix === 'string') {
          var curPos = evalInput.selectionStart;
          var curText = evalInput.value.substring(0, curPos);
          var curMatch = curText.match(/[A-Za-z_][A-Za-z0-9_:]*$/);
          var curPrefix = curMatch ? curMatch[0] : '';
          if (curPrefix !== expectedPrefix) return;
        }
        showCompletions(msg.completions || []);
        return;
      }

      // Route print-it responses — append result inline
      if (msg.id && pendingPrintIt[msg.id]) {
        delete pendingPrintIt[msg.id];
        var isErr = Array.isArray(msg.status) && msg.status.includes('error');
        if (isErr && msg.error) {
          appendTo(replOutput, 'Error: ' + msg.error + '\n', '#f38ba8');
        } else {
          var val = msg.value != null ? String(msg.value) : '';
          if (val) {
            appendTo(replOutput, val + '\n', '#a6e3a1');
          }
        }
        return;
      }

      // Route inspect responses to Inspector pane
      if (msg.id && pendingInspects[msg.id]) {
        delete pendingInspects[msg.id];
        var isError = Array.isArray(msg.status) && msg.status.includes('error');
        if (isError && msg.error) {
          clearEl(inspectorEl);
          appendTo(inspectorEl, 'Error: ' + msg.error, '#f38ba8');
        } else if (msg.state) {
          renderInspect(msg);
        } else if (msg.value != null) {
          clearEl(inspectorEl);
          appendTo(inspectorEl, String(msg.value), '#cdd6f4');
        }
        return;
      }

      // Route load-source responses to Editor status
      if (msg.id && pendingLoads[msg.id]) {
        delete pendingLoads[msg.id];
        if (msg.classes) {
          editorStatus.textContent = 'Loaded: ' + msg.classes.join(', ');
          editorStatus.style.color = '#a6e3a1';
        } else if (msg.error) {
          editorStatus.textContent = msg.error;
          editorStatus.style.color = '#f38ba8';
        }
        return;
      }

      // Eval response → Workspace pane
      if (msg.id && msg.status) {
        var isEvalError = Array.isArray(msg.status) && msg.status.includes('error');
        if (isEvalError && msg.error) {
          appendTo(replOutput, 'Error: ' + msg.error + '\n', '#f38ba8');
        } else {
          var value = msg.value != null ? String(msg.value) : '';
          if (value) {
            appendTo(replOutput, '=> ' + value + '\n', '#a6e3a1');
          }
        }
      }
      // Legacy error format
      if (msg.type === 'error' && msg.message) {
        appendTo(replOutput, 'Error: ' + msg.message + '\n', '#f38ba8');
      }
    };

    ws.onclose = function() {
      setStatus('Disconnected — refresh to reconnect', '#f38ba8');
      evalInput.disabled = true;
      sendBtn.disabled = true;
      printBtn.disabled = true;
      inspectBtn.disabled = true;
      loadBtn.disabled = true;
      connectBtn.disabled = false;
      authPanel.style.display = 'flex';
    };

    ws.onerror = function() {
      setStatus('Connection error', '#f38ba8');
    };
  };

  // --- Eval / Do It (Workspace pane) ---

  function getSelectedOrLine() {
    var start = evalInput.selectionStart;
    var end = evalInput.selectionEnd;
    if (start !== end) {
      return evalInput.value.substring(start, end);
    }
    // No selection — use current line
    var text = evalInput.value;
    var lineStart = text.lastIndexOf('\n', start - 1) + 1;
    var lineEnd = text.indexOf('\n', start);
    if (lineEnd === -1) lineEnd = text.length;
    return text.substring(lineStart, lineEnd);
  }

  function pushHistory(code) {
    if (code && (history.length === 0 || history[history.length - 1] !== code)) {
      history.push(code);
      if (history.length > MAX_HISTORY) {
        history.shift();
      }
    }
    historyIndex = -1;
    historySaved = '';
  }

  window.sendEval = function() {
    var code = getSelectedOrLine().trim();
    if (!code || !ws || ws.readyState !== WebSocket.OPEN) return;

    pushHistory(code);
    appendTo(replOutput, code + '\n', '#89b4fa');
    msgId++;
    ws.send(JSON.stringify({ op: 'eval', id: 'msg-' + msgId, code: code }));
    // Clear input only if no selection (evaluated entire content)
    if (evalInput.selectionStart === evalInput.selectionEnd) {
      evalInput.value = '';
    }
    evalInput.focus();
  };

  // --- Print It — eval selected text/line, append result inline ---

  window.sendPrintIt = function() {
    var code = getSelectedOrLine().trim();
    if (!code || !ws || ws.readyState !== WebSocket.OPEN) return;

    pushHistory(code);
    appendTo(replOutput, code + '\n', '#89b4fa');
    msgId++;
    var id = 'msg-' + msgId;
    pendingPrintIt[id] = true;
    ws.send(JSON.stringify({ op: 'eval', id: id, code: code }));
  };

  // --- Inspect (sends inspect op, routes result to Inspector) ---

  window.sendInspect = function() {
    var code = getSelectedOrLine().trim();
    if (!code || !ws || ws.readyState !== WebSocket.OPEN) return;

    pushHistory(code);
    appendTo(replOutput, code + '\n', '#89b4fa');
    msgId++;
    var id = 'msg-' + msgId;
    pendingInspects[id] = true;
    ws.send(JSON.stringify({ op: 'eval', id: id, code: code }));
    evalInput.focus();
  };

  // --- Load Source (Editor pane) ---

  window.loadSource = function() {
    var source = editorInput.value;
    if (!source.trim() || !ws || ws.readyState !== WebSocket.OPEN) return;

    editorStatus.textContent = 'Compiling…';
    editorStatus.style.color = '#f9e2af';
    msgId++;
    var id = 'msg-' + msgId;
    pendingLoads[id] = true;
    ws.send(JSON.stringify({ op: 'load-source', id: id, source: source }));
  };

  // --- Tab Completion ---

  var completionTimer = null;

  function requestCompletion() {
    if (!ws || ws.readyState !== WebSocket.OPEN) return;
    // Extract word before cursor
    var pos = evalInput.selectionStart;
    var text = evalInput.value.substring(0, pos);
    var match = text.match(/[A-Za-z_][A-Za-z0-9_:]*$/);
    if (!match) return;
    // Debounce: cancel any pending completion request
    if (completionTimer) clearTimeout(completionTimer);
    completionTimer = setTimeout(function() {
      completionTimer = null;
      msgId++;
      var id = 'msg-' + msgId;
      pendingComplete[id] = match[0];
      ws.send(JSON.stringify({ op: 'complete', id: id, code: match[0] }));
    }, 150);
  }

  var completionDropdown = null;
  var completionItems = [];
  var completionSelectedIndex = 0;
  var completionPrefix = '';

  function showCompletions(items) {
    hideCompletions();
    if (!items || items.length === 0) return;

    // Get prefix for replacement
    var pos = evalInput.selectionStart;
    var text = evalInput.value.substring(0, pos);
    var match = text.match(/[A-Za-z_][A-Za-z0-9_:]*$/);
    completionPrefix = match ? match[0] : '';

    if (items.length === 1) {
      // Single match — insert directly
      insertCompletion(items[0]);
      return;
    }

    completionItems = items;
    completionSelectedIndex = 0;

    completionDropdown = document.createElement('div');
    completionDropdown.id = 'completion-dropdown';

    for (var i = 0; i < items.length; i++) {
      var item = document.createElement('div');
      item.className = 'completion-item' + (i === 0 ? ' selected' : '');
      item.textContent = items[i];
      item.setAttribute('data-index', String(i));
      item.addEventListener('click', function() {
        insertCompletion(this.textContent);
      });
      completionDropdown.appendChild(item);
    }

    var evalPanel = document.getElementById('eval-panel');
    evalPanel.style.position = 'relative';
    evalPanel.appendChild(completionDropdown);
  }

  function hideCompletions() {
    if (completionDropdown && completionDropdown.parentNode) {
      completionDropdown.parentNode.removeChild(completionDropdown);
    }
    completionDropdown = null;
    completionItems = [];
    completionSelectedIndex = 0;
  }

  function insertCompletion(text) {
    var pos = evalInput.selectionStart;
    var before = evalInput.value.substring(0, pos);
    var after = evalInput.value.substring(pos);
    // Replace the prefix with the completion
    var prefixLen = completionPrefix.length;
    evalInput.value = before.substring(0, before.length - prefixLen) + text + after;
    evalInput.selectionStart = evalInput.selectionEnd = pos - prefixLen + text.length;
    hideCompletions();
    evalInput.focus();
  }

  function navigateCompletion(delta) {
    if (!completionDropdown || completionItems.length === 0) return false;
    var items = completionDropdown.querySelectorAll('.completion-item');
    items[completionSelectedIndex].classList.remove('selected');
    completionSelectedIndex = (completionSelectedIndex + delta + completionItems.length) % completionItems.length;
    items[completionSelectedIndex].classList.add('selected');
    items[completionSelectedIndex].scrollIntoView({ block: 'nearest' });
    return true;
  }

  // --- Command History ---

  function historyUp() {
    if (history.length === 0) return;
    if (historyIndex === -1) {
      historySaved = evalInput.value;
      historyIndex = history.length - 1;
    } else if (historyIndex > 0) {
      historyIndex--;
    }
    evalInput.value = history[historyIndex];
    evalInput.selectionStart = evalInput.selectionEnd = evalInput.value.length;
  }

  function historyDown() {
    if (historyIndex === -1) return;
    if (historyIndex < history.length - 1) {
      historyIndex++;
      evalInput.value = history[historyIndex];
    } else {
      historyIndex = -1;
      evalInput.value = historySaved;
    }
    evalInput.selectionStart = evalInput.selectionEnd = evalInput.value.length;
  }

  // --- Shortcuts Help Overlay ---

  window.toggleShortcuts = function() {
    if (shortcutsOverlay) {
      var visible = shortcutsOverlay.style.display !== 'none';
      shortcutsOverlay.style.display = visible ? 'none' : 'flex';
    }
  };

  // --- Keyboard shortcuts ---

  evalInput.addEventListener('keydown', function(e) {
    var mod = e.ctrlKey || e.metaKey;

    // Completion dropdown navigation
    if (completionDropdown) {
      if (e.key === 'ArrowDown') { e.preventDefault(); navigateCompletion(1); return; }
      if (e.key === 'ArrowUp') { e.preventDefault(); navigateCompletion(-1); return; }
      if (e.key === 'Enter') { e.preventDefault(); insertCompletion(completionItems[completionSelectedIndex]); return; }
      if (e.key === 'Escape' || e.key === ' ' || e.key === '(' || e.key === ')' || e.key === '.') {
        hideCompletions();
        // Don't return — let the key be processed normally
      }
      // Dismiss on printable character input to avoid stale prefix insertion
      if (e.key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
        hideCompletions();
      }
    }

    // Ctrl+D / Cmd+D — Do It
    if (mod && e.key === 'd') {
      e.preventDefault();
      sendEval();
      return;
    }
    // Ctrl+P / Cmd+P — Print It
    if (mod && e.key === 'p') {
      e.preventDefault();
      sendPrintIt();
      return;
    }
    // Ctrl+I / Cmd+I — Inspect It
    if (mod && e.key === 'i') {
      e.preventDefault();
      sendInspect();
      return;
    }
    // Ctrl+Enter — Do It (existing)
    if (mod && e.key === 'Enter') {
      e.preventDefault();
      sendEval();
      return;
    }
    // Tab — request completion
    if (e.key === 'Tab' && !e.shiftKey) {
      e.preventDefault();
      requestCompletion();
      return;
    }
    // Command history — Up/Down (only when no selection and cursor at start/end)
    if (e.key === 'ArrowUp' &&
        evalInput.selectionStart === evalInput.selectionEnd &&
        evalInput.selectionStart === 0) {
      e.preventDefault();
      historyUp();
      return;
    }
    if (e.key === 'ArrowDown' &&
        evalInput.selectionStart === evalInput.selectionEnd &&
        evalInput.selectionEnd === evalInput.value.length) {
      e.preventDefault();
      historyDown();
      return;
    }
  });

  editorInput.addEventListener('keydown', function(e) {
    // Ctrl+Shift+Enter to load source from editor
    if ((e.ctrlKey || e.metaKey) && e.shiftKey && e.key === 'Enter') {
      e.preventDefault();
      loadSource();
    }
    // Tab inserts spaces in editor
    if (e.key === 'Tab') {
      e.preventDefault();
      var start = editorInput.selectionStart;
      var end = editorInput.selectionEnd;
      editorInput.value = editorInput.value.substring(0, start) + '  ' + editorInput.value.substring(end);
      editorInput.selectionStart = editorInput.selectionEnd = start + 2;
    }
  });

  // Global shortcut: Ctrl+/ toggles shortcuts help from anywhere
  document.addEventListener('keydown', function(e) {
    if ((e.ctrlKey || e.metaKey) && e.key === '/') {
      e.preventDefault();
      toggleShortcuts();
    }
  });

  // Close shortcuts overlay on click
  if (shortcutsOverlay) {
    shortcutsOverlay.addEventListener('click', function(e) {
      if (e.target === shortcutsOverlay) {
        shortcutsOverlay.style.display = 'none';
      }
    });
  }

  // Auto-connect if session ID present in URL
  var resumeSession = getSessionFromUrl();
  if (resumeSession) {
    // Pre-fill cookie from sessionStorage if available
    var savedCookie = sessionStorage.getItem('beamtalk_cookie');
    if (savedCookie) {
      cookieInput.value = savedCookie;
      connect();
    }
  }
})();
