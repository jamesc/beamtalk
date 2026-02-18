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
  var clearBtn = document.getElementById('clear-btn');
  var stopBtn = document.getElementById('stop-btn');
  var testBtn = document.getElementById('test-btn');
  var codegenBtn = document.getElementById('codegen-btn');
  var stdinOverlay = document.getElementById('stdin-overlay');
  var stdinInput = document.getElementById('stdin-input');
  var stdinPrompt = document.getElementById('stdin-prompt');

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
  // Track pending test requests
  var pendingTest = {};
  // Track pending show-codegen requests
  var pendingCodegen = {};
  // Track whether an eval is in progress (for Stop button visibility)
  var evalInProgress = 0;

  // Command history
  var history = [];
  var historyIndex = -1;
  var historySaved = '';
  var MAX_HISTORY = 200;

  // Session reconnection
  var sessionId = null;

  // BT-690: Live actor tracking
  var liveActors = {};

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

  function updateStopButton() {
    stopBtn.style.display = evalInProgress > 0 ? '' : 'none';
  }

  // --- Inspector rendering ---

  // BT-690: Actor lifecycle push handling
  function handleActorPush(event, data) {
    if (!data) return;
    var pid = data.pid;
    if (event === 'spawned') {
      liveActors[pid] = data;
    } else if (event === 'stopped') {
      delete liveActors[pid];
    }
    renderActorList();
  }

  function renderActorList() {
    var pids = Object.keys(liveActors);
    if (pids.length === 0) {
      var existing = inspectorEl.querySelector('.inspector-actors');
      if (existing) existing.remove();
      return;
    }

    if (inspectorEmpty) inspectorEmpty.remove();

    // Find or create actors section
    var section = inspectorEl.querySelector('.inspector-actors');
    if (!section) {
      section = document.createElement('div');
      section.className = 'inspector-actors';
      inspectorEl.insertBefore(section, inspectorEl.firstChild);
    }
    section.innerHTML = '';

    var header = document.createElement('div');
    header.style.color = '#f9e2af';
    header.style.marginBottom = '4px';
    header.textContent = 'Live Actors (' + pids.length + ')';
    section.appendChild(header);

    for (var i = 0; i < pids.length; i++) {
      var actor = liveActors[pids[i]];
      var row = document.createElement('div');
      row.className = 'inspector-field';
      var nameSpan = document.createElement('span');
      nameSpan.className = 'inspector-field-name';
      nameSpan.textContent = actor['class'] || 'Unknown';
      var valSpan = document.createElement('span');
      valSpan.className = 'inspector-field-value';
      valSpan.textContent = pids[i];
      row.appendChild(nameSpan);
      row.appendChild(valSpan);
      section.appendChild(row);
    }
  }

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
        clearBtn.disabled = false;
        testBtn.disabled = false;
        codegenBtn.disabled = false;
        evalInput.focus();
        return;
      }

      // Push messages → Transcript pane
      if (msg.push === 'transcript') {
        if (transcriptEmpty) transcriptEmpty.remove();
        appendTo(transcriptEl, msg.text, '#cdd6f4');
        return;
      }

      // BT-690: Actor lifecycle push messages → Inspector pane
      if (msg.type === 'push' && msg.channel === 'actors') {
        handleActorPush(msg.event, msg.data);
        return;
      }

      // BT-722: stdin_request — server needs user input
      if (msg.status && Array.isArray(msg.status) && msg.status.includes('need-input')) {
        var prompt = msg.prompt || 'Input required:';
        stdinPrompt.textContent = prompt;
        stdinInput.value = '';
        stdinOverlay.style.display = 'flex';
        stdinInput.focus();
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
        if (evalInProgress > 0) { evalInProgress--; updateStopButton(); }
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
        if (evalInProgress > 0) { evalInProgress--; updateStopButton(); }
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

      // BT-722: Route test responses to Transcript pane
      if (msg.id && pendingTest[msg.id]) {
        delete pendingTest[msg.id];
        renderTestResults(msg);
        return;
      }

      // BT-722: Route show-codegen responses to Inspector pane
      if (msg.id && pendingCodegen[msg.id]) {
        delete pendingCodegen[msg.id];
        renderCodegen(msg);
        return;
      }

      // Eval response → Workspace pane
      if (msg.id && msg.status) {
        // Eval completed — decrement in-progress counter
        if (evalInProgress > 0) { evalInProgress--; updateStopButton(); }
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
      clearBtn.disabled = true;
      testBtn.disabled = true;
      codegenBtn.disabled = true;
      connectBtn.disabled = false;
      authPanel.style.display = 'flex';
      evalInProgress = 0;
      updateStopButton();
      // BT-690: Clear actor state on disconnect
      liveActors = {};
      renderActorList();
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
    evalInProgress++;
    updateStopButton();
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
    evalInProgress++;
    updateStopButton();
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
    evalInProgress++;
    updateStopButton();
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

  // --- Interrupt / Stop (BT-722) ---

  window.sendInterrupt = function() {
    if (!ws || ws.readyState !== WebSocket.OPEN) return;
    msgId++;
    ws.send(JSON.stringify({ op: 'interrupt', id: 'msg-' + msgId }));
    appendTo(replOutput, '(interrupted)\n', '#f9e2af');
    evalInProgress = 0;
    updateStopButton();
  };

  // --- Clear Bindings (BT-722) ---

  window.sendClear = function() {
    if (!ws || ws.readyState !== WebSocket.OPEN) return;
    msgId++;
    ws.send(JSON.stringify({ op: 'clear', id: 'msg-' + msgId }));
    appendTo(replOutput, '(bindings cleared)\n', '#f9e2af');
  };

  // --- Stdin (BT-722) ---

  window.submitStdin = function() {
    if (!ws || ws.readyState !== WebSocket.OPEN) return;
    var value = stdinInput.value;
    stdinOverlay.style.display = 'none';
    msgId++;
    ws.send(JSON.stringify({ op: 'stdin', id: 'msg-' + msgId, value: value }));
  };

  window.submitStdinEof = function() {
    if (!ws || ws.readyState !== WebSocket.OPEN) return;
    stdinOverlay.style.display = 'none';
    msgId++;
    ws.send(JSON.stringify({ op: 'stdin', id: 'msg-' + msgId, value: 'eof' }));
  };

  // --- Test (BT-722) ---

  window.sendTest = function() {
    var source = editorInput.value;
    if (!source.trim() || !ws || ws.readyState !== WebSocket.OPEN) return;

    // Extract class name from editor source (last subclass definition wins)
    var classMatches = source.match(/\w+\s+subclass:\s+(\w+)/g);
    var className = null;
    if (classMatches) {
      var last = classMatches[classMatches.length - 1];
      var nameMatch = last.match(/subclass:\s+(\w+)/);
      className = nameMatch ? nameMatch[1] : null;
    }
    if (!className) {
      editorStatus.textContent = 'No class definition found (expected "X subclass: Y")';
      editorStatus.style.color = '#f38ba8';
      return;
    }

    editorStatus.textContent = 'Testing ' + className + '…';
    editorStatus.style.color = '#f9e2af';
    msgId++;
    var id = 'msg-' + msgId;
    pendingTest[id] = true;
    ws.send(JSON.stringify({ op: 'test', id: id, 'class': className }));
  };

  function renderTestResults(msg) {
    if (transcriptEmpty) transcriptEmpty.remove();
    var results = msg.results;
    if (!results) {
      if (msg.error) {
        appendTo(transcriptEl, 'Test Error: ' + msg.error + '\n', '#f38ba8');
        editorStatus.textContent = 'Test error';
        editorStatus.style.color = '#f38ba8';
      }
      return;
    }

    // Summary line
    var allPassed = results.failed === 0;
    var summary = results['class'] + ': ' + results.passed + '/' + results.total + ' passed';
    if (results.duration) summary += ' (' + results.duration + 'ms)';
    var div = document.createElement('div');
    div.className = 'test-summary ' + (allPassed ? 'test-pass' : 'test-fail');
    div.textContent = summary;
    transcriptEl.appendChild(div);

    // Individual test results
    var tests = results.tests || [];
    for (var i = 0; i < tests.length; i++) {
      var t = tests[i];
      var icon = t.status === 'passed' ? '✓' : '✗';
      var row = document.createElement('div');
      row.className = t.status === 'passed' ? 'test-pass' : 'test-fail';
      row.textContent = '  ' + icon + ' ' + t.name;
      transcriptEl.appendChild(row);
      if (t.error) {
        var errRow = document.createElement('div');
        errRow.className = 'test-error-detail';
        errRow.textContent = '    ' + t.error;
        transcriptEl.appendChild(errRow);
      }
    }
    transcriptEl.scrollTop = transcriptEl.scrollHeight;

    editorStatus.textContent = allPassed ? 'All tests passed' : results.failed + ' test(s) failed';
    editorStatus.style.color = allPassed ? '#a6e3a1' : '#f38ba8';
  }

  // --- Show Codegen (BT-722) ---

  window.sendShowCodegen = function() {
    var source = editorInput.value.trim();
    if (!source || !ws || ws.readyState !== WebSocket.OPEN) return;

    msgId++;
    var id = 'msg-' + msgId;
    pendingCodegen[id] = true;
    ws.send(JSON.stringify({ op: 'show-codegen', id: id, code: source }));
  };

  function renderCodegen(msg) {
    clearEl(inspectorEl);
    if (inspectorEmpty) inspectorEmpty.remove();

    if (msg.error) {
      appendTo(inspectorEl, 'Codegen Error: ' + msg.error, '#f38ba8');
      return;
    }

    var header = document.createElement('div');
    header.style.color = '#f9e2af';
    header.style.marginBottom = '8px';
    header.textContent = 'Core Erlang';
    inspectorEl.appendChild(header);

    var pre = document.createElement('pre');
    pre.className = 'codegen-output';
    pre.textContent = msg.core_erlang || '';
    inspectorEl.appendChild(pre);

    if (msg.warnings && msg.warnings.length > 0) {
      var warnHeader = document.createElement('div');
      warnHeader.style.color = '#f9e2af';
      warnHeader.style.marginTop = '8px';
      warnHeader.textContent = 'Warnings:';
      inspectorEl.appendChild(warnHeader);
      for (var i = 0; i < msg.warnings.length; i++) {
        appendTo(inspectorEl, msg.warnings[i] + '\n', '#f9e2af');
      }
    }
  }

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
    // Ctrl+. / Cmd+. — Stop / Interrupt (BT-722)
    if (mod && e.key === '.') {
      e.preventDefault();
      sendInterrupt();
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
  // Global shortcut: Ctrl+. interrupts evaluation from anywhere (except from eval input, where it's already handled)
  document.addEventListener('keydown', function(e) {
    if ((e.ctrlKey || e.metaKey) && e.key === '/') {
      e.preventDefault();
      toggleShortcuts();
    }
    if ((e.ctrlKey || e.metaKey) && e.key === '.' && document.activeElement !== evalInput) {
      e.preventDefault();
      sendInterrupt();
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

  // BT-722: Submit stdin on Enter
  if (stdinInput) {
    stdinInput.addEventListener('keydown', function(e) {
      if (e.key === 'Enter') { e.preventDefault(); submitStdin(); }
      if (e.key === 'Escape') { e.preventDefault(); submitStdinEof(); }
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
