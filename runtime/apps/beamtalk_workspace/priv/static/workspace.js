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

  var ws = null;
  var msgId = 0;
  // Track pending inspect requests to route responses to Inspector
  var pendingInspects = {};

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
      ws.send(JSON.stringify({ type: 'auth', cookie: cookie }));
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
        return;
      }
      if (msg.type === 'auth_error') {
        setStatus('Auth failed: ' + msg.message, '#f38ba8');
        ws.close();
        return;
      }
      if (msg.op === 'session-started') {
        var display = msg.session ? msg.session.slice(0, 8) + '…' : 'unknown';
        setStatus('Connected (session ' + display + ')', '#a6e3a1');
        authPanel.style.display = 'none';
        evalInput.disabled = false;
        sendBtn.disabled = false;
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

      // Load-source response → Editor status
      if (msg.id && msg.id.indexOf('load-') === 0 && msg.classes) {
        editorStatus.textContent = 'Loaded: ' + msg.classes.join(', ');
        editorStatus.style.color = '#a6e3a1';
        return;
      }
      if (msg.id && msg.id.indexOf('load-') === 0 && msg.error) {
        editorStatus.textContent = msg.error;
        editorStatus.style.color = '#f38ba8';
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
      setStatus('Disconnected', '#f38ba8');
      evalInput.disabled = true;
      sendBtn.disabled = true;
      inspectBtn.disabled = true;
      loadBtn.disabled = true;
      connectBtn.disabled = false;
      authPanel.style.display = 'flex';
    };

    ws.onerror = function() {
      setStatus('Connection error', '#f38ba8');
    };
  };

  // --- Eval (Workspace pane) ---

  window.sendEval = function() {
    var code = evalInput.value.trim();
    if (!code || !ws || ws.readyState !== WebSocket.OPEN) return;

    appendTo(replOutput, code + '\n', '#89b4fa');
    msgId++;
    ws.send(JSON.stringify({ op: 'eval', id: 'msg-' + msgId, code: code }));
    evalInput.value = '';
    evalInput.focus();
  };

  // --- Inspect (sends inspect op, routes result to Inspector) ---

  window.sendInspect = function() {
    var code = evalInput.value.trim();
    if (!code || !ws || ws.readyState !== WebSocket.OPEN) return;

    appendTo(replOutput, code + '\n', '#89b4fa');
    msgId++;
    var id = 'msg-' + msgId;
    pendingInspects[id] = true;
    ws.send(JSON.stringify({ op: 'eval', id: id, code: code }));
    evalInput.value = '';
    evalInput.focus();
  };

  // --- Load Source (Editor pane) ---

  window.loadSource = function() {
    var source = editorInput.value;
    if (!source.trim() || !ws || ws.readyState !== WebSocket.OPEN) return;

    editorStatus.textContent = 'Compiling…';
    editorStatus.style.color = '#f9e2af';
    msgId++;
    ws.send(JSON.stringify({ op: 'load-source', id: 'load-' + msgId, source: source }));
  };

  // --- Keyboard shortcuts ---

  evalInput.addEventListener('keydown', function(e) {
    if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
      e.preventDefault();
      sendEval();
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
})();
