/* app.jsx — Beamtalk Cockpit. Attach-topology LiveView IDE: a System Browser for
   exploring classes (static, ADR 0024 Tier 1/2), a method editor write-surface
   (ADR 0082) with a ChangeLog, a Workspace for evaluating expressions, live
   session Bindings, a reference-following Inspector, and a live Transcript —
   all against an attached workspace BEAM node (ADR 0017). */

const { useState, useRef, useEffect, useCallback } = React;
const IMAGE = window.BEAM_IMAGE;

function findClass(name) { return IMAGE.classes.find((c) => c.name === name); }
function methodKey(m) { return m.klass + "|" + m.side + "|" + m.selector; }

// apply a poked message to a live value, returning the mutated copy (shared by
// the docked inspector and the floating overlay windows)
function applyPoke(o, msg) {
  if (!o || !o.fields) return o;
  const f = { ...o.fields };
  let m; const step = f.step ? f.step.value : 1;
  if (/^increment$/.test(msg) && f.value) f.value = { ...f.value, value: f.value.value + step };
  else if ((m = msg.match(/^incrementBy:\s*(-?\d+)/)) && f.value) f.value = { ...f.value, value: f.value.value + (+m[1]) };
  else if (/^decrement$/.test(msg) && f.value) f.value = { ...f.value, value: f.value.value - step };
  else if (/^reset$/.test(msg) && f.value) f.value = { ...f.value, value: 0 };
  else if ((m = msg.match(/^setTo:\s*(-?\d+)/)) && f.value) f.value = { ...f.value, value: +m[1] };
  else if ((m = msg.match(/^step:\s*(\d+)/)) && f.step) f.step = { ...f.step, value: +m[1] };
  else if ((m = msg.match(/^deposit:\s*(\d+)/)) && f.balance) f.balance = { ...f.balance, value: f.balance.value + (+m[1]) * 100 };
  else if ((m = msg.match(/^withdraw:\s*(\d+)/)) && f.balance) f.balance = { ...f.balance, value: Math.max(0, f.balance.value - (+m[1]) * 100) };
  let ps = o.printString;
  if (f.value) ps = "a Counter(" + f.value.value + ")";
  if (f.balance) ps = "an Account(" + (f.owner ? f.owner.value.split(" ")[0] : "\u2014") + " \u00b7 $" + (f.balance.value / 100).toLocaleString(undefined, { minimumFractionDigits: 2 }) + ")";
  return { ...o, fields: f, printString: ps, reductions: (o.reductions || 0) + 12, mailbox: 0 };
}

// senders / implementors search across the image
function searchRefs(kind, selector) {
  const out = [];
  IMAGE.classes.forEach((c) => {
    c.methods.forEach((m) => {
      if (kind === "implementors") {
        if (m.selector === selector) out.push(m);
      } else {
        const body = (m.source || "").split("\n").slice(1).join("\n");
        const needle = selector.endsWith(":") ? selector.split(":")[0] + ":" : selector;
        if (body.includes(needle) && m.selector !== selector) out.push(m);
      }
    });
  });
  return out;
}

function App() {
  // ---------- tweaks ----------
  const [tweaks, setTweak] = useTweaks({
    theme: "paper",
    accent: "#b9711b",
    density: "cozy",
    uiFont: "Hanken Grotesk",
    codeFont: "IBM Plex Mono",
    syntax: "warm",
    role: "owner",
    inspectorMode: "overlay",
  });
  const role = tweaks.role;
  const readOnly = role === "observer";
  const overlay = tweaks.inspectorMode === "overlay";

  useEffect(() => {
    const r = document.documentElement;
    r.setAttribute("data-theme", tweaks.theme);
    r.setAttribute("data-density", tweaks.density === "compact" ? "compact" : "cozy");
    r.style.setProperty("--ui-font", `"${tweaks.uiFont}", system-ui, sans-serif`);
    r.style.setProperty("--code-font", `"${tweaks.codeFont}", ui-monospace, monospace`);
    if (tweaks.theme === "paper" || tweaks.theme === "squeak") {
      r.style.setProperty("--accent", tweaks.accent);
      r.style.setProperty("--accent-2", shade(tweaks.accent, -22));
    } else {
      r.style.removeProperty("--accent");
      r.style.removeProperty("--accent-2");
    }
  }, [tweaks]);

  // syntax palette overrides
  useEffect(() => {
    const root = document.documentElement;
    const palettes = {
      warm: null,
      mono: {
        "--t-comment": "var(--faint)", "--t-string": "var(--ink)", "--t-symbol": "var(--ink)",
        "--t-number": "var(--ink)", "--t-keyword": "var(--accent)", "--t-reserved": "var(--accent)",
        "--t-global": "var(--ink)", "--t-var": "var(--ink)", "--t-send": "var(--accent)", "--t-return": "var(--accent)",
      },
      vivid: {
        "--t-string": "#3a8f3a", "--t-symbol": "#0f9b8a", "--t-number": "#d9700f", "--t-keyword": "#1f6fb0",
        "--t-reserved": "#d12f59", "--t-global": "#9a6314", "--t-send": "#d9700f", "--t-return": "#d12f59",
      },
    };
    const keys = ["--t-comment","--t-string","--t-symbol","--t-number","--t-keyword","--t-reserved","--t-global","--t-var","--t-send","--t-return"];
    keys.forEach((k) => root.style.removeProperty(k));
    const p = palettes[tweaks.syntax];
    if (p) Object.entries(p).forEach(([k, v]) => root.style.setProperty(k, v));
  }, [tweaks.syntax, tweaks.theme, tweaks.accent]);

  // ---------- browser state ----------
  const [view, setView] = useState("hierarchy");
  const [side, setSide] = useState("instance");
  const [selectedClass, setSelectedClass] = useState("Counter");
  const [selectedProtocol, setSelectedProtocol] = useState(null);
  const klass = findClass(selectedClass);

  // ---------- editor tabs ----------
  const initialMethod = klass.methods.find((m) => m.side === "instance" && m.selector === "increment");
  const [tabs, setTabs] = useState([{ ...initialMethod }]);
  const [activeTab, setActiveTab] = useState(0);
  const [bodies, setBodies] = useState({});

  const tab = tabs[Math.min(activeTab, tabs.length - 1)];
  const tabId = tab ? (tab.isDef ? "def:" + tab.klass : methodKey(tab)) : null;
  const baseSource = tab ? (tab.isDef ? findClass(tab.klass).definition : tab.source) : "";
  const editorValue = tab ? (bodies[tabId] != null ? bodies[tabId] : baseSource) : "";
  const isDirty = tabId != null && bodies[tabId] != null && bodies[tabId] !== baseSource;

  const [selectedMethodKey, setSelectedMethodKey] = useState(initialMethod ? methodKey(initialMethod) : null);

  const openMethod = useCallback((m) => {
    setSelectedMethodKey(methodKey(m));
    setTabs((prev) => {
      const idx = prev.findIndex((t) => !t.isDef && methodKey(t) === methodKey(m));
      if (idx >= 0) { setActiveTab(idx); return prev; }
      const next = [...prev, { ...m }];
      setActiveTab(next.length - 1);
      return next;
    });
  }, []);

  const openDefinition = useCallback((className) => {
    setTabs((prev) => {
      const idx = prev.findIndex((t) => t.isDef && t.klass === className);
      if (idx >= 0) { setActiveTab(idx); return prev; }
      const next = [...prev, { isDef: true, klass: className, selector: className + " ▸ def" }];
      setActiveTab(next.length - 1);
      return next;
    });
  }, []);

  const closeTab = (i) => {
    setTabs((prev) => {
      if (prev.length === 1) return prev;
      const next = prev.filter((_, idx) => idx !== i);
      setActiveTab((a) => (a >= i ? Math.max(0, a - 1) : a));
      return next;
    });
  };

  const onSelectClass = (name) => {
    setSelectedClass(name);
    setSelectedProtocol(null);
    const c = findClass(name);
    const first = c.methods.find((m) => m.side === side) || c.methods[0];
    if (first) { setSide(first.side); openMethod(first); }
    else openDefinition(name);
  };

  // ---------- method write-surface (ADR 0082): accept + ChangeLog + flush ----------
  const editorRef = useRef(null);
  const [flash, setFlash] = useState(0);
  const [changes, setChanges] = useState([]);
  const setEditorValue = (v) => setBodies((b) => ({ ...b, [tabId]: v }));

  const acceptMethod = () => {
    if (!tab || !isDirty || readOnly) return;
    let entry;
    if (tab.isDef) {
      findClass(tab.klass).definition = editorValue;
      entry = { klass: tab.klass, selector: "▸ class definition", intent: "redefine", flushable: true };
    } else {
      const c = findClass(tab.klass);
      const real = c.methods.find((m) => m.side === tab.side && m.selector === tab.selector);
      if (real) real.source = editorValue;
      entry = { klass: tab.klass + (tab.side === "class" ? " class" : ""), selector: tab.selector, intent: "modify", flushable: true };
    }
    setBodies((b) => { const n = { ...b }; delete n[tabId]; return n; });
    setFlash((f) => f + 1);
    // record in the ChangeLog (compiled + hot-loaded live; flushable to disk)
    setChanges((cs) => [...cs, { ...entry, id: Date.now() + Math.random(), author: "you", time: clock() }]);
    pushTranscript(`compiled ${entry.klass} » ${entry.selector} → hot-loaded`, "save");
  };

  const flushChanges = () => {
    if (readOnly) return;
    const n = changes.filter((c) => c.flushable).length;
    if (!n) return;
    setChanges((cs) => cs.map((c) => ({ ...c, flushable: false })));
    pushTranscript(`flushed ${n} change${n === 1 ? "" : "s"} to disk · .bt files written`, "save");
  };

  // ---------- workspace ----------
  const [wsCode, setWsCode] = useState(
    "// Spawn a live counter, poke it, then read its value.\n// Select any line and Print it (⌘P), or Inspect it (⌘I).\n\nc := Counter spawn\nc incrementBy: 7\nc increment\nc value"
  );
  const [wsResult, setWsResult] = useState(null);
  const wsSelRef = useRef({ text: "", s: 0, e: 0 });
  const [, forceSel] = useState(0);
  const selection = { text: wsSelRef.current.text, set: (v) => { wsSelRef.current = v; forceSel((x) => x + 1); } };
  const evalTarget = () => (wsSelRef.current.text && wsSelRef.current.text.trim()) ? wsSelRef.current.text : wsCode;

  const doIt = () => {
    if (readOnly) return;
    const r = window.beamEval(evalTarget());
    if (r.kind === "transcript") pushTranscript(r.value, "show");
    pushTranscript("do it → " + (r.ok === false ? r.error : (r.printString || "—")), r.ok === false ? "err" : "eval");
    setWsResult({ ok: true, printString: "✓ evaluated" });
  };
  const printIt = () => {
    if (readOnly) return;
    const r = window.beamEval(evalTarget());
    if (r.kind === "transcript") pushTranscript(r.value, "show");
    setWsResult(r);
    pushTranscript("print it → " + (r.ok === false ? r.error : r.printString), r.ok === false ? "err" : "eval");
  };
  const inspectIt = () => {
    if (readOnly) return;
    const r = window.beamEval(evalTarget());
    if (r.ok === false) { setWsResult(r); return; }
    const f = r.fields;
    const t = {
      printString: r.printString || String(r.value),
      className: r.className || "Object",
      pid: f && f.pid ? f.pid.value : null,
      status: f && f.pid ? "running" : null,
      mailbox: f && f.pid ? 0 : undefined,
      reductions: f && f.pid ? 240 : 0,
      fields: f || (r.kind && r.kind !== "object" ? { value: { kind: r.kind, value: r.value } } : {}),
      id: "ws" + Date.now(),
    };
    if (tweaks.inspectorMode === "overlay") { openWindow(t, "→ result"); }
    else { setInspectStack([{ label: "→ result", target: t }]); setSelectedObjId(null); }
    setWsResult(r);
    pushTranscript("inspect it → " + (r.printString || r.value), "inspect");
  };

  // ---------- transcript ----------
  const [transcript, setTranscript] = useState([
    { time: "09:14:02", text: "attached to beamtalk_workspace_demo@localhost (OTP 27)", kind: "muted" },
    { time: "09:14:02", text: "session phoenix-4821 · 4 live processes · bindings restored", kind: "muted" },
  ]);
  const pushTranscript = (text, kind) => setTranscript((t) => [...t, { time: clock(), text, kind }].slice(-80));
  const [dockTab, setDockTab] = useState("workspace");
  const transcriptRef = useRef(null);
  useEffect(() => {
    if (dockTab === "transcript" && transcriptRef.current) transcriptRef.current.scrollTop = transcriptRef.current.scrollHeight;
  }, [transcript, dockTab]);

  // ---------- live bindings + inspector ----------
  const [liveObjects, setLiveObjects] = useState(IMAGE.objects.map((o) => ({ ...o })));
  const [selectedObjId, setSelectedObjId] = useState("c");
  const [inspectStack, setInspectStack] = useState(null);
  const [flashKey, setFlashKey] = useState(0);
  // overlay mode: a stack of floating, draggable inspector windows
  const [windows, setWindows] = useState([]);
  const winZ = useRef(200);
  const winDrag = useRef(null);
  const openWindow = (target, label) => {
    const id = "w" + Date.now() + Math.round(Math.random() * 999);
    setWindows((ws) => {
      const n = ws.length;
      return [...ws, { id, rootId: target.id || null, frozen: false, stack: [{ label: label || (target.binding || target.id || "value"), target }], x: 60 + (n % 4) * 30, y: 96 + (n % 4) * 30, z: ++winZ.current }];
    });
  };
  // freeze pins the current live value as a snapshot; thawing resumes tracking
  const toggleFreeze = (id) => setWindows((ws) => ws.map((w) => {
    if (w.id !== id) return w;
    if (!w.frozen) {
      const root = w.rootId ? liveObjects.find((o) => o.id === w.rootId) : null;
      const snap = (w.stack.length === 1 && root) ? { ...root } : w.stack[w.stack.length - 1].target;
      pushTranscript((w.rootId || "value") + " \u2744 inspector frozen", "muted");
      return { ...w, frozen: true, stack: w.stack.map((s, i) => (i === 0 ? { ...s, target: snap } : s)) };
    }
    pushTranscript((w.rootId || "value") + " \u25cf inspector tracking changed: again", "muted");
    return { ...w, frozen: false };
  }));
  const closeWindow = (id) => setWindows((ws) => ws.filter((w) => w.id !== id));
  const focusWindow = (id) => setWindows((ws) => ws.map((w) => (w.id === id ? { ...w, z: ++winZ.current } : w)));
  const startWinDrag = (e, id) => {
    e.stopPropagation();
    const w = windows.find((x) => x.id === id);
    if (w) { winDrag.current = { id, dx: e.clientX - w.x, dy: e.clientY - w.y }; focusWindow(id); }
  };
  useEffect(() => {
    const move = (e) => {
      if (!winDrag.current) return;
      const { id, dx, dy } = winDrag.current;
      setWindows((ws) => ws.map((w) => (w.id === id ? { ...w, x: Math.max(0, e.clientX - dx), y: Math.max(56, e.clientY - dy) } : w)));
    };
    const up = () => { winDrag.current = null; };
    window.addEventListener("mousemove", move);
    window.addEventListener("mouseup", up);
    return () => { window.removeEventListener("mousemove", move); window.removeEventListener("mouseup", up); };
  }, [windows]);
  const drillWindow = (id, key, field) => {
    const ref = IMAGE.refs[field.ref];
    if (!ref) return;
    setWindows((ws) => ws.map((w) => (w.id === id ? { ...w, stack: [...w.stack, { label: key, target: { ...ref } }] } : w)));
  };
  const crumbWindow = (id, i) => setWindows((ws) => ws.map((w) => (w.id === id ? { ...w, stack: w.stack.slice(0, i + 1) } : w)));
  const pokeWindow = (id, msg) => {
    if (readOnly) return;
    let label = "obj";
    setWindows((ws) => ws.map((w) => {
      if (w.id !== id) return w;
      const top = w.stack[w.stack.length - 1].target;
      if (!top.pid) return w;
      label = top.binding || top.id || "obj";
      const poked = applyPoke(top, msg);
      if (top.id) setLiveObjects((objs) => objs.map((o) => (o.id === top.id ? applyPoke(o, msg) : o)));
      return { ...w, stack: w.stack.map((s, i) => (i === w.stack.length - 1 ? { ...s, target: poked } : s)) };
    }));
    pushTranscript(label + " " + msg, "send");
  };

  // the ticking actor advances on its own — the "live image" feeling
  useEffect(() => {
    const id = setInterval(() => {
      setLiveObjects((objs) => objs.map((o) => {
        if (!o.ticking || !o.fields) return o;
        const v = o.fields.value.value + 1;
        return { ...o, reductions: (o.reductions || 0) + 320, fields: { ...o.fields, value: { ...o.fields.value, value: v } }, printString: "a Counter(" + v + ")" };
      }));
    }, 1600);
    return () => clearInterval(id);
  }, []);

  const currentObj = liveObjects.find((o) => o.id === selectedObjId);
  let inspTarget = null, inspCrumbs = [];
  if (inspectStack && inspectStack.length) {
    inspTarget = inspectStack[inspectStack.length - 1].target;
    inspCrumbs = inspectStack.map((s) => ({ label: s.label }));
  } else if (currentObj) {
    inspTarget = currentObj;
    inspCrumbs = [{ label: currentObj.binding || currentObj.id }];
  }

  const drillField = (key, field) => {
    const ref = IMAGE.refs[field.ref];
    if (!ref) return;
    const base = (inspectStack && inspectStack.length) ? inspectStack : [{ label: currentObj.binding || currentObj.id, target: currentObj }];
    setInspectStack([...base, { label: key, target: { ...ref } }]);
  };
  const crumbTo = (i) => {
    if (!inspectStack) return;
    if (i === 0 && currentObj && inspectStack[0].target === currentObj) { setInspectStack(null); return; }
    setInspectStack(inspectStack.slice(0, i + 1));
  };

  const pokeObject = (msg) => {
    if (readOnly) return;
    const target = (inspectStack && inspectStack.length) ? inspectStack[inspectStack.length - 1].target : currentObj;
    if (!target || !target.pid) return;
    setLiveObjects((objs) => objs.map((o) => (o.id === target.id ? applyPoke(o, msg) : o)));
    if (inspectStack && inspectStack.length) {
      setInspectStack((st) => st.map((s, i) => (i === st.length - 1 ? { ...s, target: applyPoke(s.target, msg) } : s)));
    }
    setFlashKey((k) => k + 1);
    const label = target.binding || target.id || "obj";
    pushTranscript(label + " " + msg, "send");
  };

  const quickPokesFor = (t) => {
    if (!t) return [];
    if (t.className === "Counter") return ["increment", "incrementBy: 10", "reset"];
    if (t.className === "Account") return ["deposit: 50", "withdraw: 20"];
    if (t.className === "Room") return ['broadcast: "hi"'];
    return [];
  };

  // ---------- senders / implementors popover ----------
  const [refsPop, setRefsPop] = useState(null);
  const openRefs = (kind, ev) => {
    if (!tab || tab.isDef) return;
    const results = searchRefs(kind, tab.selector);
    const rect = ev.currentTarget.getBoundingClientRect();
    const h = Math.min(320, 56 + results.length * 26);
    setRefsPop({ x: Math.min(rect.left, window.innerWidth - 320), y: Math.max(12, rect.top - h - 8), kind, selector: tab.selector, results });
  };
  const navigateToRef = (m) => {
    setSelectedClass(m.klass);
    setSide(m.side);
    setSelectedProtocol(null);
    openMethod(m);
  };

  // global shortcuts
  useEffect(() => {
    const h = (e) => {
      if ((e.metaKey || e.ctrlKey) && e.key.toLowerCase() === "s") { e.preventDefault(); acceptMethod(); }
    };
    window.addEventListener("keydown", h);
    return () => window.removeEventListener("keydown", h);
  });

  const crumb = tab ? (tab.isDef
    ? <span className="crumb"><b>{tab.klass}</b> <span className="mono" style={{ color: "var(--accent-2)" }}>▸ class definition</span></span>
    : <span className="crumb"><b>{tab.klass}</b>{tab.side === "class" ? <span className="mono"> class</span> : null} <span style={{ color: "var(--faint)" }}>{side === "class" ? "class" : "instance"} »</span> <span className="mono">{tab.selector}</span> {tab.runtime ? <span className="tier">runtime</span> : null} <span style={{ color: "var(--faint)" }}>· {tab.category}</span></span>)
    : null;

  const pendingFlush = changes.filter((c) => c.flushable).length;

  return (
    <div className="app">
      {/* top bar — Attach topology */}
      <div className="topbar">
        <div className="brand">
          <span className="mark"><b>Beam</b>talk</span>
        </div>
        <div className="viewswitch">
          <span className="vs on">Cockpit</span>
          <a className="vs" href="Beamtalk Morphic.html">Morphic</a>
        </div>
        <div className="omni">
          <span style={{ color: "var(--accent)", fontWeight: 700 }}>⌕</span>
          <input placeholder="Search classes, selectors, senders…  (try “increment”)" spellCheck={false} />
          <kbd>⌘K</kbd>
        </div>
        <div className="insp-mode" title="Where the Inspector lives">
          <span className="im-label">Inspector</span>
          <div className="seg">
            <button className={!overlay ? "on" : ""} onClick={() => setTweak("inspectorMode", "docked")}>Dock</button>
            <button className={overlay ? "on" : ""} onClick={() => setTweak("inspectorMode", "overlay")}>Float</button>
          </div>
        </div>
        <div className="attach">
          <span className="dot live" />
          <span className="att-label">attached</span>
          <b className="att-node mono">{IMAGE.workspace.node}</b>
          <span className="att-sep">·</span>
          <span className="att-sess mono">{IMAGE.workspace.session}</span>
          <span className={"role-badge " + role}>{role}</span>
        </div>
      </div>

      <div className="cockpit">
        {/* LEFT — System Browser */}
        <div className="col">
          <div className="browser-split">
            <ClassBrowser
              image={IMAGE} view={view} setView={setView}
              selectedClass={selectedClass} onSelectClass={onSelectClass}
              side={side} setSide={setSide}
            />
            <MethodList
              klass={klass} side={side}
              selectedProtocol={selectedProtocol} setProtocol={setSelectedProtocol}
              selectedMethod={selectedMethodKey} onSelectMethod={(m) => { setSide(m.side); openMethod(m); }}
              image={IMAGE}
            />
          </div>
        </div>

        {/* CENTER — Editor + Workspace dock */}
        <div className="col">
          <div className="panel editor-panel">
            <div className="tabstrip">
              {tabs.map((t, i) => {
                const id = t.isDef ? "def:" + t.klass : methodKey(t);
                const d = bodies[id] != null && bodies[id] !== (t.isDef ? findClass(t.klass).definition : t.source);
                return (
                  <div key={id} className={"tab" + (i === activeTab ? " on" : "")} onClick={() => setActiveTab(i)}>
                    {d ? <span className="modot" /> : null}
                    <span>{t.isDef ? t.klass + " ▸ def" : t.selector}</span>
                    {tabs.length > 1 ? <span className="x" onClick={(e) => { e.stopPropagation(); closeTab(i); }}>×</span> : null}
                  </div>
                );
              })}
              <div style={{ flex: 1 }} />
              <div className="tab" style={{ color: "var(--muted)" }} onClick={() => openDefinition(selectedClass)} title="Open class definition">+ def</div>
            </div>
            <div className="editor-meta">{crumb}<span style={{ flex: 1 }} />{readOnly ? <span style={{ color: "var(--accent-2)" }}>read-only · Observer</span> : isDirty ? <span style={{ color: "var(--accent-2)" }}>edited — ⌘S to compile</span> : <span style={{ color: "var(--faint)" }}>in image</span>}</div>
            <CodeEditor
              key={"f" + flash}
              value={editorValue}
              onChange={readOnly ? undefined : setEditorValue}
              taRef={editorRef}
              wrapClass={"editor-wrap" + (flash ? " flash-ok" : "")}
              preClass="editor-pre"
              taClass="editor-ta mono"
              onKeyDown={(ev) => {
                if (readOnly) { ev.preventDefault(); return; }
                if (ev.key === "Tab") {
                  ev.preventDefault();
                  const el = ev.target; const s = el.selectionStart, e = el.selectionEnd;
                  const nv = editorValue.slice(0, s) + "  " + editorValue.slice(e);
                  setEditorValue(nv);
                  requestAnimationFrame(() => { el.selectionStart = el.selectionEnd = s + 2; });
                }
              }}
            />
            <div className="actionbar">
              <button className="btn primary" onClick={acceptMethod} disabled={!isDirty || readOnly}>Compile <span className="k">⌘S</span></button>
              <button className="btn" onClick={(e) => openRefs("senders", e)} disabled={!tab || tab.isDef}>Senders</button>
              <button className="btn" onClick={(e) => openRefs("implementors", e)} disabled={!tab || tab.isDef}>Implementors</button>
              <span className="spacer" />
              <span className="kbdhint">{klass ? klass.name : ""} · {side}</span>
            </div>
          </div>

          {/* dock */}
          <div className="dock">
            <div className="panel" style={{ height: "100%" }}>
              <div className="panel-head" style={{ gap: 14 }}>
                <span className="dock-tabs">
                  <span className={"dock-tab" + (dockTab === "workspace" ? " on" : "")} onClick={() => setDockTab("workspace")}>Workspace</span>
                  <span className={"dock-tab" + (dockTab === "transcript" ? " on" : "")} onClick={() => setDockTab("transcript")}>Transcript</span>
                  <span className={"dock-tab" + (dockTab === "changes" ? " on" : "")} onClick={() => setDockTab("changes")}>
                    Changes{changes.length ? <span className="tab-count">{changes.length}</span> : null}
                  </span>
                </span>
                <span className="spacer" />
                {dockTab === "workspace"
                  ? <span className="count">{selection.text && selection.text.trim() ? "evaluates selection" : "evaluates buffer"}</span>
                  : dockTab === "transcript"
                    ? <button className="btn ghost" style={{ padding: "3px 9px" }} onClick={() => setTranscript([])}>Clear</button>
                    : <button className="btn ghost" style={{ padding: "3px 9px" }} onClick={flushChanges} disabled={!pendingFlush || readOnly}>Save all to disk{pendingFlush ? " (" + pendingFlush + ")" : ""}</button>}
              </div>

              {dockTab === "workspace" ? (
                <>
                  <CodeEditor
                    value={wsCode}
                    onChange={setWsCode}
                    onSelect={(text, s, e) => selection.set({ text, s, e })}
                    wrapClass="ws-wrap"
                    preClass="ws-pre"
                    taClass="ws-ta mono"
                    onKeyDown={(ev) => {
                      if ((ev.metaKey || ev.ctrlKey) && ev.key.toLowerCase() === "d") { ev.preventDefault(); doIt(); }
                      if ((ev.metaKey || ev.ctrlKey) && ev.key.toLowerCase() === "p") { ev.preventDefault(); printIt(); }
                      if ((ev.metaKey || ev.ctrlKey) && ev.key.toLowerCase() === "i") { ev.preventDefault(); inspectIt(); }
                    }}
                  />
                  {wsResult ? (
                    <div className={"ws-result" + (wsResult.ok === false ? " err" : "")}>
                      <span className="arrow">→</span>
                      <span className="val">{wsResult.ok === false ? wsResult.error : wsResult.printString}</span>
                    </div>
                  ) : null}
                  <div className="actionbar">
                    <button className="btn" onClick={doIt} disabled={readOnly}>Do it <span className="k">⌘D</span></button>
                    <button className="btn primary" onClick={printIt} disabled={readOnly}>Print it <span className="k">⌘P</span></button>
                    <button className="btn" onClick={inspectIt} disabled={readOnly}>Inspect it <span className="k">⌘I</span></button>
                    <span className="spacer" />
                    <span className="kbdhint">{readOnly ? "Observer — evaluation disabled" : "select an expression, or evaluate all"}</span>
                  </div>
                </>
              ) : dockTab === "transcript" ? (
                <div className="panel-body" ref={transcriptRef}>
                  <div className="transcript">
                    {transcript.map((r, i) => (
                      <div key={i} className="t-line">
                        <span className="t-time">{r.time}</span>
                        {r.kind === "err"
                          ? <span style={{ color: "var(--err)" }}>{r.text}</span>
                          : r.kind === "muted"
                            ? <span className="t-muted">{r.text}</span>
                            : r.kind === "show"
                              ? <span><span className="t-arrow">»</span> <span className="t-show">{r.text}</span></span>
                              : <span><span className="t-arrow">{r.kind === "send" ? "←" : r.kind === "save" ? "✓" : "→"}</span> {r.text}</span>}
                      </div>
                    ))}
                  </div>
                </div>
              ) : (
                <div className="panel-body">
                  <ChangeLog changes={changes} />
                </div>
              )}
            </div>
          </div>
        </div>

        {/* RIGHT — live Bindings + (docked) Inspector, or Bindings-only in overlay mode */}
        <div className="col">
          <BindingsList
            objects={liveObjects} role={role} fill={overlay}
            selectedId={overlay ? null : (inspectStack ? null : selectedObjId)}
            onSelect={(o) => {
              if (overlay) { openWindow(o, o.binding); }
              else { setSelectedObjId(o.id); setInspectStack(null); }
            }}
          />
          {!overlay ? (
            <Inspector
              target={inspTarget}
              crumbs={inspCrumbs}
              role={role}
              onDrill={drillField}
              onCrumb={crumbTo}
              onPoke={pokeObject}
              quickPokes={quickPokesFor(inspTarget)}
              flashKey={flashKey}
            />
          ) : (
            <div className="panel insp" style={{ flex: 1 }}>
              <div className="panel-head">Inspector <span className="spacer" /><span className="count">floating</span></div>
              <div className="empty">
                Click a binding (or <b>Inspect&nbsp;it</b>) to open a floating inspector window.
                Open as many as you like, drag them around, and follow references inside each.
                {windows.length ? <div style={{ marginTop: 10, color: "var(--accent-2)" }}>{windows.length} open</div> : null}
              </div>
            </div>
          )}
        </div>
      </div>

      {/* floating inspector windows (overlay mode) — depth-1 windows observe
          their object's changed: announcements and re-render live, unless frozen */}
      {overlay ? windows.map((w) => {
        const depth = w.stack.length;
        const root = w.rootId ? liveObjects.find((o) => o.id === w.rootId) : null;
        const trackable = depth === 1 && !!root && !!root.pid;
        const tracking = trackable && !w.frozen;
        const effTop = tracking ? root : w.stack[depth - 1].target;
        const effWin = { ...w, stack: w.stack.map((s, i) => (i === depth - 1 ? { ...s, target: effTop } : s)) };
        return (
          <InspectorWindow
            key={w.id} win={effWin} role={role}
            trackable={trackable} tracking={tracking} frozen={w.frozen}
            onToggleFreeze={() => toggleFreeze(w.id)}
            onClose={closeWindow}
            onFocus={focusWindow}
            onDragStart={startWinDrag}
            onDrill={(key, field) => drillWindow(w.id, key, field)}
            onCrumb={(i) => crumbWindow(w.id, i)}
            onPoke={(msg) => pokeWindow(w.id, msg)}
            quickPokes={quickPokesFor(effTop)}
          />
        );
      }) : null}

      <RefsPopover data={refsPop} onClose={() => setRefsPop(null)} onNavigate={navigateToRef} />

      <TweaksPanel title="Tweaks">
        <TweakSection label="Session" />
        <TweakRadio label="Role" value={tweaks.role} options={["owner", "observer"]} onChange={(v) => setTweak("role", v)} />
        <TweakRadio label="Inspector" value={tweaks.inspectorMode} options={["docked", "overlay"]} onChange={(v) => setTweak("inspectorMode", v)} />
        <TweakSection label="Theme" />
        <TweakRadio label="Surface" value={tweaks.theme} options={["paper", "squeak", "dusk"]} onChange={(v) => setTweak("theme", v)} />
        <TweakColor label="Accent" value={tweaks.accent} options={["#b9711b", "#a8324e", "#2c6e8e", "#5d7a2e", "#7a4ea8"]} onChange={(v) => setTweak("accent", v)} />
        <TweakSection label="Code" />
        <TweakRadio label="Syntax" value={tweaks.syntax} options={["warm", "mono", "vivid"]} onChange={(v) => setTweak("syntax", v)} />
        <TweakSelect label="Code font" value={tweaks.codeFont} options={["IBM Plex Mono", "JetBrains Mono", "Spline Sans Mono", "Courier Prime"]} onChange={(v) => setTweak("codeFont", v)} />
        <TweakSection label="Layout" />
        <TweakRadio label="Density" value={tweaks.density} options={["cozy", "compact"]} onChange={(v) => setTweak("density", v)} />
        <TweakSelect label="UI font" value={tweaks.uiFont} options={["Hanken Grotesk", "Inter Tight", "Public Sans", "Schibsted Grotesk"]} onChange={(v) => setTweak("uiFont", v)} />
      </TweaksPanel>
    </div>
  );
}

function ChangeLog({ changes }) {
  if (!changes.length) {
    return (
      <div className="empty">
        No pending changes. Edit a method and <b>Compile</b> (⌘S) — it hot-loads into the
        live workspace and a flushable entry appears here (ADR 0082).
      </div>
    );
  }
  return (
    <table className="change-table">
      <thead>
        <tr><th>Class</th><th>Selector</th><th>Intent</th><th>State</th><th>Author</th></tr>
      </thead>
      <tbody>
        {changes.slice().reverse().map((c) => (
          <tr key={c.id}>
            <td className="mono ct-class">{c.klass}</td>
            <td className="mono ct-sel">{c.selector}</td>
            <td><span className={"intent " + c.intent}>{c.intent}</span></td>
            <td>{c.flushable ? <span className="state-pending">● pending</span> : <span className="state-saved">✓ on disk</span>}</td>
            <td className="ct-author">{c.author}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}

function clock() { return new Date().toTimeString().slice(0, 8); }

// small color utility: shade a hex toward black(neg)/white(pos) by pct
function shade(hex, pct) {
  const h = hex.replace("#", "");
  const n = parseInt(h.length === 3 ? h.split("").map((c) => c + c).join("") : h, 16);
  let r = (n >> 16) & 255, g = (n >> 8) & 255, b = n & 255;
  const f = pct / 100;
  const adj = (c) => Math.max(0, Math.min(255, Math.round(c + (f < 0 ? c * f : (255 - c) * f))));
  r = adj(r); g = adj(g); b = adj(b);
  return "#" + ((1 << 24) + (r << 16) + (g << 8) + b).toString(16).slice(1);
}

ReactDOM.createRoot(document.getElementById("root")).render(<App />);
