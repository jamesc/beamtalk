/* morphic.jsx — Beamtalk Morphic World. Live objects as draggable morphs with
   Squeak-style halos. Direct manipulation: poke a morph and its state changes in
   place while it runs; drag it around the world; open a halo to inspect, send a
   message (!), duplicate (spawn), or delete (terminate) the process. */

const { useState, useRef, useEffect, useCallback } = React;
const IMG = window.BEAM_IMAGE;

let PID = 230;
const nextPid = () => "<0." + (PID++) + ".0>";

// halo handle icons (tiny inline SVGs)
const Icon = {
  delete: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="3"><path d="M6 6l12 12M18 6L6 18"/></svg>,
  menu: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="3"><path d="M4 7h16M4 12h16M4 17h16"/></svg>,
  move: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2.5"><path d="M12 3v18M3 12h18M12 3l-3 3M12 3l3 3M12 21l-3-3M12 21l3-3M3 12l3-3M3 12l3 3M21 12l-3-3M21 12l-3 3"/></svg>,
  dup: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2.5"><rect x="8" y="8" width="12" height="12" rx="2"/><path d="M4 16V5a1 1 0 011-1h11"/></svg>,
  inspect: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2.5"><circle cx="11" cy="11" r="7"/><path d="M21 21l-4-4"/></svg>,
  resize: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2.5"><path d="M9 21H3v-6M21 9V3h-6M21 3l-7 7M3 21l7-7"/></svg>,
};

function HaloHandle({ cls, style, tip, icon, onMouseDown, onClick }) {
  return (
    <div className={"handle " + cls} style={style} onMouseDown={onMouseDown} onClick={onClick}>
      {typeof icon === "string" ? icon : icon}
      <span className="tip">{tip}</span>
    </div>
  );
}

function Halo({ morph, onAction, onMoveDown, onResizeDown }) {
  return (
    <div className="halo" style={{ left: morph.x, top: morph.y, width: morph.w, height: morph.h }}>
      <div className="halo-ring" />
      <div className="halo-name">{morph.binding} · {morph.className}</div>
      <HaloHandle cls="h-delete" tip="terminate (✕)" icon={Icon.delete} style={{ top: -14, left: -14 }} onClick={() => onAction("delete")} />
      <HaloHandle cls="h-menu" tip="menu" icon={Icon.menu} style={{ top: -14, left: "30%" }} onClick={(e) => onAction("menu", e)} />
      <HaloHandle cls="h-move" tip="pick up / move" icon={Icon.move} style={{ top: -14, left: "50%", marginLeft: -12 }} onMouseDown={onMoveDown} />
      <HaloHandle cls="h-dup" tip="duplicate (spawn)" icon={Icon.dup} style={{ top: -14, right: -14 }} onClick={() => onAction("dup")} />
      <HaloHandle cls="h-send" tip="send a message (!)" icon="!" style={{ top: "50%", right: -14, marginTop: -12 }} onClick={(e) => onAction("send", e)} />
      <HaloHandle cls="h-inspect" tip="inspect" icon={Icon.inspect} style={{ bottom: -14, left: -14 }} onClick={(e) => onAction("inspect", e)} />
      <HaloHandle cls="h-resize" tip="resize" icon={Icon.resize} style={{ bottom: -14, right: -14 }} onMouseDown={onResizeDown} />
    </div>
  );
}

/* ---- per-class morph bodies ---- */
function CounterBody({ m, bump, onSend }) {
  return (
    <div className="morph-body">
      <div className={"counter-val" + (bump ? " bump" : "")}>{m.fields.value.value}</div>
      <div className="counter-meta">step {m.fields.step.value} · {(m.reductions || 0).toLocaleString()} reds</div>
      <div className="counter-ctl">
        <button className="mbtn" onMouseDown={stop} onClick={() => onSend("decrement")}>−</button>
        <button className="mbtn ghost" onMouseDown={stop} onClick={() => onSend("reset")}>reset</button>
        <button className="mbtn" onMouseDown={stop} onClick={() => onSend("increment")}>+</button>
      </div>
    </div>
  );
}
function AccountBody({ m, bump, onSend }) {
  const bal = "$" + (m.fields.balance.value / 100).toLocaleString(undefined, { minimumFractionDigits: 2 });
  return (
    <div className="morph-body">
      <div className="acct-owner">owner <b>{m.fields.owner.value}</b></div>
      <div className={"acct-bal" + (bump ? " bump" : "")}>{bal}</div>
      <div className="acct-row">
        <button className="mbtn wide" onMouseDown={stop} onClick={() => onSend("deposit: 100")}>deposit 100</button>
        <button className="mbtn wide" onMouseDown={stop} onClick={() => onSend("withdraw: 50")}>withdraw 50</button>
      </div>
      <div className="acct-log">last: {m.lastTxn || "credit $300.00 · 11:02"}</div>
    </div>
  );
}
function RoomBody({ m, onSend }) {
  const [draft, setDraft] = useState("");
  const send = () => { if (draft.trim()) { onSend('broadcast: "' + draft.trim() + '"', draft.trim()); setDraft(""); } };
  return (
    <div className="morph-body">
      <div className="room-name">{m.fields.name.value}</div>
      <div className="room-members">
        {(m.members || ["@ada", "@grace", "@alan"]).map((x) => <span key={x} className="room-chip">{x}</span>)}
      </div>
      <div className="room-feed">
        {(m.feed || ['"@grace: spawning 1k workers"', '"@alan: mailbox looks clean"']).slice(-3).map((x, i) => (
          <div key={i} className="rmsg">{x}</div>
        ))}
      </div>
      <div className="room-cast" onMouseDown={stop}>
        <input placeholder="broadcast…" value={draft} onChange={(e) => setDraft(e.target.value)} onKeyDown={(e) => { if (e.key === "Enter") send(); }} />
        <button className="mbtn" onClick={send}>!</button>
      </div>
    </div>
  );
}
function stop(e) { e.stopPropagation(); }

/* ---- the morph shell ---- */
function MorphCard({ m, selected, bump, onSelect, onHeadDown, onSend, children, anim }) {
  return (
    <div
      className={"morph" + (selected ? " selected" : "") + (anim ? " " + anim : "")}
      style={{ left: m.x, top: m.y, width: m.w, height: m.h, zIndex: m.z }}
      onMouseDown={(e) => { e.stopPropagation(); onSelect(m.id); }}
    >
      <div className="morph-head" onMouseDown={(e) => onHeadDown(e, m.id)}>
        <span className="morph-bind">{m.binding}</span>
        <span className="morph-class">a {m.className}</span>
        <span className="morph-pid">{m.pid}</span>
        <span className={"morph-live" + (m.ticking ? " tick" : "")} />
      </div>
      {children}
    </div>
  );
}

function App() {
  const [tweaks, setTweak] = useTweaks({
    theme: "paper", accent: "#b9711b", codeFont: "IBM Plex Mono", uiFont: "Hanken Grotesk",
    grid: true, alwaysHalos: false,
  });
  useEffect(() => {
    const r = document.documentElement;
    r.setAttribute("data-theme", tweaks.theme);
    r.style.setProperty("--ui-font", `"${tweaks.uiFont}", system-ui, sans-serif`);
    r.style.setProperty("--code-font", `"${tweaks.codeFont}", ui-monospace, monospace`);
    if (tweaks.theme !== "dusk") {
      r.style.setProperty("--accent", tweaks.accent);
      r.style.setProperty("--accent-2", shade(tweaks.accent, -22));
    } else { r.style.removeProperty("--accent"); r.style.removeProperty("--accent-2"); }
  }, [tweaks]);

  // seed morphs from the live image
  const [morphs, setMorphs] = useState(() => ([
    { id: "c", binding: "c", className: "Counter", pid: "<0.142.0>", x: 150, y: 120, w: 230, h: 210, z: 1, ticking: false,
      reductions: 18440, fields: { value: { kind: "int", value: 7 }, step: { kind: "int", value: 1 }, pid: { kind: "pid", value: "<0.142.0>" } } },
    { id: "ticker", binding: "ticker", className: "Counter", pid: "<0.171.0>", x: 430, y: 150, w: 230, h: 210, z: 2, ticking: true,
      reductions: 204880, fields: { value: { kind: "int", value: 1284 }, step: { kind: "int", value: 1 }, pid: { kind: "pid", value: "<0.171.0>" } } },
    { id: "acct", binding: "acct", className: "Account", pid: "<0.151.0>", x: 175, y: 380, w: 268, h: 230, z: 3,
      lastTxn: "debit $60.00 · 14:35",
      fields: { owner: { kind: "string", value: "Ada Lovelace" }, balance: { kind: "int", value: 124000 }, log: { kind: "ref", value: "a List(3)", ref: "acctLog" }, pid: { kind: "pid", value: "<0.151.0>" } } },
    { id: "lobby", binding: "lobby", className: "Room", pid: "<0.160.0>", x: 720, y: 300, w: 290, h: 250, z: 4,
      members: ["@ada", "@grace", "@alan"], feed: ['"@ada: ship it"', '"@grace: spawning 1k workers"', '"@alan: mailbox looks clean"'],
      fields: { name: { kind: "symbol", value: "#lobby" }, members: { kind: "ref", value: "a Set(3)", ref: "lobbyMembers" }, history: { kind: "ref", value: "a List(12)", ref: "lobbyHistory" }, pid: { kind: "pid", value: "<0.160.0>" } } },
  ]));

  const [selected, setSelected] = useState("c");
  const [bumpId, setBumpId] = useState(null);
  const [anim, setAnim] = useState({});       // id -> "pop" | "poof"
  const [sendFor, setSendFor] = useState(null);
  const [menuFor, setMenuFor] = useState(null);
  const [inspect, setInspect] = useState(null); // { id, stack:[{label,target}] }
  const [log, setLog] = useState([]);
  const zTop = useRef(10);

  const pushLog = (binding, msg) => setLog((l) => [...l, { binding, msg, t: Date.now() }].slice(-4));

  const bringFront = (id) => setMorphs((ms) => ms.map((m) => (m.id === id ? { ...m, z: ++zTop.current } : m)));
  const onSelect = (id) => { setSelected(id); setMenuFor(null); bringFront(id); };

  // ---- live ticker ----
  useEffect(() => {
    const t = setInterval(() => {
      setMorphs((ms) => ms.map((m) => m.ticking
        ? { ...m, reductions: m.reductions + 320, fields: { ...m.fields, value: { ...m.fields.value, value: m.fields.value.value + 1 } } }
        : m));
    }, 1600);
    return () => clearInterval(t);
  }, []);

  // ---- dragging ----
  const drag = useRef(null);
  const startDrag = (e, id) => {
    e.stopPropagation();
    const m = morphs.find((x) => x.id === id);
    drag.current = { id, dx: e.clientX - m.x, dy: e.clientY - m.y };
    onSelect(id);
  };
  const resize = useRef(null);
  const startResize = (e, id) => {
    e.stopPropagation();
    const m = morphs.find((x) => x.id === id);
    resize.current = { id, sx: e.clientX, sy: e.clientY, w: m.w, h: m.h };
  };
  useEffect(() => {
    const move = (e) => {
      if (drag.current) {
        const { id, dx, dy } = drag.current;
        setMorphs((ms) => ms.map((m) => (m.id === id ? { ...m, x: Math.max(0, e.clientX - dx), y: Math.max(56, e.clientY - dy) } : m)));
      } else if (resize.current) {
        const { id, sx, sy, w, h } = resize.current;
        setMorphs((ms) => ms.map((m) => (m.id === id ? { ...m, w: Math.max(190, w + (e.clientX - sx)), h: Math.max(150, h + (e.clientY - sy)) } : m)));
      }
    };
    const up = () => { drag.current = null; resize.current = null; };
    window.addEventListener("mousemove", move);
    window.addEventListener("mouseup", up);
    return () => { window.removeEventListener("mousemove", move); window.removeEventListener("mouseup", up); };
  }, []);

  // ---- send a message to a morph (direct manipulation) ----
  const sendMessage = (id, msg, roomText) => {
    setMorphs((ms) => ms.map((m) => {
      if (m.id !== id) return m;
      const f = { ...m.fields };
      let mm; const step = f.step ? f.step.value : 1;
      let extra = {};
      if (/^increment$/.test(msg) && f.value) f.value = { ...f.value, value: f.value.value + step };
      else if (/^decrement$/.test(msg) && f.value) f.value = { ...f.value, value: f.value.value - step };
      else if (/^reset$/.test(msg) && f.value) f.value = { ...f.value, value: 0 };
      else if ((mm = msg.match(/^incrementBy:\s*(-?\d+)/)) && f.value) f.value = { ...f.value, value: f.value.value + (+mm[1]) };
      else if ((mm = msg.match(/^deposit:\s*(\d+)/)) && f.balance) { f.balance = { ...f.balance, value: f.balance.value + (+mm[1]) * 100 }; extra.lastTxn = "credit $" + (+mm[1]) + ".00 · now"; }
      else if ((mm = msg.match(/^withdraw:\s*(\d+)/)) && f.balance) { f.balance = { ...f.balance, value: Math.max(0, f.balance.value - (+mm[1]) * 100) }; extra.lastTxn = "debit $" + (+mm[1]) + ".00 · now"; }
      else if (/^broadcast:/.test(msg) && roomText) extra.feed = [...(m.feed || []), '"@you: ' + roomText + '"'];
      return { ...m, fields: f, reductions: (m.reductions || 0) + 12, ...extra };
    }));
    setBumpId(id); setTimeout(() => setBumpId(null), 140);
    const b = morphs.find((m) => m.id === id);
    pushLog(b ? b.binding : id, msg);
  };

  // ---- halo actions ----
  const doAction = (id, action, e) => {
    const m = morphs.find((x) => x.id === id);
    if (action === "delete") {
      setAnim((a) => ({ ...a, [id]: "poof" }));
      pushLog(m.binding, "stop  // process terminated");
      setTimeout(() => { setMorphs((ms) => ms.filter((x) => x.id !== id)); setSelected(null); if (inspect && inspect.id === id) setInspect(null); }, 240);
    } else if (action === "dup") {
      const copy = { ...m, id: m.id + "_" + (PID), binding: m.binding + "2", pid: nextPid(), x: m.x + 36, y: m.y + 36, z: ++zTop.current, ticking: false, fields: JSON.parse(JSON.stringify(m.fields)) };
      copy.fields.pid = { kind: "pid", value: copy.pid };
      setMorphs((ms) => [...ms, copy]);
      setAnim((a) => ({ ...a, [copy.id]: "pop" }));
      setSelected(copy.id);
      pushLog(m.binding, "spawn  // " + copy.pid);
    } else if (action === "send") { setSendFor(sendFor === id ? null : id); setMenuFor(null); }
    else if (action === "menu") { setMenuFor(menuFor === id ? null : id); setSendFor(null); }
    else if (action === "inspect") { openInspector(id); }
  };

  const openInspector = (id) => {
    const m = morphs.find((x) => x.id === id);
    setInspect({ id, stack: [{ label: m.binding, target: m }] });
    setMenuFor(null);
  };
  const drillInspect = (field) => {
    const ref = IMG.refs[field.ref];
    if (!ref) return;
    setInspect((s) => ({ ...s, stack: [...s.stack, { label: field.value, target: ref }] }));
  };

  const palette = [
    { cls: "Counter", make: () => ({ fields: { value: { kind: "int", value: 0 }, step: { kind: "int", value: 1 } }, w: 230, h: 210 }) },
    { cls: "Account", make: () => ({ fields: { owner: { kind: "string", value: "New Owner" }, balance: { kind: "int", value: 0 } }, lastTxn: "—", w: 268, h: 230 }) },
    { cls: "Room", make: () => ({ fields: { name: { kind: "symbol", value: "#room" } }, members: [], feed: [], w: 290, h: 230 }) },
  ];
  const spawnMorph = (entry) => {
    const base = entry.make();
    const pid = nextPid();
    const letter = entry.cls[0].toLowerCase();
    const id = letter + "_" + PID;
    base.fields.pid = { kind: "pid", value: pid };
    const m = { id, binding: letter + (morphs.length), className: entry.cls, pid, x: 520 + Math.random() * 80, y: 420 + Math.random() * 40, z: ++zTop.current, reductions: 0, ...base };
    setMorphs((ms) => [...ms, m]);
    setAnim((a) => ({ ...a, [id]: "pop" }));
    setSelected(id);
    pushLog(m.binding, entry.cls + " spawn  // " + pid);
  };

  const clearAnim = (id) => setTimeout(() => setAnim((a) => { const n = { ...a }; delete n[id]; return n; }), 280);

  const inspTarget = inspect ? inspect.stack[inspect.stack.length - 1].target : null;
  const liveInspTarget = inspTarget && inspect && inspect.stack.length === 1 ? morphs.find((m) => m.id === inspect.id) || inspTarget : inspTarget;

  return (
    <div className="world" onMouseDown={() => { setSelected(null); setSendFor(null); setMenuFor(null); }}>
      {/* toolbar */}
      <div className="world-bar" onMouseDown={stop}>
        <span className="mark"><b>Beam</b>talk</span>
        <div className="viewswitch">
          <a className="vs" href="Beamtalk Cockpit.html">Cockpit</a>
          <span className="vs on">Morphic</span>
        </div>
        <div className="attach"><span className="morph-live" /> attached <b>{IMG.workspace.node}</b></div>
        <span className="spacer" />
        <div className="palette">
          <span className="plabel">new morph</span>
          {palette.map((p) => (
            <button key={p.cls} className="pbtn" onClick={() => spawnMorph(p)}>
              <span className="glyph">+</span>{p.cls}
            </button>
          ))}
        </div>
      </div>

      {/* morphs */}
      {morphs.map((m) => {
        const a = anim[m.id];
        if (a === "pop") clearAnim(m.id);
        return (
          <React.Fragment key={m.id}>
            <MorphCard
              m={m} selected={selected === m.id} bump={bumpId === m.id}
              anim={a}
              onSelect={onSelect} onHeadDown={startDrag}
              onSend={(msg, roomText) => sendMessage(m.id, msg, roomText)}
            >
              {m.className === "Counter" ? <CounterBody m={m} bump={bumpId === m.id} onSend={(msg) => sendMessage(m.id, msg)} /> : null}
              {m.className === "Account" ? <AccountBody m={m} bump={bumpId === m.id} onSend={(msg) => sendMessage(m.id, msg)} /> : null}
              {m.className === "Room" ? <RoomBody m={m} onSend={(msg, txt) => sendMessage(m.id, msg, txt)} /> : null}
            </MorphCard>
            {(selected === m.id || tweaks.alwaysHalos) && a !== "poof" ? (
              <Halo morph={m}
                onAction={(action, e) => doAction(m.id, action, e)}
                onMoveDown={(e) => startDrag(e, m.id)}
                onResizeDown={(e) => startResize(e, m.id)}
              />
            ) : null}
            {sendFor === m.id ? (
              <SendBubble morph={m} onClose={() => setSendFor(null)} onSend={(msg) => { sendMessage(m.id, msg); setSendFor(null); }} />
            ) : null}
            {menuFor === m.id ? (
              <MorphMenu morph={m} onPick={(what) => { setMenuFor(null); if (what === "inspect") openInspector(m.id); else if (what === "browse" || what === "senders") { window.open("Beamtalk Cockpit.html", "_blank"); } }} />
            ) : null}
          </React.Fragment>
        );
      })}

      {/* floating inspector */}
      {inspect && liveInspTarget ? (
        <InspectorMorph
          stack={inspect.stack} target={liveInspTarget}
          onBack={() => setInspect((s) => ({ ...s, stack: s.stack.slice(0, -1) }))}
          onDrill={drillInspect}
          onClose={() => setInspect(null)}
        />
      ) : null}

      {/* world log */}
      <div className="world-log">
        {log.map((l, i) => <div key={l.t + "_" + i} className="wl"><span className="a">{l.binding}</span> {l.msg}</div>)}
      </div>
      <div className="world-hint">Click a morph for its halo · drag to move · poke it while it runs</div>

      <TweaksPanel title="Tweaks">
        <TweakSection label="World" />
        <TweakToggle label="Always show halos" value={tweaks.alwaysHalos} onChange={(v) => setTweak("alwaysHalos", v)} />
        <TweakSection label="Theme" />
        <TweakRadio label="Surface" value={tweaks.theme} options={["paper", "squeak", "dusk"]} onChange={(v) => setTweak("theme", v)} />
        <TweakColor label="Accent" value={tweaks.accent} options={["#b9711b", "#a8324e", "#2c6e8e", "#5d7a2e", "#7a4ea8"]} onChange={(v) => setTweak("accent", v)} />
        <TweakSection label="Type" />
        <TweakSelect label="Code font" value={tweaks.codeFont} options={["IBM Plex Mono", "JetBrains Mono", "Spline Sans Mono", "Courier Prime"]} onChange={(v) => setTweak("codeFont", v)} />
        <TweakSelect label="UI font" value={tweaks.uiFont} options={["Hanken Grotesk", "Inter Tight", "Public Sans", "Schibsted Grotesk"]} onChange={(v) => setTweak("uiFont", v)} />
      </TweaksPanel>
    </div>
  );
}

function SendBubble({ morph, onClose, onSend }) {
  const [msg, setMsg] = useState("");
  const ref = useRef(null);
  useEffect(() => { if (ref.current) ref.current.focus(); }, []);
  const go = () => { if (msg.trim()) onSend(msg.trim()); };
  return (
    <div className="send-bubble" style={{ left: morph.x + morph.w + 22, top: morph.y + morph.h / 2 - 20, zIndex: 90 }} onMouseDown={stop}>
      <span className="recv">{morph.binding} </span>
      <input ref={ref} placeholder="increment · incrementBy: 5 · deposit: 100" value={msg}
        onChange={(e) => setMsg(e.target.value)} onKeyDown={(e) => { if (e.key === "Enter") go(); if (e.key === "Escape") onClose(); }} />
      <button className="sb-go" onClick={go}>send</button>
    </div>
  );
}

function MorphMenu({ morph, onPick }) {
  return (
    <div className="morph-menu" style={{ left: morph.x + morph.w * 0.3, top: morph.y - 6, zIndex: 90 }} onMouseDown={stop}>
      <div className="mi" onClick={() => onPick("inspect")}>⌖ inspect <span className="mono">{morph.binding}</span></div>
      <div className="mi" onClick={() => onPick("browse")}>▤ browse class <span className="mono">{morph.className}</span></div>
      <div className="mi" onClick={() => onPick("senders")}>↗ senders of selectors…</div>
      <div className="sep" />
      <div className="mi" onClick={() => onPick("inspect")}>copy pid <span className="mono">{morph.pid}</span></div>
    </div>
  );
}

function InspectorMorph({ stack, target, onBack, onDrill, onClose }) {
  const fields = target.fields || {};
  const keys = Object.keys(fields);
  return (
    <div className="insp-morph" style={{ left: Math.min(window.innerWidth - 300, target.x + (target.w || 240) + 22), top: Math.max(60, (target.y || 120)) }} onMouseDown={stop}>
      <div className="imh">
        {stack.length > 1 ? <span className="imback" onClick={onBack}>‹ back</span> : <span style={{ color: "var(--accent)" }}>⌖</span>}
        <span className="t">{target.binding ? target.binding + " · " : ""}a {target.className}</span>
        <span className="x" onClick={onClose}>×</span>
      </div>
      <table>
        <tbody>
          {keys.length === 0 ? <tr><td className="v" style={{ color: "var(--faint)" }}>no fields</td></tr> : null}
          {keys.map((k) => {
            const f = fields[k];
            const drill = f.kind === "ref" && f.ref;
            const v = f.kind === "string" ? (/^".*"$/.test(f.value) ? f.value : '"' + f.value + '"') : f.value;
            return (
              <tr key={k} className={drill ? "drill" : ""} onClick={drill ? () => onDrill(f) : undefined}>
                <td className="k">{k}</td>
                <td className="v">{v}</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}

function shade(hex, pct) {
  const h = hex.replace("#", "");
  const n = parseInt(h.length === 3 ? h.split("").map((c) => c + c).join("") : h, 16);
  let r = (n >> 16) & 255, g = (n >> 8) & 255, b = n & 255;
  const f = pct / 100;
  const adj = (c) => Math.max(0, Math.min(255, Math.round(c + (f < 0 ? c * f : (255 - c) * f))));
  return "#" + ((1 << 24) + (adj(r) << 16) + (adj(g) << 8) + adj(b)).toString(16).slice(1);
}

ReactDOM.createRoot(document.getElementById("root")).render(<App />);
