/* inspector.jsx — live Bindings pane + Object Inspector that follows references.
   Bindings are the attached session's variables (Tier 3); object-valued ones
   carry a pid and can be inspected. The Inspector drills into a value's fields,
   and any field that is itself an object reference can be followed one level
   deeper — the whole point of carrying live terms, not JSON, to the browser. */

function BindingsList({ objects, selectedId, onSelect, role, fill }) {
  return (
    <div className="panel" style={fill ? { flex: 1 } : { flex: "none", maxHeight: "38%" }}>
      <div className="panel-head">
        Bindings
        <span className="spacer" />
        <span className="count">{objects.length} in session</span>
      </div>
      <div className="panel-body">
        <div className="obj-list">
          {objects.map((o) => (
            <div key={o.id} className={"obj-row" + (selectedId === o.id ? " sel" : "")} onClick={() => onSelect(o)}>
              <span className="bname mono">{o.binding}</span>
              <span className="bassign mono">:=</span>
              <span className="ps mono">{o.printString}</span>
              {o.pid ? <span className={"dot" + (o.ticking ? " live" : "")} title={o.status} /> : null}
              {o.pid ? <span className="pid">{o.pid}</span> : <span className="pid faintkind">{o.className}</span>}
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}

function valueClass(kind) {
  return "v " + (["string", "int", "symbol", "pid", "ref", "bool"].includes(kind) ? kind : "");
}
function renderVal(field) {
  if (field.kind === "string") {
    // already-quoted strings (from refs) pass through; otherwise quote
    return /^".*"$/.test(field.value) ? field.value : '"' + field.value + '"';
  }
  return field.value;
}

// shared inner content for both the docked panel and the floating window
function InspectorContent({ target, crumbs, onDrill, onCrumb, onPoke, quickPokes, flashKey, role }) {
  const fields = target.fields || {};
  const keys = Object.keys(fields);
  const readOnly = role === "observer";
  // flash a value cell when the live-tracked value changes (the "subscribed" tell)
  const prev = React.useRef({});
  const flashed = {};
  keys.forEach((k) => {
    const v = fields[k] && fields[k].value;
    if (prev.current[k] !== undefined && prev.current[k] !== v) flashed[k] = true;
    prev.current[k] = v;
  });
  return (
    <>
      <div className="insp-head">
        {crumbs && crumbs.length > 1 ? (
          <div className="insp-crumbs">
            {crumbs.map((c, i) => (
              <React.Fragment key={i}>
                {i > 0 ? <span className="sep">›</span> : null}
                <span className="c" onClick={() => onCrumb(i)}>{c.label}</span>
              </React.Fragment>
            ))}
          </div>
        ) : null}
        <div className="ps">{target.printString}</div>
        <div className="proc-chips">
          <span className="chip">class <b>{target.className}</b></span>
          {target.pid ? <span className="chip">pid <b>{target.pid}</b></span> : null}
          {target.status ? <span className="chip"><span className="dot" style={{ width: 6, height: 6 }} /> {target.status}</span> : null}
          {typeof target.mailbox === "number" ? <span className="chip">mailbox <b>{target.mailbox}</b></span> : null}
          {target.reductions ? <span className="chip">reductions <b>{target.reductions.toLocaleString()}</b></span> : null}
        </div>
      </div>

      <div className="panel-body" key={flashKey}>
        <table className="ivar-table">
          <tbody>
            {keys.length === 0 ? (
              <tr><td className="empty" colSpan={3}>No fields — this is a {target.className}.</td></tr>
            ) : null}
            {keys.map((k) => {
              const f = fields[k];
              const drillable = f.kind === "ref" && f.ref;
              return (
                <tr key={k} className={drillable ? "drillable" : ""} onClick={drillable ? () => onDrill(k, f) : undefined}>
                  <td className="k">{k}</td>
                  <td className={valueClass(f.kind) + (flashed[k] ? " vflash" : "")}>{renderVal(f)}</td>
                  <td className="follow">{drillable ? <span className="follow-link">follow →</span> : null}</td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>

      {target.pid ? (
        <div className="poke">
          <div className="poke-label">
            {readOnly ? "Read-only — Observer role can't send" : "Send a message to " + target.pid}
          </div>
          <PokeBar onPoke={onPoke} disabled={readOnly} />
          {!readOnly && quickPokes && quickPokes.length ? (
            <div className="poke-quick">
              {quickPokes.map((q) => (
                <button key={q} className="qbtn" onClick={() => onPoke(q)}>{q}</button>
              ))}
            </div>
          ) : null}
        </div>
      ) : null}
    </>
  );
}

function Inspector(props) {
  const { target } = props;
  if (!target) {
    return (
      <div className="panel insp" style={{ flex: 1 }}>
        <div className="panel-head">Inspector</div>
        <div className="empty">
          Select a binding above, or <b>Inspect&nbsp;it</b> on a Workspace expression,
          to drill into a live value and follow its references while it runs.
        </div>
      </div>
    );
  }
  return (
    <div className="panel insp" style={{ flex: 1 }}>
      <div className="panel-head">
        Inspector
        <span className="spacer" />
        {target.pid ? <span className="count">following references</span> : null}
      </div>
      <InspectorContent {...props} />
    </div>
  );
}

// floating, draggable inspector window (overlay mode) — Smalltalk-style: open
// as many as you like, drag them around, follow references inside each.
function InspectorWindow(props) {
  const { win, onClose, onDragStart, onFocus, trackable, tracking, frozen, onToggleFreeze } = props;
  const t = win.stack[win.stack.length - 1].target;
  const crumbs = win.stack.map((s) => ({ label: s.label }));
  return (
    <div className="insp-window" style={{ left: win.x, top: win.y, zIndex: win.z }} onMouseDown={() => onFocus(win.id)}>
      <div className="iw-bar" onMouseDown={(e) => onDragStart(e, win.id)}>
        <span className="iw-dot" onMouseDown={(e) => { e.stopPropagation(); onClose(win.id); }} title="close" />
        <span className="iw-title mono">{t.printString}</span>
        <span className="iw-spacer" />
        {trackable ? (
          <button
            className={"iw-freeze " + (frozen ? "frozen" : "live")}
            title={frozen ? "Frozen snapshot — click to track changed: live" : "Tracking live (subscribed to changed:) — click to freeze a snapshot"}
            onMouseDown={(e) => e.stopPropagation()}
            onClick={(e) => { e.stopPropagation(); onToggleFreeze(); }}
          >
            <span className="iwf-dot" />{frozen ? "frozen" : "live"}
          </button>
        ) : null}
        <span className="iw-x" onMouseDown={(e) => { e.stopPropagation(); onClose(win.id); }}>×</span>
      </div>
      <InspectorContent {...props} target={t} crumbs={crumbs} />
    </div>
  );
}

function PokeBar({ onPoke, disabled }) {
  const [msg, setMsg] = React.useState("");
  const send = () => { if (!disabled && msg.trim()) { onPoke(msg.trim()); setMsg(""); } };
  return (
    <div className="poke-row">
      <span className="mono" style={{ color: disabled ? "var(--faint)" : "var(--accent)", fontWeight: 700 }}>‹recv›</span>
      <input
        className="mono"
        placeholder={disabled ? "disabled" : "increment   ·   incrementBy: 10"}
        value={msg}
        disabled={disabled}
        onChange={(e) => setMsg(e.target.value)}
        onKeyDown={(e) => { if (e.key === "Enter") send(); }}
      />
      <button className="btn primary" onClick={send} disabled={disabled}>Send</button>
    </div>
  );
}

Object.assign(window, { BindingsList, Inspector, InspectorWindow });
