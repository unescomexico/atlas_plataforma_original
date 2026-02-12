// Red "neuronal" (force graph) — técnicas vs manufactura
// Dataset: ../csv/data_by_technique_app.csv (ajusta si cambias ubicación)
const CSV_PATH = "../csv/data_by_technique_app.csv";

// columnas de manufactura (conteos)
const MAN_COLS = [
  ["n_man_mano", "Mano"],
  ["n_man_pedal", "Pedal"],
  ["n_man_telar", "Telar"],
  ["n_man_mixta", "Mixta"],
  ["n_man_otra", "Otra"],
  ["n_man_tejido", "Tejido"],
  ["n_man_telar_pedal", "Telar pedal"]
];

// ---------- Utils ----------
const norm = (v) => (v ?? "").toString().trim();

// Fix mojibake típico (MichoacÃ¡n -> Michoacán)
function fixMojibake(s){
  s = norm(s);
  if (!s) return "";
  if (!/[ÃÂ�]/.test(s)) return s;
  try{
    // convierte texto latin1-mal-leído a UTF-8 correcto
    return decodeURIComponent(escape(s));
  }catch{
    return s;
  }
}

function stripDiacritics(s){
  return norm(s).normalize("NFD").replace(/[\u0300-\u036f]/g, "");
}
function normKey(s){
  return stripDiacritics(fixMojibake(s)).toLowerCase();
}

function splitEstados(cell){
  const s = fixMojibake(cell);
  if (!s) return [];
  return s
    .split(",")
    .map(x => fixMojibake(x).trim())
    .filter(Boolean);
}

function toNum(v){
  const s = norm(v).replace(",", ".");
  const n = Number(s);
  return Number.isFinite(n) ? n : 0;
}

// ---------- DOM ----------
const $ = (id) => document.getElementById(id);

const fEstado = $("fEstado");
const qTec = $("qTec");
const resetBtn = $("resetBtn");
const reloadBtn = $("reloadBtn");

const kpiTec = $("kpiTec");
const kpiLinks = $("kpiLinks");
const kpiMods = $("kpiMods");

const graphEl = $("graph");
const tt = $("tooltip");
const errBox = $("errBox");
const errMsg = $("errMsg");

function showError(msg){
  errBox.classList.remove("hidden");
  errMsg.textContent = msg;
}
function hideError(){
  errBox.classList.add("hidden");
  errMsg.textContent = "";
}

// ---------- State ----------
let RAW = [];
let svg, gRoot, gLinks, gNodes, sim, zoom;
let selectedId = null;

function fillSelect(sel, values, allLabel="Todos"){
  sel.innerHTML = `<option value="">${allLabel}</option>`;
  values.forEach(v=>{
    const o = document.createElement("option");
    o.value = v;
    o.textContent = v;
    sel.appendChild(o);
  });
}

// ---------- Build graph ----------
function buildGraph(rows){
  // technique nodes
  const techMap = new Map(); // id -> node
  const manNodes = MAN_COLS.map(([,label]) => ({
    id: `man:${label}`,
    type: "manufactura",
    label,
    size: 20
  }));

  // links
  const links = [];
  for (const r of rows){
    const tec = fixMojibake(r["tecnica_norm"]);
    if (!tec) continue;

    const id = `tec:${tec}`;
    if (!techMap.has(id)){
      techMap.set(id, {
        id,
        type: "tecnica",
        label: tec,
        // agregado: total de manufacturas reportadas (suma de conteos)
        total: 0,
        size: 7
      });
    }
    const node = techMap.get(id);

    for (const [col, manLabel] of MAN_COLS){
      const w = toNum(r[col]);
      if (w <= 0) continue;
      node.total += w;
      links.push({
        source: id,
        target: `man:${manLabel}`,
        value: w
      });
    }
  }

  // node sizing: usa log(total) para destacar técnicas con más registros
  for (const n of techMap.values()){
    const t = n.total || 1;
    n.size = 6 + Math.min(22, Math.log1p(t) * 6);
  }
  // manufactura sizing: por suma de pesos entrantes
  const manTotals = new Map(manNodes.map(n => [n.id, 0]));
  for (const l of links) manTotals.set(l.target, (manTotals.get(l.target) || 0) + l.value);
  for (const n of manNodes){
    const t = manTotals.get(n.id) || 1;
    n.size = 10 + Math.min(26, Math.log1p(t) * 6);
  }

  const nodes = [...manNodes, ...techMap.values()];
  return { nodes, links };
}

function applyFilters(){
  const selE = fEstado.value;
  const q = norm(qTec.value).toLowerCase();

  return RAW.filter(r => {
    // filtro estado
    if (selE){
      const estados = splitEstados(r["estados"]);
      // compara sin acentos/diacríticos para robustez
      const has = estados.some(e => normKey(e) === normKey(selE));
      if (!has) return false;
    }
    // filtro búsqueda técnica
    if (q){
      const tec = fixMojibake(r["tecnica_norm"]).toLowerCase();
      if (!tec.includes(q)) return false;
    }
    return true;
  });
}

// ---------- Render graph ----------
function initSvg(){
  graphEl.innerHTML = "";
  const { width, height } = graphEl.getBoundingClientRect();

  svg = d3.select(graphEl)
    .append("svg")
    .attr("width", "100%")
    .attr("height", "100%")
    .attr("viewBox", [0, 0, width, height].join(" "))
    .attr("role", "img");

  gRoot = svg.append("g");
  gLinks = gRoot.append("g").attr("class", "links");
  gNodes = gRoot.append("g").attr("class", "nodes");

  zoom = d3.zoom()
    .scaleExtent([0.2, 6])
    .on("zoom", (ev) => gRoot.attr("transform", ev.transform));

  svg.call(zoom);

  // reset selection on background click
  svg.on("click", () => {
    if (d3.event) d3.event.stopPropagation?.();
  });
}

function render({nodes, links}){
  const { width, height } = graphEl.getBoundingClientRect();
  svg.attr("viewBox", [0, 0, width, height].join(" "));

  // Links
  const linkSel = gLinks.selectAll("line")
    .data(links, d => `${d.source}->${d.target}`);

  linkSel.exit().remove();

  const linkEnter = linkSel.enter().append("line")
    .attr("stroke", "rgba(255,255,255,.10)")
    .attr("stroke-width", d => 1 + Math.min(6, Math.sqrt(d.value)));

  const link = linkEnter.merge(linkSel);

  // Nodes
  const nodeSel = gNodes.selectAll("g.node")
    .data(nodes, d => d.id);

  nodeSel.exit().remove();

  const nodeEnter = nodeSel.enter().append("g")
    .attr("class", "node")
    .style("cursor", "grab")
    .call(d3.drag()
      .on("start", (ev, d) => {
        if (!ev.active) sim.alphaTarget(0.25).restart();
        d.fx = d.x; d.fy = d.y;
      })
      .on("drag", (ev, d) => {
        d.fx = ev.x; d.fy = ev.y;
      })
      .on("end", (ev, d) => {
        if (!ev.active) sim.alphaTarget(0);
        d.fx = null; d.fy = null;
      })
    );

  nodeEnter.append("circle")
    .attr("r", d => d.size)
    .attr("fill", d => d.type === "manufactura" ? "rgba(93,214,199,.92)" : "rgba(122,162,255,.92)")
    .attr("stroke", "rgba(255,255,255,.18)")
    .attr("stroke-width", 1);

  // etiquetas solo para manufacturas (siempre)
  nodeEnter.filter(d => d.type === "manufactura").append("text")
    .attr("dy", d => -(d.size + 10))
    .attr("text-anchor", "middle")
    .attr("font-size", 12)
    .attr("fill", "rgba(233,238,245,.92)")
    .text(d => d.label);

  // Interacciones
  nodeEnter
    .on("mouseenter", (ev, d) => showTooltip(ev, d, links))
    .on("mousemove", (ev) => moveTooltip(ev))
    .on("mouseleave", () => hideTooltip())
    .on("click", (ev, d) => {
      ev.stopPropagation();
      selectedId = (selectedId === d.id) ? null : d.id;
      updateHighlight(nodes, links);
    });

  const node = nodeEnter.merge(nodeSel);

  // Simulation
  if (sim) sim.stop();

  sim = d3.forceSimulation(nodes)
    .force("link", d3.forceLink(links).id(d => d.id).distance(d => d.source.type === "manufactura" || d.target.type === "manufactura" ? 140 : 110).strength(0.18))
    .force("charge", d3.forceManyBody().strength(d => d.type === "manufactura" ? -900 : -260))
    .force("center", d3.forceCenter(width/2, height/2))
    .force("collide", d3.forceCollide().radius(d => d.size + 6).iterations(2))
    .on("tick", () => {
      link
        .attr("x1", d => d.source.x)
        .attr("y1", d => d.source.y)
        .attr("x2", d => d.target.x)
        .attr("y2", d => d.target.y);

      node.attr("transform", d => `translate(${d.x},${d.y})`);
    });

  // KPIs
  const techCount = nodes.filter(n => n.type === "tecnica").length;
  kpiTec.textContent = techCount.toLocaleString("es-MX");
  kpiLinks.textContent = links.length.toLocaleString("es-MX");
  kpiMods.textContent = nodes.filter(n => n.type === "manufactura").length.toLocaleString("es-MX");

  selectedId = null;
  updateHighlight(nodes, links);
  hideTooltip();
}

function updateHighlight(nodes, links){
  // build adjacency
  const neigh = new Set();
  if (selectedId){
    links.forEach(l => {
      const s = (typeof l.source === "string") ? l.source : l.source.id;
      const t = (typeof l.target === "string") ? l.target : l.target.id;
      if (s === selectedId) neigh.add(t);
      if (t === selectedId) neigh.add(s);
    });
  }

  gLinks.selectAll("line")
    .attr("stroke", d => {
      if (!selectedId) return "rgba(255,255,255,.10)";
      const s = (typeof d.source === "string") ? d.source : d.source.id;
      const t = (typeof d.target === "string") ? d.target : d.target.id;
      return (s === selectedId || t === selectedId) ? "rgba(255,255,255,.35)" : "rgba(255,255,255,.05)";
    })
    .attr("stroke-width", d => {
      const base = 1 + Math.min(6, Math.sqrt(d.value));
      if (!selectedId) return base;
      const s = (typeof d.source === "string") ? d.source : d.source.id;
      const t = (typeof d.target === "string") ? d.target : d.target.id;
      return (s === selectedId || t === selectedId) ? base + 1 : Math.max(1, base - 0.5);
    });

  gNodes.selectAll("g.node circle")
    .attr("opacity", d => {
      if (!selectedId) return 1;
      return (d.id === selectedId || neigh.has(d.id)) ? 1 : 0.22;
    })
    .attr("stroke-width", d => (d.id === selectedId ? 2.2 : 1));
}

// ---------- Tooltip ----------
function showTooltip(ev, d, links){
  const isTec = d.type === "tecnica";
  let extra = "";
  if (isTec){
    const conns = links.filter(l => {
      const s = (typeof l.source === "string") ? l.source : l.source.id;
      const t = (typeof l.target === "string") ? l.target : l.target.id;
      return s === d.id || t === d.id;
    });
    const parts = conns
      .map(l => {
        const other = (typeof l.target === "object" && l.target.id && l.source.id === d.id) ? l.target.id :
                      (typeof l.source === "object" && l.source.id && l.target.id === d.id) ? l.source.id :
                      (typeof l.source === "string" && l.source === d.id) ? l.target : l.source;
        const label = other.replace(/^man:/, "");
        return `${label}: ${l.value}`;
      })
      .sort((a,b)=> {
        const av = Number(a.split(": ").pop()); const bv = Number(b.split(": ").pop());
        return bv - av;
      })
      .slice(0, 6)
      .join("<br/>");
    extra = `<div style="margin-top:6px;"><b>Manufactura:</b><br/>${parts || "-"}</div>`;
    extra += `<div class="muted" style="margin-top:6px;">Total (suma conteos): ${Math.round(d.total || 0)}</div>`;
  } else {
    const conns = links.filter(l => {
      const s = (typeof l.source === "string") ? l.source : l.source.id;
      const t = (typeof l.target === "string") ? l.target : l.target.id;
      return s === d.id || t === d.id;
    });
    extra = `<div class="muted" style="margin-top:6px;">Conectada con ${conns.length} técnicas.</div>`;
  }

  tt.innerHTML = `<div><b>${d.label}</b></div>${extra}`;
  tt.classList.remove("hidden");
  moveTooltip(ev);
}

function moveTooltip(ev){
  const pad = 14;
  const rect = graphEl.getBoundingClientRect();
  const x = ev.clientX - rect.left + pad;
  const y = ev.clientY - rect.top + pad;

  tt.style.left = `${x}px`;
  tt.style.top = `${y}px`;
}

function hideTooltip(){
  tt.classList.add("hidden");
}

// ---------- Load CSV ----------
function loadCSV(){
  hideError();
  reloadBtn.disabled = true;
  reloadBtn.textContent = "Cargando…";

  return new Promise((resolve, reject) => {
    Papa.parse(CSV_PATH, {
      download: true,
      header: true,
      skipEmptyLines: true,
      complete: (res) => resolve(res.data || []),
      error: (err) => reject(err)
    });
  }).finally(() => {
    reloadBtn.disabled = false;
    reloadBtn.textContent = "Recargar datos";
  });
}

function refresh(){
  const rows = applyFilters();
  const graph = buildGraph(rows);
  render(graph);
}

function resetView(){
  if (!svg || !zoom) return;
  svg.transition().duration(250).call(zoom.transform, d3.zoomIdentity);
}

async function init(){
  try{
    initSvg();
    const rows = await loadCSV();
    if (!rows.length) throw new Error("El CSV cargó pero no tiene filas.");
    RAW = rows;

    // estados únicos (descompone por comas)
    const st = new Map(); // normKey -> display
    for (const r of rows){
      for (const e of splitEstados(r["estados"])){
        const k = normKey(e);
        if (!k) continue;
        if (!st.has(k)) st.set(k, fixMojibake(e));
      }
    }
    const estados = Array.from(st.values()).sort((a,b)=>a.localeCompare(b, "es"));
    fillSelect(fEstado, estados, "Todos");

    refresh();
  }catch(err){
    console.error(err);
    showError(err.message || String(err));
  }
}

// Events
reloadBtn.addEventListener("click", init);
fEstado.addEventListener("change", refresh);
qTec.addEventListener("input", () => {
  clearTimeout(window.__tQ);
  window.__tQ = setTimeout(refresh, 120);
});
resetBtn.addEventListener("click", resetView);

document.addEventListener("keydown", (ev) => {
  if (ev.key === "Escape"){
    selectedId = null;
    updateHighlight([], []);
    // redraw current state properly
    refresh();
  }
});

// Handle resize (re-init SVG viewBox + forces)
window.addEventListener("resize", () => {
  if (!RAW.length) return;
  initSvg();
  refresh();
});

// run
init();
