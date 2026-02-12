// Dashboard | Atlas de Técnicas Textiles
const CSV_PATH = "../csv/data_by_record_app.csv";

// ---------- Utils ----------
const norm = (v) => (v ?? "").toString().trim();
const lower = (v) => norm(v).toLowerCase();

const isYes = (v) => {
  const s = lower(v);
  return ["si","sí","1","true","x","yes"].includes(s);
};

const uniqCount = (arr) => new Set(arr.map(norm).filter(Boolean)).size;

function sumBoolCol(rows, col){
  let s = 0;
  for (const r of rows){
    const v = r[col];
    if (isYes(v) || norm(v) === "1") s += 1;
  }
  return s;
}

// Normaliza tokens (minúsculas, sin acentos, singular básico)
function stripDiacritics(s){
  return s.normalize("NFD").replace(/[\u0300-\u036f]/g, "");
}

const STOP = new Set(["de","del","la","las","el","los","y","e","para","con","sin"]);

function singularizeWord(w){
  if (!w) return w;
  if (STOP.has(w)) return w;
  if (w.length > 4 && w.endsWith("es")) return w.slice(0, -2);
  if (w.length > 3 && w.endsWith("s")) return w.slice(0, -1);
  return w;
}

function normalizeToken(raw){
  let s = stripDiacritics(lower(raw));
  s = s.replace(/[^a-z0-9\s_-]/g, " ");
  s = s.replace(/[_-]+/g, " ").replace(/\s+/g, " ").trim();
  if (!s) return "";
  const words = s.split(" ").map(singularizeWord).filter(Boolean);
  return words.join(" ").trim();
}

function splitTerms(cell){
  const s = norm(cell);
  if (!s) return [];
  return s
    .split(/[;,\n\r\t\|\/]+/g)
    .map(x => x.trim())
    .filter(Boolean);
}

function countTermsFromColumn(rows, col){
  const map = new Map();
  for (const r of rows){
    for (const t of splitTerms(r[col])){
      const key = normalizeToken(t);
      if (!key) continue;
      map.set(key, (map.get(key) || 0) + 1);
    }
  }
  return map;
}

function countTermsFromColumns(rows, cols){
  const map = new Map();
  for (const r of rows){
    for (const c of cols){
      for (const t of splitTerms(r[c])){
        const key = normalizeToken(t);
        if (!key) continue;
        map.set(key, (map.get(key) || 0) + 1);
      }
    }
  }
  return map;
}

function topEntries(map, limit=45){
  return Array.from(map.entries())
    .sort((a,b)=>b[1]-a[1])
    .slice(0, limit);
}

// ---------- DOM ----------
const $ = (id) => document.getElementById(id);

const elErr = $("errBox");
const elErrMsg = $("errMsg");

const elTec = $("kpiTecnicas");
const elEst = $("kpiEstados");
const elMun = $("kpiMunicipios");
const elLen = $("kpiLenguas");
const elNotaTenido = $("tenidoNota");

const reloadBtn = $("reloadBtn");
const fEstado = $("fEstado");
const fLengua = $("fLengua");

// Modal
const modal = $("modal");
const modalBg = $("modalBg");
const modalClose = $("modalClose");
const modalTitle = $("modalTitle");
const modalDesc = $("modalDesc");
const modalCanvas = $("modalCanvas");

// ---------- Charts registry (para re-render en modal) ----------
let charts = [];
const chartFactories = new Map(); // canvasId -> () => Chart config
const chartMeta = new Map();      // canvasId -> {title, desc}

function destroyCharts(){
  charts.forEach(ch => ch?.destroy?.());
  charts = [];
  chartFactories.clear();
  chartMeta.clear();
}

function showError(msg){
  elErr.classList.remove("hidden");
  elErrMsg.textContent = msg;
}

function hideError(){
  elErr.classList.add("hidden");
  elErrMsg.textContent = "";
}

function mkDonutConfig(labels, values){
  return {
    type: "doughnut",
    data: { labels, datasets: [{ data: values, borderWidth: 0 }] },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      plugins: { legend: { position: "bottom", labels: { color: "#e9eef5" } } }
    }
  };
}

function mkBarConfig(labels, values){
  return {
    type: "bar",
    data: { labels, datasets: [{ data: values, borderWidth: 0 }] },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      plugins: { legend: { display: false } },
      scales: {
        x: { ticks: { color: "#a9b4c2" }, grid: { color: "rgba(255,255,255,.06)" } },
        y: { ticks: { color: "#a9b4c2" }, grid: { color: "rgba(255,255,255,.06)" } }
      }
    }
  };
}

function mkTreemapConfig(entries, labelTopN=12){
  const tree = entries.map(([name, v]) => ({ name, v }));
  // cutoff: muestra etiquetas solo para los "más frecuentes"
  const cutoff = entries.length ? entries[Math.min(labelTopN - 1, entries.length - 1)][1] : Infinity;

  return {
    type: "treemap",
    data: {
      datasets: [{
        tree,
        key: "v",
        groups: ["name"],
        spacing: 1,
        borderWidth: 1,
        borderColor: "rgba(255,255,255,.08)",
        backgroundColor: (ctx) => {
          if (ctx.type !== "data") return "transparent";
          const value = ctx.raw.v;
          const alpha = Math.max(0.18, Math.min(0.75, (1 + Math.log(value)) / 5));
          return `rgba(93, 214, 199, ${alpha})`;
        },
        // Labels nativos del plugin treemap (mejor que dibujarlos a mano)
        labels: {
          display: true,
          align: "center",
          position: "middle",
          padding: 3,
          overflow: "hidden",          // si no cabe, se oculta
          color: ["#e9eef5", "rgba(233,238,245,.85)"],
          font: [
            { size: 12, weight: "700" },
            { size: 11, weight: "600" }
          ],
          formatter: (ctx) => {
            // ctx.raw.v = conteo; ctx.raw._data = objeto original de tree
            const v = ctx.raw?.v ?? 0;
            if (v < cutoff) return ""; // solo topN
            const obj = ctx.raw?._data;
            const name = (obj?.name ?? "").toString();
            if (!name) return `${v}`;
            return [name, `${v}`]; // 2 líneas: término y conteo
          }
        },
        // captions opcionales (desactivados por defecto)
      }]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        legend: { display: false },
        tooltip: {
          callbacks: {
            title: (items) => {
              const raw = items?.[0]?.raw;
              const obj = raw?._data;
              return (obj?.name ?? raw?.g ?? "") || "";
            },
            label: (item) => `Frecuencia: ${item.raw.v}`
          }
        }
      }
    }
  };
}

function mountChart(canvasId, title, desc, configFactory){
  const canvas = $(canvasId);
  if (!canvas) return;

  const cfg = configFactory();
  const chart = new Chart(canvas, cfg);
  charts.push(chart);

  chartFactories.set(canvasId, configFactory);
  chartMeta.set(canvasId, { title, desc });
}

// ---------- Data + Filters ----------
let RAW = [];

function fillSelect(sel, values, allLabel){
  sel.innerHTML = `<option value="">${allLabel}</option>`;
  values.forEach(v=>{
    const o = document.createElement("option");
    o.value = v;
    o.textContent = v;
    sel.appendChild(o);
  });
}

function applyFilters(){
  const sE = fEstado.value;
  const sL = fLengua.value;
  return RAW.filter(r => {
    if (sE && norm(r["Estado"]) !== sE) return false;
    if (sL && norm(r["Lengua"]) !== sL) return false;
    return true;
  });
}

// ---------- Compute + Render ----------
function renderDashboard(rows){
  hideError();
  destroyCharts();

  // KPIs
  elTec.textContent = uniqCount(rows.map(r => r["Tecnica"])).toLocaleString("es-MX");
  elEst.textContent = uniqCount(rows.map(r => r["Estado"])).toLocaleString("es-MX");
  elMun.textContent = uniqCount(rows.map(r => r["Municipio"])).toLocaleString("es-MX");
  elLen.textContent = uniqCount(rows.map(r => r["Lengua"])).toLocaleString("es-MX");

  // Género
  const gMap = new Map();
  for (const r of rows){
    const k = lower(r["Genero"]);
    const lbl = (k === "mujer") ? "Mujer" : (k === "hombre") ? "Hombre" : "No especifica";
    gMap.set(lbl, (gMap.get(lbl) || 0) + 1);
  }
  const gEntries = Array.from(gMap.entries()).sort((a,b)=>b[1]-a[1]);
  mountChart("chGenero", "Participantes por género",
    "Distribución de registros según el género reportado.",
    () => mkDonutConfig(gEntries.map(d=>d[0]), gEntries.map(d=>d[1]))
  );

  // Manufactura
  const manufCols = ["Mano","Pedal","Telar","Mixta","Otra","Tejido","Telar_pedal"];
  const manufLabels = ["Mano","Pedal","Telar","Mixta","Otra","Tejido","Telar pedal"];
  const manufVals = manufCols.map(c => sumBoolCol(rows, c));
  mountChart("chManufactura", "Manufactura",
    "Frecuencia de modalidades de manufactura reportadas en los registros.",
    () => mkBarConfig(manufLabels, manufVals)
  );

  // Teñido por técnica
  const tecTen = new Map();
  for (const r of rows){
    const tec = norm(r["Tecnica"]);
    if (!tec) continue;
    const yes = isYes(r["Tenido"]);
    if (!tecTen.has(tec)) tecTen.set(tec, !!yes);
    else if (yes) tecTen.set(tec, true);
  }
  let con = 0, sin = 0;
  for (const yes of tecTen.values()) (yes ? con++ : sin++);
  mountChart("chTenidoTec", "Teñido (técnicas)",
    "Proporción de técnicas que reportan teñido en al menos un registro.",
    () => mkDonutConfig(["Con teñido","Sin teñido"], [con, sin])
  );
  elNotaTenido.textContent = `Base: ${tecTen.size.toLocaleString("es-MX")} técnicas (si alguna fila tiene Tenido = Sí, cuenta como “con teñido”).`;

  // Tipo de teñido
  const tCols = ["Plantas","Minerales","Animales","Otro"];
  const tLabels = ["Plantas","Minerales","Animales","Otro"];
  const tVals = tCols.map(c => sumBoolCol(rows, c));
  mountChart("chTipoTenido", "Tipo de teñido",
    "Materiales/insumos de teñido mencionados en los registros.",
    () => mkBarConfig(tLabels, tVals)
  );

  // Aprendizaje
  const aCols = ["Madre","Abuela","Tia","Hermana","Cunada","Instructora","Aprendi_otro","Padre"];
  const aLabels = ["Madre","Abuela","Tía","Hermana","Cuñada","Instructora","Otro","Padre"];
  const aVals = aCols.map(c => sumBoolCol(rows, c));
  mountChart("chAprendizaje", "Aprendizaje más frecuente",
    "Principales vías de aprendizaje reportadas (familia, instructora u otras).",
    () => mkBarConfig(aLabels, aVals)
  );

  // Transmisión
  const trCols = ["No_ensenado","Hijas","Nietos","Sobrinos","Pareja","Estudiantes","Enseno_otra","Hijos"];
  const trLabels = ["No enseñado","Hijas","Nietos","Sobrinos","Pareja","Estudiantes","Otro","Hijos"];
  const trVals = trCols.map(c => sumBoolCol(rows, c));
  mountChart("chTransmision", "Transmisión",
    "A quién se transmite la técnica según los registros.",
    () => mkBarConfig(trLabels, trVals)
  );

  // Personas que transmiten (Sí/No)
  const transmitCols = ["Hijas","Nietos","Sobrinos","Pareja","Estudiantes","Enseno_otra","Hijos"];
  let yesTx = 0;
  for (const r of rows){
    const anyYes = transmitCols.some(c => isYes(r[c]) || norm(r[c]) === "1");
    if (anyYes) yesTx += 1;
  }
  const noTx = Math.max(0, rows.length - yesTx);
  mountChart("chTransmiten", "Personas que transmiten la técnica",
    "Registros con evidencia de transmisión (al menos un “Sí”) vs. sin evidencia.",
    () => mkDonutConfig(["Sí transmiten","No transmiten"], [yesTx, noTx])
  );

  // Treemaps
  const matEntries = topEntries(countTermsFromColumn(rows, "Materiales"), 45);
  mountChart("tmMateriales", "Materiales más utilizados",
    "Términos más mencionados en “Materiales” (normalizados para agrupar singular/plural).",
    () => mkTreemapConfig(matEntries, 16)
  );

  const prCols = ["pr1","pr2","pr3","pr4","pr5","pr6"];
  const prEntries = topEntries(countTermsFromColumns(rows, prCols), 45);
  mountChart("tmPracticas", "Prácticas más mencionadas",
    "Términos más frecuentes en pr1…pr6 (normalizados para agrupar variantes).",
    () => mkTreemapConfig(prEntries, 16)
  );

  const cCols = ["c1","c2","c3","c4","c5","c6","c7","c8"];
  const cEntries = topEntries(countTermsFromColumns(rows, cCols), 45);
  mountChart("tmCeremonias", "Ceremonias más mencionadas",
    "Términos más frecuentes en c1…c8 (normalizados para agrupar variantes).",
    () => mkTreemapConfig(cEntries, 16)
  );

  wireExpandButtons();
}

// ---------- Expand modal ----------
let modalChart = null;

function openModalFor(canvasId){
  const factory = chartFactories.get(canvasId);
  const meta = chartMeta.get(canvasId);
  if (!factory) return;

  // destroy previous
  if (modalChart) { modalChart.destroy(); modalChart = null; }

  modalTitle.textContent = meta?.title || "Gráfica";
  modalDesc.textContent = meta?.desc || "";

  // Create a fresh chart on modal canvas using same factory
  const cfg = factory();
  modalChart = new Chart(modalCanvas, cfg);

  modal.classList.remove("hidden");
  modal.setAttribute("aria-hidden", "false");
  document.body.style.overflow = "hidden";

  // ensure resize
  setTimeout(()=> modalChart?.resize?.(), 0);
}

function closeModal(){
  modal.classList.add("hidden");
  modal.setAttribute("aria-hidden", "true");
  document.body.style.overflow = "";
  if (modalChart) { modalChart.destroy(); modalChart = null; }
}

function wireExpandButtons(){
  document.querySelectorAll(".expandBtn").forEach(btn=>{
    btn.onclick = () => openModalFor(btn.dataset.target);
  });
}

modalBg.addEventListener("click", closeModal);
modalClose.addEventListener("click", closeModal);
document.addEventListener("keydown", (ev)=>{
  if (ev.key === "Escape" && !modal.classList.contains("hidden")) closeModal();
});

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
  if (!rows.length){
    renderDashboard([]); 
    return;
  }
  renderDashboard(rows);
}

async function init(){
  try{
    const rows = await loadCSV();
    if (!rows.length) throw new Error("El CSV cargó pero no tiene filas.");
    RAW = rows;

    const estados = Array.from(new Set(rows.map(r => norm(r["Estado"])).filter(Boolean))).sort((a,b)=>a.localeCompare(b,"es"));
    const lenguas = Array.from(new Set(rows.map(r => norm(r["Lengua"])).filter(Boolean))).sort((a,b)=>a.localeCompare(b,"es"));
    fillSelect(fEstado, estados, "Todos");
    fillSelect(fLengua, lenguas, "Todas");

    refresh();
  }catch(err){
    console.error(err);
    showError(err.message || String(err));
  }
}

reloadBtn.addEventListener("click", init);
fEstado.addEventListener("change", refresh);
fLengua.addEventListener("change", refresh);

init();
