// Galería: consume indice_imagenes.json (generado por R)
// Base relativo desde tecnicas/galeria/index.html
const IMAGES_BASE = "../../imagenes";
const INDEX_JSON  = "./indice_imagenes.json";

const ITEMS = { all: [], filtered: [], idx: 0 };

const $q = document.getElementById("q");
const $fT = document.getElementById("fTecnica");
const $fF = document.getElementById("fFolder");
const $grid = document.getElementById("grid");
const $count = document.getElementById("count");
const $empty = document.getElementById("empty");

const $lb = document.getElementById("lb");
const $lbBg = document.getElementById("lb_bg");
const $lbClose = document.getElementById("lb_close");
const $lbPrev = document.getElementById("lb_prev");
const $lbNext = document.getElementById("lb_next");
const $lbImg = document.getElementById("lb_img");
const $lbCounter = document.getElementById("lb_counter");
const $lbCaption = document.getElementById("lb_caption");

function norm(s){ return (s ?? "").toString().trim(); }

function escHtml(s){
  const str = String(s ?? "");
  const map = { "&":"&amp;","<":"&lt;",">":"&gt;","\"":"&quot;","'":"&#39;" };
  return str.replace(/[&<>"']/g, (m) => map[m] || m);
}

function splitTokens(v){
  return norm(v).split("|").map(x=>x.trim()).filter(Boolean);
}

function uniqSorted(arr){
  return [...new Set(arr.filter(Boolean))].sort((a,b)=>a.localeCompare(b,"es"));
}

function imgSrc(relpath){
  // encodeURI para caracteres especiales
  return encodeURI(`${IMAGES_BASE}/${relpath}`);
}

function populateFilters(rows){
  const tecnicas = uniqSorted(rows.map(r=>norm(r.tecnica)).filter(Boolean));
  tecnicas.forEach(t=>{
    const o=document.createElement("option"); o.value=t; o.textContent=t; $fT.appendChild(o);
  });

  const folders = uniqSorted(rows.map(r=>norm(r.folder)).filter(Boolean));
  folders.forEach(f=>{
    const o=document.createElement("option"); o.value=f; o.textContent=f; $fF.appendChild(o);
  });
}

function applyFilters(){
  const q = norm($q.value).toLowerCase();
  const t = $fT.value;
  const f = $fF.value;

  ITEMS.filtered = ITEMS.all.filter(r=>{
    if (t && norm(r.tecnica) !== t) return false;
    if (f && norm(r.folder) !== f) return false;

    if (q){
      const blob = `${norm(r.tecnica)} ${norm(r.filename)} ${norm(r.archivo_descargado)} ${norm(r.ruta_archivo)}`.toLowerCase();
      if (!blob.includes(q)) return false;
    }
    return true;
  });

  renderGrid();
}

function renderGrid(){
  const rows = ITEMS.filtered;
  $grid.innerHTML = "";
  $empty.style.display = rows.length ? "none" : "block";
  $count.textContent = `${rows.length} imágenes`;

  rows.forEach((r, i)=>{
    const card = document.createElement("div");
    card.className = "card";
    card.tabIndex = 0;

    const rel = r.relpath;
    const has = rel && rel !== "NA" && rel !== "null";

    card.innerHTML = `
      <img class="thumb" src="${has ? escHtml(imgSrc(rel)) : ""}" alt="${escHtml(norm(r.tecnica) || "Imagen")}">
      <div class="card_body">
        <div class="title">${escHtml(norm(r.tecnica) || "(Sin técnica)")}</div>
        <div class="meta">
          ${norm(r.filename) ? `<div><b>Archivo:</b> ${escHtml(r.filename)}</div>` : ``}
          ${norm(r.folder) ? `<div><b>Carpeta:</b> ${escHtml(r.folder)}</div>` : ``}
          ${!has ? `<div><b>⚠</b> No se encontró en /imagenes</div>` : ``}
        </div>
        <button class="btn" type="button">${has ? "Ver" : "Sin archivo"}</button>
      </div>
    `;

    card.addEventListener("click", ()=> has && openLb(i));
    card.addEventListener("keydown", (ev)=>{
      if ((ev.key === "Enter" || ev.key === " ") && has){ ev.preventDefault(); openLb(i); }
    });

    $grid.appendChild(card);
  });
}

function openLb(i){
  ITEMS.idx = i;
  updateLb();
  $lb.style.display = "grid";
  document.body.style.overflow = "hidden";
}

function closeLb(){
  $lb.style.display = "none";
  document.body.style.overflow = "";
}

function prevLb(){
  if (!ITEMS.filtered.length) return;
  ITEMS.idx = (ITEMS.idx - 1 + ITEMS.filtered.length) % ITEMS.filtered.length;
  updateLb();
}

function nextLb(){
  if (!ITEMS.filtered.length) return;
  ITEMS.idx = (ITEMS.idx + 1) % ITEMS.filtered.length;
  updateLb();
}

function updateLb(){
  const r = ITEMS.filtered[ITEMS.idx];
  const rel = r.relpath;
  $lbImg.src = imgSrc(rel);
  $lbImg.alt = norm(r.tecnica) || "Imagen";
  $lbCounter.textContent = `${ITEMS.idx + 1} / ${ITEMS.filtered.length}`;

  // Muestra metadatos (todas las columnas excepto relpath)
  const keys = Object.keys(r).filter(k => k !== "relpath");
  const rows = keys.map(k => {
    const v = r[k];
    if (v === null || v === undefined) return "";
    const s = String(v).trim();
    if (!s || s === "NA") return "";
    return `<div><b>${escHtml(k)}:</b> ${escHtml(s)}</div>`;
  }).filter(Boolean).join("");

  $lbCaption.innerHTML = rows || "<div class='muted'>(Sin metadatos)</div>";
}

$lbBg.addEventListener("click", closeLb);
$lbClose.addEventListener("click", closeLb);
$lbPrev.addEventListener("click", prevLb);
$lbNext.addEventListener("click", nextLb);

document.addEventListener("keydown", (ev)=>{
  if ($lb.style.display === "none") return;
  if (ev.key === "Escape") closeLb();
  if (ev.key === "ArrowLeft") prevLb();
  if (ev.key === "ArrowRight") nextLb();
});

async function init(){
  const res = await fetch(INDEX_JSON, { cache: "no-store" });
  if (!res.ok) throw new Error(`No se pudo cargar ${INDEX_JSON}`);
  const rows = await res.json();
  ITEMS.all = rows;
  populateFilters(ITEMS.all);
  ITEMS.filtered = ITEMS.all.slice();
  renderGrid();
}

$q.addEventListener("input", ()=> { clearTimeout(window.__t); window.__t = setTimeout(applyFilters, 120); });
$fT.addEventListener("change", applyFilters);
$fF.addEventListener("change", applyFilters);

init().catch(err=>{
  console.error(err);
  $grid.innerHTML = `<div class="empty">Error: ${escHtml(err.message)}</div>`;
  $count.textContent = "0 imágenes";
});
