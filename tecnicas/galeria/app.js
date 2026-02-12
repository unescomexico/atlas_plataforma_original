// Galería (generado por R) — usa indice_imagenes.json
const IMAGES_BASE = '../../imagenes';
const INDEX_JSON  = './indice_imagenes.json';

const $grid  = document.getElementById('grid');
const $count = document.getElementById('count');
const $empty = document.getElementById('empty');

// Filtros opcionales (si existen en tu plantilla)
const $q  = document.getElementById('q');
const $fT = document.getElementById('fTecnica');
const $fE = document.getElementById('fEstado');
const $fM = document.getElementById('fMunicipio');

// Lightbox opcional (si existe en tu plantilla)
const $lb        = document.getElementById('lb');
const $lbBg      = document.getElementById('lb_bg');
const $lbClose   = document.getElementById('lb_close');
const $lbPrev    = document.getElementById('lb_prev');
const $lbNext    = document.getElementById('lb_next');
const $lbImg     = document.getElementById('lb_img');
const $lbCounter = document.getElementById('lb_counter');
const $lbCaption = document.getElementById('lb_caption');

let ALL = [];
let FILT = [];
let CUR = 0;

const norm = (s) => (s ?? '').toString().trim();
const uniq = (arr) => [...new Set(arr.filter(Boolean))].sort((a,b)=>a.localeCompare(b,'es'));
const imgSrc = (rel) => encodeURI(`${IMAGES_BASE}/${rel}`);

function fillSelect(sel, values){
  if (!sel) return;
  // preserva el primer option (Todos) si existe
  const keep0 = sel.options && sel.options.length ? sel.options[0].outerHTML : null;
  sel.innerHTML = keep0 ? keep0 : "<option value=''>Todos</option>";
  values.forEach(v=>{ const o=document.createElement('option'); o.value=v; o.textContent=v; sel.appendChild(o); });
}

function applyFilters(){
  const q  = $q ? norm($q.value).toLowerCase() : '';
  const t  = $fT ? $fT.value : '';
  const e  = $fE ? $fE.value : '';
  const m  = $fM ? $fM.value : '';

  FILT = ALL.filter(r=>{
    if (t && norm(r.tecnica) !== t) return false;
    if (e && norm(r.estado) !== e) return false;
    if (m && norm(r.municipio) !== m) return false;
    if (q){
      const blob = `${norm(r.tecnica)} ${norm(r.estado)} ${norm(r.municipio)}`.toLowerCase();
      if (!blob.includes(q)) return false;
    }
    return true;
  });
  render();
}

function render(){
  if (!$grid) return;
  $grid.innerHTML = '';
  if ($empty) $empty.style.display = FILT.length ? 'none' : 'block';
  if ($count) $count.textContent = `${FILT.length} imágenes`;

  FILT.forEach((r, i)=>{
    const card = document.createElement('div');
    card.className = 'card';
    card.tabIndex = 0;

    const img = document.createElement('img');
    img.className = 'thumb';
    img.loading = 'lazy';
    img.alt = norm(r.tecnica) || 'Imagen';
    img.src = imgSrc(r.relpath);

    const body = document.createElement('div');
    body.className = 'card_body';

    const title = document.createElement('div');
    title.className = 'title';
    title.textContent = norm(r.tecnica) || '(Sin técnica)';

    const meta = document.createElement('div');
    meta.className = 'meta';
    const e = norm(r.estado);
    const m = norm(r.municipio);
    meta.innerHTML = `${e ? `<div><b>Estado:</b> ${e}</div>` : ''}${m ? `<div><b>Municipio:</b> ${m}</div>` : ''}`;

    body.appendChild(title);
    body.appendChild(meta);
    card.appendChild(img);
    card.appendChild(body);

    card.addEventListener('click', ()=> openLb(i));
    card.addEventListener('keydown', (ev)=>{ if(ev.key==='Enter' || ev.key===' '){ ev.preventDefault(); openLb(i);} });

    $grid.appendChild(card);
  });
}

function hasLb(){ return $lb && $lbImg && $lbCaption && $lbCounter; }

function openLb(i){
  if (!hasLb()) return;
  CUR = i;
  updateLb();
  $lb.style.display = 'grid';
  document.body.style.overflow = 'hidden';
}
function closeLb(){
  if (!hasLb()) return;
  $lb.style.display = 'none';
  document.body.style.overflow = '';
}
function prevLb(){ CUR = (CUR - 1 + FILT.length) % FILT.length; updateLb(); }
function nextLb(){ CUR = (CUR + 1) % FILT.length; updateLb(); }
function updateLb(){
  const r = FILT[CUR];
  $lbImg.src = imgSrc(r.relpath);
  $lbImg.alt = norm(r.tecnica) || 'Imagen';
  $lbCounter.textContent = `${CUR+1} / ${FILT.length}`;
  const tec = norm(r.tecnica);
  const est = norm(r.estado);
  const mun = norm(r.municipio);
  $lbCaption.innerHTML = `${tec ? `<div><b>Técnica:</b> ${tec}</div>` : ''}${est ? `<div><b>Estado:</b> ${est}</div>` : ''}${mun ? `<div><b>Municipio:</b> ${mun}</div>` : ''}`;
}

async function init(){
  const res = await fetch(INDEX_JSON, { cache: 'no-store' });
  if (!res.ok) throw new Error('No se pudo cargar indice_imagenes.json');
  ALL = await res.json();
  FILT = ALL.slice();
  fillSelect($fT, uniq(ALL.map(r=>norm(r.tecnica)).filter(Boolean)));
  fillSelect($fE, uniq(ALL.map(r=>norm(r.estado)).filter(Boolean)));
  fillSelect($fM, uniq(ALL.map(r=>norm(r.municipio)).filter(Boolean)));
  render();
}

if ($q) $q.addEventListener('input', ()=>{ clearTimeout(window.__qt); window.__qt=setTimeout(applyFilters, 120); });
if ($fT) $fT.addEventListener('change', applyFilters);
if ($fE) $fE.addEventListener('change', applyFilters);
if ($fM) $fM.addEventListener('change', applyFilters);

if ($lbBg) $lbBg.addEventListener('click', closeLb);
if ($lbClose) $lbClose.addEventListener('click', closeLb);
if ($lbPrev) $lbPrev.addEventListener('click', prevLb);
if ($lbNext) $lbNext.addEventListener('click', nextLb);
document.addEventListener('keydown', (ev)=>{
  if (!hasLb() || $lb.style.display === 'none') return;
  if (ev.key==='Escape') closeLb();
  if (ev.key==='ArrowLeft') prevLb();
  if (ev.key==='ArrowRight') nextLb();
});

init().catch(err=>{
  console.error(err);
  if ($grid) $grid.innerHTML = `<div class='empty'>Error: ${err.message}</div>`;
  if ($count) $count.textContent = '0 imágenes';
});
