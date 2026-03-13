/* ================================================
   ATLAS DE TÉCNICAS DEL ARTE TEXTIL — MAIN JS
   Lee los datos directamente desde los CSV.
   Para actualizar: reemplaza los CSV y recarga.
   ================================================ */

'use strict';

// ────────────────────────────────────────────────
// PALETA
// ────────────────────────────────────────────────
const COLORS = {
  magenta:  '#B50552',
  azulMar:  '#035A79',
  naranja:  '#FB4801',
  verde:    '#05B794',
  ocre:     '#FFA329',
  gris:     '#C8B8C0',
  cyan:     '#00B2C0',
  rosa:     '#DE599B',
  negro:    '#1A1018',
  arena:    '#8C7A80',
  crema:    '#FFF8F5',
};

// Colores por grupo de técnica
const GRUPO_COLOR = {
  'Bordado a mano':    COLORS.magenta,
  'Bordado a máquina': COLORS.rosa,
  'Telar de cintura':  COLORS.azulMar,
  'Tejido a mano':     COLORS.cyan,
  'Telar de pedal':    COLORS.verde,
  'Telar de cajón':    COLORS.naranja,
};

const GRUPO_CHIP = {
  'Bordado a mano':    'chip-bordado-mano',
  'Bordado a máquina': 'chip-bordado-maquina',
  'Telar de cintura':  'chip-telar-cintura',
  'Tejido a mano':     'chip-tejido-mano',
  'Telar de pedal':    'chip-telar-pedal',
  'Telar de cajón':    'chip-telar-cajon',
};

// Degradado coropletas: #F588AFFF → #A4D984FF → #FCBC52FF → #A91E45
function getMapColor(count) {
  if (count >= 40) return '#A91E45';
  if (count >= 20) return '#FCBC52FF';
  if (count >= 10) return '#A4D984FF';
  if (count >= 1)  return '#F588AFFF';
  return '#d8d0d4';
}

// ────────────────────────────────────────────────
// ESTADO GLOBAL
// ────────────────────────────────────────────────
let ATLAS = null;        // { tecnicas: [], estados: {} }
let tecnicasMap = {};
let estadosTecnicas = {};

// ────────────────────────────────────────────────
// CSV PARSER (sin dependencias)
// ────────────────────────────────────────────────
function parseCSV(text) {
  const lines = text.replace(/\r\n/g, '\n').replace(/\r/g, '\n').split('\n');
  if (lines.length < 2) return [];

  const headers = parseCSVRow(lines[0]);
  const rows = [];

  for (let i = 1; i < lines.length; i++) {
    if (!lines[i].trim()) continue;
    const vals = parseCSVRow(lines[i]);
    const obj = {};
    headers.forEach((h, idx) => { obj[h] = vals[idx] !== undefined ? vals[idx] : ''; });
    rows.push(obj);
  }
  return rows;
}

function parseCSVRow(line) {
  const result = [];
  let cur = '';
  let inQuotes = false;
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (ch === '"') {
      if (inQuotes && line[i + 1] === '"') { cur += '"'; i++; }
      else { inQuotes = !inQuotes; }
    } else if (ch === ',' && !inQuotes) {
      result.push(cur.trim());
      cur = '';
    } else {
      cur += ch;
    }
  }
  result.push(cur.trim());
  return result;
}

// ────────────────────────────────────────────────
// CONSTRUCCIÓN DE ATLAS DESDE CSV
// ────────────────────────────────────────────────
function buildAtlasFromCSV(techRows, recordRows, imgRows) {
  // ════════════════════════════════════════════════════
  // CRITERIO DE FUENTES — qué dataset sirve para qué
  // ════════════════════════════════════════════════════
  //
  // data_by_TECHNIQUE_id  →  datos AGREGADOS por técnica
  //   Usar para: estadísticas (n_fichas, n_mujeres, n_hombres, edad_prom),
  //   conteos de manufactura / teñido / aprendizaje / enseñanza,
  //   listas ya consolidadas de estados, prendas, ceremonias, materiales.
  //   Estos valores ya están calculados y son la fuente canónica
  //   de la ficha estructurada.
  //
  // data_by_RECORD_id  →  datos CUALITATIVOS por aportación individual
  //   Usar SOLO para: voces directas de artesanos (Historia, S1-S5),
  //   listas únicas de lenguas y municipios (no están en technique_id),
  //   temporalidad (antigua/nueva) calculada por mayoría de registros,
  //   y el campo tecnica_grupo (grupo de clasificación).
  //   NO usar Descripción (solo repite el nombre de la técnica).
  //   NO usar campos numéricos de manufactura/teñido/aprendizaje
  //   (ya están mejor calculados en technique_id).
  //
  // indice_imagenes.csv  →  mapeo archivo → técnica para imágenes.
  // ════════════════════════════════════════════════════

  // ── 1. Índice de imágenes ─────────────────────────
  const imgByTech = {};
  (imgRows || []).forEach(r => {
    const tecnica = (r['Tecnica'] || '').trim();
    const archivo = (r['archivo_descargado'] || '').trim();
    const status  = (r['status'] || '').toLowerCase().trim();
    if (!tecnica || !archivo || status === 'error') return;
    if (!imgByTech[tecnica]) imgByTech[tecnica] = [];
    imgByTech[tecnica].push(archivo);
  });

  // ── 2. Índice de registros individuales por técnica ─
  const recordsByTech = {};
  recordRows.forEach(r => {
    const key = (r['Tecnica'] || '').trim();
    if (!key) return;
    if (!recordsByTech[key]) recordsByTech[key] = [];
    recordsByTech[key].push(r);
  });

  // ── 3. Construir cada técnica ─────────────────────
  const estadosMap = {};

  const tecnicas = techRows.map(row => {
    const nombre  = (row['Tecnica'] || row['tecnica_norm'] || '').trim();
    const records = recordsByTech[nombre] || [];

    // ── Desde data_by_TECHNIQUE_id (fuente canónica) ──────────────

    // Estados — lista ya agregada
    const estados = (row['estados'] || '').split(',').map(s => s.trim()).filter(Boolean);

    // Estadísticas
    const n_fichas      = num(row['n_fichas']);
    const n_mujeres     = num(row['n_mujeres']);
    const n_hombres     = num(row['n_hombres']);
    const edad_promedio = num(row['edad_prom']) || null;

    // Materiales — string concatenado y limpio
    const materiales = (row['Materiales_concat_clean'] || '').trim();

    // Manufactura — conteos agregados
    const manufactura = {
      mano:        num(row['n_man_mano']),
      pedal:       num(row['n_man_pedal']),
      telar:       num(row['n_man_telar']),
      mixta:       num(row['n_man_mixta']),
      tejido:      num(row['n_man_tejido']),
      telar_pedal: num(row['n_man_telar_pedal']),
    };
    const manufactura_tipos = [];
    if (manufactura.mano > 0)        manufactura_tipos.push('Mano');
    if (manufactura.telar > 0)       manufactura_tipos.push('Telar de cintura');
    if (manufactura.telar_pedal > 0) manufactura_tipos.push('Telar de pedal');

    // Teñido — conteos agregados
    const tenido = {
      plantas:   num(row['n_tenido_plantas']),
      minerales: num(row['n_tenido_minerales']),
      animales:  num(row['n_tenido_animales']),
      otro:      num(row['n_tenido_otro']),
    };
    const tenido_tipos = [];
    if (tenido.plantas > 0)   tenido_tipos.push('Plantas');
    if (tenido.minerales > 0) tenido_tipos.push('Minerales');
    if (tenido.animales > 0)  tenido_tipos.push('Animales/Insectos');

    // Aprendizaje — conteos agregados
    const aprendizaje = {
      madre:      num(row['n_aprendio_madre']),
      abuela:     num(row['n_aprendio_abuela']),
      tia:        num(row['n_aprendio_tia']),
      hermana:    num(row['n_aprendio_hermana']),
      cunada:     num(row['n_aprendio_cunada']),
      instructor: num(row['n_aprendio_instructor']),
      padre:      num(row['n_aprendio_padre']),
    };

    // Enseñanza — conteos agregados
    const ensenanza = {
      hijas:       num(row['n_ens_hijas']),
      hijos:       num(row['n_ens_hijos']),
      nietos:      num(row['n_ens_nietos']),
      sobrinos:    num(row['n_ens_sobrinos']),
      pareja:      num(row['n_ens_pareja']),
      estudiantes: num(row['n_ens_estudiantes']),
      no_ensena:   num(row['n_no_ha_ensenado']),
    };

    // Prendas y ceremonias — resúmenes agregados
    const prendas_resumen    = row['prendas_resumen']    || '';
    const ceremonias_resumen = row['ceremonias_resumen'] || '';

    // ── Desde data_by_RECORD_id (solo campos cualitativos únicos) ─────

    // Grupo de técnica (campo solo disponible en records)
    const grupo = records.length > 0
      ? (records[0]['tecnica_grupo'] || 'Otras')
      : 'Otras';

    // Lenguas — lista única de las que declaran los artesanos de esta técnica
    const lenguas = [...new Set(
      records.map(r => (r['Lengua'] || '').trim()).filter(Boolean)
    )].sort();

    // Municipios — lista única (no disponible en technique_id)
    const municipios = [...new Set(
      records.map(r => (r['Municipio'] || '').trim()).filter(s => s && s !== 'NA')
    )].sort();

    // Temporalidad — calculada por mayoría de registros
    let nAntigua = 0, nNueva = 0;
    records.forEach(r => {
      const t = (r['Temporalidad'] || '').toLowerCase();
      if (t.includes('antigua')) nAntigua++;
      else if (t.includes('nueva')) nNueva++;
    });
    const temporalidad = nAntigua >= nNueva ? 'antigua' : 'nueva';

    // Historia — se toma el fragmento más extenso y rico entre todos los registros.
    // No se muestran todos los fragmentos individuales: se elige el más representativo
    // (mayor longitud) como texto único de la sección.
    const historiaFrags = records
      .map(r => (r['Historia'] || '').trim())
      .filter(s => s.length >= 20);
    const historia = historiaFrags.length > 0
      ? historiaFrags.reduce((a, b) => b.length > a.length ? b : a)
      : '';

    // Significado — se toman los fragmentos S1-S5 únicos y se agrupan
    // en un solo párrafo separado por puntos, eliminando duplicados exactos
    // y fragmentos triviales (menos de 10 chars).
    // Se limita a los 5 más representativos (los más largos) para no saturar.
    const significadoFrags = [...new Set(
      records.flatMap(r =>
        ['S1','S2','S3','S4','S5']
          .map(k => (r[k] || '').trim())
          .filter(s => s.length >= 10)
      )
    )];
    const significados = significadoFrags
      .sort((a, b) => b.length - a.length)
      .slice(0, 5);

    // Imágenes
    const imagenes = imgByTech[nombre] || [];

    // Acumular para el mapa
    estados.forEach(est => {
      if (!estadosMap[est]) estadosMap[est] = [];
      estadosMap[est].push(nombre);
    });

    return {
      tecnica: nombre,
      grupo, estados, lenguas, municipios, temporalidad,
      n_fichas, n_mujeres, n_hombres, edad_promedio,
      materiales,
      manufactura, manufactura_tipos,
      tenido, tenido_tipos,
      aprendizaje, ensenanza,
      prendas_resumen, ceremonias_resumen,
      imagenes,
      // Contenido textual consolidado
      historia, significados,
    };
  }).filter(t => t.tecnica);

  return { tecnicas, estados: estadosMap };
}

function num(v) {
  const n = parseFloat(v);
  return isNaN(n) ? 0 : n;
}

// ────────────────────────────────────────────────
// CARGA DE CSV
// ────────────────────────────────────────────────
async function loadCSVs() {
  const overlay = document.getElementById('loading-overlay');
  const errBanner = document.getElementById('error-banner');

  try {
    const [techRes, recRes, imgRes] = await Promise.all([
      fetch('data_by_technique_id.csv'),
      fetch('data_by_record_id.csv'),
      fetch('indice_imagenes.csv'),
    ]);

    if (!techRes.ok || !recRes.ok) throw new Error('No se pudieron cargar los CSV.');

    const [techText, recText, imgText] = await Promise.all([
      techRes.text(),
      recRes.text(),
      imgRes.ok ? imgRes.text() : Promise.resolve(''),
    ]);

    const techRows   = parseCSV(techText);
    const recordRows = parseCSV(recText);
    const imgRows    = imgText ? parseCSV(imgText) : [];

    ATLAS = buildAtlasFromCSV(techRows, recordRows, imgRows);
    tecnicasMap     = {};
    estadosTecnicas = ATLAS.estados;
    ATLAS.tecnicas.forEach(t => { tecnicasMap[t.tecnica] = t; });

    // Update header stats
    const nEstados = Object.keys(estadosTecnicas).length;
    const nTecnicas = ATLAS.tecnicas.length;
    const nRegistros = ATLAS.tecnicas.reduce((s, t) => s + t.n_fichas, 0);

    document.querySelectorAll('.hstat-num')[0].textContent = nTecnicas;
    document.querySelectorAll('.hstat-num')[1].textContent = Math.round(nRegistros);
    document.querySelectorAll('.hstat-num')[2].textContent = nEstados;

    // Hide loading
    overlay.classList.add('hidden');
    setTimeout(() => { overlay.style.display = 'none'; }, 450);

    // Init map
    initMap();

  } catch (err) {
    console.error('Error cargando CSV:', err);
    overlay.querySelector('.loading-spinner').style.display = 'none';
    overlay.querySelector('.loading-msg').style.display = 'none';
    errBanner.classList.add('visible');
    errBanner.querySelector('p').textContent =
      'No se encontraron los archivos data_by_technique_id.csv y data_by_record_id.csv en la carpeta del proyecto. ' +
      'Verifica que ambos archivos estén en la misma carpeta que index.html.';
  }
}

// ────────────────────────────────────────────────
// UTILS
// ────────────────────────────────────────────────
function esc(s) {
  if (!s) return '';
  return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;');
}

function escJs(s) {
  if (!s) return '';
  return String(s).replace(/\\/g,'\\\\').replace(/'/g,"\\'");
}

function imgPath(filename) {
  return filename ? `imagenes/${filename}` : '';
}

function parseSummary(str) {
  if (!str) return [];
  return str.split(';').map(s => {
    const m = s.trim().match(/^(.+?)\s*\(\d+\)$/);
    return m ? m[1].trim() : s.trim();
  }).filter(Boolean);
}

function normalizeEstado(s) {
  return (s || '').toLowerCase()
    .normalize('NFD').replace(/[\u0300-\u036f]/g, '')
    .replace(/\s+/g, ' ').trim()
    .replace('ciudad de mexico', 'districto federal')
    .replace('michoacan de ocampo', 'michoacan')
    .replace('veracruz de ignacio de la llave', 'veracruz')
    .replace('coahuila de zaragoza', 'coahuila');
}

function countForEstado(name) {
  for (const [estado, tecs] of Object.entries(estadosTecnicas)) {
    if (normalizeEstado(estado) === normalizeEstado(name)) return tecs.length;
  }
  return 0;
}

function getTecnicasForEstado(name) {
  for (const [estado, tecs] of Object.entries(estadosTecnicas)) {
    if (normalizeEstado(estado) === normalizeEstado(name)) return tecs;
  }
  return [];
}

// ────────────────────────────────────────────────
// TABS
// ────────────────────────────────────────────────
let currentTab = 'mapa';
let networkInitialized = false;
let catalogInitialized = false;

document.querySelectorAll('.nav-tab').forEach(tab => {
  tab.addEventListener('click', () => {
    const id = tab.dataset.tab;
    if (id === currentTab) return;
    currentTab = id;
    document.querySelectorAll('.nav-tab').forEach(t => t.classList.remove('active'));
    tab.classList.add('active');
    document.querySelectorAll('.view').forEach(v => v.classList.remove('active'));
    document.getElementById(`view-${id}`).classList.add('active');

    if (id === 'mapa') {
      setTimeout(() => map && map.invalidateSize(), 80);
    } else if (id === 'catalogo' && !catalogInitialized) {
      catalogInitialized = true; renderCatalog();
    } else if (id === 'red' && !networkInitialized) {
      networkInitialized = true; initNetwork();
    }
  });
});

// ────────────────────────────────────────────────
// MAP
// ────────────────────────────────────────────────
let map = null;
let geojsonLayer = null;
let selectedStateName = null;

function initMap() {
  map = L.map('map', { zoomControl: true, scrollWheelZoom: true });
  L.tileLayer('https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png', {
    attribution: '© OpenStreetMap, © CARTO', subdomains: 'abcd', maxZoom: 19
  }).addTo(map);
  map.fitBounds([[14.5, -118.4], [32.7, -86.7]]);

  fetch('https://raw.githubusercontent.com/angelnmara/geojson/master/mexicoHigh.json')
    .then(r => r.json())
    .then(data => {
      geojsonLayer = L.geoJSON(data, {
        style: stateStyle,
        onEachFeature: (feat, layer) => {
          const name = feat.properties.name || feat.properties.NAME_1 || '';
          const count = countForEstado(name);
          if (count > 0) {
            layer.bindTooltip(
              `<b>${name}</b><br>${count} técnica${count !== 1 ? 's' : ''}`,
              { className: 'leaflet-tooltip-atlas', direction: 'top', sticky: true }
            );
          }
          layer.on({
            mouseover: e => { if (name !== selectedStateName) e.target.setStyle(highlightStyle(feat)); },
            mouseout:  e => { if (name !== selectedStateName) geojsonLayer.resetStyle(e.target); },
            click: () => onStateClick(name, layer, feat),
          });
        }
      }).addTo(map);
    })
    .catch(() => {
      document.getElementById('panel-subtitle').textContent = 'Mapa no disponible sin conexión.';
    });
}

function stateStyle(feature) {
  const name  = feature.properties.name || feature.properties.NAME_1 || '';
  const count = countForEstado(name);
  return { fillColor: getMapColor(count), weight: 1.2, opacity: 1, color: '#fff', fillOpacity: count > 0 ? 0.78 : 0.18 };
}

function highlightStyle(feature) {
  const name  = feature.properties.name || feature.properties.NAME_1 || '';
  const count = countForEstado(name);
  return { fillColor: getMapColor(count), weight: 2.5, color: COLORS.negro, fillOpacity: 0.92 };
}

function onStateClick(name, layer, feat) {
  selectedStateName = name;
  if (geojsonLayer) {
    geojsonLayer.eachLayer(l => geojsonLayer.resetStyle(l));
    layer.setStyle({ weight: 3, color: COLORS.negro, fillOpacity: 0.92, fillColor: getMapColor(countForEstado(name)) });
  }
  const tecnicas = getTecnicasForEstado(name);
  document.getElementById('panel-title').textContent = name;
  document.getElementById('panel-subtitle').textContent =
    tecnicas.length > 0
      ? `${tecnicas.length} técnica${tecnicas.length !== 1 ? 's' : ''} identificada${tecnicas.length !== 1 ? 's' : ''}`
      : 'Sin técnicas registradas en el taller';
  renderSidePanel(tecnicas);
}

function renderSidePanel(tecnicas) {
  const body = document.getElementById('panel-body');
  if (!tecnicas || tecnicas.length === 0) {
    body.innerHTML = `<div class="welcome-state"><h3>Sin registros</h3><p>No se documentaron técnicas en este estado.</p></div>`;
    return;
  }
  const grupos = {};
  tecnicas.forEach(tname => {
    const td = tecnicasMap[tname];
    const g  = td ? (td.grupo || 'Otras') : 'Otras';
    if (!grupos[g]) grupos[g] = [];
    grupos[g].push(tname);
  });
  let html = '';
  Object.entries(grupos).sort().forEach(([grupo, tecs]) => {
    const chipCls = GRUPO_CHIP[grupo] || 'chip-other';
    html += `<div class="grupo-section">
      <div class="grupo-label">${esc(grupo)}</div>
      <div class="chips-wrap">`;
    tecs.forEach(tname => {
      const td = tecnicasMap[tname];
      const hasImg = td && td.imagenes && td.imagenes.length > 0;
      html += `<span class="tecnica-chip ${chipCls}" onclick="openFicha('${escJs(tname)}')">
        ${hasImg ? '<span class="has-img-dot"></span>' : ''}${esc(tname)}</span>`;
    });
    html += `</div></div>`;
  });
  body.innerHTML = html;
}

document.getElementById('map-search').addEventListener('input', function() {
  const q = this.value.toLowerCase().trim();
  if (!geojsonLayer || !q) return;
  geojsonLayer.eachLayer(l => {
    const name = (l.feature?.properties?.name || l.feature?.properties?.NAME_1 || '').toLowerCase();
    if (name.includes(q)) l.openTooltip();
  });
});

// ────────────────────────────────────────────────
// CATALOG VIEW
// ────────────────────────────────────────────────
let activeGrupo  = 'all';
let activeSearch = '';

function renderCatalog() {
  if (!ATLAS) return;
  const tecnicas = ATLAS.tecnicas.filter(t => {
    const gMatch = activeGrupo === 'all' || t.grupo === activeGrupo;
    const sMatch = !activeSearch || t.tecnica.toLowerCase().includes(activeSearch);
    return gMatch && sMatch;
  });
  document.getElementById('catalog-results').textContent = `${tecnicas.length} técnica${tecnicas.length !== 1 ? 's' : ''}`;
  document.getElementById('tecnicas-grid').innerHTML = tecnicas.map(t => {
    const img   = t.imagenes && t.imagenes.length > 0 ? t.imagenes[0] : null;
    const color = GRUPO_COLOR[t.grupo] || COLORS.arena;
    const estados = (t.estados || []).slice(0, 3).join(', ');
    return `<div class="tec-card" onclick="openFicha('${escJs(t.tecnica)}')">
      <div class="tec-card-img" style="background:${COLORS.negro}">
        ${img ? `<img src="${imgPath(esc(img))}" alt="${esc(t.tecnica)}" loading="lazy">` : `<span class="tec-card-no-img">&#129525;</span>`}
        <div style="position:absolute;top:0;left:0;right:0;height:3px;background:${color}"></div>
      </div>
      <div class="tec-card-body">
        <div class="tec-card-grupo">${esc(t.grupo)}</div>
        <div class="tec-card-name">${esc(t.tecnica)}</div>
        <div class="tec-card-meta">
          ${estados ? `<span class="tec-card-tag">${esc(estados)}</span>` : ''}
          <span class="tec-card-tag">${t.n_fichas} registro${t.n_fichas !== 1 ? 's' : ''}</span>
        </div>
      </div>
    </div>`;
  }).join('');
}

document.querySelectorAll('.filter-btn').forEach(btn => {
  btn.addEventListener('click', () => {
    document.querySelectorAll('.filter-btn').forEach(b => b.classList.remove('active'));
    btn.classList.add('active');
    activeGrupo = btn.dataset.grupo;
    renderCatalog();
  });
});

document.getElementById('catalog-search').addEventListener('input', function() {
  activeSearch = this.value.toLowerCase().trim();
  renderCatalog();
});

// ────────────────────────────────────────────────
// FICHA MODAL
// ────────────────────────────────────────────────
function openFicha(tecnicaNombre) {
  const t = tecnicasMap[tecnicaNombre];
  if (!t) return;

  // ── HERO ──
  const hero = document.getElementById('modal-hero');
  if (t.imagenes && t.imagenes.length > 0) {
    const imgs = t.imagenes.map(fn =>
      `<img class="gallery-img" src="${imgPath(esc(fn))}" alt="${esc(t.tecnica)}" loading="lazy" onclick="openLightbox('${escJs(imgPath(fn))}')">`
    ).join('');
    hero.innerHTML = `
      <div class="modal-gallery">${imgs}</div>
      <button class="modal-close" onclick="closeFicha()">✕</button>
      <div class="modal-badge">${t.n_fichas} registro${t.n_fichas !== 1 ? 's' : ''}</div>
      <div class="gallery-count">${t.imagenes.length} imagen${t.imagenes.length !== 1 ? 'es' : ''}</div>`;
  } else {
    hero.innerHTML = `
      <div class="gallery-placeholder">
        <span class="textile-icon">&#129525;</span>
        <p>Sin imágenes disponibles</p>
      </div>
      <button class="modal-close" onclick="closeFicha()">✕</button>
      <div class="modal-badge">${t.n_fichas} registro${t.n_fichas !== 1 ? 's' : ''}</div>`;
  }

  // ── BODY ──
  let html = '';

  html += `<div class="modal-titulo">${esc(t.tecnica)}</div>`;
  if (t.grupo) {
    const color = GRUPO_COLOR[t.grupo] || COLORS.arena;
    html += `<div class="modal-grupo" style="background:${color}">${esc(t.grupo)}</div>`;
  }

  // Temporalidad badge
  if (t.temporalidad) {
    const tempLabel = t.temporalidad === 'antigua' ? 'Técnica ancestral' : 'Técnica contemporánea';
    const tempColor = t.temporalidad === 'antigua' ? 'var(--azul-mar)' : 'var(--verde)';
    html += `<span class="modal-temporalidad" style="background:${tempColor}">${tempLabel}</span>`;
  }

  if (t.estados && t.estados.length > 0) {
    html += `<div class="estados-row">
      ${t.estados.map(e => `<span class="estado-tag" onclick="goToEstado('${escJs(e)}')">${esc(e)}</span>`).join('')}
    </div>`;
  }

  // Estadísticas — todos los .val toman color de CSS (.stat-box .val → --magenta)
  html += `<div class="stats-row">
    <div class="stat-box"><div class="val">${t.n_fichas}</div><div class="key">Registros</div></div>
    <div class="stat-box"><div class="val">${t.n_mujeres}</div><div class="key">Mujeres</div></div>
    <div class="stat-box"><div class="val">${t.n_hombres}</div><div class="key">Hombres</div></div>
    ${t.edad_promedio ? `<div class="stat-box"><div class="val">${Math.round(t.edad_promedio)}</div><div class="key">Edad prom.</div></div>` : ''}
  </div>`;

  if (t.lenguas && t.lenguas.length > 0) {
    html += `<div style="margin-bottom:18px">
      <div style="font-size:.68rem;font-weight:700;text-transform:uppercase;letter-spacing:1.2px;color:var(--arena);margin-bottom:7px">Lenguas documentadas</div>
      <div class="tags-row">${t.lenguas.map(l => `<span class="lengua-tag">${esc(l)}</span>`).join('')}</div>
    </div>`;
  }

  // Historia — un solo párrafo (el fragmento más representativo)
  if (t.historia) {
    html += `<div class="section">
      <div class="section-head"><span class="section-title">Historia y Origen</span></div>
      <p class="ficha-parrafo">${esc(t.historia)}</p>
    </div>`;
  }

  // Significado — hasta 5 chips, no lista de tarjetas
  if (t.significados && t.significados.length > 0) {
    html += `<div class="section">
      <div class="section-head"><span class="section-title">Significado y Simbolismo</span></div>
      <div class="tag-cloud">${t.significados.map(s => `<span class="sig-tag">${esc(s)}</span>`).join('')}</div>
    </div>`;
  }

  if (t.materiales) {
    const mats = t.materiales.split(',').map(m => m.trim()).filter(Boolean);
    const ten  = t.tenido;
    const tenTotal = Object.values(ten).reduce((a, b) => a + b, 0);
    html += `<div class="section">
      <div class="section-head"><span class="section-title">Materiales</span></div>
      <div class="tag-cloud">${mats.map(m => `<span class="mat-tag">${esc(m)}</span>`).join('')}</div>
      ${tenTotal > 0 ? `
        <div style="margin-top:14px;font-size:.72rem;font-weight:700;text-transform:uppercase;letter-spacing:1px;color:var(--verde);margin-bottom:8px">Fuentes de teñido</div>
        <div class="bar-chart">
          ${barRow('Plantas',   ten.plantas,   tenTotal, COLORS.verde)}
          ${barRow('Minerales', ten.minerales, tenTotal, COLORS.azulMar)}
          ${barRow('Animales',  ten.animales,  tenTotal, COLORS.magenta)}
          ${barRow('Otro',      ten.otro,      tenTotal, COLORS.arena)}
        </div>` : ''}
    </div>`;
  }

  if (t.prendas_resumen) {
    const prendas    = parseSummary(t.prendas_resumen);
    const ceremonias = parseSummary(t.ceremonias_resumen);
    html += `<div class="section">
      <div class="section-head"><span class="section-title">Prendas y Objetos</span></div>
      <div class="tag-cloud">${prendas.slice(0, 14).map(p => `<span class="prenda-tag">${esc(p)}</span>`).join('')}</div>
      ${ceremonias.length > 0 ? `
        <div style="margin-top:12px;font-size:.72rem;font-weight:700;text-transform:uppercase;letter-spacing:1px;color:var(--naranja);margin-bottom:7px">Uso ceremonial y festivo</div>
        <div class="tag-cloud">${ceremonias.slice(0, 10).map(c => `<span class="prenda-tag">${esc(c)}</span>`).join('')}
      </div>` : ''}
    </div>`;
  }

  const man = t.manufactura;
  const manTotal = Object.values(man).reduce((a, b) => a + b, 0);
  if (manTotal > 0) {
    html += `<div class="section">
      <div class="section-head"><span class="section-title">Técnica de Manufactura</span></div>
      <div class="bar-chart">
        ${barRow('A mano',        man.mano,        manTotal, COLORS.cyan)}
        ${barRow('Telar cintura', man.telar,        manTotal, COLORS.azulMar)}
        ${barRow('Telar pedal',   man.telar_pedal,  manTotal, COLORS.verde)}
        ${barRow('Pedal',         man.pedal,        manTotal, COLORS.ocre)}
        ${barRow('Tejido',        man.tejido,       manTotal, COLORS.rosa)}
        ${barRow('Mixta',         man.mixta,        manTotal, COLORS.naranja)}
      </div>
    </div>`;
  }

  const apr = t.aprendizaje;
  const aprTotal = Object.values(apr).reduce((a, b) => a + b, 0);
  const ens = t.ensenanza || {};
  const ensTotal = Object.values(ens).filter((_, i) => Object.keys(ens)[i] !== 'no_ensena').reduce((a, b) => a + b, 0);
  if (aprTotal > 0 || ensTotal > 0) {
    html += `<div class="section">
      <div class="section-head"><span class="section-title">Transmisión del Conocimiento</span></div>
      ${aprTotal > 0 ? `
        <div style="font-size:.72rem;font-weight:700;text-transform:uppercase;letter-spacing:1px;color:var(--arena);margin-bottom:8px">Aprendieron de…</div>
        <div class="bar-chart" style="margin-bottom:16px">
          ${barRow('De la madre',   apr.madre,      aprTotal, COLORS.magenta)}
          ${barRow('De la abuela',  apr.abuela,     aprTotal, COLORS.rosa)}
          ${barRow('De la tía',     apr.tia,        aprTotal, COLORS.cyan)}
          ${barRow('De la hermana', apr.hermana,    aprTotal, COLORS.verde)}
          ${barRow('De la cuñada',  apr.cunada,     aprTotal, COLORS.ocre)}
          ${barRow('Instructor/a',  apr.instructor, aprTotal, COLORS.azulMar)}
          ${barRow('Del padre',     apr.padre,      aprTotal, COLORS.naranja)}
        </div>` : ''}
      ${ensTotal > 0 ? `
        <div style="font-size:.72rem;font-weight:700;text-transform:uppercase;letter-spacing:1px;color:var(--arena);margin-bottom:8px">Enseñan a…</div>
        <div class="bar-chart">
          ${barRow('Hijas',         ens.hijas,       ensTotal, COLORS.magenta)}
          ${barRow('Hijos',         ens.hijos,       ensTotal, COLORS.rosa)}
          ${barRow('Nietos/as',     ens.nietos,      ensTotal, COLORS.cyan)}
          ${barRow('Sobrinos/as',   ens.sobrinos,    ensTotal, COLORS.verde)}
          ${barRow('Pareja',        ens.pareja,      ensTotal, COLORS.ocre)}
          ${barRow('Estudiantes',   ens.estudiantes, ensTotal, COLORS.azulMar)}
        </div>` : ''}
      ${(ens.no_ensena > 0) ? `<p style="font-size:.78rem;color:var(--arena);margin-top:10px">${ens.no_ensena} persona${ens.no_ensena > 1 ? 's' : ''} aún no ha${ens.no_ensena > 1 ? 'n' : ''} enseñado esta técnica.</p>` : ''}
    </div>`;
  }

  if (t.municipios && t.municipios.length > 0) {
    html += `<div style="margin-bottom:16px">
      <div style="font-size:.68rem;font-weight:700;text-transform:uppercase;letter-spacing:1.2px;color:var(--arena);margin-bottom:7px">Municipios documentados</div>
      <div class="tag-cloud">${t.municipios.map(m => `<span class="mat-tag">${esc(m)}</span>`).join('')}</div>
    </div>`;
  }

  html += `<div style="margin-top:24px;padding-top:18px;border-top:1px solid var(--border)">
    <button onclick="goToNetwork('${escJs(t.tecnica)}')" style="background:var(--negro);color:var(--crema);border:none;padding:10px 20px;border-radius:4px;font-family:var(--font-body);font-size:.8rem;font-weight:600;cursor:pointer;letter-spacing:.3px;">
      Ver en la red de técnicas
    </button>
    <button onclick="goToCatalog('${escJs(t.grupo)}')" style="background:transparent;color:var(--arena);border:1px solid var(--border);padding:10px 20px;border-radius:4px;font-family:var(--font-body);font-size:.8rem;font-weight:600;cursor:pointer;letter-spacing:.3px;margin-left:8px;">
      Ver técnicas de este grupo
    </button>
  </div>`;

  document.getElementById('modal-body').innerHTML = html;
  document.getElementById('modal-overlay').classList.add('open');
  document.body.style.overflow = 'hidden';
}

function barRow(label, val, total, color) {
  if (!val || val === 0) return '';
  const pct = total > 0 ? Math.round((val / total) * 100) : 0;
  return `<div class="bar-row">
    <div class="bar-label">${esc(label)}</div>
    <div class="bar-track"><div class="bar-fill" style="width:${pct}%;background:${color}"></div></div>
    <div class="bar-val" style="color:${color}">${val}</div>
  </div>`;
}

function closeFicha() {
  document.getElementById('modal-overlay').classList.remove('open');
  document.body.style.overflow = '';
}

document.getElementById('modal-overlay').addEventListener('click', e => {
  if (e.target === document.getElementById('modal-overlay')) closeFicha();
});

function goToEstado(nombre) {
  closeFicha();
  document.querySelector('[data-tab="mapa"]').click();
  setTimeout(() => {
    if (geojsonLayer) {
      geojsonLayer.eachLayer(l => {
        const name = l.feature?.properties?.name || l.feature?.properties?.NAME_1 || '';
        if (normalizeEstado(name) === normalizeEstado(nombre)) {
          onStateClick(name, l, l.feature);
          map.flyToBounds(l.getBounds(), { duration: 0.8 });
        }
      });
    }
  }, 200);
}

function goToNetwork(tecnicaNombre) {
  closeFicha();
  document.querySelector('[data-tab="red"]').click();
  setTimeout(() => highlightNetworkNode(tecnicaNombre), 600);
}

function goToCatalog(grupo) {
  closeFicha();
  document.querySelector('[data-tab="catalogo"]').click();
  if (!catalogInitialized) catalogInitialized = true;
  activeGrupo = grupo || 'all';
  document.querySelectorAll('.filter-btn').forEach(b => {
    b.classList.toggle('active', b.dataset.grupo === activeGrupo);
  });
  renderCatalog();
}

// ────────────────────────────────────────────────
// LIGHTBOX
// ────────────────────────────────────────────────
function openLightbox(src) {
  document.getElementById('lightbox-img').src = src;
  document.getElementById('lightbox').classList.add('open');
}

document.getElementById('lightbox-close').addEventListener('click', () => {
  document.getElementById('lightbox').classList.remove('open');
});
document.getElementById('lightbox').addEventListener('click', e => {
  if (e.target === document.getElementById('lightbox') ||
      e.target === document.getElementById('lightbox-img')) {
    document.getElementById('lightbox').classList.remove('open');
  }
});

// ────────────────────────────────────────────────
// NETWORK / FORCE GRAPH
// ────────────────────────────────────────────────
let networkNodes = [];
let networkLinks = [];
let networkCanvas, networkCtx;
let transform = { x: 0, y: 0, k: 1 };
let hoveredNode = null;
let highlightedNode = null;
let activeNetworkFilter = 'all';
let dragNode = null;
let isPanning = false;
let panStart = null;

const NET_FILTERS = [
  { id: 'all',             label: 'Todas' },
  { id: 'mano',            label: 'A mano' },
  { id: 'telar',           label: 'Telar' },
  { id: 'bordado',         label: 'Bordado' },
  { id: 'tenido-plantas',  label: 'Teñido con plantas' },
  { id: 'tenido-animales', label: 'Teñido animal' },
  { id: 'tenido-minerales',label: 'Teñido mineral' },
];

function initNetwork() {
  networkCanvas = document.getElementById('network-canvas');
  networkCtx = networkCanvas.getContext('2d');
  resizeCanvas();
  window.addEventListener('resize', resizeCanvas);
  buildNetworkData();
  startSimulation();
  bindNetworkEvents();
  renderNetworkFilters();
  renderNetworkLegend();
}

function resizeCanvas() {
  const rect = networkCanvas.parentElement.getBoundingClientRect();
  networkCanvas.width  = rect.width || 900;
  networkCanvas.height = parseInt(networkCanvas.style.height || 580);
  if (networkNodes.length) drawNetwork();
}

function buildNetworkData() {
  const tecnicas = ATLAS.tecnicas;
  const grupos   = [...new Set(tecnicas.map(t => t.grupo).filter(Boolean))];
  networkNodes = [];

  grupos.forEach(g => {
    networkNodes.push({
      id: `grupo:${g}`, label: g, type: 'grupo', grupo: g, r: 22,
      color: GRUPO_COLOR[g] || COLORS.arena,
      x: Math.random() * 600 + 100, y: Math.random() * 400 + 80, vx: 0, vy: 0, fx: null, fy: null,
    });
  });

  tecnicas.forEach(t => {
    networkNodes.push({
      id: t.tecnica, label: t.tecnica, type: 'tecnica', grupo: t.grupo,
      estados: t.estados, n_fichas: t.n_fichas,
      manufactura_tipos: t.manufactura_tipos || [],
      tenido_tipos: t.tenido_tipos || [],
      r: Math.max(6, Math.min(16, 5 + t.n_fichas * 0.7)),
      color: GRUPO_COLOR[t.grupo] || COLORS.arena,
      x: Math.random() * 700 + 50, y: Math.random() * 500 + 40, vx: 0, vy: 0, fx: null, fy: null,
    });
  });

  networkLinks = [];
  tecnicas.forEach(t => {
    if (t.grupo) networkLinks.push({ source: `grupo:${t.grupo}`, target: t.tecnica, strength: 0.6 });
  });
}

function getVisibleNodes() {
  if (activeNetworkFilter === 'all') return networkNodes;
  return networkNodes.filter(n => {
    if (n.type === 'grupo') return networkNodes.some(m => m.type === 'tecnica' && m.grupo === n.label && matchesFilter(m));
    return matchesFilter(n);
  });
}

function matchesFilter(n) {
  if (activeNetworkFilter === 'all') return true;
  const m   = n.manufactura_tipos || [];
  const ten = n.tenido_tipos || [];
  const g   = (n.grupo || '').toLowerCase();
  switch (activeNetworkFilter) {
    case 'mano':             return m.includes('Mano');
    case 'telar':            return m.includes('Telar') || m.includes('Telar de pedal');
    case 'bordado':          return g.includes('bordado');
    case 'tenido-plantas':   return ten.includes('Plantas');
    case 'tenido-animales':  return ten.includes('Animales/Insectos');
    case 'tenido-minerales': return ten.includes('Minerales');
    default: return true;
  }
}

function startSimulation() {
  const W = networkCanvas.width, H = networkCanvas.height;
  const grupos = networkNodes.filter(n => n.type === 'grupo');
  grupos.forEach((g, i) => {
    const angle = (i / grupos.length) * Math.PI * 2 - Math.PI / 2;
    g.x = W / 2 + Math.cos(angle) * 180;
    g.y = H / 2 + Math.sin(angle) * 150;
    g.fx = g.x; g.fy = g.y;
  });

  networkNodes.filter(n => n.type === 'tecnica').forEach(n => {
    const hub = networkNodes.find(h => h.type === 'grupo' && h.label === n.grupo);
    if (hub) {
      const angle = Math.random() * Math.PI * 2;
      const dist  = 40 + Math.random() * 60;
      n.x = hub.x + Math.cos(angle) * dist;
      n.y = hub.y + Math.sin(angle) * dist;
    } else {
      n.x = W / 2 + (Math.random() - 0.5) * 300;
      n.y = H / 2 + (Math.random() - 0.5) * 250;
    }
    n.vx = 0; n.vy = 0;
  });

  let alpha = 1;
  for (let i = 0; i < 300; i++) { alpha *= 0.96; applyForces(alpha); }
  grupos.forEach(g => { g.fx = null; g.fy = null; });
  drawNetwork();
}

function applyForces(alpha) {
  const visible = new Set(getVisibleNodes().map(n => n.id));
  const nodes   = networkNodes.filter(n => visible.has(n.id));
  const W = networkCanvas.width, H = networkCanvas.height;

  for (let i = 0; i < nodes.length; i++) {
    for (let j = i + 1; j < nodes.length; j++) {
      const a = nodes[i], b = nodes[j];
      let dx = b.x - a.x || 0.01, dy = b.y - a.y || 0.01;
      const dist2 = dx * dx + dy * dy;
      const dist  = Math.sqrt(dist2) || 0.01;
      const ideal = a.r + b.r + 28;
      if (dist < ideal * 5) {
        const force = (alpha * 60) / dist2;
        a.vx -= dx * force; a.vy -= dy * force;
        b.vx += dx * force; b.vy += dy * force;
      }
    }
  }

  networkLinks.forEach(link => {
    const s = networkNodes.find(n => n.id === link.source);
    const t = networkNodes.find(n => n.id === link.target);
    if (!s || !t || !visible.has(s.id) || !visible.has(t.id)) return;
    const dx = t.x - s.x, dy = t.y - s.y;
    const dist = Math.sqrt(dx * dx + dy * dy) || 0.01;
    const force = (dist - 75) * 0.018 * alpha;
    const fx = (dx / dist) * force, fy = (dy / dist) * force;
    s.vx += fx; s.vy += fy; t.vx -= fx; t.vy -= fy;
  });

  nodes.forEach(n => {
    if (n.fx !== null && n.fx !== undefined) { n.x = n.fx; n.vx = 0; return; }
    if (n.fy !== null && n.fy !== undefined) { n.y = n.fy; n.vy = 0; return; }
    n.vx += (W / 2 - n.x) * 0.002 * alpha;
    n.vy += (H / 2 - n.y) * 0.002 * alpha;
    n.vx *= 0.78; n.vy *= 0.78;
    n.x += n.vx; n.y += n.vy;
    n.x = Math.max(n.r + 8, Math.min(W - n.r - 8, n.x));
    n.y = Math.max(n.r + 8, Math.min(H - n.r - 8, n.y));
  });
}

function drawNetwork() {
  const canvas = networkCanvas, ctx = networkCtx;
  const W = canvas.width, H = canvas.height;
  const visible = new Set(getVisibleNodes().map(n => n.id));

  ctx.save();
  ctx.clearRect(0, 0, W, H);
  ctx.fillStyle = '#ffffff';
  ctx.fillRect(0, 0, W, H);
  ctx.translate(transform.x, transform.y);
  ctx.scale(transform.k, transform.k);

  networkLinks.forEach(link => {
    const s = networkNodes.find(n => n.id === link.source);
    const t = networkNodes.find(n => n.id === link.target);
    if (!s || !t || !visible.has(s.id) || !visible.has(t.id)) return;
    const isHi = highlightedNode && (highlightedNode.id === s.id || highlightedNode.id === t.id);
    const isHov= hoveredNode    && (hoveredNode.id    === s.id || hoveredNode.id    === t.id);
    ctx.beginPath();
    ctx.moveTo(s.x, s.y); ctx.lineTo(t.x, t.y);
    ctx.strokeStyle = isHi || isHov ? (s.color + 'cc') : 'rgba(26,16,24,0.08)';
    ctx.lineWidth = isHi ? 2 : 1;
    ctx.stroke();
  });

  const nodes = networkNodes.filter(n => visible.has(n.id));
  nodes.sort((a, b) => {
    if (a.type === 'grupo' && b.type !== 'grupo') return 1;
    if (b.type === 'grupo' && a.type !== 'grupo') return -1;
    return 0;
  });

  nodes.forEach(n => {
    const isHov = hoveredNode     && hoveredNode.id     === n.id;
    const isHi  = highlightedNode && highlightedNode.id === n.id;
    const dimmed = (highlightedNode || hoveredNode) && !isHi && !isHov;
    const r = n.r * (isHov || isHi ? 1.25 : 1);

    if (isHov || isHi) { ctx.shadowColor = n.color; ctx.shadowBlur = 12; }
    ctx.beginPath();
    ctx.arc(n.x, n.y, r, 0, Math.PI * 2);
    ctx.fillStyle = dimmed ? (n.color + '44') : n.color;
    ctx.fill();
    if (n.type === 'grupo') {
      ctx.strokeStyle = dimmed ? 'rgba(255,255,255,.3)' : 'rgba(255,255,255,.7)';
      ctx.lineWidth = 2; ctx.stroke();
    }
    ctx.shadowBlur = 0;

    if (n.type === 'grupo' || isHov || isHi) {
      ctx.font = n.type === 'grupo' ? `700 11px "DM Sans", sans-serif` : `500 9.5px "DM Sans", sans-serif`;
      ctx.textAlign = 'center'; ctx.textBaseline = 'top';
      ctx.fillStyle = dimmed ? 'rgba(26,16,24,.25)' : COLORS.negro;
      const label = n.label.length > 22 ? n.label.slice(0, 20) + '…' : n.label;
      ctx.fillText(label, n.x, n.y + r + 4);
    }
  });

  ctx.restore();
}

function getNodeAtPoint(mx, my) {
  const x = (mx - transform.x) / transform.k;
  const y = (my - transform.y) / transform.k;
  const visible = new Set(getVisibleNodes().map(n => n.id));
  let best = null, bestDist = Infinity;
  networkNodes.filter(n => visible.has(n.id)).forEach(n => {
    const d = Math.sqrt((n.x - x) ** 2 + (n.y - y) ** 2);
    if (d < n.r + 6 && d < bestDist) { bestDist = d; best = n; }
  });
  return best;
}

function bindNetworkEvents() {
  const canvas  = networkCanvas;
  const tooltip = document.getElementById('network-tooltip');

  canvas.addEventListener('mousemove', e => {
    const rect = canvas.getBoundingClientRect();
    const mx = e.clientX - rect.left, my = e.clientY - rect.top;

    if (isPanning && panStart) {
      transform.x += mx - panStart.x; transform.y += my - panStart.y;
      panStart = { x: mx, y: my }; drawNetwork(); return;
    }

    if (dragNode) {
      dragNode.x = (mx - transform.x) / transform.k;
      dragNode.y = (my - transform.y) / transform.k;
      dragNode.fx = dragNode.x; dragNode.fy = dragNode.y;
      drawNetwork(); return;
    }

    const node = getNodeAtPoint(mx, my);
    hoveredNode = node;
    canvas.style.cursor = node ? 'pointer' : 'grab';

    if (node) {
      tooltip.innerHTML = `<strong>${esc(node.label)}</strong>
        ${node.type === 'tecnica' ? `${node.n_fichas} registro${node.n_fichas !== 1 ? 's' : ''} · ${(node.estados || []).slice(0, 3).join(', ')}` : 'Grupo de técnicas'}`;
      tooltip.style.left = (mx + 14) + 'px';
      tooltip.style.top  = (my - 10) + 'px';
      tooltip.classList.add('visible');
    } else {
      tooltip.classList.remove('visible');
    }
    drawNetwork();
  });

  canvas.addEventListener('mousedown', e => {
    const rect = canvas.getBoundingClientRect();
    const mx = e.clientX - rect.left, my = e.clientY - rect.top;
    const node = getNodeAtPoint(mx, my);
    if (node) { dragNode = node; }
    else { isPanning = true; panStart = { x: mx, y: my }; }
  });

  canvas.addEventListener('mouseup', e => {
    const rect = canvas.getBoundingClientRect();
    const mx = e.clientX - rect.left, my = e.clientY - rect.top;
    if (dragNode) { dragNode.fx = null; dragNode.fy = null; }
    if (!isPanning || (Math.abs(mx - (panStart?.x || 0)) < 5)) {
      const node = getNodeAtPoint(mx, my);
      if (node && node.type === 'tecnica') openFicha(node.id);
      else if (node && node.type === 'grupo') { highlightedNode = highlightedNode?.id === node.id ? null : node; drawNetwork(); }
    }
    dragNode = null; isPanning = false; panStart = null;
  });

  canvas.addEventListener('mouseleave', () => {
    hoveredNode = null; dragNode = null; isPanning = false;
    tooltip.classList.remove('visible'); drawNetwork();
  });

  canvas.addEventListener('wheel', e => {
    e.preventDefault();
    const rect = canvas.getBoundingClientRect();
    const mx = e.clientX - rect.left, my = e.clientY - rect.top;
    const delta = e.deltaY > 0 ? 0.88 : 1.14;
    const newK  = Math.max(0.3, Math.min(3, transform.k * delta));
    transform.x = mx - (mx - transform.x) * (newK / transform.k);
    transform.y = my - (my - transform.y) * (newK / transform.k);
    transform.k = newK; drawNetwork();
  }, { passive: false });

  document.getElementById('net-zoom-in').addEventListener('click',  () => { transform.k = Math.min(3, transform.k * 1.25); drawNetwork(); });
  document.getElementById('net-zoom-out').addEventListener('click', () => { transform.k = Math.max(0.3, transform.k * 0.8); drawNetwork(); });
  document.getElementById('net-reset').addEventListener('click',    () => { transform = { x: 0, y: 0, k: 1 }; drawNetwork(); });
}

function renderNetworkFilters() {
  const wrap = document.getElementById('network-filters');
  wrap.innerHTML = NET_FILTERS.map(f =>
    `<button class="filter-btn ${f.id === 'all' ? 'active' : ''}" data-netfilter="${f.id}">${f.label}</button>`
  ).join('');
  wrap.querySelectorAll('.filter-btn').forEach(btn => {
    btn.addEventListener('click', () => {
      wrap.querySelectorAll('.filter-btn').forEach(b => b.classList.remove('active'));
      btn.classList.add('active');
      activeNetworkFilter = btn.dataset.netfilter;
      highlightedNode = null; drawNetwork();
    });
  });
}

function renderNetworkLegend() {
  const wrap  = document.getElementById('network-legend-items');
  const grupos = [...new Set(ATLAS.tecnicas.map(t => t.grupo).filter(Boolean))];
  wrap.innerHTML = grupos.map(g => {
    const color = GRUPO_COLOR[g] || COLORS.arena;
    return `<div class="net-legend-item" data-grupo="${g}">
      <div class="net-legend-dot" style="background:${color}"></div>${g}</div>`;
  }).join('');
  wrap.querySelectorAll('.net-legend-item').forEach(item => {
    item.addEventListener('click', () => {
      const g    = item.dataset.grupo;
      const node = networkNodes.find(n => n.type === 'grupo' && n.label === g);
      if (node) {
        highlightedNode = highlightedNode?.id === node.id ? null : node;
        item.classList.toggle('filtered', !!highlightedNode); drawNetwork();
      }
    });
  });
}

function highlightNetworkNode(tecnicaNombre) {
  const node = networkNodes.find(n => n.id === tecnicaNombre);
  if (node) {
    highlightedNode = node;
    const W = networkCanvas.width, H = networkCanvas.height;
    transform.x = W / 2 - node.x * transform.k;
    transform.y = H / 2 - node.y * transform.k;
    drawNetwork();
  }
}

/* ────────────────────────────────────────────────
   DRAWER — REPORTE "ACERCA DEL ATLAS"
──────────────────────────────────────────────── */
let reportCharsInited = false;

const REPORT_HTML = `<nav class="report-internal-nav" id="report-internal-nav">
  <a href="#r-metodologia">Metodología</a>
  <a href="#r-participacion">Participación</a>
  <a href="#r-distribucion">Distribución</a>
  <a href="#r-tecnicas">Técnicas</a>
  <a href="#r-tenido">Teñido</a>
  <a href="#r-transmision">Transmisión</a>
</nav>
<!-- ============================================================
     INTRO
     ============================================================ -->
<div id="intro">
  <div class="intro-eyebrow">Reporte de registros</div>
  <h1 class="intro-title">Aplicación y<br><em>metodología</em></h1>
  <div class="intro-divider"></div>
  <p class="intro-lead">
    La propuesta del <strong>Atlas De Técnicas De Artes Textiles En ORIGINAL</strong> surgió ante la necesidad de reconocer, documentar y fortalecer la diversidad de técnicas textiles que existen en México, muchas de las cuales están en riesgo por la falta de registro, la desvalorización cultural y el debilitamiento de los procesos de transmisión intergeneracional.
  </p>
  <p class="intro-lead">
    Impulsada en el marco del Encuentro Nacional de Artes Textiles ORIGINAL, esta iniciativa representa una oportunidad para promover formas <strong>participativas de consulta y registro</strong> que contribuyan a la salvaguardia de técnicas artesanales en todo el país, aprovechando la amplia convocatoria del evento.
  </p>
  <div class="callout">
    Este atlas no pretende ser exhaustivo, sino representar una muestra de la diversidad técnica del arte textil en México. Su estructura permanece abierta y podrá enriquecerse con futuras experiencias, registros y actividades.
  </div>
</div>

<div class="report-wrap">

  <!-- ============================================================
       01 METODOLOGÍA
       ============================================================ -->
  <section class="section-block" id="r-metodologia">
    <div class="section-eyebrow">01 — Técnicas del arte textil</div>
    <h2 class="section-title">Las tres grandes categorías</h2>
    <p class="section-lead">Son los diversos métodos usados en la elaboración y manipulación de telas y tejidos, esenciales para producir prendas de vestir, decoración de interiores y otros productos textiles.</p>

    <div class="tree-grid">
      <div class="tree-card">
        <div class="tree-card-header">
          <div class="tree-card-dot" style="background:var(--magenta)"></div>
          <span class="tree-card-label">Bordado</span>
        </div>
        <p class="tree-card-desc">Actividad artística sobre telas tejidas con agujas e hilos que forman motivos decorativos. Pueden incorporarse perlas, abalorios o lentejuelas.</p>
        <div class="tree-item">Bordado a mano</div>
        <div class="tree-item">Bordado a máquina de pedal</div>
        <div class="tree-item">Bordado a máquina de motor</div>
        <div class="tree-item tree-sub">+300 tipos de puntadas en el mundo</div>
      </div>
      <div class="tree-card">
        <div class="tree-card-header">
          <div class="tree-card-dot" style="background:var(--azul-mar)"></div>
          <span class="tree-card-label">Tejido</span>
        </div>
        <p class="tree-card-desc">Técnicas que implican el entrelazado de hilos para crear prendas o telas, a mano o con telares tradicionales o mecánicos.</p>
        <div class="tree-item">Tejido a mano (punto y crochet)</div>
        <div class="tree-item">Telar de cintura</div>
        <div class="tree-item">Telar de pedal / mecánico</div>
        <div class="tree-item">Telar de cajón</div>
      </div>
      <div class="tree-card">
        <div class="tree-card-header">
          <div class="tree-card-dot" style="background:var(--verde)"></div>
          <span class="tree-card-label">Teñido</span>
        </div>
        <p class="tree-card-desc">Técnica transversal que añade color a los tejidos. Puede incorporarse a cualquier práctica de bordado o tejido.</p>
        <div class="tree-item">Tintes naturales (plantas)</div>
        <div class="tree-item">Sales y minerales</div>
        <div class="tree-item">Productos animales</div>
        <div class="tree-item">Tintes sintéticos</div>
      </div>
    </div>

    <h2 class="section-title" style="margin-top:3rem">Recolección de datos</h2>
    <p class="section-lead">
      Se diseñó una <strong>Ficha de Registro de Técnicas Textiles</strong> en plataforma digital, aplicada en campo mediante dispositivos móviles en tres sedes: <strong>CDMX, Mérida y Tijuana</strong>. La ficha incluyó datos generales de la persona entrevistada, identificación de la técnica, ubicación geográfica, características técnicas, contexto cultural, prendas y transmisión del conocimiento.
    </p>
    <p class="section-lead">
      El levantamiento se acompañó de una <strong>actividad de cartografía participativa</strong> en la que las y los asistentes colocaban en un mapa las técnicas que iban registrando, permitiendo visualizar la diversidad geográfica de los saberes compartidos.
    </p>
  </section>

  <!-- ============================================================
       02 PARTICIPACIÓN
       ============================================================ -->
  <section class="section-block" id="r-participacion">
    <div class="section-eyebrow">02 — Participación</div>
    <h2 class="section-title">¿Quiénes participaron?</h2>
    <p class="section-lead">Se recibieron un total de <strong>638 aportaciones</strong>, correspondientes a <strong>516 participantes</strong> provenientes de <strong>29 estados</strong> y <strong>205 municipios</strong> del país, con técnicas vinculadas a <strong>31 lenguas indígenas</strong>.</p>

    <!-- Sede + Género -->
    <div class="two-col">
      <div class="chart-card">
        <div class="chart-card-title">Participantes por sede</div>
        <div class="chart-card-sub">Personas únicas registradas en cada sede del evento</div>
        <canvas id="r-chart-sede" height="210"></canvas>
      </div>

      <div class="chart-card">
        <div class="chart-card-title">Distribución por género</div>
        <div class="chart-card-sub">Porcentaje de participantes según género declarado</div>
        <div class="donut-wrap" style="margin-top:8px">
          <div class="donut-canvas-wrap">
            <canvas id="r-chart-genero" height="200"></canvas>
          </div>
          <div class="donut-legend">
            <div class="donut-legend-item">
              <div class="donut-legend-dot" style="background:var(--magenta)"></div>
              <span class="donut-legend-name">Mujer</span>
              <span class="donut-legend-val">80.9 %</span>
            </div>
            <div class="donut-legend-item">
              <div class="donut-legend-dot" style="background:var(--rosa)"></div>
              <span class="donut-legend-name">Hombre</span>
              <span class="donut-legend-val">18.5 %</span>
            </div>
            <div class="donut-legend-item">
              <div class="donut-legend-dot" style="background:var(--gris)"></div>
              <span class="donut-legend-name">No especificó</span>
              <span class="donut-legend-val">0.6 %</span>
            </div>
          </div>
        </div>
      </div>
    </div>

    <!-- Participantes por estado -->
    <div class="chart-card-full">
      <div class="chart-card-title">Participantes por estado de origen</div>
      <div class="chart-card-sub">Top 15 estados con mayor número de participantes únicos registrados</div>
      <canvas id="r-chart-part-estado" height="320"></canvas>
    </div>

    <!-- Lenguas -->
    <div class="chart-card-full">
      <div class="chart-card-title">Participantes por lengua indígena</div>
      <div class="chart-card-sub">Las 12 lenguas con mayor número de portadores registrados en el Atlas</div>
      <canvas id="r-chart-lengua" height="260"></canvas>
    </div>
  </section>

  <!-- ============================================================
       03 DISTRIBUCIÓN GEOGRÁFICA
       ============================================================ -->
  <section class="section-block" id="r-distribucion">
    <div class="section-eyebrow">03 — Distribución geográfica</div>
    <h2 class="section-title">El mapa del textil mexicano</h2>
    <p class="section-lead">Ranking de estados con mayor diversidad de técnicas distintas reportadas. <strong>Oaxaca, Chiapas y Puebla</strong> concentran la mayor riqueza técnica registrada en este ejercicio.</p>

    <div class="chart-card-full">
      <div class="chart-card-title">Técnicas distintas por estado</div>
      <div class="chart-card-sub">Número de técnicas únicas registradas en cada entidad federativa</div>
      <canvas id="r-chart-tec-estado" height="320"></canvas>
    </div>

    <div class="chart-card-full">
      <div class="chart-card-title">Municipios con mayor número de técnicas</div>
      <div class="chart-card-sub">Top 20 municipios con más técnicas distintas registradas</div>
      <canvas id="r-chart-municipios" height="320"></canvas>
    </div>

    <!-- Tabla -->
    <div class="chart-card-full" style="margin-top:2rem">
      <div class="chart-card-title">Técnica más representativa por estado</div>
      <div class="chart-card-sub" style="margin-bottom:14px">Para cada estado, la técnica con mayor número de registros</div>
      <div class="table-search-wrap">
        <input class="table-search" type="text" id="r-table-search" placeholder="Buscar estado o técnica…" oninput="filterTable()">
      </div>
      <div class="data-table-wrap">
        <table class="data-table" id="r-tec-table">
          <thead>
            <tr>
              <th>Estado</th>
              <th>Técnica más común</th>
              <th style="text-align:center">Registros</th>
            </tr>
          </thead>
          <tbody id="r-table-body"></tbody>
        </table>
      </div>
    </div>
  </section>

  <!-- ============================================================
       04 TÉCNICAS
       ============================================================ -->
  <section class="section-block" id="r-tecnicas">
    <div class="section-eyebrow">04 — Características de las técnicas</div>
    <h2 class="section-title">Técnicas registradas</h2>
    <p class="section-lead">Se identificaron <strong>161 técnicas distintas</strong> en todo el ejercicio. Las más reportadas son el <strong>Telar de cintura</strong> y el <strong>Punto de Cruz</strong>, presentes en múltiples estados y lenguas indígenas.</p>

    <div class="chart-card-full">
      <div class="chart-card-title">Las 20 técnicas más frecuentes</div>
      <div class="chart-card-sub">Número total de registros por técnica en el Atlas</div>
      <canvas id="r-chart-top-tec" height="400"></canvas>
    </div>

    <div class="two-col">
      <div class="chart-card">
        <div class="chart-card-title">Grupo de técnica general</div>
        <div class="chart-card-sub">Agrupación según tipo de manufactura artesanal</div>
        <canvas id="r-chart-grupos" height="260"></canvas>
      </div>
      <div class="chart-card">
        <div class="chart-card-title">Tipo de manufactura</div>
        <div class="chart-card-sub">Forma en que se realiza la técnica reportada</div>
        <canvas id="r-chart-manufactura" height="260"></canvas>
      </div>
    </div>

    <div class="chart-card-full">
      <div class="chart-card-title">Distribución de técnicas por lengua indígena</div>
      <div class="chart-card-sub">Top 10 técnicas entre las 10 lenguas con más registros (barras apiladas)</div>
      <canvas id="r-chart-lengua-tec" height="360"></canvas>
    </div>
  </section>

  <!-- ============================================================
       05 TEÑIDO
       ============================================================ -->
  <section class="section-block" id="r-tenido">
    <div class="section-eyebrow">05 — Teñido</div>
    <h2 class="section-title">Prácticas de teñido</h2>
    <p class="section-lead">El teñido es una técnica <strong>transversal</strong> que puede incorporarse al bordado o al tejido. La mayoría de los artesanos reportaron el uso de <strong>tintes naturales de plantas</strong> como práctica predominante, seguido de insectos y animales.</p>

    <div class="two-col">
      <div class="chart-card">
        <div class="chart-card-title">Tipos de teñido registrados</div>
        <div class="chart-card-sub">Distribución general de los métodos de teñido</div>
        <div class="donut-wrap" style="margin-top:8px">
          <div class="donut-canvas-wrap">
            <canvas id="r-chart-tenido-donut" height="200"></canvas>
          </div>
          <div class="donut-legend">
            <div class="donut-legend-item">
              <div class="donut-legend-dot" style="background:var(--verde)"></div>
              <span class="donut-legend-name">Con plantas</span>
              <span class="donut-legend-val">222</span>
            </div>
            <div class="donut-legend-item">
              <div class="donut-legend-dot" style="background:var(--naranja)"></div>
              <span class="donut-legend-name">Insectos / animales</span>
              <span class="donut-legend-val">86</span>
            </div>
            <div class="donut-legend-item">
              <div class="donut-legend-dot" style="background:var(--ocre)"></div>
              <span class="donut-legend-name">Sales y minerales</span>
              <span class="donut-legend-val">79</span>
            </div>
            <div class="donut-legend-item">
              <div class="donut-legend-dot" style="background:var(--gris)"></div>
              <span class="donut-legend-name">Con otro</span>
              <span class="donut-legend-val">68</span>
            </div>
          </div>
        </div>
      </div>

      <div class="chart-card">
        <div class="chart-card-title">Frecuencia por tipo de teñido</div>
        <div class="chart-card-sub">Número de registros que reportan cada método</div>
        <canvas id="r-chart-tenido-bar" height="260"></canvas>
      </div>
    </div>
  </section>

  <!-- ============================================================
       06 TRANSMISIÓN
       ============================================================ -->
  <section class="section-block" id="r-transmision">
    <div class="section-eyebrow">06 — Transmisión del conocimiento</div>
    <h2 class="section-title">¿Cómo se aprende y se enseña?</h2>
    <p class="section-lead">La transmisión de los saberes textiles ocurre principalmente dentro del <strong>núcleo familiar</strong>. La madre es la figura más frecuente en el proceso de aprendizaje, seguida de tías y abuelas, lo que evidencia el papel central de las mujeres como portadoras y transmisoras de este patrimonio.</p>

    <div class="chart-card-full">
      <div class="chart-card-title">Fuentes de aprendizaje de la técnica</div>
      <div class="chart-card-sub">¿De quién aprendió esta técnica? (respuesta múltiple posible)</div>
      <canvas id="r-chart-aprendizaje" height="260"></canvas>
    </div>

    <div class="callout">
      El aprendizaje intergeneracional —especialmente de madres a hijas— constituye el principal mecanismo de transmisión del patrimonio textil. La instructora formal representa una vía complementaria, especialmente en contextos urbanos.
    </div>
  </section>

</div><!-- /report-wrap -->`;

function openReport() {
  const drawer  = document.getElementById('report-drawer');
  const overlay = document.getElementById('report-overlay');

  // Inject content only once
  if (!reportCharsInited) {
    document.getElementById('report-drawer-body').innerHTML = REPORT_HTML;
    initReportCharts();
    initReportScrollSpy();
    reportCharsInited = true;
  }

  drawer.classList.add('open');
  overlay.classList.add('open');
  document.body.style.overflow = 'hidden';
}

function closeReport() {
  document.getElementById('report-drawer').classList.remove('open');
  document.getElementById('report-overlay').classList.remove('open');
  document.body.style.overflow = '';
}

// Escape key closes drawer
document.addEventListener('keydown', e => { if (e.key === 'Escape') closeReport(); });

function initReportScrollSpy() {
  const drawerBody = document.getElementById('report-drawer-body');
  const sections   = ['r-metodologia','r-participacion','r-distribucion','r-tecnicas','r-tenido','r-transmision'];

  // Scroll reveal for section-blocks
  const observer = new IntersectionObserver(entries => {
    entries.forEach(e => { if (e.isIntersecting) e.target.classList.add('visible'); });
  }, { root: drawerBody, threshold: 0.07 });
  document.querySelectorAll('.section-block').forEach(s => observer.observe(s));

  // Scroll spy for internal nav
  drawerBody.addEventListener('scroll', () => {
    let current = sections[0];
    for (const id of sections) {
      const el = document.getElementById(id);
      if (el && el.getBoundingClientRect().top < 160) current = id;
    }
    const nav = document.getElementById('report-internal-nav');
    if (nav) {
      nav.querySelectorAll('a').forEach(a => {
        a.classList.toggle('active', a.getAttribute('href') === '#' + current);
      });
    }
  });

  // Smooth scroll for internal nav links (scroll inside drawer)
  const nav = document.getElementById('report-internal-nav');
  if (nav) {
    nav.querySelectorAll('a').forEach(a => {
      a.addEventListener('click', e => {
        e.preventDefault();
        const id = a.getAttribute('href').replace('#', '');
        const target = document.getElementById(id);
        if (target) target.scrollIntoView({ behavior: 'smooth', block: 'start' });
      });
    });
  }
}

function initReportCharts() {

/* ============================================================
   PROGRESO + SCROLL REVEAL
   ============================================================ */
window.addEventListener('scroll', () => {
  const top = document.documentElement.scrollTop;
  const h = document.documentElement.scrollHeight - window.innerHeight;
  document.getElementById('progress-bar').style.width = (top / h * 100) + '%';

  // Nav active tab
  const sections = ['metodologia','participacion','distribucion','tecnicas','tenido','transmision'];
  let current = sections[0];
  for (const id of sections) {
    const el = document.getElementById(id);
    if (el && el.getBoundingClientRect().top < 120) current = id;
  }
  document.querySelectorAll('.nav-tab').forEach(t => {
    t.classList.toggle('active', t.getAttribute('href') === '#' + current);
  });
});

const observer = new IntersectionObserver(entries => {
  entries.forEach(e => { if (e.isIntersecting) e.target.classList.add('visible'); });
}, { threshold: 0.07 });
document.querySelectorAll('.section-block').forEach(s => observer.observe(s));

/* ============================================================
   CHART.JS DEFAULTS — paleta crema (fondo claro)
   ============================================================ */
Chart.defaults.color = '#8C7A80';
Chart.defaults.borderColor = 'rgba(26,16,24,.08)';
Chart.defaults.font.family = "'DM Sans', sans-serif";

const MAGENTA  = '#B50552';
const AZUL     = '#035A79';
const NARANJA  = '#FB4801';
const VERDE    = '#05B794';
const OCRE     = '#FFA329';
const CYAN     = '#00B2C0';
const ROSA     = '#DE599B';
const ARENA    = '#8C7A80';

// Rampa monocromática magenta
const rampMagenta = n => Array.from({length:n}, (_,i) => {
  const t = i / Math.max(n-1,1);
  // claro → oscuro: #DE599B → #B50552
  const r = Math.round(0xDE + t*(0xB5-0xDE));
  const g = Math.round(0x59 + t*(0x05-0x59));
  const b = Math.round(0x9B + t*(0x52-0x9B));
  return `rgb(${r},${g},${b})`;
});

const MULTI = [MAGENTA, AZUL, VERDE, OCRE, NARANJA, CYAN, ROSA, '#7F8C8D', '#9B59B6', ARENA];

const baseOpts = (indexAxis='x') => ({
  indexAxis,
  responsive: true,
  plugins: { legend: { display: false } },
  scales: {
    x: { grid: { color: 'rgba(26,16,24,.06)' } },
    y: { grid: { display: indexAxis==='y' ? false : true, color: 'rgba(26,16,24,.06)' } }
  }
});

/* ── SEDE ── */
new Chart(document.getElementById('r-chart-sede'), {
  type: 'bar',
  data: {
    labels: ['CDMX', 'Mérida', 'Tijuana'],
    datasets: [{ data: [264,150,102], backgroundColor:[MAGENTA, ROSA, '#C8B8C0'], borderWidth:0, borderRadius:4 }]
  },
  options: { ...baseOpts(), plugins:{ legend:{display:false} } }
});

/* ── GÉNERO ── */
new Chart(document.getElementById('r-chart-genero'), {
  type: 'doughnut',
  data: {
    labels: ['Mujer','Hombre','No especificó'],
    datasets: [{ data:[516,118,1], backgroundColor:[MAGENTA, ROSA, ARENA], borderWidth:3, borderColor:'#FFF8F5' }]
  },
  options: { cutout:'68%', plugins:{ legend:{display:false} } }
});

/* ── PARTICIPANTES POR ESTADO ── */
const partEstado = [['Oaxaca',115],['Chiapas',88],['Yucatán',42],['Michoacán',35],['Puebla',35],['Guerrero',30],['Veracruz',30],['México',23],['Hidalgo',15],['Chihuahua',14],['Tlaxcala',13],['Morelos',11],['Campeche',10],['Sonora',7],['Jalisco',7]];
new Chart(document.getElementById('r-chart-part-estado'), {
  type: 'bar',
  data: {
    labels: partEstado.map(d=>d[0]),
    datasets: [{ data: partEstado.map(d=>d[1]), backgroundColor: rampMagenta(partEstado.length), borderWidth:0, borderRadius:3 }]
  },
  options: { ...baseOpts('y') }
});

/* ── LENGUA ── */
const lenguaData = [['Náhuatl',86],['Tsotsil',71],['Maya Yucateco',66],['Otomí',27],['Purépecha',26],['Zapoteco',26],['Tseltal',23],['Amuzgo',20],['Mazateco',19],['Mixteco',15],['Huave',10],['Chinanteco',8]];
new Chart(document.getElementById('r-chart-lengua'), {
  type: 'bar',
  data: {
    labels: lenguaData.map(d=>d[0]),
    datasets: [{ data: lenguaData.map(d=>d[1]), backgroundColor: MAGENTA, borderWidth:0, borderRadius:3 }]
  },
  options: { ...baseOpts() }
});

/* ── TEC POR ESTADO ── */
const tecEstado = [['Oaxaca',43],['Chiapas',37],['Puebla',29],['México',19],['Yucatán',18],['Veracruz',17],['Tlaxcala',13],['Chihuahua',12],['Hidalgo',11],['Michoacán',11],['Guerrero',10],['Campeche',6],['Querétaro',5],['Morelos',4],['Sonora',4]];
new Chart(document.getElementById('r-chart-tec-estado'), {
  type: 'bar',
  data: {
    labels: tecEstado.map(d=>d[0]),
    datasets: [{ data: tecEstado.map(d=>d[1]), backgroundColor: rampMagenta(tecEstado.length), borderWidth:0, borderRadius:3 }]
  },
  options: { ...baseOpts('y') }
});

/* ── MUNICIPIOS ── */
const munData = [['Larráinzar',20],['Hueyapan',20],['Venustiano Carranza',15],['Cuetzalan del Progreso',15],['Villa de Allende',14],['Paracho',11],['Chamula',11],['Xochistlahuaca',10],['Huautla de Jiménez',10],['Maní',9],['Zinacantán',9],['Tenango de Doria',8],['Ometepec',8],['S.B. Ayautla',8],['Aldama',7],['S.A. Castillo Velasco',7],['Teabo',6],['Atlequizayan',6],['Sol. Atzompa',6],['Bocoyna',6]];
new Chart(document.getElementById('r-chart-municipios'), {
  type: 'bar',
  data: {
    labels: munData.map(d=>d[0]),
    datasets: [{ data: munData.map(d=>d[1]), backgroundColor: AZUL, borderWidth:0, borderRadius:3 }]
  },
  options: {
    plugins: { legend: {display:false} },
    scales: {
      x: { grid:{display:false}, ticks:{color:ARENA, maxRotation:45, font:{size:11}} },
      y: { grid:{color:'rgba(26,16,24,.06)'} }
    }
  }
});

/* ── TABLA ── */
const tableData = [
  ['Aguascalientes','Deshilado',4],['Baja California','Trencilla',1],['Campeche','Punto de Cruz',8],
  ['Chiapas','Telar de cintura',45],['Chihuahua','Bordado Rarámuri',2],['Ciudad de México','Telar de cintura',1],
  ['Coahuila','Sarape Fino de Saltillo',2],['Colima','Punto de Cruz',1],['Durango','Punto de Cruz',3],
  ['Guanajuato','Contabilidad y vuelta por hilo',2],['Guerrero','Telar de cintura',13],['Hidalgo','Telar de cintura',4],
  ['Jalisco','Punto de Cruz',4],['Michoacán','Telar de cintura',15],['Morelos','Telar de cintura',7],
  ['México','Bordado Mazahua',3],['Nayarit','Telar de cintura',1],['Oaxaca','Telar de cintura',33],
  ['Puebla','Telar de cintura',12],['Querétaro','Punto de Cruz',3],['Quintana Roo','Punto de Cruz',2],
  ['Sinaloa','Telar de cintura',1],['Sonora','Cadeneta o cadenilla',4],['Tabasco','Lomillo',2],
  ['Tamaulipas','Punto de Cruz',1],['Tlaxcala','Telar de pedal',2],['Veracruz','Telar de cintura',12],
  ['Yucatán','Punto de Cruz',18],['Zacatecas','Telar de pedal',2]
];

function renderTable(data) {
  document.getElementById('r-table-body').innerHTML = data.map(([e,t,n]) => `
    <tr>
      <td style="color:var(--tinta);font-weight:500">${e}</td>
      <td><span class="tec-badge">${t}</span></td>
      <td style="text-align:center"><span class="num-badge">${n}</span></td>
    </tr>`).join('');
}
function filterTable() {
  const q = document.getElementById('r-table-search').value.toLowerCase();
  renderTable(tableData.filter(([e,t]) => e.toLowerCase().includes(q) || t.toLowerCase().includes(q)));
}
renderTable(tableData);

/* ── TOP TÉCNICAS ── */
const topTec = [['Telar de cintura',146],['Punto de Cruz',89],['Hilván',27],['Telar de pedal',24],['Cadeneta o cadenilla',23],['Brocado',19],['Macizo',13],['Pepenado',13],['Deshilado',13],['Relleno',12],['Lomillo',10],['Pata de gallo',9],['Rejilla',7],['Bordado tradicional',7],['Rococó',6],['Gasa fina',6],['Contabilidad/vuelta',5],['Xmanikté',4],['Tafetán',4],['Bordado de chaquira',4]];
new Chart(document.getElementById('r-chart-top-tec'), {
  type: 'bar',
  data: {
    labels: topTec.map(d=>d[0]),
    datasets: [{ data: topTec.map(d=>d[1]), backgroundColor: rampMagenta(topTec.length), borderWidth:0, borderRadius:3 }]
  },
  options: { ...baseOpts('y') }
});

/* ── GRUPOS ── */
new Chart(document.getElementById('r-chart-grupos'), {
  type: 'doughnut',
  data: {
    labels: ['Bordado a mano','Telar de cintura','Bordado a máquina','Tejido a mano','Telar de pedal','Telar de cajón'],
    datasets: [{ data:[184,91,36,15,15,1], backgroundColor:[MAGENTA,AZUL,ROSA,CYAN,VERDE,ARENA], borderWidth:3, borderColor:'#FFF8F5' }]
  },
  options: { cutout:'52%', plugins:{ legend:{ position:'bottom', labels:{ color:ARENA, font:{size:11}, padding:10 } } } }
});

/* ── MANUFACTURA ── */
const manData = [['A mano',370],['Telar cintura',232],['Máquina/pedal',56],['Mixta',40],['Tejido a mano',34],['Otra',26],['Telar pedal',25]];
new Chart(document.getElementById('r-chart-manufactura'), {
  type: 'bar',
  data: {
    labels: manData.map(d=>d[0]),
    datasets: [{ data: manData.map(d=>d[1]), backgroundColor:[MAGENTA,AZUL,ROSA,OCRE,CYAN,ARENA,VERDE], borderWidth:0, borderRadius:3 }]
  },
  options: {
    plugins:{legend:{display:false}},
    scales:{
      x:{grid:{display:false}, ticks:{color:ARENA, maxRotation:35, font:{size:11}}},
      y:{grid:{color:'rgba(26,16,24,.06)'}}
    }
  }
});

/* ── TÉCNICAS POR LENGUA ── */
const topTecNames = ['Telar de cintura','Punto de Cruz','Hilván','Telar de pedal','Cadeneta','Brocado','Macizo','Pepenado','Deshilado','Relleno'];
const lenguaMatrix = {
  'Náhuatl':      [22,12,7,0,0,0,1,4,1,1],
  'Tsotsil':      [32,1,1,0,2,10,0,0,0,0],
  'Maya Yucateco':[1,27,1,0,1,0,7,0,0,0],
  'Otomí':        [4,4,3,0,0,0,0,3,1,0],
  'Purépecha':    [11,5,0,2,0,1,0,0,2,0],
  'Zapoteco':     [4,2,0,6,3,0,0,0,0,1],
  'Tseltal':      [7,5,0,1,0,2,0,0,0,1],
  'Amuzgo':       [13,1,0,0,0,1,0,0,0,0],
  'Mazateco':     [0,3,5,0,0,0,0,0,0,5],
  'Mixteco':      [6,1,0,1,3,1,0,0,0,0]
};
const lenguaLabels = Object.keys(lenguaMatrix);
new Chart(document.getElementById('r-chart-lengua-tec'), {
  type: 'bar',
  data: {
    labels: lenguaLabels,
    datasets: topTecNames.map((tec,i) => ({
      label: tec,
      data: lenguaLabels.map(l => lenguaMatrix[l][i]),
      backgroundColor: MULTI[i],
      borderWidth: 1, borderColor: '#FFF8F5'
    }))
  },
  options: {
    scales: {
      x: { stacked:true, grid:{display:false}, ticks:{color:ARENA} },
      y: { stacked:true, grid:{color:'rgba(26,16,24,.06)'} }
    },
    plugins: { legend:{ position:'bottom', labels:{ color:ARENA, font:{size:10}, padding:8 } } }
  }
});

/* ── TEÑIDO DONA ── */
new Chart(document.getElementById('r-chart-tenido-donut'), {
  type: 'doughnut',
  data: {
    labels: ['Con plantas','Insectos/animales','Sales y minerales','Con otro'],
    datasets: [{ data:[222,86,79,68], backgroundColor:[VERDE,NARANJA,OCRE,ARENA], borderWidth:3, borderColor:'#FFF8F5' }]
  },
  options: { cutout:'65%', plugins:{legend:{display:false}} }
});

/* ── TEÑIDO BARRAS ── */
new Chart(document.getElementById('r-chart-tenido-bar'), {
  type: 'bar',
  data: {
    labels: ['Con plantas','Insectos/animales','Sales y minerales','Con otro'],
    datasets: [{ data:[222,86,79,68], backgroundColor:[VERDE,NARANJA,OCRE,ARENA], borderWidth:0, borderRadius:4 }]
  },
  options: { plugins:{legend:{display:false}}, scales:{ x:{grid:{display:false}}, y:{grid:{color:'rgba(26,16,24,.06)'}} } }
});

/* ── APRENDIZAJE ── */
new Chart(document.getElementById('r-chart-aprendizaje'), {
  type: 'bar',
  data: {
    labels: ['Madre','Tía','Abuela','Otra persona','Padre','Instructora','Hermana','Cuñada'],
    datasets: [{ data:[413,145,136,47,42,29,28,12], backgroundColor: rampMagenta(8), borderWidth:0, borderRadius:3 }]
  },
  options: { ...baseOpts('y') }
});

}

// ────────────────────────────────────────────────
// ARRANQUE
// ────────────────────────────────────────────────
loadCSVs();
