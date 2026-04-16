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

// Colores por categoría experta (CAT-N-1)
const CAT1_COLOR = {
  'Tejido':               COLORS.azulMar,
  'Técnicas decorativas': COLORS.magenta,
  'Acabados':             COLORS.ocre,
  'Teñidos':              COLORS.verde,
};

// Colores por CAT-N-2 (subcategorías)
const CAT2_COLOR = {
  'Tejidos':       COLORS.cyan,
  'Anudados':      COLORS.azulMar,
  'Tapicería':     '#5B4FCF',
  'Bordados':      COLORS.magenta,
  'Aplicaciones':  COLORS.rosa,
  'Otras':         COLORS.gris,
  'Artesanales':   COLORS.verde,
};

function getCatColor(t) {
  return CAT2_COLOR[t.cat2] || CAT1_COLOR[t.cat1] || COLORS.arena;
}

// Compat: chip CSS class derivada de CAT-N-1
function catChipClass(cat1) {
  const map = {
    'Tejido':               'chip-tejido',
    'Técnicas decorativas': 'chip-decorativas',
    'Acabados':             'chip-acabados',
    'Teñidos':              'chip-tenidos',
  };
  return map[cat1] || 'chip-other';
}

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

    // ── Clasificación experta (CAT-N-1 a 4 desde technique_id) ──────
    const cat1 = (row['CAT-N-1'] || '').trim();
    const cat2 = (row['CAT-N-2'] || '').trim();
    const cat3 = (row['CAT-N-3'] || '').trim();
    const cat4 = (row['CAT-N-4'] || '').trim();

    // ── Desde data_by_RECORD_id (solo campos cualitativos únicos) ─────

    // Grupo derivado de clasificación experta CAT-N-1
    const grupo = cat1 || (records.length > 0 ? (records[0]['tecnica_grupo'] || 'Otras') : 'Otras');

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
      cat1, cat2, cat3, cat4,
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
    // Init map filter panel (needs ATLAS)
    initMapFilter();

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
let arbolInitialized = false;

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
    } else if (id === 'arbol' && !arbolInitialized) {
      arbolInitialized = true; initArbol();
    } else if (id === 'tenido') {
      initTenidoView();
    } else if (id === 'tenido') {
      initTenidoView();
    }
    if (id !== 'red' && _animFrame) { cancelAnimationFrame(_animFrame); _animFrame = null; }
    if (id === 'red' && networkInitialized && !_animFrame) { _simAlpha = 0.05; (function loop(){drawNetwork(); _animFrame=requestAnimationFrame(loop);})(); }
  });
});

// ────────────────────────────────────────────────
// MAP
// ────────────────────────────────────────────────
let map = null;
let geojsonLayer = null;
let selectedStateName = null;

function initMap() {
  map = L.map('map', { zoomControl: false, scrollWheelZoom: true });
  L.control.zoom({ position: 'bottomright' }).addTo(map);
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
            mouseover: e => {
              if (name !== selectedStateName) {
                if (mapFilteredTecnicas) {
                  const base = filteredStateStyle(name);
                  e.target.setStyle({ ...base, weight: 2.5, color: COLORS.negro });
                } else {
                  e.target.setStyle(highlightStyle(feat));
                }
              }
            },
            mouseout: e => {
              if (name !== selectedStateName) {
                if (mapFilteredTecnicas) {
                  e.target.setStyle(filteredStateStyle(name));
                } else {
                  geojsonLayer.resetStyle(e.target);
                }
              }
            },
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
    geojsonLayer.eachLayer(l => {
      const lname = l.feature?.properties?.name || l.feature?.properties?.NAME_1 || '';
      if (mapFilteredTecnicas) {
        l.setStyle(filteredStateStyle(lname));
      } else {
        geojsonLayer.resetStyle(l);
      }
    });
    const count = mapFilteredTecnicas ? countForEstadoFiltered(name) : countForEstado(name);
    layer.setStyle({ weight: 3, color: COLORS.negro, fillOpacity: 0.92, fillColor: getMapColor(count) });
  }
  let tecnicas = getTecnicasForEstado(name);
  if (mapFilteredTecnicas) tecnicas = tecnicas.filter(t => mapFilteredTecnicas.has(t));
  document.getElementById('panel-title').textContent = name;
  document.getElementById('panel-subtitle').textContent =
    tecnicas.length > 0
      ? `${tecnicas.length} técnica${tecnicas.length !== 1 ? 's' : ''} identificada${tecnicas.length !== 1 ? 's' : ''}`
      : 'Sin técnicas que coincidan con el filtro';
  renderSidePanel(tecnicas);
}

function renderSidePanel(tecnicas) {
  const body = document.getElementById('panel-body');
  if (!tecnicas || tecnicas.length === 0) {
    body.innerHTML = `<div class="welcome-state"><h3>Sin registros</h3><p>No se documentaron técnicas en este estado.</p></div>`;
    return;
  }
  // Agrupar por CAT-N-1 → CAT-N-2
  const cat1Map = {};
  tecnicas.forEach(tname => {
    const td  = tecnicasMap[tname];
    const c1  = td ? (td.cat1 || 'Sin clasificar') : 'Sin clasificar';
    const c2  = td ? (td.cat2 || '') : '';
    const key = c1;
    if (!cat1Map[key]) cat1Map[key] = {};
    const sub = c2 || '_';
    if (!cat1Map[key][sub]) cat1Map[key][sub] = [];
    cat1Map[key][sub].push(tname);
  });
  let html = '';
  Object.keys(cat1Map).sort().forEach(c1 => {
    const color1   = CAT1_COLOR[c1] || COLORS.arena;
    const chipCls  = catChipClass(c1);
    html += `<div class="grupo-section">
      <div class="grupo-label" style="color:${color1}">${esc(c1)}</div>`;
    Object.keys(cat1Map[c1]).sort().forEach(c2 => {
      if (c2 !== '_') {
        const color2 = CAT2_COLOR[c2] || color1;
        html += `<div class="cat2-label" style="color:${color2}">${esc(c2)}</div>`;
      }
      html += `<div class="chips-wrap">`;
      cat1Map[c1][c2].forEach(tname => {
        const td = tecnicasMap[tname];
        const hasImg = td && td.imagenes && td.imagenes.length > 0;
        html += `<span class="tecnica-chip ${chipCls}" onclick="openFicha('${escJs(tname)}')">
          ${hasImg ? '<span class="has-img-dot"></span>' : ''}${esc(tname)}</span>`;
      });
      html += `</div>`;
    });
    html += `</div>`;
  });
  body.innerHTML = html;
}

// ────────────────────────────────────────────────
// MAP FILTER — filtro jerárquico por categoría y técnica
// ────────────────────────────────────────────────
let mapFilterState = { cat1: null, cat2: null, cat3: null, cat4: null, tecnica: null };
let mapFilteredTecnicas = null; // Set de nombres de técnica activas, null = todas

function initMapFilter() {
  if (!ATLAS) return;
  // Construir los chips de CAT-N-1
  const cat1s = [...new Set(ATLAS.tecnicas.map(t => t.cat1).filter(Boolean))].sort();
  const wrap1 = document.getElementById('map-chips-cat1');
  if (!wrap1) return;
  wrap1.innerHTML = '';
  cat1s.forEach(c1 => {
    const color = CAT1_COLOR[c1] || COLORS.arena;
    const btn = document.createElement('button');
    btn.className = 'map-chip';
    btn.dataset.val = c1;
    btn.textContent = c1;
    btn.style.setProperty('--chip-color', color);
    btn.addEventListener('click', () => onMapChipClick('cat1', c1));
    wrap1.appendChild(btn);
  });

  // Autocomplete
  const searchEl = document.getElementById('map-filter-search');
  const acEl = document.getElementById('map-filter-autocomplete');
  if (searchEl) {
    searchEl.addEventListener('input', function() {
      const q = this.value.toLowerCase().trim();
      acEl.innerHTML = '';
      acEl.style.display = 'none';
      if (q.length < 2) return;
      const matches = ATLAS.tecnicas
        .filter(t => t.tecnica.toLowerCase().includes(q))
        .slice(0, 10);
      if (!matches.length) return;
      matches.forEach(t => {
        const item = document.createElement('div');
        item.className = 'map-ac-item';
        item.innerHTML = `<span class="map-ac-name">${esc(t.tecnica)}</span>
          <span class="map-ac-cat" style="color:${CAT1_COLOR[t.cat1]||COLORS.arena}">${esc(t.cat1||'')}</span>`;
        item.addEventListener('click', () => {
          searchEl.value = t.tecnica;
          acEl.style.display = 'none';
          selectTecnicaOnMap(t.tecnica);
        });
        acEl.appendChild(item);
      });
      acEl.style.display = 'block';
    });
    document.addEventListener('click', e => {
      if (!searchEl.contains(e.target) && !acEl.contains(e.target)) acEl.style.display = 'none';
    });
  }
}

function onMapChipClick(level, val) {
  // Toggle
  if (mapFilterState[level] === val) {
    // Deselect — reset to parent level
    mapFilterState[level] = null;
    if (level === 'cat1') { mapFilterState.cat2 = null; mapFilterState.cat3 = null; mapFilterState.cat4 = null; mapFilterState.tecnica = null; }
    if (level === 'cat2') { mapFilterState.cat3 = null; mapFilterState.cat4 = null; mapFilterState.tecnica = null; }
    if (level === 'cat3') { mapFilterState.cat4 = null; mapFilterState.tecnica = null; }
    if (level === 'cat4') { mapFilterState.tecnica = null; }
  } else {
    mapFilterState[level] = val;
    // Reset children
    if (level === 'cat1') { mapFilterState.cat2 = null; mapFilterState.cat3 = null; mapFilterState.cat4 = null; mapFilterState.tecnica = null; }
    if (level === 'cat2') { mapFilterState.cat3 = null; mapFilterState.cat4 = null; mapFilterState.tecnica = null; }
    if (level === 'cat3') { mapFilterState.cat4 = null; mapFilterState.tecnica = null; }
  }
  rebuildMapFilterCascade();
  applyMapFilter();
}

function selectTecnicaOnMap(nombre) {
  mapFilterState = { cat1: null, cat2: null, cat3: null, cat4: null, tecnica: nombre };
  const t = tecnicasMap[nombre];
  if (t) {
    mapFilterState.cat1 = t.cat1;
    mapFilterState.cat2 = t.cat2;
    mapFilterState.cat3 = t.cat3;
    mapFilterState.cat4 = t.cat4;
  }
  rebuildMapFilterCascade();
  applyMapFilter();
}

function clearMapFilter() {
  mapFilterState = { cat1: null, cat2: null, cat3: null, cat4: null, tecnica: null };
  const search = document.getElementById('map-filter-search');
  if (search) search.value = '';
  rebuildMapFilterCascade();
  applyMapFilter();
}

function rebuildMapFilterCascade() {
  const { cat1, cat2, cat3 } = mapFilterState;

  // Highlight active chip level 1
  document.querySelectorAll('#map-chips-cat1 .map-chip').forEach(b => {
    b.classList.toggle('active', b.dataset.val === cat1);
  });

  // CAT-N-2
  const level2 = document.getElementById('map-level-cat2');
  const chips2 = document.getElementById('map-chips-cat2');
  if (cat1 && chips2) {
    const cat2s = [...new Set(ATLAS.tecnicas.filter(t => t.cat1 === cat1 && t.cat2).map(t => t.cat2))].sort();
    if (cat2s.length) {
      chips2.innerHTML = '';
      cat2s.forEach(c2 => {
        const color = CAT2_COLOR[c2] || CAT1_COLOR[cat1] || COLORS.arena;
        const btn = document.createElement('button');
        btn.className = 'map-chip' + (c2 === cat2 ? ' active' : '');
        btn.dataset.val = c2; btn.textContent = c2;
        btn.style.setProperty('--chip-color', color);
        btn.addEventListener('click', () => onMapChipClick('cat2', c2));
        chips2.appendChild(btn);
      });
      level2.style.display = '';
    } else { level2.style.display = 'none'; }
  } else { if (level2) level2.style.display = 'none'; }

  // CAT-N-3
  const level3 = document.getElementById('map-level-cat3');
  const chips3 = document.getElementById('map-chips-cat3');
  if (cat2 && chips3) {
    const cat3s = [...new Set(ATLAS.tecnicas.filter(t => t.cat2 === cat2 && t.cat3).map(t => t.cat3))].sort();
    if (cat3s.length) {
      chips3.innerHTML = '';
      cat3s.forEach(c3 => {
        const btn = document.createElement('button');
        btn.className = 'map-chip' + (c3 === cat3 ? ' active' : '');
        btn.dataset.val = c3; btn.textContent = c3;
        btn.style.setProperty('--chip-color', CAT2_COLOR[cat2] || COLORS.arena);
        btn.addEventListener('click', () => onMapChipClick('cat3', c3));
        chips3.appendChild(btn);
      });
      level3.style.display = '';
    } else { level3.style.display = 'none'; }
  } else { if (level3) level3.style.display = 'none'; }

  // CAT-N-4
  const level4 = document.getElementById('map-level-cat4');
  const chips4 = document.getElementById('map-chips-cat4');
  if (mapFilterState.cat3 && chips4) {
    const cat4s = [...new Set(ATLAS.tecnicas.filter(t => t.cat3 === mapFilterState.cat3 && t.cat4).map(t => t.cat4))].sort();
    if (cat4s.length) {
      chips4.innerHTML = '';
      cat4s.forEach(c4 => {
        const btn = document.createElement('button');
        btn.className = 'map-chip' + (c4 === mapFilterState.cat4 ? ' active' : '');
        btn.dataset.val = c4; btn.textContent = c4;
        btn.style.setProperty('--chip-color', COLORS.arena);
        btn.addEventListener('click', () => onMapChipClick('cat4', c4));
        chips4.appendChild(btn);
      });
      level4.style.display = '';
    } else { level4.style.display = 'none'; }
  } else { if (level4) level4.style.display = 'none'; }

  // Técnicas resultantes
  const levelT = document.getElementById('map-level-tecs');
  const chipsT = document.getElementById('map-chips-tecs');
  const labelT = document.getElementById('map-tecs-label');
  const filtered = getFilteredTecnicas();
  if (cat1 && filtered.length > 0 && filtered.length < 80 && chipsT) {
    chipsT.innerHTML = '';
    filtered.slice(0, 60).forEach(t => {
      const btn = document.createElement('button');
      btn.className = 'map-chip map-chip-tec' + (mapFilterState.tecnica === t.tecnica ? ' active' : '');
      btn.dataset.val = t.tecnica;
      btn.textContent = t.tecnica;
      btn.style.setProperty('--chip-color', getCatColor(t));
      btn.addEventListener('click', () => {
        mapFilterState.tecnica = mapFilterState.tecnica === t.tecnica ? null : t.tecnica;
        rebuildMapFilterCascade();
        applyMapFilter();
      });
      chipsT.appendChild(btn);
    });
    if (labelT) labelT.textContent = `Técnicas (${filtered.length})`;
    levelT.style.display = '';
  } else { if (levelT) levelT.style.display = 'none'; }
}

function getFilteredTecnicas() {
  const { cat1, cat2, cat3, cat4, tecnica } = mapFilterState;
  return ATLAS.tecnicas.filter(t => {
    if (tecnica && t.tecnica !== tecnica) return false;
    if (cat4 && t.cat4 !== cat4) return false;
    if (cat3 && t.cat3 !== cat3) return false;
    if (cat2 && t.cat2 !== cat2) return false;
    if (cat1 && t.cat1 !== cat1) return false;
    return true;
  });
}

function applyMapFilter() {
  const filtered = getFilteredTecnicas();
  const activeEl = document.getElementById('map-filter-active');
  const activeText = document.getElementById('map-filter-active-text');
  const { cat1, cat2, cat3, cat4, tecnica } = mapFilterState;

  const hasFilter = cat1 || tecnica;

  if (!hasFilter) {
    mapFilteredTecnicas = null;
    if (activeEl) activeEl.style.display = 'none';
    const badge = document.getElementById('map-filter-badge');
    if (badge) badge.style.display = 'none';
  } else {
    mapFilteredTecnicas = new Set(filtered.map(t => t.tecnica));
    const parts = [cat1, cat2, cat3, cat4, tecnica].filter(Boolean);
    const label = parts.join(' › ');
    if (activeText) activeText.textContent = `${label} · ${filtered.length} técnica${filtered.length!==1?'s':''}`;
    if (activeEl) activeEl.style.display = '';
    const badge = document.getElementById('map-filter-badge');
    if (badge) badge.style.display = '';
  }

  // Repaint map
  if (geojsonLayer) {
    geojsonLayer.eachLayer(l => {
      const name = l.feature?.properties?.name || l.feature?.properties?.NAME_1 || '';
      l.setStyle(mapFilteredTecnicas ? filteredStateStyle(name) : stateStyle(l.feature));
      // update tooltip
      l.unbindTooltip();
      const count = mapFilteredTecnicas ? countForEstadoFiltered(name) : countForEstado(name);
      if (count > 0) {
        l.bindTooltip(`<b>${name}</b><br>${count} técnica${count!==1?'s':''}`,
          { className: 'leaflet-tooltip-atlas', direction: 'top', sticky: true });
      }
    });
  }
  // Reset panel
  document.getElementById('panel-title').textContent = 'Selecciona un estado';
  document.getElementById('panel-subtitle').textContent = hasFilter
    ? `Mostrando ${filtered.length} técnica${filtered.length!==1?'s':''} filtrada${filtered.length!==1?'s':''}`
    : 'Haz clic en el mapa para explorar las técnicas de cada estado';
  document.getElementById('panel-body').innerHTML = `<div class="welcome-state"><h3>${hasFilter ? 'Filtro activo' : 'Explora el mapa'}</h3><p>${hasFilter ? 'Los estados coloreados tienen técnicas que coinciden con el filtro.' : 'Cada estado está coloreado según el número de técnicas documentadas.'}</p></div>`;
}

function countForEstadoFiltered(name) {
  const tecs = getTecnicasForEstado(name);
  return tecs.filter(t => mapFilteredTecnicas && mapFilteredTecnicas.has(t)).length;
}

function filteredStateStyle(name) {
  const count = countForEstadoFiltered(name);
  return { fillColor: getMapColor(count), weight: 1.2, opacity: 1, color: '#fff', fillOpacity: count > 0 ? 0.88 : 0.08 };
}

// ────────────────────────────────────────────────
// CATALOG VIEW
// ────────────────────────────────────────────────
let activeGrupo  = 'all';
let activeSearch = '';

function renderCatalog() {
  if (!ATLAS) return;
  const tecnicas = ATLAS.tecnicas.filter(t => {
    const gMatch = activeGrupo === 'all' || t.cat1 === activeGrupo;
    const sMatch = !activeSearch || t.tecnica.toLowerCase().includes(activeSearch);
    return gMatch && sMatch;
  });
  document.getElementById('catalog-results').textContent = `${tecnicas.length} técnica${tecnicas.length !== 1 ? 's' : ''}`;
  document.getElementById('tecnicas-grid').innerHTML = tecnicas.map(t => {
    const img   = t.imagenes && t.imagenes.length > 0 ? t.imagenes[0] : null;
    const color = getCatColor(t);
    const catLabel = [t.cat1, t.cat2].filter(Boolean).join(' › ');
    const estados = (t.estados || []).slice(0, 3).join(', ');
    return `<div class="tec-card" onclick="openFicha('${escJs(t.tecnica)}')">
      <div class="tec-card-img" style="background:${COLORS.negro}">
        ${img ? `<img src="${imgPath(esc(img))}" alt="${esc(t.tecnica)}" loading="lazy">` : `<span class="tec-card-no-img">&#129525;</span>`}
        <div style="position:absolute;top:0;left:0;right:0;height:3px;background:${color}"></div>
      </div>
      <div class="tec-card-body">
        <div class="tec-card-grupo">${esc(catLabel)}</div>
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

  // ── HERO — Carrusel ──
  const hero = document.getElementById('modal-hero');
  if (t.imagenes && t.imagenes.length > 0) {
    const slides = t.imagenes.map((fn, i) =>
      `<div class="carousel-slide" data-index="${i}">
        <img class="carousel-img" src="${imgPath(esc(fn))}" alt="${esc(t.tecnica)} ${i+1}" loading="${i === 0 ? 'eager' : 'lazy'}">
      </div>`
    ).join('');

    const dots = t.imagenes.length > 1
      ? `<div class="carousel-dots">${t.imagenes.map((_, i) =>
          `<button class="carousel-dot${i === 0 ? ' active' : ''}" data-i="${i}" onclick="carouselGoTo(${i})"></button>`
        ).join('')}</div>`
      : '';

    const arrows = t.imagenes.length > 1
      ? `<button class="carousel-btn carousel-prev" onclick="carouselPrev()">&#8249;</button>
         <button class="carousel-btn carousel-next" onclick="carouselNext()">&#8250;</button>`
      : '';

    hero.innerHTML = `
      <div class="carousel-track" id="carousel-track">${slides}</div>
      ${arrows}
      ${dots}
      <button class="modal-close" onclick="closeFicha()">✕</button>
      <div class="modal-badge">${t.n_fichas} registro${t.n_fichas !== 1 ? 's' : ''}</div>
      <div class="carousel-counter" id="carousel-counter">1 / ${t.imagenes.length}</div>`;

    // Click on image → lightbox
    hero.querySelectorAll('.carousel-img').forEach((img, i) => {
      img.addEventListener('click', () => openLightbox(imgPath(t.imagenes[i]), t.imagenes, i));
    });

    // Store images on hero for navigation functions
    hero._images = t.imagenes;
    hero._current = 0;

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
  // Clasificación experta
  if (t.cat1) {
    const color1 = CAT1_COLOR[t.cat1] || COLORS.arena;
    const color2 = CAT2_COLOR[t.cat2] || color1;
    let breadcrumb = esc(t.cat1);
    if (t.cat2) breadcrumb += ` <span style="opacity:.6">›</span> ${esc(t.cat2)}`;
    if (t.cat3) breadcrumb += ` <span style="opacity:.6">›</span> ${esc(t.cat3)}`;
    if (t.cat4) breadcrumb += ` <span style="opacity:.6">›</span> ${esc(t.cat4)}`;
    html += `<div class="modal-grupo" style="background:${color1}">${breadcrumb}</div>`;
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

  // Clasificación experta en ficha
  if (t.cat1) {
    const color1 = CAT1_COLOR[t.cat1] || COLORS.arena;
    const color2 = CAT2_COLOR[t.cat2] || color1;
    const color3 = color2;
    const color4 = COLORS.arena;
    html += `<div class="section">
      <div class="section-head"><span class="section-title">Clasificación</span></div>
      <div class="clasificacion-tree">
        <div class="clas-row"><div class="clas-dot" style="background:${color1}"></div><div class="clas-level">Categoría</div><div class="clas-val" style="color:${color1}">${esc(t.cat1)}</div></div>
        ${t.cat2 ? `<div class="clas-row"><div class="clas-dot" style="background:${color2}"></div><div class="clas-level">Subcategoría</div><div class="clas-val" style="color:${color2}">${esc(t.cat2)}</div></div>` : ''}
        ${t.cat3 ? `<div class="clas-row"><div class="clas-dot" style="background:${color3}"></div><div class="clas-level">Tipo</div><div class="clas-val">${esc(t.cat3)}</div></div>` : ''}
        ${t.cat4 ? `<div class="clas-row"><div class="clas-dot" style="background:${color4}"></div><div class="clas-level">Variante</div><div class="clas-val">${esc(t.cat4)}</div></div>` : ''}
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
    <button onclick="goToCatalog('${escJs(t.cat1)}')" style="background:transparent;color:var(--arena);border:1px solid var(--border);padding:10px 20px;border-radius:4px;font-family:var(--font-body);font-size:.8rem;font-weight:600;cursor:pointer;letter-spacing:.3px;margin-left:8px;">
      Ver técnicas de esta categoría
    </button>
    <button onclick="closeFicha();openKobo('cambio')" style="background:transparent;color:var(--azul-mar);border:1px solid var(--azul-mar);padding:10px 20px;border-radius:4px;font-family:var(--font-body);font-size:.8rem;font-weight:600;cursor:pointer;letter-spacing:.3px;margin-left:8px;">
      ✎ ¿Deseas proponer algún cambio a esta técnica?
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

function goToCatalog(cat1) {
  closeFicha();
  document.querySelector('[data-tab="catalogo"]').click();
  if (!catalogInitialized) catalogInitialized = true;
  activeGrupo = cat1 || 'all';
  document.querySelectorAll('.filter-btn').forEach(b => {
    b.classList.toggle('active', b.dataset.grupo === activeGrupo);
  });
  renderCatalog();
}

// ────────────────────────────────────────────────
// LIGHTBOX
// ────────────────────────────────────────────────
// ────────────────────────────────────────────────
// CARRUSEL
// ────────────────────────────────────────────────
function _carousel() {
  return document.getElementById('modal-hero');
}

function carouselGoTo(i) {
  const hero  = _carousel();
  const track = document.getElementById('carousel-track');
  if (!track) return;
  const slides = track.querySelectorAll('.carousel-slide');
  if (!slides.length) return;
  i = ((i % slides.length) + slides.length) % slides.length;   // wrap
  hero._current = i;
  track.style.transform = `translateX(-${i * 100}%)`;

  // Dots
  hero.querySelectorAll('.carousel-dot').forEach((d, idx) =>
    d.classList.toggle('active', idx === i)
  );
  // Counter
  const counter = document.getElementById('carousel-counter');
  if (counter) counter.textContent = `${i + 1} / ${slides.length}`;
}

function carouselPrev() {
  const hero = _carousel();
  carouselGoTo((hero._current || 0) - 1);
}

function carouselNext() {
  const hero = _carousel();
  carouselGoTo((hero._current || 0) + 1);
}

// Keyboard navigation while modal is open
document.addEventListener('keydown', e => {
  const modal = document.getElementById('modal-overlay');
  if (!modal.classList.contains('open')) return;
  if (e.key === 'ArrowRight') carouselNext();
  if (e.key === 'ArrowLeft')  carouselPrev();
});

// Touch/swipe support
(function() {
  let startX = null;
  document.addEventListener('touchstart', e => {
    const hero = document.getElementById('modal-hero');
    if (hero && hero.contains(e.target)) startX = e.touches[0].clientX;
  }, { passive: true });
  document.addEventListener('touchend', e => {
    if (startX === null) return;
    const dx = e.changedTouches[0].clientX - startX;
    startX = null;
    if (Math.abs(dx) < 40) return;
    if (dx < 0) carouselNext(); else carouselPrev();
  }, { passive: true });
})();

// ────────────────────────────────────────────────
// LIGHTBOX — con navegación entre imágenes
// ────────────────────────────────────────────────
let _lbImages  = [];
let _lbCurrent = 0;

function openLightbox(src, images, index) {
  _lbImages  = images  || [src];
  _lbCurrent = index   || 0;
  _renderLightbox();
  document.getElementById('lightbox').classList.add('open');
}

function _renderLightbox() {
  document.getElementById('lightbox-img').src = _lbImages[_lbCurrent]
    ? `imagenes/${_lbImages[_lbCurrent]}` : _lbImages[_lbCurrent];
  const counter = document.getElementById('lightbox-counter');
  if (counter) counter.textContent = `${_lbCurrent + 1} / ${_lbImages.length}`;
  // show/hide arrows
  const prev = document.getElementById('lightbox-prev');
  const next = document.getElementById('lightbox-next');
  if (prev) prev.style.display = _lbImages.length > 1 ? '' : 'none';
  if (next) next.style.display = _lbImages.length > 1 ? '' : 'none';
}

document.getElementById('lightbox-close').addEventListener('click', () => {
  document.getElementById('lightbox').classList.remove('open');
});
document.getElementById('lightbox').addEventListener('click', e => {
  if (e.target === document.getElementById('lightbox')) {
    document.getElementById('lightbox').classList.remove('open');
  }
});
document.addEventListener('keydown', e => {
  const lb = document.getElementById('lightbox');
  if (!lb.classList.contains('open')) return;
  if (e.key === 'Escape')      lb.classList.remove('open');
  if (e.key === 'ArrowRight')  { _lbCurrent = (_lbCurrent + 1) % _lbImages.length; _renderLightbox(); }
  if (e.key === 'ArrowLeft')   { _lbCurrent = (_lbCurrent - 1 + _lbImages.length) % _lbImages.length; _renderLightbox(); }
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
  { id: 'all',         label: 'Todas' },
  { id: 'Tejido',      label: 'Tejido' },
  { id: 'decorativas', label: 'Técnicas decorativas' },
  { id: 'Acabados',    label: 'Acabados' },
  { id: 'Teñidos',     label: 'Teñidos' },
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
  initNetworkSearch();
}

function resizeCanvas() {
  const rect = networkCanvas.parentElement.getBoundingClientRect();
  networkCanvas.width  = rect.width || 900;
  networkCanvas.height = parseInt(networkCanvas.style.height || 580);
  if (networkNodes.length) drawNetwork();
}

function buildNetworkData() {
  const tecnicas = ATLAS.tecnicas;
  // Use CAT-N-2 as hub nodes (more granular than cat1, fewer than individual tecnicas)
  const hubs = [...new Set(tecnicas.map(t => t.cat1 + '||' + (t.cat2 || '')).filter(Boolean))];
  networkNodes = [];

  hubs.forEach(hubKey => {
    const [c1, c2] = hubKey.split('||');
    const label = c2 || c1;
    const color = CAT2_COLOR[c2] || CAT1_COLOR[c1] || COLORS.arena;
    networkNodes.push({
      id: `hub:${hubKey}`, label, type: 'grupo', cat1: c1, cat2: c2, r: 22,
      color,
      x: Math.random() * 600 + 100, y: Math.random() * 400 + 80, vx: 0, vy: 0, fx: null, fy: null,
    });
  });

  tecnicas.forEach(t => {
    const color = getCatColor(t);
    networkNodes.push({
      id: t.tecnica, label: t.tecnica, type: 'tecnica', grupo: t.cat1, cat1: t.cat1, cat2: t.cat2,
      estados: t.estados, n_fichas: t.n_fichas,
      cat3: t.cat3, cat4: t.cat4,
      r: Math.max(6, Math.min(16, 5 + t.n_fichas * 0.7)),
      color,
      x: Math.random() * 700 + 50, y: Math.random() * 500 + 40, vx: 0, vy: 0, fx: null, fy: null,
    });
  });

  networkLinks = [];
  tecnicas.forEach(t => {
    const hubKey = t.cat1 + '||' + (t.cat2 || '');
    networkLinks.push({ source: `hub:${hubKey}`, target: t.tecnica, strength: 0.6 });
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
  const c1 = (n.cat1 || '');
  switch (activeNetworkFilter) {
    case 'Tejido':      return c1 === 'Tejido';
    case 'decorativas': return c1 === 'Técnicas decorativas';
    case 'Acabados':    return c1 === 'Acabados';
    case 'Teñidos':     return c1 === 'Teñidos';
    default: return true;
  }
}

let _simAlpha = 0;
let _animFrame = null;

function startSimulation() {
  const W = networkCanvas.width, H = networkCanvas.height;
  const grupos = networkNodes.filter(n => n.type === 'grupo');

  // Place hub nodes in a circle — fixed so tecnicas cluster around them
  grupos.forEach((g, i) => {
    const angle = (i / grupos.length) * Math.PI * 2 - Math.PI / 2;
    const rx = Math.min(W, H) * 0.28;
    const ry = Math.min(W, H) * 0.24;
    g.x = W / 2 + Math.cos(angle) * rx;
    g.y = H / 2 + Math.sin(angle) * ry;
    g.fx = g.x; g.fy = g.y;
  });

  // Spread tecnicas near their hub
  networkNodes.filter(n => n.type === 'tecnica').forEach(n => {
    const hubKey = `hub:${n.cat1}||${n.cat2 || ''}`;
    const hub = networkNodes.find(h => h.id === hubKey);
    const angle = Math.random() * Math.PI * 2;
    const dist  = 30 + Math.random() * 80;
    if (hub) { n.x = hub.x + Math.cos(angle) * dist; n.y = hub.y + Math.sin(angle) * dist; }
    else { n.x = W / 2 + (Math.random() - .5) * 300; n.y = H / 2 + (Math.random() - .5) * 250; }
    n.vx = 0; n.vy = 0;
  });

  // Warm-up: 150 silent iters
  _simAlpha = 1;
  for (let i = 0; i < 150; i++) { _simAlpha *= 0.97; applyForces(_simAlpha); }

  // Release hubs after warm-up (optional drift)
  // grupos.forEach(g => { g.fx = null; g.fy = null; });

  // Start live animation loop
  if (_animFrame) cancelAnimationFrame(_animFrame);
  _simAlpha = 0.25;
  function loop() {
    if (_simAlpha > 0.003) {
      _simAlpha *= 0.985;
      applyForces(_simAlpha);
    }
    drawNetwork();
    _animFrame = requestAnimationFrame(loop);
  }
  loop();
}

function applyForces(alpha) {
  const visible = new Set(getVisibleNodes().map(n => n.id));
  const nodes   = networkNodes.filter(n => visible.has(n.id));
  const W = networkCanvas.width, H = networkCanvas.height;

  // Repulsion — only between same-cluster nodes to avoid overcrowding globally
  for (let i = 0; i < nodes.length; i++) {
    for (let j = i + 1; j < nodes.length; j++) {
      const a = nodes[i], b = nodes[j];
      if (a.type === 'grupo' || b.type === 'grupo') continue; // hubs handle themselves via spring
      if (a.cat2 !== b.cat2) continue; // only repel within same cluster
      let dx = b.x - a.x || 0.01, dy = b.y - a.y || 0.01;
      const dist2 = dx * dx + dy * dy;
      const dist  = Math.sqrt(dist2) || 0.01;
      const ideal = a.r + b.r + 18;
      if (dist < ideal * 4) {
        const force = (alpha * 45) / dist2;
        a.vx -= dx * force; a.vy -= dy * force;
        b.vx += dx * force; b.vy += dy * force;
      }
    }
  }

  // Spring links — strong pull toward hub
  networkLinks.forEach(link => {
    const s = networkNodes.find(n => n.id === link.source);
    const t = networkNodes.find(n => n.id === link.target);
    if (!s || !t || !visible.has(s.id) || !visible.has(t.id)) return;
    const dx = t.x - s.x, dy = t.y - s.y;
    const dist = Math.sqrt(dx * dx + dy * dy) || 0.01;
    // Preferred distance scales with how many nodes in cluster
    const preferred = 55 + t.r * 1.5;
    const force = (dist - preferred) * 0.032 * alpha;
    const fx = (dx / dist) * force, fy = (dy / dist) * force;
    if (s.fx === null || s.fx === undefined) { s.vx += fx; s.vy += fy; }
    t.vx -= fx * 0.5; t.vy -= fy * 0.5;
  });

  nodes.forEach(n => {
    if (n.fx !== null && n.fx !== undefined) { n.x = n.fx; n.vx = 0; return; }
    if (n.fy !== null && n.fy !== undefined) { n.y = n.fy; n.vy = 0; return; }
    // Light gravity to hub rather than canvas center
    const hubKey = `hub:${n.cat1}||${n.cat2 || ''}`;
    const hub = networkNodes.find(h => h.id === hubKey);
    if (hub) {
      n.vx += (hub.x - n.x) * 0.004 * alpha;
      n.vy += (hub.y - n.y) * 0.004 * alpha;
    }
    n.vx *= 0.72; n.vy *= 0.72;
    n.x += n.vx; n.y += n.vy;
    n.x = Math.max(n.r + 10, Math.min(W - n.r - 10, n.x));
    n.y = Math.max(n.r + 10, Math.min(H - n.r - 10, n.y));
  });
}

function drawNetwork() {
  const canvas = networkCanvas, ctx = networkCtx;
  const W = canvas.width, H = canvas.height;
  const visible = new Set(getVisibleNodes().map(n => n.id));

  ctx.save();
  ctx.clearRect(0, 0, W, H);
  // Subtle warm background
  ctx.fillStyle = '#FDFAF8';
  ctx.fillRect(0, 0, W, H);
  ctx.translate(transform.x, transform.y);
  ctx.scale(transform.k, transform.k);

  // Draw links
  networkLinks.forEach(link => {
    const s = networkNodes.find(n => n.id === link.source);
    const t = networkNodes.find(n => n.id === link.target);
    if (!s || !t || !visible.has(s.id) || !visible.has(t.id)) return;
    const isHovLink = hoveredNode && (hoveredNode.id === s.id || hoveredNode.id === t.id);
    const isHiLink  = highlightedNode && (highlightedNode.id === s.id || highlightedNode.id === t.id
      || (highlightedNode.type === 'grupo' && t.cat2 === highlightedNode.cat2));
    ctx.beginPath();
    ctx.moveTo(s.x, s.y); ctx.lineTo(t.x, t.y);
    if (isHovLink) {
      ctx.strokeStyle = s.color + 'bb'; ctx.lineWidth = 1.5;
    } else if (isHiLink) {
      ctx.strokeStyle = s.color + '88'; ctx.lineWidth = 1.2;
    } else {
      ctx.strokeStyle = 'rgba(26,16,24,0.06)'; ctx.lineWidth = 0.8;
    }
    ctx.stroke();
  });

  // Sort: tecnicas behind grupos
  const nodes = networkNodes.filter(n => visible.has(n.id));
  nodes.sort((a,b) => {
    if (a.type === 'grupo' && b.type !== 'grupo') return 1;
    if (b.type === 'grupo' && a.type !== 'grupo') return -1;
    return 0;
  });

  nodes.forEach(n => {
    const isHov = hoveredNode     && hoveredNode.id     === n.id;
    const isHi  = highlightedNode && (highlightedNode.id === n.id
      || (highlightedNode.type === 'grupo' && n.cat2 === highlightedNode.cat2 && n.type === 'tecnica'));
    const dimmed = (highlightedNode || hoveredNode) && !isHi && !isHov;
    const r = n.r * (isHov || isHi ? 1.3 : 1);

    // Glow for hub nodes always, for tecnica on hover/highlight
    if (n.type === 'grupo' || isHov || isHi) {
      ctx.shadowColor = n.color;
      ctx.shadowBlur = n.type === 'grupo' ? 18 : 10;
    }

    ctx.beginPath();
    ctx.arc(n.x, n.y, r, 0, Math.PI * 2);

    if (n.type === 'grupo') {
      // Hub: solid fill + white ring
      ctx.fillStyle = dimmed ? (n.color + '55') : n.color;
      ctx.fill();
      ctx.shadowBlur = 0;
      ctx.strokeStyle = dimmed ? 'rgba(255,255,255,.2)' : 'rgba(255,255,255,.85)';
      ctx.lineWidth = 2.5; ctx.stroke();
    } else {
      // Tecnica: filled circle
      ctx.fillStyle = dimmed ? (n.color + '30') : (isHov || isHi ? n.color : n.color + 'cc');
      ctx.fill();
      ctx.shadowBlur = 0;
    }

    // Labels
    if (n.type === 'grupo') {
      // Hub label always visible — pill background
      const label = n.label;
      ctx.font = `700 10.5px "DM Sans", sans-serif`;
      const tw = ctx.measureText(label).width;
      const px = 6, py = 3;
      ctx.fillStyle = dimmed ? 'rgba(26,16,24,.08)' : 'rgba(255,255,255,0.92)';
      ctx.beginPath();
      const lx = n.x - tw/2 - px, ly = n.y + r + 3;
      ctx.roundRect(lx, ly, tw + px*2, 16 + py*2, 4);
      ctx.fill();
      ctx.fillStyle = dimmed ? 'rgba(26,16,24,.25)' : n.color;
      ctx.textAlign = 'center'; ctx.textBaseline = 'top';
      ctx.fillText(label, n.x, ly + py);
    } else if (isHov || isHi) {
      ctx.font = `500 9px "DM Sans", sans-serif`;
      ctx.textAlign = 'center'; ctx.textBaseline = 'top';
      const label = n.label.length > 24 ? n.label.slice(0, 22) + '…' : n.label;
      const tw = ctx.measureText(label).width;
      ctx.fillStyle = 'rgba(255,255,255,0.9)';
      ctx.fillRect(n.x - tw/2 - 3, n.y + r + 2, tw + 6, 13);
      ctx.fillStyle = COLORS.negro;
      ctx.fillText(label, n.x, n.y + r + 3);
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
      if (node.type === 'tecnica') {
        const catBreadcrumb = [node.cat1, node.cat2].filter(Boolean).join(' › ');
        tooltip.innerHTML = `<strong>${esc(node.label)}</strong><br>
          <span style="font-size:.72rem;opacity:.7">${esc(catBreadcrumb)}</span><br>
          ${node.n_fichas} registro${node.n_fichas !== 1 ? 's' : ''} · ${(node.estados || []).slice(0, 2).join(', ')}`;
      } else {
        const count = networkNodes.filter(n => n.type === 'tecnica' && n.cat2 === node.cat2).length;
        tooltip.innerHTML = `<strong>${esc(node.label)}</strong><br>
          <span style="font-size:.72rem;opacity:.7">${esc(node.cat1||'')}</span><br>
          ${count} técnica${count !== 1 ? 's' : ''}`;
      }
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
  const wrap = document.getElementById('network-legend-items');
  wrap.innerHTML = Object.entries(CAT1_COLOR).map(([cat1, color]) =>
    `<div class="net-legend-item" data-cat1="${cat1}">
      <div class="net-legend-dot" style="background:${color}"></div>${cat1}</div>`
  ).join('');
  wrap.querySelectorAll('.net-legend-item').forEach(item => {
    item.addEventListener('click', () => {
      const cat1 = item.dataset.cat1;
      // highlight all hub nodes of this cat1
      const hubNode = networkNodes.find(n => n.type === 'grupo' && n.cat1 === cat1);
      if (hubNode) {
        highlightedNode = highlightedNode?.id === hubNode.id ? null : hubNode;
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

function initNetworkSearch() {
  const input = document.getElementById('network-search');
  const ac    = document.getElementById('network-search-ac');
  if (!input || !ac) return;

  input.addEventListener('input', function() {
    const q = this.value.toLowerCase().trim();
    ac.innerHTML = ''; ac.style.display = 'none';
    if (q.length < 2) { highlightedNode = null; drawNetwork(); return; }
    const matches = ATLAS.tecnicas
      .filter(t => t.tecnica.toLowerCase().includes(q))
      .slice(0, 12);
    if (!matches.length) return;
    matches.forEach(t => {
      const item = document.createElement('div');
      item.className = 'network-ac-item';
      const color = getCatColor(t);
      item.innerHTML = `<span class="network-ac-dot" style="background:${color}"></span>
        <span class="network-ac-name">${esc(t.tecnica)}</span>
        <span class="network-ac-cat" style="color:${CAT1_COLOR[t.cat1]||COLORS.arena}">${esc(t.cat1||'')}</span>`;
      item.addEventListener('click', () => {
        input.value = t.tecnica;
        ac.style.display = 'none';
        highlightNetworkNode(t.tecnica);
        // also open ficha on double intent: just highlight
        const node = networkNodes.find(n => n.id === t.tecnica);
        if (node) { highlightedNode = node; drawNetwork(); }
      });
      ac.appendChild(item);
    });
    ac.style.display = 'block';
  });

  input.addEventListener('keydown', e => {
    if (e.key === 'Escape') { ac.style.display = 'none'; input.value = ''; highlightedNode = null; drawNetwork(); }
    if (e.key === 'Enter') {
      const first = ac.querySelector('.network-ac-item');
      if (first) first.click();
    }
  });

  document.addEventListener('click', e => {
    if (!input.contains(e.target) && !ac.contains(e.target)) ac.style.display = 'none';
  });
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
        <div class="chart-card-title">Categoría de técnica (clasificación experta)</div>
        <div class="chart-card-sub">Distribución de registros por categoría principal según el esquema experto</div>
        <canvas id="r-chart-grupos" height="260"></canvas>
      </div>
      <div class="chart-card">
        <div class="chart-card-title">Subcategoría (CAT-N-2)</div>
        <div class="chart-card-sub">Número de registros por subcategoría dentro del esquema de clasificación experto</div>
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

/* ── CATEGORÍA EXPERTA (CAT-N-1) ── */
new Chart(document.getElementById('r-chart-grupos'), {
  type: 'doughnut',
  data: {
    labels: ['Tejido','Técnicas decorativas','Acabados','Teñidos'],
    datasets: [{ data:[317,310,5,5], backgroundColor:[AZUL,MAGENTA,OCRE,VERDE], borderWidth:3, borderColor:'#FFF8F5' }]
  },
  options: { cutout:'52%', plugins:{ legend:{ position:'bottom', labels:{ color:ARENA, font:{size:11}, padding:10 } } } }
});

/* ── SUBCATEGORÍA EXPERTA (CAT-N-2) ── */
const catN2Data = [['Bordados',309],['Tejidos',284],['Anudados',24],['Tapicería',9],['Artesanales',5],['Aplicaciones',1]];
new Chart(document.getElementById('r-chart-manufactura'), {
  type: 'bar',
  data: {
    labels: catN2Data.map(d=>d[0]),
    datasets: [{ data: catN2Data.map(d=>d[1]), backgroundColor:[MAGENTA,AZUL,CYAN,'#5B4FCF',VERDE,ROSA], borderWidth:0, borderRadius:3 }]
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
// ÁRBOL DE CLASIFICACIÓN
// ────────────────────────────────────────────────
function initArbol() {
  if (!ATLAS) return;
  const tecnicas = ATLAS.tecnicas;

  // Build tree from expert classification CAT-N-1 → N-2 → N-3 → N-4
  const tree = {};
  tecnicas.forEach(t => {
    const c1 = t.cat1 || 'Sin clasificar';
    const c2 = t.cat2 || null;
    const c3 = t.cat3 || null;
    const c4 = t.cat4 || null;
    if (!tree[c1]) tree[c1] = { color: CAT1_COLOR[c1] || COLORS.arena, children: {} };
    if (!c2) {
      // leaf at level 1
      if (!tree[c1].tecnicas) tree[c1].tecnicas = [];
      tree[c1].tecnicas.push(t.tecnica);
      return;
    }
    if (!tree[c1].children[c2]) tree[c1].children[c2] = { color: CAT2_COLOR[c2] || tree[c1].color, children: {} };
    const node2 = tree[c1].children[c2];
    if (!c3) {
      if (!node2.tecnicas) node2.tecnicas = [];
      node2.tecnicas.push(t.tecnica);
      return;
    }
    if (!node2.children[c3]) node2.children[c3] = { color: node2.color, children: {} };
    const node3 = node2.children[c3];
    if (!c4) {
      if (!node3.tecnicas) node3.tecnicas = [];
      node3.tecnicas.push(t.tecnica);
      return;
    }
    if (!node3.children[c4]) node3.children[c4] = { color: node3.color, tecnicas: [] };
    node3.children[c4].tecnicas.push(t.tecnica);
  });

  let activeLeaf = null;

  function renderTree() {
    const container = document.getElementById('arbol-container');

    function leafHTML(name, node, color) {
      const isActive = activeLeaf === name;
      const count = (node.tecnicas || []).length;
      return `<div class="arbol-leaf ${isActive ? 'active' : ''}" data-leaf="${esc(name)}" style="--leaf-color:${color}">
        <span class="arbol-leaf-name">${esc(name)}</span>
        <span class="arbol-leaf-count">${count}</span>
      </div>`;
    }

    function subgroupHTML(name, node, parentColor) {
      const color = node.color || parentColor;
      if (node.tecnicas) {
        return leafHTML(name, node, color || parentColor);
      }
      // Has children
      const childrenHTML = Object.entries(node.children || {}).map(([cName, cNode]) =>
        subgroupHTML(cName, cNode, color)
      ).join('');
      return `<div class="arbol-subgroup">
        <div class="arbol-subgroup-label" style="color:${color}">${esc(name)}</div>
        <div class="arbol-subgroup-children">${childrenHTML}</div>
      </div>`;
    }

    const mainBranches = Object.entries(tree).map(([branchName, branch]) => {
      const childrenHTML = Object.entries(branch.children || {}).map(([cName, cNode]) =>
        subgroupHTML(cName, cNode, branch.color)
      ).join('');
      // If branch itself has tecnicas (no sub-children)
      const selfLeafs = (branch.tecnicas || []).length > 0 ? leafHTML(branchName, branch, branch.color) : '';
      return `<div class="arbol-branch">
        <div class="arbol-branch-label" style="background:${branch.color}">${esc(branchName)}</div>
        <div class="arbol-branch-children">${selfLeafs}${childrenHTML}</div>
      </div>`;
    }).join('');

    // Find active leaf node
    let activeTecnicas = [];
    let activeColor = COLORS.magenta;
    if (activeLeaf) {
      function findLeaf(name, node, color) {
        if (node.tecnicas && name === activeLeaf) {
          activeTecnicas = node.tecnicas;
          activeColor = node.color || color;
          return true;
        }
        for (const [cName, cNode] of Object.entries(node.children || {})) {
          if (findLeaf(cName, cNode, node.color || color)) return true;
        }
        return false;
      }
      Object.entries(tree).forEach(([branchName, branch]) => {
        findLeaf(branchName, branch, branch.color);
        Object.entries(branch.children || {}).forEach(([cName, cNode]) => {
          findLeaf(cName, cNode, branch.color);
        });
      });
    }

    const panelHTML = activeLeaf ? `
      <div class="arbol-panel">
        <div class="arbol-panel-header" style="border-color:${activeColor}">
          <span class="arbol-panel-title" style="color:${activeColor}">${esc(activeLeaf)}</span>
          <span class="arbol-panel-count">${activeTecnicas.length} técnica${activeTecnicas.length !== 1 ? 's' : ''}</span>
          <button class="arbol-panel-close" onclick="arbolClosePanel()">✕</button>
        </div>
        <div class="arbol-panel-note">Una técnica puede clasificarse en más de una rama.</div>
        <div class="arbol-panel-list">
          ${activeTecnicas.map(tname => {
            const td = tecnicasMap[tname];
            const color = td ? getCatColor(td) : COLORS.arena;
            return `<div class="arbol-tec-item" onclick="openFicha('${escJs(tname)}')">
              <div class="arbol-tec-dot" style="background:${color}"></div>
              <span>${esc(tname)}</span>
            </div>`;
          }).join('')}
        </div>
      </div>` : `<div class="arbol-panel arbol-panel-empty">
        <p>Selecciona una categoría para ver las técnicas clasificadas en ella.</p>
      </div>`;

    container.innerHTML = `
      <div class="arbol-layout">
        <div class="arbol-tree">${mainBranches}</div>
        ${panelHTML}
      </div>`;

    // Bind leaf clicks
    container.querySelectorAll('.arbol-leaf').forEach(el => {
      el.addEventListener('click', () => {
        activeLeaf = el.dataset.leaf === activeLeaf ? null : el.dataset.leaf;
        renderTree();
      });
    });
  }

  window.arbolClosePanel = function() { activeLeaf = null; renderTree(); };
  renderTree();
}

// ────────────────────────────────────────────────
// ARRANQUE
// ────────────────────────────────────────────────

// ────────────────────────────────────────────────
// SECCIÓN TEÑIDO — visualización técnica × tipo teñido
// ────────────────────────────────────────────────
let tenidoInitialized = false;

const TENIDO_TYPES = [
  { key: 'n_tenido_plantas',   label: 'Con plantas',       color: '#05B794' },
  { key: 'n_tenido_animales',  label: 'Insectos/animales', color: '#FB4801' },
  { key: 'n_tenido_minerales', label: 'Sales y minerales', color: '#FFA329' },
  { key: 'n_tenido_otro',      label: 'Otro',              color: '#8C7A80' },
];

let activeTenidoFilter = null;

function initTenidoView() {
  if (tenidoInitialized) return;
  tenidoInitialized = true;
  if (!ATLAS) return;

  // Build filter buttons
  const filtersEl = document.getElementById('tenido-filters');
  filtersEl.innerHTML = '';
  TENIDO_TYPES.forEach(tipo => {
    const btn = document.createElement('button');
    btn.className = 'tenido-filter-btn';
    btn.dataset.key = tipo.key;
    btn.style.setProperty('--tc', tipo.color);
    btn.innerHTML = `<span class="tenido-filter-dot" style="background:${tipo.color}"></span>${tipo.label}`;
    btn.addEventListener('click', () => {
      activeTenidoFilter = activeTenidoFilter === tipo.key ? null : tipo.key;
      document.querySelectorAll('.tenido-filter-btn').forEach(b =>
        b.classList.toggle('active', b.dataset.key === activeTenidoFilter)
      );
      renderTenidoMatrix();
    });
    filtersEl.appendChild(btn);
  });

  renderTenidoMatrix();
}

function renderTenidoMatrix() {
  if (!ATLAS) return;

  // Get tecnicas that have at least one tenido type
  let tecnicas = ATLAS.tecnicas.filter(t =>
    TENIDO_TYPES.some(tipo => t.tenido && t.tenido[tipo.key.replace('n_tenido_','')] > 0)
  );

  if (activeTenidoFilter) {
    const fieldKey = activeTenidoFilter.replace('n_tenido_', '');
    tecnicas = tecnicas.filter(t => t.tenido && t.tenido[fieldKey] > 0);
  }

  tecnicas = tecnicas.sort((a,b) => {
    if (a.cat1 !== b.cat1) return (a.cat1 || '').localeCompare(b.cat1 || '');
    if (a.cat2 !== b.cat2) return (a.cat2 || '').localeCompare(b.cat2 || '');
    return a.tecnica.localeCompare(b.tecnica);
  });

  const body = document.getElementById('tenido-body');
  if (!tecnicas.length) {
    body.innerHTML = '<div style="padding:40px;text-align:center;color:var(--arena)">Sin datos de teñido disponibles.</div>';
    return;
  }

  // Build matrix HTML
  const activeTypes = activeTenidoFilter
    ? TENIDO_TYPES.filter(t => t.key === activeTenidoFilter)
    : TENIDO_TYPES;

  const colCount = activeTypes.length + 1; // row-head + type columns
  let html = `<div class="tenido-matrix" style="grid-template-columns: minmax(190px,260px) repeat(${activeTypes.length}, minmax(72px,110px))">`;

  // Header row
  html += '<div class="tenido-matrix-row tenido-matrix-header">';
  html += '<div class="tenido-matrix-cell tenido-matrix-corner"></div>';
  activeTypes.forEach(tipo => {
    html += `<div class="tenido-matrix-cell tenido-matrix-col-head" title="${tipo.label}">
      <div class="tenido-col-dot" style="background:${tipo.color}"></div>
      <span class="tenido-col-label">${tipo.label.replace('Con ','').replace('/animales','')}</span>
    </div>`;
  });
  html += '</div>';

  // Group rows by cat1
  let lastCat1 = null;
  tecnicas.forEach(t => {
    if (t.cat1 !== lastCat1) {
      lastCat1 = t.cat1;
      const color = CAT1_COLOR[t.cat1] || COLORS.arena;
      // span all columns — need explicit cell count
      const emptyColsHtml = activeTypes.map(() => '<div class="tenido-matrix-cell"></div>').join('');
      html += `<div class="tenido-matrix-row">
        <div class="tenido-matrix-cell tenido-cat-label" style="color:${color}; font-size:.65rem; font-weight:700; text-transform:uppercase; letter-spacing:1.5px; padding: 10px 10px 4px; background:var(--bg-page); border-bottom:none">${esc(t.cat1)}</div>
        ${emptyColsHtml}
      </div>`;
    }

    html += `<div class="tenido-matrix-row" data-tecnica="${esc(t.tecnica)}">`;
    const catColor = getCatColor(t);
    html += `<div class="tenido-matrix-cell tenido-matrix-row-head">
      <span class="tenido-row-dot" style="background:${catColor}"></span>
      <button class="tenido-tec-btn" onclick="openFicha('${escJs(t.tecnica)}')">${esc(t.tecnica)}</button>
    </div>`;

    activeTypes.forEach(tipo => {
      const fieldKey = tipo.key.replace('n_tenido_', '');
      const val = (t.tenido && t.tenido[fieldKey]) || 0;
      const intensity = val > 0 ? Math.min(1, 0.25 + val / 10) : 0;
      if (val > 0) {
        html += `<div class="tenido-matrix-cell tenido-matrix-val tenido-matrix-val-on"
          style="--tc:${tipo.color};--intensity:${intensity}"
          title="${t.tecnica} · ${tipo.label}: ${val}">
          <span class="tenido-val-num">${val}</span>
        </div>`;
      } else {
        html += `<div class="tenido-matrix-cell tenido-matrix-val tenido-matrix-val-off"></div>`;
      }
    });
    html += '</div>';
  });

  html += '</div>';
  body.innerHTML = html;
}

loadCSVs();
