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
let municipiosTecnicas = {}; // "Estado||Municipio" -> [tecnicas]

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
  const municipiosMap = {}; // "Estado||Municipio" -> Set<tecnica>

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
    // Materiales — se prefiere la columna curada `materiales_lista` (lista
    // limpia separada por comas, deduplicada y consolidada). Como fallback
    // se usa `Materiales_concat_clean`, que es el campo crudo.
    const materiales = (row['materiales_lista'] || row['Materiales_concat_clean'] || '').trim();

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
    const prenda_principal   = row['prenda_principal']   || '';
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

    // ── Contenido textual curado ──────────────────────────────────
    // Las columnas `historia`, `significados` y `materiales_lista` del
    // CSV de técnicas son redacciones sintéticas hechas a partir de
    // todos los registros de cada técnica. Se prefieren sobre cualquier
    // cálculo automático desde records porque consolidan las voces
    // múltiples en una sola narrativa coherente y eliminan repeticiones.
    const historia     = (row['historia'] || '').trim();
    const significadosRaw = (row['significados'] || '').trim();
    // Para mantener compatibilidad con el resto del código, dejamos
    // `significados` como array (la ficha decide cómo presentarlo).
    // Si el CSV trae un párrafo completo, lo guardamos como un único
    // elemento del array.
    const significados = significadosRaw ? [significadosRaw] : [];

    // Imágenes
    const imagenes = imgByTech[nombre] || [];

    // Acumular para el mapa
    estados.forEach(est => {
      if (!estadosMap[est]) estadosMap[est] = [];
      estadosMap[est].push(nombre);
    });

    // Acumular para el mapa de municipios — clave "Estado||Municipio"
    // (necesario porque hay nombres de municipio que se repiten entre estados)
    records.forEach(r => {
      const est = (r['Estado'] || '').trim();
      const mun = (r['Municipio'] || '').trim();
      if (!est || !mun || mun === 'NA') return;
      const key = `${est}||${mun}`;
      if (!municipiosMap[key]) municipiosMap[key] = new Set();
      municipiosMap[key].add(nombre);
    });

    return {
      tecnica: nombre,
      grupo, estados, lenguas, municipios, temporalidad,
      n_fichas, n_mujeres, n_hombres, edad_promedio,
      materiales,
      manufactura, manufactura_tipos,
      tenido, tenido_tipos,
      aprendizaje, ensenanza,
      prenda_principal, prendas_resumen, ceremonias_resumen,
      imagenes,
      // Contenido textual consolidado
      historia, significados,
      cat1, cat2, cat3, cat4,
      // ── Validación curatorial (calidad de los datos) ─────────────
      // El score combina la riqueza de los reactivos completados en cada
      // record (historia, significados, materiales, lengua, multimedia,
      // transmisión, manufactura, etc.) con un bonus logarítmico por
      // número de registros. La columna `incluida` marca las 60 técnicas
      // mejor calificadas, que son las que aparecen en el Atlas público.
      score_calidad: num(row['score_calidad']),
      score_volumen: num(row['score_volumen']),
      score_total:   num(row['score_total']),
      ranking:       num(row['ranking']),
      incluida:      ((row['incluida'] || '').trim().toLowerCase() === 'sí'),
    };
  })
  .filter(t => t.tecnica)
  // ─────────────────────────────────────────────────────────────────
  // FILTRO GLOBAL DE LA PLATAFORMA: solo técnicas con datos sólidos
  // (`incluida === sí` en data_by_technique_id.csv).
  //
  // El criterio NO es "número de registros". Se basa en un score que
  // evalúa la riqueza de los reactivos completados en cada ficha
  // (historia, significados, materiales, lengua indígena, multimedia,
  // transmisión, manufactura, teñido, etc.) más un bonus logarítmico
  // por volumen. Esto permite conservar técnicas con un solo registro
  // pero documentación rica (ej. Punto de cruz, Telar de cintura), y
  // descartar técnicas con reactivos pobres aunque tengan más fichas.
  //
  // Se aplica una sola vez aquí; todas las vistas (mapa, catálogo, red,
  // clasificación, reporte, "Acerca del Atlas") operan sobre el subset
  // resultante sin saberlo.
  // ─────────────────────────────────────────────────────────────────
  .filter(t => t.incluida);

  // Set con los nombres de técnicas que pasaron el filtro
  const tecnicasValidas = new Set(tecnicas.map(t => t.tecnica));

  // Re-derivar estadosMap y municipiosMap SOLO con técnicas válidas
  // (los originales se construyeron antes del filtro y tienen huérfanos)
  const estadosMapFiltrado = {};
  Object.keys(estadosMap).forEach(estado => {
    const ts = estadosMap[estado].filter(t => tecnicasValidas.has(t));
    if (ts.length > 0) estadosMapFiltrado[estado] = ts;
  });

  const municipiosMapFiltrado = {};
  Object.entries(municipiosMap).forEach(([key, set]) => {
    const ts = [...set].filter(t => tecnicasValidas.has(t));
    if (ts.length > 0) municipiosMapFiltrado[key] = ts.sort();
  });

  // Records crudos: solo los que pertenecen a técnicas válidas. Esto
  // garantiza que cualquier cálculo que vaya por records (reporte,
  // gráficas de "Acerca del Atlas") respete el filtro global.
  const recordsFiltrados = recordRows.filter(r =>
    tecnicasValidas.has((r['Tecnica'] || '').trim())
  );

  return {
    tecnicas,
    estados: estadosMapFiltrado,
    municipios: municipiosMapFiltrado,
    records: recordsFiltrados,
  };
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
    municipiosTecnicas = ATLAS.municipios || {};
    ATLAS.tecnicas.forEach(t => { tecnicasMap[t.tecnica] = t; });

    // Update header stats
    const nEstados = Object.keys(estadosTecnicas).length;
    const nTecnicas = ATLAS.tecnicas.length;
    const nRegistros = ATLAS.tecnicas.reduce((s, t) => s + t.n_fichas, 0);

    document.querySelector('.hstat[data-stat="registros"] .hstat-num').textContent = Math.round(nRegistros);
    document.querySelector('.hstat[data-stat="estados"] .hstat-num').textContent = nEstados;
    document.querySelector('.hstat[data-stat="tecnicas"] .hstat-num').textContent = nTecnicas;

    // Hide loading
    overlay.classList.add('hidden');
    setTimeout(() => { overlay.style.display = 'none'; }, 450);

    // Init map
    initMap();
    // Init map filter panel (needs ATLAS)
    initMapFilter();
    // Bind toggle de capa Comunidades (centroides)
    const comunidadesToggle = document.getElementById('comunidades-toggle');
    if (comunidadesToggle) {
      comunidadesToggle.addEventListener('change', e => {
        toggleComunidadesLayer(e.target.checked);
      });
    }

  } catch (err) {
    console.error('Error cargando CSV:', err);
    overlay.querySelector('.loading-spinner').style.display = 'none';
    overlay.querySelector('.loading-msg').style.display = 'none';
    errBanner.classList.add('visible');
    errBanner.querySelector('p').textContent =
      'No se pudieron cargar los datos del Atlas. Por favor recarga la página o intenta de nuevo más tarde.';
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
// MUNICIPIOS — funciones simétricas a las de estados
// La clave de lookup es "Estado||Municipio" (necesario porque hay nombres
// de municipio que se repiten entre estados, ej. "Hidalgo" en varios).
// ────────────────────────────────────────────────
function municipioKey(estado, municipio) { return `${estado}||${municipio}`; }

function countForMunicipio(estado, municipio) {
  // Tolerante a variaciones en la grafía del estado
  const target = normalizeEstado(estado);
  for (const [key, tecs] of Object.entries(municipiosTecnicas)) {
    const [est, mun] = key.split('||');
    if (mun === municipio && normalizeEstado(est) === target) return tecs.length;
  }
  return 0;
}

function getTecnicasForMunicipio(estado, municipio) {
  const target = normalizeEstado(estado);
  for (const [key, tecs] of Object.entries(municipiosTecnicas)) {
    const [est, mun] = key.split('||');
    if (mun === municipio && normalizeEstado(est) === target) return tecs;
  }
  return [];
}

function countForMunicipioFiltered(estado, municipio) {
  const tecs = getTecnicasForMunicipio(estado, municipio);
  return mapFilteredTecnicas ? tecs.filter(t => mapFilteredTecnicas.has(t)).length : tecs.length;
}

// ────────────────────────────────────────────────
// LISTA DE ESTADOS (panel derecho, siempre visible)
// ────────────────────────────────────────────────
let _statesListCache = null; // Array de nombres de estado tomados del geojson

// Busca el layer Leaflet de un estado por nombre (usado al hacer clic en un item)
function findLayerByStateName(name) {
  if (!geojsonLayer) return null;
  let found = null;
  geojsonLayer.eachLayer(l => {
    const lname = l.feature?.properties?.name || l.feature?.properties?.NAME_1 || '';
    if (normalizeEstado(lname) === normalizeEstado(name)) found = l;
  });
  return found;
}

// Construye / actualiza la lista de estados en el panel derecho.
// Se llama: (a) una vez cuando el geojson termina de cargar,
// (b) cada vez que cambia el estado seleccionado o el filtro activo.
function renderStatesList() {
  const wrap = document.getElementById('states-list');
  if (!wrap || !geojsonLayer) return;

  // Cache de nombres en orden alfabético la primera vez
  if (!_statesListCache) {
    const names = [];
    geojsonLayer.eachLayer(l => {
      const n = l.feature?.properties?.name || l.feature?.properties?.NAME_1 || '';
      if (n) names.push(n);
    });
    _statesListCache = names.sort((a, b) => a.localeCompare(b, 'es'));
  }

  wrap.innerHTML = '';
  _statesListCache.forEach(name => {
    const count = mapFilteredTecnicas ? countForEstadoFiltered(name) : countForEstado(name);
    const isActive = selectedStateName && normalizeEstado(selectedStateName) === normalizeEstado(name);
    const isDisabled = count === 0;
    const btn = document.createElement('button');
    btn.className = 'state-item' + (isActive ? ' active' : '') + (isDisabled ? ' disabled' : '');
    btn.innerHTML = `<span>${esc(name)}</span><span class="state-count">${count}</span>`;
    btn.addEventListener('click', () => {
      const layer = findLayerByStateName(name);
      if (layer) {
        onStateClick(name, layer, layer.feature);
        try { map.fitBounds(layer.getBounds(), { padding: [20, 20], maxZoom: 7 }); } catch (e) {}
      }
    });
    wrap.appendChild(btn);
  });
}

// ────────────────────────────────────────────────
// TABS
// ────────────────────────────────────────────────
let currentTab = 'mapa';
let networkInitialized = false;
let catalogInitialized = false;
let arbolInitialized = false;
let reporteInitialized = false;

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
    } else if (id === 'reporte' && !reporteInitialized) {
      reporteInitialized = true; initReporteView();
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

// ── Capa de comunidades (centroides de municipios) ──
let municipiosLayer = null;
let municipiosLoaded = false;       // true cuando el geojson terminó de cargar
let comunidadesVisible = false;     // si la capa está visible en el mapa
let selectedMunicipioKey = null;    // "Estado||Municipio" del seleccionado, o null

function initMap() {
  // Bounds aproximados de México con un poco de aire para que el viewport
  // pueda enmarcar el país completo sin permitir que el usuario se aleje al
  // resto de América. minZoom evita el zoom-out excesivo; maxBounds + viscosity
  // hacen que el mapa "regrese" si el usuario intenta arrastrar fuera.
  const MX_BOUNDS = L.latLngBounds([13.5, -120.0], [33.5, -84.5]);

  map = L.map('map', {
    zoomControl: false,
    scrollWheelZoom: true,
    minZoom: 5,
    maxBounds: MX_BOUNDS,
    maxBoundsViscosity: 1.0, // 1.0 = el mapa rebota al borde, no se sale
  });
  L.control.zoom({ position: 'bottomright' }).addTo(map);
  L.tileLayer('https://www.unesco.org/tiles/clearmap/{z}/{x}/{y}.png', {
    attribution: '© UNESCO',
    maxZoom: 19,
    minZoom: 5,
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
      // Construir la lista de estados en el panel derecho una vez que el geojson está cargado
      renderStatesList();
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

// ────────────────────────────────────────────────
// CAPA DE COMUNIDADES (centroides de municipios)
// Se dibujan como puntos circulares uniformes en magenta.
// Es una capa adicional que se enciende/apaga independientemente
// de la capa de estados (ambas pueden estar visibles a la vez).
// ────────────────────────────────────────────────
const COMUNIDAD_STYLE = {
  radius: 4,
  fillColor: '#B50552',     // magenta del Atlas
  color: '#FFFFFF',         // contorno blanco para legibilidad
  weight: 1.2,
  fillOpacity: 0.85,
};

const COMUNIDAD_STYLE_HOVER = {
  radius: 6,
  fillColor: '#B50552',
  color: '#1A1018',
  weight: 2,
  fillOpacity: 1,
};

const COMUNIDAD_STYLE_SELECTED = {
  radius: 7,
  fillColor: '#FB4801',     // naranja para diferenciar la selección
  color: '#1A1018',
  weight: 2.5,
  fillOpacity: 1,
};

function onMunicipioClick(estado, municipio, layer) {
  selectedMunicipioKey = municipioKey(estado, municipio);
  selectedStateName = estado;

  // Reset estilos de todas las comunidades y resaltar la seleccionada
  if (municipiosLayer) {
    municipiosLayer.eachLayer(l => l.setStyle(COMUNIDAD_STYLE));
  }
  layer.setStyle(COMUNIDAD_STYLE_SELECTED);

  let tecnicas = getTecnicasForMunicipio(estado, municipio);
  if (mapFilteredTecnicas) tecnicas = tecnicas.filter(t => mapFilteredTecnicas.has(t));

  document.getElementById('panel-title').textContent = municipio;
  document.getElementById('panel-subtitle').textContent =
    tecnicas.length > 0
      ? `${estado} · ${tecnicas.length} técnica${tecnicas.length !== 1 ? 's' : ''} identificada${tecnicas.length !== 1 ? 's' : ''}`
      : `${estado} · sin técnicas que coincidan con el filtro`;
  renderSidePanel(tecnicas);
}

// Carga lazy del geojson de centroides. Solo se invoca la primera vez que
// el usuario activa la capa de comunidades.
async function loadMunicipiosLayer() {
  if (municipiosLoaded) return municipiosLayer;
  const res = await fetch('mexico_municipios_centroids.geojson');
  if (!res.ok) throw new Error('No se pudo cargar el archivo de comunidades');
  const data = await res.json();

  // Filtrar features: SOLO las comunidades con datos.
  const conDatos = new Set();
  for (const key of Object.keys(municipiosTecnicas)) {
    const [est, mun] = key.split('||');
    conDatos.add(`${normalizeEstado(est)}||${mun.toLowerCase()}`);
  }

  const featuresFiltradas = data.features.filter(f => {
    const est = f.properties.estado_nombre || '';
    const mun = f.properties.NOMGEO || '';
    return conDatos.has(`${normalizeEstado(est)}||${mun.toLowerCase()}`);
  });

  municipiosLayer = L.geoJSON(
    { type: 'FeatureCollection', features: featuresFiltradas },
    {
      pointToLayer: (feat, latlng) => L.circleMarker(latlng, COMUNIDAD_STYLE),
      onEachFeature: (feat, layer) => {
        const est = feat.properties.estado_nombre || '';
        const mun = feat.properties.NOMGEO || '';

        // Tooltip simple: solo el nombre de la comunidad
        layer.bindTooltip(esc(mun), {
          className: 'leaflet-tooltip-atlas',
          direction: 'top',
          offset: [0, -6],
          sticky: false,
        });

        layer.on({
          mouseover: e => {
            const k = municipioKey(est, mun);
            if (k !== selectedMunicipioKey) e.target.setStyle(COMUNIDAD_STYLE_HOVER);
          },
          mouseout: e => {
            const k = municipioKey(est, mun);
            if (k !== selectedMunicipioKey) e.target.setStyle(COMUNIDAD_STYLE);
          },
          click: () => onMunicipioClick(est, mun, layer),
        });
      },
    }
  );

  municipiosLoaded = true;
  return municipiosLayer;
}

// Activa o desactiva la capa de comunidades (carga lazy la primera vez)
async function toggleComunidadesLayer(visible) {
  const checkbox = document.getElementById('comunidades-toggle');
  const wrap = checkbox?.closest('.map-layer-checkbox');

  if (visible && !municipiosLoaded) {
    wrap?.classList.add('loading');
    try { await loadMunicipiosLayer(); }
    catch (e) {
      console.error(e);
      wrap?.classList.remove('loading');
      if (checkbox) checkbox.checked = false;
      return;
    }
    wrap?.classList.remove('loading');
  }

  if (visible) {
    if (municipiosLayer && !map.hasLayer(municipiosLayer)) municipiosLayer.addTo(map);
    comunidadesVisible = true;
  } else {
    if (municipiosLayer && map.hasLayer(municipiosLayer)) map.removeLayer(municipiosLayer);
    comunidadesVisible = false;
    selectedMunicipioKey = null;
  }
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
  renderStatesList(); // refresca el item activo de la lista
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
      // Click en una técnica del sidebar → abre la ficha (igual que en el panel derecho)
      btn.addEventListener('click', () => {
        openFicha(t.tecnica);
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
  } else {
    mapFilteredTecnicas = new Set(filtered.map(t => t.tecnica));
    const parts = [cat1, cat2, cat3, cat4, tecnica].filter(Boolean);
    const label = parts.join(' › ');
    if (activeText) activeText.textContent = `${label} · ${filtered.length} técnica${filtered.length!==1?'s':''}`;
    if (activeEl) activeEl.style.display = '';
  }

  // Repaint estados
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
  // La capa de comunidades es de simbología uniforme, no se recolorea por filtro.
  // Solo limpiamos la selección visual.
  if (municipiosLayer) {
    municipiosLayer.eachLayer(l => l.setStyle(COMUNIDAD_STYLE));
  }
  // Reset panel
  selectedStateName = null;
  selectedMunicipioKey = null;
  document.getElementById('panel-title').textContent = 'Estados de la República';
  document.getElementById('panel-subtitle').textContent = hasFilter
    ? `Mostrando ${filtered.length} técnica${filtered.length!==1?'s':''} filtrada${filtered.length!==1?'s':''}`
    : 'Selecciona un estado para ver sus técnicas';
  document.getElementById('panel-body').innerHTML = `<div class="welcome-state"><h3>${hasFilter ? 'Filtro activo' : 'Explora el mapa'}</h3><p>${hasFilter ? 'Los estados coloreados tienen técnicas que coinciden con el filtro. Selecciona un estado de la lista o del mapa.' : 'Cada estado está coloreado según el número de técnicas documentadas.'}</p></div>`;
  renderStatesList(); // refresca conteos según filtro activo
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

  // Significado y Simbolismo — se renderiza como párrafo único. La fuente
  // es la columna `significados` del CSV, que contiene una redacción
  // curada que sintetiza los fragmentos S1-S5 de los records en un texto
  // coherente. Si en el futuro la columna trae múltiples párrafos
  // separados por dos saltos de línea, cada uno se renderiza por separado.
  if (t.significados && t.significados.length > 0) {
    const parrafos = t.significados[0]
      .split(/\n\s*\n/)
      .map(s => s.trim())
      .filter(Boolean);
    html += `<div class="section">
      <div class="section-head"><span class="section-title">Significado y Simbolismo</span></div>
      ${parrafos.map(p => `<p class="ficha-parrafo">${esc(p)}</p>`).join('')}
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
let dragNode = null;
let isPanning = false;
let panStart = null;

// ════════════════════════════════════════════════
// LAYERS — distintas formas de conectar las técnicas
// ════════════════════════════════════════════════
// Cada layer define:
//   - getHubs(t)      → array de strings con los hubs a los que conecta esa técnica
//   - color(hub)      → color del hub (y de los nodos-técnica cuando ese layer está activo)
//   - subOrder(hubs)  → orden visual de los hubs en el panel de subcategorías
// Una técnica puede conectar a varios hubs (excepto en Clasificación, que es jerárquica).

const LANG_PALETTE = ['#035A79','#B50552','#FB4801','#05B794','#C8932A','#0EB0E2','#E07AAA','#7B4F9D','#3A8E5C','#D14B2A','#1E6B8E','#A03864','#C76A1F','#2A8C7E'];
const STATE_PALETTE = ['#B50552','#035A79','#FB4801','#05B794','#C8932A'];
const APRENDIZAJE_HUBS = [
  { key: 'madre',      label: 'Madre',       color: '#B50552' },
  { key: 'abuela',     label: 'Abuela',      color: '#7B4F9D' },
  { key: 'tia',        label: 'Tía',         color: '#E07AAA' },
  { key: 'hermana',    label: 'Hermana',     color: '#FB4801' },
  { key: 'cunada',     label: 'Cuñada',      color: '#C76A1F' },
  { key: 'padre',      label: 'Padre',       color: '#035A79' },
  { key: 'instructor', label: 'Instructor/a',color: '#05B794' },
];
const ENSENANZA_HUBS = [
  { key: 'hijas',       label: 'Hijas',         color: '#B50552' },
  { key: 'hijos',       label: 'Hijos',         color: '#035A79' },
  { key: 'nietos',      label: 'Nietos',        color: '#7B4F9D' },
  { key: 'sobrinos',    label: 'Sobrinos',      color: '#FB4801' },
  { key: 'pareja',      label: 'Pareja',        color: '#05B794' },
  { key: 'estudiantes', label: 'Estudiantes',   color: '#C8932A' },
];
const TENIDO_HUBS = [
  { key: 'Plantas',           label: 'Plantas',          color: '#05B794' },
  { key: 'Minerales',         label: 'Minerales',        color: '#C8932A' },
  { key: 'Animales/Insectos', label: 'Animales/Insectos',color: '#B50552' },
];

// ── Hubs de MATERIALES ──────────────────────────────────────────────
// Agrupamos los materiales en familias curatoriales para que la red
// tenga un número manejable de hubs en lugar de los ~77 materiales
// únicos del catálogo. Cada hub agrupa palabras clave que aparecen en
// las listas de materiales. El orden de declaración importa: las
// categorías más específicas (Telares, Máquinas, Abalorios, Cueros)
// se evalúan ANTES que la categoría genérica "Herramientas" para
// evitar que un telar caiga en herramientas.
const MATERIALES_HUBS = [
  { key: 'Fibras vegetales',  color: '#05B794',
    keywords: ['algodón','algodon','lino','ixtle','henequén','henequen','palma',
               'fibra natural','fibras vegetales','torote','sotol','crin',
               'pita','fibra de maguey','maguey','fibra'] },
  { key: 'Fibras animales',   color: '#B50552',
    keywords: ['lana','lana de borrego','lana cardada','lana virgen','lana teñida',
               'seda','hilo de seda'] },
  { key: 'Hilos e hilazas',   color: '#FB4801',
    keywords: ['hilo','hilos','hilo de algodón','hilo de seda','hilo de lana',
               'hilo dorado','hilo industrial','hilo natural','hilo crochet',
               'hilo cera','hilo vela','hilo iris','hilo mish','hilo fino',
               'hilaza','estambre','hilera','hilo de bordar'] },
  { key: 'Telas y soportes',  color: '#035A79',
    keywords: ['tela','manta','popelina','popelín','popelin','cambray','dacrón',
               'dacron','lino','satín','satin','crepé','crepe','poliéster',
               'poliester','cuadrillé','cuadrille','cañamazo','organza','encaje',
               'tela industrial','tela de manta','tela de algodón',
               'tela hoja de pino','manta cruda','rayón'] },
  { key: 'Tintes naturales',  color: '#7B4F9D',
    keywords: ['añil','indigo','índigo','cochinilla','grana cochinilla','muicle',
               'aliso','huizache','pericón','palo de Brasil','palo de campeche',
               'corteza','cortezas','flores','hojas','raíz','raiz','flor de muerto',
               'tintes naturales','tinte','colorante','plantas tintóreas'] },
  // ── Telares y bastidores: separados de "Herramientas" porque son los
  // instrumentos icónicos del oficio textil mexicano (Saltillo, Tenancingo,
  // Teotitlán) y merecen su propia familia visual.
  { key: 'Telares y bastidores', color: '#0EB0E2',
    keywords: ['telar','telar de cintura','telar de pedal','telar pequeño',
               'telar de madera','urdidor','komen','malacate','huso','rueca',
               'lanzadera','peine','peine de carrizo','mecapal','bastidor',
               'aro','marco'] },
  // ── Máquinas: era moderna del oficio (s. XX en adelante).
  { key: 'Máquinas',          color: '#8B6914',
    keywords: ['máquina','maquina','máquina de coser','máquina de pedal',
               'máquina industrial'] },
  // ── Herramientas manuales: agujas, tijeras, ganchos.
  { key: 'Herramientas',      color: '#5A7D9A',
    keywords: ['aguja','agujas','aguja chica','aguja capotera','tijeras','tijera',
               'dedal','molde','plancha','gancho','crochet','ganchillo',
               'horquilla','mazo','maso'] },
  // ── Abalorios y aplicaciones: bordados con chaquira (huichol, nahua),
  // lentejuela, cuentas. Categoría visual única.
  { key: 'Abalorios y aplicaciones', color: '#E07AAA',
    keywords: ['chaquira','mostacilla','cuentas','cuenta','lentejuela',
               'aplicación','aplicacion','listón','liston','espejo','botones',
               'cinta de renacimiento'] },
  // ── Pieles y cueros: piteado, talabartería, cuera tamaulipeca.
  { key: 'Pieles y cueros',   color: '#A0522D',
    keywords: ['cuero','vaqueta','piel'] },
  { key: 'Otros',             color: '#C8932A',
    keywords: ['cera','cera de campeche','barniz','pegamento','jabón',
               'laqueado','espinas','hueso','leña','ollas','chatal'] },
];

function clasificarMaterial(material) {
  const m = (material || '').toLowerCase().trim();
  if (!m) return null;
  for (const hub of MATERIALES_HUBS) {
    if (hub.keywords.some(kw => m === kw || m.includes(kw))) return hub.key;
  }
  return null;
}

// Devuelve set de hubs (categorías) que aplican a una técnica
function hubsMaterialesDe(t) {
  const set = new Set();
  (t.materiales || '').split(',').forEach(m => {
    const hub = clasificarMaterial(m);
    if (hub) set.add(hub);
  });
  return [...set];
}

// ── Hubs de PRENDAS Y OBJETOS ───────────────────────────────────────
// Las prendas vienen del campo `prendas_resumen` con MUCHO ruido
// (variantes, plurales, erratas). Las agrupamos en categorías macro,
// destacando prendas icónicas mexicanas (rebozos, huipiles) en sus
// propios hubs. El orden de declaración importa: las categorías más
// específicas se evalúan ANTES que las genéricas para evitar que un
// huipil ceremonial caiga en "Indumentaria femenina" antes de llegar a
// "Indumentaria ceremonial".
const PRENDAS_HUBS = [
  // ── Indumentaria ceremonial: prioridad alta para que huipiles
  // ceremoniales no caigan en "Huipiles y blusas" antes.
  { key: 'Indumentaria ceremonial', color: '#7B4F9D',
    keywords: ['huipil ceremonial','huipiles ceremoniales','huipil de gala',
               'traje de gala','indumentaria ceremonial','traje ceremonial',
               'capa','capas','indumentaria de boda','traje de boda',
               'indumentaria de fiesta','vestido ceremonial','vestido de novia',
               'fiesta tradicional','yumare'] },
  // ── Rebozos y chales: categoría icónica de México (Saltillo,
  // Tenancingo, Santa María del Río).
  { key: 'Rebozos y chales',  color: '#FB4801',
    keywords: ['rebozo','rebozos','reboso','rebosos','chalina','chalinas',
               'chal','chales','pañoleta','pañoletas'] },
  // ── Huipiles y blusas: prendas más documentadas del Atlas.
  { key: 'Huipiles y blusas', color: '#B50552',
    keywords: ['huipil','huipiles','blusa','blusas','hipil','hipiles'] },
  // ── Resto de indumentaria femenina (faldas, vestidos, enaguas).
  { key: 'Indumentaria femenina', color: '#D85B89',
    keywords: ['enagua','enaguas','falda','faldas','vestido','vestidos','vestimenta',
               'quechquémitl','quechquemitl','enredo','enredos','nagua','naguas',
               'mañanita','mañanitas','ruana','ruanas','indumentaria','bestido',
               'bestidos','terno'] },
  { key: 'Indumentaria masculina', color: '#035A79',
    keywords: ['camisa','camisas','calzón','calzon','calzones','jorongo','jorongos',
               'sarape','sarapes','gabán','gaban','gabanes','poncho','ponchos',
               'guayabera','guayaberas','sombrero','sombreros','traje tradicional',
               'traje típico','traje tipico','pantalón','pantalon','pantalones',
               'chaleco','chalecos','cotón','coton','tomicoton','tomicotones',
               'chaqueta','chaquetas','corset'] },
  // ── Joyería textil: aretes, collares, anillos, pulseras hechos con
  // técnicas textiles (chaquira, randa, etc.).
  { key: 'Joyería textil',    color: '#E07AAA',
    keywords: ['arete','aretes','collar','collares','pulsera','pulseras',
               'pulcera','pulceras','anillo','anillos','tocado','tocados',
               'diadema','diademas'] },
  { key: 'Accesorios',        color: '#0EB0E2',
    keywords: ['bolsa','bolsas','bolso','bolsos','morral','morrales',
               'mochila','mochilas','cinturón','cinturon','cinturones',
               'faja','fajas','pañuelo','pañuelos',
               'monedero','monederos','cartera','carteras','billetera','billeteras',
               'cosmetiquera','cosmetiqueras','llavero','llaveros'] },
  { key: 'Hogar y mantelería', color: '#05B794',
    keywords: ['servilleta','servilletas','mantel','manteles','máteles',
               'camino de mesa','caminos de mesa','cojín','cojin','cojines',
               'almohada','almohadas','colcha','colchas','tapete','tapetes',
               'tortillero','tortilleros','portavaso','portavasos','funda','fundas',
               'colchón','colchon','cubrebandeja','manta de uso','sábana','sabana',
               'sábanas','sabanas','cobertor','cobertores','cobija','cobijas',
               'cortina','cortinas','carpeta','carpetas'] },
  { key: 'Decorativos',       color: '#C8932A',
    keywords: ['cuadro','cuadros','figura','figuras','tapiz','tapices','adorno',
               'adornos','muñeco','muñecos','muñeca','muñecas','animalitos',
               'mural','murales','escultura','esculturas','arte utilitario',
               'decoración','decoracion'] },
  { key: 'Abrigo',            color: '#8B6914',
    keywords: ['suéter','sueter','suéteres','sueteres','chamarra','chamarras',
               'saco','sacos','abrigo','abrigos','bufanda','bufandas','chuj',
               'pañal','pañales'] },
];

function clasificarPrenda(prenda) {
  const p = (prenda || '').toLowerCase().trim()
    .replace(/\(\d+\)/g, '')   // quitar "(1)", "(2)" etc.
    .replace(/[,\.]/g, '')     // puntuación
    .trim();
  if (!p) return null;
  for (const hub of PRENDAS_HUBS) {
    if (hub.keywords.some(kw => p === kw || p.includes(kw))) return hub.key;
  }
  return null;
}

// Devuelve set de hubs (categorías) que aplican a una técnica
function hubsPrendasDe(t) {
  const set = new Set();
  // 1) prenda_principal (canónica, una por técnica)
  if (t.prenda_principal) {
    const hub = clasificarPrenda(t.prenda_principal);
    if (hub) set.add(hub);
  }
  // 2) prendas_resumen (lista compleja). Tomamos las top 8 prendas mencionadas
  if (t.prendas_resumen) {
    const items = t.prendas_resumen.split(/[;,]/)
      .map(s => s.split(':')[0].trim())
      .filter(Boolean)
      .slice(0, 8);
    items.forEach(it => {
      const hub = clasificarPrenda(it);
      if (hub) set.add(hub);
    });
  }
  return [...set];
}

// Cache para colores asignados por hub (lenguas y estados — generados al vuelo)
let _layerColorCache = { lenguas: {}, estados: {} };

function colorForLengua(lengua) {
  if (_layerColorCache.lenguas[lengua]) return _layerColorCache.lenguas[lengua];
  const idx = Object.keys(_layerColorCache.lenguas).length;
  const c = LANG_PALETTE[idx % LANG_PALETTE.length];
  _layerColorCache.lenguas[lengua] = c;
  return c;
}
function colorForEstado(estado) {
  if (_layerColorCache.estados[estado]) return _layerColorCache.estados[estado];
  const idx = Object.keys(_layerColorCache.estados).length;
  const c = STATE_PALETTE[idx % STATE_PALETTE.length];
  _layerColorCache.estados[estado] = c;
  return c;
}

const NET_LAYERS = [
  {
    id: 'clasificacion', label: 'Clasificación', icon: '◆',
    description: 'Categorías expertas',
    getHubs: t => [t.cat2 || t.cat1 || 'Sin clasificar'],
    hubColor: hubLabel => CAT2_COLOR[hubLabel] || COLORS.arena,
    nodeColor: t => getCatColor(t),
    listHubs: () => {
      const set = new Set();
      ATLAS.tecnicas.forEach(t => set.add(t.cat2 || t.cat1 || 'Sin clasificar'));
      return [...set].sort();
    },
  },
  {
    id: 'lenguas', label: 'Lenguas indígenas', icon: '✦',
    description: 'Lenguas en que se nombra y enseña la técnica',
    getHubs: t => (t.lenguas || []).filter(l => l && l.toLowerCase() !== 'español'),
    hubColor: hubLabel => colorForLengua(hubLabel),
    nodeColor: t => {
      const langs = (t.lenguas || []).filter(l => l && l.toLowerCase() !== 'español');
      return langs.length ? colorForLengua(langs[0]) : COLORS.arena;
    },
    listHubs: () => {
      const counts = {};
      ATLAS.tecnicas.forEach(t => {
        (t.lenguas || []).forEach(l => {
          if (!l || l.toLowerCase() === 'español') return;
          counts[l] = (counts[l] || 0) + 1;
        });
      });
      return Object.keys(counts).sort((a, b) => counts[b] - counts[a]);
    },
  },
  {
    id: 'tenidos', label: 'Teñidos', icon: '◉',
    description: 'Tipo de tinte utilizado',
    getHubs: t => t.tenido_tipos || [],
    hubColor: hubLabel => (TENIDO_HUBS.find(h => h.key === hubLabel) || {}).color || COLORS.arena,
    nodeColor: t => {
      const hub = (t.tenido_tipos || [])[0];
      return hub ? ((TENIDO_HUBS.find(h => h.key === hub) || {}).color || COLORS.arena) : COLORS.arena;
    },
    listHubs: () => TENIDO_HUBS.map(h => h.key),
  },
  {
    id: 'materiales', label: 'Materiales', icon: '◈',
    description: 'Familias de materiales utilizados',
    getHubs: t => hubsMaterialesDe(t),
    hubColor: hubLabel => (MATERIALES_HUBS.find(h => h.key === hubLabel) || {}).color || COLORS.arena,
    nodeColor: t => {
      const hubs = hubsMaterialesDe(t);
      const first = hubs[0];
      return first ? ((MATERIALES_HUBS.find(h => h.key === first) || {}).color || COLORS.arena) : COLORS.arena;
    },
    listHubs: () => MATERIALES_HUBS.map(h => h.key),
  },
  {
    id: 'prendas', label: 'Prendas y objetos', icon: '◇',
    description: 'Tipo de prenda u objeto resultante',
    getHubs: t => hubsPrendasDe(t),
    hubColor: hubLabel => (PRENDAS_HUBS.find(h => h.key === hubLabel) || {}).color || COLORS.arena,
    nodeColor: t => {
      const hubs = hubsPrendasDe(t);
      const first = hubs[0];
      return first ? ((PRENDAS_HUBS.find(h => h.key === first) || {}).color || COLORS.arena) : COLORS.arena;
    },
    listHubs: () => PRENDAS_HUBS.map(h => h.key),
  },
  {
    id: 'estados', label: 'Estados', icon: '⬢',
    description: 'Distribución geográfica',
    getHubs: t => t.estados || [],
    hubColor: hubLabel => colorForEstado(hubLabel),
    nodeColor: t => {
      const e = (t.estados || [])[0];
      return e ? colorForEstado(e) : COLORS.arena;
    },
    listHubs: () => {
      const counts = {};
      ATLAS.tecnicas.forEach(t => (t.estados || []).forEach(e => { counts[e] = (counts[e] || 0) + 1; }));
      return Object.keys(counts).sort((a, b) => counts[b] - counts[a]);
    },
  },
  {
    id: 'aprendizaje', label: 'Aprendizaje', icon: '↺',
    description: 'De quién aprendió la técnica',
    getHubs: t => {
      const hubs = [];
      APRENDIZAJE_HUBS.forEach(h => {
        if ((t.aprendizaje && t.aprendizaje[h.key]) > 0) hubs.push(h.label);
      });
      return hubs;
    },
    hubColor: hubLabel => (APRENDIZAJE_HUBS.find(h => h.label === hubLabel) || {}).color || COLORS.arena,
    nodeColor: t => {
      // Color del hub principal (el de mayor conteo)
      let best = null, bestN = 0;
      APRENDIZAJE_HUBS.forEach(h => {
        const n = (t.aprendizaje && t.aprendizaje[h.key]) || 0;
        if (n > bestN) { bestN = n; best = h; }
      });
      return best ? best.color : COLORS.arena;
    },
    listHubs: () => APRENDIZAJE_HUBS.map(h => h.label),
  },
  {
    id: 'ensenanza', label: 'Enseñanza', icon: '↻',
    description: 'A quién le enseña la técnica',
    getHubs: t => {
      const hubs = [];
      ENSENANZA_HUBS.forEach(h => {
        if ((t.ensenanza && t.ensenanza[h.key]) > 0) hubs.push(h.label);
      });
      return hubs;
    },
    hubColor: hubLabel => (ENSENANZA_HUBS.find(h => h.label === hubLabel) || {}).color || COLORS.arena,
    nodeColor: t => {
      let best = null, bestN = 0;
      ENSENANZA_HUBS.forEach(h => {
        const n = (t.ensenanza && t.ensenanza[h.key]) || 0;
        if (n > bestN) { bestN = n; best = h; }
      });
      return best ? best.color : COLORS.arena;
    },
    listHubs: () => ENSENANZA_HUBS.map(h => h.label),
  },
];

let activeLayerId = 'clasificacion';
function getActiveLayer() { return NET_LAYERS.find(l => l.id === activeLayerId) || NET_LAYERS[0]; }

function initNetwork() {
  networkCanvas = document.getElementById('network-canvas');
  networkCtx = networkCanvas.getContext('2d');
  resizeCanvas();
  window.addEventListener('resize', resizeCanvas);
  buildNetworkData();
  startSimulation();
  bindNetworkEvents();
  renderLayerDropdown();
  renderSublayerChips();
  initNetworkSearch();
}

function resizeCanvas() {
  const parent = networkCanvas.parentElement;
  const rect = parent.getBoundingClientRect();
  networkCanvas.width  = rect.width || 900;
  networkCanvas.height = rect.height || 600;
  if (networkNodes.length) drawNetwork();
}

function buildNetworkData() {
  const layer = getActiveLayer();
  const tecnicas = ATLAS.tecnicas;

  // 1) Reunir todos los hubs únicos que aparecen en este layer
  const hubSet = new Set();
  tecnicas.forEach(t => layer.getHubs(t).forEach(h => h && hubSet.add(h)));
  const hubs = [...hubSet];

  // 2) Calcular tamaño de cada hub según número de técnicas conectadas
  const hubCount = {};
  tecnicas.forEach(t => layer.getHubs(t).forEach(h => { if (h) hubCount[h] = (hubCount[h] || 0) + 1; }));

  networkNodes = [];

  // Nodos de hub
  hubs.forEach(hub => {
    const count = hubCount[hub] || 1;
    networkNodes.push({
      id: `hub:${hub}`, label: hub, type: 'grupo', hubKey: hub,
      r: Math.max(16, Math.min(32, 14 + Math.sqrt(count) * 2)),
      color: layer.hubColor(hub),
      x: Math.random() * 600 + 100, y: Math.random() * 400 + 80,
      vx: 0, vy: 0, fx: null, fy: null,
    });
  });

  // Nodos de técnica — guardamos lista de hubs conectados para usarla en forces y links
  tecnicas.forEach(t => {
    const myHubs = layer.getHubs(t).filter(Boolean);
    networkNodes.push({
      id: t.tecnica, label: t.tecnica, type: 'tecnica',
      cat1: t.cat1, cat2: t.cat2, cat3: t.cat3, cat4: t.cat4,
      grupo: t.cat1, estados: t.estados, lenguas: t.lenguas,
      n_fichas: t.n_fichas,
      hubs: myHubs,
      r: Math.max(6, Math.min(16, 5 + t.n_fichas * 0.7)),
      color: layer.nodeColor(t),
      x: Math.random() * 700 + 50, y: Math.random() * 500 + 40,
      vx: 0, vy: 0, fx: null, fy: null,
    });
  });

  // 3) Construir links — una arista por (técnica, hub)
  networkLinks = [];
  networkNodes.filter(n => n.type === 'tecnica').forEach(n => {
    n.hubs.forEach(h => {
      networkLinks.push({ source: `hub:${h}`, target: n.id, strength: 0.6 });
    });
  });
}

// Cuando hay un hub destacado, las técnicas resaltadas son las que tienen ese hub en su lista.
function nodeMatchesHighlight(n, hi) {
  if (!hi) return false;
  if (n.id === hi.id) return true;
  if (hi.type === 'grupo' && n.type === 'tecnica' && (n.hubs || []).includes(hi.hubKey)) return true;
  return false;
}

let _simAlpha = 0;
let _animFrame = null;

function startSimulation() {
  const W = networkCanvas.width, H = networkCanvas.height;
  const grupos = networkNodes.filter(n => n.type === 'grupo');

  // Distribuir hubs en círculo, fijos
  grupos.forEach((g, i) => {
    const angle = (i / grupos.length) * Math.PI * 2 - Math.PI / 2;
    const rx = Math.min(W, H) * 0.32;
    const ry = Math.min(W, H) * 0.30;
    g.x = W / 2 + Math.cos(angle) * rx;
    g.y = H / 2 + Math.sin(angle) * ry;
    g.fx = g.x; g.fy = g.y;
  });

  // Posicionar técnicas cerca del centroide de sus hubs
  networkNodes.filter(n => n.type === 'tecnica').forEach(n => {
    if (n.hubs.length === 0) {
      n.x = W / 2 + (Math.random() - .5) * 200;
      n.y = H / 2 + (Math.random() - .5) * 160;
    } else {
      let cx = 0, cy = 0, found = 0;
      n.hubs.forEach(h => {
        const hub = networkNodes.find(nn => nn.id === `hub:${h}`);
        if (hub) { cx += hub.x; cy += hub.y; found++; }
      });
      if (found) {
        cx /= found; cy /= found;
        const angle = Math.random() * Math.PI * 2;
        const dist = 30 + Math.random() * 60;
        n.x = cx + Math.cos(angle) * dist;
        n.y = cy + Math.sin(angle) * dist;
      } else {
        n.x = W / 2; n.y = H / 2;
      }
    }
    n.vx = 0; n.vy = 0;
  });

  // Calentamiento silencioso
  _simAlpha = 1;
  for (let i = 0; i < 150; i++) { _simAlpha *= 0.97; applyForces(_simAlpha); }

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
  const W = networkCanvas.width, H = networkCanvas.height;
  const tecs = networkNodes.filter(n => n.type === 'tecnica');

  // Repulsión — solo entre técnicas que comparten al menos un hub (evita repulsión global cara)
  for (let i = 0; i < tecs.length; i++) {
    for (let j = i + 1; j < tecs.length; j++) {
      const a = tecs[i], b = tecs[j];
      const sharesHub = a.hubs.some(h => b.hubs.includes(h));
      if (!sharesHub) continue;
      let dx = b.x - a.x || 0.01, dy = b.y - a.y || 0.01;
      const dist2 = dx * dx + dy * dy;
      const dist = Math.sqrt(dist2) || 0.01;
      const ideal = a.r + b.r + 18;
      if (dist < ideal * 4) {
        const force = (alpha * 45) / dist2;
        a.vx -= dx * force; a.vy -= dy * force;
        b.vx += dx * force; b.vy += dy * force;
      }
    }
  }

  // Atracción a hubs vía links (resorte)
  networkLinks.forEach(link => {
    const s = networkNodes.find(n => n.id === link.source);
    const t = networkNodes.find(n => n.id === link.target);
    if (!s || !t) return;
    const dx = t.x - s.x, dy = t.y - s.y;
    const dist = Math.sqrt(dx * dx + dy * dy) || 0.01;
    const preferred = 70 + t.r * 1.5;
    // Si la técnica conecta a múltiples hubs, debilitamos la fuerza para que se quede en el centroide
    const linkScale = 1 / Math.max(1, t.hubs.length);
    const force = (dist - preferred) * 0.025 * alpha * linkScale;
    const fx = (dx / dist) * force, fy = (dy / dist) * force;
    if (s.fx === null || s.fx === undefined) { s.vx += fx; s.vy += fy; }
    t.vx -= fx * 0.5; t.vy -= fy * 0.5;
  });

  // Integración + clamp a la pantalla
  networkNodes.forEach(n => {
    if (n.fx !== null && n.fx !== undefined) { n.x = n.fx; n.vx = 0; return; }
    if (n.fy !== null && n.fy !== undefined) { n.y = n.fy; n.vy = 0; return; }
    n.vx *= 0.72; n.vy *= 0.72;
    n.x += n.vx; n.y += n.vy;
    n.x = Math.max(n.r + 10, Math.min(W - n.r - 10, n.x));
    n.y = Math.max(n.r + 10, Math.min(H - n.r - 10, n.y));
  });
}

function drawNetwork() {
  const canvas = networkCanvas, ctx = networkCtx;
  const W = canvas.width, H = canvas.height;

  ctx.save();
  ctx.clearRect(0, 0, W, H);
  ctx.fillStyle = '#FDFAF8';
  ctx.fillRect(0, 0, W, H);
  ctx.translate(transform.x, transform.y);
  ctx.scale(transform.k, transform.k);

  // Aristas
  networkLinks.forEach(link => {
    const s = networkNodes.find(n => n.id === link.source);
    const t = networkNodes.find(n => n.id === link.target);
    if (!s || !t) return;
    const isHovLink = hoveredNode && (hoveredNode.id === s.id || hoveredNode.id === t.id);
    const isHiLink  = highlightedNode && (
      highlightedNode.id === s.id || highlightedNode.id === t.id
      || (highlightedNode.type === 'grupo' && t.type === 'tecnica' && (t.hubs || []).includes(highlightedNode.hubKey))
    );
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

  // Nodos: técnicas detrás, hubs delante
  const sorted = [...networkNodes].sort((a, b) => {
    if (a.type === 'grupo' && b.type !== 'grupo') return 1;
    if (b.type === 'grupo' && a.type !== 'grupo') return -1;
    return 0;
  });

  sorted.forEach(n => {
    const isHov = hoveredNode && hoveredNode.id === n.id;
    const isHi  = nodeMatchesHighlight(n, highlightedNode);
    const dimmed = (highlightedNode || hoveredNode) && !isHi && !isHov;
    const r = n.r * (isHov || isHi ? 1.3 : 1);

    if (n.type === 'grupo' || isHov || isHi) {
      ctx.shadowColor = n.color;
      ctx.shadowBlur = n.type === 'grupo' ? 18 : 10;
    }

    ctx.beginPath();
    ctx.arc(n.x, n.y, r, 0, Math.PI * 2);

    if (n.type === 'grupo') {
      ctx.fillStyle = dimmed ? (n.color + '55') : n.color;
      ctx.fill();
      ctx.shadowBlur = 0;
      ctx.strokeStyle = dimmed ? 'rgba(255,255,255,.2)' : 'rgba(255,255,255,.85)';
      ctx.lineWidth = 2.5; ctx.stroke();
    } else {
      ctx.fillStyle = dimmed ? (n.color + '30') : (isHov || isHi ? n.color : n.color + 'cc');
      ctx.fill();
      ctx.shadowBlur = 0;
    }

    // Etiquetas
    if (n.type === 'grupo') {
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
  let best = null, bestDist = Infinity;
  networkNodes.forEach(n => {
    const d = Math.sqrt((n.x - x) ** 2 + (n.y - y) ** 2);
    if (d < n.r + 6 && d < bestDist) { bestDist = d; best = n; }
  });
  return best;
}

function bindNetworkEvents() {
  const canvas = networkCanvas;
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
      const layer = getActiveLayer();
      if (node.type === 'tecnica') {
        const catBreadcrumb = [node.cat1, node.cat2].filter(Boolean).join(' › ');
        const hubsLine = node.hubs && node.hubs.length
          ? `<span style="font-size:.7rem;opacity:.65">${esc(layer.label)}: ${esc(node.hubs.slice(0, 3).join(', '))}${node.hubs.length > 3 ? '…' : ''}</span><br>`
          : '';
        tooltip.innerHTML = `<strong>${esc(node.label)}</strong>
          <span style="font-size:.72rem;opacity:.7">${esc(catBreadcrumb)}</span><br>
          ${hubsLine}${node.n_fichas} registro${node.n_fichas !== 1 ? 's' : ''}`;
      } else {
        const count = networkNodes.filter(n => n.type === 'tecnica' && (n.hubs || []).includes(node.hubKey)).length;
        tooltip.innerHTML = `<strong>${esc(node.label)}</strong>
          <span style="font-size:.72rem;opacity:.7">${esc(layer.label)}</span><br>
          ${count} técnica${count !== 1 ? 's' : ''}`;
      }
      tooltip.style.left = (mx + 14) + 'px';
      tooltip.style.top = (my - 10) + 'px';
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
      else if (node && node.type === 'grupo') {
        highlightedNode = highlightedNode?.id === node.id ? null : node;
        renderSublayerChips(); // refresca el highlight del chip activo
        drawNetwork();
      }
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
    const newK = Math.max(0.3, Math.min(3, transform.k * delta));
    transform.x = mx - (mx - transform.x) * (newK / transform.k);
    transform.y = my - (my - transform.y) * (newK / transform.k);
    transform.k = newK; drawNetwork();
  }, { passive: false });

  document.getElementById('net-zoom-in').addEventListener('click',  () => { transform.k = Math.min(3, transform.k * 1.25); drawNetwork(); });
  document.getElementById('net-zoom-out').addEventListener('click', () => { transform.k = Math.max(0.3, transform.k * 0.8); drawNetwork(); });
  document.getElementById('net-reset').addEventListener('click',    () => { transform = { x: 0, y: 0, k: 1 }; drawNetwork(); });
}

function renderLayerDropdown() {
  const trigger = document.getElementById('network-dropdown-trigger');
  const menu = document.getElementById('network-dropdown-menu');
  const currentLabel = document.getElementById('network-dropdown-current');
  const dropdown = document.getElementById('network-layer-dropdown');
  if (!trigger || !menu || !currentLabel || !dropdown) return;

  const active = getActiveLayer();
  currentLabel.textContent = active.label;

  menu.innerHTML = '';
  NET_LAYERS.forEach(layer => {
    const item = document.createElement('button');
    item.type = 'button';
    item.className = 'network-dropdown-item' + (layer.id === activeLayerId ? ' active' : '');
    item.dataset.layerId = layer.id;
    item.innerHTML = `
      <span class="network-dropdown-item-text">
        <span class="network-dropdown-item-label">${esc(layer.label)}</span>
        <span class="network-dropdown-item-desc">${esc(layer.description || '')}</span>
      </span>`;
    item.addEventListener('click', () => {
      dropdown.classList.remove('open');
      trigger.setAttribute('aria-expanded', 'false');
      switchLayer(layer.id);
    });
    menu.appendChild(item);
  });

  // Toggle (solo bindear una vez)
  if (!trigger.dataset.bound) {
    trigger.dataset.bound = '1';
    trigger.addEventListener('click', e => {
      e.stopPropagation();
      const isOpen = dropdown.classList.toggle('open');
      trigger.setAttribute('aria-expanded', isOpen ? 'true' : 'false');
    });
    document.addEventListener('click', e => {
      if (!dropdown.contains(e.target)) {
        dropdown.classList.remove('open');
        trigger.setAttribute('aria-expanded', 'false');
      }
    });
    document.addEventListener('keydown', e => {
      if (e.key === 'Escape') {
        dropdown.classList.remove('open');
        trigger.setAttribute('aria-expanded', 'false');
      }
    });
  }
}

function switchLayer(layerId) {
  if (layerId === activeLayerId) return;
  activeLayerId = layerId;
  highlightedNode = null;
  hoveredNode = null;
  transform = { x: 0, y: 0, k: 1 };
  buildNetworkData();
  startSimulation();
  renderLayerDropdown();
  renderSublayerChips();
}

function renderSublayerChips() {
  const layer = getActiveLayer();
  const wrap = document.getElementById('network-sublayer-chips');
  const labelEl = document.getElementById('network-sublayer-label');
  if (!wrap || !labelEl) return;

  labelEl.textContent = layer.label;
  wrap.innerHTML = '';

  const hubs = layer.listHubs();
  hubs.forEach(hub => {
    const color = layer.hubColor(hub);
    const chip = document.createElement('button');
    chip.className = 'network-sublayer-chip';
    chip.style.setProperty('--chip-color', color);
    if (highlightedNode && highlightedNode.type === 'grupo' && highlightedNode.hubKey === hub) {
      chip.classList.add('active');
    }
    chip.innerHTML = `<span class="network-sublayer-chip-dot"></span>${esc(hub)}`;
    chip.addEventListener('click', () => {
      const hubNode = networkNodes.find(n => n.type === 'grupo' && n.hubKey === hub);
      if (!hubNode) return;
      // Toggle: si ya estaba resaltado, quitamos; si no, lo activamos y centramos vista
      if (highlightedNode && highlightedNode.id === hubNode.id) {
        highlightedNode = null;
      } else {
        highlightedNode = hubNode;
        const W = networkCanvas.width, H = networkCanvas.height;
        transform.x = W / 2 - hubNode.x * transform.k;
        transform.y = H / 2 - hubNode.y * transform.k;
      }
      renderSublayerChips();
      drawNetwork();
    });
    wrap.appendChild(chip);
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
  const ac = document.getElementById('network-search-ac');
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
        <div class="chart-card-title">Subcategoría experta</div>
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
// SECCIÓN REPORTE — Constructor de reportes personalizados
// Wizard de 3 pasos: (1) selección, (2) secciones, (3) vista previa.
// ────────────────────────────────────────────────
let reporteCurrentStep = 1;

// Estado de la selección. Cada filtro funciona como un set; si los tres
// están vacíos, el reporte incluye TODAS las técnicas del Atlas.
const reporteState = {
  titulo: '',
  autor: '',
  cat1: new Set(),       // CAT-N-1 (categorías madre)
  cat2: new Set(),       // CAT-N-2 (subcategorías)
  estados: new Set(),    // Nombres de estado
  tecnicas: new Set(),   // Nombres exactos de técnica
  secciones: new Set(),  // IDs de secciones incluidas (ver REPORTE_SECCIONES)
};

// Definición de las 8 secciones del documento.
// Cada una declara si aplica al subset filtrado (predicado `applies`).
const REPORTE_SECCIONES = [
  {
    id: 'resumen', label: 'Resumen ejecutivo',
    desc: 'Cifras totales y datos clave de tu selección.',
    icon: '◆', defaultOn: true,
    applies: subset => subset.tecnicas.length > 0,
  },
  {
    id: 'geografico', label: 'Distribución geográfica',
    desc: 'Mapa coloreado y tabla de técnicas por estado.',
    icon: '⬢', defaultOn: true,
    applies: subset => subset.estados.length > 0,
  },
  {
    id: 'catalogo', label: 'Catálogo de técnicas',
    desc: 'Ficha resumida de cada técnica con foto principal.',
    icon: '◉', defaultOn: true,
    applies: subset => subset.tecnicas.length > 0,
  },
  {
    id: 'categorias', label: 'Categorías expertas',
    desc: 'Distribución por categoría y subcategoría experta con gráfica de barras.',
    icon: '▦', defaultOn: true,
    applies: subset => subset.tecnicas.length > 1,
  },
  {
    id: 'lenguas', label: 'Lenguas indígenas',
    desc: 'Lenguas en que se nombra y enseña cada técnica.',
    icon: '✦', defaultOn: true,
    applies: subset => (subset.records || []).some(r => {
      const l = (r['Lengua'] || '').trim().toLowerCase();
      return l && l !== 'español' && l !== 'na';
    }),
  },
  {
    id: 'transmision', label: 'Aprendizaje y enseñanza',
    desc: 'De quién aprendieron y a quién enseñan los artesanos.',
    icon: '↺', defaultOn: true,
    applies: subset => (subset.records || []).length > 0,
  },
  {
    id: 'tenidos', label: 'Teñidos',
    desc: 'Tipos de tinte (plantas, minerales, animales) por técnica.',
    icon: '◐', defaultOn: true,
    applies: subset => (subset.records || []).some(r =>
      parseInt(r['Plantas'] || 0, 10) > 0 ||
      parseInt(r['Minerales'] || 0, 10) > 0 ||
      parseInt(r['Animales'] || 0, 10) > 0
    ),
  },
  {
    id: 'testimonios', label: 'Testimonios cualitativos',
    desc: 'Historia y significados narrados por los artesanos.',
    icon: '❝', defaultOn: false,
    applies: subset => subset.tecnicas.some(t =>
      (t.historia && t.historia.length > 30) ||
      (t.significados && t.significados.some(s => s && s.length > 20))
    ),
  },
];

function initReporteView() {
  if (!ATLAS) return;

  // Inicializar secciones con sus defaults
  REPORTE_SECCIONES.forEach(s => {
    if (s.defaultOn) reporteState.secciones.add(s.id);
  });

  // Botones de navegación
  document.getElementById('reporte-prev')?.addEventListener('click', () => goToStep(reporteCurrentStep - 1));
  document.getElementById('reporte-next')?.addEventListener('click', () => {
    if (reporteCurrentStep < 3) goToStep(reporteCurrentStep + 1);
    else generarReporte();
  });

  // Click en pasos del stepper
  document.querySelectorAll('.reporte-step').forEach(step => {
    step.addEventListener('click', () => {
      const target = parseInt(step.dataset.step, 10);
      if (target && target !== reporteCurrentStep) goToStep(target);
    });
  });

  // Inicializar cada paso
  initReportePaso1();
  initReportePaso2();
  bindReportePaso3();
  goToStep(1);
}

function goToStep(n) {
  if (n < 1 || n > 3) return;
  reporteCurrentStep = n;

  document.querySelectorAll('.reporte-step').forEach(s => {
    const sn = parseInt(s.dataset.step, 10);
    s.classList.toggle('active', sn === n);
    s.classList.toggle('done', sn < n);
  });

  document.querySelectorAll('.reporte-panel').forEach(p => {
    p.classList.toggle('active', parseInt(p.dataset.panel, 10) === n);
  });

  const prevBtn = document.getElementById('reporte-prev');
  const nextBtn = document.getElementById('reporte-next');
  const progress = document.getElementById('reporte-progress');
  if (prevBtn) prevBtn.disabled = (n === 1);
  if (nextBtn) {
    if (n === 3) { nextBtn.style.display = 'none'; }
    else { nextBtn.style.display = ''; nextBtn.textContent = 'Siguiente →'; }
  }
  if (progress) progress.textContent = `Paso ${n} de 3`;

  // Al entrar al Paso 3, generar la vista previa automáticamente
  if (n === 3) {
    setTimeout(() => generarReporte(), 50);
  }
}

// ─────────────────────────────────────────────
// PASO 1 — Selección
// ─────────────────────────────────────────────

function initReportePaso1() {
  // Inputs de metadata
  const tituloEl = document.getElementById('reporte-titulo');
  const autorEl = document.getElementById('reporte-autor');
  if (tituloEl) tituloEl.addEventListener('input', e => { reporteState.titulo = e.target.value; });
  if (autorEl)  autorEl.addEventListener('input',  e => { reporteState.autor  = e.target.value; });

  // Renderizar chips de categorías
  renderReporteChipsCategorias();
  // Renderizar chips de estados
  renderReporteChipsEstados();
  // Inicializar buscador de técnicas
  initReporteBuscadorTecnicas();
  // Bind presets
  document.querySelectorAll('.reporte-preset').forEach(btn => {
    btn.addEventListener('click', () => aplicarPreset(btn.dataset.preset));
  });

  actualizarContador();
}

function renderReporteChipsCategorias() {
  const wrap = document.getElementById('reporte-chips-cat');
  if (!wrap) return;
  wrap.innerHTML = '';

  // CAT-N-1 con sus CAT-N-2 anidadas
  const cat1Map = {};
  ATLAS.tecnicas.forEach(t => {
    const c1 = t.cat1 || 'Sin clasificar';
    const c2 = t.cat2 || '';
    if (!cat1Map[c1]) cat1Map[c1] = new Set();
    if (c2) cat1Map[c1].add(c2);
  });

  Object.keys(cat1Map).sort().forEach(c1 => {
    const color = CAT1_COLOR[c1] || COLORS.arena;
    // Chip CAT-N-1
    const chip1 = document.createElement('button');
    chip1.className = 'reporte-chip reporte-chip-cat1';
    chip1.style.setProperty('--chip-color', color);
    if (reporteState.cat1.has(c1)) chip1.classList.add('active');
    chip1.innerHTML = `<span class="reporte-chip-dot"></span>${esc(c1)}`;
    chip1.addEventListener('click', () => {
      if (reporteState.cat1.has(c1)) reporteState.cat1.delete(c1);
      else reporteState.cat1.add(c1);
      renderReporteChipsCategorias();
      actualizarContador();
    });
    wrap.appendChild(chip1);

    // Chips CAT-N-2 hijas
    [...cat1Map[c1]].sort().forEach(c2 => {
      const chip2 = document.createElement('button');
      chip2.className = 'reporte-chip reporte-chip-cat2';
      chip2.style.setProperty('--chip-color', color);
      if (reporteState.cat2.has(c2)) chip2.classList.add('active');
      chip2.innerHTML = `<span class="reporte-chip-arrow">↳</span>${esc(c2)}`;
      chip2.addEventListener('click', () => {
        if (reporteState.cat2.has(c2)) reporteState.cat2.delete(c2);
        else reporteState.cat2.add(c2);
        renderReporteChipsCategorias();
        actualizarContador();
      });
      wrap.appendChild(chip2);
    });
  });
}

function renderReporteChipsEstados() {
  const wrap = document.getElementById('reporte-chips-estados');
  if (!wrap) return;
  wrap.innerHTML = '';

  const estados = Object.keys(estadosTecnicas).sort((a, b) => a.localeCompare(b, 'es'));
  estados.forEach(est => {
    const count = estadosTecnicas[est].length;
    const chip = document.createElement('button');
    chip.className = 'reporte-chip reporte-chip-estado';
    if (reporteState.estados.has(est)) chip.classList.add('active');
    chip.innerHTML = `<span>${esc(est)}</span><span class="reporte-chip-count">${count}</span>`;
    chip.addEventListener('click', () => {
      if (reporteState.estados.has(est)) reporteState.estados.delete(est);
      else reporteState.estados.add(est);
      renderReporteChipsEstados();
      actualizarContador();
    });
    wrap.appendChild(chip);
  });
}

function initReporteBuscadorTecnicas() {
  const input = document.getElementById('reporte-search-tec');
  const ac = document.getElementById('reporte-search-ac');
  const chipsWrap = document.getElementById('reporte-chips-tecs');
  if (!input || !ac || !chipsWrap) return;

  function renderTecnicasSeleccionadas() {
    chipsWrap.innerHTML = '';
    [...reporteState.tecnicas].sort().forEach(nombre => {
      const t = ATLAS.tecnicas.find(x => x.tecnica === nombre);
      const color = t ? getCatColor(t) : COLORS.arena;
      const chip = document.createElement('button');
      chip.className = 'reporte-chip reporte-chip-tec active';
      chip.style.setProperty('--chip-color', color);
      chip.innerHTML = `<span class="reporte-chip-dot"></span>${esc(nombre)}<span class="reporte-chip-x">×</span>`;
      chip.addEventListener('click', () => {
        reporteState.tecnicas.delete(nombre);
        renderTecnicasSeleccionadas();
        actualizarContador();
      });
      chipsWrap.appendChild(chip);
    });
  }

  input.addEventListener('input', () => {
    const q = input.value.toLowerCase().trim();
    ac.innerHTML = ''; ac.style.display = 'none';
    if (q.length < 2) return;
    const matches = ATLAS.tecnicas
      .filter(t => t.tecnica.toLowerCase().includes(q) && !reporteState.tecnicas.has(t.tecnica))
      .slice(0, 10);
    if (!matches.length) return;
    matches.forEach(t => {
      const item = document.createElement('div');
      item.className = 'reporte-ac-item';
      const color = getCatColor(t);
      item.innerHTML = `<span class="reporte-ac-dot" style="background:${color}"></span>
        <span class="reporte-ac-name">${esc(t.tecnica)}</span>
        <span class="reporte-ac-cat">${esc(t.cat1 || '')}</span>`;
      item.addEventListener('click', () => {
        reporteState.tecnicas.add(t.tecnica);
        input.value = '';
        ac.style.display = 'none';
        renderTecnicasSeleccionadas();
        actualizarContador();
      });
      ac.appendChild(item);
    });
    ac.style.display = 'block';
  });

  document.addEventListener('click', e => {
    if (!input.contains(e.target) && !ac.contains(e.target)) ac.style.display = 'none';
  });

  renderTecnicasSeleccionadas();
}

function aplicarPreset(preset) {
  // Reset
  reporteState.cat1.clear();
  reporteState.cat2.clear();
  reporteState.estados.clear();
  reporteState.tecnicas.clear();

  switch (preset) {
    case 'all':
      // Sin filtros = todas las técnicas
      break;
    case 'oaxaca':
      reporteState.estados.add('Oaxaca');
      break;
    case 'chiapas':
      reporteState.estados.add('Chiapas');
      break;
    case 'bordados':
      reporteState.cat2.add('Bordados');
      break;
    case 'tejidos':
      reporteState.cat2.add('Tejidos');
      break;
    case 'top10':
      // 10 técnicas con más fichas
      const top = [...ATLAS.tecnicas].sort((a, b) => b.n_fichas - a.n_fichas).slice(0, 10);
      top.forEach(t => reporteState.tecnicas.add(t.tecnica));
      break;
    case 'clear':
      // ya está limpio
      break;
  }

  renderReporteChipsCategorias();
  renderReporteChipsEstados();
  document.getElementById('reporte-search-tec').dispatchEvent(new Event('input'));
  // Refrescar chips de técnicas seleccionadas (reusa lógica)
  initReporteBuscadorTecnicas();
  actualizarContador();
}

// Calcula el subset de técnicas que cumple TODOS los filtros activos.
// CRUCIAL: también devuelve `records` filtrado, que es la fuente de verdad
// para todas las secciones cuantitativas del reporte (aprendizaje, teñido,
// enseñanza, etc.). Los conteos pre-agregados de `t.aprendizaje`, `t.tenido`
// etc. son sobre TODOS los registros nacionales — no respetan el filtro
// de estado, así que NO se deben usar para el reporte filtrado.
function obtenerSubset() {
  const tieneFiltroCat = reporteState.cat1.size > 0 || reporteState.cat2.size > 0;
  const tieneFiltroEst = reporteState.estados.size > 0;
  const tieneFiltroTec = reporteState.tecnicas.size > 0;

  let tecs = ATLAS.tecnicas;

  if (tieneFiltroCat) {
    tecs = tecs.filter(t => {
      const okCat1 = reporteState.cat1.size === 0 || reporteState.cat1.has(t.cat1);
      const okCat2 = reporteState.cat2.size === 0 || reporteState.cat2.has(t.cat2);
      return okCat1 && okCat2;
    });
  }

  if (tieneFiltroEst) {
    const setEst = new Set([...reporteState.estados].map(e => normalizeEstado(e)));
    tecs = tecs.filter(t => (t.estados || []).some(e => setEst.has(normalizeEstado(e))));
  }

  if (tieneFiltroTec) {
    tecs = tecs.filter(t => reporteState.tecnicas.has(t.tecnica));
  }

  // Set de nombres de técnica que pasaron el filtro
  const tecsSet = new Set(tecs.map(t => t.tecnica));

  // Filtrar records crudos: deben pertenecer a una técnica del subset Y,
  // si el usuario filtró estados, el record debe ser de uno de esos estados
  let records = (ATLAS.records || []).filter(r => tecsSet.has((r['Tecnica'] || '').trim()));

  if (tieneFiltroEst) {
    const setEst = new Set([...reporteState.estados].map(e => normalizeEstado(e)));
    records = records.filter(r => setEst.has(normalizeEstado((r['Estado'] || '').trim())));
  }

  // Estados que aparecen en el subset (derivados de los records filtrados, no de t.estados,
  // para que respete también el filtro de estado del usuario)
  const estSet = new Set();
  records.forEach(r => {
    const e = (r['Estado'] || '').trim();
    if (e) estSet.add(e);
  });

  return {
    tecnicas: tecs,
    records,
    estados: [...estSet],
    totalRegistros: records.length,
  };
}

// El contador del paso 1 fue eliminado a pedido del usuario.
// Se conserva la función vacía para no romper las llamadas existentes.
function actualizarContador() { /* no-op */ }

// ─────────────────────────────────────────────
// PASO 2 — Secciones del reporte
// ─────────────────────────────────────────────

function initReportePaso2() {
  const wrap = document.getElementById('reporte-secciones');
  if (!wrap) return;
  wrap.innerHTML = '';

  REPORTE_SECCIONES.forEach(sec => {
    const card = document.createElement('label');
    card.className = 'reporte-seccion-card';
    card.dataset.seccionId = sec.id;
    card.innerHTML = `
      <input type="checkbox" data-section="${sec.id}" ${reporteState.secciones.has(sec.id) ? 'checked' : ''}>
      <div class="reporte-seccion-icon">${sec.icon}</div>
      <div class="reporte-seccion-text">
        <div class="reporte-seccion-label">${esc(sec.label)}</div>
        <div class="reporte-seccion-desc">${esc(sec.desc)}</div>
      </div>
      <span class="reporte-seccion-check">✓</span>
    `;
    const cb = card.querySelector('input');
    cb.addEventListener('change', () => {
      if (cb.checked) reporteState.secciones.add(sec.id);
      else reporteState.secciones.delete(sec.id);
      card.classList.toggle('selected', cb.checked);
    });
    if (cb.checked) card.classList.add('selected');
    wrap.appendChild(card);
  });

  document.getElementById('reporte-sec-all')?.addEventListener('click', () => {
    REPORTE_SECCIONES.forEach(s => reporteState.secciones.add(s.id));
    initReportePaso2();
  });
  document.getElementById('reporte-sec-none')?.addEventListener('click', () => {
    reporteState.secciones.clear();
    initReportePaso2();
  });
  document.getElementById('reporte-sec-default')?.addEventListener('click', () => {
    reporteState.secciones.clear();
    REPORTE_SECCIONES.forEach(s => { if (s.defaultOn) reporteState.secciones.add(s.id); });
    initReportePaso2();
  });
}

// ─────────────────────────────────────────────
// PASO 3 — Generación del reporte
// ─────────────────────────────────────────────

let _reporteChartsInstances = []; // para destruir Charts viejos al regenerar
let _reporteLastSubset = null;     // cache para regenerar y exportar

// Punto de entrada: se llama al pasar al Paso 3 o al hacer clic en Regenerar
function generarReporte() {
  const subset = obtenerSubset();
  if (subset.tecnicas.length === 0) {
    alert('Tu selección no incluye ninguna técnica. Vuelve al Paso 1 y ajusta los filtros.');
    goToStep(1);
    return;
  }

  if (reporteState.secciones.size === 0) {
    alert('No has marcado ninguna sección. Vuelve al Paso 2 y selecciona al menos una.');
    goToStep(2);
    return;
  }

  // Confirmación si el reporte va a ser muy grande
  if (subset.tecnicas.length > 80 && reporteState.secciones.has('catalogo')) {
    if (!confirm(
      `Tu selección tiene ${subset.tecnicas.length} técnicas y la sección "Catálogo" está activa, ` +
      `lo que puede generar un PDF de 80+ páginas. ¿Quieres continuar?`
    )) return;
  }

  _reporteLastSubset = subset;

  // Limpiar charts previos
  _reporteChartsInstances.forEach(c => { try { c.destroy(); } catch(e){} });
  _reporteChartsInstances = [];

  const frame = document.getElementById('reporte-preview-frame');
  if (!frame) return;

  // Generar el HTML del reporte y mostrarlo
  const html = construirReporteHTML(subset);
  frame.innerHTML = html;

  // Renderizar charts (después de inyectar el DOM)
  setTimeout(() => renderizarChartsReporte(subset), 50);

  // Actualizar título de la vista previa
  const titulo = reporteState.titulo.trim() || 'Reporte del Atlas Textil';
  document.getElementById('reporte-preview-title').textContent = titulo;
  document.getElementById('reporte-preview-subtitle').textContent =
    `${subset.tecnicas.length} técnicas · ${subset.estados.length} estados · ${subset.totalRegistros} registros`;
}

// ─────────────────────────────────────────────
// HTML del reporte
// ─────────────────────────────────────────────

function construirReporteHTML(subset) {
  const titulo = reporteState.titulo.trim() || 'Reporte del Atlas Textil';
  const autor = reporteState.autor.trim() || '';
  const fecha = new Date().toLocaleDateString('es-MX', { year: 'numeric', month: 'long', day: 'numeric' });

  // Resumen automático de filtros aplicados (subtítulo)
  const filtrosResumen = construirResumenFiltros();

  let html = `<div class="rep-doc">`;

  // ─── Portada ───
  html += construirPortada(titulo, autor, fecha, filtrosResumen, subset);

  // ─── Secciones ─── (en orden definido)
  REPORTE_SECCIONES.forEach(sec => {
    if (!reporteState.secciones.has(sec.id)) return;
    if (!sec.applies(subset)) return;
    html += construirSeccion(sec.id, subset);
  });

  // ─── Pie ───
  html += `
    <footer class="rep-footer">
      <div class="rep-footer-line"></div>
      <p>Generado el ${esc(fecha)} a partir del Atlas Nacional de Técnicas del Arte Textil.<br>
      Encuentro Nacional de Arte Textil "Original" · Secretaría de Cultura · UNESCO México.</p>
    </footer>
  `;

  html += `</div>`;
  return html;
}

function construirResumenFiltros() {
  const partes = [];
  if (reporteState.cat1.size) partes.push([...reporteState.cat1].join(', '));
  if (reporteState.cat2.size) partes.push([...reporteState.cat2].join(', '));
  if (reporteState.estados.size) partes.push(`Estados: ${[...reporteState.estados].slice(0, 4).join(', ')}${reporteState.estados.size > 4 ? `… (+${reporteState.estados.size - 4})` : ''}`);
  if (reporteState.tecnicas.size) partes.push(`${reporteState.tecnicas.size} técnica${reporteState.tecnicas.size !== 1 ? 's' : ''} específica${reporteState.tecnicas.size !== 1 ? 's' : ''}`);
  return partes.length ? partes.join(' · ') : 'Atlas completo';
}

function construirPortada(titulo, autor, fecha, filtrosResumen, subset) {
  return `
    <section class="rep-cover">
      <div class="rep-cover-logos">
        <img src="atlas_plataforma_logos.png" alt="Logos institucionales" />
      </div>
      <div class="rep-cover-eyebrow">Atlas Nacional de Técnicas del Arte Textil</div>
      <h1 class="rep-cover-title">${esc(titulo)}</h1>
      <div class="rep-cover-subtitle">${esc(filtrosResumen)}</div>
      <div class="rep-cover-stats">
        <div class="rep-stat">
          <div class="rep-stat-num">${subset.tecnicas.length}</div>
          <div class="rep-stat-lbl">Técnicas</div>
        </div>
        <div class="rep-stat">
          <div class="rep-stat-num">${subset.estados.length}</div>
          <div class="rep-stat-lbl">Estados</div>
        </div>
        <div class="rep-stat">
          <div class="rep-stat-num">${subset.totalRegistros}</div>
          <div class="rep-stat-lbl">Registros</div>
        </div>
      </div>
      <div class="rep-cover-meta">
        ${autor ? `<div><span>Elaborado por</span>${esc(autor)}</div>` : ''}
        <div><span>Fecha</span>${esc(fecha)}</div>
      </div>
    </section>
  `;
}

function construirSeccion(id, subset) {
  switch (id) {
    case 'resumen':      return construirResumenEjecutivo(subset);
    case 'geografico':   return construirDistribucionGeografica(subset);
    case 'catalogo':     return construirCatalogoTecnicas(subset);
    case 'categorias':   return construirCategoriasExpertas(subset);
    case 'lenguas':      return construirLenguasIndigenas(subset);
    case 'transmision':  return construirTransmision(subset);
    case 'tenidos':      return construirTenidos(subset);
    case 'testimonios':  return construirTestimonios(subset);
    default: return '';
  }
}

// ─── Secciones individuales ───

function construirResumenEjecutivo(subset) {
  const tecs = subset.tecnicas;
  const promFichas = (subset.totalRegistros / tecs.length).toFixed(1);

  // Técnica con más registros DENTRO DEL SUBSET (no la nacional)
  const cuentaPorTec = {};
  subset.records.forEach(r => {
    const k = (r['Tecnica'] || '').trim();
    cuentaPorTec[k] = (cuentaPorTec[k] || 0) + 1;
  });
  const topTec = Object.entries(cuentaPorTec).sort((a, b) => b[1] - a[1])[0];

  const cat1Counts = {};
  tecs.forEach(t => { const c = t.cat1 || 'Sin clasificar'; cat1Counts[c] = (cat1Counts[c] || 0) + 1; });
  const topCat1 = Object.entries(cat1Counts).sort((a, b) => b[1] - a[1])[0];

  // Lenguas presentes en los records filtrados (no en las técnicas nacionales)
  const lenguasSet = new Set();
  subset.records.forEach(r => {
    const l = (r['Lengua'] || '').trim();
    if (l && l.toLowerCase() !== 'español' && l.toLowerCase() !== 'na') lenguasSet.add(l);
  });

  return `
    <section class="rep-section rep-section-resumen">
      <header class="rep-section-head">
        <span class="rep-section-num">01</span>
        <h2>Resumen ejecutivo</h2>
      </header>
      <p class="rep-lead">Este reporte abarca <strong>${tecs.length} técnicas textiles</strong> documentadas
        en <strong>${subset.estados.length} estados</strong> de la República Mexicana, con un total de
        <strong>${subset.totalRegistros} registros</strong> levantados durante el Encuentro Nacional de Arte Textil "Original".</p>

      <div class="rep-stat-grid">
        <div class="rep-stat-card">
          <div class="rep-stat-card-num">${promFichas}</div>
          <div class="rep-stat-card-lbl">Registros promedio<br>por técnica</div>
        </div>
        <div class="rep-stat-card">
          <div class="rep-stat-card-num">${topCat1 ? topCat1[1] : 0}</div>
          <div class="rep-stat-card-lbl">Técnicas en<br>${topCat1 ? esc(topCat1[0]) : 'la categoría principal'}</div>
        </div>
        <div class="rep-stat-card">
          <div class="rep-stat-card-num">${lenguasSet.size}</div>
          <div class="rep-stat-card-lbl">Lenguas indígenas<br>asociadas</div>
        </div>
        <div class="rep-stat-card">
          <div class="rep-stat-card-num">${topTec ? topTec[1] : 0}</div>
          <div class="rep-stat-card-lbl">Registros de la técnica<br>más documentada${topTec ? `<br><em>${esc(topTec[0])}</em>` : ''}</div>
        </div>
      </div>
    </section>
  `;
}

function construirDistribucionGeografica(subset) {
  // Contar registros por estado, y técnicas únicas por estado, desde el subset filtrado
  const cuentaRegPorEstado = {};
  const tecsPorEstado = {};
  subset.records.forEach(r => {
    const est = (r['Estado'] || '').trim();
    const tec = (r['Tecnica'] || '').trim();
    if (!est) return;
    cuentaRegPorEstado[est] = (cuentaRegPorEstado[est] || 0) + 1;
    if (!tecsPorEstado[est]) tecsPorEstado[est] = new Set();
    tecsPorEstado[est].add(tec);
  });

  const filas = Object.keys(cuentaRegPorEstado)
    .map(est => ({
      estado: est,
      tecnicas: tecsPorEstado[est].size,
      registros: cuentaRegPorEstado[est],
      pct: ((tecsPorEstado[est].size / subset.tecnicas.length) * 100).toFixed(1),
    }))
    .sort((a, b) => b.tecnicas - a.tecnicas);

  let tablaHtml = `<table class="rep-table"><thead><tr>
    <th>Estado</th><th>Técnicas</th><th>Registros</th><th>% del subset</th></tr></thead><tbody>`;
  filas.forEach(f => {
    tablaHtml += `<tr>
      <td>${esc(f.estado)}</td>
      <td class="num">${f.tecnicas}</td>
      <td class="num">${f.registros}</td>
      <td class="num"><div class="rep-bar-cell"><div class="rep-bar-fill" style="width:${f.pct}%"></div><span>${f.pct}%</span></div></td>
    </tr>`;
  });
  tablaHtml += `</tbody></table>`;

  return `
    <section class="rep-section">
      <header class="rep-section-head">
        <span class="rep-section-num">02</span>
        <h2>Distribución geográfica</h2>
      </header>
      <p class="rep-prose">Las técnicas seleccionadas se documentaron en <strong>${filas.length}
        ${filas.length === 1 ? 'estado' : 'estados'}</strong> de la República.
        ${filas[0] ? `${esc(filas[0].estado)} concentra la mayor diversidad con ${filas[0].tecnicas}
        técnicas y ${filas[0].registros} registros.` : ''}</p>
      ${tablaHtml}
    </section>
  `;
}

function construirCatalogoTecnicas(subset) {
  // Pre-calcular conteos por técnica desde records filtrados
  const recordsPorTec = {};
  subset.records.forEach(r => {
    const t = (r['Tecnica'] || '').trim();
    if (!recordsPorTec[t]) recordsPorTec[t] = [];
    recordsPorTec[t].push(r);
  });

  const ordenadas = [...subset.tecnicas].sort((a, b) => {
    if (a.cat1 !== b.cat1) return (a.cat1 || '').localeCompare(b.cat1 || '');
    if (a.cat2 !== b.cat2) return (a.cat2 || '').localeCompare(b.cat2 || '');
    return a.tecnica.localeCompare(b.tecnica, 'es');
  });

  let html = `<section class="rep-section">
    <header class="rep-section-head">
      <span class="rep-section-num">03</span>
      <h2>Catálogo de técnicas</h2>
    </header>
    <p class="rep-prose">Ficha resumida de cada técnica incluida en este reporte.</p>
    <div class="rep-cards">`;

  let lastCat1 = null;
  ordenadas.forEach(t => {
    if (t.cat1 !== lastCat1) {
      lastCat1 = t.cat1;
      const color = CAT1_COLOR[t.cat1] || COLORS.arena;
      html += `</div><div class="rep-cat-divider" style="border-color:${color};color:${color}">${esc(t.cat1 || 'Sin clasificar')}</div><div class="rep-cards">`;
    }

    const breadcrumb = [t.cat2, t.cat3, t.cat4].filter(Boolean).join(' › ');
    const recs = recordsPorTec[t.tecnica] || [];
    const nRegs = recs.length;

    // Estados, lenguas y materiales DEL SUBSET filtrado, no globales
    const estSet = new Set(); recs.forEach(r => { const e = (r['Estado'] || '').trim(); if (e) estSet.add(e); });
    const lenSet = new Set(); recs.forEach(r => {
      const l = (r['Lengua'] || '').trim();
      if (l && l.toLowerCase() !== 'español' && l.toLowerCase() !== 'na') lenSet.add(l);
    });
    const matSet = new Set(); recs.forEach(r => {
      (r['Materiales'] || '').split(/[,;]/).forEach(m => {
        const v = m.trim().toLowerCase();
        if (v && v !== 'na' && v.length < 40) matSet.add(v);
      });
    });

    const estadosTxt = [...estSet].slice(0, 3).join(', ') + (estSet.size > 3 ? `, +${estSet.size - 3}` : '');
    const lenguasTxt = [...lenSet].slice(0, 3).join(', ');
    const materialesTxt = [...matSet].slice(0, 6).join(', ');

    html += `<div class="rep-card">
      <div class="rep-card-body">
        <div class="rep-card-breadcrumb">${esc(breadcrumb)}</div>
        <h3 class="rep-card-title">${esc(t.tecnica)}</h3>
        <div class="rep-card-meta">
          <span><strong>${nRegs}</strong> registro${nRegs !== 1 ? 's' : ''}</span>
          ${estadosTxt ? `<span>${esc(estadosTxt)}</span>` : ''}
        </div>
        ${lenguasTxt ? `<div class="rep-card-tag rep-card-tag-lengua">Lenguas: ${esc(lenguasTxt)}</div>` : ''}
        ${materialesTxt ? `<div class="rep-card-tag rep-card-tag-mat">Materiales: ${esc(materialesTxt.slice(0, 100))}${materialesTxt.length > 100 ? '…' : ''}</div>` : ''}
      </div>
    </div>`;
  });

  html += `</div></section>`;
  return html;
}

function construirCategoriasExpertas(subset) {
  const cat1Counts = {};
  const cat2Counts = {};
  subset.tecnicas.forEach(t => {
    if (t.cat1) cat1Counts[t.cat1] = (cat1Counts[t.cat1] || 0) + 1;
    if (t.cat2) cat2Counts[t.cat2] = (cat2Counts[t.cat2] || 0) + 1;
  });

  return `
    <section class="rep-section">
      <header class="rep-section-head">
        <span class="rep-section-num">04</span>
        <h2>Categorías expertas</h2>
      </header>
      <p class="rep-prose">Distribución de técnicas según la clasificación experta del Atlas.</p>
      <div class="rep-charts-grid">
        <div class="rep-chart-wrap">
          <h4>Por categoría madre</h4>
          <div class="rep-chart-canvas-wrap"><canvas data-rep-chart="cat1"></canvas></div>
        </div>
        <div class="rep-chart-wrap">
          <h4>Por subcategoría</h4>
          <div class="rep-chart-canvas-wrap"><canvas data-rep-chart="cat2"></canvas></div>
        </div>
      </div>
    </section>
  `;
}

function construirLenguasIndigenas(subset) {
  // Conteo de menciones por lengua en los records filtrados, y técnicas únicas por lengua
  const tecsPorLengua = {};
  subset.records.forEach(r => {
    const l = (r['Lengua'] || '').trim();
    const t = (r['Tecnica'] || '').trim();
    if (!l || l.toLowerCase() === 'español' || l.toLowerCase() === 'na') return;
    if (!tecsPorLengua[l]) tecsPorLengua[l] = new Set();
    tecsPorLengua[l].add(t);
  });

  const totalLenguas = Object.keys(tecsPorLengua).length;

  return `
    <section class="rep-section">
      <header class="rep-section-head">
        <span class="rep-section-num">05</span>
        <h2>Lenguas indígenas</h2>
      </header>
      <p class="rep-prose">Las técnicas de este reporte se nombran y enseñan en <strong>${totalLenguas}
        lengua${totalLenguas !== 1 ? 's' : ''} indígena${totalLenguas !== 1 ? 's' : ''}</strong>,
        además del español. La gráfica muestra cuántas técnicas se asocian a cada una.</p>
      <div class="rep-chart-canvas-wrap rep-chart-tall"><canvas data-rep-chart="lenguas"></canvas></div>
    </section>
  `;
}

function construirTransmision(subset) {
  // Conteos desde records filtrados (cada record es una persona/familia)
  const aprende = { madre: 0, abuela: 0, tia: 0, hermana: 0, cunada: 0, padre: 0, instructor: 0 };
  const ensena  = { hijas: 0, hijos: 0, nietos: 0, sobrinos: 0, pareja: 0, estudiantes: 0 };
  const recordCols = {
    aprende: { madre: 'Madre', abuela: 'Abuela', tia: 'Tia', hermana: 'Hermana',
               cunada: 'Cunada', padre: 'Padre', instructor: 'Instructora' },
    ensena:  { hijas: 'Hijas', hijos: 'Hijos', nietos: 'Nietos', sobrinos: 'Sobrinos',
               pareja: 'Pareja', estudiantes: 'Estudiantes' },
  };

  subset.records.forEach(r => {
    Object.entries(recordCols.aprende).forEach(([k, col]) => {
      if (parseInt(r[col] || 0, 10) > 0) aprende[k]++;
    });
    Object.entries(recordCols.ensena).forEach(([k, col]) => {
      if (parseInt(r[col] || 0, 10) > 0) ensena[k]++;
    });
  });

  const topAprende = Object.entries(aprende).sort((a, b) => b[1] - a[1])[0];
  const topEnsena  = Object.entries(ensena).sort((a, b) => b[1] - a[1])[0];

  const labelAprende = { madre: 'la madre', abuela: 'la abuela', tia: 'una tía', hermana: 'una hermana', cunada: 'una cuñada', padre: 'el padre', instructor: 'un instructor o instructora' };
  const labelEnsena = { hijas: 'hijas', hijos: 'hijos', nietos: 'nietos', sobrinos: 'sobrinos', pareja: 'pareja', estudiantes: 'estudiantes' };

  return `
    <section class="rep-section">
      <header class="rep-section-head">
        <span class="rep-section-num">06</span>
        <h2>Aprendizaje y enseñanza</h2>
      </header>
      <p class="rep-prose">La transmisión de las técnicas en este reporte sigue patrones eminentemente
        familiares y matrilineales.
        ${topAprende && topAprende[1] > 0 ? `La fuente principal de aprendizaje declarada es <strong>${labelAprende[topAprende[0]]}</strong> (${topAprende[1]} menciones),` : ''}
        ${topEnsena && topEnsena[1] > 0 ? `y la enseñanza se dirige principalmente a <strong>${labelEnsena[topEnsena[0]]}</strong> (${topEnsena[1]} menciones).` : ''}</p>
      <div class="rep-charts-grid">
        <div class="rep-chart-wrap">
          <h4>De quién aprendieron</h4>
          <div class="rep-chart-canvas-wrap"><canvas data-rep-chart="aprende"></canvas></div>
        </div>
        <div class="rep-chart-wrap">
          <h4>A quién enseñan</h4>
          <div class="rep-chart-canvas-wrap"><canvas data-rep-chart="ensena"></canvas></div>
        </div>
      </div>
    </section>
  `;
}

function construirTenidos(subset) {
  // Cuenta de records que reportan cada tipo de tinte (no técnicas, registros)
  const counts = { Plantas: 0, Minerales: 0, 'Animales/Insectos': 0 };
  const tecsConTenido = new Set();

  subset.records.forEach(r => {
    const tec = (r['Tecnica'] || '').trim();
    let tieneAlguno = false;
    if (parseInt(r['Plantas'] || 0, 10) > 0)   { counts.Plantas++; tieneAlguno = true; }
    if (parseInt(r['Minerales'] || 0, 10) > 0) { counts.Minerales++; tieneAlguno = true; }
    if (parseInt(r['Animales'] || 0, 10) > 0)  { counts['Animales/Insectos']++; tieneAlguno = true; }
    if (tieneAlguno) tecsConTenido.add(tec);
  });

  const totalRecConTenido = counts.Plantas + counts.Minerales + counts['Animales/Insectos'];

  let html = `<section class="rep-section">
    <header class="rep-section-head">
      <span class="rep-section-num">07</span>
      <h2>Teñidos</h2>
    </header>
    <p class="rep-prose"><strong>${tecsConTenido.size}</strong> de las ${subset.tecnicas.length} técnicas
      reportan algún tipo de teñido en los registros de este subset. La distribución de menciones por origen del tinte es:</p>
    <table class="rep-table"><thead><tr><th>Tipo de tinte</th><th>Menciones</th><th>% del subset con teñido</th></tr></thead><tbody>`;

  Object.entries(counts).forEach(([tipo, n]) => {
    const pct = totalRecConTenido ? ((n / totalRecConTenido) * 100).toFixed(1) : '0.0';
    html += `<tr>
      <td>${esc(tipo)}</td>
      <td class="num">${n}</td>
      <td class="num"><div class="rep-bar-cell"><div class="rep-bar-fill" style="width:${pct}%"></div><span>${pct}%</span></div></td>
    </tr>`;
  });
  html += `</tbody></table></section>`;
  return html;
}

function construirTestimonios(subset) {
  // Recogemos historia y significados como testimonios cualitativos
  const testimonios = [];
  subset.tecnicas.forEach(t => {
    if (t.historia && t.historia.length > 30) {
      testimonios.push({ tecnica: t.tecnica, texto: t.historia, tipo: 'Historia' });
    }
    if (t.significados && t.significados.length) {
      t.significados.slice(0, 1).forEach(s => {
        if (s && s.length > 20) testimonios.push({ tecnica: t.tecnica, texto: s, tipo: 'Significado' });
      });
    }
  });

  if (testimonios.length === 0) {
    return `<section class="rep-section">
      <header class="rep-section-head">
        <span class="rep-section-num">08</span>
        <h2>Testimonios</h2>
      </header>
      <p class="rep-prose">No hay testimonios cualitativos disponibles para esta selección.</p>
    </section>`;
  }

  // Limitar a 30 para no saturar el documento
  const seleccion = testimonios.slice(0, 30);

  let html = `<section class="rep-section">
    <header class="rep-section-head">
      <span class="rep-section-num">08</span>
      <h2>Testimonios</h2>
    </header>
    <p class="rep-prose">Citas de los artesanos y artesanas participantes
      ${testimonios.length > 30 ? `(muestra de ${seleccion.length} de ${testimonios.length})` : ''}.</p>
    <div class="rep-testimonios">`;

  seleccion.forEach(t => {
    const txt = t.texto.length > 280 ? t.texto.slice(0, 277) + '…' : t.texto;
    html += `<blockquote class="rep-testim">
      <p>${esc(txt)}</p>
      <cite>${esc(t.tecnica)}${t.tipo ? ` · ${t.tipo}` : ''}</cite>
    </blockquote>`;
  });

  html += `</div></section>`;
  return html;
}

// ─────────────────────────────────────────────
// Charts (se renderizan después de inyectar el HTML)
// ─────────────────────────────────────────────

function renderizarChartsReporte(subset) {
  const palette = ['#B50552', '#035A79', '#FB4801', '#05B794', '#C8932A', '#0EB0E2', '#7B4F9D', '#E07AAA'];

  // CAT-N-1
  const cv1 = document.querySelector('canvas[data-rep-chart="cat1"]');
  if (cv1) {
    const counts = {};
    subset.tecnicas.forEach(t => { if (t.cat1) counts[t.cat1] = (counts[t.cat1] || 0) + 1; });
    const labels = Object.keys(counts);
    const data = labels.map(l => counts[l]);
    const colors = labels.map(l => CAT1_COLOR[l] || palette[0]);
    _reporteChartsInstances.push(new Chart(cv1, {
      type: 'bar',
      data: { labels, datasets: [{ data, backgroundColor: colors, borderRadius: 4 }] },
      options: chartOptsBar(),
    }));
  }

  // CAT-N-2
  const cv2 = document.querySelector('canvas[data-rep-chart="cat2"]');
  if (cv2) {
    const counts = {};
    subset.tecnicas.forEach(t => { if (t.cat2) counts[t.cat2] = (counts[t.cat2] || 0) + 1; });
    const sorted = Object.entries(counts).sort((a, b) => b[1] - a[1]);
    const labels = sorted.map(s => s[0]);
    const data = sorted.map(s => s[1]);
    _reporteChartsInstances.push(new Chart(cv2, {
      type: 'bar',
      data: { labels, datasets: [{ data, backgroundColor: palette, borderRadius: 4 }] },
      options: chartOptsBar(),
    }));
  }

  // Lenguas — desde records filtrados, contando técnicas únicas por lengua
  const cvL = document.querySelector('canvas[data-rep-chart="lenguas"]');
  if (cvL) {
    const tecsPorLengua = {};
    subset.records.forEach(r => {
      const l = (r['Lengua'] || '').trim();
      const t = (r['Tecnica'] || '').trim();
      if (!l || l.toLowerCase() === 'español' || l.toLowerCase() === 'na') return;
      if (!tecsPorLengua[l]) tecsPorLengua[l] = new Set();
      tecsPorLengua[l].add(t);
    });
    const sorted = Object.entries(tecsPorLengua)
      .map(([l, s]) => [l, s.size])
      .sort((a, b) => b[1] - a[1]);
    const labels = sorted.map(s => s[0]);
    const data = sorted.map(s => s[1]);
    _reporteChartsInstances.push(new Chart(cvL, {
      type: 'bar',
      data: { labels, datasets: [{ data, backgroundColor: '#035A79', borderRadius: 4 }] },
      options: chartOptsBar({ indexAxis: 'y' }),
    }));
  }

  // Aprendizaje — desde records filtrados
  const cvA = document.querySelector('canvas[data-rep-chart="aprende"]');
  if (cvA) {
    const cols = { Madre: 'Madre', Abuela: 'Abuela', Tía: 'Tia', Hermana: 'Hermana',
                   Cuñada: 'Cunada', Padre: 'Padre', 'Instructor/a': 'Instructora' };
    const acc = {}; Object.keys(cols).forEach(k => acc[k] = 0);
    subset.records.forEach(r => {
      Object.entries(cols).forEach(([etiqueta, col]) => {
        if (parseInt(r[col] || 0, 10) > 0) acc[etiqueta]++;
      });
    });
    const labels = Object.keys(acc);
    const data = labels.map(l => acc[l]);
    _reporteChartsInstances.push(new Chart(cvA, {
      type: 'bar',
      data: { labels, datasets: [{ data, backgroundColor: '#B50552', borderRadius: 4 }] },
      options: chartOptsBar(),
    }));
  }

  // Enseñanza — desde records filtrados
  const cvE = document.querySelector('canvas[data-rep-chart="ensena"]');
  if (cvE) {
    const cols = { Hijas: 'Hijas', Hijos: 'Hijos', Nietos: 'Nietos',
                   Sobrinos: 'Sobrinos', Pareja: 'Pareja', Estudiantes: 'Estudiantes' };
    const acc = {}; Object.keys(cols).forEach(k => acc[k] = 0);
    subset.records.forEach(r => {
      Object.entries(cols).forEach(([etiqueta, col]) => {
        if (parseInt(r[col] || 0, 10) > 0) acc[etiqueta]++;
      });
    });
    const labels = Object.keys(acc);
    const data = labels.map(l => acc[l]);
    _reporteChartsInstances.push(new Chart(cvE, {
      type: 'bar',
      data: { labels, datasets: [{ data, backgroundColor: '#05B794', borderRadius: 4 }] },
      options: chartOptsBar(),
    }));
  }
}

function chartOptsBar(extra = {}) {
  return {
    responsive: true, maintainAspectRatio: false,
    animation: { duration: 600 },
    plugins: {
      legend: { display: false },
      tooltip: { backgroundColor: '#1A1018', padding: 8, titleFont: { weight: 'bold' } },
    },
    scales: {
      x: { grid: { display: false }, ticks: { font: { size: 10, family: 'DM Sans' } } },
      y: { grid: { color: 'rgba(0,0,0,.05)' }, ticks: { font: { size: 10, family: 'DM Sans' } } },
    },
    ...extra,
  };
}

// ─────────────────────────────────────────────
// EXPORTACIÓN — PDF y Word
// ─────────────────────────────────────────────

function descargarReportePDF() {
  if (!_reporteLastSubset) {
    alert('Primero genera la vista previa.'); return;
  }
  const elemento = document.getElementById('reporte-preview-frame');
  if (!elemento) return;

  const titulo = (reporteState.titulo.trim() || 'reporte_atlas_textil').replace(/\s+/g, '_').replace(/[^\w-]/g, '');
  const filename = `${titulo}_${new Date().toISOString().slice(0, 10)}.pdf`;

  const btn = document.getElementById('reporte-download-pdf');
  const originalText = btn.textContent;
  btn.textContent = '⏳ Generando…';
  btn.disabled = true;

  const opt = {
    margin: [12, 12, 14, 12],
    filename,
    image: { type: 'jpeg', quality: 0.92 },
    html2canvas: { scale: 2, useCORS: true, logging: false, letterRendering: true },
    jsPDF: { unit: 'mm', format: 'a4', orientation: 'portrait' },
    pagebreak: { mode: ['avoid-all', 'css', 'legacy'], avoid: '.rep-card, .rep-testim, .rep-chart-wrap, table' },
  };

  html2pdf().set(opt).from(elemento.querySelector('.rep-doc')).save()
    .then(() => { btn.textContent = originalText; btn.disabled = false; })
    .catch(err => {
      console.error(err);
      alert('Error generando el PDF: ' + err.message);
      btn.textContent = originalText; btn.disabled = false;
    });
}

function descargarReporteWord() {
  if (!_reporteLastSubset) { alert('Primero genera la vista previa.'); return; }
  if (!window.docx) { alert('La librería de Word no está disponible.'); return; }

  const btn = document.getElementById('reporte-download-word');
  const originalText = btn.textContent;
  btn.textContent = '⏳ Generando…';
  btn.disabled = true;

  try {
    const subset = _reporteLastSubset;
    const titulo = reporteState.titulo.trim() || 'Reporte del Atlas Textil';
    const autor = reporteState.autor.trim();
    const fecha = new Date().toLocaleDateString('es-MX', { year: 'numeric', month: 'long', day: 'numeric' });

    const { Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType, Table, TableRow, TableCell, WidthType, BorderStyle } = docx;

    const children = [];

    // Portada
    children.push(new Paragraph({
      text: 'Atlas Nacional de Técnicas del Arte Textil',
      alignment: AlignmentType.CENTER,
      spacing: { before: 1200, after: 400 },
    }));
    children.push(new Paragraph({
      text: titulo,
      heading: HeadingLevel.TITLE,
      alignment: AlignmentType.CENTER,
      spacing: { after: 300 },
    }));
    children.push(new Paragraph({
      text: construirResumenFiltros(),
      alignment: AlignmentType.CENTER,
      spacing: { after: 600 },
    }));
    children.push(new Paragraph({
      children: [
        new TextRun({ text: `${subset.tecnicas.length} técnicas · `, bold: true }),
        new TextRun({ text: `${subset.estados.length} estados · `, bold: true }),
        new TextRun({ text: `${subset.totalRegistros} registros`, bold: true }),
      ],
      alignment: AlignmentType.CENTER,
      spacing: { after: 600 },
    }));
    if (autor) children.push(new Paragraph({ text: `Elaborado por: ${autor}`, alignment: AlignmentType.CENTER, spacing: { after: 100 } }));
    children.push(new Paragraph({ text: `Fecha: ${fecha}`, alignment: AlignmentType.CENTER, spacing: { after: 1200 } }));

    // Secciones
    REPORTE_SECCIONES.forEach((sec, idx) => {
      if (!reporteState.secciones.has(sec.id)) return;
      if (!sec.applies(subset)) return;

      children.push(new Paragraph({
        text: `${String(idx + 1).padStart(2, '0')} · ${sec.label}`,
        heading: HeadingLevel.HEADING_1,
        spacing: { before: 400, after: 200 },
      }));

      // Contenido específico por sección (versión texto plano)
      añadirSeccionWord(children, sec.id, subset, { Paragraph, TextRun, HeadingLevel, Table, TableRow, TableCell, WidthType, BorderStyle, AlignmentType });
    });

    // Pie
    children.push(new Paragraph({
      text: `Generado el ${fecha} a partir del Atlas Nacional de Técnicas del Arte Textil. Encuentro Nacional de Arte Textil "Original" · Secretaría de Cultura · UNESCO México.`,
      alignment: AlignmentType.CENTER,
      spacing: { before: 800 },
    }));

    const doc = new Document({
      sections: [{ children }],
      styles: {
        default: { document: { run: { font: 'Calibri', size: 22 } } },
      },
    });

    Packer.toBlob(doc).then(blob => {
      const filename = `${titulo.replace(/\s+/g, '_').replace(/[^\w-]/g, '')}_${new Date().toISOString().slice(0, 10)}.docx`;
      saveAs(blob, filename);
      btn.textContent = originalText; btn.disabled = false;
    });
  } catch (err) {
    console.error(err);
    alert('Error generando Word: ' + err.message);
    btn.textContent = originalText; btn.disabled = false;
  }
}

function añadirSeccionWord(children, id, subset, dx) {
  const { Paragraph, TextRun, Table, TableRow, TableCell, WidthType, BorderStyle, AlignmentType } = dx;

  const noBorder = { style: BorderStyle.SINGLE, size: 4, color: 'DDDDDD' };
  const tableBorders = { top: noBorder, bottom: noBorder, left: noBorder, right: noBorder, insideHorizontal: noBorder, insideVertical: noBorder };

  if (id === 'resumen') {
    const promFichas = (subset.totalRegistros / subset.tecnicas.length).toFixed(1);
    children.push(new Paragraph({
      text: `Este reporte abarca ${subset.tecnicas.length} técnicas textiles documentadas en ${subset.estados.length} estados de la República Mexicana, con un total de ${subset.totalRegistros} registros. El promedio es de ${promFichas} registros por técnica.`,
      spacing: { after: 200 },
    }));
  }

  if (id === 'geografico') {
    const cuentaRegPorEstado = {};
    const tecsPorEstado = {};
    subset.records.forEach(r => {
      const e = (r['Estado'] || '').trim();
      const t = (r['Tecnica'] || '').trim();
      if (!e) return;
      cuentaRegPorEstado[e] = (cuentaRegPorEstado[e] || 0) + 1;
      if (!tecsPorEstado[e]) tecsPorEstado[e] = new Set();
      tecsPorEstado[e].add(t);
    });
    const filas = Object.keys(cuentaRegPorEstado)
      .map(e => [e, tecsPorEstado[e].size, cuentaRegPorEstado[e]])
      .sort((a, b) => b[1] - a[1]);
    const rows = [
      new TableRow({ children: ['Estado', 'Técnicas', 'Registros'].map(t =>
        new TableCell({ children: [new Paragraph({ children: [new TextRun({ text: t, bold: true })] })] })
      ) }),
      ...filas.map(([est, n, regs]) => new TableRow({ children: [
        new TableCell({ children: [new Paragraph(est)] }),
        new TableCell({ children: [new Paragraph(String(n))] }),
        new TableCell({ children: [new Paragraph(String(regs))] }),
      ]})),
    ];
    children.push(new Table({ rows, width: { size: 100, type: WidthType.PERCENTAGE }, borders: tableBorders }));
    children.push(new Paragraph({ text: '', spacing: { after: 200 } }));
  }

  if (id === 'catalogo') {
    // Pre-agrupar records por técnica
    const recsPorTec = {};
    subset.records.forEach(r => {
      const k = (r['Tecnica'] || '').trim();
      if (!recsPorTec[k]) recsPorTec[k] = [];
      recsPorTec[k].push(r);
    });

    const ordenadas = [...subset.tecnicas].sort((a, b) => (a.cat1 || '').localeCompare(b.cat1 || '') || a.tecnica.localeCompare(b.tecnica, 'es'));
    let lastCat1 = null;
    ordenadas.forEach(t => {
      if (t.cat1 !== lastCat1) {
        lastCat1 = t.cat1;
        children.push(new Paragraph({
          children: [new TextRun({ text: (t.cat1 || 'Sin clasificar').toUpperCase(), bold: true, color: 'B50552' })],
          spacing: { before: 240, after: 80 },
        }));
      }
      const breadcrumb = [t.cat2, t.cat3, t.cat4].filter(Boolean).join(' › ');
      children.push(new Paragraph({
        children: [new TextRun({ text: t.tecnica, bold: true, size: 24 })],
        spacing: { before: 120, after: 40 },
      }));
      if (breadcrumb) children.push(new Paragraph({ children: [new TextRun({ text: breadcrumb, italics: true, color: '888888', size: 18 })] }));

      // Datos del subset filtrado (no globales)
      const recs = recsPorTec[t.tecnica] || [];
      const estSet = new Set(); recs.forEach(r => { const e = (r['Estado'] || '').trim(); if (e) estSet.add(e); });
      const lenSet = new Set(); recs.forEach(r => {
        const l = (r['Lengua'] || '').trim();
        if (l && l.toLowerCase() !== 'español' && l.toLowerCase() !== 'na') lenSet.add(l);
      });

      const meta = [];
      meta.push(`${recs.length} registro${recs.length !== 1 ? 's' : ''}`);
      if (estSet.size) meta.push([...estSet].slice(0, 4).join(', ') + (estSet.size > 4 ? '…' : ''));
      if (lenSet.size) meta.push(`Lenguas: ${[...lenSet].slice(0, 3).join(', ')}`);
      meta.forEach(m => children.push(new Paragraph({ children: [new TextRun({ text: m, size: 20 })], spacing: { after: 40 } })));
    });
  }

  if (id === 'categorias') {
    const cuenta = {};
    subset.tecnicas.forEach(t => { if (t.cat1) cuenta[t.cat1] = (cuenta[t.cat1] || 0) + 1; });
    Object.entries(cuenta).sort((a, b) => b[1] - a[1]).forEach(([cat, n]) => {
      children.push(new Paragraph({ text: `• ${cat}: ${n} técnica${n !== 1 ? 's' : ''}`, spacing: { after: 60 } }));
    });
  }

  if (id === 'lenguas') {
    const tecsPorLengua = {};
    subset.records.forEach(r => {
      const l = (r['Lengua'] || '').trim();
      const t = (r['Tecnica'] || '').trim();
      if (!l || l.toLowerCase() === 'español' || l.toLowerCase() === 'na') return;
      if (!tecsPorLengua[l]) tecsPorLengua[l] = new Set();
      tecsPorLengua[l].add(t);
    });
    Object.entries(tecsPorLengua)
      .map(([l, s]) => [l, s.size])
      .sort((a, b) => b[1] - a[1])
      .forEach(([l, n]) => {
        children.push(new Paragraph({ text: `• ${l}: ${n} técnica${n !== 1 ? 's' : ''}`, spacing: { after: 60 } }));
      });
  }

  if (id === 'transmision') {
    const colsA = { Madre: 'Madre', Abuela: 'Abuela', Tía: 'Tia', Hermana: 'Hermana',
                    Cuñada: 'Cunada', Padre: 'Padre', 'Instructor/a': 'Instructora' };
    const colsE = { Hijas: 'Hijas', Hijos: 'Hijos', Nietos: 'Nietos',
                    Sobrinos: 'Sobrinos', Pareja: 'Pareja', Estudiantes: 'Estudiantes' };
    const aprende = {}; Object.keys(colsA).forEach(k => aprende[k] = 0);
    const ensena  = {}; Object.keys(colsE).forEach(k => ensena[k]  = 0);
    subset.records.forEach(r => {
      Object.entries(colsA).forEach(([k, col]) => { if (parseInt(r[col] || 0, 10) > 0) aprende[k]++; });
      Object.entries(colsE).forEach(([k, col]) => { if (parseInt(r[col] || 0, 10) > 0) ensena[k]++; });
    });
    children.push(new Paragraph({ children: [new TextRun({ text: 'De quién aprendieron:', bold: true })], spacing: { before: 120, after: 60 } }));
    Object.entries(aprende).forEach(([k, n]) => children.push(new Paragraph({ text: `• ${k}: ${n}`, spacing: { after: 40 } })));
    children.push(new Paragraph({ children: [new TextRun({ text: 'A quién enseñan:', bold: true })], spacing: { before: 120, after: 60 } }));
    Object.entries(ensena).forEach(([k, n]) => children.push(new Paragraph({ text: `• ${k}: ${n}`, spacing: { after: 40 } })));
  }

  if (id === 'tenidos') {
    const cuentas = { Plantas: 0, Minerales: 0, 'Animales/Insectos': 0 };
    subset.records.forEach(r => {
      if (parseInt(r['Plantas'] || 0, 10) > 0)   cuentas.Plantas++;
      if (parseInt(r['Minerales'] || 0, 10) > 0) cuentas.Minerales++;
      if (parseInt(r['Animales'] || 0, 10) > 0)  cuentas['Animales/Insectos']++;
    });
    Object.entries(cuentas).forEach(([tipo, n]) => {
      children.push(new Paragraph({ text: `• ${tipo}: ${n} mencione${n !== 1 ? 's' : ''}`, spacing: { after: 60 } }));
    });
  }

  if (id === 'testimonios') {
    const ts = [];
    subset.tecnicas.forEach(t => {
      if (t.historia && t.historia.length > 30) {
        ts.push({ tecnica: t.tecnica, texto: t.historia, tipo: 'Historia' });
      }
      if (t.significados && t.significados.length) {
        t.significados.slice(0, 1).forEach(s => {
          if (s && s.length > 20) ts.push({ tecnica: t.tecnica, texto: s, tipo: 'Significado' });
        });
      }
    });
    ts.slice(0, 30).forEach(t => {
      const txt = t.texto.length > 280 ? t.texto.slice(0, 277) + '…' : t.texto;
      children.push(new Paragraph({
        children: [new TextRun({ text: '"' + txt + '"', italics: true })],
        spacing: { before: 100, after: 30 },
      }));
      children.push(new Paragraph({
        children: [new TextRun({ text: `— ${t.tecnica} · ${t.tipo}`, color: 'B50552', size: 18 })],
        spacing: { after: 80 },
      }));
    });
  }
}

// Hook de los botones del Paso 3 — se hace una sola vez en initReporteView
function bindReportePaso3() {
  document.getElementById('reporte-regenerate')?.addEventListener('click', generarReporte);
  document.getElementById('reporte-download-pdf')?.addEventListener('click', descargarReportePDF);
  document.getElementById('reporte-download-word')?.addEventListener('click', descargarReporteWord);
}

loadCSVs();
