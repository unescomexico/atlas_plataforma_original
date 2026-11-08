/* ================================================
   ATLAS COMUNITARIO DEL ARTE TEXTIL DE MÉXICO — MAIN JS
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

// Coropleta del enfoque Estado: aportaciones individuales por demarcación.
// No confundir estos conteos con el número de entradas/técnicas del catálogo.
function getMapColor(count) {
  if (count >= 50) return '#FCBC52FF';
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
let GALLERY_ITEMS = [];
let IMAGE_META_BY_FILE = {};
let BIBLIO_TECH_CARDS = new Map();
let BIBLIO_TAXONOMY_CARDS = new Map();
let CURRENT_FICHA_TECHNIQUE = null;
let CURRENT_FICHA_SOURCE = 'testimonio';

// ────────────────────────────────────────────────
// CSV PARSER (sin dependencias)
// ────────────────────────────────────────────────
function parseCSV(text) {
  const source = String(text || '').replace(/^\uFEFF/, '');
  const matrix = [];
  let row = [];
  let cur = '';
  let inQuotes = false;

  for (let i = 0; i < source.length; i++) {
    const ch = source[i];

    if (ch === '"') {
      if (inQuotes && source[i + 1] === '"') {
        cur += '"';
        i++;
      } else {
        inQuotes = !inQuotes;
      }
      continue;
    }

    if (ch === ',' && !inQuotes) {
      row.push(cur.trim());
      cur = '';
      continue;
    }

    if ((ch === '\n' || ch === '\r') && !inQuotes) {
      if (ch === '\r' && source[i + 1] === '\n') i++;
      row.push(cur.trim());
      cur = '';
      if (row.some(v => v !== '')) matrix.push(row);
      row = [];
      continue;
    }

    cur += ch;
  }

  if (cur.length || row.length) {
    row.push(cur.trim());
    if (row.some(v => v !== '')) matrix.push(row);
  }

  if (matrix.length < 2) return [];
  const headers = matrix[0].map(h => h.trim());
  return matrix.slice(1).map(vals => {
    const obj = {};
    headers.forEach((h, idx) => { obj[h] = vals[idx] !== undefined ? vals[idx] : ''; });
    return obj;
  });
}

function normalizeLookup(value) {
  return String(value || '')
    .normalize('NFD').replace(/[\u0300-\u036f]/g, '')
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, ' ')
    .trim();
}

function splitBibliography(value) {
  return String(value || '')
    .split(/\n+/)
    .map(s => s.trim())
    .filter(Boolean);
}

function buildBibliographyIndexes(techRows, taxonomyRows) {
  BIBLIO_TECH_CARDS = new Map();
  (techRows || []).forEach(row => {
    const name = (row.technique_name || '').trim();
    const ready = String(row.ready_for_platform || '').trim().toLowerCase();
    if (!name || !['true', '1', 'yes', 'si', 'sí'].includes(ready)) return;
    BIBLIO_TECH_CARDS.set(normalizeLookup(name), row);
  });

  BIBLIO_TAXONOMY_CARDS = new Map();
  (taxonomyRows || []).forEach(row => {
    const level = normalizeLookup(row.nivel);
    const name = normalizeLookup(row.nombre);
    if (!level || !name) return;
    BIBLIO_TAXONOMY_CARDS.set(`${level}|||${name}`, row);
  });
}

function getTechniqueBibliographyCard(tecnicaNombre) {
  const key = normalizeLookup(tecnicaNombre);
  if (BIBLIO_TECH_CARDS.has(key)) return BIBLIO_TECH_CARDS.get(key);

  // Equivalencias editoriales confirmadas entre el catálogo público y el archivo bibliográfico.
  const aliases = {
    'bordado de chaquira': 'bordados en chaquira',
  };
  const alias = aliases[key];
  return alias ? (BIBLIO_TECH_CARDS.get(alias) || null) : null;
}

function getTaxonomyBibliographyCard(level, name) {
  const levelKey = normalizeLookup(level);
  let nameKey = normalizeLookup(name);

  // El archivo bibliográfico denomina "Tejido artesanal" a la categoría
  // que la plataforma presenta de forma abreviada como "Tejido".
  if (levelKey === 'categoria' && nameKey === 'tejido') nameKey = 'tejido artesanal';

  return BIBLIO_TAXONOMY_CARDS.get(`${levelKey}|||${nameKey}`) || null;
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
  // La clave se normaliza para que diferencias de mayúsculas/acentos no dejen
  // una técnica sin fotografía (p. ej. "Sarape Fino..." vs "Sarape fino...").
  const imageKey = value => String(value || '')
    .normalize('NFD').replace(/[\u0300-\u036f]/g, '')
    .toLowerCase().replace(/[^a-z0-9]+/g, ' ').trim();
  const imgByTech = {};
  (imgRows || []).forEach(r => {
    const tecnica = (r['Tecnica'] || '').trim();
    const archivo = (r['archivo_descargado'] || '').trim();
    const status  = (r['status'] || '').toLowerCase().trim();
    if (!archivo || status === 'error' ||
        (window.ATLAS_AVAILABLE_IMAGES && !window.ATLAS_AVAILABLE_IMAGES.has(archivo))) return;
    if (tecnica) {
      const key = imageKey(tecnica);
      if (!imgByTech[key]) imgByTech[key] = [];
      imgByTech[key].push(archivo);
    }
  });

  // La galería contiene registros multimedia que no figuraban en indice_imagenes.csv.
  // Reconciliar por denominación normalizada y eliminar duplicados al preparar la ficha.
  GALLERY_ITEMS.forEach(item => {
    const archivo = String(item?.archivo || '').trim();
    const key = imageKey(item?.tecnica);
    if (!key || !archivo ||
        (window.ATLAS_AVAILABLE_IMAGES && !window.ATLAS_AVAILABLE_IMAGES.has(archivo))) return;
    const files = (imgByTech[key] ||= []);
    if (!files.includes(archivo)) files.push(archivo);
  });

  // Índice independiente de videos; sus notas internas nunca se muestran.
  const videoByTech = {};
  (window.ATLAS_VIDEO_ROWS || []).forEach(r => {
    if ((r.publicar || '').toLowerCase() !== 'si') return;
    const key = imageKey(r.Tecnica);
    if (!key || !r.archivo) return;
    (videoByTech[key] ||= []).push(r.archivo);
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

    // Imágenes. Primero se empareja por nombre normalizado y, como respaldo,
    // por el identificador del registro individual. Se deduplican manteniendo orden.
    const imagenes = [...new Set([
      ...(imgByTech[imageKey(nombre)] || [])
    ])];

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
      destacada_catalogo: (row['destacada_catalogo'] || '').trim().toLowerCase() === 'sí',
      videos: [...new Set(videoByTech[imageKey(nombre)] || [])],
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
  .filter(t => t.tecnica); // Todos los registros normalizadas; el catálogo decide cuáles destacar.

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
    const [techRes, recRes, imgRes, galleryRes, biblioTechRes, biblioTaxRes, videoRes, culturalRes, pueblosRes] = await Promise.all([
      fetch('data/data_by_technique_id.csv'),
      fetch('data/data_by_record_id.csv'),
      fetch('data/indice_imagenes.csv'),
      fetch('data/galeria.json'),
      fetch('data/fichas_bibliograficas_tecnicas.csv'),
      fetch('data/fichas_bibliograficas_taxonomia.csv'),
      fetch('data/indice_videos.csv'),
      fetch('data/catalogo_cultural_normalizado.csv'),
      fetch('data/correspondencia_lenguas_pueblos_inpi.csv'),
    ]);

    if (!techRes.ok || !recRes.ok || !culturalRes.ok) throw new Error('No se pudieron cargar los datos del catálogo.');

    const [techText, recText, imgText, galleryPayload, biblioTechText, biblioTaxText, videoText, culturalText, pueblosText] = await Promise.all([
      techRes.text(),
      recRes.text(),
      imgRes.ok ? imgRes.text() : Promise.resolve(''),
      galleryRes.ok ? galleryRes.json() : Promise.resolve({ images: [] }),
      biblioTechRes.ok ? biblioTechRes.text() : Promise.resolve(''),
      biblioTaxRes.ok ? biblioTaxRes.text() : Promise.resolve(''),
      videoRes.ok ? videoRes.text() : Promise.resolve(''),
      culturalRes.ok ? culturalRes.text() : Promise.resolve(''),
      pueblosRes.ok ? pueblosRes.text() : Promise.resolve(''),
    ]);

    const techRows   = parseCSV(techText);
    const recordRows = parseCSV(recText);
    const imgRows    = imgText ? parseCSV(imgText) : [];
    const biblioTechRows = biblioTechText ? parseCSV(biblioTechText) : [];
    const biblioTaxRows = biblioTaxText ? parseCSV(biblioTaxText) : [];
    buildBibliographyIndexes(biblioTechRows, biblioTaxRows);
    GALLERY_ITEMS = Array.isArray(galleryPayload?.images) ? galleryPayload.images : [];
    IMAGE_META_BY_FILE = {};
    GALLERY_ITEMS.forEach(item => { if (item?.archivo) IMAGE_META_BY_FILE[item.archivo] = item; });

    window.ATLAS_VIDEO_ROWS = videoText ? parseCSV(videoText) : [];
    // ATLAS_VIDEO_DETAILS_V3: metadatos procedentes de cada respuesta original Kobo
    const metadataResponse = await fetch('data/metadata_videos.csv');
    const videoMetadata = metadataResponse.ok ? parseCSV(await metadataResponse.text()) : [];
    const videoMetadataMap = new Map(videoMetadata.map(row => [row.archivo, row]));
    window.ATLAS_VIDEO_ROWS = window.ATLAS_VIDEO_ROWS.map(row => ({...row, ...(videoMetadataMap.get(row.archivo) || {})}));

    // Lista de archivos presentes en esta publicación: evita referencias rotas
    // y respeta fotografías retiradas por motivos de consentimiento.
    try {
      const mediaResponse = await fetch('data/imagenes_disponibles.json');
      window.ATLAS_AVAILABLE_IMAGES = mediaResponse.ok
        ? new Set(await mediaResponse.json()) : null;
    } catch (error) {
      window.ATLAS_AVAILABLE_IMAGES = null;
    }
    ATLAS = buildAtlasFromCSV(techRows, recordRows, imgRows);
    // Correspondencias documentales entre nombres de lenguas y pueblos de referencia INPI.
    // No se interpreta la lengua como una adscripción étnica de una persona.
    const puebloCorrespondencias = new Map();
    (pueblosText ? parseCSV(pueblosText) : []).forEach(row => {
      if (row.estatus !== 'referencia_inpi' || !row.pueblo_referencia_inpi) return;
      puebloCorrespondencias.set(catalogNorm(row.lengua_origen), row.pueblo_referencia_inpi.trim());
    });
    // Variantes de denominación presentes en las respuestas del Atlas.
    for (const [source, original] of Object.entries({
      'Chol': "Ch'ol", 'Purépecha': 'Tarasco', 'Pima (Nevome)': 'Pima'
    })) {
      const standard = puebloCorrespondencias.get(catalogNorm(original));
      if (standard) puebloCorrespondencias.set(catalogNorm(source), standard);
    }
    const puebloDesdeLengua = value => puebloCorrespondencias.get(catalogNorm(value)) || null;
    window.ATLAS_PUEBLO_DESDE_LENGUA = puebloDesdeLengua;
    const pueblosPorTecnica = new Map();
    ATLAS.records.forEach(row => {
      const pueblo = puebloDesdeLengua(row.Lengua);
      if (!pueblo || !row.Estado || !row.Tecnica) return;
      const key = normalizeLookup(row.Tecnica);
      const place = `${normalizeEstado(row.Estado)}|||${pueblo}`;
      if (!pueblosPorTecnica.has(key)) pueblosPorTecnica.set(key, new Map());
      pueblosPorTecnica.get(key).set(place, {estado: row.Estado.trim(), pueblo});
    });
    ATLAS.tecnicas.forEach(t => {
      const entries = [...(pueblosPorTecnica.get(normalizeLookup(t.tecnica))?.values() || [])];
      t.pueblos_por_estado = entries;
      t.pueblos_indigenas = [...new Set(entries.map(x => x.pueblo))].sort((a,b) => a.localeCompare(b,'es'));
    });
    // Vocabulario cultural curado: se conservan intactas las respuestas originales.
    const culturalByTechnique = new Map((culturalText ? parseCSV(culturalText) : [])
      .map(r => [normalizeLookup(r.tecnica_norm), r]));
    ATLAS.tecnicas.forEach(t => {
      const entry = culturalByTechnique.get(normalizeLookup(t.tecnica));
      t.prendas_normalizadas = entry ? entry.prendas_normalizadas.split(';').map(s => s.trim()).filter(Boolean) : [];
      t.ceremonias_normalizadas = entry ? entry.ceremonias_normalizadas.split(';').map(s => s.trim()).filter(Boolean) : [];
      t.materiales_normalizados = entry ? (entry.materiales_normalizados || '').split(';').map(s => s.trim()).filter(Boolean) : [];
    });

    // Territorialidad verificada: los puntos magenta son centroides MUNICIPALES.
    // Las localidades proceden de la galería y no se confunden con esos centroides.
    const territoriosPorTecnica = new Map();
    recordRows.forEach(r => {
      const key = normalizeLookup(r.Tecnica);
      const estado = String(r.Estado || '').trim();
      const municipio = String(r.Municipio || '').trim();
      if (!key || !estado || !municipio || /^(na|sin dato|no aplica)$/i.test(municipio)) return;
      if (!territoriosPorTecnica.has(key)) territoriosPorTecnica.set(key, new Map());
      const codigo = territoryKey(estado, municipio);
      territoriosPorTecnica.get(key).set(codigo, {estado, municipio, codigo, etiqueta: `${municipio} · ${estado}`});
    });
    const localidadesPorTecnica = new Map();
    GALLERY_ITEMS.forEach(item => {
      const key = normalizeLookup(item.tecnica);
      const localidad = String(item.localidad || '').trim();
      const estado = String(item.estado || '').trim();
      const municipio = String(item.municipio || '').trim();
      if (!key || !localidad || /^(na|n\/?a|sin dato|no aplica)$/i.test(localidad)) return;
      if (!localidadesPorTecnica.has(key)) localidadesPorTecnica.set(key, new Map());
      const etiqueta = [localidad, municipio, estado].filter(Boolean).join(" · ");
      const codigo = estado && municipio ? territoryKey(estado, municipio) : '';
      localidadesPorTecnica.get(key).set(catalogNorm(etiqueta), {localidad, estado, municipio, codigo, etiqueta});
    });
    ATLAS.tecnicas.forEach(t => {
      const key = normalizeLookup(t.tecnica);
      t.territorios = [...(territoriosPorTecnica.get(key)?.values() || [])]
        .sort((a,b) => a.etiqueta.localeCompare(b.etiqueta, 'es'));
      t.localidades = [...(localidadesPorTecnica.get(key)?.values() || [])]
        .sort((a,b) => a.etiqueta.localeCompare(b.etiqueta, 'es'));
      // Propiedad conservada para compatibilidad con código no migrado.
      t.comunidades = t.localidades.map(x => x.etiqueta);
    });
    tecnicasMap     = {};
    estadosTecnicas = ATLAS.estados;
    municipiosTecnicas = ATLAS.municipios || {};
    ATLAS.tecnicas.forEach(t => { tecnicasMap[t.tecnica] = t; });

    // Panorama del Atlas: se integra en la introducción inicial.
        // Los estados se cuentan de las aportaciones, no de las 32 geometrías del mapa.
    // Los participantes son nombres distintos informados; no equivalen a personas censadas.
    const normalizaIndicador = valor => String(valor ?? '')
      .normalize('NFD').replace(/[\u0300-\u036f]/g, '')
      .trim().toLocaleLowerCase('es-MX').replace(/\s+/g, ' ');
    const estadosRegistrados = new Set(recordRows.map(r => normalizaIndicador(r.Estado)).filter(Boolean));
    const participantesRegistrados = new Set(recordRows.map(r => normalizaIndicador(r.Artesano)).filter(Boolean));
    const nEstados = estadosRegistrados.size;
    // El CSV reducido de la interfaz no incluye la columna Artesano.
    // 503 procede de la base completa: 600 fichas con nombre, 503 nombres distintos normalizados.
    const nParticipantes = 503; // Cifra agregada: 503 nombres únicos normalizados entre 600 aportaciones con nombre (638 total).
    const nTecnicas = ATLAS.tecnicas.length;
    const nRegistros = ATLAS.tecnicas.reduce((s, t) => s + t.n_fichas, 0);
    updateIntroStats(Math.round(nRegistros), nEstados, nTecnicas, nParticipantes);

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
  return String(s)
    .replace(/\\/g, '\\\\')
    .replace(/'/g, "\\'")
    .replace(/&/g, '&amp;')
    .replace(/"/g, '&quot;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/\r/g, '\\r')
    .replace(/\n/g, '\\n');
}

function imgPath(filename) {
  if (!filename) return '';
  // Encode each filename for browser-safe URLs. Image filenames in the deployment
  // are ASCII-safe, but this also protects against spaces and reserved characters.
  return `imagenes/${encodeURIComponent(String(filename))}`;
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

// Aportaciones por estado: mismos registros del catálogo, y respeta filtros activos.
// Los puntos del mapa siguen siendo centroides municipales de procedencia.
function countMapRecordsEstado(name, filtered = false) {
  if (!ATLAS || !Array.isArray(ATLAS.records)) return 0;
  const key = normalizeEstado(name);
  const allowed = filtered && mapFilteredTecnicas instanceof Set ? mapFilteredTecnicas : null;
  const municipalities = filtered && typeof mapTerritoryFilter !== 'undefined' && mapTerritoryFilter instanceof Set
    ? mapTerritoryFilter : null;
  return ATLAS.records.reduce((total, row) => {
    if (normalizeEstado(row.Estado) !== key) return total;
    if (allowed && !allowed.has(String(row.Tecnica || '').trim())) return total;
    if (municipalities && !municipalities.has(territoryKey(row.Estado, row.Municipio))) return total;
    return total + 1;
  }, 0);
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
function territoryKey(estado, municipio) {
  return `${normalizeEstado(estado)}||${catalogNorm(municipio)}`;
}

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
    const count = countMapRecordsEstado(name, Boolean(mapFilteredTecnicas));
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
// MAPA — resumen inicial del panel derecho
// Se muestra únicamente al cargar la plataforma. Cualquier interacción
// cartográfica lo reemplaza y no vuelve a aparecer hasta recargar.
// ────────────────────────────────────────────────
let initialMapOverviewActive = false;

function updateIntroStats(registros, estados, tecnicas, participantes) {
  const values = { registros, estados, tecnicas, participantes };
  document.querySelectorAll('[data-intro-stat]').forEach(el => {
    const key = el.getAttribute('data-intro-stat');
    if (key && values[key] != null) {
      el.textContent = Number(values[key]).toLocaleString('es-MX');
    }
  });
}

function renderInitialMapOverview() {
  initialMapOverviewActive = false;
}

function dismissInitialMapOverview() {
  initialMapOverviewActive = false;
  document.querySelector('.side-panel')?.classList.remove('initial-overview');
}

// ────────────────────────────────────────────────
// TABS
// ────────────────────────────────────────────────
let currentTab = 'mapa';
let networkInitialized = false;
let catalogInitialized = false;
let arbolInitialized = false;
let galleryInitialized = false;
let reporteInitialized = false;
let alluvialInitialized = false;

document.querySelectorAll('.nav-tab').forEach(tab => {
  tab.addEventListener('click', () => {
    const id = tab.dataset.tab;
    if (!id || id === 'red') return; // Red conservada en código, no disponible públicamente.
    if (id === currentTab) return;
    if (id !== 'mapa' && typeof dismissInitialMapOverview === 'function') dismissInitialMapOverview();
    currentTab = id;
    document.querySelectorAll('.nav-tab').forEach(t => t.classList.remove('active'));
    tab.classList.add('active');
    document.querySelectorAll('.view').forEach(v => v.classList.remove('active'));
    document.getElementById(`view-${id}`).classList.add('active');

    if (id === 'mapa') {
      setTimeout(() => {
        if (map) map.invalidateSize({ animate: false, pan: false });
        if (typeof refreshMapViewport === 'function') refreshMapViewport();
      }, 80);
    } else if (id === 'catalogo' && !catalogInitialized) {
      catalogInitialized = true; renderCatalogFilters(); renderCatalog();
    } else if (id === 'red' && !networkInitialized) {
      networkInitialized = true; initNetwork();
    } else if (id === 'arbol' && !arbolInitialized) {
      arbolInitialized = true; initArbol();
    } else if (id === 'galeria' && !galleryInitialized) {
      galleryInitialized = true; initGallery();
    } else if (id === 'reporte' && !reporteInitialized) {
      reporteInitialized = true; initReporteView();
    } else if (id === 'aluvial') {
      if (!alluvialInitialized) {
        alluvialInitialized = true;
        if (typeof initAlluvial === 'function') initAlluvial();
      } else if (typeof refreshAlluvial === 'function') {
        refreshAlluvial();
      }
    }
    if (id === 'red' && !networkHelpSeen) {
      networkHelpSeen = true;
      openNetworkHelp();
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
let municipiosAllFeatureLayers = []; // referencia estable para filtros espaciales
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
  // Capas con orden explícito: contexto territorial < comunidades.
  map.createPane('atlasTerritoryPane');
  map.getPane('atlasTerritoryPane').style.zIndex = 430;
  map.createPane('atlasCommunitiesPane');
  map.getPane('atlasCommunitiesPane').style.zIndex = 520;

  L.control.zoom({ position: 'topright' }).addTo(map);
  // Sustituye el prefijo gráfico predeterminado de Leaflet por un enlace textual.
  map.attributionControl.setPrefix('<a href="https://leafletjs.com/" target="_blank" rel="noopener">Leaflet</a>');
  L.tileLayer('https://www.unesco.org/tiles/clearmap/{z}/{x}/{y}.png', {
    attribution: '© UNESCO',
    maxZoom: 19,
    minZoom: 5,
  }).addTo(map);
  map.fitBounds([[14.5, -118.4], [32.7, -86.7]]);
  // Leaflet necesita recalcularse después de que el layout responsive termine de asentarse.
  setTimeout(() => { if (typeof refreshMapViewport === 'function') refreshMapViewport({ refit: true }); }, 120);

  fetch('https://raw.githubusercontent.com/angelnmara/geojson/master/mexicoHigh.json')
    .then(r => r.json())
    .then(data => {
      geojsonLayer = L.geoJSON(data, {
        style: stateStyle,
        onEachFeature: (feat, layer) => {
          const name = feat.properties.name || feat.properties.NAME_1 || '';
          const count = countMapRecordsEstado(name);
          if (count > 0) {
            layer.bindTooltip(
              `<b>${esc(name)}</b><br>${count} registro${count !== 1 ? 's' : ''}`,
              { className: 'leaflet-tooltip-atlas', direction: 'top', sticky: true }
            );
          }
          layer.on({
            mouseover: e => {
              // Los estados solo son interactivos visualmente en el enfoque Estado.
              // En Lengua/Ecosistema funcionan como contexto neutro y no deben
              // conservar resaltados al mover el cursor.
              const activeDriver = document.querySelector('.map-driver-btn.active')?.dataset.mapDriver || 'estado';
              if (activeDriver !== 'estado') return;
              if (!(typeof isStateSelected === 'function' ? isStateSelected(name) : name === selectedStateName)) {
                if (mapFilteredTecnicas) {
                  const base = filteredStateStyle(name);
                  e.target.setStyle({ ...base, weight: 2.5, color: COLORS.negro });
                } else {
                  e.target.setStyle(highlightStyle(feat));
                }
              }
            },
            mouseout: e => {
              const activeDriver = document.querySelector('.map-driver-btn.active')?.dataset.mapDriver || 'estado';
              if (activeDriver !== 'estado') {
                e.target.setStyle({
                  fillColor: '#F3EEE9', weight: 0.7, opacity: 0.72,
                  color: '#C7BFB8', fillOpacity: 0.08,
                });
                return;
              }
              if (!(typeof isStateSelected === 'function' ? isStateSelected(name) : name === selectedStateName)) {
                if (mapFilteredTecnicas) {
                  e.target.setStyle(filteredStateStyle(name));
                } else {
                  geojsonLayer.resetStyle(e.target);
                }
              }
            },
            click: () => {
              const activeDriver = document.querySelector('.map-driver-btn.active')?.dataset.mapDriver || 'estado';
              if (activeDriver === 'estado') onStateClick(name, layer, feat);
            },
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
  const count = countMapRecordsEstado(name);
  return { fillColor: getMapColor(count), weight: 1.2, opacity: 1, color: '#fff', fillOpacity: count > 0 ? 0.78 : 0.18 };
}

function highlightStyle(feature) {
  const name  = feature.properties.name || feature.properties.NAME_1 || '';
  const count = countMapRecordsEstado(name);
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
  if (typeof dismissInitialMapOverview === 'function') dismissInitialMapOverview();
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
  const res = await fetch('geodata/mexico_municipios_centroids.geojson');
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
      pointToLayer: (feat, latlng) => L.circleMarker(latlng, { ...COMUNIDAD_STYLE, pane: 'atlasCommunitiesPane' }),
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

  municipiosAllFeatureLayers = municipiosLayer.getLayers();
  municipiosLoaded = true;
  return municipiosLayer;
}

// Filtra la capa principal de comunidades sin destruir sus objetos Leaflet.
// predicate(feature, layer) => true conserva la comunidad visible.
function setCommunityFilter(predicate = null) {
  if (!municipiosLayer || !municipiosAllFeatureLayers.length) return;
  municipiosAllFeatureLayers.forEach(layer => {
    const visible = !predicate || Boolean(predicate(layer.feature, layer));
    const present = municipiosLayer.hasLayer(layer);
    if (visible && !present) municipiosLayer.addLayer(layer);
    if (!visible && present) municipiosLayer.removeLayer(layer);
  });
  municipiosLayer.eachLayer(layer => layer.setStyle(COMUNIDAD_STYLE));
  selectedMunicipioKey = null;
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
    const count = countMapRecordsEstado(name, Boolean(mapFilteredTecnicas));
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
    body.innerHTML = `<div class="welcome-state"><h3>Sin registros</h3><p>No hay técnicas que coincidan con la selección actual.</p></div>`;
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
  Object.keys(cat1Map).sort((a,b) => (catalogNorm(a) === 'otras' ? 1 : 0) - (catalogNorm(b) === 'otras' ? 1 : 0) || a.localeCompare(b,'es')).forEach(c1 => {
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
// MAP FILTER — filtros multiselección por categoría y técnica
// Regla: OR dentro de un mismo nivel; AND entre niveles diferentes.
// ────────────────────────────────────────────────
let mapFilterState = {
  cat1: new Set(), cat2: new Set(), cat3: new Set(), cat4: new Set(), tecnica: new Set()
};
let mapFilteredTecnicas = null; // Set de nombres de técnica activas, null = todas

function mapSet(level) {
  const value = mapFilterState[level];
  if (value instanceof Set) return value;
  const set = new Set(value ? [value] : []);
  mapFilterState[level] = set;
  return set;
}

function checkboxSummary(set, emptyLabel = 'Todas') {
  if (!set || !set.size) return emptyLabel;
  if (set.size === 1) {
    const value = [...set][0];
    return value.length > 28 ? `${value.slice(0, 26)}…` : value;
  }
  return `${set.size} seleccionadas`;
}

function setFilterSummary(id, set, emptyLabel = 'Todas') {
  const el = document.getElementById(id);
  if (el) el.textContent = checkboxSummary(set, emptyLabel);
}

function valuesAvailableAtLevel(level) {
  const levels = ['cat1','cat2','cat3','cat4'];
  const idx = levels.indexOf(level);
  if (idx < 0) return [];
  const rows = ATLAS.tecnicas.filter(t => {
    for (let i = 0; i < idx; i++) {
      const selected = mapSet(levels[i]);
      if (selected.size && !selected.has(t[levels[i]])) return false;
    }
    return true;
  });
  return [...new Set(rows.map(t => t[level]).filter(Boolean))].sort((a,b)=>a.localeCompare(b,'es'));
}

function filterRowColor(level, value) {
  if (level === 'cat1') return CAT1_COLOR[value] || COLORS.arena;
  const row = ATLAS.tecnicas.find(t => t[level] === value);
  if (level === 'cat2') return CAT2_COLOR[value] || CAT1_COLOR[row?.cat1] || COLORS.arena;
  return CAT2_COLOR[row?.cat2] || CAT1_COLOR[row?.cat1] || COLORS.arena;
}

function appendSelectAllFilterRow(wrap, values, selected, onToggle, inputId) {
  if (!wrap || !values.length) return;
  const allSelected = values.every(value => selected.has(value));
  const someSelected = values.some(value => selected.has(value));

  const row = document.createElement('div');
  row.className = 'filter-checkbox-item filter-select-all';
  row.innerHTML = `<input type="checkbox" id="${inputId}">
    <span class="filter-select-all-mark" aria-hidden="true">✓</span>
    <label for="${inputId}" class="filter-checkbox-name">Aplicar todas</label>`;

  const input = row.querySelector('input');
  input.checked = allSelected;
  input.indeterminate = someSelected && !allSelected;
  input.addEventListener('change', () => onToggle(input.checked));
  wrap.appendChild(row);
}

function renderClassificationLevel(level, containerId, summaryId, emptyLabel) {
  const wrap = document.getElementById(containerId);
  if (!wrap) return;
  const selected = mapSet(level);
  const values = valuesAvailableAtLevel(level);
  wrap.innerHTML = '';
  appendSelectAllFilterRow(
    wrap,
    values,
    selected,
    checked => {
      selected.clear();
      if (checked) values.forEach(value => selected.add(value));
      pruneMapFilterChildren(level);
      rebuildMapFilterCascade();
      applyMapFilter();
    },
    `map-${level}-all`
  );
  values.forEach((value, i) => {
    const row = document.createElement('div');
    row.className = 'filter-checkbox-item';
    const inputId = `map-${level}-${i}`;
    const checked = selected.has(value);
    row.innerHTML = `<input type="checkbox" id="${inputId}" ${checked ? 'checked' : ''}>
      <span class="map-chip-swatch" style="background:${filterRowColor(level, value)}"></span>
      <label for="${inputId}" class="filter-checkbox-name">${esc(value)}</label>`;
    row.querySelector('input').addEventListener('change', () => onMapChipClick(level, value));
    wrap.appendChild(row);
  });
  setFilterSummary(summaryId, selected, emptyLabel);
}

function renderTechniqueFilterList() {
  const wrap = document.getElementById('map-chips-tecs');
  if (!wrap) return;
  const selected = mapSet('tecnica');
  const q = (document.getElementById('map-filter-search')?.value || '').toLowerCase().trim();
  const base = getFilteredTecnicas({ ignoreTechnique: true });
  const rows = q ? base.filter(t => t.tecnica.toLowerCase().includes(q)) : base;
  wrap.innerHTML = '';
  const allTechniqueNames = base.map(t => t.tecnica);
  appendSelectAllFilterRow(
    wrap,
    allTechniqueNames,
    selected,
    checked => {
      selected.clear();
      if (checked) allTechniqueNames.forEach(name => selected.add(name));
      rebuildMapFilterCascade();
      applyMapFilter();
    },
    'map-tecnica-all'
  );
  rows.forEach((t, i) => {
    const row = document.createElement('div');
    row.className = 'filter-checkbox-item filter-technique-item';
    const inputId = `map-tecnica-${i}`;
    row.innerHTML = `<input type="checkbox" id="${inputId}" ${selected.has(t.tecnica) ? 'checked' : ''}>
      <span class="map-chip-swatch" style="background:${getCatColor(t)}"></span>
      <label for="${inputId}" class="filter-checkbox-name">${esc(t.tecnica)}</label>
      <button type="button" class="filter-row-action">Ver ficha</button>`;
    row.querySelector('input').addEventListener('change', () => selectTecnicaOnMap(t.tecnica));
    row.querySelector('.filter-row-action').addEventListener('click', e => {
      e.preventDefault(); e.stopPropagation(); openFicha(t.tecnica);
    });
    wrap.appendChild(row);
  });
  const summary = document.getElementById('map-tecs-summary');
  if (summary) summary.textContent = selected.size
    ? checkboxSummary(selected, 'Todas')
    : `${base.length} disponibles`;
}

function initMapFilter() {
  if (!ATLAS) return;
  const searchEl = document.getElementById('map-filter-search');
  if (searchEl && !searchEl.dataset.bound) {
    searchEl.dataset.bound = '1';
    searchEl.addEventListener('input', renderTechniqueFilterList);
  }
  rebuildMapFilterCascade();
}

function onMapChipClick(level, val) {
  dismissInitialMapOverview();
  const set = mapSet(level);
  if (set.has(val)) set.delete(val); else set.add(val);
  pruneMapFilterChildren(level);
  rebuildMapFilterCascade();
  applyMapFilter();
}

function selectTecnicaOnMap(nombre) {
  dismissInitialMapOverview();
  const set = mapSet('tecnica');
  if (set.has(nombre)) set.delete(nombre); else set.add(nombre);
  rebuildMapFilterCascade();
  applyMapFilter();
}

function clearMapFilter() {
  dismissInitialMapOverview();
  mapFilterState = { cat1: new Set(), cat2: new Set(), cat3: new Set(), cat4: new Set(), tecnica: new Set() };
  const search = document.getElementById('map-filter-search');
  if (search) search.value = '';
  rebuildMapFilterCascade();
  applyMapFilter();
}

function techniquesMatchingThrough(level) {
  const levels = ['cat1','cat2','cat3','cat4'];
  const idx = levels.indexOf(level);
  return ATLAS.tecnicas.filter(t => {
    for (let i=0; i<=idx; i++) {
      const key = levels[i];
      const set = mapSet(key);
      if (set.size && !set.has(t[key])) return false;
    }
    return true;
  });
}

function pruneMapFilterChildren(changedLevel) {
  const levels = ['cat1','cat2','cat3','cat4'];
  const idx = levels.indexOf(changedLevel);
  if (idx < 0) return;
  for (let i = idx + 1; i < levels.length; i++) {
    const key = levels[i];
    const allowed = new Set(valuesAvailableAtLevel(key));
    [...mapSet(key)].forEach(v => { if (!allowed.has(v)) mapSet(key).delete(v); });
  }
  const allowedTechs = new Set(getFilteredTecnicas({ ignoreTechnique: true }).map(t => t.tecnica));
  [...mapSet('tecnica')].forEach(v => { if (!allowedTechs.has(v)) mapSet('tecnica').delete(v); });
}

function rebuildMapFilterCascade() {
  renderClassificationLevel('cat1', 'map-chips-cat1', 'map-cat1-summary', 'Todas');
  renderClassificationLevel('cat2', 'map-chips-cat2', 'map-cat2-summary', 'Todas');
  renderClassificationLevel('cat3', 'map-chips-cat3', 'map-cat3-summary', 'Todos');
  renderClassificationLevel('cat4', 'map-chips-cat4', 'map-cat4-summary', 'Todas');
  renderTechniqueFilterList();
}

function getFilteredTecnicas(options={}) {
  const ignoreTechnique=Boolean(options.ignoreTechnique);
  return ATLAS.tecnicas.filter(t=>{
    for (const key of ['cat1','cat2','cat3','cat4']) {
      const set=mapSet(key);
      if (set.size && !set.has(t[key])) return false;
    }
    const techSet=mapSet('tecnica');
    if (!ignoreTechnique && techSet.size && !techSet.has(t.tecnica)) return false;
    return true;
  });
}

function mapFilterHasSelection() {
  return ['cat1','cat2','cat3','cat4','tecnica'].some(k=>mapSet(k).size>0);
}

function mapFilterSummaryParts() {
  const labels={cat1:'Categoría',cat2:'Subcategoría',cat3:'Tipo',cat4:'Variante',tecnica:'Técnicas'};
  return ['cat1','cat2','cat3','cat4','tecnica'].flatMap(key=>{
    const set=mapSet(key); if(!set.size) return [];
    const vals=[...set];
    const shown=vals.slice(0,3).join(', ')+(vals.length>3?` +${vals.length-3}`:'');
    return [`${labels[key]}: ${shown}`];
  });
}

function applyMapFilter() {
  dismissInitialMapOverview();
  const filtered=getFilteredTecnicas();
  const hasFilter=mapFilterHasSelection();
  mapFilteredTecnicas=hasFilter?new Set(filtered.map(t=>t.tecnica)):null;

  if (geojsonLayer) {
    geojsonLayer.eachLayer(l=>{
      const name=l.feature?.properties?.name || l.feature?.properties?.NAME_1 || '';
      l.setStyle(mapFilteredTecnicas?filteredStateStyle(name):stateStyle(l.feature));
      l.unbindTooltip();
      const count=countMapRecordsEstado(name, Boolean(mapFilteredTecnicas));
      if(count>0) l.bindTooltip(`<b>${esc(name)}</b><br>${count} registro${count!==1?'s':''}`,{className:'leaflet-tooltip-atlas',direction:'top',sticky:true});
    });
  }
  if(municipiosLayer) municipiosLayer.eachLayer(l=>l.setStyle(COMUNIDAD_STYLE));
  selectedMunicipioKey=null;

  if (typeof updateStateSelectionStyles === 'function') updateStateSelectionStyles();
  if (typeof renderStatesList === 'function') renderStatesList();
  if (typeof refreshCommunities === 'function') refreshCommunities();
  if (typeof updateStatePanel === 'function') updateStatePanel();
  else {
    document.getElementById('panel-title').textContent='Estados de la República';
    document.getElementById('panel-subtitle').textContent=hasFilter?`Mostrando ${filtered.length} técnicas filtradas`:'Selecciona uno o varios estados';
    document.getElementById('panel-body').innerHTML=`<div class="welcome-state"><h3>${hasFilter?'Filtro activo':'Explora el mapa'}</h3><p>${hasFilter?'Selecciona uno o varios estados para combinar el filtro territorial con la clasificación activa.':'Puedes seleccionar más de un estado en el mapa o en la lista.'}</p></div>`;
  }
}

function territoryNamesForState(name) {
  if (!(mapTerritoryFilter instanceof Set)) return null;
  const names = new Set();
  Object.entries(municipiosTecnicas).forEach(([key, techs]) => {
    const [estado, municipio] = key.split('||');
    if (normalizeEstado(estado) !== normalizeEstado(name)) return;
    if (!mapTerritoryFilter.has(territoryKey(estado, municipio))) return;
    techs.forEach(t => { if (!mapFilteredTecnicas || mapFilteredTecnicas.has(t)) names.add(t); });
  });
  return names;
}

function countForEstadoFiltered(name) {
  const allowed = territoryNamesForState(name);
  if (allowed) return allowed.size;
  const tecs=getTecnicasForEstado(name);
  return tecs.filter(t=>mapFilteredTecnicas && mapFilteredTecnicas.has(t)).length;
}

function filteredStateStyle(name) {
  const count=countMapRecordsEstado(name, true);
  return {fillColor:getMapColor(count),weight:1.2,opacity:1,color:'#fff',fillOpacity:count>0?0.88:0.08};
}

// ────────────────────────────────────────────────
// CATALOG VIEW
// ────────────────────────────────────────────────
// Catálogo: OR dentro de cada dimensión y AND entre dimensiones.
// Conservar la selección al navegar entre fichas, catálogo y mapa.
const CATALOG_GROUPS = [
  {title:'Clasificación', fields:[['cat1','Categoría'],['cat2','Subcategoría'],['cat3','Tipo'],['cat4','Variante']]},
  {title:'Ubicación', fields:[['estados','Estados'],['municipios','Municipios'],['comunidades','Comunidades']]},
  {title:'Otras características asociadas', fields:[['pueblos','Pueblos indígenas'],['ceremonias','Ceremonias y festividades'],['prendas','Prendas y objetos'],['materiales','Materiales']]}
];
const CATALOG_FILTERS = CATALOG_GROUPS.flatMap(group => group.fields);
let activeSearch = '';
let catalogShowAll = false;
const catalogSelected = Object.fromEntries(CATALOG_FILTERS.map(([k]) => [k, new Set()]));

function catalogNorm(v) {
  return String(v || '').normalize('NFD').replace(/[\u0300-\u036f]/g, '')
    .toLowerCase().replace(/\s+/g, ' ').trim();
}

function catalogValues(t, field) {
  if (['cat1', 'cat2', 'cat3', 'cat4'].includes(field)) return t[field] ? [t[field]] : [];
  if (field === 'prendas') return t.prendas_normalizadas || [];
  if (field === 'materiales') return t.materiales_normalizados || [];
  if (field === 'ceremonias') return t.ceremonias_normalizadas || [];
  if (field === 'comunidades') return (t.localidades || []).map(x => x.etiqueta);
  if (field === 'municipios') return (t.territorios || []).map(x => x.municipio);
  if (field === 'pueblos') return t.pueblos_indigenas || [];
  return t[field] || [];
}
function matchesCatalog(t) {
  if (activeSearch && !catalogNorm(t.tecnica).includes(catalogNorm(activeSearch))) return false;
  const estados = catalogSelected.estados;
  const comunidades = catalogSelected.comunidades;
  if (comunidades.size && estados.size && !(t.localidades || []).some(loc =>
    comunidades.has(loc.etiqueta) && [...estados].some(e => normalizeEstado(e) === normalizeEstado(loc.estado)))) return false;
  const municipios = catalogSelected.municipios;
  if (municipios.size && estados.size && !(t.territorios || []).some(loc =>
    municipios.has(loc.municipio) && [...estados].some(e => normalizeEstado(e) === normalizeEstado(loc.estado)))) return false;
  const pueblos = catalogSelected.pueblos;
  if (pueblos.size && estados.size && !(t.pueblos_por_estado || []).some(loc =>
    pueblos.has(loc.pueblo) && [...estados].some(e => normalizeEstado(e) === normalizeEstado(loc.estado)))) return false;
  return CATALOG_FILTERS.every(([field]) => {
    const set = catalogSelected[field];
    if (!set.size) return true;
    return catalogValues(t, field).some(value => set.has(value));
  });
}
function getCatalogResults() {
  if (!ATLAS) return [];
  const hayDestacadas = ATLAS.tecnicas.some(t => t.destacada_catalogo);
  return ATLAS.tecnicas.filter(t =>
    matchesCatalog(t) && (catalogShowAll || activeSearch || CATALOG_FILTERS.some(([f]) => catalogSelected[f].size) || (hayDestacadas
      ? t.destacada_catalogo
      : (t.n_fichas >= 2 && ((t.imagenes || []).length || (t.videos || []).length))))
  ).sort((a,b) => a.tecnica.localeCompare(b.tecnica,'es',{sensitivity:'base'}));
}
function availableCatalogOptions(field) {
  const estados = catalogSelected.estados;
  let techs = ATLAS.tecnicas;
  if (field === 'municipios' && estados.size) {
    return [...new Set(techs.flatMap(t => (t.territorios || [])
      .filter(loc => [...estados].some(e => normalizeEstado(e) === normalizeEstado(loc.estado)))
      .map(loc => loc.municipio)))].filter(Boolean).sort((a,b) => a.localeCompare(b,'es',{sensitivity:'base'}));
  }
  if (field === 'pueblos' && estados.size) {
    return [...new Set(techs.flatMap(t => (t.pueblos_por_estado || [])
      .filter(loc => [...estados].some(e => normalizeEstado(e) === normalizeEstado(loc.estado)))
      .map(loc => loc.pueblo)))].filter(Boolean).sort((a,b) => a.localeCompare(b,'es',{sensitivity:'base'}));
  }
  if (field === 'comunidades' && estados.size) {
    techs = techs.filter(t => t.estados.some(s => [...estados].some(e => normalizeEstado(e) === normalizeEstado(s))));
    return [...new Set(techs.flatMap(t => (t.localidades || [])
      .filter(loc => [...estados].some(e => normalizeEstado(e) === normalizeEstado(loc.estado)))
      .map(loc => loc.etiqueta)))].filter(Boolean).sort((a,b) => a.localeCompare(b,'es',{sensitivity:'base'}));
  }
  return [...new Set(techs.flatMap(t => catalogValues(t, field)))]
    .filter(Boolean).sort((a,b) => (catalogNorm(a) === 'otras' ? 1 : 0) - (catalogNorm(b) === 'otras' ? 1 : 0) || a.localeCompare(b,'es',{sensitivity:'base'}));
}
function renderCatalogFilters() {
  if (!ATLAS) return;
  const wrap = document.getElementById('catalog-filters');
  if (!wrap) return;
  const previousOpen = wrap.querySelector('.catalog-filter-dropdown[open]')?.dataset.filter;
  const mobileCollapsed = wrap.dataset.initialized ? wrap.classList.contains('catalog-filters-collapsed') : true;
  wrap.dataset.initialized = 'true';
  wrap.innerHTML = `<button type="button" class="catalog-mobile-toggle" aria-expanded="${!mobileCollapsed}" aria-controls="catalog-filter-groups"><span aria-hidden="true">☰</span> Filtros del catálogo <span class="catalog-mobile-toggle-count"></span></button><div id="catalog-filter-groups" class="catalog-filter-groups">` + CATALOG_GROUPS.map(group => `<section class="catalog-filter-group">
    <h3>${esc(group.title)}</h3>
    <div class="catalog-filter-group-grid">${group.fields.map(([field, label]) => {
      const values = availableCatalogOptions(field);
      const checkboxes = values.map((value, i) => `<label class="catalog-filter-option">
        <input type="checkbox" data-field="${field}" data-value="${esc(value)}" ${catalogSelected[field].has(value) ? 'checked' : ''}>
        <span>${esc(value)}</span>
      </label>`).join('');
      return `<details class="catalog-filter-dropdown" data-filter="${field}" ${previousOpen === field ? 'open' : ''}>
        <summary><span class="catalog-filter-label">${esc(label)}</span><span class="catalog-filter-count" data-count="${field}"></span></summary>
        <div class="catalog-filter-options">
          <div class="catalog-filter-actions"><button type="button" class="catalog-filter-none">Limpiar selección</button></div>
          ${values.length > 8 ? `<input type="search" class="catalog-filter-search" placeholder="Buscar…" aria-label="Buscar ${esc(label)}">` : ''}
          <div class="catalog-filter-scroll">${checkboxes || '<span class="catalog-filter-empty">Sin opciones disponibles</span>'}</div>
        </div>
      </details>`;
    }).join('')}</div>
  </section>`).join('') + '</div>';
  wrap.classList.toggle('catalog-filters-collapsed', mobileCollapsed);
  wrap.querySelector('.catalog-mobile-toggle')?.addEventListener('click', () => {
    const collapsed = wrap.classList.toggle('catalog-filters-collapsed');
    wrap.querySelector('.catalog-mobile-toggle').setAttribute('aria-expanded', String(!collapsed));
  });
  wrap.querySelectorAll('.catalog-filter-dropdown').forEach(details => {
    details.addEventListener('toggle', () => {
      if (!details.open) return;
      wrap.querySelectorAll('.catalog-filter-dropdown[open]').forEach(other => {
        if (other !== details) other.open = false;
      });
    });
    const search = details.querySelector('.catalog-filter-search');
    if (search) search.addEventListener('input', () => {
      const term = catalogNorm(search.value);
      details.querySelectorAll('.catalog-filter-option').forEach(row => {
        row.hidden = !catalogNorm(row.textContent).includes(term);
      });
    });
    details.querySelectorAll('input[type="checkbox"]').forEach(box => box.addEventListener('change', () => {
      const set = catalogSelected[box.dataset.field];
      if (box.checked) set.add(box.dataset.value);
      else set.delete(box.dataset.value);
      onCatalogSelectionChanged(box.dataset.field);
    }));
    details.querySelector('.catalog-filter-none')?.addEventListener('click', () => {
      const field = details.dataset.filter;
      catalogSelected[field].clear();
      onCatalogSelectionChanged(field);
    });
  });
  updateCatalogFilterCounts();
}
function onCatalogSelectionChanged(field) {
  if (field === 'estados') {
    for (const linkedField of ['comunidades', 'municipios', 'pueblos']) {
      const options = new Set(availableCatalogOptions(linkedField));
      [...catalogSelected[linkedField]].forEach(v => { if (!options.has(v)) catalogSelected[linkedField].delete(v); });
    }
    renderCatalogFilters();
    renderCatalog();
    return;
  }
  // Mantener abiertos los menús al marcar opciones, sin perder el texto de búsqueda.
  if (field === 'estados' || field === 'comunidades') {
    const values = availableCatalogOptions('comunidades');
    const communityDetails = document.querySelector('.catalog-filter-dropdown[data-filter="comunidades"]');
    if (communityDetails && field === 'estados') {
      const opened = communityDetails.open;
      communityDetails.querySelector('.catalog-filter-scroll').innerHTML = values.map(v => `<label class="catalog-filter-option">
        <input type="checkbox" data-field="comunidades" data-value="${esc(v)}" ${catalogSelected.comunidades.has(v) ? 'checked' : ''}>
        <span>${esc(v)}</span></label>`).join('') || '<span class="catalog-filter-empty">Sin opciones disponibles</span>';
      communityDetails.querySelectorAll('input[type="checkbox"]').forEach(box => box.addEventListener('change', () => {
        if (box.checked) catalogSelected.comunidades.add(box.dataset.value);
        else catalogSelected.comunidades.delete(box.dataset.value);
        updateCatalogFilterCounts(); renderCatalog();
      }));
      communityDetails.open = opened;
    }
  }
  updateCatalogFilterCounts();
  renderCatalog();
}
function updateCatalogFilterCounts() {
  const mobileTotal = CATALOG_FILTERS.reduce((sum, [field]) => sum + catalogSelected[field].size, 0);
  const mobileCounter = document.querySelector('.catalog-mobile-toggle-count');
  if (mobileCounter) mobileCounter.textContent = mobileTotal ? `${mobileTotal} seleccionados` : '';
  CATALOG_FILTERS.forEach(([field]) => {
    const counter = document.querySelector(`[data-count="${field}"]`);
    if (counter) counter.textContent = catalogSelected[field].size ? String(catalogSelected[field].size) : '';
  });
}
// Cerrar al pulsar fuera o Escape, como un menú de selección convencional.
document.addEventListener('click', event => {
  if (!event.target.closest('#catalog-filters')) document.querySelectorAll('#catalog-filters details[open]').forEach(x => x.open = false);
});
document.addEventListener('keydown', event => {
  if (event.key === 'Escape') document.querySelectorAll('#catalog-filters details[open]').forEach(x => x.open = false);
});
function resetCatalogFilters() {
  CATALOG_FILTERS.forEach(([field]) => catalogSelected[field].clear());
  activeSearch = '';
  const input = document.getElementById('catalog-search');
  if (input) input.value = '';
  renderCatalogFilters();
  renderCatalog();
}
// Enlaces de las fichas: sustituir filtros anteriores por el elemento elegido.
function goToCatalogFilter(field, value, state = '') {
  if (!catalogSelected[field]) return;
  closeFicha();
  CATALOG_FILTERS.forEach(([key]) => catalogSelected[key].clear());
  catalogSelected[field].add(value);
  if (state && field !== 'estados') catalogSelected.estados.add(state);
  activeSearch = '';
  const search = document.getElementById('catalog-search');
  if (search) search.value = '';
  // Una ficha puede no ser 'destacada': no ocultarla al seguir el enlace.
  catalogShowAll = true;
  const showAll = document.getElementById('catalog-show-all');
  if (showAll) showAll.checked = true;
  document.querySelector('[data-tab="catalogo"]').click();
  renderCatalogFilters();
  renderCatalog();
  document.getElementById('view-catalogo')?.scrollIntoView({block:'start'});
}
function goToCatalog(cat1) { goToCatalogFilter('cat1', cat1); }

let mapTerritoryFilter = null; // centroids municipales válidos para la selección del catálogo

async function showCatalogSelectionOnMap() {
  const selected = getCatalogResults();
  if (!selected.length) return;
  const territoryKeys = new Set();
  const filteredLocalities = catalogSelected.comunidades;
  const filteredEstados = catalogSelected.estados;
  selected.forEach(t => {
    let keys = new Set((t.territorios || [])
      .filter(x => !filteredEstados.size || filteredEstados.has(x.estado))
      .map(x => x.codigo));
    if (filteredLocalities.size) {
      const localKeys = new Set((t.localidades || [])
        .filter(x => filteredLocalities.has(x.etiqueta) && x.codigo && (!filteredEstados.size || [...filteredEstados].some(e => normalizeEstado(e) === normalizeEstado(x.estado))))
        .map(x => x.codigo));
      keys = new Set([...keys].filter(x => localKeys.has(x)));
    }
    keys.forEach(x => territoryKeys.add(x));
  });
  mapFilterState = {
    cat1:new Set(), cat2:new Set(), cat3:new Set(), cat4:new Set(),
    tecnica:new Set(selected.map(t => t.tecnica))
  };
  mapTerritoryFilter = territoryKeys;
  if (typeof selectedStateNames !== 'undefined') selectedStateNames.clear();
  if (typeof selectedMaterials !== 'undefined') selectedMaterials.estado.clear();
  document.querySelector('[data-tab="mapa"]')?.click();
  if (typeof setMapDriver === 'function') await setMapDriver('estado');
  rebuildMapFilterCascade();
  applyMapFilter();
  // La capa de puntos se activa explícitamente al mostrar la selección.
  const cb = document.getElementById('comunidades-toggle');
  if (cb) cb.checked = true;
  await toggleComunidadesLayer(true);
  if (typeof refreshCommunities === 'function') refreshCommunities();
}

function renderCatalog() {
  if (!ATLAS) return;
  const tecnicas = getCatalogResults();
  const resultLabel = document.getElementById('catalog-results');
  if (resultLabel) resultLabel.textContent = `${tecnicas.length} entrada${tecnicas.length !== 1 ? 's' : ''} del catálogo`;
  const button = document.getElementById('catalog-to-map');
  if (button) button.disabled = tecnicas.length === 0;
  document.getElementById('tecnicas-grid').innerHTML = tecnicas.map(t => {
    const img = t.imagenes && t.imagenes.length ? t.imagenes[0] : null;
    const color = getCatColor(t);
    const catLabel = [t.cat1, t.cat2].filter(Boolean).join(' › ');
    const estados = (t.estados || []).slice(0, 3).join(', ');
    return `<div class="tec-card" onclick="openFicha('${escJs(t.tecnica)}')">
      <div class="tec-card-img" style="background:${COLORS.negro}">
        ${img ? `<img src="${esc(imgPath(img))}" alt="${esc(t.tecnica)}" loading="lazy" onerror="this.hidden=true;this.nextElementSibling.hidden=false"><span class="tec-card-no-img" hidden aria-hidden="true">&#129525;</span>` : `<span class="tec-card-no-img">&#129525;</span>`}
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
document.getElementById('catalog-show-all')?.addEventListener('change', function() {
  catalogShowAll = this.checked; renderCatalog();
});
document.getElementById('catalog-search')?.addEventListener('input', function() {
  activeSearch = this.value.toLowerCase().trim(); renderCatalog();
});
document.getElementById('catalog-clear')?.addEventListener('click', resetCatalogFilters);
document.getElementById('catalog-to-map')?.addEventListener('click', showCatalogSelectionOnMap);
// Leyenda plegable, independiente de los controles y filtros del mapa.
function syncMapLegendReopen() {
  // La cabecera de la leyenda siempre queda a la vista, también en móvil.
  // Se conserva el botón legado oculto para no duplicar controles.
  const reopen = document.getElementById('map-legend-reopen');
  if (reopen) reopen.hidden = true;
}
// La simbología compacta reproduce tres colores de la leyenda activa.
// Se actualiza al cambiar entre Estado, Lengua y Ecosistema.
function refreshMapLegendPreview() {
  const body = document.getElementById('map-legend-body');
  const preview = document.getElementById('map-legend-preview');
  if (!body || !preview) return;
  const colors = [...body.querySelectorAll('.legend-dot, .atlas-legend-swatch, .legend-circle')]
    .map(el => getComputedStyle(el).backgroundColor)
    .filter(color => color && color !== 'rgba(0, 0, 0, 0)' && color !== 'transparent');
  const samples = [...new Set(colors)].slice(0, 3);
  preview.replaceChildren(...samples.map(color => {
    const swatch = document.createElement('i');
    swatch.style.backgroundColor = color;
    return swatch;
  }));
  preview.hidden = !samples.length;
}
const atlasLegendBody = document.getElementById('map-legend-body');
if (atlasLegendBody) {
  const legendPreviewObserver = new MutationObserver(refreshMapLegendPreview);
  legendPreviewObserver.observe(atlasLegendBody, { childList:true, subtree:true });
  refreshMapLegendPreview();
}
document.getElementById('map-legend-reopen')?.addEventListener('click', () => {
  document.getElementById('map-legend-toggle')?.click();
});
document.getElementById('map-legend-toggle')?.addEventListener('click', function() {
  const legend = document.getElementById('map-legend');
  const collapsed = legend.classList.toggle('is-collapsed');
  this.textContent = collapsed ? '+' : '−';
  this.setAttribute('aria-expanded', String(!collapsed));
  this.setAttribute('aria-label', collapsed ? 'Expandir leyenda' : 'Contraer leyenda');
  this.title = collapsed ? 'Expandir leyenda' : 'Contraer leyenda';
  syncMapLegendReopen();
});

// Colapsar/expandir el panel derecho sin desmontar el mapa ni perder filtros.
document.getElementById('territory-panel-toggle')?.addEventListener('click', function() {
  const wrap = document.getElementById('view-mapa');
  const collapsed = wrap.classList.toggle('territory-panel-collapsed');
  this.textContent = collapsed ? '<' : '>';
  this.setAttribute('aria-expanded', String(!collapsed));
  this.setAttribute('aria-label', collapsed ? 'Mostrar panel de información' : 'Ocultar panel de información');
  window.setTimeout(() => { if (map) map.invalidateSize({animate:false,pan:false}); }, 220);
});


// Ayuda de lectura: aparece al entrar por primera vez en la Red y se reabre con «i».
let networkHelpSeen = false;
function openNetworkHelp() {
  const backdrop = document.getElementById('network-help-backdrop');
  if (!backdrop) return;
  backdrop.classList.add('open');
  backdrop.setAttribute('aria-hidden', 'false');
  backdrop.querySelector('.network-help-dialog')?.focus();
}
function closeNetworkHelp() {
  const backdrop = document.getElementById('network-help-backdrop');
  if (!backdrop) return;
  backdrop.classList.remove('open');
  backdrop.setAttribute('aria-hidden', 'true');
  document.getElementById('network-help-button')?.focus();
}
document.getElementById('network-help-button')?.addEventListener('click', openNetworkHelp);
document.getElementById('network-help-close')?.addEventListener('click', closeNetworkHelp);
document.getElementById('network-help-understood')?.addEventListener('click', closeNetworkHelp);
document.getElementById('network-help-backdrop')?.addEventListener('click', e => {
  if (e.target === e.currentTarget) closeNetworkHelp();
});
document.addEventListener('keydown', e => {
  if (e.key === 'Escape' && document.getElementById('network-help-backdrop')?.classList.contains('open')) closeNetworkHelp();
});

// ────────────────────────────────────────────────
// GALERÍA FOTOGRÁFICA
// Clasificación curatorial desde Notas/Clasificación.
// ────────────────────────────────────────────────
let activeGalleryCategory = 'Todas';
let activeGalleryTechnique = '';
let activeGalleryState = '';
let galleryFilteredItems = [];

function gallerySafe(value, fallback = 'No registrado') {
  const v = String(value || '').trim();
  return v && v.toUpperCase() !== 'NA' ? v : fallback;
}

function galleryPlace(item) {
  return [item.localidad, item.municipio, item.estado]
    .map(v => String(v || '').trim())
    .filter(v => v && v.toUpperCase() !== 'NA')
    .join(' · ');
}

function populateGallerySelects() {
  const all = atlasGalleryEntries();
  const techniques = [...new Set(all.map(item => String(item.tecnica || '').trim()).filter(Boolean))]
    .sort((a, b) => a.localeCompare(b, 'es', { sensitivity: 'base' }));
  const states = [...new Set(all.map(item => String(item.estado || '').trim()).filter(Boolean))]
    .sort((a, b) => a.localeCompare(b, 'es', { sensitivity: 'base' }));

  const techniqueSelect = document.getElementById('gallery-technique-filter');
  const stateSelect = document.getElementById('gallery-state-filter');
  if (techniqueSelect) {
    techniqueSelect.innerHTML = '<option value="">Todos los registros</option>' + techniques
      .map(value => `<option value="${esc(value)}">${esc(value)}</option>`).join('');
    techniqueSelect.value = activeGalleryTechnique;
  }
  if (stateSelect) {
    stateSelect.innerHTML = '<option value="">Todos los estados</option>' + states
      .map(value => `<option value="${esc(value)}">${esc(value)}</option>`).join('');
    stateSelect.value = activeGalleryState;
  }
}

function initGallery() {
  populateGallerySelects();
  document.querySelectorAll('.gallery-filter').forEach(btn => {
    btn.addEventListener('click', () => {
      activeGalleryCategory = btn.dataset.galleryCategory || 'Todas';
      document.querySelectorAll('.gallery-filter').forEach(b => b.classList.toggle('active', b === btn));
      renderGallery();
    });
  });
  document.getElementById('gallery-technique-filter')?.addEventListener('change', e => {
    activeGalleryTechnique = e.target.value || '';
    renderGallery();
  });
  document.getElementById('gallery-state-filter')?.addEventListener('change', e => {
    activeGalleryState = e.target.value || '';
    renderGallery();
  });
  document.getElementById('gallery-reset-filters')?.addEventListener('click', () => {
    activeGalleryCategory = 'Todas';
    activeGalleryTechnique = '';
    activeGalleryState = '';
    document.querySelectorAll('.gallery-filter').forEach(btn => btn.classList.toggle('active', btn.dataset.galleryCategory === 'Todas'));
    const techniqueSelect = document.getElementById('gallery-technique-filter');
    const stateSelect = document.getElementById('gallery-state-filter');
    if (techniqueSelect) techniqueSelect.value = '';
    if (stateSelect) stateSelect.value = '';
    renderGallery();
  });
  renderGallery();
}

// ATLAS_VIDEO_GALLERY_V2: galería pública para imágenes y videos indexados
function atlasGalleryEntries() {
  const photos = GALLERY_ITEMS.filter(item => item && item.archivo)
    .map(item => ({...item, tipo: 'imagen'}));
  const videos = (window.ATLAS_VIDEO_ROWS || [])
    .filter(row => row.archivo && String(row.publicar || '').toLowerCase() === 'si')
    .map(row => ({tipo: 'video', archivo: row.archivo, tecnica: row.tecnica || row.Tecnica || '',
       categoria: 'Videos', artesano: row.artesano || '', estado: row.estado || '', municipio: row.municipio || '', localidad: row.localidad || ''}));
  return [...photos, ...videos];
}
function atlasGalleryMedia(item) {
  return {tipo: item.tipo === 'video' ? 'video' : 'imagen', archivo: item.archivo,
    tecnica: item.tecnica || '', categoria: item.categoria || '', artesano:item.artesano || '',
    estado:item.estado || '', municipio:item.municipio || '', localidad:item.localidad || ''};
}
function atlasVideoUrl(filename) {
  return 'videos/' + String(filename).split('/').map(encodeURIComponent).join('/');
}

function renderGallery() {
  const grid = document.getElementById('gallery-grid');
  const count = document.getElementById('gallery-count');
  if (!grid) return;
  galleryFilteredItems = atlasGalleryEntries().filter(item => {
    if (activeGalleryCategory !== 'Todas' && item.categoria !== activeGalleryCategory) return false;
    if (activeGalleryTechnique && String(item.tecnica || '').trim() !== activeGalleryTechnique) return false;
    if (activeGalleryState && String(item.estado || '').trim() !== activeGalleryState) return false;
    return true;
  }).sort((a,b) => String(a.tecnica || '').localeCompare(String(b.tecnica || ''), 'es', {sensitivity:'base'}));
  if (count) {
    const nV=galleryFilteredItems.filter(item=>item.tipo==='video').length;
    count.textContent = `${galleryFilteredItems.length} archivos multimedia${nV ? ` · ${nV} videos` : ''}`;
  }
  if (!galleryFilteredItems.length) {
    grid.innerHTML='<div class="gallery-empty"><h3>Sin archivos</h3><p>No hay material multimedia para esta selección.</p></div>';
    return;
  }
  grid.innerHTML = galleryFilteredItems.map((item,index) => {
    const isVideo = item.tipo==='video';
    const place=galleryPlace(item);
    const artisan=gallerySafe(item.artesano,'');
    const visual=isVideo
      ? `<div class="atlas-gallery-video-cover"><span aria-hidden="true">▶</span><small>Video</small></div>`
      : `<img src="${esc(imgPath(item.archivo))}" alt="${esc(item.tecnica || 'Técnica textil')}" loading="lazy">`;
    return `<button class="gallery-card" type="button" data-gallery-index="${index}" aria-label="Abrir ${isVideo?'video':'fotografía'} de ${esc(item.tecnica || 'técnica textil')}">
      <div class="gallery-card-image">${visual}
        ${!isVideo && item.categoria ? `<span class="gallery-category-badge">${esc(item.categoria)}</span>` : ''}
      </div>
      <div class="gallery-card-copy"><strong>${esc(item.tecnica || 'Registro sin denominación')}</strong>
        ${!isVideo && artisan ? `<span>${esc(artisan)}</span>` : ''}
        ${place ? `<small>${esc(place)}</small>` : ''}
      </div></button>`;
  }).join('');
  grid.querySelectorAll('.gallery-card').forEach(card => card.addEventListener('click', () => {
    const index=Number(card.dataset.galleryIndex)||0;
    openLightbox('', galleryFilteredItems.map(atlasGalleryMedia), index);
  }));
}

// ────────────────────────────────────────────────
// FICHA MODAL
// ────────────────────────────────────────────────
function openFicha(tecnicaNombre) {
  const t = tecnicasMap[tecnicaNombre];
  if (!t) return;

  CURRENT_FICHA_TECHNIQUE = tecnicaNombre;
  CURRENT_FICHA_SOURCE = 'testimonio';

  // ── HERO — Carrusel ──
  const hero = document.getElementById('modal-hero');
  const slidesMedia = [
    ...(t.videos || []).map(fn => ({tipo: 'video', archivo: fn})),
    ...(t.imagenes || []).map(fn => ({tipo: 'imagen', archivo: fn}))
  ];
  if (slidesMedia.length > 0) {
    const slides = slidesMedia.map((m, i) =>
      `<div class="carousel-slide" data-index="${i}">
        ${m.tipo === 'imagen'
          ? `<img class="carousel-img" src="${esc(imgPath(m.archivo))}" alt="${esc(t.tecnica)} ${i+1}" loading="${i === 0 ? 'eager' : 'lazy'}">`
          : `<video class="carousel-video" controls preload="metadata" playsinline aria-label="Video de ${esc(t.tecnica)}"><source src="${esc(atlasVideoUrl(m.archivo))}" type="video/mp4">Tu navegador no admite video MP4.</video><button class="atlas-expand-video" type="button" data-video-index="${i}">Ampliar video</button>`}
      </div>`
    ).join('');

    const dots = slidesMedia.length > 1
      ? `<div class="carousel-dots">${slidesMedia.map((_, i) =>
          `<button class="carousel-dot${i === 0 ? ' active' : ''}" data-i="${i}" onclick="carouselGoTo(${i})"></button>`
        ).join('')}</div>` : '';
    const arrows = slidesMedia.length > 1
      ? `<button class="carousel-btn carousel-prev" onclick="carouselPrev()">&#8249;</button>
         <button class="carousel-btn carousel-next" onclick="carouselNext()">&#8250;</button>` : '';
    hero.innerHTML = `
      <div class="carousel-track" id="carousel-track">${slides}</div>
      ${arrows}
      ${dots}
      ${t.videos?.length ? `<span class="atlas-video-label">▶ ${t.videos.length} video${t.videos.length === 1 ? '' : 's'} disponible${t.videos.length === 1 ? '' : 's'}</span>` : ''}
      <button class="modal-close" onclick="closeFicha()">✕</button>
      <div class="modal-badge">${t.n_fichas} registro${t.n_fichas !== 1 ? 's' : ''}</div>
      <div class="carousel-counter" id="carousel-counter">1 / ${slidesMedia.length}</div>`;

    hero.querySelectorAll('.carousel-img').forEach((img) => {
      img.addEventListener('click', () => {
        const files = (t.imagenes || []).slice();
        const selected = Math.max(0, Number(img.closest('.carousel-slide')?.dataset.index || 0) - (t.videos || []).length);
        openLightbox(imgPath(files[selected]), files, selected);
      });
    });
    hero.querySelectorAll('.atlas-expand-video').forEach(btn => btn.addEventListener('click', () => {
      hero.querySelectorAll('video').forEach(v => v.pause());
      openLightbox('', slidesMedia.map(m => {
        const info = (window.ATLAS_VIDEO_ROWS || []).find(v => v.archivo === m.archivo) || {};
        return {tipo:m.tipo, archivo:m.archivo, tecnica:t.tecnica, artesano:info.artesano || '',
                estado:info.estado || '', municipio:info.municipio || '', localidad:info.localidad || ''};
      }), Number(btn.dataset.videoIndex));
    }));
    hero._images = t.imagenes || [];
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

  renderFichaBody(t, 'testimonio');
  document.getElementById('modal-overlay').classList.add('open');
  document.body.style.overflow = 'hidden';
}

function switchFichaSource(source) {
  if (!CURRENT_FICHA_TECHNIQUE) return;
  const t = tecnicasMap[CURRENT_FICHA_TECHNIQUE];
  if (!t) return;
  if (source === 'bibliografia' && !getTechniqueBibliographyCard(t.tecnica)) return;
  CURRENT_FICHA_SOURCE = source === 'bibliografia' ? 'bibliografia' : 'testimonio';
  renderFichaBody(t, CURRENT_FICHA_SOURCE);
}

function fichaHeaderHTML(t, source, hasBibliography) {
  let html = `<div class="modal-titulo">${esc(t.tecnica)}</div>`;

  if (t.cat1) {
    const color1 = CAT1_COLOR[t.cat1] || COLORS.arena;
    let breadcrumb = esc(t.cat1);
    if (t.cat2) breadcrumb += ` <span style="opacity:.6">›</span> ${esc(t.cat2)}`;
    if (t.cat3) breadcrumb += ` <span style="opacity:.6">›</span> ${esc(t.cat3)}`;
    if (t.cat4) breadcrumb += ` <span style="opacity:.6">›</span> ${esc(t.cat4)}`;
    html += `<div class="modal-grupo" style="background:${color1}">${breadcrumb}</div>`;
  }

  html += `<div class="ficha-source-switch" role="group" aria-label="Fuente de la ficha">
    <button type="button" class="ficha-source-btn${source === 'testimonio' ? ' active' : ''}" onclick="switchFichaSource('testimonio')" title="Ficha basada en testimonios" aria-label="Mostrar ficha basada en testimonios">
      <img src="assets/testimonio.png" alt="" aria-hidden="true">
      <span class="sr-only">Testimonios</span>
    </button>
    ${hasBibliography ? `<button type="button" class="ficha-source-btn${source === 'bibliografia' ? ' active' : ''}" onclick="switchFichaSource('bibliografia')" title="Ficha bibliográfica" aria-label="Mostrar ficha bibliográfica">
      <img src="assets/bibliografia.png" alt="" aria-hidden="true">
      <span class="sr-only">Bibliografía</span>
    </button>` : ''}
  </div>`;

  if (source === 'bibliografia') {
    html += `<div class="ficha-source-note biblio-note"><strong>Esta ficha está basada en la revisión documental y bibliográfica de diferentes fuentes. Las referencias utilizadas se presentan al final.</strong></div>`;
  } else {
    html += `<div class="ficha-source-note"><strong>Esta ficha está basada en testimonios e información proporcionada por artesanas y artesanos participantes en ORIGINAL, sistematizados para el Atlas.</strong></div>`;
  }

  return html;
}

function renderBibliographyContent(card, materialsLabel = 'Materiales y procesos') {
  if (!card) return '';
  let html = '';

  if (card.descripcion) {
    html += `<div class="section biblio-section">
      <div class="section-head"><span class="section-title">Descripción</span></div>
      <p class="ficha-parrafo">${esc(card.descripcion)}</p>
    </div>`;
  }

  if (card.historia_antecedentes) {
    html += `<div class="section biblio-section">
      <div class="section-head"><span class="section-title">Historia y antecedentes</span></div>
      <p class="ficha-parrafo">${esc(card.historia_antecedentes)}</p>
    </div>`;
  }

  const materials = card.materiales_procesos || card.materiales || '';
  if (materials) {
    html += `<div class="section biblio-section">
      <div class="section-head"><span class="section-title">${esc(materialsLabel)}</span></div>
      <p class="ficha-parrafo">${esc(materials)}</p>
    </div>`;
  }

  const refs = splitBibliography(card.bibliografia);
  if (refs.length) {
    html += `<div class="section biblio-section">
      <div class="section-head"><span class="section-title">Bibliografía</span></div>
      <ol class="biblio-reference-list">
        ${refs.map(ref => `<li>${esc(ref)}</li>`).join('')}
      </ol>
    </div>`;
  }

  return html;
}

function renderFichaBody(t, source = 'testimonio') {
  const biblioCard = getTechniqueBibliographyCard(t.tecnica);
  let html = fichaHeaderHTML(t, source, Boolean(biblioCard));

  if (source === 'bibliografia' && biblioCard) {
    html += renderBibliographyContent(biblioCard, 'Materiales');
    html += `<div class="ficha-return-row">
      <button type="button" class="ficha-return-btn" onclick="switchFichaSource('testimonio')">
        <img src="assets/testimonio.png" alt="" aria-hidden="true"> Regresar a la ficha basada en testimonios
      </button>
    </div>`;
    document.getElementById('modal-body').innerHTML = html;
    return;
  }

  // Temporalidad badge
  if (t.temporalidad) {
    const tempLabel = t.temporalidad === 'antigua' ? 'Técnica ancestral' : 'Técnica contemporánea';
    const tempColor = t.temporalidad === 'antigua' ? 'var(--azul-mar)' : 'var(--verde)';
    html += `<span class="modal-temporalidad" style="background:${tempColor}">${tempLabel}</span>`;
  }

  if (t.estados && t.estados.length > 0) {
    html += `<div class="estados-row">
      ${t.estados.map(e => `<button type="button" class="estado-tag catalog-link-tag" onclick="goToCatalogFilter('estados', '${escJs(e)}')">${esc(e)}</button>`).join('')}
    </div>`;
  }

  html += `<div class="stats-row">
    <div class="stat-box"><div class="val">${t.n_fichas}</div><div class="key">Registros</div></div>
    <div class="stat-box"><div class="val">${t.n_mujeres}</div><div class="key">Mujeres</div></div>
    <div class="stat-box"><div class="val">${t.n_hombres}</div><div class="key">Hombres</div></div>
    ${t.edad_promedio ? `<div class="stat-box"><div class="val">${Math.round(t.edad_promedio)}</div><div class="key">Edad prom.</div></div>` : ''}
  </div>`;

  if (t.pueblos_indigenas && t.pueblos_indigenas.length > 0) {
    html += `<div style="margin-bottom:18px">
      <div style="font-size:.68rem;font-weight:700;text-transform:uppercase;letter-spacing:1.2px;color:var(--arena);margin-bottom:7px">Pueblos indígenas de referencia</div>
      <div class="tags-row">${t.pueblos_indigenas.map(p => `<button type="button" class="lengua-tag catalog-link-tag" onclick="goToCatalogFilter('pueblos', '${escJs(p)}')">${esc(p)}</button>`).join('')}</div>
    </div>`;
  }

  if (t.historia) {
    html += `<div class="section">
      <div class="section-head"><span class="section-title">Historia y Origen</span></div>
      <p class="ficha-parrafo">${esc(t.historia)}</p>
    </div>`;
  }

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

  if (t.materiales_normalizados?.length) {
    const mats = t.materiales_normalizados || [];
    const ten  = t.tenido;
    const tenTotal = Object.values(ten).reduce((a, b) => a + b, 0);
    html += `<div class="section">
      <div class="section-head"><span class="section-title">Materiales</span></div>
      <div class="tag-cloud">${mats.map(m => `<button type="button" class="mat-tag catalog-link-tag" onclick="goToCatalogFilter('materiales', '${escJs(m)}')">${esc(m)}</button>`).join('')}</div>
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

  if (t.prendas_normalizadas?.length || t.ceremonias_normalizadas?.length) {
    const prendas = t.prendas_normalizadas || [];
    const ceremonias = t.ceremonias_normalizadas || [];
    html += `<div class="section">
      ${prendas.length ? `<div class="section-head"><span class="section-title">Prendas y objetos</span></div>
        <div class="tag-cloud">${prendas.map(value => `<button type="button" class="prenda-tag catalog-link-tag" onclick="goToCatalogFilter('prendas', '${escJs(value)}')">${esc(value)}</button>`).join('')}</div>` : ''}
      ${ceremonias.length ? `<div class="section-head" style="margin-top:16px"><span class="section-title">Ceremonias y festividades</span></div>
        <div class="tag-cloud">${ceremonias.map(value => `<button type="button" class="prenda-tag catalog-link-tag" onclick="goToCatalogFilter('ceremonias', '${escJs(value)}')">${esc(value)}</button>`).join('')}</div>` : ''}
    </div>`;
  }

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

  if (t.localidades && t.localidades.length) {
    html += `<div style="margin-bottom:16px">
      <div class="ficha-list-heading">Comunidades y localidades documentadas</div>
      <div class="tag-cloud">${t.localidades.map(loc => `<button type="button" class="mat-tag catalog-link-tag" onclick="goToCatalogFilter('comunidades', '${escJs(loc.etiqueta)}')">${esc(loc.etiqueta)}</button>`).join('')}</div>
    </div>`;
  }

  html += `<div style="margin-top:24px;padding-top:18px;border-top:1px solid var(--border)">
    <button hidden aria-hidden="true" tabindex="-1" onclick="goToNetwork('${escJs(t.tecnica)}')" style="background:var(--negro);color:var(--crema);border:none;padding:10px 20px;border-radius:4px;font-family:var(--font-body);font-size:.8rem;font-weight:600;cursor:pointer;letter-spacing:.3px;">
      Ver en la red de registros
    </button>
    <button onclick="goToCatalog('${escJs(t.cat1)}')" style="background:transparent;color:var(--arena);border:1px solid var(--border);padding:10px 20px;border-radius:4px;font-family:var(--font-body);font-size:.8rem;font-weight:600;cursor:pointer;letter-spacing:.3px;margin-left:8px;">
      Ver técnicas de esta categoría
    </button>
    <button onclick="closeFicha();openKobo('cambio')" style="background:transparent;color:var(--azul-mar);border:1px solid var(--azul-mar);padding:10px 20px;border-radius:4px;font-family:var(--font-body);font-size:.8rem;font-weight:600;cursor:pointer;letter-spacing:.3px;margin-left:8px;">
      ✎ ¿Deseas proponer algún cambio a esta técnica?
    </button>
  </div>`;

  document.getElementById('modal-body').innerHTML = html;
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
  return; // Red de registros desactivada para visitantes; código retenido.
  closeFicha();
  document.querySelector('[data-tab="red"]').click();
  setTimeout(() => highlightNetworkNode(tecnicaNombre), 600);
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
  hero.querySelectorAll('video').forEach(video => { video.pause(); });
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
let _lbImages = [];
let _lbCurrent = 0;
function atlasLbItem(entry) {
  if (typeof entry==='string') return {tipo:'imagen', archivo:entry};
  return entry || {tipo:'imagen', archivo:''};
}
function atlasLbPhotoUrl(filename) {
  const s=String(filename||'');
  return /^(?:https?:)?\/\//i.test(s) || /^(?:\/|imagenes\/|data:image\/)/i.test(s) ? s : imgPath(s);
}
function atlasCloseLightbox() {
  const video=document.getElementById('lightbox-video');
  if (video) { video.pause(); video.removeAttribute('src'); video.load(); }
  document.getElementById('lightbox').classList.remove('open');
}
function openLightbox(src, images, index) {
  _lbImages=(images && images.length ? images : [src]).map(atlasLbItem);
  _lbCurrent=Math.min(Math.max(Number(index)||0,0),_lbImages.length-1);
  document.getElementById('lightbox').classList.add('open');
  _renderLightbox();
}
function _renderLightbox() {
  const item=atlasLbItem(_lbImages[_lbCurrent]);
  const isVideo=item.tipo==='video';
  const image=document.getElementById('lightbox-img');
  const video=document.getElementById('lightbox-video');
  if (video) { video.pause(); video.removeAttribute('src'); video.load(); video.hidden=!isVideo; }
  if (image) {
    image.hidden=isVideo;
    if (!isVideo) {
      image.src=atlasLbPhotoUrl(item.archivo);
      image.alt=item.tecnica ? `Fotografía de ${item.tecnica}` : 'Fotografía del Atlas';
    } else image.removeAttribute('src');
  }
  if (isVideo && video) {
    video.src=atlasVideoUrl(item.archivo);
    video.load(); // la reproducción requiere una acción del visitante
  }
  const counter=document.getElementById('lightbox-counter');
  if (counter) counter.textContent=`${_lbCurrent+1} / ${_lbImages.length}`;
  const credit=document.getElementById('lightbox-credit');
  const meta=isVideo ? item : IMAGE_META_BY_FILE[item.archivo];
  if (credit) {
    if (!meta || (!isVideo && !IMAGE_META_BY_FILE[item.archivo])) {
      credit.innerHTML=''; credit.style.display='none';
    } else {
      const name=gallerySafe(meta.tecnica, 'Técnica textil');
      credit.style.display='';
      if (isVideo) {
        const artisan=gallerySafe(meta.artesano,'No registrado');
        const state=gallerySafe(meta.estado,'No registrado');
        const town=gallerySafe(meta.municipio,'No registrado');
        const locality=gallerySafe(meta.localidad,'No registrado');
        credit.innerHTML=`<div class="lightbox-credit-kicker">Registro audiovisual</div><h3>${esc(name)}</h3>
          <dl><div><dt>Artesana/o</dt><dd>${esc(artisan)}</dd></div>
          <div><dt>Estado</dt><dd>${esc(state)}</dd></div>
          <div><dt>Municipio</dt><dd>${esc(town)}</dd></div>
          <div><dt>Localidad</dt><dd>${esc(locality)}</dd></div></dl>`;
      } else {
        const artisan=gallerySafe(meta.artesano,'Nombre no registrado');
        const state=gallerySafe(meta.estado);
        const town=gallerySafe(meta.municipio);
        const locality=gallerySafe(meta.localidad);
        credit.innerHTML=`${meta.categoria ? `<div class="lightbox-credit-kicker">${esc(meta.categoria)}</div>` : '<div class="lightbox-credit-kicker">Registro fotográfico</div>'}
          <h3>${esc(name)}</h3><dl><div><dt>Artesana/o</dt><dd>${esc(artisan)}</dd></div>
          <div><dt>Estado</dt><dd>${esc(state)}</dd></div><div><dt>Municipio</dt><dd>${esc(town)}</dd></div>
          <div><dt>Localidad</dt><dd>${esc(locality)}</dd></div></dl>`;
      }
    }
  }
  const prev=document.getElementById('lightbox-prev');
  const next=document.getElementById('lightbox-next');
  if (prev) prev.style.display=_lbImages.length>1?'':'none';
  if (next) next.style.display=_lbImages.length>1?'':'none';
}
document.getElementById('lightbox-close').addEventListener('click',atlasCloseLightbox);
document.getElementById('lightbox').addEventListener('click',e => {
  if (e.target===document.getElementById('lightbox')) atlasCloseLightbox();
});
document.addEventListener('keydown',e=>{
  const lb=document.getElementById('lightbox');
  if (!lb.classList.contains('open')) return;
  if (e.key==='Escape') {atlasCloseLightbox();return;}
  if (e.target?.tagName==='VIDEO') return;
  if (e.key==='ArrowRight') {_lbCurrent=(_lbCurrent+1)%_lbImages.length;_renderLightbox();}
  if (e.key==='ArrowLeft') {_lbCurrent=(_lbCurrent-1+_lbImages.length)%_lbImages.length;_renderLightbox();}
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

  // ── Soporte táctil (móvil/tablet): un dedo = arrastrar nodo o desplazar
  // el lienzo (equivalente a mousedown/mousemove/mouseup); dos dedos = pinch-zoom.
  let touchMode = null;       // 'pan' | 'drag' | 'pinch'
  let touchStart = null;      // {x,y} del primer toque, para distinguir tap de arrastre
  let pinchStartDist = 0;
  let pinchStartK = 1;
  let pinchMidpoint = null;

  function touchPoint(t) {
    const rect = canvas.getBoundingClientRect();
    return { x: t.clientX - rect.left, y: t.clientY - rect.top };
  }
  function touchDist(t1, t2) {
    return Math.hypot(t1.clientX - t2.clientX, t1.clientY - t2.clientY);
  }

  canvas.addEventListener('touchstart', e => {
    if (e.touches.length === 1) {
      const p = touchPoint(e.touches[0]);
      touchStart = p;
      const node = getNodeAtPoint(p.x, p.y);
      if (node) { dragNode = node; touchMode = 'drag'; }
      else { isPanning = true; panStart = { x: p.x, y: p.y }; touchMode = 'pan'; }
    } else if (e.touches.length === 2) {
      dragNode = null; isPanning = false; touchMode = 'pinch';
      pinchStartDist = touchDist(e.touches[0], e.touches[1]);
      pinchStartK = transform.k;
      const p1 = touchPoint(e.touches[0]), p2 = touchPoint(e.touches[1]);
      pinchMidpoint = { x: (p1.x + p2.x) / 2, y: (p1.y + p2.y) / 2 };
    }
    e.preventDefault();
  }, { passive: false });

  canvas.addEventListener('touchmove', e => {
    if (touchMode === 'pinch' && e.touches.length === 2) {
      const dist = touchDist(e.touches[0], e.touches[1]);
      const newK = Math.max(0.3, Math.min(3, pinchStartK * (dist / pinchStartDist)));
      const mx = pinchMidpoint.x, my = pinchMidpoint.y;
      transform.x = mx - (mx - transform.x) * (newK / transform.k);
      transform.y = my - (my - transform.y) * (newK / transform.k);
      transform.k = newK; drawNetwork();
    } else if (e.touches.length === 1) {
      const p = touchPoint(e.touches[0]);
      if (touchMode === 'pan' && panStart) {
        transform.x += p.x - panStart.x; transform.y += p.y - panStart.y;
        panStart = { x: p.x, y: p.y }; drawNetwork();
      } else if (touchMode === 'drag' && dragNode) {
        dragNode.x = (p.x - transform.x) / transform.k;
        dragNode.y = (p.y - transform.y) / transform.k;
        dragNode.fx = dragNode.x; dragNode.fy = dragNode.y;
        drawNetwork();
      }
    }
    e.preventDefault();
  }, { passive: false });

  canvas.addEventListener('touchend', e => {
    if (dragNode) { dragNode.fx = null; dragNode.fy = null; }
    // Tap (toque breve sin desplazamiento) = equivalente a click: abre ficha o resalta grupo
    if (touchMode !== 'pinch' && touchStart) {
      const p = touchStart;
      const node = getNodeAtPoint(p.x, p.y);
      const moved = touchMode === 'pan' && panStart
        ? Math.abs(p.x - panStart.x) >= 5
        : false;
      if (node && !moved) {
        if (node.type === 'tecnica') openFicha(node.id);
        else if (node.type === 'grupo') {
          highlightedNode = highlightedNode?.id === node.id ? null : node;
          renderSublayerChips();
          drawNetwork();
        }
      }
    }
    dragNode = null; isPanning = false; panStart = null;
    touchMode = null; touchStart = null;
    tooltip.classList.remove('visible');
  }, { passive: true });

  canvas.addEventListener('touchcancel', () => {
    dragNode = null; isPanning = false; panStart = null;
    touchMode = null; touchStart = null;
  }, { passive: true });

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


// ────────────────────────────────────────────────
// ÁRBOL DE CLASIFICACIÓN — diagrama de llaves
// ────────────────────────────────────────────────
function initArbol() {
  if (!ATLAS) return;
  const tecnicas = ATLAS.tecnicas || [];
  const container = document.getElementById('arbol-container');
  if (!container) return;

  const root = { name: 'Clasificación del arte textil', count: tecnicas.length, categories: [] };
  const byCategory = new Map();

  tecnicas.forEach(t => {
    const cat1 = (t.cat1 || 'Sin categoría').trim();
    const cat2Original = (t.cat2 || 'Sin subcategoría').trim();
    const cat2 = cat1 === 'Acabados' && normalizeLookup(cat2Original) === 'sin subcategoria' ? 'Otras' : cat2Original;
    const cat3 = (t.cat3 || '').trim();
    const cat4 = (t.cat4 || '').trim();

    if (!byCategory.has(cat1)) {
      byCategory.set(cat1, {
        name: cat1,
        color: CAT1_COLOR[cat1] || COLORS.arena,
        tecnicas: new Set(),
        subcats: new Map(),
      });
    }

    const catNode = byCategory.get(cat1);
    catNode.tecnicas.add(t.tecnica);

    if (!catNode.subcats.has(cat2)) {
      catNode.subcats.set(cat2, {
        name: cat2,
        parent: cat1,
        color: CAT2_COLOR[cat2] || catNode.color,
        tecnicas: new Set(),
        tipos: new Map(),
      });
    }

    const subNode = catNode.subcats.get(cat2);
    subNode.tecnicas.add(t.tecnica);

    if (cat3) {
      if (!subNode.tipos.has(cat3)) {
        subNode.tipos.set(cat3, {
          name: cat3,
          parent: cat2,
          category: cat1,
          tecnicas: new Set(),
          variantes: new Map(),
        });
      }
      const typeNode = subNode.tipos.get(cat3);
      typeNode.tecnicas.add(t.tecnica);

      if (cat4) {
        if (!typeNode.variantes.has(cat4)) {
          typeNode.variantes.set(cat4, {
            name: cat4,
            parent: cat3,
            subcategory: cat2,
            category: cat1,
            tecnicas: new Set(),
          });
        }
        typeNode.variantes.get(cat4).tecnicas.add(t.tecnica);
      }
    }
  });


  // Estructura de referencia completa de la clasificación textil.
  // Los niveles ausentes en el CSV se muestran con 0 registros.
  // Se preservan asimismo las categorías adicionales realmente presentes en los datos.
  const TAXONOMY_REFERENCE = {
    'Tejido': {
      'Tejidos': {
        'Telares': ['Telar de cintura', 'Telar de pedal', 'Otros telares'],
        'Tejidos planos': [],
        'Gasas': [],
        'Tejidos de urdimbre': [],
        'Tapicería': [],
      },
      'Anudados': {},
    },
    'Técnicas decorativas': {
      'Bordados': {
        'A mano': ['Lineal', 'Punto', 'Libres', 'Deshilado'],
        'Máquina de pedal': [],
      },
      'Aplicaciones': {},
      'Otras': {},
    },
    'Acabados': {
      'Empuntados': {}, 'Randas': {}, 'Festón': {},
      'Sobre teñido': {}, 'Pintura textil': {}, 'Otras': {},
    },
  };
  Object.entries(TAXONOMY_REFERENCE).forEach(([categoryName, subcategories]) => {
    if (!byCategory.has(categoryName)) {
      byCategory.set(categoryName, {
        name: categoryName, color: CAT1_COLOR[categoryName] || COLORS.arena,
        tecnicas: new Set(), subcats: new Map(),
      });
    }
    const category = byCategory.get(categoryName);
    Object.entries(subcategories).forEach(([subcategoryName, types]) => {
      if (!category.subcats.has(subcategoryName)) {
        category.subcats.set(subcategoryName, {
          name: subcategoryName, parent: categoryName,
          color: CAT2_COLOR[subcategoryName] || category.color,
          tecnicas: new Set(), tipos: new Map(),
        });
      }
      const subcategory = category.subcats.get(subcategoryName);
      Object.entries(types).forEach(([typeName, variants]) => {
        if (!subcategory.tipos.has(typeName)) {
          subcategory.tipos.set(typeName, {
            name: typeName, parent: subcategoryName, category: categoryName,
            tecnicas: new Set(), variantes: new Map(),
          });
        }
        const type = subcategory.tipos.get(typeName);
        variants.forEach(name => {
          if (!type.variantes.has(name)) {
            type.variantes.set(name, {
              name, parent: typeName, subcategory: subcategoryName,
              category: categoryName, tecnicas: new Set(),
            });
          }
        });
      });
    });
  });

  root.categories = [...byCategory.values()].map(cat => ({
    ...cat,
    subcatsArr: [...cat.subcats.values()].map(sub => ({
      ...sub,
      tiposArr: [...sub.tipos.values()].map(type => ({
        ...type,
        variantesArr: [...type.variantes.values()].sort((a, b) => b.tecnicas.size - a.tecnicas.size || a.name.localeCompare(b.name, 'es')),
      })).sort((a, b) => a.name.localeCompare(b.name, 'es')),
    })).sort((a, b) => {
      const isOtras = name => /^otras?$/i.test(String(name).trim());
      return Number(isOtras(a.name)) - Number(isOtras(b.name)) || a.name.localeCompare(b.name, 'es');
    }),
  })).sort((a, b) => {
    // En el diagrama, la categoría Otras siempre aparece al final.
    const isOther = name => /^(otras|otros)$/i.test(String(name || '').trim());
    const order = ['Tejido', 'Técnicas decorativas', 'Acabados', 'Teñidos'];
    const pos = n => order.includes(n) ? order.indexOf(n) : 100;
    return Number(isOther(a.name)) - Number(isOther(b.name)) || pos(a.name) - pos(b.name) || a.name.localeCompare(b.name, 'es');
  });

  const categoryIndex = new Map(root.categories.map(cat => [cat.name, cat]));
  const subcategoryIndex = new Map();
  const typeIndex = new Map();
  const variantIndex = new Map();

  root.categories.forEach(cat => {
    cat.subcatsArr.forEach(sub => {
      const subKey = `${cat.name}|||${sub.name}`;
      subcategoryIndex.set(subKey, sub);
      sub.tiposArr.forEach(type => {
        const typeKey = `${subKey}|||${type.name}`;
        typeIndex.set(typeKey, type);
        type.variantesArr.forEach(variant => {
          const variantKey = `${typeKey}|||${variant.name}`;
          variantIndex.set(variantKey, variant);
        });
      });
    });
  });

  const expandedCategories = new Set(root.categories.map(cat => cat.name));
  const expandedSubcategories = new Set();
  const expandedTypes = new Set();

  let activeCategory = root.categories[0]?.name || null;
  let activeSubcategoryKey = null;
  let activeTypeKey = null;
  let activeVariantKey = null;

  function escAttr(s) { return esc(String(s || '')); }

  function getCategoryCounts(cat) {
    const tipos = new Set();
    const variantes = new Set();
    cat.subcatsArr.forEach(sub => {
      sub.tiposArr.forEach(type => {
        tipos.add(type.name);
        type.variantesArr.forEach(v => variantes.add(v.name));
      });
    });
    return { tipos: tipos.size, variantes: variantes.size };
  }

  function activateCategory(catName) {
    activeCategory = catName;
    activeSubcategoryKey = null;
    activeTypeKey = null;
    activeVariantKey = null;
  }

  function activateSubcategory(subKey) {
    activeSubcategoryKey = subKey;
    activeTypeKey = null;
    activeVariantKey = null;
    activeCategory = subKey.split('|||')[0] || activeCategory;
  }

  function activateType(typeKey) {
    activeTypeKey = typeKey;
    activeVariantKey = null;
    activeSubcategoryKey = typeKey.split('|||').slice(0,2).join('|||');
    activeCategory = typeKey.split('|||')[0] || activeCategory;
  }

  function activateVariant(variantKey) {
    activeVariantKey = variantKey;
    activeTypeKey = variantKey.split('|||').slice(0,3).join('|||');
    activeSubcategoryKey = variantKey.split('|||').slice(0,2).join('|||');
    activeCategory = variantKey.split('|||')[0] || activeCategory;
  }

  function renderVariantRows(type, typeKey) {
    if (!type.variantesArr.length) return '';
    const open = expandedTypes.has(typeKey);
    return `<div class="brace-variant-list${open ? ' open' : ''}">
      ${type.variantesArr.map(v => {
        const variantKey = `${typeKey}|||${v.name}`;
        const isActive = activeVariantKey === variantKey;
        return `<div class="brace-variant-row">
          <span class="brace-variant-bullet" aria-hidden="true"></span>
          <button type="button" class="brace-variant-card${isActive ? ' is-active' : ''}${v.tecnicas.size ? '' : ' is-empty'}" data-variant-select="${escAttr(variantKey)}">
            <span class="brace-level-copy">
              <span class="brace-level-label">Variante</span>
              <span class="brace-level-name">${esc(v.name)}</span>
            </span>
            <span class="brace-subcat-count">${v.tecnicas.size}</span>
          </button>
        </div>`;
      }).join('')}
    </div>`;
  }

  function renderTypeRows(sub, subKey) {
    if (!sub.tiposArr.length) return '';
    const open = expandedSubcategories.has(subKey);
    return `<div class="brace-type-list${open ? ' open' : ''}">
      ${sub.tiposArr.map(type => {
        const typeKey = `${subKey}|||${type.name}`;
        const typeOpen = expandedTypes.has(typeKey);
        const isActive = activeTypeKey === typeKey;
        return `<div class="brace-type-node">
          <div class="brace-type-row">
            <button type="button" class="brace-mini-toggle" data-type-toggle="${escAttr(typeKey)}" aria-expanded="${typeOpen}" title="${typeOpen ? 'Ocultar' : 'Mostrar'} variantes">
              ${type.variantesArr.length ? (typeOpen ? '−' : '+') : '•'}
            </button>
            <button type="button" class="brace-type-card${isActive ? ' is-active' : ''}${type.tecnicas.size ? '' : ' is-empty'}" data-type-select="${escAttr(typeKey)}">
              <span class="brace-level-copy">
                <span class="brace-level-label">Tipo</span>
                <span class="brace-level-name">${esc(type.name)}</span>
              </span>
              <span class="brace-subcat-count">${type.tecnicas.size}</span>
            </button>
          </div>
          ${renderVariantRows(type, typeKey)}
        </div>`;
      }).join('')}
    </div>`;
  }

  function categoryCardHTML(cat) {
    const expanded = expandedCategories.has(cat.name);
    const active = activeCategory === cat.name && !activeSubcategoryKey && !activeTypeKey && !activeVariantKey;
    return `<section class="brace-group${active ? ' active' : ''}" style="--brace-color:${cat.color}">
      <div class="brace-cat-col">
        <div class="brace-cat-main">
          <button type="button" class="brace-mini-toggle brace-cat-toggle" data-cat-toggle="${escAttr(cat.name)}" aria-expanded="${expanded}" title="${expanded ? 'Ocultar' : 'Mostrar'} subcategorías">${expanded ? '−' : '+'}</button>
          <button type="button" class="brace-cat-card${active ? ' is-active' : ''}${cat.tecnicas.size ? '' : ' is-empty'}" data-cat-select="${escAttr(cat.name)}">
            <span class="brace-cat-name">${esc(cat.name === 'Tejido' ? 'Tejido artesanal' : cat.name)}</span>
            <span class="brace-cat-count">${cat.tecnicas.size}</span>
          </button>
        </div>
      </div>
      <div class="brace-link${expanded ? ' open' : ''}" aria-hidden="true"></div>
      <div class="brace-subcats-wrap${expanded ? '' : ' collapsed'}"${expanded ? '' : ' hidden'}>
        <div class="brace-subcats-list${expanded ? ' open' : ''}">
          ${cat.subcatsArr.map(sub => {
            const key = `${cat.name}|||${sub.name}`;
            const activeSub = activeSubcategoryKey === key && !activeTypeKey && !activeVariantKey;
            const subOpen = expandedSubcategories.has(key);
            return `<div class="brace-subcat-node" style="--subcat-color:${sub.color}">
              <div class="brace-subcat-row">
                <button type="button" class="brace-mini-toggle" data-subcat-toggle="${escAttr(key)}" aria-expanded="${subOpen}" title="${subOpen ? 'Ocultar' : 'Mostrar'} tipos">${sub.tiposArr.length ? (subOpen ? '−' : '+') : '•'}</button>
                <button type="button" class="brace-subcat-card${activeSub ? ' is-active' : ''}${sub.tecnicas.size ? '' : ' is-empty'}" data-subcat-select="${escAttr(key)}">
                  <span class="brace-subcat-copy">
                    <span class="brace-subcat-name">${esc(sub.name)}</span>
                  </span>
                  <span class="brace-subcat-count">${sub.tecnicas.size}</span>
                </button>
              </div>
              ${renderTypeRows(sub, key)}
            </div>`;
          }).join('')}
        </div>
      </div>
    </section>`;
  }

  function renderDefaultDetail() {
    return `<aside class="class-detail-panel class-detail-empty">
      <div class="class-detail-kicker">Cómo leer el diagrama</div>
      <h3>Diagrama de llaves</h3>
      <p>Cada llave muestra la relación entre <strong>categorías</strong>, <strong>subcategorías</strong>, <strong>tipos</strong> y <strong>variantes</strong>. Selecciona cualquier nivel para consultar las técnicas registradas dentro de ese grupo.</p>
    </aside>`;
  }

  function renderTechList(names) {
    return `<div class="class-detail-list">
      ${names.length ? '' : '<p class="class-detail-zero">Aún no hay registros vinculados con este nivel de la clasificación.</p>'}
      ${names.map(name => `<div class="class-detail-tech-row">
        <span class="class-detail-tech-name">${esc(name)}</span>
        <div class="class-detail-tech-actions">
          <button type="button" class="class-detail-action class-detail-action-view" data-tech-name="${escAttr(name)}" aria-label="Ver ficha de ${escAttr(name)}">Ver ficha</button>
          <button type="button" class="class-detail-action class-detail-action-propose" data-tech-propose="${escAttr(name)}" aria-label="Proponer cambio de ${escAttr(name)}">Proponer cambio</button>
        </div>
      </div>`).join('')}
    </div>`;
  }

  function taxonomyBibliographyButton(card, level, displayName) {
    if (!card) return '';
    return `<button type="button" class="taxonomy-biblio-btn" data-tax-biblio="1" data-tax-level="${escAttr(level)}" data-tax-name="${escAttr(displayName)}">
      <img src="assets/bibliografia.png" alt="" aria-hidden="true">
      <span><b>Bibliografía</b><small>Consultar información documental</small></span>
    </button>`;
  }

  function openTaxonomyBibliographyModal(level, displayName, card) {
    if (!card) return;
    const overlay = document.getElementById('taxonomy-biblio-overlay');
    const body = document.getElementById('taxonomy-biblio-modal-body');
    if (!overlay || !body) return;

    body.innerHTML = `
      <div class="taxonomy-modal-kicker">${esc(level)} · información bibliográfica</div>
      <h2 class="taxonomy-modal-title" id="taxonomy-biblio-modal-title">${esc(displayName)}</h2>
      <div class="ficha-source-note biblio-note"><strong>Esta ficha está basada en la revisión documental y bibliográfica de diferentes fuentes. Las referencias utilizadas se presentan al final.</strong></div>
      ${renderBibliographyContent(card)}
    `;

    overlay.classList.add('open');
    overlay.setAttribute('aria-hidden', 'false');
    document.body.style.overflow = 'hidden';
  }

  function closeTaxonomyBibliographyModal() {
    const overlay = document.getElementById('taxonomy-biblio-overlay');
    if (!overlay) return;
    overlay.classList.remove('open');
    overlay.setAttribute('aria-hidden', 'true');
    document.body.style.overflow = '';
  }

  function detailPanelHTML() {
    if (activeVariantKey && variantIndex.has(activeVariantKey)) {
      const node = variantIndex.get(activeVariantKey);
      const names = [...node.tecnicas].sort((a,b) => a.localeCompare(b, 'es'));
      return `<aside class="class-detail-panel">
        <div class="class-detail-kicker">Variante</div>
        <h3>${esc(node.name)}</h3>
        <div class="class-detail-path"><span>${esc(node.category)}</span><b>›</b><span>${esc(node.subcategory)}</span><b>›</b><span>${esc(node.parent)}</span><b>›</b><span>${esc(node.name)}</span></div>
        <div class="class-detail-summary"><strong>${names.length}</strong><span>entrada${names.length !== 1 ? 's' : ''} del catálogo vinculadas con esta variante</span></div>
        <p class="class-detail-note">Selecciona un registro para consultar su ficha.</p>
        ${renderTechList(names)}
      </aside>`;
    }

    if (activeTypeKey && typeIndex.has(activeTypeKey)) {
      const node = typeIndex.get(activeTypeKey);
      const names = [...node.tecnicas].sort((a,b) => a.localeCompare(b, 'es'));
      return `<aside class="class-detail-panel">
        <div class="class-detail-kicker">Tipo</div>
        <h3>${esc(node.name)}</h3>
        <div class="class-detail-path"><span>${esc(node.category)}</span><b>›</b><span>${esc(node.parent)}</span><b>›</b><span>${esc(node.name)}</span></div>
        <div class="class-detail-summary"><strong>${names.length}</strong><span>entrada${names.length !== 1 ? 's' : ''} del catálogo vinculadas con este tipo</span></div>
        <p class="class-detail-note">Selecciona un registro para consultar su ficha.</p>
        ${renderTechList(names)}
      </aside>`;
    }

    if (activeSubcategoryKey && subcategoryIndex.has(activeSubcategoryKey)) {
      const node = subcategoryIndex.get(activeSubcategoryKey);
      const biblioCard = getTaxonomyBibliographyCard('Subcategoría', node.name);
      const names = [...node.tecnicas].sort((a,b) => a.localeCompare(b, 'es'));
      const variantCount = node.tiposArr.reduce((sum, t) => sum + t.variantesArr.length, 0);
      return `<aside class="class-detail-panel">
        <div class="class-detail-kicker">Subcategoría</div>
        <h3>${esc(node.name)}</h3>
        <div class="class-detail-path"><span>${esc(node.parent)}</span><b>›</b><span>${esc(node.name)}</span></div>
        ${taxonomyBibliographyButton(biblioCard, 'Subcategoría', node.name)}
        <div class="class-detail-summary"><strong>${names.length}</strong><span>entrada${names.length !== 1 ? 's' : ''} del catálogo vinculadas con esta subcategoría</span></div>
        <p class="class-detail-note">Selecciona un registro para consultar su ficha.</p>
        ${renderTechList(names)}
      </aside>`;
    }

    if (activeCategory && categoryIndex.has(activeCategory)) {
      const cat = categoryIndex.get(activeCategory);
      const biblioCard = getTaxonomyBibliographyCard('Categoría', cat.name);
      const counts = getCategoryCounts(cat);
      return `<aside class="class-detail-panel">
        <div class="class-detail-kicker">Categoría</div>
        <h3>${esc(cat.name === 'Tejido' ? 'Tejido artesanal' : cat.name)}</h3>
        ${taxonomyBibliographyButton(biblioCard, 'Categoría', cat.name)}
        <div class="class-detail-summary"><strong>${cat.tecnicas.size}</strong><span>entrada${cat.tecnicas.size !== 1 ? 's' : ''} del catálogo vinculadas con esta categoría</span></div>
        <p class="class-detail-note">Selecciona una subcategoría del diagrama para consultar los registros asociados.</p>
        <div class="class-detail-list compact">
          ${cat.subcatsArr.map(sub => `<button type="button" class="class-detail-tech" data-subcat-detail="${escAttr(`${cat.name}|||${sub.name}`)}"><span>${esc(sub.name)}</span><b>${sub.tecnicas.size}</b></button>`).join('')}
        </div>
      </aside>`;
    }

    return renderDefaultDetail();
  }

  function renderTree() {
    container.innerHTML = `<div class="class-schema-layout brace-layout">
      <section class="class-schema brace-schema">
        <div class="brace-toolbar">
          <div class="brace-toolbar-copy">Selecciona cualquier grupo para consultar sus registros.</div>
          <div class="brace-toolbar-actions">
            <button type="button" class="brace-toolbar-btn" id="brace-expand-all">Expandir todas</button>
            <button type="button" class="brace-toolbar-btn" id="brace-collapse-all">Contraer todas</button>
          </div>
        </div>
        <div class="brace-groups">
          ${root.categories.map(categoryCardHTML).join('')}
        </div>
      </section>
      ${detailPanelHTML()}
    </div>`;

    container.querySelectorAll('[data-cat-toggle]').forEach(btn => {
      btn.addEventListener('click', e => {
        e.stopPropagation();
        const name = btn.dataset.catToggle;
        if (expandedCategories.has(name)) expandedCategories.delete(name);
        else expandedCategories.add(name);
        renderTree();
      });
    });

    container.querySelectorAll('[data-subcat-toggle]').forEach(btn => {
      btn.addEventListener('click', e => {
        e.stopPropagation();
        const key = btn.dataset.subcatToggle;
        if (expandedSubcategories.has(key)) expandedSubcategories.delete(key);
        else expandedSubcategories.add(key);
        renderTree();
      });
    });

    container.querySelectorAll('[data-type-toggle]').forEach(btn => {
      btn.addEventListener('click', e => {
        e.stopPropagation();
        const key = btn.dataset.typeToggle;
        if (expandedTypes.has(key)) expandedTypes.delete(key);
        else expandedTypes.add(key);
        renderTree();
      });
    });

    container.querySelectorAll('[data-cat-select]').forEach(btn => {
      btn.addEventListener('click', () => {
        activateCategory(btn.dataset.catSelect);
        renderTree();
      });
    });

    container.querySelectorAll('[data-subcat-select],[data-subcat-detail]').forEach(btn => {
      btn.addEventListener('click', () => {
        const key = btn.dataset.subcatSelect || btn.dataset.subcatDetail;
        if (!key) return;
        activateSubcategory(key);
        expandedCategories.add(activeCategory);
        expandedSubcategories.add(key);
        renderTree();
      });
    });

    container.querySelectorAll('[data-type-select]').forEach(btn => {
      btn.addEventListener('click', () => {
        const key = btn.dataset.typeSelect;
        activateType(key);
        expandedCategories.add(activeCategory);
        expandedSubcategories.add(activeSubcategoryKey);
        expandedTypes.add(key);
        renderTree();
      });
    });

    container.querySelectorAll('[data-variant-select]').forEach(btn => {
      btn.addEventListener('click', () => {
        const key = btn.dataset.variantSelect;
        activateVariant(key);
        expandedCategories.add(activeCategory);
        expandedSubcategories.add(activeSubcategoryKey);
        expandedTypes.add(activeTypeKey);
        renderTree();
      });
    });

    container.querySelectorAll('[data-tech-name]').forEach(btn => {
      btn.addEventListener('click', () => openFicha(btn.dataset.techName));
    });
    container.querySelectorAll('[data-tech-propose]').forEach(btn => {
      btn.addEventListener('click', event => {
        event.preventDefault();
        if (typeof openKoboForm === 'function') openKoboForm('cambio');
      });
    });

    container.querySelectorAll('[data-tax-biblio]').forEach(btn => {
      btn.addEventListener('click', () => {
        const level = btn.dataset.taxLevel || '';
        const name = btn.dataset.taxName || '';
        const card = getTaxonomyBibliographyCard(level, name);
        openTaxonomyBibliographyModal(level, name, card);
      });
    });

    const taxOverlay = document.getElementById('taxonomy-biblio-overlay');
    const taxClose = document.getElementById('taxonomy-biblio-modal-close');
    if (taxClose && !taxClose.dataset.bound) {
      taxClose.addEventListener('click', closeTaxonomyBibliographyModal);
      taxClose.dataset.bound = '1';
    }
    if (taxOverlay && !taxOverlay.dataset.bound) {
      taxOverlay.addEventListener('click', e => {
        if (e.target === taxOverlay) closeTaxonomyBibliographyModal();
      });
      taxOverlay.dataset.bound = '1';
    }

    document.getElementById('brace-expand-all')?.addEventListener('click', () => {
      root.categories.forEach(cat => {
        cat.subcatsArr.forEach(sub => {
          const subKey = `${cat.name}|||${sub.name}`;
          if (sub.tiposArr.length) expandedSubcategories.add(subKey);
          sub.tiposArr.forEach(type => { if (type.variantesArr.length) expandedTypes.add(`${subKey}|||${type.name}`); });
        });
      });
      renderTree();
    });

    document.getElementById('brace-collapse-all')?.addEventListener('click', () => {
      expandedSubcategories.clear();
      expandedTypes.clear();
      renderTree();
    });
  }

  if (!activeCategory && root.categories.length) activeCategory = root.categories[0].name;
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
    id: 'catalogo', label: 'Catálogo de registros',
    desc: 'Ficha resumida de cada entrada del catálogo, con su fotografía principal.',
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

function validarReportePaso(paso) {
  if (paso === 1) {
    const subset = obtenerSubset();
    if (!subset.tecnicas.length) {
      alert('La selección actual no incluye ninguna técnica. Ajusta los filtros del Paso 1 antes de continuar.');
      return false;
    }
  }

  if (paso === 2 && reporteState.secciones.size === 0) {
    alert('Selecciona al menos una sección en el Paso 2 antes de continuar a la vista previa.');
    return false;
  }

  return true;
}

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

  if (n > reporteCurrentStep) {
    for (let paso = reporteCurrentStep; paso < n; paso++) {
      if (!validarReportePaso(paso)) return;
    }
  }

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

  Object.keys(cat1Map).sort((a,b) => (catalogNorm(a) === 'otras' ? 1 : 0) - (catalogNorm(b) === 'otras' ? 1 : 0) || a.localeCompare(b,'es')).forEach(c1 => {
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
      <p>Generado el ${esc(fecha)} a partir del Atlas Comunitario del Arte Textil de México.<br>
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
        <img src="assets/atlas_plataforma_logos_cropped.png" alt="Logos institucionales" />
      </div>
      <div class="rep-cover-eyebrow">Atlas Comunitario del Arte Textil de México</div>
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
      <p class="rep-lead">Este reporte abarca <strong>${tecs.length} entradas del catálogo</strong> documentadas
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
      <h2>Catálogo de registros</h2>
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
      <p class="rep-prose">Distribución de los registros según la clasificación de textiles del Atlas.</p>
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
      text: 'Atlas Comunitario del Arte Textil de México',
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
      text: `Generado el ${fecha} a partir del Atlas Comunitario del Arte Textil de México. Encuentro Nacional de Arte Textil "Original" · Secretaría de Cultura · UNESCO México.`,
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
      text: `Este reporte abarca ${subset.tecnicas.length} entradas del catálogo documentadas en ${subset.estados.length} estados de la República Mexicana, con un total de ${subset.totalRegistros} registros. El promedio es de ${promFichas} aportaciones por entrada del catálogo.`,
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

// ────────────────────────────────────────────────
// ADAPTACIÓN MÓVIL — altura real del header + nav
// ────────────────────────────────────────────────
(function () {
  function updateChromeHeight() {
    const header = document.querySelector('.header');
    const nav = document.querySelector('.nav-tabs');
    const h = (header ? header.offsetHeight : 0) + (nav ? nav.offsetHeight : 0);
    if (h > 0) document.documentElement.style.setProperty('--chrome-h', h + 'px');
  }
  updateChromeHeight();
  window.addEventListener('resize', updateChromeHeight);
  window.addEventListener('orientationchange', () => setTimeout(updateChromeHeight, 250));
  window.addEventListener('load', updateChromeHeight);
  if (window.ResizeObserver) {
    const ro = new ResizeObserver(updateChromeHeight);
    document.addEventListener('DOMContentLoaded', () => {
      const header = document.querySelector('.header');
      const nav = document.querySelector('.nav-tabs');
      if (header) ro.observe(header);
      if (nav) ro.observe(nav);
    });
  }
})();

// Cerrar modal bibliográfico de clasificación con Escape
document.addEventListener('keydown', (e) => {
  if (e.key === 'Escape') {
    const overlay = document.getElementById('taxonomy-biblio-overlay');
    if (overlay?.classList.contains('open')) {
      overlay.classList.remove('open');
      overlay.setAttribute('aria-hidden', 'true');
      document.body.style.overflow = '';
    }
  }
});
