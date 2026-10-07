/*
 * Atlas textil — drivers territoriales v6
 * ---------------------------------------
 * Principio: las comunidades son la capa principal y siempre permanecen arriba.
 * Estado, Lengua y Ecosistema son contextos territoriales independientes.
 * No se utilizan relaciones precalculadas lengua↔técnica ni ecosistema↔material.
 */

const _atlasInitMapFilter = initMapFilter;
const _atlasClearMapFilter = clearMapFilter;
const _atlasApplyMapFilter = applyMapFilter;
const _atlasRenderStatesList = renderStatesList;
const _atlasOnStateClick = onStateClick;

let activeMapDriver = 'estado';
let activeEcosystemSource = 'ecorregiones';
const selectedLanguageIds = new Set();
const selectedStateNames = new Set();
const selectedEcosystemCodes = new Set();
let languageCloudData = [];
let languageCloudLayer = null;
let ecosystemLayers = { ecorregiones: null, usv: null };
let ecosystemData = { ecorregiones: null, usv: null };
let materialMeta = {};
let driverReady = false;
let languageSearchText = '';
let materialSearchText = { estado: '', lengua: '', ecosistema: '' };
const selectedMaterials = {
  estado: new Set(),
  lengua: new Set(),
  ecosistema: new Set(),
};

const LANGUAGE_RADIUS_KM = 20;
const MX_BOUNDS = [[14.5, -118.4], [32.7, -86.7]];
const CONTEXT_STATE_STYLE = {
  fillColor: '#F3EEE9',
  weight: 0.7,
  opacity: 0.72,
  color: '#C7BFB8',
  fillOpacity: 0.08,
};

const USV_COLORS = {
  'Bosques templados': '#2E7D32',
  'Matorrales y zonas áridas': '#B58B3A',
  'Selvas secas': '#D9782D',
  'Selvas húmedas': '#0B8F6A',
  'Humedales y vegetación ribereña': '#2C8ECF',
  'Palmares': '#9A7B32',
};

function normalizeDriverText(value) {
  return String(value || '')
    .toLowerCase()
    .normalize('NFD').replace(/[\u0300-\u036f]/g, '')
    .replace(/[^a-z0-9]+/g, ' ')
    .trim();
}

function splitSemicolon(value) {
  return String(value || '').split(';').map(v => v.trim()).filter(Boolean);
}

function hashString(value) {
  let h = 2166136261;
  const text = String(value || '');
  for (let i = 0; i < text.length; i++) {
    h ^= text.charCodeAt(i);
    h = Math.imul(h, 16777619);
  }
  return h >>> 0;
}

function hslColorForKey(key, index = 0) {
  const hue = ((hashString(key) % 360) + index * 37) % 360;
  const sat = 62 + (index % 3) * 7;
  const light = 42 + ((index >> 1) % 3) * 6;
  return `hsl(${hue} ${sat}% ${light}%)`;
}

function materialTechniqueSet(driver) {
  const selected = selectedMaterials[driver];
  if (!selected || !selected.size) return null;
  const out = new Set();
  selected.forEach(name => {
    const item = materialMeta[name];
    if (!item) return;
    item.techniques.forEach(t => out.add(t));
  });
  return out;
}

function communityTechniques(feature) {
  const est = feature?.properties?.estado_nombre || '';
  const mun = feature?.properties?.NOMGEO || '';
  return getTecnicasForMunicipio(est, mun) || [];
}

function communityMatchesMaterials(feature, driver) {
  const allowed = materialTechniqueSet(driver);
  if (!allowed) return true;
  return communityTechniques(feature).some(t => allowed.has(t));
}

// ---------- Spatial helpers ----------
function pointInRing(lon, lat, ring) {
  let inside = false;
  for (let i = 0, j = ring.length - 1; i < ring.length; j = i++) {
    const xi = Number(ring[i][0]), yi = Number(ring[i][1]);
    const xj = Number(ring[j][0]), yj = Number(ring[j][1]);
    const intersects = ((yi > lat) !== (yj > lat)) &&
      (lon < (xj - xi) * (lat - yi) / ((yj - yi) || 1e-12) + xi);
    if (intersects) inside = !inside;
  }
  return inside;
}

function pointInPolygonCoordinates(lon, lat, polygon) {
  if (!polygon?.length || !pointInRing(lon, lat, polygon[0])) return false;
  for (let i = 1; i < polygon.length; i++) {
    if (pointInRing(lon, lat, polygon[i])) return false;
  }
  return true;
}

function pointInGeometry(lon, lat, geometry) {
  if (!geometry) return false;
  if (geometry.type === 'Polygon') return pointInPolygonCoordinates(lon, lat, geometry.coordinates);
  if (geometry.type === 'MultiPolygon') return geometry.coordinates.some(poly => pointInPolygonCoordinates(lon, lat, poly));
  if (geometry.type === 'GeometryCollection') return geometry.geometries.some(g => pointInGeometry(lon, lat, g));
  return false;
}

function haversineKm(lon1, lat1, lon2, lat2) {
  const toRad = Math.PI / 180;
  const dLat = (lat2 - lat1) * toRad;
  const dLon = (lon2 - lon1) * toRad;
  const a = Math.sin(dLat / 2) ** 2 +
    Math.cos(lat1 * toRad) * Math.cos(lat2 * toRad) * Math.sin(dLon / 2) ** 2;
  return 6371.0088 * 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
}

const languageGridCache = new Map();
function languageSpatialIndex(language) {
  if (!language) return null;
  if (languageGridCache.has(language.id)) return languageGridCache.get(language.id);
  const cell = 0.25;
  const grid = new Map();
  language.coords.forEach(([lon, lat]) => {
    const key = `${Math.floor(lon / cell)}|${Math.floor(lat / cell)}`;
    if (!grid.has(key)) grid.set(key, []);
    grid.get(key).push([lon, lat]);
  });
  const index = { cell, grid };
  languageGridCache.set(language.id, index);
  return index;
}

function communityInsideLanguageCloud(feature, language) {
  if (!language) return true;
  const geom = feature?.geometry;
  if (!geom || geom.type !== 'Point') return false;
  const [lon, lat] = geom.coordinates;
  const idx = languageSpatialIndex(language);
  const cx = Math.floor(lon / idx.cell), cy = Math.floor(lat / idx.cell);
  const reach = 1;
  for (let dx = -reach; dx <= reach; dx++) {
    for (let dy = -reach; dy <= reach; dy++) {
      const points = idx.grid.get(`${cx + dx}|${cy + dy}`) || [];
      for (const [plon, plat] of points) {
        if (haversineKm(lon, lat, plon, plat) <= LANGUAGE_RADIUS_KM) return true;
      }
    }
  }
  return false;
}

function selectedEcosystemFeatures() {
  if (!selectedEcosystemCodes.size) return [];
  const data = ecosystemData[activeEcosystemSource];
  return (data?.features || []).filter(f => selectedEcosystemCodes.has(ecosystemFeatureCode(f, activeEcosystemSource)));
}

function communityInsideEcosystem(feature) {
  const selected = selectedEcosystemFeatures();
  if (!selected.length) return true;
  const geom = feature?.geometry;
  if (!geom || geom.type !== 'Point') return false;
  const lon = Number(geom.coordinates[0]), lat = Number(geom.coordinates[1]);
  return selected.some(item => pointInGeometry(lon, lat, item.geometry));
}

function refreshCommunities() {
  if (!municipiosLoaded || typeof setCommunityFilter !== 'function') return;
  const languages = languageCloudData.filter(x => selectedLanguageIds.has(x.id));
  setCommunityFilter(feature => {
    // La clasificación/técnica es un filtro común a los tres enfoques.
    if (mapFilteredTecnicas) {
      const matchesTechnique = communityTechniques(feature).some(t => mapFilteredTecnicas.has(t));
      if (!matchesTechnique) return false;
    }
    if (activeMapDriver === 'estado') {
      if (selectedStateNames.size) {
        const estado = feature?.properties?.estado_nombre || '';
        const wanted = [...selectedStateNames].some(name => normalizeEstado(name) === normalizeEstado(estado));
        if (!wanted) return false;
      }
      if (!communityMatchesMaterials(feature, 'estado')) return false;
      return true;
    }
    if (activeMapDriver === 'lengua') {
      if (!communityMatchesMaterials(feature, 'lengua')) return false;
      if (languages.length && !languages.some(language => communityInsideLanguageCloud(feature, language))) return false;
      return true;
    }
    if (!communityMatchesMaterials(feature, 'ecosistema')) return false;
    if (selectedEcosystemCodes.size && !communityInsideEcosystem(feature)) return false;
    return true;
  });
  updateTerritoryPanel();
}

function visibleCommunityLayers() {
  if (!municipiosLayer) return [];
  return municipiosLayer.getLayers ? municipiosLayer.getLayers() : [];
}

function visibleTechniqueNames(driver = activeMapDriver) {
  const names = new Set();
  visibleCommunityLayers().forEach(layer => communityTechniques(layer.feature).forEach(t => names.add(t)));
  let out = [...names].filter(t => tecnicasMap[t]);
  const mats = materialTechniqueSet(driver);
  if (mats) out = out.filter(t => mats.has(t));
  if (mapFilteredTecnicas) out = out.filter(t => mapFilteredTecnicas.has(t));
  return out.sort((a, b) => a.localeCompare(b, 'es'));
}

// ---------- Language cloud canvas ----------
class AtlasLanguageCloudLayer extends L.Layer {
  constructor(languages) {
    super();
    this.languages = languages || [];
    this.selectedIds = new Set();
    this._move = this._reset.bind(this);
  }
  onAdd(mapRef) {
    this._map = mapRef;
    if (!mapRef.getPane('atlasLanguageCloudPane')) {
      mapRef.createPane('atlasLanguageCloudPane');
      const pane = mapRef.getPane('atlasLanguageCloudPane');
      pane.style.zIndex = 440;
      pane.style.pointerEvents = 'none';
    }
    this._canvas = L.DomUtil.create('canvas', 'atlas-language-canvas', mapRef.getPane('atlasLanguageCloudPane'));
    this._canvas.style.pointerEvents = 'none';
    mapRef.on('moveend zoomend resize', this._move);
    this._reset();
  }
  onRemove(mapRef) {
    mapRef.off('moveend zoomend resize', this._move);
    if (this._canvas?.parentNode) this._canvas.parentNode.removeChild(this._canvas);
    this._canvas = null;
  }
  setSelected(ids) {
    this.selectedIds = new Set(ids || []);
    this._reset();
  }
  _reset() {
    if (!this._map || !this._canvas) return;
    const size = this._map.getSize();
    const ratio = Math.min(window.devicePixelRatio || 1, 2);
    this._canvas.width = Math.max(1, Math.round(size.x * ratio));
    this._canvas.height = Math.max(1, Math.round(size.y * ratio));
    this._canvas.style.width = `${size.x}px`;
    this._canvas.style.height = `${size.y}px`;
    const topLeft = this._map.containerPointToLayerPoint([0, 0]);
    L.DomUtil.setPosition(this._canvas, topLeft);
    const ctx = this._canvas.getContext('2d');
    ctx.setTransform(ratio, 0, 0, ratio, 0, 0);
    ctx.clearRect(0, 0, size.x, size.y);
    const bounds = this._map.getBounds().pad(0.08);
    const selectedOnly = this.selectedIds.size > 0;
    const radius = selectedOnly ? 3.2 : 2.25;
    const alpha = selectedOnly ? 0.44 : 0.24;
    const langs = selectedOnly ? this.languages.filter(l => this.selectedIds.has(l.id)) : this.languages;
    langs.forEach(lang => {
      ctx.beginPath();
      let n = 0;
      lang.coords.forEach(([lon, lat]) => {
        if (!bounds.contains([lat, lon])) return;
        const p = this._map.latLngToContainerPoint([lat, lon]);
        ctx.moveTo(p.x + radius, p.y);
        ctx.arc(p.x, p.y, radius, 0, Math.PI * 2);
        n++;
      });
      if (!n) return;
      ctx.globalAlpha = alpha;
      ctx.fillStyle = lang.color;
      ctx.fill();
    });
    ctx.globalAlpha = 1;
  }
}

function ensureLanguageLayer() {
  if (!languageCloudData.length || !map) return;
  if (!languageCloudLayer) languageCloudLayer = new AtlasLanguageCloudLayer(languageCloudData);
  if (!map.hasLayer(languageCloudLayer)) languageCloudLayer.addTo(map);
  languageCloudLayer.setSelected(selectedLanguageIds);
}

function removeLanguageLayer() {
  if (languageCloudLayer && map?.hasLayer(languageCloudLayer)) map.removeLayer(languageCloudLayer);
}

// ---------- Ecosystems ----------
function ecosystemFeatureCode(feature, source) {
  if (source === 'usv') return String(feature?.properties?.codigo || feature?.properties?.categoria || '');
  return String(feature?.properties?.ecorregion_codigo || '');
}

function ecosystemFeatureName(feature, source) {
  if (source === 'usv') return String(feature?.properties?.categoria || feature?.properties?.codigo || '');
  return String(feature?.properties?.ecorregion_nombre || feature?.properties?.ecorregion_codigo || '');
}

function ecosystemColor(feature, source, index = 0) {
  const name = ecosystemFeatureName(feature, source);
  if (source === 'usv' && USV_COLORS[name]) return USV_COLORS[name];
  return hslColorForKey(ecosystemFeatureCode(feature, source), index);
}

function ecosystemStyle(feature, source) {
  const code = ecosystemFeatureCode(feature, source);
  const hasSelection = selectedEcosystemCodes.size > 0;
  const isSelected = selectedEcosystemCodes.has(code);
  const idx = ecosystemData[source]?.features?.indexOf(feature) ?? 0;
  const color = ecosystemColor(feature, source, idx);
  if (hasSelection && !isSelected) {
    return { color: 'transparent', weight: 0, fillColor: color, fillOpacity: 0 };
  }
  return {
    color: source === 'usv' ? 'rgba(255,255,255,.5)' : 'rgba(255,255,255,.65)',
    weight: source === 'usv' ? 0.45 : 0.8,
    fillColor: color,
    fillOpacity: hasSelection ? 0.56 : (source === 'usv' ? 0.42 : 0.34),
  };
}

async function ensureEcosystemLayer(source) {
  if (ecosystemLayers[source]) {
    if (!map.hasLayer(ecosystemLayers[source])) ecosystemLayers[source].addTo(map);
    applyEcosystemStyles();
    return;
  }
  const file = source === 'usv' ? 'geodata/uso_suelo_vegetacion_atlas.geojson' : 'geodata/ecorregiones_atlas.geojson';
  const res = await fetch(file);
  if (!res.ok) throw new Error(`No se pudo cargar ${file}`);
  const data = await res.json();
  ecosystemData[source] = data;
  const layer = L.geoJSON(data, {
    pane: 'atlasTerritoryPane',
    style: feature => ecosystemStyle(feature, source),
    onEachFeature: (feature, featureLayer) => {
      const code = ecosystemFeatureCode(feature, source);
      const name = ecosystemFeatureName(feature, source);
      featureLayer.bindTooltip(`<b>${esc(name)}</b>`, { className: 'leaflet-tooltip-atlas', sticky: true });
      featureLayer.on('click', () => selectEcosystem(code));
    },
  });
  ecosystemLayers[source] = layer;
  layer.addTo(map);
  renderEcosystemUnits();
}

function removeEcosystemLayers() {
  Object.values(ecosystemLayers).forEach(layer => {
    if (layer && map?.hasLayer(layer)) map.removeLayer(layer);
  });
}

function applyEcosystemStyles() {
  const layer = ecosystemLayers[activeEcosystemSource];
  if (!layer) return;
  layer.eachLayer(item => item.setStyle(ecosystemStyle(item.feature, activeEcosystemSource)));
}

function selectEcosystem(code) {
  const key = String(code);
  if (selectedEcosystemCodes.has(key)) selectedEcosystemCodes.delete(key);
  else selectedEcosystemCodes.add(key);
  applyEcosystemStyles();
  renderEcosystemUnits();
  refreshCommunities();
  updateLegend();

  if (selectedEcosystemCodes.size && ecosystemLayers[activeEcosystemSource]) {
    let bounds = null;
    ecosystemLayers[activeEcosystemSource].eachLayer(layer => {
      const layerCode = ecosystemFeatureCode(layer.feature, activeEcosystemSource);
      if (!selectedEcosystemCodes.has(layerCode)) return;
      try {
        const b = layer.getBounds();
        bounds = bounds ? bounds.extend(b) : b;
      } catch (e) {}
    });
    if (bounds?.isValid()) map.fitBounds(bounds, { padding: [26, 26], maxZoom: 8 });
  } else map.fitBounds(MX_BOUNDS);
}

async function setEcosystemSource(source) {
  if (!['ecorregiones', 'usv'].includes(source)) return;
  activeEcosystemSource = source;
  selectedEcosystemCodes.clear();
  document.querySelectorAll('[data-ecosystem-source]').forEach(btn => btn.classList.toggle('active', btn.dataset.ecosystemSource === source));
  removeEcosystemLayers();
  await ensureEcosystemLayer(source);
  renderEcosystemUnits();
  refreshCommunities();
  updateLegend();
  map.fitBounds(MX_BOUNDS);
}

// ---------- Materials ----------
const MATERIAL_FILTER_GROUPS = [
  { name: 'Fibras e hilos', hubs: ['Fibras vegetales', 'Fibras animales', 'Hilos e hilazas'] },
  { name: 'Telas y soportes', hubs: ['Telas y soportes'] },
  { name: 'Tintes y colorantes', hubs: ['Tintes naturales'] },
  { name: 'Telares y bastidores', hubs: ['Telares y bastidores'] },
  { name: 'Herramientas y maquinaria', hubs: ['Herramientas', 'Máquinas'] },
  { name: 'Abalorios y aplicaciones', hubs: ['Abalorios y aplicaciones'] },
  { name: 'Pieles y cueros', hubs: ['Pieles y cueros'] },
  { name: 'Otros materiales', hubs: ['Otros'] },
];

function generalMaterialCategory(materialName) {
  const hub = typeof clasificarMaterial === 'function' ? clasificarMaterial(materialName) : null;
  const group = MATERIAL_FILTER_GROUPS.find(item => item.hubs.includes(hub));
  return group?.name || 'Otros materiales';
}

function renderMaterialChips(driver) {
  const wrap = document.getElementById(`map-${driver}-material-chips`);
  if (!wrap) return;
  const items = Object.values(materialMeta).sort((a, b) => a.order - b.order);
  wrap.innerHTML = '';
  items.forEach((item, i) => {
    const row = document.createElement('div');
    row.className = 'filter-checkbox-item';
    const inputId = `map-${driver}-material-${i}`;
    row.innerHTML = `<input type="checkbox" id="${inputId}" ${selectedMaterials[driver].has(item.name) ? 'checked' : ''}>
      <span class="map-chip-swatch material-filter-swatch"></span>
      <label for="${inputId}" class="filter-checkbox-name">${esc(item.name)}</label>
      <span class="filter-checkbox-meta">${item.materials.size}</span>`;
    row.title = `${item.materials.size} material${item.materials.size !== 1 ? 'es' : ''} · ${item.techniques.size} técnica${item.techniques.size !== 1 ? 's' : ''}`;
    row.querySelector('input').addEventListener('change', () => {
      dismissInitialMapOverview?.();
      if (selectedMaterials[driver].has(item.name)) selectedMaterials[driver].delete(item.name);
      else selectedMaterials[driver].add(item.name);
      renderMaterialChips(driver);
      if (driver === 'estado') applyStateCompositeFilter();
      else refreshCommunities();
    });
    wrap.appendChild(row);
  });
  const summary = document.getElementById(`map-${driver}-material-summary`);
  if (summary) summary.textContent = selectedMaterials[driver].size
    ? (selectedMaterials[driver].size === 1 ? [...selectedMaterials[driver]][0] : `${selectedMaterials[driver].size} seleccionadas`)
    : 'Todas las categorías';
}

function bindMaterialSearch(driver) {
  const el = document.getElementById(`map-${driver}-material-search`);
  if (!el) return;
  el.addEventListener('input', () => {
    materialSearchText[driver] = el.value || '';
    renderMaterialChips(driver);
  });
}

// ---------- Language UI ----------
function renderLanguageChips() {
  const wrap = document.getElementById('map-language-chips');
  if (!wrap) return;
  const q = normalizeDriverText(languageSearchText);
  const items = languageCloudData.filter(item => !q || normalizeDriverText(`${item.name} ${item.pueblo}`).includes(q));
  wrap.innerHTML = '';
  items.forEach((item, i) => {
    const row = document.createElement('div');
    row.className = 'filter-checkbox-item';
    const inputId = `map-language-${i}`;
    row.innerHTML = `<input type="checkbox" id="${inputId}" ${selectedLanguageIds.has(item.id) ? 'checked' : ''}>
      <span class="map-chip-swatch" style="background:${item.color}"></span>
      <label for="${inputId}" class="filter-checkbox-name">${esc(item.name)}</label>
      <span class="filter-checkbox-meta">${item.point_count.toLocaleString('es-MX')}</span>`;
    row.title = `${item.point_count.toLocaleString('es-MX')} localidades en la capa`;
    row.querySelector('input').addEventListener('change', () => selectLanguage(item.id));
    wrap.appendChild(row);
  });
  const summary = document.getElementById('map-language-summary');
  if (summary) summary.textContent = selectedLanguageIds.size
    ? (selectedLanguageIds.size === 1 ? (languageCloudData.find(x => selectedLanguageIds.has(x.id))?.name || '1 seleccionada') : `${selectedLanguageIds.size} seleccionadas`)
    : 'Todas';
}

function selectLanguage(id) {
  dismissInitialMapOverview?.();
  if (selectedLanguageIds.has(id)) selectedLanguageIds.delete(id); else selectedLanguageIds.add(id);
  renderLanguageChips();
  ensureLanguageLayer();
  refreshCommunities();
  updateLegend();
  const selected = languageCloudData.filter(x => selectedLanguageIds.has(x.id));
  if (selected.length) {
    const points = selected.flatMap(language => language.coords.map(([lon, lat]) => [lat, lon]));
    const bounds = L.latLngBounds(points);
    if (bounds.isValid()) map.fitBounds(bounds, { padding: [28, 28], maxZoom: 8 });
  } else map.fitBounds(MX_BOUNDS);
}

// ---------- Ecosystem UI ----------
function renderEcosystemUnits() {
  const wrap = document.getElementById('map-ecosystem-unit-chips');
  const summary = document.getElementById('map-ecosystem-summary');
  if (!wrap) return;
  const data = ecosystemData[activeEcosystemSource];
  wrap.innerHTML = '';
  if (!data) return;
  const items = [...data.features].sort((a, b) => ecosystemFeatureName(a, activeEcosystemSource).localeCompare(ecosystemFeatureName(b, activeEcosystemSource), 'es'));
  items.forEach((feature, i) => {
    const code = ecosystemFeatureCode(feature, activeEcosystemSource);
    const name = ecosystemFeatureName(feature, activeEcosystemSource);
    const row = document.createElement('div');
    row.className = 'filter-checkbox-item';
    const inputId = `map-ecosystem-${i}`;
    row.innerHTML = `<input type="checkbox" id="${inputId}" ${selectedEcosystemCodes.has(code) ? 'checked' : ''}>
      <span class="map-chip-swatch" style="background:${ecosystemColor(feature, activeEcosystemSource, i)}"></span>
      <label for="${inputId}" class="filter-checkbox-name">${esc(name)}</label>`;
    row.querySelector('input').addEventListener('change', () => selectEcosystem(code));
    wrap.appendChild(row);
  });
  if (summary) {
    const n = selectedEcosystemCodes.size;
    if (!n) summary.textContent = 'Todas';
    else if (n === 1) {
      const feature = items.find(f => selectedEcosystemCodes.has(ecosystemFeatureCode(f, activeEcosystemSource)));
      const name = feature ? ecosystemFeatureName(feature, activeEcosystemSource) : '1 seleccionada';
      summary.textContent = name.length > 26 ? `${name.slice(0,24)}…` : name;
    } else summary.textContent = `${n} seleccionadas`;
  }
}

// ---------- State composite filter ----------
function stateHasBaseFilter() {
  return typeof mapFilterHasSelection === 'function' ? mapFilterHasSelection() : false;
}

function applyStateCompositeFilter() {
  if (!ATLAS) return;
  let names = new Set(getFilteredTecnicas().map(t => t.tecnica));
  const materialSet = materialTechniqueSet('estado');
  if (materialSet) names = new Set([...names].filter(t => materialSet.has(t)));
  const hasFilter = stateHasBaseFilter() || selectedMaterials.estado.size > 0;
  mapFilteredTecnicas = hasFilter ? names : null;

  if (geojsonLayer) {
    geojsonLayer.eachLayer(layer => {
      const name = layer.feature?.properties?.name || layer.feature?.properties?.NAME_1 || '';
      layer.setStyle(mapFilteredTecnicas ? filteredStateStyle(name) : stateStyle(layer.feature));
      layer.unbindTooltip();
      const count = mapFilteredTecnicas ? countForEstadoFiltered(name) : countForEstado(name);
      if (count > 0) layer.bindTooltip(`<b>${esc(name)}</b><br>${count} técnica${count !== 1 ? 's' : ''}`, { className: 'leaflet-tooltip-atlas', direction: 'top', sticky: true });
    });
  }

  const activeEl = document.getElementById('map-filter-active');
  const activeText = document.getElementById('map-filter-active-text');
  if (activeEl) activeEl.style.display = hasFilter ? '' : 'none';
  if (activeText && hasFilter) {
    const parts = typeof mapFilterSummaryParts === 'function' ? mapFilterSummaryParts() : [];
    if (selectedMaterials.estado.size) parts.push(`${selectedMaterials.estado.size} categoría${selectedMaterials.estado.size !== 1 ? 's' : ''} de materiales`);
    activeText.textContent = `${parts.join(' · ')} · ${names.size} técnica${names.size !== 1 ? 's' : ''}`;
  }

  selectedStateName = null;
  selectedMunicipioKey = null;
  if (activeMapDriver === 'estado') {
    updateStateSelectionStyles();
    renderDriverList();
    refreshCommunities();
    updateStatePanel();
  }
}

function syncSharedTechniqueFilter() {
  const filtered = getFilteredTecnicas();
  const hasFilter = mapFilterHasSelection();
  mapFilteredTecnicas = hasFilter ? new Set(filtered.map(t => t.tecnica)) : null;
  const activeEl = document.getElementById('map-filter-active');
  const activeText = document.getElementById('map-filter-active-text');
  if (activeEl) activeEl.style.display = hasFilter ? '' : 'none';
  if (activeText && hasFilter) activeText.textContent = `${mapFilterSummaryParts().join(' · ')} · ${filtered.length} técnica${filtered.length !== 1 ? 's' : ''}`;
}

applyMapFilter = function() {
  syncSharedTechniqueFilter();
  if (activeMapDriver === 'estado') {
    applyStateCompositeFilter();
  } else {
    refreshCommunities();
    updateLegend();
  }
};

// ---------- Side panel / lists ----------
function isStateSelected(name) {
  return [...selectedStateNames].some(item => normalizeEstado(item) === normalizeEstado(name));
}

function baseStateStyleFor(name, feature) {
  return mapFilteredTecnicas ? filteredStateStyle(name) : stateStyle(feature);
}

function updateStateSelectionStyles() {
  if (!geojsonLayer || activeMapDriver !== 'estado') return;
  geojsonLayer.eachLayer(layer => {
    const name = layer.feature?.properties?.name || layer.feature?.properties?.NAME_1 || '';
    const base = baseStateStyleFor(name, layer.feature);
    layer.setStyle(isStateSelected(name)
      ? { ...base, weight: 3, color: COLORS.negro, fillOpacity: Math.max(base.fillOpacity || 0, 0.92) }
      : base);
  });
}

function selectedStateTechniques() {
  const names = new Set();
  selectedStateNames.forEach(state => {
    getTecnicasForEstado(state).forEach(t => {
      if (!mapFilteredTecnicas || mapFilteredTecnicas.has(t)) names.add(t);
    });
  });
  const materialSet = materialTechniqueSet('estado');
  let out = [...names];
  if (materialSet) out = out.filter(t => materialSet.has(t));
  return out.sort((a,b)=>a.localeCompare(b,'es'));
}

function updateStatePanel() {
  if (activeMapDriver !== 'estado') return;
  if (typeof initialMapOverviewActive !== 'undefined' && initialMapOverviewActive) return;
  const title = document.getElementById('panel-title');
  const subtitle = document.getElementById('panel-subtitle');
  const body = document.getElementById('panel-body');
  if (selectedStateNames.size) {
    const techniques = selectedStateTechniques();
    if (title) title.textContent = selectedStateNames.size === 1 ? [...selectedStateNames][0] : `${selectedStateNames.size} estados seleccionados`;
    if (subtitle) subtitle.textContent = `${techniques.length} técnica${techniques.length !== 1 ? 's' : ''} en la selección territorial`;
    renderSidePanel(techniques);
    return;
  }
  const hasFilter = stateHasBaseFilter() || selectedMaterials.estado.size > 0;
  const techniques = hasFilter ? getFilteredTecnicas().filter(t => {
    const materialSet = materialTechniqueSet('estado');
    return !materialSet || materialSet.has(t.tecnica);
  }).map(t=>t.tecnica) : [];
  if (title) title.textContent = 'Estados de la República';
  if (subtitle) subtitle.textContent = hasFilter
    ? `${techniques.length} técnica${techniques.length !== 1 ? 's' : ''} coincide${techniques.length !== 1 ? 'n' : ''} con los filtros`
    : 'Selecciona uno o varios estados';
  if (body) body.innerHTML = `<div class="welcome-state"><h3>${hasFilter ? 'Filtro activo' : 'Explora el mapa'}</h3><p>${hasFilter ? 'Ahora puedes seleccionar varios estados para limitar territorialmente estos resultados.' : 'Selecciona uno o varios estados en la lista o directamente sobre el mapa. Las selecciones se combinarán.'}</p></div>`;
}

function renderStateFilterDropdown() {
  const wrap = document.getElementById('map-state-checkboxes');
  const summary = document.getElementById('map-state-summary');
  if (!wrap || !geojsonLayer) return;
  const names = [];
  geojsonLayer.eachLayer(layer => {
    const name = layer.feature?.properties?.name || layer.feature?.properties?.NAME_1 || '';
    if (name) names.push(name);
  });
  const ordered = [...new Set(names)].sort((a,b)=>a.localeCompare(b,'es'));
  wrap.innerHTML = '';
  ordered.forEach((name, i) => {
    const count = mapFilteredTecnicas ? countForEstadoFiltered(name) : countForEstado(name);
    const checked = isStateSelected(name);
    const row = document.createElement('div');
    row.className = 'filter-checkbox-item' + (count === 0 && !checked ? ' disabled' : '');
    const inputId = `map-state-filter-${i}`;
    row.innerHTML = `<input type="checkbox" id="${inputId}" ${checked ? 'checked' : ''} ${count === 0 && !checked ? 'disabled' : ''}>
      <span class="map-chip-swatch state-filter-swatch" style="background:${getMapColor(count)}"></span>
      <label for="${inputId}" class="filter-checkbox-name">${esc(name)}</label>
      <span class="filter-checkbox-meta">${count}</span>`;
    row.querySelector('input').addEventListener('change', () => {
      const layer = findLayerByStateName(name);
      if (layer) onStateClick(name, layer, layer.feature);
    });
    wrap.appendChild(row);
  });
  if (summary) summary.textContent = selectedStateNames.size
    ? (selectedStateNames.size === 1 ? [...selectedStateNames][0] : `${selectedStateNames.size} seleccionados`)
    : 'Todos';
}

function renderDriverList() {
  const label = document.getElementById('driver-list-label');
  const wrap = document.getElementById('states-list');
  if (!wrap) return;
  if (activeMapDriver === 'estado') {
    renderStateFilterDropdown();
    if (label) label.textContent = selectedStateNames.size ? `Estados · ${selectedStateNames.size} seleccionados` : 'Estados';
    const names = [];
    geojsonLayer?.eachLayer(l => {
      const n = l.feature?.properties?.name || l.feature?.properties?.NAME_1 || '';
      if (n) names.push(n);
    });
    const ordered = [...new Set(names)].sort((a,b)=>a.localeCompare(b,'es'));
    wrap.innerHTML = '';
    ordered.forEach(name => {
      const count = mapFilteredTecnicas ? countForEstadoFiltered(name) : countForEstado(name);
      const btn = document.createElement('button');
      btn.className = 'state-item' + (isStateSelected(name) ? ' active' : '') + (count === 0 ? ' disabled' : '');
      btn.innerHTML = `<span>${esc(name)}</span><span class="state-count">${count}</span>`;
      btn.addEventListener('click', () => {
        if (count === 0) return;
        const layer = findLayerByStateName(name);
        if (layer) onStateClick(name, layer, layer.feature);
      });
      wrap.appendChild(btn);
    });
    return;
  }

  if (label) label.textContent = 'Comunidades';
  const layers = visibleCommunityLayers();
  wrap.innerHTML = '';
  layers.slice(0, 250).forEach(layer => {
    const p = layer.feature?.properties || {};
    const btn = document.createElement('button');
    btn.className = 'state-item';
    let communityCountTechniques = communityTechniques(layer.feature);
    if (mapFilteredTecnicas) communityCountTechniques = communityCountTechniques.filter(t => mapFilteredTecnicas.has(t));
    const activeMaterialSet = materialTechniqueSet(activeMapDriver);
    if (activeMaterialSet) communityCountTechniques = communityCountTechniques.filter(t => activeMaterialSet.has(t));
    btn.innerHTML = `<span>${esc(p.NOMGEO || '')}</span><span class="state-count">${communityCountTechniques.length}</span>`;
    btn.title = p.estado_nombre || '';
    btn.addEventListener('click', () => {
      const ll = layer.getLatLng?.();
      if (ll) map.setView(ll, Math.max(map.getZoom(), 8));
      onMunicipioClick(p.estado_nombre || '', p.NOMGEO || '', layer);
    });
    wrap.appendChild(btn);
  });
  if (layers.length > 250) {
    const note = document.createElement('div');
    note.className = 'driver-list-note';
    note.textContent = `Mostrando las primeras 250 de ${layers.length} comunidades.`;
    wrap.appendChild(note);
  }
}

renderStatesList = function() { return renderDriverList(); };

function updateTerritoryPanel() {
  if (!driverReady) return;
  if (activeMapDriver === 'estado') { updateStatePanel(); return; }
  renderDriverList();
  const techniques = visibleTechniqueNames(activeMapDriver);
  const communities = visibleCommunityLayers().length;
  const title = document.getElementById('panel-title');
  const subtitle = document.getElementById('panel-subtitle');

  if (activeMapDriver === 'lengua') {
    const languages = languageCloudData.filter(x => selectedLanguageIds.has(x.id));
    if (title) title.textContent = languages.length === 1 ? languages[0].name : languages.length > 1 ? `${languages.length} lenguas seleccionadas` : 'Lenguas indígenas';
    if (subtitle) subtitle.textContent = languages.length
      ? `${communities} comunidad${communities !== 1 ? 'es' : ''} dentro de las nubes seleccionadas · ${techniques.length} técnica${techniques.length !== 1 ? 's' : ''}`
      : `${languageCloudData.length} lenguas en la capa · puedes seleccionar varias`;
  } else {
    const features = selectedEcosystemFeatures();
    if (title) title.textContent = features.length === 1
      ? ecosystemFeatureName(features[0], activeEcosystemSource)
      : features.length > 1 ? `${features.length} unidades territoriales` : (activeEcosystemSource === 'usv' ? 'Uso del suelo y vegetación' : 'Ecorregiones');
    if (subtitle) subtitle.textContent = features.length
      ? `${communities} comunidad${communities !== 1 ? 'es' : ''} dentro del área seleccionada · ${techniques.length} técnica${techniques.length !== 1 ? 's' : ''}`
      : 'Selecciona una o varias unidades territoriales para filtrar las comunidades';
  }
  renderSidePanel(techniques);
}

onStateClick = function(name, layer, feat) {
  if (activeMapDriver !== 'estado') return;
  dismissInitialMapOverview?.();
  const existing = [...selectedStateNames].find(item => normalizeEstado(item) === normalizeEstado(name));
  if (existing) selectedStateNames.delete(existing); else selectedStateNames.add(name);
  selectedStateName = null;
  selectedMunicipioKey = null;
  updateStateSelectionStyles();
  renderDriverList();
  refreshCommunities();
  updateStatePanel();

  if (selectedStateNames.size) {
    let bounds = null;
    geojsonLayer?.eachLayer(item => {
      const itemName = item.feature?.properties?.name || item.feature?.properties?.NAME_1 || '';
      if (!isStateSelected(itemName)) return;
      try { const b=item.getBounds(); bounds=bounds?bounds.extend(b):b; } catch(e) {}
    });
    if (bounds?.isValid()) map.fitBounds(bounds,{padding:[24,24],maxZoom:7});
  } else map.fitBounds(MX_BOUNDS);
};

// ---------- Legend ----------
function updateLegend() {
  const title = document.getElementById('map-legend-title');
  const body = document.getElementById('map-legend-body');
  if (!title || !body) return;
  if (activeMapDriver === 'estado') {
    title.textContent = 'Técnicas por estado';
    body.innerHTML = `
      <div class="legend-item"><div class="legend-dot" style="background:#FCBC52"></div><span>20 o más</span></div>
      <div class="legend-item"><div class="legend-dot" style="background:#A4D984"></div><span>10 – 19</span></div>
      <div class="legend-item"><div class="legend-dot" style="background:#F588AF"></div><span>1 – 9</span></div>
      <div class="map-legend-note">Puedes seleccionar varios estados. Las comunidades permanecen en primer plano.</div>`;
  } else if (activeMapDriver === 'lengua') {
    const languages = languageCloudData.filter(x => selectedLanguageIds.has(x.id));
    title.textContent = languages.length === 1 ? languages[0].name : languages.length > 1 ? `${languages.length} lenguas` : 'Lenguas indígenas';
    if (languages.length) {
      const shown = languages.slice(0,5).map(language => `<div class="legend-item"><div class="legend-dot" style="background:${language.color}"></div><span>${esc(language.name)}</span></div>`).join('');
      body.innerHTML = `${shown}${languages.length>5?`<div class="map-legend-note">+ ${languages.length-5} lengua${languages.length-5!==1?'s':''} seleccionada${languages.length-5!==1?'s':''}</div>`:''}<div class="map-legend-note">Las comunidades se incluyen si caen dentro de la presencia aproximada de cualquiera de las lenguas seleccionadas.</div>`;
    } else {
      body.innerHTML = `<div class="map-legend-note">${languageCloudData.length} lenguas específicas se representan con colores únicos. Puedes seleccionar varias a la vez.</div>`;
    }
  } else {
    title.textContent = activeEcosystemSource === 'usv' ? 'Uso del suelo y vegetación' : 'Ecorregiones';
    body.innerHTML = `<div class="map-legend-note">Selecciona una o varias unidades desde el menú desplegable. Solo se mostrarán sus territorios y las comunidades ubicadas dentro de ellos.</div><div class="map-legend-note">Las categorías de materiales filtran las comunidades, no la capa ecológica.</div>`;
  }
}

// ---------- Driver switching ----------
function styleStateContext() {
  if (!geojsonLayer) return;
  geojsonLayer.eachLayer(layer => {
    layer.setStyle(CONTEXT_STATE_STYLE);
    layer.closeTooltip?.();
    layer.unbindTooltip?.();
  });
}

async function setMapDriver(driver) {
  if (!['estado', 'lengua', 'ecosistema'].includes(driver)) return;
  dismissInitialMapOverview?.();
  activeMapDriver = driver;
  selectedStateName = null;
  selectedMunicipioKey = null;
  document.querySelectorAll('[data-map-driver]').forEach(btn => btn.classList.toggle('active', btn.dataset.mapDriver === driver));
  document.querySelectorAll('.map-driver-filter').forEach(panel => panel.classList.remove('active'));
  document.getElementById(`map-filter-driver-${driver}`)?.classList.add('active');
  const title = document.getElementById('map-filter-title');
  if (title) title.textContent = 'Explorar';

  removeLanguageLayer();
  removeEcosystemLayers();

  if (driver === 'estado') {
    applyStateCompositeFilter();
    updateStateSelectionStyles();
    renderDriverList();
    updateStatePanel();
  } else if (driver === 'lengua') {
    syncSharedTechniqueFilter();
    styleStateContext();
    ensureLanguageLayer();
    renderLanguageChips();
    renderMaterialChips('lengua');
  } else {
    syncSharedTechniqueFilter();
    styleStateContext();
    await ensureEcosystemLayer(activeEcosystemSource);
    renderEcosystemUnits();
    renderMaterialChips('ecosistema');
  }
  refreshCommunities();
  updateLegend();
  map.fitBounds(MX_BOUNDS);
}

clearMapFilter = function() {
  dismissInitialMapOverview?.();

  // Los filtros de clasificación/técnica son comunes a los tres enfoques.
  mapFilterState = { cat1: new Set(), cat2: new Set(), cat3: new Set(), cat4: new Set(), tecnica: new Set() };
  const techSearch = document.getElementById('map-filter-search');
  if (techSearch) techSearch.value = '';
  rebuildMapFilterCascade();
  mapFilteredTecnicas = null;

  if (activeMapDriver === 'estado') {
    selectedStateNames.clear();
    selectedMaterials.estado.clear();
    renderMaterialChips('estado');
    applyStateCompositeFilter();
    updateStateSelectionStyles();
    renderDriverList();
    updateStatePanel();
  } else if (activeMapDriver === 'lengua') {
    selectedLanguageIds.clear();
    selectedMaterials.lengua.clear();
    languageSearchText = '';
    const ls = document.getElementById('map-language-search'); if (ls) ls.value = '';
    renderLanguageChips();
    renderMaterialChips('lengua');
    ensureLanguageLayer();
    refreshCommunities();
  } else {
    selectedEcosystemCodes.clear();
    selectedMaterials.ecosistema.clear();
    applyEcosystemStyles();
    renderEcosystemUnits();
    renderMaterialChips('ecosistema');
    refreshCommunities();
  }
  updateLegend();
  map.fitBounds(MX_BOUNDS);
};

// ---------- Data loading ----------
async function loadDriverData() {
  try {
    const [languageRes, materialRes] = await Promise.all([
      fetch('geodata/lenguas_cloud.json'),
      fetch('data/catalogo_materiales_atlas.csv'),
    ]);
    if (!languageRes.ok || !materialRes.ok) throw new Error('No se pudieron cargar los datos territoriales.');
    const langPayload = await languageRes.json();
    languageCloudData = langPayload.languages || [];
    const materialRows = parseCSV(await materialRes.text());
    materialMeta = {};
    MATERIAL_FILTER_GROUPS.forEach((group, order) => {
      materialMeta[group.name] = { name: group.name, order, materials: new Set(), techniques: new Set() };
    });
    materialRows.forEach(row => {
      const materialName = String(row.material_nombre_atlas || '').trim();
      if (!materialName) return;
      const category = generalMaterialCategory(materialName);
      const item = materialMeta[category] || materialMeta['Otros materiales'];
      item.materials.add(materialName);
      splitSemicolon(row.tecnicas_publicadas).filter(t => tecnicasMap[t]).forEach(t => item.techniques.add(t));
    });
    driverReady = true;
    ['estado', 'lengua', 'ecosistema'].forEach(renderMaterialChips);
    renderLanguageChips();
    updateLegend();
    refreshCommunities();
  } catch (error) {
    console.error('Error cargando drivers territoriales:', error);
  }
}

let mapDriversEnhancementsStarted = false;
function initMapDriverEnhancements() {
  if (mapDriversEnhancementsStarted) return;
  mapDriversEnhancementsStarted = true;

  document.querySelectorAll('[data-map-driver]').forEach(btn => btn.addEventListener('click', () => setMapDriver(btn.dataset.mapDriver)));
  document.querySelectorAll('[data-ecosystem-source]').forEach(btn => btn.addEventListener('click', () => setEcosystemSource(btn.dataset.ecosystemSource)));
  const languageSearch = document.getElementById('map-language-search');
  if (languageSearch) languageSearch.addEventListener('input', () => { languageSearchText = languageSearch.value || ''; renderLanguageChips(); });
  ['estado', 'lengua', 'ecosistema'].forEach(bindMaterialSearch);

  // Comunidades: capa principal, siempre visible.
  Promise.resolve(toggleComunidadesLayer(true)).then(() => {
    refreshCommunities();
    renderDriverList();
  }).catch(error => console.error('Error preparando la capa de comunidades:', error));

  loadDriverData().catch?.(error => console.error('Error inicializando perfiles territoriales:', error));
}

initMapFilter = function() {
  _atlasInitMapFilter();
  initMapDriverEnhancements();
};

/* ═══════════════════════════════════════════════════════════════
   V11 — PANEL DERECHO: PERFILES TERRITORIALES TESTIMONIALES
   Todos los controles/filtros permanecen en la columna izquierda.
   El panel derecho se reserva para una lectura narrativa de los
   datos derivados de los testimonios del Atlas.
   ═══════════════════════════════════════════════════════════════ */
let territoryProfileData = null;
let territoryCarouselPreviewType = null; // solo al cargar: puede mostrar un territorio de cualquier enfoque
let territoryCarouselIndex = 0;
let territoryCarouselSignature = '';
let territoryCarouselTimer = null;

function territoryProfileType() {
  if (activeMapDriver === 'estado') return 'estado';
  if (activeMapDriver === 'lengua') return 'lengua';
  return activeEcosystemSource === 'usv' ? 'uso_suelo' : 'ecorregion';
}

function territoryTypeLabel(type) {
  return ({
    estado: 'Estado',
    lengua: 'Lengua',
    ecorregion: 'Ecorregión',
    uso_suelo: 'Uso del suelo y vegetación',
  })[type] || 'Territorio';
}

function territoryTypePluralLabel(type) {
  return ({
    estado: 'Estados',
    lengua: 'Lenguas',
    ecorregion: 'Ecorregiones',
    uso_suelo: 'Usos del suelo y vegetación',
  })[type] || 'Territorios';
}

function clearTerritoryCarouselPreview() {
  territoryCarouselPreviewType = null;
}

function findStateProfile(name) {
  const items = Object.values(territoryProfileData?.estado || {});
  return items.find(item => normalizeEstado(item.name) === normalizeEstado(name)) || null;
}

function hasTechniqueRecords(profile) {
  return Number(profile?.summary?.n_registros || 0) > 0;
}

function selectedTerritoryProfiles() {
  if (!territoryProfileData) return [];
  const type = territoryCarouselPreviewType || territoryProfileType();
  let items = [];

  if (type === 'estado') {
    items = selectedStateNames.size
      ? [...selectedStateNames].map(findStateProfile).filter(Boolean)
      : Object.values(territoryProfileData.estado || {});
  } else if (type === 'lengua') {
    items = selectedLanguageIds.size
      ? [...selectedLanguageIds].map(id => territoryProfileData.lengua?.[String(id)]).filter(Boolean)
      : Object.values(territoryProfileData.lengua || {});
  } else {
    const bucket = territoryProfileData[type] || {};
    items = selectedEcosystemCodes.size
      ? [...selectedEcosystemCodes].map(code => bucket[String(code)]).filter(Boolean)
      : Object.values(bucket);
  }

  return items
    .filter(hasTechniqueRecords)
    .sort((a, b) => String(a.name || '').localeCompare(String(b.name || ''), 'es'));
}

function compactList(values, maxItems = 6) {
  const items = Array.isArray(values) ? values.filter(Boolean) : [];
  if (!items.length) return '';
  const shown = items.slice(0, maxItems).map(v => `<span class="territory-profile-chip">${esc(v)}</span>`).join('');
  const more = items.length > maxItems ? `<span class="territory-profile-more">+${items.length - maxItems}</span>` : '';
  return `<div class="territory-profile-chips">${shown}${more}</div>`;
}

function principalTechniqueItems(profile, maxItems = 6) {
  const raw = profile?.summary?.tecnicas_principales || '';
  const items = String(raw).split(';').map(x => x.trim()).filter(Boolean).slice(0, maxItems);
  if (!items.length) return '';
  return `<div class="territory-tech-list">${items.map(item => {
    const match = item.match(/^(.*?)(?:\s+\((\d+)\))?$/);
    const name = (match?.[1] || item).trim();
    const count = match?.[2] || '';
    const clickable = Boolean(tecnicasMap?.[name]);
    return `<button class="territory-tech-item${clickable ? '' : ' static'}" ${clickable ? `onclick="openFicha('${escJs(name)}')" title="Ver ficha de la técnica"` : ''}>
      <span>${esc(name)}</span>${count ? `<strong>${esc(count)}</strong>` : ''}
    </button>`;
  }).join('')}</div>`;
}

function topMentions(obj, maxItems = 3) {
  return Object.entries(obj || {})
    .filter(([, value]) => Number(value) > 0)
    .sort((a, b) => Number(b[1]) - Number(a[1]) || String(a[0]).localeCompare(String(b[0]), 'es'))
    .slice(0, maxItems)
    .map(([name, value]) => `${name} (${value})`);
}

function territoryInterpretationNote(type) {
  if (type === 'estado') {
    return 'Este perfil reúne los registros de técnicas vinculados con este estado dentro del Atlas.';
  }
  if (type === 'lengua') {
    return 'Aquí se reúnen los registros de técnicas de comunidades ubicadas dentro del área de presencia representada para esta lengua. Es una lectura territorial del Atlas, no una afirmación de que las técnicas pertenezcan exclusivamente a una lengua.';
  }
  if (type === 'ecorregion') {
    return 'Aquí se reúnen los registros de técnicas de comunidades ubicadas dentro de esta ecorregión.';
  }
  return 'Aquí se reúnen los registros de técnicas de comunidades ubicadas dentro de esta categoría de uso del suelo y vegetación.';
}

function territoryProfileCard(profile, type) {
  const s = profile?.summary || {};
  const registros = Number(s.n_registros || 0);
  const comunidades = Number(s.n_comunidades || 0);
  const tecnicas = Number(s.n_tecnicas || 0);
  const learning = topMentions(s.aprendizaje_menciones);
  const teaching = topMentions(s.ensenanza_menciones);
  const languages = s.lenguas_mencionadas || [];
  const states = s.estados || [];
  const municipios = s.municipios_reportados || [];

  const empty = registros === 0 ? `
    <div class="territory-profile-empty">
      <strong>Sin registros de técnicas en la versión actual del Atlas</strong>
      <p>Esta ausencia no significa que no existan prácticas textiles en el territorio; indica únicamente que la versión actual del Atlas no contiene registros de técnicas ubicados aquí.</p>
    </div>` : '';

  return `
    <article class="territory-profile-card">
      <div class="territory-profile-kicker">Perfil del Atlas · ${esc(territoryTypeLabel(type))}</div>
      <h3>${esc(profile.name || '')}</h3>
      ${type === 'lengua' && profile.pueblo ? `<div class="territory-profile-pueblo">Pueblo de referencia: ${esc(profile.pueblo)}</div>` : ''}

      <div class="territory-profile-stats">
        <div><strong>${registros.toLocaleString('es-MX')}</strong><span>registros de técnicas</span></div>
        <div><strong>${comunidades.toLocaleString('es-MX')}</strong><span>comunidades</span></div>
        <div><strong>${tecnicas.toLocaleString('es-MX')}</strong><span>técnicas documentadas</span></div>
      </div>

      <p class="territory-profile-method">${esc(territoryInterpretationNote(type))}</p>
      ${empty}

      ${registros && s.tecnicas_principales ? `<section class="territory-profile-section"><h4>Técnicas más registradas</h4>${principalTechniqueItems(profile)}</section>` : ''}
      ${registros && (s.categorias_material || []).length ? `<section class="territory-profile-section"><h4>Familias de materiales registradas</h4>${compactList(s.categorias_material, 8)}</section>` : ''}
      ${registros && languages.length ? `<section class="territory-profile-section"><h4>Lenguas mencionadas en los registros</h4>${compactList(languages, 8)}</section>` : ''}
      ${registros && (learning.length || teaching.length) ? `<section class="territory-profile-section territory-transmission"><h4>Transmisión del conocimiento mencionada</h4>
        ${learning.length ? `<p><b>Aprendizaje:</b> ${esc(learning.join(' · '))}</p>` : ''}
        ${teaching.length ? `<p><b>Enseñanza:</b> ${esc(teaching.join(' · '))}</p>` : ''}
      </section>` : ''}
      ${registros && states.length && type !== 'estado' ? `<section class="territory-profile-section"><h4>Estados presentes en estos registros</h4>${compactList(states, 6)}</section>` : ''}
      ${registros && municipios.length ? `<section class="territory-profile-section"><h4>Comunidades documentadas</h4>${compactList(municipios, 8)}</section>` : ''}

      <div class="territory-profile-source">Fuente: registros de técnicas compartidos por artesanas y artesanos participantes en Original.</div>
    </article>`;
}

function stopTerritoryCarousel() {
  if (territoryCarouselTimer) clearTimeout(territoryCarouselTimer);
  territoryCarouselTimer = null;
}

function scheduleTerritoryCarousel(total) {
  stopTerritoryCarousel();
  if (total <= 1) return;
  territoryCarouselTimer = setTimeout(() => territoryCarouselGo(1), 9000);
}

function territoryCarouselGo(delta) {
  const items = selectedTerritoryProfiles();
  if (!items.length) return;
  territoryCarouselIndex = (territoryCarouselIndex + delta + items.length) % items.length;
  renderTerritoryProfileCarousel({ keepIndex: true });
}

function territoryCarouselTo(index) {
  territoryCarouselIndex = Number(index) || 0;
  renderTerritoryProfileCarousel({ keepIndex: true });
}

function renderTerritoryProfileCarousel(options = {}) {
  if (typeof initialMapOverviewActive !== 'undefined' && initialMapOverviewActive) return;
  const body = document.getElementById('panel-body');
  const title = document.getElementById('panel-title');
  const subtitle = document.getElementById('panel-subtitle');
  if (!body || !title || !subtitle) return;

  if (!territoryProfileData) {
    title.textContent = 'Perfiles del Atlas';
    subtitle.textContent = '';
    body.innerHTML = '<div class="welcome-state"><h3>Cargando perfiles</h3><p>La información del panel se está preparando a partir de los registros de técnicas del Atlas.</p></div>';
    return;
  }

  const type = territoryCarouselPreviewType || territoryProfileType();
  const items = selectedTerritoryProfiles();
  const signature = `${type}|${items.map(x => x.id).join('|')}`;
  if (options.randomStart && items.length) {
    territoryCarouselIndex = Math.floor(Math.random() * items.length);
  } else if (!options.keepIndex || signature !== territoryCarouselSignature) {
    territoryCarouselIndex = 0;
  }
  territoryCarouselSignature = signature;
  territoryCarouselIndex = Math.max(0, Math.min(territoryCarouselIndex, Math.max(0, items.length - 1)));

  title.textContent = territoryTypePluralLabel(type);
  subtitle.textContent = '';

  if (!items.length) {
    body.innerHTML = '<div class="welcome-state"><h3>Sin registros de técnicas</h3><p>No hay perfiles con registros de técnicas para la selección actual.</p></div>';
    stopTerritoryCarousel();
    return;
  }

  const item = items[territoryCarouselIndex];
  const dots = items.length <= 12
    ? `<div class="territory-carousel-dots">${items.map((_, i) => `<button class="territory-carousel-dot${i === territoryCarouselIndex ? ' active' : ''}" onclick="territoryCarouselTo(${i})" aria-label="Ir al perfil ${i + 1}"></button>`).join('')}</div>`
    : '';

  body.innerHTML = `
    <div class="territory-carousel" onmouseenter="stopTerritoryCarousel()" onmouseleave="scheduleTerritoryCarousel(${items.length})">
      <div class="territory-carousel-toolbar">
        <button class="territory-carousel-btn" onclick="territoryCarouselGo(-1)" ${items.length <= 1 ? 'disabled' : ''} aria-label="Perfil anterior">‹</button>
        <span>${territoryCarouselIndex + 1} / ${items.length}</span>
        <button class="territory-carousel-btn" onclick="territoryCarouselGo(1)" ${items.length <= 1 ? 'disabled' : ''} aria-label="Perfil siguiente">›</button>
      </div>
      ${territoryProfileCard(item, type)}
      ${dots}
    </div>`;
  body.scrollTop = 0;
  scheduleTerritoryCarousel(items.length);
}

// El panel derecho deja de ser una segunda lista de filtros.
renderDriverList = function() {
  if (activeMapDriver === 'estado') renderStateFilterDropdown();
};
renderStatesList = function() { return renderDriverList(); };

// El panel derecho presenta siempre perfiles territoriales, también en Estado.
updateStatePanel = function() {
  if (activeMapDriver !== 'estado') return;
  clearTerritoryCarouselPreview();
  renderTerritoryProfileCarousel();
};

updateTerritoryPanel = function() {
  if (!driverReady) return;
  clearTerritoryCarouselPreview();
  renderDriverList();
  renderTerritoryProfileCarousel();
};

// Al hacer clic en una comunidad, se conserva el carrusel territorial.
// La comunidad se identifica en un popup del propio mapa.
onMunicipioClick = function(estado, municipio, layer) {
  dismissInitialMapOverview?.();
  clearTerritoryCarouselPreview();
  selectedMunicipioKey = municipioKey(estado, municipio);
  if (municipiosLayer) municipiosLayer.eachLayer(l => l.setStyle(COMUNIDAD_STYLE));
  layer.setStyle(COMUNIDAD_STYLE_SELECTED);
  let tecnicas = getTecnicasForMunicipio(estado, municipio);
  if (mapFilteredTecnicas) tecnicas = tecnicas.filter(t => mapFilteredTecnicas.has(t));
  const mats = materialTechniqueSet(activeMapDriver);
  if (mats) tecnicas = tecnicas.filter(t => mats.has(t));
  layer.bindPopup(`<div class="community-map-popup"><strong>${esc(municipio)}</strong><br><span>${esc(estado)}</span><br><small>${tecnicas.length} técnica${tecnicas.length !== 1 ? 's' : ''} coincide${tecnicas.length !== 1 ? 'n' : ''} con los filtros activos</small></div>`, { maxWidth: 260 }).openPopup();
  renderTerritoryProfileCarousel({ keepIndex: true });
};

// Carga adicional del dataset territorial ya derivado de los testimonios.
const _atlasLoadDriverDataV10 = loadDriverData;
loadDriverData = async function() {
  const territoryPromise = fetch('data/datos_territoriales.json')
    .then(res => {
      if (!res.ok) throw new Error('No se pudo cargar datos_territoriales.json');
      return res.json();
    })
    .then(data => { territoryProfileData = data; })
    .catch(error => console.error('Error cargando perfiles territoriales:', error));

  await Promise.all([_atlasLoadDriverDataV10(), territoryPromise]);

  // La plataforma abre en el enfoque Estado. El panel derecho debe ser coherente
  // con esa selección: comienza con un estado al azar entre los que tienen datos.
  territoryCarouselPreviewType = 'estado';
  territoryCarouselSignature = '';
  renderTerritoryProfileCarousel({ randomStart: true, keepIndex: true });
};


/* V38 — arranque tolerante al orden de carga de scripts/datos.
   Si los CSV terminaron antes de que map_drivers.js estuviera listo,
   inicializamos aquí las mejoras territoriales sin duplicar listeners. */
queueMicrotask(() => {
  try {
    if (typeof ATLAS !== 'undefined' && Array.isArray(ATLAS?.tecnicas) && ATLAS.tecnicas.length) {
      initMapDriverEnhancements();
    }
  } catch (error) {
    console.error('No se pudo completar el arranque territorial tardío:', error);
  }
});
