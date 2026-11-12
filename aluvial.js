/* Atlas Comunitario — Explorador aluvial configurable, sin dependencias externas.
   Unidad analítica: una denominación (entrada) del catálogo; no entrevistas individuales.
   Cada entrada tiene peso total 1, dividido entre valores múltiples de cada dimensión.
   Cada par adyacente se calcula de forma independiente para no inventar trayectorias
   individuales entre estados, lenguas y características consolidadas por entrada. */
'use strict';

const ALLUVIAL_DIMENSIONS = [
  { key: 'registro', label: 'Registros del catálogo', short: 'Registros' },
  { key: 'categoria', label: 'Clasificación · categoría', short: 'Categorías' },
  { key: 'subcategoria', label: 'Clasificación · subcategoría', short: 'Subcategorías' },
  { key: 'estado', label: 'Estados', short: 'Estados' },
  { key: 'lengua', label: 'Lenguas', short: 'Lenguas' },
  { key: 'prenda', label: 'Prendas y productos', short: 'Prendas y productos' },
  { key: 'ceremonia', label: 'Ceremonias y festividades', short: 'Ceremonias' },
  { key: 'material', label: 'Materiales', short: 'Materiales' },
];

const ALLUVIAL_COLORS = [
  ['#B50552', '#A72F84', '#E35D85', '#813E99', '#DC7775', '#924A6B', '#E2A259'],
  ['#035A79', '#23869D', '#0A8190', '#49689C', '#55A9B3', '#6C8D9D', '#367B92'],
  ['#069B85', '#E49B31', '#42B8A1', '#C56B45', '#81A467', '#B87A8D', '#6798AF'],
];

const ALLUVIAL_NS = 'http://www.w3.org/2000/svg';
let alluvialReady = false;
const ALLUVIAL_DEFAULTS = Object.freeze({
  dims: Object.freeze(['categoria', 'estado', 'prenda']),
  limit: 10,
});
const alluvialState = {
  dims: [...ALLUVIAL_DEFAULTS.dims],
  limit: ALLUVIAL_DEFAULTS.limit,
  search: '',
  focus: null,
};

function aluvialHasActiveFilters() {
  return Boolean(alluvialState.focus || alluvialState.search ||
    alluvialState.limit !== ALLUVIAL_DEFAULTS.limit ||
    alluvialState.dims.some((dim, i) => dim !== ALLUVIAL_DEFAULTS.dims[i]));
}

function clearAlluvialFilters() {
  // Restaurar todos los controles de esta vista, no solo la búsqueda o el nodo activo.
  alluvialState.dims = [...ALLUVIAL_DEFAULTS.dims];
  alluvialState.limit = ALLUVIAL_DEFAULTS.limit;
  alluvialState.search = '';
  alluvialState.focus = null;

  const search = document.getElementById('alluvial-search');
  if (search) search.value = '';
  const limit = document.getElementById('alluvial-limit');
  if (limit) limit.value = String(ALLUVIAL_DEFAULTS.limit);
  aluvialHideTooltip();
  refreshAlluvial();
  const chart = document.getElementById('alluvial-chart-scroll');
  if (chart) { chart.scrollLeft = 0; chart.scrollTop = 0; }
}


function aluvialCleanValues(input) {
  const values = Array.isArray(input) ? input : [input];
  const found = new Map();
  values.forEach(item => {
    const value = String(item || '').trim();
    const norm = value.normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase().trim();
    if (value && !/^(na|n\/?a|no aplica|sin dato|sin informacion|sin información|desconocido)$/i.test(norm)) {
      if (!found.has(norm)) found.set(norm, value);
    }
  });
  return [...found.values()];
}

function aluvialEstado(value) {
  const raw = String(value || '').trim();
  const key = raw.normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase();
  const canonical = {
    'ciudad de mexico': 'Ciudad de México',
    'coahuila de zaragoza': 'Coahuila de Zaragoza',
    'michoacan de ocampo': 'Michoacán de Ocampo',
    'veracruz de ignacio de la llave': 'Veracruz de Ignacio de la Llave',
  };
  return canonical[key] || raw;
}

function aluvialValueList(entry, key) {
  switch (key) {
    case 'registro': return aluvialCleanValues(entry.tecnica);
    case 'categoria': return aluvialCleanValues(entry.cat1);
    case 'subcategoria': return aluvialCleanValues(entry.cat2);
    case 'estado': return aluvialCleanValues(aluvialCleanValues(entry.estados).map(aluvialEstado));
    case 'lengua': return aluvialCleanValues(entry.lenguas);
    case 'prenda': return aluvialCleanValues(entry.prendas_normalizadas);
    case 'ceremonia': return aluvialCleanValues(entry.ceremonias_normalizadas);
    case 'material': return aluvialCleanValues(entry.materiales_normalizados);
    default: return [];
  }
}

function aluvialSvgElement(tag, attrs = {}, textValue = null) {
  const el = document.createElementNS(ALLUVIAL_NS, tag);
  Object.entries(attrs).forEach(([key, value]) => el.setAttribute(key, String(value)));
  if (textValue !== null) el.textContent = String(textValue);
  return el;
}

function aluvialSortByWeight(a, b) {
  return b.weight - a.weight || a.label.localeCompare(b.label, 'es');
}

function aluvialPrepare(entries, dimensions, limit) {
  const rows = entries.map(entry => ({
    entry,
    values: dimensions.map(dim => aluvialValueList(entry, dim)),
  })).filter(row => row.values.every(values => values.length));
  const stageMeta = dimensions.map((dim, stage) => {
    const weights = new Map();
    rows.forEach(row => row.values[stage].forEach(value => {
      weights.set(value, (weights.get(value) || 0) + 1 / row.values[stage].length);
    }));
    const ranked = [...weights.entries()].map(([label, weight]) => ({ label, weight })).sort(aluvialSortByWeight);
    const keep = new Set(ranked.slice(0, ranked.length > limit ? limit - 1 : limit).map(x => x.label));
    const hidden = ranked.filter(x => !keep.has(x.label)).map(x => x.label);
    return { dim, keep, hidden, hiddenLabel: `Otros (${hidden.length})` };
  });

  const nodes = dimensions.map((dim, stage) => new Map());
  const links = [new Map(), new Map()];
  rows.forEach(row => {
    const grouped = row.values.map((values, stage) => {
      const meta = stageMeta[stage];
      return [...new Set(values.map(value => meta.keep.has(value) ? value : meta.hiddenLabel))];
    });
    grouped.forEach((labels, stage) => labels.forEach(label => {
      const map = nodes[stage];
      if (!map.has(label)) map.set(label, {
        id: `${stage}|${label}`, stage, label, weight: 0,
        entries: new Set(), members: new Set(),
        isOther: label === stageMeta[stage].hiddenLabel,
      });
      const node = map.get(label);
      node.weight += 1 / labels.length;
      node.entries.add(row.entry.tecnica);
      row.values[stage].filter(v => metaHasLabel(v, label, stageMeta[stage])).forEach(v => node.members.add(v));
    }));

    for (let step = 0; step < 2; step++) {
      const from = grouped[step];
      const to = grouped[step + 1];
      from.forEach(a => to.forEach(b => {
        const key = JSON.stringify([a, b]);
        if (!links[step].has(key)) links[step].set(key, {
          sourceLabel: a, targetLabel: b,
          source: nodes[step].get(a), target: nodes[step + 1].get(b),
          weight: 0, entries: new Set(), step,
        });
        const link = links[step].get(key);
        link.weight += 1 / (from.length * to.length);
        link.entries.add(row.entry.tecnica);
      }));
    }
  });
  const sortedStages = nodes.map(stage => [...stage.values()].sort((a,b) => {
    if (a.isOther !== b.isOther) return Number(a.isOther) - Number(b.isOther);
    return aluvialSortByWeight(a,b);
  }));
  return { rows, nodes: sortedStages, links: links.map(m => [...m.values()]), stageMeta };
}

function metaHasLabel(value, label, meta) {
  return meta.keep.has(value) ? value === label : label === meta.hiddenLabel;
}

function aluvialSetSelections() {
  alluvialState.dims.forEach((dim, i) => {
    const select = document.getElementById(`alluvial-dim-${i}`);
    if (!select) return;
    select.value = dim;
    [...select.options].forEach(option => {
      option.disabled = option.value !== dim && alluvialState.dims.includes(option.value);
    });
  });
  document.querySelectorAll('[data-alluvial-preset]').forEach(button => {
    const active = button.dataset.alluvialPreset === alluvialState.dims.join(',');
    button.classList.toggle('active', active);
    button.setAttribute('aria-pressed', String(active));
  });
}

function aluvialTooltip(target, title, detail, evt) {
  const el = document.getElementById('alluvial-tooltip');
  const frame = document.getElementById('alluvial-chart-frame');
  if (!el || !frame) return;
  el.replaceChildren();
  const heading = document.createElement('strong'); heading.textContent = title;
  const body = document.createElement('span'); body.textContent = detail;
  el.append(heading, body);
  el.hidden = false;
  if (evt) {
    const r = frame.getBoundingClientRect();
    const width = Math.min(265, r.width - 30);
    el.style.left = `${Math.max(9, Math.min(r.width - width - 9, evt.clientX - r.left + 14))}px`;
    el.style.top = `${Math.max(12, Math.min(r.height - 90, evt.clientY - r.top - 75))}px`;
  }
}

function aluvialHideTooltip() {
  const el = document.getElementById('alluvial-tooltip');
  if (el) el.hidden = true;
}

function aluvialHighlight(svg, selected) {
  svg.querySelectorAll('.alluvial-ribbon').forEach(path => {
    const matching = selected && (path.dataset.source === selected.id || path.dataset.target === selected.id);
    path.classList.toggle('is-muted', Boolean(selected && !matching));
    path.classList.toggle('is-highlighted', Boolean(matching));
  });
  svg.querySelectorAll('.alluvial-node').forEach(group => {
    const matching = selected && group.dataset.nodeId === selected.id;
    group.classList.toggle('is-muted', Boolean(selected && !matching));
    group.classList.toggle('is-highlighted', Boolean(matching));
  });
}

function aluvialDraw(data, dimensions) {
  const svg = document.getElementById('alluvial-svg');
  if (!svg) return;
  svg.replaceChildren();
  const maxNodes = Math.max(...data.nodes.map(stage => stage.length), 1);
  const height = Math.max(580, maxNodes * 51 + 130);
  const width = 1240;
  svg.setAttribute('viewBox', `0 0 ${width} ${height}`);
  svg.setAttribute('width', String(width));
  svg.setAttribute('height', String(height));
  const xPositions = [274, 680, 1060];
  const nodeWidth = 16;
  const minHeight = 20;
  const gap = 14;
  const padTop = 88;
  const availableHeight = height - padTop - 35;
  const total = data.rows.length;

  const defs = aluvialSvgElement('defs');
  svg.appendChild(defs);
  const bg = aluvialSvgElement('rect', { x: 0, y: 0, width, height, fill: '#ffffff', rx: 12 });
  svg.appendChild(bg);

  const headers = data.nodes.map((items, stage) => {
    const totalVals = data.stageMeta[stage].keep.size + data.stageMeta[stage].hidden.length;
    return `${String(stage+1).padStart(2,'0')}   ${ALLUVIAL_DIMENSIONS.find(d=>d.key===dimensions[stage]).short}   ·   ${totalVals} opciones`;
  });
  headers.forEach((heading, stage) => {
    svg.appendChild(aluvialSvgElement('text', { x: xPositions[stage]-120, y: 34, class: 'alluvial-svg-heading' }, heading));
    svg.appendChild(aluvialSvgElement('line', { x1: xPositions[stage]-120, x2: xPositions[stage]+96, y1: 51, y2: 51, stroke:'#e8dcdf', 'stroke-width': 1.2 }));
  });
  // Constant proportion per catalog entry; blank-data entries are excluded before drawing.
  const flowScale = Math.max(0.7, Math.min(24, ...data.nodes.map(stage =>
    (availableHeight - stage.length * (minHeight + gap)) / Math.max(total, 1))));
  data.nodes.forEach((stage, stageIndex) => {
    const occupied = stage.reduce((sum,n) => sum + minHeight + n.weight * flowScale, 0) + Math.max(0,stage.length-1) * gap;
    let cursor = padTop + Math.max(0, (availableHeight - occupied)/2);
    stage.forEach((node, index) => {
      node.y = cursor;
      node.height = minHeight + node.weight * flowScale;
      node.x = xPositions[stageIndex];
      node.color = ALLUVIAL_COLORS[stageIndex][index % ALLUVIAL_COLORS[stageIndex].length];
      node.flowStart = cursor + minHeight/2;
      cursor += node.height + gap;
    });
  });

  const allLinks = [...data.links[0], ...data.links[1]];
  // Flow offsets are computed separately at each side of each column.
  data.links.forEach((links, step) => {
    const out = new Map(), incoming = new Map();
    const bySource = [...links].sort((a,b) => a.source.y - b.source.y || a.target.y - b.target.y);
    bySource.forEach(link => {
      link.sourceY = link.source.flowStart + (out.get(link.source.id) || 0);
      out.set(link.source.id, (out.get(link.source.id) || 0) + link.weight * flowScale);
    });
    const byTarget = [...links].sort((a,b) => a.target.y - b.target.y || a.source.y - b.source.y);
    byTarget.forEach(link => {
      link.targetY = link.target.flowStart + (incoming.get(link.target.id) || 0);
      incoming.set(link.target.id, (incoming.get(link.target.id) || 0) + link.weight * flowScale);
    });
  });

  const ribbons = aluvialSvgElement('g', { class:'alluvial-ribbons' });
  svg.appendChild(ribbons);
  allLinks.sort((a,b) => b.weight-a.weight).forEach((link,i) => {
    const x0=link.source.x+nodeWidth, x1=link.target.x;
    const mid=(x1-x0)*.5;
    const w=Math.max(.15,link.weight*flowScale);
    const y0=link.sourceY, y1=link.targetY;
    const d=`M${x0},${y0} C${x0+mid},${y0} ${x1-mid},${y1} ${x1},${y1} L${x1},${y1+w} C${x1-mid},${y1+w} ${x0+mid},${y0+w} ${x0},${y0+w} Z`;
    const gradientId=`aluvial-grad-${i}`;
    const gradient=aluvialSvgElement('linearGradient', { id:gradientId, x1:'0%',y1:'0%',x2:'100%',y2:'0%' });
    gradient.appendChild(aluvialSvgElement('stop',{offset:'0%', 'stop-color':link.source.color}));
    gradient.appendChild(aluvialSvgElement('stop',{offset:'100%', 'stop-color':link.target.color}));
    defs.appendChild(gradient);
    const path=aluvialSvgElement('path', {d,fill:`url(#${gradientId})`,class:'alluvial-ribbon', 'data-source':link.source.id,'data-target':link.target.id});
    path.appendChild(aluvialSvgElement('title',{},`${link.source.label} → ${link.target.label}: ${link.entries.size} entradas relacionadas`));
    path.addEventListener('pointermove', evt => {
      aluvialHighlight(svg,null);
      path.classList.add('is-highlighted');
      aluvialTooltip(path, `${link.source.label} → ${link.target.label}`, `${link.entries.size} entradas del catálogo comparten esta asociación.`,evt);
    });
    path.addEventListener('pointerleave', () => {path.classList.remove('is-highlighted'); aluvialHideTooltip();});
    ribbons.appendChild(path);
  });

  const nodesGroup = aluvialSvgElement('g', {class:'alluvial-nodes'});
  svg.appendChild(nodesGroup);
  data.nodes.forEach((stage, stageIndex) => stage.forEach(node => {
    const group = aluvialSvgElement('g', {
      class:'alluvial-node', 'data-node-id':node.id, role:'button', tabindex:'0',
      'aria-label':`${node.label}, ${node.entries.size} entradas relacionadas. Filtrar relaciones`,
    });
    // Área pulsable accesible sobre el texto completo, no solo sobre la barra.
    group.appendChild(aluvialSvgElement('rect', {
      x:stageIndex===2 ? node.x-3 : node.x-248,
      y:node.y+node.height/2-13,
      width:stageIndex===2 ? 238 : 270, height:26,
      rx:5, fill:'transparent', 'pointer-events':'all',
    }));
    group.appendChild(aluvialSvgElement('rect', {
      x:node.x,y:node.y,width:nodeWidth,height:node.height,
      rx:8,fill:node.color,class:'alluvial-node-bar',
    }));
    const shortName = dimensions[stageIndex]==='estado' && /^Veracruz de Ignacio/i.test(node.label) ? 'Veracruz' : node.label;
    const label = node.isOther ? node.label : (shortName.length>28 ? `${shortName.slice(0,26)}…`: shortName);
    const labelX = stageIndex===2 ? node.x+nodeWidth+13 : node.x-14;
    const anchor = stageIndex===2 ? 'start' : 'end';
    group.appendChild(aluvialSvgElement('text', {
      x:labelX, y:node.y+node.height/2+4, 'text-anchor':anchor,
      class:'alluvial-svg-label',
    },label));
    group.appendChild(aluvialSvgElement('title',{},`${node.label}: ${node.entries.size} entradas relacionadas. Pulsa para filtrar.`));
    const focus = () => {
      if (alluvialState.focus && alluvialState.focus.stage===stageIndex && alluvialState.focus.label===node.label) {
        alluvialState.focus=null;
      } else {
        alluvialState.focus={stage:stageIndex,label:node.label, values:[...node.members]};
      }
      aluvialHideTooltip();
      refreshAlluvial();
    };
    group.addEventListener('click',focus);
    group.addEventListener('keydown',event => {if(event.key==='Enter' || event.key===' '){event.preventDefault();focus();}});
    group.addEventListener('pointermove',evt => {
      aluvialHighlight(svg,node);
      aluvialTooltip(group,node.label,`${node.entries.size} entradas relacionadas. Pulsa para mostrar solo estas relaciones.`,evt);
    });
    group.addEventListener('pointerleave',()=>{aluvialHighlight(svg,null);aluvialHideTooltip();});
    nodesGroup.appendChild(group);
  }));
}

function refreshAlluvial() {
  const svg = document.getElementById('alluvial-svg');
  const tag = document.getElementById('alluvial-active-filter');
  const reset = document.getElementById('alluvial-reset');
  const clearAll = document.getElementById('alluvial-clear-all');
  if (!svg || !ATLAS || !ATLAS.tecnicas) return;
  aluvialSetSelections();
  const query = alluvialState.search.normalize('NFD').replace(/[\u0300-\u036f]/g,'').toLowerCase();
  const entries = ATLAS.tecnicas.filter(entry => {
    const searchable = entry.tecnica.normalize('NFD').replace(/[\u0300-\u036f]/g,'').toLowerCase();
    if (query && !searchable.includes(query)) return false;
    if (alluvialState.focus) {
      const stage = alluvialState.focus.stage;
      const dimension = alluvialState.dims[stage];
      if (!aluvialValueList(entry,dimension).some(v=>alluvialState.focus.values.includes(v))) return false;
    }
    return true;
  });
  const data = aluvialPrepare(entries,alluvialState.dims,alluvialState.limit);
  if (reset) reset.disabled=!alluvialState.focus && !query;
  if (clearAll) clearAll.disabled=!aluvialHasActiveFilters();
  if (tag) {
    tag.hidden=!alluvialState.focus;
    if (alluvialState.focus) tag.textContent=`Filtrando por ${ALLUVIAL_DIMENSIONS.find(d=>d.key===alluvialState.dims[alluvialState.focus.stage]).short}: ${alluvialState.focus.label}`;
  }
  if (!data.rows.length) {
    svg.replaceChildren();
    svg.setAttribute('viewBox','0 0 1000 310');
    svg.appendChild(aluvialSvgElement('text',{x:500,y:146,'text-anchor':'middle',class:'alluvial-empty-title'},'No hay relaciones para esta combinación.'));
    svg.appendChild(aluvialSvgElement('text',{x:500,y:180,'text-anchor':'middle',class:'alluvial-empty-help'},'Prueba otras columnas o elimina el filtro actual.'));
    return;
  }
  aluvialDraw(data,alluvialState.dims);
}

function initAlluvial() {
  if (alluvialReady) {refreshAlluvial();return;}
  alluvialReady=true;
  const selects=[0,1,2].map(i=>document.getElementById(`alluvial-dim-${i}`));
  selects.forEach((select,index)=>{
    if (!select) return;
    ALLUVIAL_DIMENSIONS.forEach(d => {
      const option=document.createElement('option');option.value=d.key;option.textContent=d.label;
      select.appendChild(option);
    });
    select.addEventListener('change',()=>{
      if (alluvialState.dims.some((dim,i)=>i!==index && dim===select.value)) {aluvialSetSelections();return;}
      alluvialState.dims[index]=select.value;
      alluvialState.focus=null;
      refreshAlluvial();
    });
  });
  document.querySelectorAll('[data-alluvial-preset]').forEach(btn=>btn.addEventListener('click',()=>{
    alluvialState.dims=btn.dataset.alluvialPreset.split(',');
    alluvialState.focus=null;
    alluvialState.search='';
    const search=document.getElementById('alluvial-search');if(search)search.value='';
    refreshAlluvial();
  }));
  document.getElementById('alluvial-limit')?.addEventListener('change',e=>{
    alluvialState.limit=Math.min(40,Math.max(10,Number(e.target.value)||20));
    alluvialState.focus=null;
    refreshAlluvial();
  });
  document.getElementById('alluvial-search')?.addEventListener('input',e=>{
    alluvialState.search=e.target.value.trim();
    alluvialState.focus=null;
    refreshAlluvial();
  });
  document.getElementById('alluvial-clear-all')?.addEventListener('click', clearAlluvialFilters);
  document.getElementById('alluvial-reset')?.addEventListener('click',()=>{
    alluvialState.focus=null;alluvialState.search='';
    const search=document.getElementById('alluvial-search');if(search)search.value='';
    refreshAlluvial();
  });
  document.getElementById('alluvial-help-toggle')?.addEventListener('click',function(){
    const help=document.getElementById('alluvial-help');if(!help)return;
    help.hidden=!help.hidden;
    this.setAttribute('aria-expanded',String(!help.hidden));
  });
  // En pantallas pequeñas el gráfico se consulta primero; filtros e instrucciones
  // siguen disponibles desde el botón visible del panel lateral.
  const mobileLayout=window.matchMedia('(max-width: 1080px)');
  const controls=document.getElementById('alluvial-sidebar-controls');
  const sidebarToggle=document.getElementById('alluvial-sidebar-toggle');
  function setSidebarOpen(isOpen) {
    if (!controls || !sidebarToggle) return;
    controls.hidden=!isOpen;
    sidebarToggle.setAttribute('aria-expanded',String(isOpen));
    sidebarToggle.textContent=isOpen?'Ocultar filtros':'Mostrar filtros';
  }
  function updateSidebarLayout() {
    setSidebarOpen(!mobileLayout.matches);
    const help=document.getElementById('alluvial-help');
    if (help) {
      help.hidden=mobileLayout.matches;
      document.getElementById('alluvial-help-toggle')?.setAttribute('aria-expanded',String(!help.hidden));
    }
  }
  sidebarToggle?.addEventListener('click',()=>setSidebarOpen(controls?.hidden));
  document.getElementById('alluvial-show-diagram')?.addEventListener('click', () => {
    if (mobileLayout.matches) setSidebarOpen(false);
    document.querySelector('.alluvial-visualization')?.scrollIntoView({behavior:'smooth',block:'start'});
  });
  if (mobileLayout.addEventListener) mobileLayout.addEventListener('change',updateSidebarLayout);
  else mobileLayout.addListener?.(updateSidebarLayout);
  updateSidebarLayout();

  refreshAlluvial();
}
