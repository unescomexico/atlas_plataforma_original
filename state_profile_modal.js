/* Ficha emergente de estado. Se superpone al mapa sin reemplazar el panel. */
(() => {
  'use strict';
  let overlay = null;
  let returnFocus = null;
  let photosInModal = [];
  let priorOverflow = '';
  let stateMap = null;
  let stateMapMarkers = [];
  let stateMapRows = new Map();
  let stateMapSelection = { pueblos: new Set(), registros: new Set() };
  let stateMapToken = 0;
  let centroidPromise = null;
  let polygonPromise = null;
  let stateMapFallback = null;
  let stateTileTimer = null;

  const safe = value => String(value ?? '')
    .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
  const canonicalState = value => String(typeof normalizeEstado === 'function' ? normalizeEstado(value) : (value || ''))
    .normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLocaleLowerCase('es')
    .replace(/^(?:districto federal|distrito federal|ciudad de mexico)$/, 'ciudad de mexico')
    .replace(/^estado de mexico$/, 'mexico')
    .replace(/^veracruz de ignacio de la llave$/, 'veracruz')
    .replace(/^coahuila de zaragoza$/, 'coahuila')
    .replace(/^michoacan de ocampo$/, 'michoacan').trim();
  const sameState = (a, b) => canonicalState(a) === canonicalState(b);

  function profileForState(name) {
    // Reutilizamos el mismo objeto que alimenta los perfiles del panel derecho.
    if (typeof territoryProfileData === 'undefined' || !territoryProfileData?.estado) return null;
    return Object.values(territoryProfileData.estado).find(p => sameState(p.name, name)) || null;
  }

  function asList(value) {
    if (Array.isArray(value)) return value.map(String).filter(Boolean);
    if (value && typeof value === 'object') return Object.keys(value);
    const raw = String(value || '').trim();
    if (!raw || raw === '[]') return [];
    try { const parsed = JSON.parse(raw); if (Array.isArray(parsed)) return parsed.map(String); } catch (_) {}
    // El archivo territorial original también guarda ciertas listas como repr() de Python.
    const quoted = [...raw.matchAll(/['"]([^'"]+)['"]/g)].map(m => m[1]);
    return quoted.length ? quoted : raw.split(';').map(s => s.trim()).filter(Boolean);
  }

  // Las etiquetas de los perfiles son enlaces reales a los filtros existentes del catálogo.
  function chips(items, field, state, limit = 120) {
    const unique = new Map();
    items.filter(item => item && item.label && item.value).forEach(item => {
      const key = String(item.value).toLocaleLowerCase('es-MX');
      if (!unique.has(key)) unique.set(key, item);
    });
    const trimmed = [...unique.values()];
    if (!trimmed.length) return '<p class="atlas-state-sheet-muted">Sin información disponible</p>';
    return `<div class="atlas-state-sheet-chips">${trimmed.slice(0,limit).map(item =>
      `<button type="button" class="atlas-state-sheet-filter" data-atlas-sheet-filter="${safe(field)}" data-atlas-sheet-value="${safe(encodeURIComponent(item.value))}" data-atlas-sheet-state="${safe(encodeURIComponent(state))}" title="Filtrar el catálogo por ${safe(item.label)}">${safe(item.label)} <span aria-hidden="true">↗</span></button>`
    ).join('')}${trimmed.length > limit ? `<span class="atlas-state-sheet-more">+${trimmed.length-limit}</span>` : ''}</div>`;
  }

  function entriesForState(state) {
    if (typeof ATLAS === 'undefined' || !Array.isArray(ATLAS?.tecnicas)) return [];
    return ATLAS.tecnicas.filter(t => (t.estados || []).some(name => sameState(name, state)));
  }

  function stateFacets(state) {
    const rows = entriesForState(state);
    const municipalities = new Map();
    const communities = new Map();
    const materials = new Set();
    const pueblos = new Set();
    rows.forEach(t => {
      (t.territorios || []).filter(x => sameState(x.estado, state)).forEach(x => {
        if (x.municipio) municipalities.set(x.municipio, {label:x.municipio, value:x.municipio});
      });
      (t.localidades || []).filter(x => sameState(x.estado, state)).forEach(x => {
        if (x.etiqueta) communities.set(x.etiqueta, {
          label: x.localidad ? [x.localidad, x.municipio].filter(Boolean).join(' · ') : x.etiqueta,
          value: x.etiqueta
        });
      });
      (t.materiales_normalizados || []).forEach(value => materials.add(value));
      (t.pueblos_por_estado || []).filter(x => sameState(x.estado, state)).forEach(x => pueblos.add(x.pueblo));
    });
    const sort = (a,b) => a.label.localeCompare(b.label,'es',{sensitivity:'base'});
    return {
      municipalities: [...municipalities.values()].sort(sort),
      communities: [...communities.values()].sort(sort),
      materials: [...materials].map(v => ({label:v,value:v})).sort(sort),
      pueblos: [...pueblos].map(v => ({label:v,value:v})).sort(sort),
    };
  }

  function availablePhotos(state) {
    const rows = typeof GALLERY_ITEMS !== 'undefined' && Array.isArray(GALLERY_ITEMS) ? GALLERY_ITEMS : [];
    const allowed = window.ATLAS_AVAILABLE_IMAGES;
    const seen = new Set();
    return rows.filter(item => {
      if (!item?.archivo || !sameState(item.estado, state) || seen.has(item.archivo)) return false;
      if (allowed instanceof Set && !allowed.has(item.archivo)) return false;
      seen.add(item.archivo);
      return true;
    }).slice(0, 12);
  }

  function techniques(profile) {
    const summary = profile.summary || {};
    const raw = String(summary.tecnicas_principales || '');
    const ranked = raw.split(';').map(x => x.trim()).filter(Boolean);
    const all = asList(summary.tecnicas);
    return ranked.length ? ranked : all;
  }

  // Se reutiliza la MISMA capa de puntos y el MISMO archivo geográfico
  // del mapa principal; las fuentes actuales no contienen coordenadas por localidad.
  const norm = value => String(value || '').normalize('NFD')
    .replace(/[\u0300-\u036f]/g,'').toLocaleLowerCase('es')
    .replace(/[^a-z0-9]+/g, ' ').trim();
  const stateKey = value => (typeof normalizeEstado === 'function' ? normalizeEstado(value) : norm(value));
  const featureName = feature => feature?.properties?.name || feature?.properties?.NAME_1 || '';
  const uniqueSorted = values => [...new Set(values.filter(Boolean))]
    .sort((a,b)=>a.localeCompare(b,'es',{sensitivity:'base'}));

  function stateContributions(state) {
    const rows = typeof ATLAS !== 'undefined' && Array.isArray(ATLAS?.records) ? ATLAS.records : [];
    const puebloFromLanguage = window.ATLAS_PUEBLO_DESDE_LENGUA;
    return rows.filter(r => sameState(r.Estado,state) && r.Municipio && !/^(?:na|sin dato|no aplica)$/i.test(r.Municipio.trim()))
      .map(row => ({
        municipio: String(row.Municipio || '').trim(),
        registro: String(row.Tecnica || '').trim(),
        pueblo: typeof puebloFromLanguage === 'function' ? (puebloFromLanguage(row.Lengua) || '') : ''
      }));
  }

  function municipalityRows(state) {
    const groups = new Map();
    stateContributions(state).forEach(row => {
      const key = norm(row.municipio);
      if (!groups.has(key)) groups.set(key, {name:row.municipio, rows:[]});
      groups.get(key).rows.push(row);
    });
    return groups;
  }

  function mapFilterOptions(groups) {
    const rows = [...groups.values()].flatMap(x=>x.rows);
    return {
      pueblos: uniqueSorted(rows.map(r=>r.pueblo)),
      registros: uniqueSorted(rows.map(r=>r.registro))
    };
  }

  function mapFiltersHtml(state, groups, profile) {
    const moreThanOne = Number(profile?.summary?.n_comunidades || 0) > 1;
    const options = mapFilterOptions(groups);
    if (!moreThanOne) return '';
    return `<div class="atlas-mini-filters" aria-label="Filtros del mapa de ${safe(state)}">
      ${[['pueblos','Pueblo indígena'],['registros','Registros']].map(([key,title])=>`
      <details class="atlas-mini-filter" data-atlas-mini-filter-group="${key}">
        <summary><span class="atlas-mini-filter-label-full">${title}</span><span class="atlas-mini-filter-label-short">${key==='pueblos'?'Pueblo':title}</span> <span data-atlas-mini-count="${key}"></span></summary>
        <div class="atlas-mini-filter-options">${options[key].length
          ? options[key].map((item,i)=>`<label><input type="checkbox" data-atlas-mini-filter="${key}" value="${safe(item)}"><span>${safe(item)}</span></label>`).join('')
          : '<p class="atlas-state-sheet-muted">Sin información disponible</p>'}</div>
      </details>`).join('')}
      <button type="button" class="atlas-mini-filter-reset" data-atlas-mini-reset disabled aria-label="Limpiar los filtros del mapa">Limpiar</button>
    </div>`;
  }

  function mapBlockHtml(profile) {
    const groups = municipalityRows(profile.name);
    return `<section class="atlas-state-sheet-section atlas-state-sheet-map-section" aria-label="Mapa de ${safe(profile.name)}">
      <div class="atlas-state-map-heading"><h3>Mapa de aportaciones</h3></div>
      <div class="atlas-state-map-shell">
        <div id="atlas-state-mini-map" class="atlas-state-mini-map" role="region" aria-label="Mapa interactivo de ${safe(profile.name)}">
          <p class="atlas-state-map-message">Cargando mapa del estado…</p>
        </div>
        <div id="atlas-state-map-popup" class="atlas-mini-map-popup" hidden aria-live="polite"></div>
        ${mapFiltersHtml(profile.name, groups, profile)}
      </div>
    </section>`;
  }

  // Las fichas reutilizan la capa de límites del mapa principal para situar
  // cada estado entre sus vecinos. Si todavía no está lista, se consulta la
  // misma fuente que utiliza el mapa principal (sin incorporar otra fuente).
  function getStateGeometries(state) {
    const mapped = [];
    if (typeof geojsonLayer !== 'undefined' && geojsonLayer?.eachLayer) {
      geojsonLayer.eachLayer(layer => {
        if (layer.feature?.geometry) mapped.push(layer.feature);
      });
    }
    if (mapped.length >= 30) {
      return Promise.resolve({
        selected: mapped.find(feature => sameState(featureName(feature), state)) || null,
        all: mapped
      });
    }
    if (!polygonPromise) {
      polygonPromise = fetch('https://raw.githubusercontent.com/angelnmara/geojson/master/mexicoHigh.json')
        .then(res => { if(!res.ok) throw new Error('Geometría estatal no disponible'); return res.json(); })
        .catch(error => { polygonPromise = null; throw error; });
    }
    return polygonPromise.then(data => {
      const all = (data.features || []).filter(feature => feature?.geometry);
      return { selected: all.find(feature => sameState(featureName(feature), state)) || null, all };
    });
  }

  function getMunicipalCentroids() {
    if (!centroidPromise) {
      centroidPromise = fetch('geodata/mexico_municipios_centroids.geojson')
        .then(res => { if(!res.ok) throw new Error('Puntos municipales no disponibles'); return res.json(); })
        .catch(error => {centroidPromise = null; throw error;});
    }
    return centroidPromise;
  }

  function activeRow(row) {
    const sel = stateMapSelection;
    return (!sel.pueblos.size || sel.pueblos.has(row.pueblo)) &&
      (!sel.registros.size || sel.registros.has(row.registro));
  }

  function matchingCommunities(state, municipio, rows) {
    const gallery = typeof GALLERY_ITEMS !== 'undefined' && Array.isArray(GALLERY_ITEMS) ? GALLERY_ITEMS : [];
    const records = new Set(rows.map(x=>norm(x.registro)));
    return uniqueSorted(gallery.filter(x => x.localidad && sameState(x.estado,state) &&
      norm(x.municipio)===norm(municipio) && records.has(norm(x.tecnica)))
      .map(x=>String(x.localidad).trim()));
  }

  function markerPopup(state, group, filtered) {
    const regs = uniqueSorted(filtered.map(r=>r.registro));
    const pueblos = uniqueSorted(filtered.map(r=>r.pueblo));
    const localities = matchingCommunities(state,group.name,filtered);
    const short = (items, max=6) => items.length
      ? safe(items.slice(0,max).join(' · ')) + (items.length > max ? ` · +${items.length-max}` : '')
      : 'Sin información disponible';
    return `<div class="atlas-state-marker-popup"><strong>${safe(group.name)}</strong>
      <p>${filtered.length} aportación${filtered.length===1?'':'es'}</p>
      <p><b>Localidad:</b> ${short(localities)}</p>
      <p><b>Pueblo indígena:</b> ${short(pueblos)}</p>
      <p><b>Registros:</b> ${short(regs)}</p>
      </div>`;
  }

  // El detalle de un punto siempre permanece abajo a la izquierda del mapa:
  // no se ancla a la posición del marcador ni se cruza con los filtros superiores.
  function hideMapPopup() {
    const popup = overlay?.querySelector('#atlas-state-map-popup');
    if (popup) { popup.hidden = true; popup.innerHTML = ''; }
  }

  function showMapPopup(content) {
    const popup = overlay?.querySelector('#atlas-state-map-popup');
    if (!popup) return;
    // Evita que un menú abierto cubra el detalle de la aportación.
    overlay.querySelectorAll('.atlas-mini-filter[open]').forEach(detail => { detail.open = false; });
    popup.innerHTML = `<button type="button" class="atlas-mini-map-popup-close" data-atlas-mini-popup-close aria-label="Cerrar información del punto">×</button>${content}`;
    popup.hidden = false;
  }

  // Vista cartográfica de respaldo: el perímetro y los puntos se dibujan con
  // las mismas geometrías utilizadas por Leaflet. Evita que la ficha quede
  // completamente vacía si un mosaico remoto o la inicialización fallan.
  function renderStateMapFallback(profile, geometry, otherStates, centroids, groups) {
    const holder = overlay?.querySelector('#atlas-state-mini-map');
    if (!holder || !geometry?.geometry) return null;
    const rings = [];
    const collect = coordinates => {
      if (!Array.isArray(coordinates) || !coordinates.length) return;
      if (typeof coordinates[0] === 'number') return;
      if (Array.isArray(coordinates[0]) && typeof coordinates[0][0] === 'number') {
        if (coordinates.length >= 3) rings.push(coordinates);
      } else coordinates.forEach(collect);
    };
    collect(geometry.geometry.coordinates);
    if (!rings.length) return null;
    const all = rings.flat();
    let west=Infinity,east=-Infinity,south=Infinity,north=-Infinity;
    for(const [lon,lat] of all){
      west=Math.min(west,lon);east=Math.max(east,lon);
      south=Math.min(south,lat);north=Math.max(north,lat);
    }
    const cos=Math.cos(((north+south)/2)*Math.PI/180);
    // Usar la relación de aspecto real del modal: en móvil, el polígono debe
    // aprovechar la altura disponible, no quedar reducido a una franja.
    const w=Math.max(240,holder.clientWidth || 860);
    const h=Math.max(220,holder.clientHeight || 310);
    const pad=Math.min(26,w*0.065,h*0.10);
    const spanX=Math.max(0.001,(east-west)*cos),spanY=Math.max(0.001,north-south);
    // Un pequeño margen adicional hace visibles los estados vecinos sin
    // alejar el encuadre del estado seleccionado.
    const scale=Math.min((w-2*pad)/(spanX*1.28),(h-2*pad)/(spanY*1.28));
    const toXY=(lon,lat)=>[
      w/2+(lon-(west+east)/2)*cos*scale,
      h/2- (lat-(south+north)/2)*scale
    ];
    const ringPath = ring => ring.map((pos, i) => {
      const [x,y] = toXY(pos[0], pos[1]);
      return `${i?'L':'M'}${x.toFixed(2)} ${y.toFixed(2)}`;
    }).join(' ')+' Z';
    const path = rings.map(ringPath).join(' ');
    const contextRings = [];
    // Dibujar solo polígonos que intersecten la ventana aproximada del SVG.
    // Esto evita procesar líneas de estados que ni siquiera son visibles.
    const viewLon = w/(2*scale*cos), viewLat = h/(2*scale);
    const visible = [((west+east)/2)-viewLon, ((west+east)/2)+viewLon,
      ((south+north)/2)-viewLat, ((south+north)/2)+viewLat];
    const collectContext = coordinates => {
      if (!Array.isArray(coordinates) || !coordinates.length) return;
      if (Array.isArray(coordinates[0]) && typeof coordinates[0][0] === 'number') {
        if (coordinates.length < 3) return;
        const bbox = coordinates.reduce((box, [lon,lat]) => [
          Math.min(box[0],lon), Math.max(box[1],lon),
          Math.min(box[2],lat), Math.max(box[3],lat)
        ], [Infinity,-Infinity,Infinity,-Infinity]);
        if (bbox[1]>=visible[0] && bbox[0]<=visible[1] && bbox[3]>=visible[2] && bbox[2]<=visible[3]) {
          contextRings.push(coordinates);
        }
      } else coordinates.forEach(collectContext);
    };
    (otherStates || []).forEach(feature => collectContext(feature.geometry?.coordinates));
    const contextPath = contextRings.map(ringPath).join(' ');
    const markers = [];
    const used = new Set();
    (centroids?.features || []).forEach(f => {
      if (!sameState(f.properties?.estado_nombre,profile.name) || f.geometry?.type!=='Point') return;
      const key=norm(f.properties?.NOMGEO);
      if (!groups.has(key) || used.has(key)) return;
      const coords=f.geometry.coordinates;
      if (!Number.isFinite(coords?.[0]) || !Number.isFinite(coords?.[1])) return;
      used.add(key);
      const [x,y]=toXY(coords[0],coords[1]);
      markers.push({key,x,y,group:groups.get(key)});
    });
    const node=document.createElement('div');
    node.className='atlas-state-map-fallback';
    node.setAttribute('aria-label',`Mapa interactivo de respaldo para ${profile.name}`);
    node.innerHTML=`<div class="atlas-state-map-canvas">
      <svg xmlns="http://www.w3.org/2000/svg" class="atlas-state-map-svg" viewBox="0 0 ${w} ${h}" preserveAspectRatio="xMidYMid meet" role="img" aria-label="Contorno de ${safe(profile.name)} y puntos del Atlas">
        <rect x="0" y="0" width="${w}" height="${h}" fill="#deedf3"/>
        <path class="atlas-fallback-context-path" d="${contextPath}" fill="#f2f5f5" fill-rule="evenodd" stroke="#aab7b9" stroke-width="0.85" stroke-linejoin="round" vector-effect="non-scaling-stroke" pointer-events="none"/>
        <path class="atlas-fallback-state-path" d="${path}" fill="#fae6e9" fill-rule="evenodd" stroke="#970b49" stroke-width="2.4" stroke-linejoin="round" vector-effect="non-scaling-stroke"/>
        ${markers.map(m=>`<circle class="atlas-fallback-marker" data-atlas-fallback-point="${safe(m.key)}" cx="${m.x.toFixed(2)}" cy="${m.y.toFixed(2)}" r="8.2" fill="#b50552" stroke="#fff" stroke-width="2" role="button" tabindex="0" aria-label="Consultar ${safe(m.group.name)}"/>`).join('')}
      </svg>
    </div>
    <div class="atlas-fallback-zoom" aria-label="Controles de acercamiento"><button type="button" data-atlas-fallback-zoom="in" aria-label="Acercar">+</button><button type="button" data-atlas-fallback-zoom="out" aria-label="Alejar">−</button></div>
    `;
    holder.append(node);
    let zoom=1, shiftX=0, shiftY=0;
    const svg=node.querySelector('svg');
    const canvas=node.querySelector('.atlas-state-map-canvas');
    const updateZoom=()=>{
      const bounds=canvas.getBoundingClientRect();
      const mx=(zoom-1)*bounds.width/2,my=(zoom-1)*bounds.height/2;
      shiftX=Math.min(mx,Math.max(-mx,shiftX));
      shiftY=Math.min(my,Math.max(-my,shiftY));
      svg.style.transform=`translate(${shiftX}px, ${shiftY}px) scale(${zoom})`;
      node.querySelector('[data-atlas-fallback-zoom="out"]').disabled=zoom===1;
      node.querySelector('[data-atlas-fallback-zoom="in"]').disabled=zoom>=4;
    };
    const setZoom = direction => {
      zoom=Math.min(4,Math.max(1,+(zoom+(direction==='in'?0.5:-0.5)).toFixed(2)));
      if (zoom===1) shiftX=shiftY=0;
      updateZoom();hideMapPopup();
    };
    node.querySelectorAll('[data-atlas-fallback-zoom]').forEach(btn=>btn.addEventListener('click',()=>setZoom(btn.dataset.atlasFallbackZoom)));
    // Permite explorar el polígono cuando el usuario se acerca. El alejamiento
    // nunca supera el encuadre inicial del estado.
    let dragging=null;
    canvas.addEventListener('pointerdown',event=>{
      if (event.target.closest('[data-atlas-fallback-point]') || zoom===1) return;
      dragging={x:event.clientX,y:event.clientY,sx:shiftX,sy:shiftY};
      canvas.setPointerCapture(event.pointerId);
    });
    canvas.addEventListener('pointermove',event=>{
      if(!dragging) return;
      shiftX=dragging.sx+event.clientX-dragging.x;
      shiftY=dragging.sy+event.clientY-dragging.y;
      updateZoom();
    });
    const stop=()=>{dragging=null;};
    canvas.addEventListener('pointerup',stop);
    canvas.addEventListener('pointercancel',stop);
    const update=()=>{
      let visible=0;
      markers.forEach(m=>{
        const selected=m.group.rows.filter(activeRow);
        const dot=node.querySelectorAll('[data-atlas-fallback-point]');
        const element=[...dot].find(el=>el.getAttribute('data-atlas-fallback-point')===m.key);
        if(element)element.style.display=selected.length?'':'none';
        if(selected.length) visible++;
      });
      hideMapPopup();
      return visible;
    };
    const showPopup=(key)=>{
      const m=markers.find(x=>x.key===key);
      if(!m)return;
      const selected=m.group.rows.filter(activeRow);
      if(!selected.length)return;
      showMapPopup(markerPopup(profile.name,m.group,selected));
    };
    node.addEventListener('click',event=>{
      const dot=event.target.closest?.('[data-atlas-fallback-point]');
      if(dot)showPopup(dot.getAttribute('data-atlas-fallback-point'));
      else if(event.target===canvas || event.target===svg) hideMapPopup();
    });
    node.addEventListener('keydown',event=>{
      if((event.key==='Enter'||event.key===' ') && event.target.matches?.('[data-atlas-fallback-point]')){
        event.preventDefault();showPopup(event.target.getAttribute('data-atlas-fallback-point'));
      }
    });
    updateZoom();
    return {node,update,markers,dispose(){node.remove();}};
  }

  function syncStateMarkers() {
    let visible = 0;
    let count = 0;
    if (stateMap && stateMapMarkers.length) {
      stateMapMarkers.forEach(({marker,group,state}) => {
        const matched = group.rows.filter(activeRow);
        if (matched.length) {
          if (!stateMap.hasLayer(marker)) marker.addTo(stateMap);
          marker.setStyle({radius:typeof COMUNIDAD_STYLE !== 'undefined' ? COMUNIDAD_STYLE.radius : 4});
          // El popup es un panel fijo en la esquina inferior izquierda.
          // El contenido se calcula al hacer clic, respetando los filtros actuales.
          visible++; count+=matched.length;
        } else if (stateMap.hasLayer(marker)) stateMap.removeLayer(marker);
      });
    } else {
      const groups = stateMapFallback?.markers?.map(m=>m.group) || [];
      groups.forEach(group=>{
        const matched=group.rows.filter(activeRow);
        if(matched.length){visible++;count+=matched.length;}
      });
    }
    stateMapFallback?.update();
    hideMapPopup();
    const reset = overlay?.querySelector('[data-atlas-mini-reset]');
    if (reset) reset.disabled = !Object.values(stateMapSelection).some(selection => selection.size);
    overlay?.querySelectorAll('[data-atlas-mini-count]').forEach(el => {
      const size=stateMapSelection[el.dataset.atlasMiniCount]?.size || 0;
      el.textContent = size ? `(${size})` : '';
    });
  }

  function destroyStateMap() {
    stateMapToken++;
    if(stateTileTimer)clearTimeout(stateTileTimer);
    stateTileTimer=null;
    stateMapFallback?.dispose();stateMapFallback=null;
    stateMapMarkers=[];
    stateMapRows=new Map();
    stateMapSelection={pueblos:new Set(),registros:new Set()};
    if(stateMap){try{stateMap.remove();}catch(e){console.warn('Mapa de la ficha no pudo cerrarse limpiamente:',e);}stateMap=null;}
  }

  async function initStateMap(profile, token) {
    const holder = overlay?.querySelector('#atlas-state-mini-map');
    if (!holder) return;
    try {
      // No se bloquea el perímetro por un fallo en la descarga de centroides.
      const [stateGeometries,centroids] = await Promise.all([
        getStateGeometries(profile.name),
        getMunicipalCentroids().catch(error=>{
          console.warn('No fue posible obtener todos los centroides municipales:',error);
          return {type:'FeatureCollection',features:[]};
        })
      ]);
      if(token !== stateMapToken || overlay?.hidden || !holder.isConnected) return;
      const stateGeo = stateGeometries?.selected;
      if(!stateGeo) throw new Error('No se encontró el polígono del estado');
      const neighbors = (stateGeometries.all || []).filter(feature =>
        !sameState(featureName(feature), profile.name));
      holder.innerHTML='';
      stateMapRows=municipalityRows(profile.name);
      // Se muestra inmediatamente el contorno y los puntos como vista de
      // respaldo, antes de intentar cargar el fondo de mosaicos remoto.
      stateMapFallback=renderStateMapFallback(profile,stateGeo,neighbors,centroids,stateMapRows);
      syncStateMarkers();
      if(typeof L==='undefined'||!L.map) {
        console.warn('Leaflet no está disponible: se conserva la vista cartográfica local.');
        return;
      }
      let tileWasLoaded=false;
      let renderReady=false;
      const revealLeaflet = () => {
        if(!tileWasLoaded || !renderReady || !stateMapFallback || token!==stateMapToken)return;
        requestAnimationFrame(()=>{
          const layer=holder.querySelector('.leaflet-overlay-pane path.atlas-selected-state-outline');
          const rect=layer?.getBoundingClientRect();
          const mapRect=holder.getBoundingClientRect();
          // No ocultar el respaldo si Leaflet obtuvo mosaicos pero su
          // polígono todavía no fue dibujado o quedó fuera de pantalla.
          if(rect && rect.width>3 && rect.height>3 && rect.right>mapRect.left && rect.left<mapRect.right &&
             rect.bottom>mapRect.top && rect.top<mapRect.bottom) {
            stateMapFallback?.dispose();stateMapFallback=null;
          }
        });
      };
      try {
        stateMap=L.map(holder,{zoomControl:false,scrollWheelZoom:window.matchMedia('(min-width: 768px)').matches,
          minZoom:1,maxZoom:17,zoomSnap:0.25,zoomDelta:1,maxBoundsViscosity:1});
        const tiles=L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png',{
          attribution:'&copy; <a href="https://www.openstreetmap.org/copyright" target="_blank" rel="noopener noreferrer">OpenStreetMap contributors</a>',
          maxZoom:19,minZoom:1,
        });
        tiles.on('tileload',()=>{
          tileWasLoaded=true;
          revealLeaflet();
        });
        tiles.addTo(stateMap);
        L.control.zoom({position:'topright'}).addTo(stateMap);
        // Contornos ligeros para contextualizar. No son clicables y se dibujan
        // debajo del polígono destacado y de los puntos de aportaciones.
        if (neighbors.length) L.geoJSON({type:'FeatureCollection',features:neighbors},{
          style:{color:'#aab7b9',weight:0.85,opacity:0.75,fillColor:'#f2f5f5',fillOpacity:0.08},
          interactive:false
        }).addTo(stateMap);
        const border=L.geoJSON(stateGeo,{style:{className:'atlas-selected-state-outline',color:'#8B0440',weight:2.6,opacity:1,fillColor:'#BD4772',fillOpacity:0.095},interactive:false}).addTo(stateMap);
        const bounds=border.getBounds();
        const contextBounds=bounds.pad(0.18);
        if (!bounds.isValid()) throw new Error('El polígono no tiene límites válidos');
        (centroids.features || []).forEach(f => {
          if (!sameState(f.properties?.estado_nombre, profile.name) || f.geometry?.type!=='Point') return;
          const group=stateMapRows.get(norm(f.properties?.NOMGEO));
          if(!group) return;
          const coords=f.geometry.coordinates;
          if (!Number.isFinite(coords?.[0]) || !Number.isFinite(coords?.[1])) return;
          const marker=L.circleMarker([coords[1],coords[0]],
            typeof COMUNIDAD_STYLE !== 'undefined' ? { ...COMUNIDAD_STYLE } :
              { color:'#FFFFFF',weight:1.2,fillColor:'#B50552',fillOpacity:.85,radius:4 }
          ).addTo(stateMap);
          marker.bindTooltip(safe(group.name),{direction:'top'});
          marker.on('click', event => {
            if (event.originalEvent && L.DomEvent?.stopPropagation) L.DomEvent.stopPropagation(event.originalEvent);
            const selected = group.rows.filter(activeRow);
            if (selected.length) showMapPopup(markerPopup(profile.name,group,selected));
          });
          stateMapMarkers.push({marker,group,state:profile.name});
        });
        stateMap.on('click', hideMapPopup);
        // Espera a un tamaño real y luego fija el zoom mínimo del estado.
        stateMap.invalidateSize({pan:false});
        stateMap.fitBounds(contextBounds,{padding:[12,12],animate:false});
        stateMap.setMinZoom(stateMap.getZoom());
        stateMap.setMaxBounds(bounds.pad(0.6));
        renderReady=true;
        syncStateMarkers();
        revealLeaflet();
        requestAnimationFrame(()=>{
          if(token!==stateMapToken || !stateMap) return;
          stateMap.invalidateSize({pan:false});
          stateMap.fitBounds(contextBounds,{padding:[12,12],animate:false});
          stateMap.setMinZoom(stateMap.getZoom());
        });
        // Si los mosaicos no llegan, se conserva el mapa SVG interactivo:
        // ninguna ficha debe quedar como un rectángulo vacío.
        stateTileTimer=setTimeout(()=>{
          if(token===stateMapToken && !tileWasLoaded && stateMapFallback) {
            const notice=stateMapFallback.node.querySelector('.atlas-fallback-notice');
            if(notice)notice.textContent='Mapa de referencia: sin fondo cartográfico disponible';
          }
        },3500);
      } catch(error) {
        console.error('Leaflet no pudo construir el mapa de la ficha; se muestra la vista territorial de respaldo:',error);
        if(stateMap){try{stateMap.remove();}catch(_){ }stateMap=null;}
        stateMapMarkers=[];
        syncStateMarkers();
      }
    } catch(error) {
      if(token !== stateMapToken || !holder.isConnected) return;
      console.error('No se pudo mostrar el mapa estatal:',error);
      if(stateMap){try{stateMap.remove();}catch(_){ }stateMap=null;}
      if (!stateMapFallback) holder.innerHTML='<p class="atlas-state-map-message">No fue posible obtener la geometría del estado. Comprueba la conexión y vuelve a intentarlo.</p>';
    }
  }

  function renderSheet(profile) {
    const s = profile.summary || {};
    const aportaciones = Number(s.n_registros || 0);
    const registros = Number(s.n_tecnicas || 0);
    const comunidades = Number(s.n_comunidades || 0);
    const entries = techniques(profile);
    const facets = stateFacets(profile.name);
    photosInModal = availablePhotos(profile.name);
    const imageTiles = photosInModal.map((photo, i) => {
      const src = typeof imgPath === 'function' ? imgPath(photo.archivo) : `imagenes/${encodeURIComponent(photo.archivo)}`;
      return `<button class="atlas-state-sheet-photo" type="button" data-atlas-sheet-photo="${i}" title="Ampliar fotografía">
          <img src="${safe(src)}" loading="lazy" alt="${safe(photo.tecnica || 'Registro textil')} · ${safe(profile.name)}" onerror="this.closest('button').remove()">
          <span>${safe(photo.tecnica || 'Registro textil')}</span>
        </button>`;
    }).join('');
    const media = photosInModal.length
      ? `<div class="atlas-state-sheet-gallery">${imageTiles}</div>`
      : `<p class="atlas-state-sheet-muted">No hay fotografías identificadas para este estado en la galería disponible.</p>`;

    return `<section class="atlas-state-sheet-dialog" role="dialog" aria-modal="true" aria-labelledby="atlas-state-sheet-title" tabindex="-1">
      <div class="atlas-state-sheet-top"><div><div class="atlas-state-sheet-eyebrow">Atlas del Arte Textil de México · Perfil por estado</div>
        <h2 id="atlas-state-sheet-title">${safe(profile.name)}</h2></div>
        <button type="button" class="atlas-state-sheet-close" data-atlas-sheet-close aria-label="Cerrar ficha del estado">×</button></div>
      <div class="atlas-state-sheet-scroll">
        <div class="atlas-state-sheet-intro">Información documentada a partir de las aportaciones de artesanas y artesanos.</div>
        <div class="atlas-state-sheet-kpis"><div><strong>${aportaciones.toLocaleString('es-MX')}</strong><span>Aportaciones</span></div><div><strong>${registros.toLocaleString('es-MX')}</strong><span>Registros</span></div><div><strong>${comunidades.toLocaleString('es-MX')}</strong><span>Comunidades</span></div></div>
        ${mapBlockHtml(profile)}
        <div class="atlas-state-sheet-grid">
          <section class="atlas-state-sheet-section"><h3>Registros documentados</h3>
            ${entries.length ? `<div class="atlas-state-sheet-techs">${entries.map(item=>{
              const name = String(item).replace(/\s+\(\d+\)\s*$/, '').trim();
              const link = typeof tecnicasMap !== 'undefined' && tecnicasMap[name];
              return link ? `<button type="button" data-atlas-sheet-tech="${encodeURIComponent(name)}">${safe(item)} <span aria-hidden="true">↗</span></button>` : `<span>${safe(item)}</span>`;
            }).join('')}</div>` : '<p class="atlas-state-sheet-muted">Sin registros documentados.</p>'}
          </section>
          <section class="atlas-state-sheet-section"><h3>Municipios</h3>${chips(facets.municipalities,'municipios',profile.name)}</section>
          <section class="atlas-state-sheet-section"><h3>Comunidades</h3>${chips(facets.communities,'comunidades',profile.name)}</section>
          <section class="atlas-state-sheet-section"><h3>Materiales</h3>${chips(facets.materials,'materiales',profile.name)}</section>
          <section class="atlas-state-sheet-section"><h3>Pueblos indígenas identificados</h3>${chips(facets.pueblos,'pueblos',profile.name)}</section>
        </div>
        <section class="atlas-state-sheet-section atlas-state-sheet-media"><h3>Fotografías asociadas al estado</h3>${media}</section>
      </div></section>`;
  }

  function closeSheet() {
    if (!overlay || overlay.hidden) return;
    destroyStateMap();
    overlay.hidden = true;
    overlay.innerHTML = '';
    photosInModal = [];
    document.body.style.overflow = priorOverflow;
    returnFocus?.focus?.();
    returnFocus = null;
    // El panel derecho y la selección del mapa quedan intactos.
  }

  function openSheet(name) {
    const profile = profileForState(name);
    if (!profile) return;
    if (!overlay) {
      overlay = document.createElement('div');
      overlay.id = 'atlas-state-sheet-overlay';
      overlay.className = 'atlas-state-sheet-overlay';
      overlay.hidden = true;
      document.body.appendChild(overlay);
    }
    destroyStateMap();
    returnFocus = document.activeElement;
    priorOverflow = document.body.style.overflow;
    overlay.innerHTML = renderSheet(profile);
    overlay.hidden = false;
    document.body.style.overflow = 'hidden';
    overlay.querySelector('.atlas-state-sheet-dialog')?.focus();
    const token=stateMapToken;
    requestAnimationFrame(()=>initStateMap(profile,token));
  }

  document.addEventListener('click', event => {
    const launcher = event.target.closest?.('[data-atlas-state-modal]');
    if (launcher) {
      event.preventDefault();
      const state = decodeURIComponent(launcher.getAttribute('data-atlas-state-modal') || '');
      openSheet(state);
      return;
    }
    if (!overlay || overlay.hidden) return;
    if (event.target === overlay || event.target.closest?.('[data-atlas-sheet-close]')) {
      event.preventDefault(); closeSheet(); return;
    }
    const photoButton = event.target.closest?.('[data-atlas-sheet-photo]');
    if (photoButton) {
      const i = Number(photoButton.getAttribute('data-atlas-sheet-photo'));
      if (photosInModal[i] && typeof openLightbox === 'function') {
        const files = photosInModal.map(p => p.archivo);
        openLightbox(files[i], files, i);
      }
      return;
    }
    const filter = event.target.closest?.('[data-atlas-sheet-filter]');
    if (filter && typeof goToCatalogFilter === 'function') {
      const field = filter.getAttribute('data-atlas-sheet-filter') || '';
      const value = decodeURIComponent(filter.getAttribute('data-atlas-sheet-value') || '');
      const state = decodeURIComponent(filter.getAttribute('data-atlas-sheet-state') || '');
      event.preventDefault();
      closeSheet();
      goToCatalogFilter(field, value, state);
      return;
    }
    const tech = event.target.closest?.('[data-atlas-sheet-tech]');
    if (tech && typeof openFicha === 'function') {
      const name = decodeURIComponent(tech.getAttribute('data-atlas-sheet-tech') || '');
      closeSheet(); openFicha(name);
    }
  });

  document.addEventListener('click', event => {
    if (!overlay || overlay.hidden) return;
    if (event.target.closest?.('[data-atlas-mini-popup-close]')) {
      event.preventDefault();
      hideMapPopup();
      return;
    }
    if (event.target.closest?.('[data-atlas-mini-reset]')) {
      event.preventDefault();
      Object.values(stateMapSelection).forEach(selection => selection.clear());
      overlay.querySelectorAll('[data-atlas-mini-filter]').forEach(checkbox => { checkbox.checked = false; });
      overlay.querySelectorAll('.atlas-mini-filter[open]').forEach(detail => { detail.open = false; });
      syncStateMarkers();
    }
  });

  document.addEventListener('toggle', event => {
    if (event.target.matches?.('.atlas-mini-filter') && event.target.open) {
      overlay?.querySelectorAll('.atlas-mini-filter[open]').forEach(detail => {
        if (detail !== event.target) detail.open = false;
      });
      hideMapPopup();
    }
  }, true);

  document.addEventListener('change', event => {
    const checkbox=event.target.closest?.('[data-atlas-mini-filter]');
    if (!checkbox || !overlay || overlay.hidden) return;
    const group=checkbox.dataset.atlasMiniFilter;
    if (!stateMapSelection[group]) return;
    if (checkbox.checked) stateMapSelection[group].add(checkbox.value);
    else stateMapSelection[group].delete(checkbox.value);
    syncStateMarkers();
  });

  document.addEventListener('keydown', event => {
    if (!overlay || overlay.hidden) return;
    // Primero se debe cerrar el visor de fotografías si está abierto.
    if (event.key === 'Escape') {
      if (document.getElementById('lightbox')?.classList.contains('open')) return;
      event.preventDefault(); closeSheet();
    }
    if (event.key === 'Tab') {
      const focusable = [...overlay.querySelectorAll('button:not([disabled]), a[href], [tabindex="0"]')];
      if (!focusable.length) return;
      const first = focusable[0], last = focusable[focusable.length - 1];
      if (event.shiftKey && document.activeElement === first) {event.preventDefault();last.focus();}
      else if (!event.shiftKey && document.activeElement === last) {event.preventDefault();first.focus();}
    }
  });
})();
