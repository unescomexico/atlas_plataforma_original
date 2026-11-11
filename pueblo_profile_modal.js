/* Atlas del Arte Textil de México — ficha ampliada por pueblo indígena.
 * Utiliza el MISMO perfil de referencia territorial que el panel derecho.
 * Los datos expresan proximidad geográfica a la capa lingüística, NO
 * adscripción étnica de las personas ni una distribución exclusiva del pueblo.
 * Intencionalmente no se renderiza un mapa en estas fichas. */
(() => {
  'use strict';
  let overlay = null;
  let focusBefore = null;
  let overflowBefore = '';
  let photosForCurrentSheet = [];

  const esc = v => String(v ?? '')
    .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
  const normalize = v => String(v ?? '').normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '').toLocaleLowerCase('es')
    .replace(/[^a-z0-9]+/g, ' ').trim();
  const normalizedState = v => {
    const base = typeof normalizeEstado === 'function' ? normalizeEstado(v) : v;
    return normalize(base).replace(/^coahuila de zaragoza$/, 'coahuila')
      .replace(/^michoacan de ocampo$/, 'michoacan')
      .replace(/^veracruz de ignacio de la llave$/, 'veracruz');
  };
  const stateMunicipalityKey = (estado, municipio) => `${normalizedState(estado)}|${normalize(municipio)}`;
  const count = v => Math.max(0, Number(v) || 0).toLocaleString('es-MX');
  const cleanArray = v => Array.isArray(v) ? [...new Set(v.map(x => String(x ?? '').trim()).filter(Boolean))] : [];

  // Esta es exactamente la misma instancia utilizada por el panel derecho.
  function findProfile(id) {
    return typeof territoryProfileData === 'undefined'
      ? null : territoryProfileData?.lengua?.[String(id)] || null;
  }
  function profileTitle(profile) {
    const cloud = typeof languageCloudData === 'undefined' ? [] : languageCloudData;
    const entry = (cloud || []).find(p => String(p.id) === String(profile.id));
    return entry?.pueblo_inpi || entry?.pueblo || profile.pueblo || profile.name || 'Pueblo indígena';
  }

  // Solo archivos cuya técnica Y municipio/estado coinciden con la lectura
  // geográfica. No constituyen evidencia de pertenencia a un pueblo indígena.
  function relatedPhotos(summary) {
    const items = typeof GALLERY_ITEMS !== 'undefined' && Array.isArray(GALLERY_ITEMS) ? GALLERY_ITEMS : [];
    if (!items.length) return [];
    const regionKeys = new Set(cleanArray(summary.community_keys).map(item => {
      const [state, mun] = item.split('||');
      return stateMunicipalityKey(state, mun);
    }));
    const names = new Set(cleanArray(summary.tecnicas).map(normalize));
    if (!regionKeys.size || !names.size) return [];
    const knownImages = window.ATLAS_AVAILABLE_IMAGES;
    const seen = new Set();
    const results = [];
    for (const item of items) {
      if (!item?.archivo || seen.has(item.archivo) || !item.estado || !item.municipio) continue;
      if (knownImages instanceof Set && !knownImages.has(item.archivo)) continue;
      if (!regionKeys.has(stateMunicipalityKey(item.estado, item.municipio))) continue;
      if (!names.has(normalize(item.tecnica))) continue;
      seen.add(item.archivo);
      results.push(item);
      if (results.length === 10) break;
    }
    return results;
  }

  function labelGroup(title, values, limit = 16) {
    const list = cleanArray(values);
    const tag = value => `<span>${esc(value)}</span>`;
    const initial = list.slice(0, limit).map(tag).join('');
    const remaining = list.length > limit
      ? `<details class="atlas-pueblo-sheet-more"><summary>Mostrar ${list.length - limit} más</summary>
           <div class="atlas-state-sheet-chips">${list.slice(limit).map(tag).join('')}</div></details>` : '';
    return `<section class="atlas-state-sheet-section"><h3>${esc(title)}</h3>
      ${list.length ? `<div class="atlas-state-sheet-chips">${initial}</div>${remaining}`
        : '<p class="atlas-state-sheet-muted">Sin información disponible</p>'}</section>`;
  }

  function techniqueGroup(summary) {
    const techniques = cleanArray(summary.tecnicas).sort((a, b) => a.localeCompare(b, 'es'));
    const makeChip = value => {
      const exists = typeof tecnicasMap !== 'undefined' && !!tecnicasMap[value];
      return exists
        ? `<button type="button" class="atlas-pueblo-tech-btn" data-pueblo-tech="${esc(encodeURIComponent(value))}">${esc(value)} <span aria-hidden="true">↗</span></button>`
        : `<span>${esc(value)}</span>`;
    };
    return `<section class="atlas-state-sheet-section atlas-pueblo-sheet-registros">
      <h3>Registros</h3>
      ${techniques.length ? `<div class="atlas-state-sheet-chips">${techniques.map(makeChip).join('')}</div>`
        : '<p class="atlas-state-sheet-muted">Sin información disponible</p>'}
    </section>`;
  }

  function photosGroup(photos) {
    if (!photos.length) return '';
    return `<section class="atlas-state-sheet-section atlas-state-sheet-media">
      <h3>Fotografías de registros en los territorios de referencia</h3>
      <div class="atlas-state-sheet-gallery">${photos.map((item, index) => {
        const url = typeof imgPath === 'function' ? imgPath(item.archivo) : `imagenes/${encodeURIComponent(item.archivo)}`;
        return `<button type="button" class="atlas-state-sheet-photo" data-pueblo-photo="${index}" title="Ampliar fotografía">
          <img src="${esc(url)}" loading="lazy" alt="${esc(item.tecnica || 'Registro textil')}" onerror="this.closest('button').remove()">
          <span>${esc(item.tecnica || 'Registro textil')}</span></button>`;
      }).join('')}</div></section>`;
  }

  function render(profile) {
    const summary = profile.summary || {};
    const name = profileTitle(profile);
    photosForCurrentSheet = relatedPhotos(summary);

    return `<section class="atlas-state-sheet-dialog" role="dialog" aria-modal="true" aria-labelledby="atlas-pueblo-sheet-title" tabindex="-1">
      <div class="atlas-state-sheet-top"><div>
        <div class="atlas-state-sheet-eyebrow">Atlas del Arte Textil de México · Perfil por pueblo indígena</div>
        <h2 id="atlas-pueblo-sheet-title">${esc(name)}</h2>
        <div class="atlas-pueblo-sheet-language"><strong>Lengua:</strong> ${esc(profile.name || 'Sin información disponible')}</div>
      </div><button type="button" class="atlas-state-sheet-close" data-pueblo-close aria-label="Cerrar ficha del pueblo indígena">×</button></div>
      <div class="atlas-state-sheet-scroll">
        <p class="atlas-state-sheet-intro">Este perfil amplía la lectura territorial que aparece en el panel derecho. Los registros se vinculan con la distribución geográfica de una lengua utilizada como referencia. No se atribuye pertenencia a un pueblo indígena a las personas ni a las comunidades registradas.</p>
        <div class="atlas-state-sheet-kpis">
          <div><strong>${count(summary.n_registros)}</strong><span>Aportaciones</span></div>
          <div><strong>${count(summary.n_comunidades)}</strong><span>Comunidades</span></div>
          <div><strong>${count(summary.n_tecnicas)}</strong><span>Registros documentados</span></div>
        </div>
        <div class="atlas-state-sheet-grid">
          ${techniqueGroup(summary)}
          ${labelGroup('Estados con aportaciones', summary.estados, 20)}
          ${labelGroup('Municipios de referencia', summary.municipios_reportados, 20)}
          ${labelGroup('Categorías de materiales', summary.categorias_material, 20)}
          ${labelGroup('Materiales mencionados', summary.materiales, 20)}
          ${labelGroup('Otras lenguas identificadas en los registros', summary.lenguas_mencionadas, 20)}
        </div>
        ${photosGroup(photosForCurrentSheet)}
      </div>
    </section>`;
  }

  function open(profile) {
    if (!profile) return;
    if (!overlay) {
      overlay = document.createElement('div');
      overlay.id = 'atlas-pueblo-sheet-overlay';
      overlay.className = 'atlas-state-sheet-overlay atlas-pueblo-sheet-overlay';
      overlay.hidden = true;
      document.body.appendChild(overlay);
    }
    focusBefore = document.activeElement;
    overflowBefore = document.body.style.overflow;
    overlay.innerHTML = render(profile);
    overlay.hidden = false;
    document.body.style.overflow = 'hidden';
    overlay.querySelector('.atlas-state-sheet-dialog')?.focus();
  }
  function close() {
    if (!overlay || overlay.hidden) return;
    overlay.hidden = true;
    overlay.replaceChildren();
    photosForCurrentSheet = [];
    document.body.style.overflow = overflowBefore;
    focusBefore?.focus?.();
    focusBefore = null;
  }

  document.addEventListener('click', event => {
    const launch = event.target.closest?.('[data-atlas-pueblo-modal]');
    if (launch) {
      event.preventDefault();
      open(findProfile(launch.dataset.atlasPuebloModal));
      return;
    }
    if (!overlay || overlay.hidden) return;
    if (event.target === overlay || event.target.closest?.('[data-pueblo-close]')) {
      event.preventDefault(); close(); return;
    }
    const tech = event.target.closest?.('[data-pueblo-tech]');
    if (tech && typeof openFicha === 'function') {
      event.preventDefault();
      const name = decodeURIComponent(tech.dataset.puebloTech || '');
      close(); openFicha(name); return;
    }
    const photo = event.target.closest?.('[data-pueblo-photo]');
    if (photo && typeof openLightbox === 'function') {
      event.preventDefault();
      const i = Number(photo.dataset.puebloPhoto);
      const files = photosForCurrentSheet.map(item => item.archivo);
      if (files[i]) openLightbox(files[i], files, i);
    }
  });
  document.addEventListener('keydown', event => {
    if (!overlay || overlay.hidden) return;
    if (event.key === 'Escape') {
      if (document.getElementById('lightbox')?.classList.contains('open')) return;
      event.preventDefault(); close(); return;
    }
    if (event.key !== 'Tab') return;
    const items = [...overlay.querySelectorAll('button:not([disabled]),a[href],summary,[tabindex="0"]')];
    if (!items.length) return;
    if (event.shiftKey && document.activeElement === items[0]) {
      event.preventDefault(); items[items.length - 1].focus();
    } else if (!event.shiftKey && document.activeElement === items[items.length - 1]) {
      event.preventDefault(); items[0].focus();
    }
  });
})();
