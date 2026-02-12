# generate_index_with_gallery_portable.R
# Genera tecnicas/index.html + tecnicas/galeria/* usando plantillas (sin JS/CSS embebidos en strings)
# Requisitos en el proyecto:
#   - csv/data_by_technique_app.csv
#   - csv/data_by_pic.csv
#   - imagenes/ (con los archivos físicos)
#   - plantillas/galeria.html, plantillas/galeria_styles.css, plantillas/galeria_app.js

library(readr)
library(dplyr)
library(stringr)
library(tibble)

if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")

# === Ajusta SOLO esta ruta (carpeta tecnicas de tu proyecto) ===
out_dir <- "C:/Users/ah_corona-amador/Documents/Banorte Bordadoras/Atlas Nacional de Técnicas/ATLAS_PLATAFORMA/tecnicas"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# === Helpers ===
safe_filename <- function(x) {
  x <- str_squish(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^a-z0-9]+", "_")
  x <- str_replace_all(x, "^_|_$", "")
  paste0(x, ".html")
}

fix_mojibake <- function(x) {
  x <- as.character(x)
  score <- function(s) ifelse(is.na(s), 999L, str_count(s, "Ã|Â|�"))
  a <- x
  b <- iconv(x, from = "latin1", to = "UTF-8")
  c <- iconv(iconv(x, from = "UTF-8", to = "latin1"), from = "latin1", to = "UTF-8")
  sa <- score(a); sb <- score(b); sc <- score(c)
  out <- a
  out[!is.na(sb) & sb < sa] <- b[!is.na(sb) & sb < sa]
  out[!is.na(sc) & sc < pmin(sa, sb, na.rm = TRUE)] <- c[!is.na(sc) & sc < pmin(sa, sb, na.rm = TRUE)]
  out
}

# === Proyecto portable ===
project_dir <- normalizePath(file.path(out_dir, ".."), winslash = "/", mustWork = FALSE)
csv_dir     <- file.path(project_dir, "csv")
images_dir  <- file.path(project_dir, "imagenes")
tpl_dir     <- file.path(project_dir, "plantillas")

data_path   <- file.path(csv_dir, "data_by_technique_app.csv")
pic_csv     <- file.path(csv_dir, "data_by_pic.csv")

if (!file.exists(data_path)) stop("No existe: ", data_path)
if (!file.exists(pic_csv))   stop("No existe: ", pic_csv)
if (!dir.exists(images_dir)) stop("No existe carpeta imagenes/: ", images_dir)

# ===============================
# 1) ÍNDICE DE TÉCNICAS
# ===============================
df <- readr::read_csv(data_path, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))

if (!"tecnica_norm" %in% names(df)) {
  stop("No existe la columna 'tecnica_norm' en: ", data_path,
       "\nColumnas disponibles: ", paste(names(df), collapse = ", "))
}

for (col in intersect(names(df), c("estados", "tecnica_grup", "tecnica_norm"))) {
  df[[col]] <- fix_mojibake(df[[col]])
}

tecnicas <- df %>%
  mutate(tecnica_norm = str_squish(as.character(tecnica_norm))) %>%
  filter(!is.na(tecnica_norm), tecnica_norm != "") %>%
  distinct(tecnica_norm) %>%
  pull(tecnica_norm) %>%
  sort()

wanted_cols <- c("estados", "tecnica_grup")
meta_cols <- intersect(names(df), wanted_cols)

meta <- df %>%
  mutate(tecnica_norm = str_squish(as.character(tecnica_norm))) %>%
  filter(!is.na(tecnica_norm), tecnica_norm != "") %>%
  group_by(tecnica_norm) %>%
  summarise(
    across(all_of(meta_cols), ~{
      vals <- unique(na.omit(str_squish(as.character(.x))))
      vals <- vals[vals != ""]
      paste(sort(vals), collapse = " | ")
    }),
    .groups = "drop"
  )

index_df <- tibble(tecnica_norm = tecnicas) %>%
  mutate(file = vapply(tecnica_norm, safe_filename, character(1))) %>%
  left_join(meta, by = "tecnica_norm") %>%
  mutate(exists = file.exists(file.path(out_dir, file)))

items_json <- jsonlite::toJSON(index_df, dataframe = "rows", auto_unbox = TRUE)

index_html <- paste0(
'<!doctype html>
<html lang="es">
<head>
  <meta charset="utf-8"/>
  <meta name="viewport" content="width=device-width,initial-scale=1"/>
  <title>Índice de técnicas</title>
  <style>
    :root{ --bg:#0b0d10; --card:#12161b; --text:#e9eef5; --muted:#a9b4c2; --line:#24303b; --accent:#7aa2ff; }
    body{ margin:0; font-family:system-ui,-apple-system,Segoe UI,Roboto,Ubuntu,Cantarell,Arial; background:var(--bg); color:var(--text); }
    .wrap{ max-width:1100px; margin:0 auto; padding:24px; }
    h1{ font-size:22px; margin:0 0 12px; }
    .toolbar{ display:grid; grid-template-columns: 1fr 240px 240px; gap:10px; margin:12px 0 18px; }
    input, select{
      width:100%; padding:10px 12px; border-radius:10px; border:1px solid var(--line);
      background:#0f1317; color:var(--text); outline:none;
    }
    .count{ color:var(--muted); font-size:13px; margin:6px 0 16px; }
    .grid{ display:grid; grid-template-columns: repeat(auto-fill, minmax(260px,1fr)); gap:12px; }
    .card{
      background:var(--card); border:1px solid var(--line); border-radius:14px; padding:14px;
      box-shadow:0 8px 20px rgba(0,0,0,.25);
    }
    .title{ font-weight:650; font-size:15px; margin:0 0 8px; line-height:1.25; }
    .meta{ font-size:12px; color:var(--muted); line-height:1.35; }
    .meta b{ color:#cfe0ff; font-weight:600; }
    a.btn{
      display:inline-block; margin-top:10px; padding:8px 10px; border-radius:10px;
      border:1px solid var(--line); color:var(--text); text-decoration:none;
      background:#0f1317;
    }
    a.btn:hover{ border-color:var(--accent); }
    .badge-miss{
      display:inline-block; margin-left:8px; font-size:11px; color:#ffd1d1;
      border:1px solid rgba(255,120,120,.35); padding:2px 6px; border-radius:999px;
      background: rgba(255,120,120,.08);
    }
    .empty{ color:var(--muted); padding:18px; border:1px dashed var(--line); border-radius:14px; }
    @media (max-width:860px){ .toolbar{ grid-template-columns: 1fr; } }
  </style>
</head>
<body>
  <div class="wrap">
    <h1>Índice de fichas por técnica</h1>

    <div style="margin:8px 0 0;">
      <a class="btn" href="./galeria/index.html">Abrir galería de imágenes</a>
    </div>

    <div class="toolbar">
      <input id="q" type="search" placeholder="Buscar técnica…">
      <select id="fEstado"><option value="">Estado (todos)</option></select>
      <select id="fGrupo"><option value="">Grupo (todos)</option></select>
    </div>

    <div class="count" id="count"></div>
    <div class="grid" id="grid"></div>
    <div class="empty" id="empty" style="display:none;">No hay resultados con esos filtros.</div>
  </div>

  <script>
    const ITEMS = ', items_json, ';

    function splitTokens(fieldKey, value){
      if (!value) return [];
      const sep = (fieldKey === "estados") ? "," : "|";
      return String(value).split(sep).map(x => x.trim()).filter(Boolean);
    }

    function tokenSet(fieldKey){
      const s = new Set();
      ITEMS.forEach(it => splitTokens(fieldKey, it[fieldKey]).forEach(x => s.add(x)));
      return Array.from(s).sort((a,b)=>a.localeCompare(b,"es"));
    }

    function matchesTokenField(fieldKey, fieldValue, selected){
      if (!selected) return true;
      return splitTokens(fieldKey, fieldValue).includes(selected);
    }

    const $q = document.getElementById("q");
    const $fE = document.getElementById("fEstado");
    const $fG = document.getElementById("fGrupo");
    const $grid = document.getElementById("grid");
    const $count = document.getElementById("count");
    const $empty = document.getElementById("empty");

    tokenSet("estados").forEach(v=>{
      const o=document.createElement("option"); o.value=v; o.textContent=v; $fE.appendChild(o);
    });
    tokenSet("tecnica_grup").forEach(v=>{
      const o=document.createElement("option"); o.value=v; o.textContent=v; $fG.appendChild(o);
    });

    function render(){
      const q = ($q.value || "").trim().toLowerCase();
      const selE = $fE.value;
      const selG = $fG.value;

      const out = ITEMS.filter(it=>{
        const name = (it.tecnica_norm || "").toLowerCase();
        if (q && !name.includes(q)) return false;

        const est = it["estados"] || "";
        const grp = it["tecnica_grup"] || "";

        if (!matchesTokenField("estados", est, selE)) return false;
        if (!matchesTokenField("tecnica_grup", grp, selG)) return false;

        return true;
      });

      $grid.innerHTML = "";
      $empty.style.display = out.length ? "none" : "block";
      $count.textContent = `${out.length} de ${ITEMS.length} técnicas`;

      out.forEach(it=>{
        const est = it["estados"] || "";
        const grp = it["tecnica_grup"] || "";
        const missing = (it.exists === false);

        const card = document.createElement("div");
        card.className = "card";
        card.innerHTML = `
          <div class="title">
            ${it.tecnica_norm}
            ${missing ? `<span class="badge-miss">no existe html</span>` : ``}
          </div>
          <div class="meta">
            ${grp ? `<div><b>Grupo:</b> ${grp}</div>` : ``}
            ${est ? `<div><b>Estados:</b> ${est}</div>` : ``}
          </div>
          ${missing ? `` : `<a class="btn" href="./${it.file}">Abrir ficha</a>`}
        `;
        $grid.appendChild(card);
      });
    }

    $q.addEventListener("input", render);
    $fE.addEventListener("change", render);
    $fG.addEventListener("change", render);

    render();
  </script>
</body>
</html>'
)

writeLines(index_html, file.path(out_dir, "index.html"), useBytes = TRUE)

# ===============================
# 2) GALERÍA (CSV -> JSON + plantillas)
# ===============================
pics <- readr::read_csv(pic_csv, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))

if (!all(c("ruta_archivo","archivo_descargado") %in% names(pics))) {
  stop("data_by_pic.csv debe incluir: ruta_archivo, archivo_descargado. Columnas: ",
       paste(names(pics), collapse=", "))
}

# Indexa archivos reales en /imagenes (portable)
img_files <- list.files(images_dir, recursive = TRUE, full.names = TRUE)
img_map <- tibble(fullpath = img_files) %>%
  mutate(
    fullpath = normalizePath(fullpath, winslash="/", mustWork = FALSE),
    relpath  = str_replace(fullpath, fixed(paste0(normalizePath(images_dir, winslash="/", mustWork = FALSE), "/")), ""),
    relpath  = str_replace_all(relpath, "^/+", ""),
    base     = basename(relpath),
    base_fix = fix_mojibake(base)
  ) %>%
  distinct(base, .keep_all = TRUE)

# helper: basename desde ruta del CSV
csv_base <- function(x){
  x <- as.character(x)
  x <- str_replace_all(x, "\\\\", "/")
  basename(x)
}

# Helper: obtiene la primera columna existente (si no existe, regresa NA)
get_col <- function(df, candidates){
  for (k in candidates){
    if (k %in% names(df)) return(as.character(df[[k]]))
  }
  rep(NA_character_, nrow(df))
}

# Prepara vectores (evita .data[[..]] cuando la columna no existe)
tecnica_vec   <- get_col(pics, c("Tecnica","tecnica","Técnica","TECNICA"))
estado_vec    <- get_col(pics, c("Estado","estado","ESTADO"))
municipio_vec <- get_col(pics, c("Municipio","municipio","MUNICIPIO"))

pics2 <- pics %>%
  mutate(
    ruta_archivo = as.character(ruta_archivo),
    archivo_descargado = as.character(archivo_descargado),
    base_ruta = csv_base(ruta_archivo),
    base_desc = csv_base(archivo_descargado),
    base_ruta_fix = fix_mojibake(base_ruta),
    base_desc_fix = fix_mojibake(base_desc),
    # técnica: lo que viene después del último "-"
    stem_any = str_replace(base_desc, "\\.[^.]*$", ""),
    tecnica_raw = str_extract(stem_any, "[^-]+$"),
    # Si el CSV trae técnica/estado/municipio, úsalo; si no, deriva técnica del nombre de archivo.
    tecnica = fix_mojibake(dplyr::coalesce(tecnica_vec, tecnica_raw)),
    estado = fix_mojibake(estado_vec),
    municipio = fix_mojibake(municipio_vec)
  )


gal_dir <- file.path(out_dir, "galeria")
dir.create(gal_dir, showWarnings = FALSE, recursive = TRUE)

# Resolver relpath: 1) match exacto por archivo_descargado, 2) por ruta_archivo, 3) por versión fix_mojibake
resolved <- pics2 %>%
  left_join(img_map %>% select(base, relpath), by = c("base_desc" = "base")) %>%
  left_join(img_map %>% select(base, relpath_ruta = relpath), by = c("base_ruta" = "base")) %>%
  left_join(img_map %>% select(base_fix, relpath_fix = relpath), by = c("base_desc_fix" = "base_fix")) %>%
  left_join(img_map %>% select(base_fix, relpath_fix2 = relpath), by = c("base_ruta_fix" = "base_fix")) %>%
  mutate(
    relpath = ifelse(!is.na(relpath), relpath,
              ifelse(!is.na(relpath_ruta), relpath_ruta,
              ifelse(!is.na(relpath_fix), relpath_fix,
              ifelse(!is.na(relpath_fix2), relpath_fix2, NA_character_))))
  ) %>%
  select(-relpath_ruta, -relpath_fix, -relpath_fix2)

# fallback por stem si sigue NA
if (any(is.na(resolved$relpath))) {
  img_map2 <- img_map %>%
    mutate(stem = str_replace(base, "\\.[^.]*$", "")) %>%
    group_by(stem) %>%
    summarise(relpath_stem = relpath[1], .groups="drop")

  resolved <- resolved %>%
    mutate(stem_csv = str_replace(base_desc, "\\.[^.]*$", "")) %>%
    left_join(img_map2, by = c("stem_csv" = "stem")) %>%
    mutate(relpath = ifelse(is.na(relpath), relpath_stem, relpath)) %>%
    select(-relpath_stem)
}

# JSON final para la galería (solo campos necesarios para UI)
img_index <- resolved %>%
  mutate(
    # Asegura que existan las columnas clave
    tecnica = fix_mojibake(as.character(tecnica)),
    estado = fix_mojibake(as.character(estado)),
    municipio = fix_mojibake(as.character(municipio))
  ) %>%
  select(relpath, tecnica, estado, municipio) %>%
  arrange(tecnica, estado, municipio, relpath)

json_path <- file.path(gal_dir, "indice_imagenes.json")
jsonlite::write_json(img_index, json_path, auto_unbox = TRUE, pretty = TRUE)

# Copia plantillas (sin reescribir JS/CSS en strings)
tpl_html <- file.path(tpl_dir, "galeria.html")
tpl_css  <- file.path(tpl_dir, "galeria_styles.css")

if (!file.exists(tpl_html) || !file.exists(tpl_css)) {
  stop("Faltan plantillas en: ", tpl_dir,
       "
Se esperan: galeria.html, galeria_styles.css")
}

file.copy(tpl_html, file.path(gal_dir, "index.html"), overwrite = TRUE)
file.copy(tpl_css,  file.path(gal_dir, "styles.css"), overwrite = TRUE)

# Genera app.js (robusto, sin depender de escapes raros) — SOLO muestra tecnica/estado/municipio
js_path <- file.path(gal_dir, "app.js")
js_lines <- c(
  "// Galería (generado por R) — usa indice_imagenes.json",
  "const IMAGES_BASE = '../../imagenes';",
  "const INDEX_JSON  = './indice_imagenes.json';",
  "",
  "const $grid  = document.getElementById('grid');",
  "const $count = document.getElementById('count');",
  "const $empty = document.getElementById('empty');",
  "",
  "// Filtros opcionales (si existen en tu plantilla)",
  "const $q  = document.getElementById('q');",
  "const $fT = document.getElementById('fTecnica');",
  "const $fE = document.getElementById('fEstado');",
  "const $fM = document.getElementById('fMunicipio');",
  "",
  "// Lightbox opcional (si existe en tu plantilla)",
  "const $lb        = document.getElementById('lb');",
  "const $lbBg      = document.getElementById('lb_bg');",
  "const $lbClose   = document.getElementById('lb_close');",
  "const $lbPrev    = document.getElementById('lb_prev');",
  "const $lbNext    = document.getElementById('lb_next');",
  "const $lbImg     = document.getElementById('lb_img');",
  "const $lbCounter = document.getElementById('lb_counter');",
  "const $lbCaption = document.getElementById('lb_caption');",
  "",
  "let ALL = [];",
  "let FILT = [];",
  "let CUR = 0;",
  "",
  "const norm = (s) => (s ?? '').toString().trim();",
  "const uniq = (arr) => [...new Set(arr.filter(Boolean))].sort((a,b)=>a.localeCompare(b,'es'));",
  "const imgSrc = (rel) => encodeURI(`${IMAGES_BASE}/${rel}`);",
  "",
  "function fillSelect(sel, values){",
  "  if (!sel) return;",
  "  // preserva el primer option (Todos) si existe",
  "  const keep0 = sel.options && sel.options.length ? sel.options[0].outerHTML : null;",
  "  sel.innerHTML = keep0 ? keep0 : \"<option value=''>Todos</option>\";",
  "  values.forEach(v=>{ const o=document.createElement('option'); o.value=v; o.textContent=v; sel.appendChild(o); });",
  "}",
  "",
  "function applyFilters(){",
  "  const q  = $q ? norm($q.value).toLowerCase() : '';",
  "  const t  = $fT ? $fT.value : '';",
  "  const e  = $fE ? $fE.value : '';",
  "  const m  = $fM ? $fM.value : '';",
  "",
  "  FILT = ALL.filter(r=>{",
  "    if (t && norm(r.tecnica) !== t) return false;",
  "    if (e && norm(r.estado) !== e) return false;",
  "    if (m && norm(r.municipio) !== m) return false;",
  "    if (q){",
  "      const blob = `${norm(r.tecnica)} ${norm(r.estado)} ${norm(r.municipio)}`.toLowerCase();",
  "      if (!blob.includes(q)) return false;",
  "    }",
  "    return true;",
  "  });",
  "  render();",
  "}",
  "",
  "function render(){",
  "  if (!$grid) return;",
  "  $grid.innerHTML = '';",
  "  if ($empty) $empty.style.display = FILT.length ? 'none' : 'block';",
  "  if ($count) $count.textContent = `${FILT.length} imágenes`;",
  "",
  "  FILT.forEach((r, i)=>{",
  "    const card = document.createElement('div');",
  "    card.className = 'card';",
  "    card.tabIndex = 0;",
  "",
  "    const img = document.createElement('img');",
  "    img.className = 'thumb';",
  "    img.loading = 'lazy';",
  "    img.alt = norm(r.tecnica) || 'Imagen';",
  "    img.src = imgSrc(r.relpath);",
  "",
  "    const body = document.createElement('div');",
  "    body.className = 'card_body';",
  "",
  "    const title = document.createElement('div');",
  "    title.className = 'title';",
  "    title.textContent = norm(r.tecnica) || '(Sin técnica)';",
  "",
  "    const meta = document.createElement('div');",
  "    meta.className = 'meta';",
  "    const e = norm(r.estado);",
  "    const m = norm(r.municipio);",
  "    meta.innerHTML = `${e ? `<div><b>Estado:</b> ${e}</div>` : ''}${m ? `<div><b>Municipio:</b> ${m}</div>` : ''}`;",
  "",
  "    body.appendChild(title);",
  "    body.appendChild(meta);",
  "    card.appendChild(img);",
  "    card.appendChild(body);",
  "",
  "    card.addEventListener('click', ()=> openLb(i));",
  "    card.addEventListener('keydown', (ev)=>{ if(ev.key==='Enter' || ev.key===' '){ ev.preventDefault(); openLb(i);} });",
  "",
  "    $grid.appendChild(card);",
  "  });",
  "}",
  "",
  "function hasLb(){ return $lb && $lbImg && $lbCaption && $lbCounter; }",
  "",
  "function openLb(i){",
  "  if (!hasLb()) return;",
  "  CUR = i;",
  "  updateLb();",
  "  $lb.style.display = 'grid';",
  "  document.body.style.overflow = 'hidden';",
  "}",
  "function closeLb(){",
  "  if (!hasLb()) return;",
  "  $lb.style.display = 'none';",
  "  document.body.style.overflow = '';",
  "}",
  "function prevLb(){ CUR = (CUR - 1 + FILT.length) % FILT.length; updateLb(); }",
  "function nextLb(){ CUR = (CUR + 1) % FILT.length; updateLb(); }",
  "function updateLb(){",
  "  const r = FILT[CUR];",
  "  $lbImg.src = imgSrc(r.relpath);",
  "  $lbImg.alt = norm(r.tecnica) || 'Imagen';",
  "  $lbCounter.textContent = `${CUR+1} / ${FILT.length}`;",
  "  const tec = norm(r.tecnica);",
  "  const est = norm(r.estado);",
  "  const mun = norm(r.municipio);",
  "  $lbCaption.innerHTML = `${tec ? `<div><b>Técnica:</b> ${tec}</div>` : ''}${est ? `<div><b>Estado:</b> ${est}</div>` : ''}${mun ? `<div><b>Municipio:</b> ${mun}</div>` : ''}`;",
  "}",
  "",
  "async function init(){",
  "  const res = await fetch(INDEX_JSON, { cache: 'no-store' });",
  "  if (!res.ok) throw new Error('No se pudo cargar indice_imagenes.json');",
  "  ALL = await res.json();",
  "  FILT = ALL.slice();",
  "  fillSelect($fT, uniq(ALL.map(r=>norm(r.tecnica)).filter(Boolean)));",
  "  fillSelect($fE, uniq(ALL.map(r=>norm(r.estado)).filter(Boolean)));",
  "  fillSelect($fM, uniq(ALL.map(r=>norm(r.municipio)).filter(Boolean)));",
  "  render();",
  "}",
  "",
  "if ($q) $q.addEventListener('input', ()=>{ clearTimeout(window.__qt); window.__qt=setTimeout(applyFilters, 120); });",
  "if ($fT) $fT.addEventListener('change', applyFilters);",
  "if ($fE) $fE.addEventListener('change', applyFilters);",
  "if ($fM) $fM.addEventListener('change', applyFilters);",
  "",
  "if ($lbBg) $lbBg.addEventListener('click', closeLb);",
  "if ($lbClose) $lbClose.addEventListener('click', closeLb);",
  "if ($lbPrev) $lbPrev.addEventListener('click', prevLb);",
  "if ($lbNext) $lbNext.addEventListener('click', nextLb);",
  "document.addEventListener('keydown', (ev)=>{",
  "  if (!hasLb() || $lb.style.display === 'none') return;",
  "  if (ev.key==='Escape') closeLb();",
  "  if (ev.key==='ArrowLeft') prevLb();",
  "  if (ev.key==='ArrowRight') nextLb();",
  "});",
  "",
  "init().catch(err=>{",
  "  console.error(err);",
  "  if ($grid) $grid.innerHTML = `<div class='empty'>Error: ${err.message}</div>`;",
  "  if ($count) $count.textContent = '0 imágenes';",
  "});"
)
writeLines(js_lines, js_path, useBytes = TRUE)

message("Índice generado: ", file.path(out_dir, "index.html"))
message("Galería generada: ", file.path(gal_dir, "index.html"))
message("Índice imágenes: ", json_path)