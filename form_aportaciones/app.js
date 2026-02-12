// Formulario de aportaciones (maqueta)

const TEC_CSV_PATH = "../csv/data_by_technique_app.csv";

const $ = (id) => document.getElementById(id);

const btnComentario = $("btnComentario");
const btnCambio = $("btnCambio");
const btnNueva = $("btnNueva");

const panelComentario = $("panelComentario");
const panelCambio = $("panelCambio");
const panelNueva = $("panelNueva");

const formComentario = $("formComentario");
const formCambio = $("formCambio");
const formNueva = $("formNueva");

const selTecnica = $("selTecnica");
const qSelectTec = $("qSelectTec");

const filesImg = $("filesImg");
const filesVid = $("filesVid");
const filesImgHint = $("filesImgHint");
const filesVidHint = $("filesVidHint");

const toast = $("toast");

function showToast(msg){
  toast.textContent = msg;
  toast.classList.remove("hidden");
  clearTimeout(window.__toastT);
  window.__toastT = setTimeout(() => toast.classList.add("hidden"), 3200);
}

function setActive(btn){
  [btnComentario, btnCambio, btnNueva].forEach(b => b.classList.toggle("is-active", b === btn));
}

function showPanel(which){
  panelComentario.classList.toggle("hidden", which !== "comentario");
  panelCambio.classList.toggle("hidden", which !== "cambio");
  panelNueva.classList.toggle("hidden", which !== "nueva");
}

btnComentario.addEventListener("click", () => { setActive(btnComentario); showPanel("comentario"); });
btnCambio.addEventListener("click", () => { setActive(btnCambio); showPanel("cambio"); });
btnNueva.addEventListener("click", () => { setActive(btnNueva); showPanel("nueva"); });

document.querySelectorAll("[data-close]").forEach(el => {
  el.addEventListener("click", () => {
    setActive(null);
    showPanel("none");
  });
});

// ---- Envíos (mock) ----
function interceptSubmit(form, label){
  form.addEventListener("submit", (ev) => {
    ev.preventDefault();
    showToast(`✅ ${label}: Gracias. (Aún no se envía a servidor en esta versión)`);
  });
}
interceptSubmit(formComentario, "Comentario");
interceptSubmit(formCambio, "Sugerencia de cambio");
interceptSubmit(formNueva, "Nueva técnica");

// ---- File pickers (solo muestra nombres) ----
function listNames(input){
  const files = Array.from(input.files || []);
  if (!files.length) return "No se seleccionaron archivos.";
  const names = files.slice(0, 6).map(f => f.name);
  return names.join(" · ") + (files.length > 6 ? ` · +${files.length-6} más` : "");
}
filesImg.addEventListener("change", () => filesImgHint.textContent = listNames(filesImg));
filesVid.addEventListener("change", () => filesVidHint.textContent = listNames(filesVid));

// ---- Técnicas desde CSV ----
const norm = (v) => (v ?? "").toString().trim();

// mojibake (MichoacÃ¡n) -> Michoacán
function fixMojibake(s){
  s = norm(s);
  if (!s) return "";
  if (!/[ÃÂ�]/.test(s)) return s;
  try{ return decodeURIComponent(escape(s)); }catch{ return s; }
}

function loadTecnicas(){
  selTecnica.innerHTML = `<option value="">Cargando técnicas…</option>`;

  return new Promise((resolve, reject) => {
    Papa.parse(TEC_CSV_PATH, {
      download: true,
      header: true,
      skipEmptyLines: true,
      complete: (res) => resolve(res.data || []),
      error: (err) => reject(err)
    });
  }).then(rows => {
    const vals = Array.from(
      new Set(rows.map(r => fixMojibake(r["tecnica_norm"])).filter(Boolean))
    ).sort((a,b)=>a.localeCompare(b,"es"));

    if (!vals.length){
      selTecnica.innerHTML = `<option value="">No se encontraron técnicas (revisa el CSV)</option>`;
      return;
    }

    selTecnica.innerHTML = `<option value="">Selecciona una técnica…</option>`;
    for (const v of vals){
      const o = document.createElement("option");
      o.value = v;
      o.textContent = v;
      selTecnica.appendChild(o);
    }
  }).catch(err => {
    console.error(err);
    selTecnica.innerHTML = `<option value="">No se pudo cargar el listado (revisa TEC_CSV_PATH)</option>`;
  });
}

// filtro del select por búsqueda
qSelectTec.addEventListener("input", () => {
  const q = norm(qSelectTec.value).toLowerCase();
  const opts = Array.from(selTecnica.options);

  // mantiene la primera opción (placeholder)
  opts.forEach((o, i) => {
    if (i === 0) return;
    const show = !q || o.textContent.toLowerCase().includes(q);
    o.hidden = !show;
  });

  // si la opción actual quedó oculta, regresa al placeholder
  const selected = selTecnica.selectedOptions?.[0];
  if (selected && selected.hidden) selTecnica.value = "";
});

// init
loadTecnicas();
