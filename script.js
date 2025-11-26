// --- Cambiar de sección ---
const buttons = document.querySelectorAll(".nav-btn");
const sections = document.querySelectorAll(".section");

buttons.forEach(btn => {
  btn.addEventListener("click", () => {
    // Quitar "active" de todos los botones
    buttons.forEach(b => b.classList.remove("active"));
    btn.classList.add("active");

    // Ocultar todas las secciones
    sections.forEach(sec => sec.classList.remove("active"));

    // Mostrar solo la correspondiente
    if (btn.id === "btn-maps") document.getElementById("maps-section").classList.add("active");
    if (btn.id === "btn-graphic") {
      document.getElementById("graphic-section").classList.add("active");
      renderDashboardBase(); // ← genera el dashboard
      cargarDatosLocales();  // ← carga datos desde Excel
    }
    if (btn.id === "btn-info") document.getElementById("info-section").classList.add("active");
    if (btn.id === "btn-send") document.getElementById("send-section").classList.add("active");
  });
});

// --- Renderizar estructura del dashboard ---
function renderDashboardBase() {
  const container = document.getElementById("graphic-section");
  container.innerHTML = `
    <h2>Dashboard general</h2>
    <div class="cards-container">
      <div class="card orange" id="card-tecnicas">Cargando...</div>
      <div class="card red" id="card-estados">Cargando...</div>
      <div class="card blue" id="card-municipios">Cargando...</div>
    </div>

    <div class="dashboard-container">
      <div class="chart-block"><h3>Técnicas por estado</h3><div class="chart-space">Espacio para gráfica</div></div>
      <div class="chart-block"><h3>Mapa coroplético</h3><div class="chart-space">Espacio para mapa</div></div>
      <div class="chart-block"><h3>Técnicas por municipio</h3><div class="chart-space">Espacio para gráfica</div></div>
      <div class="chart-block"><h3>Manufactura</h3><div class="chart-space">Gráfica de pie</div></div>
      <div class="chart-block"><h3>Conocimientos</h3><div class="chart-space">Gráfica de términos comunes</div></div>
      <div class="chart-block"><h3>Materiales comunes</h3><div class="chart-space">Gráfica de términos comunes</div></div>
      <div class="chart-block"><h3>Aprendizaje</h3><div class="chart-space">Gráfica de barras</div></div>
      <div class="chart-block"><h3>Enseñanza</h3><div class="chart-space">Gráfica de barras</div></div>
      <div class="chart-block"><h3>Tipo de teñido</h3><div class="chart-space">Gráfica de pie</div></div>
    </div>
  `;
}

// --- Función para cargar datos desde el archivo local ---
async function cargarDatosLocales() {
  try {
    const response = await fetch('C:/Users/ah_corona-amador/Documents/Banorte Bordadoras/Atlas Nacional de Técnicas/Plataforma/data/data.xlsx');
    if (!response.ok) throw new Error("No se pudo cargar el archivo Excel");

    const arrayBuffer = await response.arrayBuffer();
    const workbook = XLSX.read(arrayBuffer, { type: 'array' });
    const sheetName = workbook.SheetNames[0];
    const data = XLSX.utils.sheet_to_json(workbook.Sheets[sheetName]);

    // --- Cálculos (ajusta los nombres de columnas según el XLSX) ---
    const tecnicas = data.filter(r => r["Nombre_de_la_t_cnica_en_espa_ol"]).length;
    const estados = new Set(
      data.map(r => r["Estado"]).filter(e => e && e.trim() !== "")
    ).size;
    const municipios = new Set(
      data.map(r => r["Municipio"]).filter(e => e && e.trim() !== "")
    ).size;

    // --- Mostrar resultados en las tarjetas ---
    document.getElementById("card-tecnicas").textContent = `${tecnicas} técnicas`;
    document.getElementById("card-estados").textContent = `${estados} estados`;
    document.getElementById("card-municipios").textContent = `${municipios} municipios`;

    console.log("✅ Datos locales actualizados correctamente");
  } catch (error) {
    console.error("Error al cargar datos locales:", error);
    document.getElementById("card-tecnicas").textContent = "Error";
    document.getElementById("card-estados").textContent = "Error";
    document.getElementById("card-municipios").textContent = "Error";
  }
}

// --- Actualización automática cada 5 minutos (300,000 ms) ---
setInterval(() => {
  const activeSection = document.querySelector("#graphic-section.active");
  if (activeSection) {
    console.log("🔄 Actualizando datos del dashboard...");
    cargarDatosLocales();
  }
}, 5 * 60 * 1000);

