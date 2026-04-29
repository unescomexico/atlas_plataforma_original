# Atlas Nacional de Técnicas del Arte Textil

Plataforma de visualización interactiva del patrimonio textil mexicano, desarrollada en el marco del **Encuentro Nacional de Arte Textil ORIGINAL**, en colaboración con UNESCO México y la Secretaría de Cultura.

---

## Descripción

El Atlas documenta y clasifica **159 técnicas textiles** registradas a través de un ejercicio participativo con artesanas y artesanos de todo México. Los datos fueron recopilados en tres sedes —CDMX, Mérida y Tijuana— mediante fichas digitales de campo, y organizados en un esquema de clasificación jerárquica de cuatro niveles elaborado por expertas y expertos en textiles mexicanos.

El atlas no pretende ser exhaustivo: es una muestra viva de la diversidad técnica del arte textil en México, abierta a nuevas contribuciones.

---

## Datos

| Indicador | Valor |
|---|---|
| Técnicas documentadas | 159 |
| Aportaciones registradas | 638 |
| Estados representados | 29 |
| Municipios | 205 |
| Lenguas indígenas | 31 |

Los datos se distribuyen en tres archivos CSV que alimentan toda la plataforma en tiempo de ejecución:

| Archivo | Descripción |
|---|---|
| `data_by_technique_id.csv` | Una fila por técnica. Contiene clasificación experta (CAT-N-1 a CAT-N-4), número de fichas, estados, materiales, teñido, transmisión del conocimiento e imágenes. **Fuente principal de datos estructurados.** |
| `data_by_record_id.csv` | Una fila por ficha individual levantada en campo. Contiene lengua, municipio, estado, sede, género, aprendizaje y datos cualitativos. Usado para análisis de participación y testimonios. |
| `indice_imagenes.csv` | Índice de imágenes asociadas a cada técnica por ID. |

> **Principio de diseño:** ningún dato está embebido en el HTML ni en el JS. Toda la plataforma se construye leyendo los CSV mediante `fetch()` al cargar la página. Para actualizar el atlas, basta con actualizar los CSV.

---

## Estructura del proyecto

```
atlas/
├── index.html              # Plataforma principal del atlas
├── main.js                 # Lógica de la aplicación (mapa, catálogo, red, árbol, teñido)
├── styles.css              # Estilos globales
├── atlas_textil_v2.html    # Reporte "Acerca del Atlas" (metodología y datos)
├── data_by_technique_id.csv
├── data_by_record_id.csv
└── indice_imagenes.csv
```

---

## Vistas de la plataforma

### Mapa interactivo
Mapa coroplético de México donde cada estado se colorea según el número de técnicas documentadas. Incluye un panel lateral colapsable de filtros jerárquicos (categoría → subcategoría → tipo → variante → técnica) y un panel de detalle por estado con listado de técnicas.

### Catálogo de técnicas
Cuadrícula de fichas por técnica con imagen, categoría, estados y número de registros. Filtrable por categoría y búsqueda libre.

### Red de técnicas
Visualización de red (grafo de fuerza) que muestra relaciones entre técnicas y categorías. Interactiva: zoom, arrastre de nodos, filtro por categoría.

### Clasificación
Árbol jerárquico progresivo con los cuatro niveles de clasificación experta: Hilados, Teñidos, Tejido, Técnicas decorativas, Acabados.

### Teñido
Matriz de relaciones entre técnicas y tipos de teñido (plantas, animales, minerales). Filtrable por tipo de teñido.

### Acerca del Atlas (`atlas_textil_v2.html`)
Reporte completo con metodología, estadísticas de participación y gráficas analíticas. Todas las gráficas se generan dinámicamente desde los CSV.

---

## Uso local

Por usar `fetch()` para leer los CSV, la plataforma requiere un servidor HTTP local (no funciona abriendo el HTML directamente desde el sistema de archivos).

**Con la extensión Live Server de VS Code:** abrir la carpeta del proyecto y hacer clic en *Go Live*.

**Con Python:**
```bash
python3 -m http.server 8000
# Abrir http://localhost:8000
```

**Con Node.js:**
```bash
npx serve .
```

---

## Publicación en GitHub Pages

1. Subir todos los archivos al repositorio (rama `main` o carpeta `/docs`).
2. En *Settings → Pages*, seleccionar la rama y carpeta raíz.
3. GitHub Pages servirá el `index.html` como página principal.

Los CSV deben estar en la misma carpeta que el HTML para que los `fetch()` funcionen correctamente.

---

## Tecnologías

| Capa | Tecnología |
|---|---|
| Frontend | HTML, CSS, JavaScript (vanilla) |
| Mapas | [Leaflet.js](https://leafletjs.com/) 1.9.4 |
| Gráficas | [Chart.js](https://www.chartjs.org/) 4.4.0 |
| Datos | CSV cargado en tiempo de ejecución vía `fetch()` |
| Fuentes | Fraunces, DM Sans, Space Mono (Google Fonts) |
| GeoJSON | [angelnmara/geojson](https://github.com/angelnmara/geojson) — México estados |

---

## Contribuir al atlas

La plataforma incluye una sección de contribución integrada con formularios KoboToolbox para:

- **Registrar una nueva técnica** no documentada.
- **Proponer correcciones** a una técnica ya existente.
- **Enviar comentarios** generales.

---

## Créditos

Desarrollado en conexión con **UNESCO México**, **Secretaría de Cultura** y el **Encuentro Nacional de Arte Textil ORIGINAL**.

La clasificación jerárquica de técnicas fue elaborada por un equipo de expertas y expertos en textiles mexicanos. El registro de datos fue realizado de forma participativa con artesanas y artesanos en las tres sedes del evento.

---

## Licencia

Los datos y la plataforma son de uso público con fines de investigación, educación y salvaguardia del patrimonio cultural. Para otros usos, contactar a los organizadores del proyecto.
