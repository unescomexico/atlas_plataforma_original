# Atlas Comunitario del Arte Textil de México

Plataforma web estática para documentar y visualizar técnicas textiles registradas a partir de ORIGINAL, Encuentro Nacional de Arte Textil.

## Arquitectura

La aplicación utiliza principalmente:

- HTML5
- CSS3
- JavaScript
- CSV / JSON / GeoJSON
- WebP / PNG
- Leaflet
- Chart.js
- KoboToolbox para formularios de contribución

No requiere backend propio, base de datos, PHP, Node.js en producción, autenticación propia ni API propia.

## Estructura principal

- `index.html`: aplicación principal.
- `styles.css`: estilos.
- `main.js`: lógica principal.
- `aluvial.js`: explorador de relaciones de tres columnas, configurable con datos del catálogo.
- `map_drivers.js`: lógica territorial y del mapa.
- `data/`: datasets públicos minimizados.
- `geodata/`: capas territoriales.
- `imagenes/`: recursos fotográficos optimizados.
- `assets/`: iconos e imágenes de interfaz.
- `forms/`: formularios XML de referencia para KoboToolbox.
- `scripts/security_check.py`: revisión preventiva del repositorio.
- `security/`: estado y recomendaciones de seguridad.
- `docs/`: documentación de transferencia y despliegue recomendado.
- `docs/examples/github-pages-pages.yml.example`: workflow de referencia para GitHub Pages; no se ejecuta mientras permanezca fuera de `.github/workflows/`.

## Fichas bibliográficas

La plataforma integra una vista documental para técnicas y niveles de clasificación con información bibliográfica disponible. La ficha basada en testimonios continúa siendo la vista inicial.

Archivos utilizados:

- `data/fichas_bibliograficas_tecnicas.csv`
- `data/fichas_bibliograficas_taxonomia.csv`
- `assets/testimonio.png`
- `assets/bibliografia.png`

Los campos internos de revisión editorial no forman parte de la interfaz pública.

## Seguridad

Antes de una actualización o publicación se recomienda ejecutar:

```bash
python scripts/security_check.py
```

Resultado esperado:

```text
SECURITY CHECK PASSED
```

Consultar:

- `security/SECURITY_STATUS.md`
- `security/SECURITY_HEADERS.md`
- `docs/DEPLOYMENT_RECOMMENDED.md`
- `docs/HANDOVER_CHECKLIST.md`

## Despliegue

La entrega no activa una configuración CI/CD específica. La plataforma puede desplegarse en cualquier hosting capaz de servir archivos estáticos mediante HTTPS.

Se incluye un **workflow de referencia no activo** en `docs/examples/github-pages-pages.yml.example`. Si la Secretaría decide utilizar GitHub Pages con GitHub Actions, puede revisarlo y copiarlo a `.github/workflows/pages.yml`. Mientras permanezca en `docs/examples/`, GitHub no lo ejecutará.

Para GitHub Pages, servidor institucional u otra alternativa, consultar `docs/DEPLOYMENT_RECOMMENDED.md`.

## Estado consolidado para publicación

Esta versión consolida la plataforma preparada para GitHub con la colección completa de imágenes optimizadas en WebP, los controles de seguridad del repositorio y el cuarto formulario de colaboración para compartir bibliografía.

- Las imágenes públicas de la galería se sirven desde `imagenes/` en formato WebP.
- La sección **Colabora** incluye cuatro accesos Kobo: nueva técnica, modificación, comentario y bibliografía.
- El workflow `.github/workflows/pages.yml` puede utilizarse para GitHub Pages y ejecuta `scripts/security_check.py` antes del despliegue. Este workflow no condiciona otros métodos de alojamiento; fuera de GitHub puede ignorarse o excluirse del despliegue.
- Los manuales existentes se conservan en `manuales/`.

Antes de sustituir una versión anterior del repositorio, se recomienda eliminar la carpeta `imagenes/` previa y cualquier archivo de datos legado ubicado en la raíz que ya tenga su equivalente bajo `data/` o `geodata/`, para evitar conservar archivos pesados u obsoletos. Consulte `docs/ACTUALIZACION_REPOSITORIO.md`.

## Explorador de relaciones (diagrama aluvial)

El botón **Explorar relaciones** permite elegir tres campos del catálogo,
por ejemplo Registros → Estados → Ceremonias o Clasificación → Lenguas → Prendas.
Cada entrada con valores en las tres columnas participa una vez en el total,
y se distribuye proporcionalmente entre sus valores múltiples. Las bandas
se calculan entre columnas contiguas a nivel de entrada del catálogo.
**No deben interpretarse como coincidencias observadas dentro de una misma entrevista.**
Los nombres largos se agrupan visualmente en «Otros», pero permanecen en los datos.
La vista ofrece búsqueda, ejemplos y selección interactiva de nodos.
No utiliza servicios externos ni depende de una biblioteca de gráficos.

## Ajustes de interfaz (4 de noviembre de 2026)

- Leyenda contraíble visible como control compacto en móvil y separada del zoom al contraerse en escritorio.
- Botones laterales del panel territorial ocultos solo en dispositivos de hasta 960 píxeles.
- Clasificación: nota sobre carácter dinámico y retiro de indicadores laterales de subcategorías y registros.
- Explorador de relaciones: límites 10, 20, 30 o 40 por columna y sin encabezados/metodología bajo el gráfico.
- En «Acerca del Atlas», lenguaje orientado a registros, denominaciones y prácticas, conservando claves y categorías originales en los datos.

## Actualización de nomenclatura y enfoque de pueblos (4 de noviembre de 2026)

- La denominación pública es **Atlas Comunitario del Arte Textil de México**. La introducción proporcionada por el equipo se presenta íntegra, en orden, a través de las cuatro pantallas de apertura.
- La pestaña y el acceso desde las fichas a **Red de registros** están ocultos; la implementación original permanece en `main.js` y `index.html`, pero no se inicializa desde la navegación pública.
- La vista territorial antes titulada *Lengua* se presenta como **Pueblo indígena**. Conserva el origen cartográfico lingüístico y sus 66 subcapas, con correspondencias a denominaciones del INPI en `geodata/lenguas_cloud.json`.
- El archivo `data/correspondencia_lenguas_pueblos_inpi.csv` registra las 66 correspondencias, el nombre anterior y su estatus. **63** se vinculan a una denominación de referencia del catálogo, **2** son provisionales (Oluteco y Sayulteco respecto a Popoluca) y **1** queda por verificar (Teko). No se atribuye identidad o pertenencia de personas o comunidades a partir de estas capas.
- El selector y la leyenda agrupan las subcapas por pueblo de referencia, preservando el nombre lingüístico para transparencia en perfiles, búsquedas y datos originales.
- Fuente de nomenclatura: [Catálogo INPI, consulta por pueblo](https://catalogo.inpi.gob.mx/consulta-por-pueblo-2/). Antes de usar estas relaciones para investigación etnográfica o atribuir adscripción comunitaria, se requiere validación de las personas portadoras y/o información sobre autoidentificación.

### Archivos necesarios para esta actualización

`index.html`, `main.js`, `styles.css`, `map_drivers.js`, `aluvial.js`, `atlas_textil_v2.html`, `atlas_info_embedded.js`, `geodata/lenguas_cloud.json`, `data/correspondencia_lenguas_pueblos_inpi.csv`.

No se sustituyen fotografías, vídeos, el índice multimedia, otros conjuntos de datos o archivos de CI.
