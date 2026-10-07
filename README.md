# Atlas Nacional de Técnicas del Arte Textil

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
