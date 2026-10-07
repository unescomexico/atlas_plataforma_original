# Despliegue recomendado — Atlas Nacional de Técnicas del Arte Textil

## 1. Alcance de esta entrega

El repositorio contiene una aplicación web **estática**. No requiere backend propio, base de datos, PHP, Node.js en producción ni servicios de aplicación persistentes.

La plataforma se sirve directamente a partir de:

- `index.html`
- `styles.css`
- `main.js`
- `map_drivers.js`
- archivos CSV, JSON y GeoJSON
- imágenes y otros recursos estáticos
- formularios externos de KoboToolbox embebidos desde `https://ee.kobotoolbox.org`

La configuración y operación del hosting corresponde al equipo responsable de la infraestructura de la Secretaría de Cultura.

## 2. Requisitos mínimos de hosting

El hosting debe:

1. servir archivos estáticos por **HTTPS**;
2. conservar codificación UTF-8;
3. respetar nombres y mayúsculas/minúsculas de los archivos;
4. servir correctamente los MIME types de HTML, CSS, JavaScript, CSV, JSON, GeoJSON, PNG y WebP;
5. permitir acceso de lectura a `assets/`, `data/`, `geodata/`, `imagenes/` y `forms/`;
6. no exponer carpetas de control de versiones como `.git/`;
7. aplicar las cabeceras recomendadas en `security/SECURITY_HEADERS.md` cuando la plataforma de hosting lo permita.

## 3. Opción recomendada si se utiliza GitHub Pages

Debido a que el Atlas no requiere proceso de compilación, la opción más sencilla es publicar desde una rama del repositorio.

Flujo recomendado:

1. Subir los archivos a la rama definida para producción, por ejemplo `main`.
2. En GitHub: **Settings → Pages**.
3. Elegir como origen de publicación la rama correspondiente y la carpeta raíz `/`.
4. Guardar la configuración y esperar la publicación.
5. Activar **Enforce HTTPS** cuando la opción esté disponible.
6. Verificar la URL publicada y ejecutar la lista de pruebas de la sección 6.

El archivo `.nojekyll` incluido en la raíz indica a GitHub Pages que el contenido debe servirse como archivos estáticos sin procesamiento Jekyll.

### Dominio personalizado

Si se utiliza un dominio institucional, por ejemplo `atlas.cultura.gob.mx`, el equipo responsable deberá configurar el dominio en **Settings → Pages** y realizar los cambios DNS correspondientes. Se recomienda verificar el dominio dentro de la organización de GitHub antes de utilizarlo.

## 4. Opción alternativa: GitHub Actions

La Secretaría puede optar por un workflow propio de GitHub Actions. Esto permite ejecutar controles antes de publicar y bloquear el despliegue si alguna validación falla.

La entrega incluye un **workflow de referencia no activo** en:

```text
docs/examples/github-pages-pages.yml.example
```

Ese archivo no se ejecuta automáticamente porque GitHub solo interpreta workflows ubicados dentro de `.github/workflows/`.

Si el equipo de infraestructura decide utilizarlo, el proceso recomendado es:

1. revisar el contenido del archivo de ejemplo;
2. confirmar la rama de producción y las reglas de aprobación;
3. copiarlo como `.github/workflows/pages.yml`;
4. configurar GitHub Pages para utilizar GitHub Actions como origen de publicación;
5. ejecutar el workflow manualmente o realizar un cambio aprobado en `main`;
6. comprobar que `security_check.py` se ejecute antes del job de despliegue;
7. validar la URL resultante y ejecutar las pruebas de la sección 6.

El flujo de referencia es:

```text
Push / ejecución manual
        ↓
python scripts/security_check.py
        ↓
si falla → no se publica
        ↓
configuración de GitHub Pages
        ↓
carga del sitio estático
        ↓
despliegue
```

El ejemplo utiliza las acciones oficiales de Pages (`actions/configure-pages`, `actions/upload-pages-artifact` y `actions/deploy-pages`) y concede los permisos `pages: write` e `id-token: write` únicamente al job de despliegue.

La decisión de activar este workflow, modificarlo o utilizar otro mecanismo corresponde al equipo responsable de la infraestructura de la Secretaría de Cultura.

## 5. Revisión de seguridad antes de publicar

Desde la raíz del repositorio:

```bash
python scripts/security_check.py
```

En Windows también puede ejecutarse con:

```powershell
py .\scripts\security_check.py
```

El resultado esperado es:

```text
SECURITY CHECK PASSED
```

Este script es un control preventivo y no sustituye una auditoría de infraestructura.

## 6. Pruebas funcionales posteriores al despliegue

Después de publicar, verificar como mínimo:

- carga completa de la página principal;
- mapa interactivo y cambio entre Estado, Lengua y Ecosistema;
- carrusel territorial del panel derecho;
- filtros y botón Limpiar;
- Catálogo de técnicas;
- fichas testimoniales y bibliográficas;
- Clasificación y modal bibliográfico;
- Galería e imágenes;
- Red de técnicas;
- Reporte y generación de documentos;
- formularios de Contribuir mediante KoboToolbox;
- modal **Acerca del Atlas**;
- funcionamiento en teléfono móvil y tablet;
- ausencia de errores 404 en consola/red;
- HTTPS válido y sin contenido mixto.

## 7. Controles que corresponden a infraestructura

Antes de considerar el despliegue productivo cerrado, el equipo responsable debe validar:

- MFA y principio de menor privilegio en GitHub y cuentas administrativas;
- protección de ramas y reglas de aprobación, si aplican;
- HTTPS y dominio institucional;
- cabeceras HTTP de seguridad;
- permisos y exposición de archivos en el hosting;
- logging, monitoreo, alertas y respuesta a incidentes;
- backups o procedimiento de recuperación;
- moderación de nuevas contribuciones de Kobo antes de incorporarlas a los archivos públicos.

## 8. Qué no debe publicarse

No incorporar al repositorio público:

- contraseñas;
- tokens;
- API keys;
- llaves SSH;
- archivos `.env` con secretos;
- exportaciones originales de Kobo con datos personales innecesarios;
- dumps de bases de datos;
- configuraciones internas de servidores o redes privadas.
