# Checklist de transferencia y publicación

## Repositorio entregado

- [ ] `index.html` abre correctamente de forma local mediante servidor HTTP.
- [ ] Los recursos utilizan rutas relativas compatibles con subdirectorios.
- [ ] `python scripts/security_check.py` devuelve `SECURITY CHECK PASSED`.
- [ ] No existen secretos o credenciales en el repositorio.
- [ ] Los datasets públicos están minimizados.
- [ ] Los formularios Kobo usan `sandbox` y política de referrer.
- [ ] La documentación de seguridad está incluida.
- [ ] No se incluye configuración CI/CD activa de infraestructura de terceros.
- [ ] El workflow de GitHub Pages se entrega únicamente como ejemplo en `docs/examples/github-pages-pages.yml.example`.

## Antes de producción — equipo de infraestructura

- [ ] MFA y accesos administrativos revisados.
- [ ] Rama de publicación y permisos definidos.
- [ ] HTTPS habilitado y verificado.
- [ ] Dominio institucional configurado, si aplica.
- [ ] Cabeceras HTTP revisadas según `security/SECURITY_HEADERS.md`.
- [ ] Logging y monitoreo habilitados.
- [ ] Procedimiento de respaldo/recuperación definido.
- [ ] Flujo de moderación Kobo confirmado.

## Después de producción

- [ ] Mapa y carrusel territorial funcionan.
- [ ] Filtros funcionan.
- [ ] Fichas de técnicas y bibliografía funcionan.
- [ ] Clasificación funciona.
- [ ] Galería funciona.
- [ ] Red de técnicas funciona.
- [ ] Reporte funciona.
- [ ] Acerca del Atlas funciona.
- [ ] Formularios Kobo funcionan.
- [ ] Vista móvil revisada.
- [ ] Consola del navegador sin errores críticos.
