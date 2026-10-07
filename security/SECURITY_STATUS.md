# Estado de endurecimiento de seguridad

Este documento resume los controles aplicados en el repositorio y los puntos que deben validarse en el entorno final de publicación. El repositorio no presupone GitLab, GitHub Pages ni un servidor específico.

| ID | Estado | Medida / pendiente |
|---|---|---|
| F-01 | Requiere validación institucional | Confirmar MFA, privilegio mínimo y lista de usuarios autorizados en GitHub/hosting y KoboToolbox. |
| F-02 | Mitigado, sujeto a pruebas | Escape reforzado de valores dinámicos, protección de valores insertados en el DOM y CSP defensiva. Se recomienda mantener análisis estático y pruebas de seguridad en el entorno institucional. |
| F-03 | Mitigado | Los datasets públicos se reducen a una allowlist; no se depende de ocultar información sensible mediante JavaScript. |
| F-04 | Mitigado con riesgo residual | Se eliminan UUID, URLs Kobo, campos internos y datos personales no necesarios de los datasets públicos. La acreditación pública de la galería debe contar con base/autorización institucional. |
| F-05 | Verificado en repositorio | No se detectan tokens o credenciales Kobo; únicamente URLs públicas de formularios. |
| F-06 | Mitigado | CSV saneados y `scripts/security_check.py` detecta candidatos de Formula Injection. |
| F-07 | Requiere proceso institucional | Toda contribución Kobo debe revisarse antes de incorporarse a archivos públicos. |
| F-08 | Mitigado | Kobo se carga en iframe con `sandbox` y Referrer-Policy. |
| F-09 | Parcial | Dependencias externas están fijadas por versión y se usa SRI donde fue verificado. Se recomienda completar SRI o vendorizar dependencias restantes cuando sea viable. |
| F-10 | Parcial / hosting | Se incluye CSP defensiva y Referrer-Policy en HTML. HSTS, `nosniff`, Permissions-Policy, `frame-ancestors` y CSP HTTP deben validarse/configurarse en el hosting final. |
| F-11 | Requiere validación del hosting | Permisos de archivos/directorios, exposición de `.git` y usuario de despliegue dependen del entorno final. |
| F-12 | Mitigado en frontend | Las dependencias JavaScript están fijadas por versión. Cualquier herramienta de despliegue adicional deberá fijar también sus propias versiones. |
| F-13 | Parcial / infraestructura | Se incluye `scripts/security_check.py` y documentación de seguimiento. Logging, monitoreo, alertas, backups y respuesta a incidentes corresponden al entorno productivo. |

## Minimización aplicada

- `data/data_by_record_id.csv`: únicamente campos necesarios para visualización y reportes.
- `data/indice_imagenes.csv`: sin URLs originales de Kobo ni identificadores internos innecesarios.
- `data/data_by_technique_id.csv`: solo técnicas públicas, sin UUID ni scoring/ranking editorial.
- fichas bibliográficas: sin campos internos `notas` / `notas_revision`.
- `data/datos_territoriales.json`: sin `record_uuids`.
- `data/catalogo_materiales_atlas.csv`: únicamente columnas consumidas por la interfaz.

## Pendientes para cierre institucional

1. MFA y revisión de accesos en GitHub/hosting y KoboToolbox.
2. Confirmar autorización/base institucional para los créditos personales publicados en la galería.
3. Moderación Kobo antes de publicación.
4. Validar cabeceras HTTP reales en el entorno productivo.
5. Revisar permisos de hosting y evitar publicación de `.git`, secretos o archivos administrativos.
6. Logging, monitoreo, backups y procedimiento de respuesta a incidentes.
7. Completar SRI verificado o vendorizar dependencias restantes cuando sea viable.
