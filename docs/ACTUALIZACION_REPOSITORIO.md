# Actualización del repositorio del Atlas

## Objetivo

Esta carpeta contiene la versión consolidada preparada para sustituir el estado de trabajo de los repositorios GitHub del Atlas. Incluye imágenes optimizadas WebP, la versión segura/documentada de la aplicación y el formulario adicional de bibliografía en la sección **Colabora**.

## Reemplazo recomendado

Para evitar que permanezcan recursos pesados u obsoletos de versiones anteriores:

1. Conserve la carpeta `.git/` de su clon local.
2. Elimine del árbol de trabajo la carpeta `imagenes/` anterior.
3. Elimine archivos de datos legados ubicados en la raíz si existen, en particular `data_by_record_id.csv`, `data_by_technique_id.csv`, `indice_imagenes.csv`, `mexico_municipios.geojson` y `mexico_municipios_centroids.geojson`. Sus versiones vigentes están en `data/` y `geodata/`.
4. Copie **todo el contenido** de esta carpeta consolidada a la raíz del repositorio, permitiendo sobrescribir archivos.
5. Ejecute `python .\scripts\security_check.py`.
6. Revise `git status` y confirme que las eliminaciones y nuevos archivos correspondan a la actualización.
7. Realice commit y push al remoto correspondiente.

## Imágenes

La colección de publicación se encuentra en `imagenes/` y utiliza archivos `.webp`. Las referencias de galería e índice de imágenes están contenidas en `data/galeria.json` y `data/indice_imagenes.csv`.

## GitHub Pages

El archivo `.github/workflows/pages.yml` implementa un despliegue de referencia para GitHub Pages con una verificación de seguridad previa. No impide ni modifica el despliegue del mismo contenido en otro servidor: los servidores web ajenos a GitHub ignoran `.github/workflows/`.

## Colabora

La interfaz contiene cuatro formularios Kobo: nueva técnica, modificación, comentario y envío de bibliografía. El formulario de bibliografía utiliza la misma ventana modal protegida que los formularios existentes.
