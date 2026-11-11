#!/usr/bin/env python3
"""Crea una carpeta de publicación con allowlist, sin respaldos ni herramientas internas."""
from pathlib import Path
import shutil,sys
ROOT=Path(__file__).resolve().parents[1]
DEST=ROOT/'_site'
FILE_ALLOW=['index.html','atlas_textil_v2.html','styles.css','main.js','map_drivers.js','state_profile_modal.js','pueblo_profile_modal.js','aluvial.js','atlas_info_embedded.js','atlas_plataforma_logos.png','.nojekyll']
DIR_ALLOW=['assets','data','geodata','imagenes','videos']
EXCLUDE={'revision_nombres_pendientes.csv','indice_imagenes.csv.bak_multimedia'}
if DEST.exists(): shutil.rmtree(DEST)
DEST.mkdir()
for name in FILE_ALLOW:
 p=ROOT/name
 if p.is_file(): shutil.copy2(p,DEST/name)
for name in DIR_ALLOW:
 src=ROOT/name
 if not src.is_dir(): continue
 for p in src.rglob('*'):
  if not p.is_file() or p.is_symlink(): continue
  if p.name in EXCLUDE or '.bak' in p.name or p.name.startswith('.') or any(x.startswith('respaldo_') for x in p.relative_to(src).parts): continue
  target=DEST/p.relative_to(ROOT)
  target.parent.mkdir(parents=True,exist_ok=True)
  shutil.copy2(p,target)
files=list(DEST.rglob('*'))
assert (DEST/'index.html').is_file()
assert all(not p.is_symlink() for p in files)
assert all('respaldo_' not in str(p) for p in files)
print(f'Sitio preparado: {DEST} | {sum(p.is_file() for p in files)} archivos | {sum(p.stat().st_size for p in files if p.is_file())/1048576:.1f} MiB')
