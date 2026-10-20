from pathlib import Path
import csv, json, re, sys
ROOT = Path(__file__).resolve().parents[1]
errors=[]
def fail(m): errors.append(m)
def headers(rel):
    with (ROOT/rel).open(encoding='utf-8-sig', newline='') as f:
        return next(csv.reader(f), [])

rec=headers('data/data_by_record_id.csv')
for c in ['Artesano','Edad','Notas','Localidad','uuid','pic','video','Fecha','start','end']:
    if c in rec: fail(f'data_by_record_id.csv exposes forbidden column: {c}')
allowed={'Sede','Genero','tecnica_grupo','Tecnica','Lengua','Estado','Municipio','Materiales','Temporalidad','Plantas','Minerales','Animales','Madre','Abuela','Tia','Hermana','Cunada','Instructora','Padre','Hijas','Hijos','Nietos','Sobrinos','Pareja','Estudiantes'}
extra=set(rec)-allowed
if extra: fail(f'data_by_record_id.csv has unreviewed columns: {sorted(extra)}')
idx=headers('data/indice_imagenes.csv')
for c in ['url','Genero','Municipio','archivo_original','error_msg','id']:
    if c in idx: fail(f'indice_imagenes.csv exposes forbidden column: {c}')
for rel, forbidden in [('data/fichas_bibliograficas_tecnicas.csv',{'notas_revision'}),('data/fichas_bibliograficas_taxonomia.csv',{'notas'}),('data/data_by_technique_id.csv',{'uuid','score_calidad','score_volumen','score_total','ranking'})]:
    bad=set(headers(rel)) & forbidden
    if bad: fail(f'{rel} exposes internal columns: {sorted(bad)}')
for p in (ROOT/'data').glob('*.csv'):
    with p.open(encoding='utf-8-sig', newline='') as f:
        for rno,row in enumerate(csv.reader(f),1):
            for cno,val in enumerate(row,1):
                s=val.lstrip()
                if (s.startswith(('=','+','@')) or (s.startswith('-') and not re.fullmatch(r'-?\d+(?:\.\d+)?',s))) and not val.startswith("'"):
                    fail(f'Formula-injection candidate {p.name}:{rno}:{cno}')
territory=json.loads((ROOT/'data/datos_territoriales.json').read_text(encoding='utf-8'))
def has_key(o,k):
    if isinstance(o,dict): return k in o or any(has_key(v,k) for v in o.values())
    if isinstance(o,list): return any(has_key(v,k) for v in o)
    return False
if has_key(territory,'record_uuids'): fail('datos_territoriales.json exposes record_uuids')
html=(ROOT/'index.html').read_text(encoding='utf-8')
if 'sandbox="allow-forms allow-scripts allow-same-origin allow-popups allow-popups-to-escape-sandbox"' not in html: fail('Kobo iframe sandbox missing')
if 'Content-Security-Policy' not in html: fail('index.html CSP missing')
for h in ['sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY=','sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo=','sha512-GsLlZN/3F2ErC5ifS5QtgpiJtWd43JWSuIgh7mbzZ8zBps+dvLusV+eNQATqgA/HdeKFVgA5v3S/cIrLF7QnIg==']:
    if h not in html: fail('Expected SRI hash missing')
for p in list((ROOT/'data').glob('*'))+list((ROOT/'geodata').glob('*')):
    if not p.is_file(): continue
    try: txt=p.read_text(encoding='utf-8')
    except UnicodeDecodeError: continue
    if 'kc.kobotoolbox.org/media/original' in txt: fail(f'Kobo media URL exposed in {p.relative_to(ROOT)}')
if errors:
    print('SECURITY CHECK FAILED')
    for e in errors: print(' -',e)
    sys.exit(1)
print('SECURITY CHECK PASSED')
