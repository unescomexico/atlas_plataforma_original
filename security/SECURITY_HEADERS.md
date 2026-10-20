# Cabeceras de seguridad recomendadas para producción

La aplicación incluye CSP y Referrer-Policy mediante HTML como defensa adicional. Las siguientes protecciones deben configurarse y comprobarse también como **cabeceras HTTP del servidor/reverse proxy**:

```text
Strict-Transport-Security: max-age=31536000; includeSubDomains
X-Content-Type-Options: nosniff
Referrer-Policy: strict-origin-when-cross-origin
Permissions-Policy: camera=(), microphone=(), geolocation=(), payment=(), usb=()
Cross-Origin-Opener-Policy: same-origin
Content-Security-Policy: default-src 'self'; base-uri 'self'; object-src 'none'; script-src 'self' 'unsafe-inline' https://unpkg.com https://cdnjs.cloudflare.com; style-src 'self' 'unsafe-inline' https://fonts.googleapis.com https://unpkg.com; font-src 'self' https://fonts.gstatic.com data:; img-src 'self' data: blob: https:; connect-src 'self' https://raw.githubusercontent.com; frame-src 'self' https://ee.kobotoolbox.org; form-action 'self' https://ee.kobotoolbox.org; worker-src 'self' blob:; frame-ancestors 'self'; upgrade-insecure-requests
```

La aplicación todavía contiene manejadores y estilos inline, por lo que la política conserva `'unsafe-inline'`. Una fase posterior puede migrarlos a `addEventListener`/CSS para eliminar esa excepción.
