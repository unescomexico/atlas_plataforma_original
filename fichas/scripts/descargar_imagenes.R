# ============================================================
# Script: Descargar imágenes desde URLs de Kobo
# Guarda cada imagen con el nombre "_id-Técnica.ext"
# e informa cuántas se descargaron.
# ============================================================

library(tidyverse)
library(httr)
library(fs)

# 1. Parámetros ----------------------------------------------

ruta_csv    <- "C:/Users/ah_corona-amador/Documents/GitHub/atlas_fichas/data/data.csv"
carpeta_out <- "C:/Users/ah_corona-amador/Documents/GitHub/atlas_fichas/imagenes_kobo"

# Nombre EXACTO de las columnas en tu CSV
col_id      <- "_id"      # ID interno de Kobo
col_tecnica <- "Técnica"  # nombre de la técnica
col_imagen  <- "pic"      # columna con la URL de la imagen

# API:
kobo_token <- "xxxxxxxxxxxxxxxxxxxxxxxxxxx"

# 2. Crear carpeta de salida si no existe --------------------

fs::dir_create(carpeta_out)

# 3. Leer CSV ------------------------------------------------

df <- readr::read_csv(ruta_csv, show_col_types = FALSE)

# 4. Seleccionar _id, Técnica y URL de imagen ----------------

imgs <- df %>% 
  select(
    id      = all_of(col_id),
    tecnica = all_of(col_tecnica),
    url     = all_of(col_imagen)
  ) %>% 
  filter(!is.na(url), url != "")

n_registros_con_url <- nrow(imgs)
message("Registros con URL de imagen no vacía: ", n_registros_con_url)
message("IDs únicos: ", length(unique(imgs$id)))

# 5. Función para hacer el nombre de archivo "seguro" --------

make_safe_filename <- function(x) {
  x %>% 
    as.character() %>% 
    stringr::str_squish() %>%                    # quita espacios extra
    stringr::str_replace_all("[/\\\\]", "_") %>% # reemplaza / y \ por _
    stringr::str_replace_all("[[:space:]]+", "_")# espacios -> _
}

# 6. Función para descargar una imagen (siempre con token) ---

descargar_imagen <- function(id_val, tecnica_val, url, carpeta_out, token) {
  # Nombre base: _id-Técnica
  base_name <- make_safe_filename(
    paste(id_val, tecnica_val, sep = "-")
  )
  
  # Intentar obtener extensión desde la URL
  ext <- tools::file_ext(url)
  if (ext == "") {
    # Si la URL de la API no trae extensión, asumimos jpg
    ext <- "jpg"
  }
  
  archivo_destino <- file.path(carpeta_out, paste0(base_name, ".", ext))
  
  # Llamada a la API de Kobo con Authorization: Token <token>
  res <- httr::GET(
    url,
    httr::add_headers(Authorization = paste("Token", token)),
    httr::write_disk(archivo_destino, overwrite = TRUE),
    httr::timeout(60)
  )
  
  # Si la respuesta no es 200, lanza error
  httr::stop_for_status(res)
  
  archivo_destino
}

# 7. Bucle de descarga con contadores ------------------------

n_intentos <- 0L
n_exitos   <- 0L
n_fallas   <- 0L

for (i in seq_len(nrow(imgs))) {
  id_val      <- imgs$id[i]
  tecnica_val <- imgs$tecnica[i]
  url         <- imgs$url[i]
  
  if (is.na(url) || url == "") next
  
  n_intentos <- n_intentos + 1L
  
  tryCatch(
    {
      archivo_destino <- descargar_imagen(id_val, tecnica_val, url, carpeta_out, token = kobo_token)
      n_exitos <- n_exitos + 1L
      message(
        "[", n_intentos, "/", n_registros_con_url, 
        "] OK: _id = ", id_val, 
        " | Técnica = ", tecnica_val,
        " → ", archivo_destino
      )
    },
    error = function(e) {
      n_fallas <- n_fallas + 1L
      message(
        "[", n_intentos, "/", n_registros_con_url, 
        "] ERROR en _id = ", id_val, 
        " | Técnica = ", tecnica_val,
        ": ", conditionMessage(e)
      )
    }
  )
}

# 8. Resumen final -------------------------------------------

message("=======================================")
message("Resumen de descarga de imágenes")
message("Registros con URL válida: ", n_registros_con_url)
message("Intentos de descarga:     ", n_intentos)
message("Descargas exitosas:       ", n_exitos)
message("Descargas con error:      ", n_fallas)
message("Carpeta de salida:        ", carpeta_out)
message("=======================================")