library(tidyverse)

# Ruta a la carpeta del proyecto
ruta_base <- "C:/Users/ah_corona-amador/Documents/GitHub/atlas_fichas"

# Listar todos los archivos de la carpeta pics
pics_dir <- file.path(ruta_base, "pics")

archivos <- list.files(pics_dir, pattern = "\\.(jpg|jpeg|png|gif)$",
                       full.names = FALSE)

# Función para extraer la técnica a partir del nombre "ID-Tecnica.ext"
extraer_tecnica <- function(nombre_archivo) {
  # Separar en "ID" y "Tecnica.ext"
  partes <- strsplit(nombre_archivo, "-", fixed = TRUE)[[1]]
  if (length(partes) < 2) return(NA_character_)
  
  # Tomar todo lo que viene después del primer guion por si hubiera más
  tecnica_con_ext <- paste(partes[-1], collapse = "-")
  
  # Quitar la extensión (.jpg, .png, etc.)
  tecnica_sin_ext <- tools::file_path_sans_ext(tecnica_con_ext)
  
  # Cambiar guiones bajos por espacios (Punto_de_cruz -> Punto de cruz)
  tecnica_limpia <- gsub("_", " ", tecnica_sin_ext)
  
  # Opcional: poner primera letra mayúscula y resto minúsculas
  stringr::str_to_sentence(tecnica_limpia)
}

fotos_df <- tibble(
  archivo  = file.path("pics", archivos),
  tecnica  = map_chr(archivos, extraer_tecnica)
) %>%
  filter(!is.na(tecnica))

# Crear carpeta data si no existe
dir.create(file.path(ruta_base, "data"), showWarnings = FALSE)

# Guardar CSV
ruta_csv_fotos <- file.path(ruta_base, "data", "fotos_tecnicas.csv")
readr::write_csv(fotos_df, ruta_csv_fotos)

ruta_csv_fotos
