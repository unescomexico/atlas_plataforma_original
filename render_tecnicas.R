# render_all_tecnicas_only.R
# Renderiza TODAS las fichas (una por técnica) a HTML en /tecnicas
# No genera índice.

library(rmarkdown)
library(readr)
library(dplyr)
library(stringr)

# === Ajusta rutas ===
rmd_file    <- "C:/Users/ah_corona-amador/Documents/Banorte Bordadoras/Atlas Nacional de Técnicas/ATLAS_PLATAFORMA/ficha.Rmd"
data_path   <- "C:/Users/ah_corona-amador/Documents/Banorte Bordadoras/Atlas Nacional de Técnicas/ATLAS_PLATAFORMA/csv/data_by_technique_app.csv"
record_path <- "C:/Users/ah_corona-amador/Documents/Banorte Bordadoras/Atlas Nacional de Técnicas/ATLAS_PLATAFORMA/csv/data_by_record_app.csv"

# === Proyecto / salida ===
project_dir <- normalizePath(dirname(rmd_file), winslash = "/", mustWork = TRUE)
out_dir <- file.path(project_dir, "tecnicas")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Opcional: fija WD en la raíz del proyecto para que rutas relativas del Rmd funcionen
old_wd <- getwd()
setwd(project_dir)
on.exit(setwd(old_wd), add = TRUE)

# === Lee dataset ===
df <- readr::read_csv(
  data_path,
  show_col_types = FALSE,
  locale = readr::locale(encoding = "UTF-8")
)

if (!"tecnica_norm" %in% names(df)) {
  stop("No existe la columna 'tecnica_norm' en ", data_path,
       "\nColumnas disponibles: ", paste(names(df), collapse = ", "))
}

# === Helpers ===
safe_filename <- function(x) {
  x <- str_squish(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^a-z0-9]+", "_")
  x <- str_replace_all(x, "^_|_$", "")
  paste0(x, ".html")
}

# ---- 1) Lista de técnicas ----
tecnicas <- df %>%
  mutate(tecnica_norm = str_squish(as.character(tecnica_norm))) %>%
  filter(!is.na(tecnica_norm), tecnica_norm != "") %>%
  distinct(tecnica_norm) %>%
  pull(tecnica_norm) %>%
  sort()

message("Técnicas a renderizar: ", length(tecnicas))
message("Salida HTML en: ", out_dir)

failed <- character(0)

# ---- 2) Render de fichas ----
for (tec in tecnicas) {
  out_file <- safe_filename(tec)
  message("Render: ", tec, " -> tecnicas/", out_file)

  ok <- tryCatch({
    rmarkdown::render(
      input = rmd_file,
      output_format = "html_document",
      output_file = out_file,
      output_dir  = out_dir,
      params = list(
        tecnica     = tec,
        data_path   = data_path,
        record_path = record_path
      ),
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    TRUE
  }, error = function(e) {
    message("  ERROR en '", tec, "': ", conditionMessage(e))
    FALSE
  })

  if (!ok) failed <- c(failed, tec)
}

# ---- 3) Resumen final ----
message("\nListo. Fichas HTML generadas en: ", out_dir)

if (length(failed) > 0) {
  message("\nTécnicas con error (", length(failed), "):")
  print(failed)
} else {
  message("Sin errores.")
}
