# ============================================================
# Script: Crear dataset resumido por técnica
# Origen: fichas individuales (data.csv)
# Salida: resumen por técnica (data_tecnicas_resumen.csv)
# ============================================================

# 1. Paquetes ------------------------------------------------

# install.packages("tidyverse")  # Ejecutar una sola vez si no lo tienes
library(tidyverse)
library(readr)

# 2. Rutas ---------------------------------------------------

ruta_entrada <- "C:/Users/ah_corona-amador/Documents/GitHub/atlas_fichas/data/data.csv"
ruta_salida  <- "C:/Users/ah_corona-amador/Documents/GitHub/atlas_fichas/data/data_tecnicas_resumen.csv"

# 3. Leer dataset original (UTF-8, para conservar acentos) ---

df <- readr::read_csv(
  file           = ruta_entrada,
  show_col_types = FALSE
)

# 4. Normalizar nombres de técnica, estado y género ----------

df <- df %>% 
  mutate(
    tecnica_norm = `Técnica` |> 
      stringr::str_squish() |> 
      stringr::str_to_sentence(),   # "Bordado tenango"
    
    estado_norm = Estado |> 
      stringr::str_squish() |> 
      stringr::str_to_title(),      # "Ciudad De México"
    
    genero_norm = Género |> 
      stringr::str_squish() |> 
      stringr::str_to_lower()       # "mujer", "hombre"
  )

# 5. Helper: columnas tipo casilla marcada -> indicador ------

to_indicator <- function(x) {
  as.integer(!is.na(x) & x != 0 & x != "0" & x != "No" & x != "NO" & x != "no")
}

# 6. Género --------------------------------------------------

df <- df %>% 
  mutate(
    es_mujer  = if_else(genero_norm == "mujer",  1L, 0L, missing = 0L),
    es_hombre = if_else(genero_norm == "hombre", 1L, 0L, missing = 0L)
  )

# 7. Demografía por técnica ----------------------------------

demografia_tecnica <- df %>% 
  group_by(tecnica_norm) %>% 
  summarise(
    n_fichas  = n(),
    n_mujeres = sum(es_mujer,  na.rm = TRUE),
    n_hombres = sum(es_hombre, na.rm = TRUE),
    edad_prom = mean(Edad, na.rm = TRUE),
    estados   = estado_norm %>% 
      na.omit() %>% 
      unique() %>% 
      sort() %>% 
      paste(collapse = ", "),
    .groups = "drop"
  )

# 8. Ceremonias: c1..c8 -> largo -> conteos ------------------

ceremony_cols <- paste0("c", 1:8)
ceremony_cols <- ceremony_cols[ceremony_cols %in% names(df)]

ceremonias_long <- df %>% 
  select(tecnica_norm, all_of(ceremony_cols)) %>% 
  pivot_longer(
    cols      = all_of(ceremony_cols),
    names_to  = "slot",
    values_to = "ceremonia"
  ) %>% 
  filter(!is.na(ceremonia), ceremonia != "")

ceremonias_count <- ceremonias_long %>% 
  group_by(tecnica_norm, ceremonia) %>% 
  summarise(
    n = n(),
    .groups = "drop_last"
  ) %>% 
  arrange(tecnica_norm, desc(n))

# Ceremonia principal
ceremonia_principal <- ceremonias_count %>% 
  group_by(tecnica_norm) %>% 
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  rename(
    ceremonia_principal   = ceremonia,
    n_ceremonia_principal = n
  )

# Todas las ceremonias como resumen de texto
ceremonias_lista <- ceremonias_count %>% 
  group_by(tecnica_norm) %>% 
  summarise(
    ceremonias_resumen = paste0(ceremonia, " (", n, ")", collapse = "; "),
    .groups = "drop"
  )

# 9. Aprendizaje de la técnica -------------------------------

# Columnas: Madre, Abuela, Tía, Hermana, Cuñada, Instructor, Apprendi de otro, Padre
aprendizaje_tecnica <- df %>% 
  group_by(tecnica_norm) %>% 
  summarise(
    n_aprendio_madre      = sum(to_indicator(`Madre`),            na.rm = TRUE),
    n_aprendio_abuela     = sum(to_indicator(`Abuela`),           na.rm = TRUE),
    n_aprendio_tia        = sum(to_indicator(`Tía`),              na.rm = TRUE),
    n_aprendio_hermana    = sum(to_indicator(`Hermana`),          na.rm = TRUE),
    n_aprendio_cunada     = sum(to_indicator(`Cuñada`),           na.rm = TRUE),
    n_aprendio_instructor = sum(to_indicator(`Instructor`),       na.rm = TRUE),
    n_aprendio_otro       = sum(to_indicator(`Apprendi de otro`), na.rm = TRUE),
    n_aprendio_padre      = sum(to_indicator(`Padre`),            na.rm = TRUE),
    .groups = "drop"
  )

# 10. Transmisión de la técnica ------------------------------

# Columnas: No he enseñado, Hijas, Nietos, Sobrinos, Pareja,
#           Estudiantes, Enseño otra, Hijos
transmision_tecnica <- df %>% 
  group_by(tecnica_norm) %>% 
  summarise(
    n_no_ha_ensenado = sum(to_indicator(`No he enseñado`), na.rm = TRUE),
    n_ens_hijas      = sum(to_indicator(`Hijas`),          na.rm = TRUE),
    n_ens_nietos     = sum(to_indicator(`Nietos`),         na.rm = TRUE),
    n_ens_sobrinos   = sum(to_indicator(`Sobrinos`),       na.rm = TRUE),
    n_ens_pareja     = sum(to_indicator(`Pareja`),         na.rm = TRUE),
    n_ens_estudiantes = sum(to_indicator(`Estudiantes`),   na.rm = TRUE),
    n_ens_otra       = sum(to_indicator(`Enseño otra`),    na.rm = TRUE),
    n_ens_hijos      = sum(to_indicator(`Hijos`),          na.rm = TRUE),
    .groups = "drop"
  )

# 11. Prendas: pr1..pr10, igual que ceremonias ---------------

pr_cols <- paste0("pr", 1:10)
pr_cols <- pr_cols[pr_cols %in% names(df)]

prendas_long <- df %>% 
  select(tecnica_norm, all_of(pr_cols)) %>% 
  pivot_longer(
    cols      = all_of(pr_cols),
    names_to  = "slot",
    values_to = "prenda"
  ) %>% 
  filter(!is.na(prenda), prenda != "")

prendas_count <- prendas_long %>% 
  group_by(tecnica_norm, prenda) %>% 
  summarise(
    n = n(),
    .groups = "drop_last"
  ) %>% 
  arrange(tecnica_norm, desc(n))

# Prenda principal
prenda_principal <- prendas_count %>% 
  group_by(tecnica_norm) %>% 
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  rename(
    prenda_principal   = prenda,
    n_prenda_principal = n
  )

# Todas las prendas como resumen de texto
prendas_lista <- prendas_count %>% 
  group_by(tecnica_norm) %>% 
  summarise(
    prendas_resumen = paste0(prenda, " (", n, ")", collapse = "; "),
    .groups = "drop"
  )

# 12. Manufactura: Mano, Pedal, Telar, Mixta, Otra -----------

manufactura_tecnica <- df %>% 
  group_by(tecnica_norm) %>% 
  summarise(
    n_man_mano  = sum(to_indicator(Mano),  na.rm = TRUE),
    n_man_pedal = sum(to_indicator(Pedal), na.rm = TRUE),
    n_man_telar = sum(to_indicator(Telar), na.rm = TRUE),
    n_man_mixta = sum(to_indicator(Mixta), na.rm = TRUE),
    n_man_otra  = sum(to_indicator(Otra),  na.rm = TRUE),
    .groups = "drop"
  )

# 13. Teñido: Plantas, Minerales, Animales, Otro -------------

tenido_tecnica <- df %>% 
  group_by(tecnica_norm) %>% 
  summarise(
    n_tenido_plantas   = sum(to_indicator(Plantas),   na.rm = TRUE),
    n_tenido_minerales = sum(to_indicator(Minerales), na.rm = TRUE),
    n_tenido_animales  = sum(to_indicator(Animales),  na.rm = TRUE),
    n_tenido_otro      = sum(to_indicator(Otro),      na.rm = TRUE),
    .groups = "drop"
  )

# 14. Unir todo en un solo dataset por técnica ---------------

tecnicas_final <- demografia_tecnica %>% 
  left_join(ceremonia_principal,   by = "tecnica_norm") %>% 
  left_join(ceremonias_lista,      by = "tecnica_norm") %>% 
  left_join(prenda_principal,      by = "tecnica_norm") %>% 
  left_join(prendas_lista,         by = "tecnica_norm") %>% 
  left_join(aprendizaje_tecnica,   by = "tecnica_norm") %>% 
  left_join(transmision_tecnica,   by = "tecnica_norm") %>% 
  left_join(manufactura_tecnica,   by = "tecnica_norm") %>% 
  left_join(tenido_tecnica,        by = "tecnica_norm")

# 15. Guardar (UTF-8) ----------------------------------------

# Para uso en R/Shiny:
readr::write_csv(tecnicas_final, ruta_salida)

# Si lo quieres abrir en Excel con acentos correctos:
# readr::write_excel_csv(tecnicas_final, ruta_salida)

cat("Listo. Dataset resumido por técnica guardado en:\n", ruta_salida, "\n")
