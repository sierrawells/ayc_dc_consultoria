#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, data.table, here, googledrive, janitor)

# Files
drive_auth(email = "sierra.wells@datacivica.org")

id_indesol <- drive_ls(as_id("1aEn_E1afgQLEwqxvj3YdKjjmWzFT89hw"),
                       pattern = ".csv") %>% 
  pull(id)

stopifnot(length(id_indesol) == 1)

drive_out <- as_id("1OSsKmIjuCKkFWokAiI1f9808rZYM3Udu")

# READ REGISTRO INDESOL =====

drive_download(id_indesol,
               path = paste0(tempdir(), "/indesol.csv"),
               overwrite = TRUE)

indesol_raw <- fread(paste0(tempdir(), "/indesol.csv"),
                     encoding = "Latin-1",
                     na.strings = c("NA", "N/A"))

stopifnot(nrow(indesol_raw) > 45e3)

file.remove(paste0(tempdir(), "/indesol.csv"))

# CLEAN REGISTRO ====

indesol_pre <- indesol_raw %>% 
  clean_names() %>% 
  mutate(
    across(ends_with("_presentado"), 
           ~ case_when(.x == "SI" ~ 1,
                       .x == "NO" ~ 0,
                       TRUE ~ NA)),
    across(starts_with("estatus"),
           ~str_to_sentence(.x)),
    estatus = ifelse(estatus == "Disolucion", "Disolución", estatus),
    anio_constitucion = as.numeric(format(fecha_de_constitucion, "%Y")),
    anio_inscripcion_indesol = as.numeric(format(fecha_de_inscripcion, "%Y"))) %>% 
  rename(estatus_indesol = estatus,
         estatus_representacion = estatus_de_la_representacion) %>% 
  select(rfc, estatus_indesol, estatus_representacion,
         starts_with("anio_"), ends_with("_presentado")) %>% 
  distinct()

df_ultimo_anio_informe <- indesol_pre %>% 
  select(rfc, ends_with("_presentado")) %>%
  pivot_longer(
    cols = ends_with("_presentado"),
    names_to = "anio",
    values_to = "informe_presentado") %>% 
  mutate(anio = str_extract(anio, "\\d{4}")) %>% 
  filter(informe_presentado == 1) %>%
  group_by(rfc) %>% 
  summarize(
    ultimo_anio_informe_indesol = max(as.integer(anio))) %>% 
  ungroup()

indesol_w_ultimo_anio <- indesol_pre %>%
  left_join(df_ultimo_anio_informe, by = "rfc") %>% 
  mutate(
    ultimo_anio_informe_indesol = as.character(ultimo_anio_informe_indesol),
    ultimo_anio_informe_indesol = case_when(
      is.na(ultimo_anio_informe_indesol) & as.numeric(anio_inscripcion_indesol) >= 2024 ~ NA_character_,
      is.na(ultimo_anio_informe_indesol) & as.numeric(anio_inscripcion_indesol) < 2024 ~ "Nunca",
      T ~ ultimo_anio_informe_indesol))

cols_to_invert <- indesol_w_ultimo_anio %>% 
  select(informe_2005_presentado:ultimo_anio_informe_indesol) %>%
  names()

indesol_col_order <- c(
  "rfc", "estatus_indesol", "estatus_representacion",
  "anio_constitucion", "anio_inscripcion_indesol",
  rev(cols_to_invert))

indesol_w_ultimo_anio <- indesol_w_ultimo_anio %>%
  select(all_of(indesol_col_order))
  
# ELIMINATE DUPLICATES ====

# Create df with duplicates
duplicates_rfc <- indesol_w_ultimo_anio %>% 
  group_by(rfc) %>% 
  filter(n() > 1) %>% 
  pull(rfc)

# Collapse duplicates
duplicates_rfc_clean <- indesol_w_ultimo_anio %>% 
  filter(rfc %in% duplicates_rfc) %>% 
  group_by(rfc) %>% 
  mutate(
    across(.cols = ends_with("_presentado"),
           .fns = ~ case_when(
             sum(.x, na.rm = TRUE) > 0 ~ 1,
             sum(!is.na(.x)) > 0 ~ 0,
             TRUE ~ NA)),
    estatus_representacion = case_when(
      sum(estatus_representacion == "Vencida") > 0 ~ "Vencida",
      sum(estatus_representacion == "Vigente") > 0 ~ "Vigente",
      TRUE ~ NA_character_),
    estatus_indesol = case_when(
      sum(estatus_indesol == "Disolución") > 0 ~ "Disolución",
      sum(estatus_indesol == "Inactiva") > 0 ~ "Inactiva",
      sum(estatus_indesol == "Activa") > 0 ~ "Activa",
      sum(estatus_indesol == "Activa condicionada") > 0 ~ "Activa condicionada"),
    anio_constitucion = min(anio_constitucion, na.rm = T),
    anio_inscripcion_indesol = min(anio_inscripcion_indesol, na.rm = T)) %>% 
  distinct()

stopifnot(length(unique(duplicates_rfc)) == nrow(duplicates_rfc_clean))

# Replace duplicates
indesol_w_o_duplicates <- indesol_w_ultimo_anio %>% 
  filter(!rfc %in% duplicates_rfc) %>% 
  bind_rows(duplicates_rfc_clean) %>% 
  rename_with(.cols = ends_with("_presentado"),
              .fn = ~ paste0(., "_indesol"))

stopifnot(
  nrow(indesol_w_o_duplicates) == length(unique(indesol_w_o_duplicates$rfc)),
  all(!is.na(indesol_w_o_duplicates$estatus_indesol)),
  all(!is.na(indesol_w_o_duplicates$estatus_representacion)),
  "anio_constitucion" %in% names(indesol_w_o_duplicates),
  "anio_inscripcion_indesol" %in% names(indesol_w_o_duplicates))
  
# EXPORT DATA ====

write_csv(indesol_w_o_duplicates,
          paste0(tempdir(), "/indesol_status.csv"))

drive_upload(
  paste0(tempdir(), "/indesol_status.csv"),
  path = drive_out,
  overwrite = TRUE)

file.remove(paste0(tempdir(), "/indesol_status.csv"))

# done.
