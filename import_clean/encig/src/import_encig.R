# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2024 ©
# ---------------------------------------------

# TODO

# Packages

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, R.utils, janitor, foreign, here, yaml, googledrive,
               purrr, readxl)

# Files
paths <- list(
  vars_rename = here("encig/import_clean/hand/vars_rename.yaml"),
  vars_to_keep = here("encig/import_clean/hand/vars_to_keep.yaml"),
  catalogo_entidades = here("encig/import_clean/hand/catalogo_entidades.xlsx"))

# READ DATA ====
# Define Drive folders
drive_auth(email = "sierra.wells@datacivica.org")
drive_in <- drive_ls(as_id("1ZhVCLDWigWSPEuebjoTJhMb2RqNc-uN8"))

drive_out <- as_id("1-n5ZlkcH1L5naEthn9dGKFOQ7bRunp59")

# Create list of files to read
file_ids <- setNames(drive_in$id, tolower(drive_in$name))

# Read raw files from Drive
for (file_name in names(file_ids)) {
  file_id <- file_ids[file_name]
  
  year_from_name <- str_extract(file_name, "\\d{4}")
  
  drive_download(as_id(file_id), path = paste0(tempdir(), "/", file_name), overwrite = TRUE)
  
  tempo <- read.dbf(paste0(tempdir(), "/", file_name)) %>% 
    mutate(year = year_from_name) %>% 
    clean_names()
  
  assign(str_replace(file_name, ".dbf", "_raw"), tempo)
  
  file.remove(paste0(tempdir(), "/", file_name))
  
  message(paste0(file_name, " read."))
}

# Catálogo de entidades

# JOIN TABLES OF DIFFERENT YEARS ====
joined_2019 <- encig2019_01_sec1_3_4_5_8_9_10_raw %>% 
  left_join(encig2019_01_sec_11_raw) %>% 
  left_join(encig2019_02_residentes_sec_2_raw) %>% 
  mutate(year = 2019)

joined_2021 <- encig2021_01_sec1_a_3_4_5_8_9_10_raw %>% 
  left_join(encig2021_01_sec_11_raw) %>% 
  left_join(encig2021_02_residentes_sec_2_raw) %>% 
  mutate(year = 2021)

joined_2023 <- encig2023_01_sec1_a_3_4_5_8_9_10_raw %>% 
  left_join(encig2023_01_sec_11_raw) %>% 
  left_join(encig2023_02_residentes_sec_2_raw) %>% 
  mutate(year = 2023)

# CHANGE VAR NAMES ====
vars_rename <- read_yaml(paths$vars_rename) %>% 
  unlist()

renamed_2019 <- joined_2019
renamed_2021 <- joined_2021
renamed_2023 <- joined_2023

for (name in names(vars_rename)){
  new_name <- str_extract(name, "^[^.]+")
  
  year_from_name <- str_extract(name, "\\.(\\d+)$") %>% 
    str_remove_all("\\.")
  
  old_name <- vars_rename[name]
  
  if (year_from_name == "2019") {
    renamed_2019 <- renamed_2019 %>% 
      rename(!!new_name := !!sym(old_name))
  }
  
  if (year_from_name == "2021") {
    renamed_2021 <- renamed_2021 %>% 
      rename(!!new_name := !!sym(old_name))
  }
  
  if (year_from_name == "2023") {
    renamed_2023 <- renamed_2023 %>% 
      rename(!!new_name := !!sym(old_name))
  }
  
  print(paste0("Renamed ", old_name, " to ", new_name, " for year ", year_from_name))
}

# BIND ROWS & SELECT COLS ====
vars_to_keep <- read_yaml(paths$vars_to_keep) %>% 
  unlist()

vars_to_keep_regex <- vars_to_keep %>% 
  str_subset("\\*$") %>% 
  str_remove_all("\\*$")

vars_to_keep_exact <- vars_to_keep %>%
  str_subset("^[^\\*]+$")

encig_bound <- mget(c("renamed_2019", "renamed_2021", "renamed_2023")) %>%
  # Convert all columns to characters
  map(~ mutate_all(., as.character)) %>%
  bind_rows() %>% 
  select(all_of(vars_to_keep_exact), starts_with(vars_to_keep_regex))

# CLEAN DATA ====
catalogo_entidades <- read_excel(paths$catalogo_entidades) %>% 
  clean_names()

encig_clean <- encig_bound %>% 
  mutate(
    sexo = case_when(
      sexo == "1" ~ "Hombre",
      sexo == "2" ~ "Mujer",
      T ~ NA_character_),
    edad = as.numeric(edad),
    edad = ifelse(edad > 97, NA_real_, edad),
    niv = as.numeric(niv),
    niv = case_when(
      niv %in% 0:2 ~ "Primaria o menos",
      niv == 3 ~ "Secundaria",
      niv %in% 4:7 ~ "Preparatoria o bachillerato",
      niv %in% 8:9 ~ "Licenciatura o más",
      T ~ NA_character_),
    across(
      .cols = starts_with("corr_"),
      .fns = ~ case_when(
        .x == "1" ~ "Muy frecuente",
        .x == "2" ~ "Frecuente",
        .x == "3" ~ "Poco frecuente",
        .x == "4" ~ "Nunca",
        T ~ NA_character_)),
    across(
      .cols = starts_with("conf_"),
      .fns = ~ case_when(
        .x == 1 ~ "Mucha confianza",
        .x == 2 ~ "Algo de confianza",
        .x == 3 ~ "Algo de desconfianza",
        .x == 4 ~ "Mucha desconfianza",
        T ~ NA_character_))) %>% 
  left_join(catalogo_entidades, by = c("cve_ent" = "clave_entidad"))
  
# EXPORT DATA ====
write_csv(encig_clean,
          paste0(tempdir(), "/encig_clean.csv"))

drive_upload(paste0(tempdir(), "/encig_clean.csv"),
             path = drive_out,
             name = "encig_clean.csv",
             overwrite = TRUE)

file.remove(paste0(tempdir(), "/encig_clean.csv"))

# done.
