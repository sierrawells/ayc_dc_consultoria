#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, janitor, readxl)

# Files
drive_auth(email = "sierra.wells@datacivica.org")

ls_da_2025 <- drive_ls(as_id("1aXdONpAJLwHnyg9k-ANTsPIwnSq5tZ4l")) %>% 
  filter(str_detect(name, "Directorio DAs 2025")) 

ls_da_2008_2024 <- drive_ls(as_id("1Ri7Arj_44WVedUDmzqnIby09cvTbnrod")) %>% 
  filter(str_detect(tolower(name), "limpio"),
         !name %in% c("SAT -Directorio DA 2020 -20200113 20200514 20200727 DOF  Limpio.xlsx",
                      "SAT -Directorio DAs 2019 - DOF 20190503 - Limpio.xlsx"))

ls_da <- rbind(ls_da_2025, ls_da_2008_2024)

stopifnot(nrow(ls_da) == length(2008:2025))

drive_out <- as_id("1Ij4lr0GTfDX5mT0ewhLnnHkeVXe3EFiK") 

# LOOP THROUGH ACTIVE DA BY YEAR  =====

patterns <- c("das ", "da ")

dfs_da <- list() 

for (anio in 2008:2025) {
  
  file_anio <- ls_da %>% 
    filter(str_detect(tolower(name),
                      str_flatten(paste0(patterns, anio), collapse = "|"))) %>% 
    pull(name)
  
  stopifnot(length(file_anio) == 1)
  
  drive_download(file_anio,
                 path = paste0(tempdir(), "/", file_anio),
                 overwrite = TRUE)
  
  sheet_num <- ifelse(anio == 2025, 1, 2)
  
  excel_raw <- read_excel(paste0(tempdir(), "/", file_anio),
                         sheet = sheet_num) %>% 
   clean_names()
  
  file.remove(paste0(tempdir(), "/", file_anio))
  
  if (anio == 2025){
    excel_raw <- excel_raw[21:nrow(excel_raw),] %>% 
      row_to_names(row_number = 1) %>% 
      clean_names()
  }
  
  rfcs <- excel_raw %>% 
   pull(rfc)
 
 # Fix dates when read as numeric
 if (all(class(excel_raw$fecha_de_oficio) %in% c("numeric", "character"))){
   excel_raw$fecha_de_oficio <- as.Date(as.numeric(excel_raw$fecha_de_oficio),
                                        origin = "1899-12-30")
 }
 
 anio_autorizacion_da <- excel_raw %>% 
   mutate(anio_autorizacion_da = format(fecha_de_oficio, "%Y")) %>% 
   pull(anio_autorizacion_da)
 
 stopifnot(all(as.numeric(anio_autorizacion_da) %in% 1990:(as.numeric(anio) + 1) |
                 is.na(anio_autorizacion_da)),
           all(!is.na(rfcs)),
           mean(!is.na(anio_autorizacion_da)) > 0.95)
  
  tempo <- data.frame(
    rfc = rfcs,
    anio_autorizacion_da = anio_autorizacion_da,
    anio_directorio = anio)
  
  dfs_da[[paste0("da_", anio)]] <- tempo
  
}

# BIND DIRECTORIOS FROM DIFFERENT YEARS ====
sat_bound <- bind_rows(dfs_da) %>% 
  group_by(rfc) %>% 
  summarize(
    anio_autorizacion_da = min(anio_autorizacion_da, na.rm = TRUE),
    anios_directorio = paste(unique(anio_directorio), collapse = ", "),
    ultimo_anio_da = max(anio_directorio, na.rm = TRUE),
    estatus_da = ifelse(ultimo_anio_da == 2025, "Activa", "Inactiva"))

sat_w_year_cols <- sat_bound

for (anio in 2008:2025){
  sat_w_year_cols[[paste0("directorio_", anio, "_da")]] <- ifelse(
    str_detect(sat_w_year_cols$anios_directorio, as.character(anio)),
    1, 0)
}

sat_clean <- sat_w_year_cols %>% 
  select(-anios_directorio)

# EXPORT DATA ====
write_csv(sat_clean, 
          paste0(tempdir(), "/sat_clean.csv"))

drive_upload(paste0(tempdir(), "/sat_clean.csv"),
             path = drive_out,
             overwrite = TRUE)

file.remove(paste0(tempdir(), "/sat_clean.csv"))

# done.