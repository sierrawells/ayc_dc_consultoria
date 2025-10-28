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

id_ayc <- drive_ls(as_id("1hfvoyXHh3Kz4stxY1B8JPyBss-vSRmHN"),
                   pattern = "2024") %>% 
  pull(id)

id_sat <- drive_ls(as_id("1Ij4lr0GTfDX5mT0ewhLnnHkeVXe3EFiK")) %>% 
  pull(id)

id_indesol <- drive_ls(as_id("1OSsKmIjuCKkFWokAiI1f9808rZYM3Udu")) %>% 
  pull(id)

stopifnot(length(id_ayc) == 1,
          length(id_sat) == 1,
          length(id_indesol) == 1)

drive_out <- as_id("1f8-zhH4OxE-vipndMsIJgVJz4O7o76My")

# READ DATA =====

# Directorio AyC

drive_download(id_ayc,
               path = paste0(tempdir(), "/ayc.xlsx"),
               overwrite = TRUE)

ayc_raw <- read_excel(paste0(tempdir(), "/ayc.xlsx"))

stopifnot(nrow(ayc_raw) > 45e3)

file.remove(paste0(tempdir(), "/ayc.xlsx"))

# SAT status

drive_download(id_sat,
               path = paste0(tempdir(), "/sat.csv"),
               overwrite = TRUE)

sat_status <- read_csv(paste0(tempdir(), "/sat.csv"))

file.remove(paste0(tempdir(), "/sat.csv"))

# INDESOL status

drive_download(id_indesol,
               path = paste0(tempdir(), "/indesol.csv"),
               overwrite = TRUE)

indesol_status <- read_csv(paste0(tempdir(), "/indesol.csv"))

file.remove(paste0(tempdir(), "/indesol.csv"))
          

# CLEAN AyC DIRECTORIO =====

ayc_col_names <- c(
  ayc_raw[1, 1:36] %>% unlist() %>% unname(),
  ayc_raw[2, 37:54] %>% unlist() %>% unname(),
  ayc_raw[1, 55:ncol(ayc_raw)] %>% unlist() %>% unname()
)

names(ayc_raw) <- ayc_col_names

ayc_clean <- ayc_raw %>% 
  clean_names() %>% 
  slice(-c(1:2)) %>% 
  select(rfc, razon_social) %>% 
  distinct()

# JOIN DIRECTORIO WITH SAT & INDESOL STATUS ====

ayc_w_status <- ayc_clean %>% 
  full_join(sat_status) %>% 
  full_join(indesol_status) %>% 
  relocate(rfc, razon_social, anio_constitucion, estatus_representacion,
           estatus_da, anio_autorizacion_da, ultimo_anio_da,
           estatus_indesol, anio_inscripcion_indesol, ultimo_anio_informe_indesol)
  

# EXPORT DATA ====

write_csv(ayc_w_status, 
          paste0(tempdir(), "/da_indesol_historico.csv"))

drive_upload(paste0(tempdir(), "/da_indesol_historico.csv"),
             path = drive_out,
             overwrite = TRUE)

file.remove(paste0(tempdir(), "/da_indesol_historico.csv"))

# done.
