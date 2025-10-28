#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, janitor, googledrive, readxl)

# Files
paths <- list(output = here("donor_analysis/import_clean/output/"))

drive_auth(email = "sierra.wells@datacivica.org")

drive_out <- as_id("1K6ebrM6DPQR9aUTwD6PiSw9fWuvCeYhn")

id_ayc_directorio <- drive_ls(as_id("1hfvoyXHh3Kz4stxY1B8JPyBss-vSRmHN")) %>% 
  filter(str_detect(name, "2024")) %>% 
  pull(id)

id_mision <- drive_ls(as_id("1C1nq47Q8fnMRaVa1wMeZb1ZIpIRl-Pt0")) %>% 
  filter(str_detect(name, "2024")) %>% 
  pull(id)

# READ DATA ====
# DIRECTORIO AYC
drive_download(as_id(id_ayc_directorio), 
               path = paste0(tempdir(), "/ayc_directorio_2024.xlsx"),
               overwrite = TRUE)

ayc_directorio <- read_excel(paste0(tempdir(), "/ayc_directorio_2024.xlsx")) %>% 
  clean_names() %>% 
  mutate(across(.cols = everything(),
                .fns = ~ ifelse(.x == "", NA, .x))) %>% 
  rename(rfc = x2,
         razon_social = osc_institucional,
         ntee = x38,
         ntee_2 = x39) %>% 
  select(rfc, ntee, ntee_2)

file.remove(paste0(tempdir(), "/ayc_directorio_2024.xlsx"))

# Misiones
drive_download(as_id(id_mision), 
               path = paste0(tempdir(), "/misiones_2024.xlsx"),
               overwrite = TRUE)

misiones <- read_excel(paste0(tempdir(), "/misiones_2024.xlsx"),
                       sheet = "Generales") %>% 
  clean_names() %>% 
  mutate(across(.cols = everything(),
                .fns = ~ ifelse(.x == "", NA, .x))) %>% 
  select(rfc, razon_social, mision, valores)

file.remove(paste0(tempdir(), "/misiones_2024.xlsx"))

# JOIN DATA ====
ayc_directorio_c_misiones <- misiones %>% 
  left_join(ayc_directorio, by = "rfc") %>%
  mutate(
    mision = ifelse(mision %in% c("XXX", ".", "NO TIENE", "NINGUNO", "NINGUNA",
                                       "NA", "EN TRAMITE", "N/A", "NO EXISTE",
                                       "SIN DESCRIPCION", "-", "NO TENEMOS",
                                       "PENDIENTE", "EN DESARROLLO", "SIN DATOS",
                                       "NO DEFINIDOS", "NO APLICA", "NO HAY", "MISION",
                                       "VALORES", "X", "CULTURAL", "SIN MISION",
                                       "SIN VALORES", "NO", "S/N", "SI", "NADA",
                                       "NO TENEMOS TODAVIA", "EN CONSTRUCCION", "XX",
                                       "YA ESTÁ CERRADA", "NO HUBO", "SIN INFORMACIÓN",
                                       "A", "EN PROCESO", "POR DEFINIR", "SIN MISION TODAVIA",
                                       "SIN VALORES TODAVIA", "YA ESTA CERRADA",
                                       "SIN INFORMACION", "EN CONSTRUCCIÓN") |
                           # If there are no non-alphabetical characters
                           !str_detect(mision, "[:alpha:]"),
                         NA, mision))

# DEFINE MISSIONS TO CLASSIFY VS. ALREADY CLASSIFIED ====
ntee_already_classified <- ayc_directorio_c_misiones %>% 
  filter(!is.na(ntee),
         ntee != "Z99",
         nchar(ntee) == 3)

ntee_training_data <- ntee_already_classified %>% 
  slice_sample(n = 20)

ntee_to_classify <- ayc_directorio_c_misiones %>% 
  filter(is.na(ntee)) %>% 
  mutate(row_number = row_number()) %>% 
  relocate(row_number, .before = rfc)

# To reclassify: OSC classified as "Z99" or one letter by AyC
ntee_to_reclassify <- ayc_directorio_c_misiones %>% 
  filter(startsWith(ntee, "Z") | nchar(ntee) < 3,
         !is.na(mision)) %>% 
  mutate(row_number = row_number() + nrow(ntee_to_classify),
         ntee_2 = NA_character_) %>% 
  relocate(row_number, .before = rfc)

# EXPORT DATA ====
# Already classified
write_csv(ntee_already_classified, 
          paste0(paths$output, "ntee_already_classified.csv"))

drive_upload(paste0(paths$output, "ntee_already_classified.csv"),
             path = drive_out)

file.remove(paste0(paths$output, "ntee_already_classified.csv"))

# Training data
write_csv(ntee_training_data, 
          paste0(paths$output, "ntee_training_data.csv"))

drive_upload(paste0(paths$output, "ntee_training_data.csv"),
             path = drive_out,
             overwrite = TRUE)

file.remove(paste0(paths$output, "ntee_training_data.csv"))

# To be classified
# Divide ntee_to_classify into n parts and save each part
n <- 12

chunk_size <- trunc(nrow(ntee_to_classify) / n)

for (i in 1:n){
  start_row <- ((i - 1) * chunk_size) + 1
  
  end_row <- ifelse(i == n, nrow(ntee_to_classify), i * chunk_size)

  ntee_to_classify_part <- ntee_to_classify %>% 
    slice(start_row:end_row)
  
  stopifnot(nrow(ntee_to_classify_part) >= chunk_size)
  
  write_csv(ntee_to_classify_part, 
            paste0(paths$output, "ntee_to_classify_part_", i, ".csv"))
  
  drive_upload(paste0(paths$output, "ntee_to_classify_part_", i, ".csv"),
               path = drive_out,
               overwrite = TRUE)
  
  file.remove(paste0(paths$output, "ntee_to_classify_part_", i, ".csv"))
}

# To reclassify: OSC classified as "Z99" or one letter by AyC
for (i in 1:3){
  start_row <- ((i - 1) * 200) + 1
  
  end_row <- ifelse(i == 3, nrow(ntee_to_reclassify), i * 200)
  
  ntee_to_reclassify_part <- ntee_to_reclassify %>% 
    slice(start_row:end_row)
  
  stopifnot(nrow(ntee_to_reclassify_part) >= 200)
  
  write_csv(ntee_to_reclassify_part, 
            paste0(paths$output, "ntee_to_reclassify_part_", i, ".csv"))
  
  drive_upload(paste0(paths$output, "ntee_to_reclassify_part_", i, ".csv"),
               path = drive_out,
               overwrite = TRUE)
  
  file.remove(paste0(paths$output, "ntee_to_reclassify_part_", i, ".csv"))
}

# done.
