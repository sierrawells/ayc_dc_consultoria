#
# Author: SW
# Maintainer(s): SW, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive)

# Files ====
drive_auth(email = "sierra.wells@datacivica.org")

# Previously classified NTEE codes (AyC)
id_ayc_class <- drive_ls(as_id("1K6ebrM6DPQR9aUTwD6PiSw9fWuvCeYhn")) %>% 
  filter(name == "ntee_already_classified.csv") %>% 
  pull(id)

# Predicted NTEE codes (Gemini)
df_gemini_class <- drive_ls(as_id("16CVo8XsSrjyMQyoS2g5R-yPIQhV5nGjm")) %>% 
  filter(startsWith(name, "ntee_predicted_"))

stopifnot(nrow(df_gemini_class) == 13)

# Reclassified NTEE codes (Gemini)
df_gemini_reclass <- drive_ls(as_id("16CVo8XsSrjyMQyoS2g5R-yPIQhV5nGjm")) %>% 
  filter(startsWith(name, "ntee_reclassified_"))

stopifnot(nrow(df_gemini_reclass) == 3)

# Drive for output
drive_out <- as_id("16CVo8XsSrjyMQyoS2g5R-yPIQhV5nGjm")

# READ DATA ====
# Previously classified NTEE codes (AyC)
drive_download(id_ayc_class,
               path = paste0(tempdir(), "/ntee_ayc.csv"),
               overwrite = TRUE)

ntee_ayc <- read_csv(paste0(tempdir(), "/ntee_ayc.csv")) %>% 
  mutate(tipo_clasificacion_ntee = "AyC")

file.remove(paste0(tempdir(), "/ntee_ayc.csv"))

# Predicted NTEE codes (Gemini)
# TODOS: REDO PART 6: MISSION ROWS
chunk_size <- 282

ntee_gemini_ls <- list()
for (row_num in 1:nrow(df_gemini_class)){
  part_num <- str_extract(df_gemini_class$name[row_num], "\\d+")
  
  id <- df_gemini_class$id[row_num]
  
  drive_download(id,
                 path = paste0(tempdir(), "ntee_gemini_", part_num, ".csv"),
                 overwrite = TRUE)
  
  tempo <- read_csv(paste0(tempdir(), "ntee_gemini_", part_num, ".csv"))
  
  if (!is.na(part_num)) stopifnot(nrow(tempo) >= chunk_size)
  
  ntee_gemini_ls[[part_num]] <- tempo %>%
    mutate(tipo_clasificacion_ntee = "Gemini") %>% 
    filter(!is.na(mision),
           ntee != "Z99")
  
}

# Reclassified NTEE codes (Gemini)
ntee_gemini_reclass_ls <- list()

for (row_num in 1:nrow(df_gemini_reclass)){
  part_num <- str_extract(df_gemini_reclass$name[row_num], "\\d+")
  
  id <- df_gemini_reclass$id[row_num]
  
  drive_download(id,
                 path = paste0(tempdir(), "ntee_gemini_reclass_", part_num, ".csv"),
                 overwrite = TRUE)
  
  tempo <- read_csv(paste0(tempdir(), "ntee_gemini_reclass_", part_num, ".csv"))
  
  stopifnot(nrow(tempo) == 200)
  
  ntee_gemini_reclass_ls[[part_num]] <- tempo %>%
    mutate(tipo_clasificacion_ntee = "Gemini") %>% 
    filter(!is.na(mision),
           ntee != "Z99")
}

# JOIN DATA ====
ntee_joined <- ntee_ayc %>% 
  bind_rows(bind_rows(ntee_gemini_ls)) %>% 
  bind_rows(bind_rows(ntee_gemini_reclass_ls)) %>%
  select(rfc:tipo_clasificacion_ntee) %>% 
  group_by(rfc) %>% 
  reframe(
    multiple_codes = length(unique(ntee)) > 1,
    ntee = ifelse(multiple_codes, 
                  ntee[tipo_clasificacion_ntee == "Gemini"],
                  unique(ntee)),
    tipo_clasificacion_ntee = ifelse(multiple_codes, 
                                     "Gemini",
                                     tipo_clasificacion_ntee)) %>% 
  select(-multiple_codes)

stopifnot(length(unique(ntee_joined$rfc)) == nrow(ntee_joined))

# EXPORT DATA ====
write_csv(ntee_joined,
          paste0(tempdir(), "/ntee_classified_joined.csv"))

drive_upload(paste0(tempdir(), "/ntee_classified_joined.csv"),
             path = drive_out,
             overwrite = TRUE)

file.remove(paste0(tempdir(), "/ntee_classified_joined.csv"))

# done.