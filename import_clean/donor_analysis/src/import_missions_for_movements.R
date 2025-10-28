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

id_orgs_w_ntee <- drive_ls(as_id("16CVo8XsSrjyMQyoS2g5R-yPIQhV5nGjm")) %>% 
  filter(name == "ntee_classified_joined.csv") %>% 
  pull(id)

id_mision <- drive_ls(as_id("1C1nq47Q8fnMRaVa1wMeZb1ZIpIRl-Pt0")) %>% 
  filter(str_detect(name, "2024")) %>% 
  pull(id)

id_ntee_to_movements <- drive_ls(as_id("1p25pJ15lMJUhtlGQCbb8ADo8vuUueM5J")) %>% 
  filter(name == "ntee_to_movements_class") %>% 
  pull(id)

id_training_data <- drive_ls(drive_out) %>% 
  filter(name == "movements_training_data") %>%
  pull(id)

stopifnot(length(id_orgs_w_ntee) == 1,
          length(id_mision) == 1,
          length(id_ntee_to_movements) == 1,
          length(id_training_data) == 1)

# READ DATA ====
# Orgs w/ NTEE classification
drive_download(as_id(id_orgs_w_ntee), 
               path = paste0(tempdir(), "/orgs_w_ntee_2024.csv"),
               overwrite = TRUE)

orgs_w_ntee <- read_csv(paste0(tempdir(), "/orgs_w_ntee_2024.csv"))

file.remove(paste0(tempdir(), "/orgs_w_ntee_2024.csv"))

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

# NTEE to movements
drive_download(as_id(id_ntee_to_movements), 
               path = paste0(tempdir(), "/ntee_to_movements_2024.csv"),
               overwrite = TRUE)

ntee_to_movements <- read_csv(paste0(tempdir(), "/ntee_to_movements_2024.csv")) %>% 
  select(ntee_code:second_classification)

file.remove(paste0(tempdir(), "/ntee_to_movements_2024.csv"))

# Training data
drive_download(as_id(id_training_data), 
               path = paste0(tempdir(), "/movements_training_data.csv"),
               overwrite = TRUE)

training_data_rfc <- read_csv(paste0(tempdir(), "/movements_training_data.csv")) %>% 
  pull(rfc)

file.remove(paste0(tempdir(), "/movements_training_data.csv"))

# JOIN DATA ====
orgs_w_second_classification <- orgs_w_ntee %>% 
  left_join(ntee_to_movements, by = c("ntee" = "ntee_code")) %>% 
  left_join(misiones, by = "rfc") %>% 
  relocate(c(razon_social, mision, valores), .after = rfc) %>%
  select(rfc:valores, ntee, second_classification) %>% 
  distinct()

non_movements <- orgs_w_second_classification %>% 
  filter(second_classification != "Clasificar movimiento")

movements_to_classify_pre <- orgs_w_second_classification %>% 
  filter(second_classification == "Clasificar movimiento",
         !is.na(mision)) %>% 
  select(-second_classification)

# movements_training_data <- movements_to_classify %>% 
#   slice_sample(n = 20)

movements_to_classify <- movements_to_classify_pre %>% 
  filter(!rfc %in% training_data_rfc)

stopifnot(nrow(movements_to_classify) + length(training_data_rfc) == nrow(movements_to_classify_pre))

# EXPORT DATA ====
# Non movements
write_csv(non_movements, 
          paste0(paths$output, "non_movements.csv"))

drive_upload(paste0(paths$output, "non_movements.csv"),
             path = drive_out,
             overwrite = TRUE)

file.remove(paste0(paths$output, "non_movements.csv"))

# Training data
# write_csv(movements_training_data, 
#           paste0(paths$output, "movements_training_data.csv"))
# 
# drive_upload(paste0(paths$output, "movements_training_data.csv"),
#              path = drive_out,
#              overwrite = TRUE)
# 
# file.remove(paste0(paths$output, "movements_training_data.csv"))

# To be classified
# Divide movements_to_classify into n parts and save each part
n <- 10

chunk_size <- trunc(nrow(movements_to_classify) / n)

for (i in 1:n){
  start_row <- ((i - 1) * chunk_size) + 1
  
  end_row <- ifelse(i == n, nrow(movements_to_classify), i * chunk_size)
  
  movements_to_classify_part <- movements_to_classify %>% 
    slice(start_row:end_row)
  
  stopifnot(nrow(movements_to_classify_part) >= chunk_size)
  
  write_csv(movements_to_classify_part, 
            paste0(paths$output, "movements_to_classify_part_", i, ".csv"))
  
  drive_upload(paste0(paths$output, "movements_to_classify_part_", i, ".csv"),
               path = drive_out,
               overwrite = TRUE)
  
  file.remove(paste0(paths$output, "movements_to_classify_part_", i, ".csv"))
}

# done.
