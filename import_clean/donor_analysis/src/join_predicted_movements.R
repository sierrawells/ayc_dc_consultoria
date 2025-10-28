#
# Author: SW
# Maintainer(s): SW, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, janitor)

# Files ====
drive_auth(email = "sierra.wells@datacivica.org")

# Non-movements
id_nonmovements <- drive_ls(as_id("1K6ebrM6DPQR9aUTwD6PiSw9fWuvCeYhn")) %>% 
  filter(name == "non_movements") %>% 
  pull(id)

# Predicted movements (Gemini)
df_gemini_class <- drive_ls(as_id("16CVo8XsSrjyMQyoS2g5R-yPIQhV5nGjm")) %>% 
  filter(startsWith(name, "movements_predicted_"))

stopifnot(nrow(df_gemini_class) == 10)

# Drive for output
drive_out <- as_id("16CVo8XsSrjyMQyoS2g5R-yPIQhV5nGjm")

# READ DATA ====
# Non-movements
drive_download(id_nonmovements,
               path = paste0(tempdir(), "/nonmovements.csv"),
               overwrite = TRUE)

nonmovements <- read_csv(paste0(tempdir(), "/nonmovements.csv")) %>% 
  rename(main_movement = second_classification)

file.remove(paste0(tempdir(), "/nonmovements.csv"))

# Predicted movements (Gemini)
chunk_size <- 510

movements_gemini_ls <- list()
for (row_num in 1:nrow(df_gemini_class)){
  part_num <- str_extract(df_gemini_class$name[row_num], "\\d+")
  
  id <- df_gemini_class$id[row_num]
  
  drive_download(id,
                 path = paste0(tempdir(), "movements_gemini_", part_num, ".csv"),
                 overwrite = TRUE)
  
  tempo <- read_csv(paste0(tempdir(), "movements_gemini_", part_num, ".csv"))
  
  file.remove(paste0(tempdir(), "movements_gemini_", part_num, ".csv"))
  
  stopifnot(nrow(tempo) >= chunk_size,
            "rfc" %in% names(tempo))
  
  movements_gemini_ls[[part_num]] <- tempo %>%
    mutate(across(.cols = contains("movement"),
                  .fns = ~ifelse(.x %in% c("Otro tipo de ONG", "Información insuficiente"),
                                NA_character_,
                                .x)),
           part = part_num)
  
}

# JOIN DATA ====
movements_joined <- nonmovements %>% 
  bind_rows(bind_rows(movements_gemini_ls)) %>% 
  arrange(rfc)

stopifnot(all(!is.na(movements_joined$rfc)),
          length(unique(movements_joined$rfc)) == nrow(movements_joined))

# EXPORT DATA ====
write_csv(movements_joined,
          paste0(tempdir(), "/movements_classified_joined.csv"))

drive_upload(paste0(tempdir(), "/movements_classified_joined.csv"),
             path = drive_out,
             overwrite = TRUE)

file.remove(paste0(tempdir(), "/movements_classified_joined.csv"))

# done.