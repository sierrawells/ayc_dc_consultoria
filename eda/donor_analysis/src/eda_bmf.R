#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# Out of random sample of 500 "national" orgs, 10 were hand-identified as foreign (2%)

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, janitor, curl, data.table, googledrive, readxl,
               upstartr, fuzzyjoin)

# Files
drive_auth(email = "sierra.wells@datacivica.org")

drive_bmf <- drive_ls(as_id("138SSYICwXX860y3jjXAJgc9b39N9VEHa")) %>% 
  filter(name == "bmf.csv")

bmf_id <- drive_bmf$id

stopifnot(length(bmf_id) == 1)

drive_ayc <- drive_ls(as_id("1C1nq47Q8fnMRaVa1wMeZb1ZIpIRl-Pt0"))

# FUNCTIONS TO STRIP SUBSTRINGS ====
strings_to_remove <- c("the", "inc\\.?$", "corp\\.?$", "corporation",
                       "asociación", "ac\\.?$", "iap\\.?$", "agrupación",
                       "fundación")

strip_strings <- function(string) {
 string_clean <- string %>%
   tolower() %>% 
   # remove all "inc" when it is the end of the string
   str_remove_all(., str_flatten(strings_to_remove, collapse = "|")) %>%
   str_squish() %>% 
   unaccent()
 
 return(string_clean)
}

substring_match <- function(vector1, vector2){

  # Create a data frame with all combinations of the two vectors
  combinations <- expand.grid(vector1 = strip_strings(vector1),
                              vector2 = strip_strings(vector2),
                              stringsAsFactors = FALSE)

  # See if any strings of vector1 are substrings of values in vector2 (& vice versa)
  matches <- combinations %>%
    mutate(match = str_detect(vector2, vector1) | str_detect(vector1, vector2)) %>%
    filter(match) %>%
    select(-match)

  return(matches)
}

# READ DATA ====
# BMF
drive_download(as_id(bmf_id), 
               path = paste0(tempdir(), "/bmf.csv"),
               overwrite = TRUE)

bmf <- fread(paste0(tempdir(), "/bmf.csv"))

file.remove(paste0(tempdir(), "/bmf.csv"))

bmf_clean <- bmf

bmf_clean$org_name_current <- bmf$org_name_current %>% 
  strip_strings()

bmf_clean$org_name_sec <- bmf$org_name_sec %>% 
  strip_strings()

# AyC
ayc_ls <- list()

for (i in seq_along(drive_ayc$id)) {
  name <- drive_ayc$name[i]
  
  year <- str_extract(name, "\\d{4}") %>% 
    as.numeric()
  
  drive_download(as_id(drive_ayc$id[i]), 
                 path = paste0(tempdir(), "/", name),
                 overwrite = TRUE)
  
  tempo <- read_excel(paste0(tempdir(), "/", name)) %>% 
    clean_names() %>% 
    mutate(across(.cols = everything(),
                  .fns = ~ ifelse(.x == "", NA, .x)),
           year = year)
  
  ayc_ls[[i]] <- tempo
    
}

ayc_names <- bind_rows(ayc_ls) %>% 
  select(rfc, razon_social) %>% 
  distinct() %>% 
  filter(!is.na(razon_social)) %>% 
  slice_sample(n = 500)

write_csv(ayc_names, 
          "~/Downloads/ayc_names.csv")

# FIND MATCHES ====
n <- 100

chunk_size_bmf <- ceiling(nrow(bmf_clean) / n)
chunk_size_ayc <- ceiling(nrow(ayc_names) / n)

matches_done <- c()
matches <- list()

for (bmf_i in 1:n){
  for (ayc_i in 1:n){
    match_hash <- paste0("bmf_", bmf_i, "_ayc_", ayc_i)
    
    if (match_hash %in% matches_done) {
      next
    }
    
    bmf_start <- 1 + chunk_size_bmf * (bmf_i - 1)
    bmf_end <- chunk_size_bmf * bmf_i
    
    if (bmf_end > nrow(bmf_clean)) {
      bmf_end <- nrow(bmf_clean)
    }
    
    ayc_start <- 1 + chunk_size_ayc * (ayc_i - 1)
    ayc_end <- chunk_size_ayc * ayc_i
    
    if (ayc_end > nrow(ayc_names)) {
      ayc_end <- nrow(ayc_names)
    }
  
    bmf_chunk_current <- bmf_clean %>% 
      slice(bmf_start:bmf_end) %>%
      filter(!is.na(org_name_current)) %>% 
      pull(org_name_current)
    
    bmf_chunk_sec <- bmf_clean %>% 
      slice(bmf_start:bmf_end) %>%
      filter(!is.na(org_name_sec)) %>% 
      pull(org_name_sec)
    
    ayc_chunk <- ayc_names %>%
      slice(ayc_start:ayc_end) %>%
      filter(!is.na(razon_social)) %>% 
      pull(razon_social)
    
    # Find matches for current name
    matches_current <- substring_match(bmf_chunk_current, ayc_chunk)
    
    # Find matches for secondary name
    matches_sec <- substring_match(bmf_chunk_current, ayc_chunk)
    
    matches[[paste0("current_bmf_", bmf_i, "_ayc_", ayc_i)]] <- matches_current
    matches[[paste0("sec_bmf", bmf_i, "_ayc_", ayc_i)]] <- matches_sec
    
    # Mark match as done
    matches_done <- unique(matches_done, 
                           match_hash)
    
    message(matches_done, " done.")
    
    }
}
