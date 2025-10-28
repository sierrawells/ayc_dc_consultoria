#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, janitor, curl, data.table, googledrive)

# Files
drive_auth(email = "sierra.wells@datacivica.org")

drive_out <- as_id("138SSYICwXX860y3jjXAJgc9b39N9VEHa")

# Define URL for BMF download
url_bmf <- "https://nccsdata.s3.amazonaws.com/harmonized/bmf/unified/BMF_UNIFIED_V1.1.csv"
current_date <- format(Sys.time(), "%Y-%m-")  
filename <- paste0(current_date, "bmf.csv")   
# url <- paste0(url_bmf, filename)       

# Download BMF
curl_download(url = url_bmf, destfile = paste0(tempdir(), filename),
              mode= "wb")

bmf_raw <- fread(paste0(tempdir(), filename))

bmf_clean <- bmf_raw %>% 
  clean_names %>% 
  mutate(
    across(.cols = everything(),
           .fns = ~ ifelse(.x == "", NA, .x))) %>% 
  select(ein2, ntee_irs:nccs_level_3, bmf_income_code, bmf_asset_code,
         bmf_foundation_code, org_year_first, org_year_last,
         org_name_sec, org_name_current)

mean(!is.na(bmf_clean$ntee_irs)) # 98.5%
mean(!is.na(bmf_clean$nccs_level_1)) # 100%
mean(!is.na(bmf_clean$nccs_level_3)) # 100%
mean(!is.na(bmf_clean$org_name_current)) # 100%

# Upload to Google Drive
write_csv(bmf_clean, 
          file = paste0(tempdir(), "/bmf.csv"))

drive_upload(media = paste0(tempdir(), "/bmf.csv"),
             path = drive_out,
             name = "bmf.csv",
             type = "csv",
             overwrite = TRUE)

file.remove(paste0(tempdir(), "/bmf.csv")) 

# done.
