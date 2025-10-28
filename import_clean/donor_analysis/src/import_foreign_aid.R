#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, janitor, data.table)

# Files
drive_auth(email = "sierra.wells@datacivica.org")

drive_in <- drive_ls(as_id("1AjMoUSkLXbkOmSIdkBGj7WZZTtB6JaMn")) %>% 
  filter(name == "us_foreign_aid_complete.csv")

stopifnot(nrow(drive_in) == 1)

drive_out <- as_id("1QRR0Ltw0Ih-yM9IdvbU0PFkTNrVqEsHz")

# READ DATA ====
drive_download(drive_in$id,
               path = paste0(tempdir(), "/foreign_aid_raw.csv"),
               overwrite = TRUE)

foreign_aid_raw <- fread(paste0(tempdir(), "/foreign_aid_raw.csv"))

file.remove(paste0(tempdir(), "/foreign_aid_raw.csv"))

# CLEAN DATA ====
foreign_aid_clean <- foreign_aid_raw %>% 
  clean_names() %>% 
  select(country_name,
         region_name,
         income_group_name,
         funding_agency_name,
         managing_agency_name,
         managing_sub_agency_or_bureau_name,
         implementing_partner_category_name,
         implementing_partner_sub_category_name,
         implementing_partner_name,
         international_category_name,
         international_sector_name,
         international_purpose_name,
         foreign_assistance_objective_name,
         aid_type_group_name,
         aid_type_name,
         activity_id,
         submission_id,
         submission_activity_id,
         activity_description,
         activity_start_date,
         activity_end_date,
         fiscal_year,
         transaction_date,
         current_dollar_amount,
         constant_dollar_amount,
         activity_budget_amount) %>% 
  distinct() %>% 
  mutate(
    across(
      .cols = current_dollar_amount:activity_budget_amount,
      .fns = ~ as.integer(.x)),
    across(
      .cols = everything(),
      .fns = ~ ifelse(is.character(.x) & .x %in% c("NULL", ""), NA, .x)))

# EXPORT DATA ====
write_csv(foreign_aid_clean, 
          file = paste0(tempdir(), "/foreign_aid_clean.csv"))

drive_upload(media = paste0(tempdir(), "/foreign_aid_clean.csv"),
               path = drive_out,
               name = "foreign_aid.csv",
               overwrite = TRUE)

file.remove(paste0(tempdir(), "/foreign_aid_clean.csv"))

# done.
