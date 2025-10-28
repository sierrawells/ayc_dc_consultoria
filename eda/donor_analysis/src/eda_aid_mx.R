#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, data.table, ggthemes, scales,
               ggrepel, janitor)

# Files
paths <- list(output = here("donor_analysis/eda/output/"))

drive_auth(email = "sierra.wells@datacivica.org")

drive_in <- drive_ls(as_id("1QRR0Ltw0Ih-yM9IdvbU0PFkTNrVqEsHz"))

drive_id_aid <- drive_in %>% 
  filter(name == "foreign_aid.csv") %>% 
  pull(id)

drive_id_pop <- drive_in %>% 
  filter(name == "population_by_country.csv") %>% 
  pull(id)

stopifnot(length(drive_id_aid) == 1,
          length(drive_id_pop) == 1)

# READ DATA ====

# Foreign aid data base
drive_download(drive_id_aid,
               path = paste0(tempdir(), "/foreign_aid.csv"),
               overwrite = TRUE)

foreign_aid <- fread(paste0(tempdir(), "/foreign_aid.csv"))

file.remove(paste0(tempdir(), "/foreign_aid.csv"))

# constant_dollar_amount = 2023 USD

# AID PURPOSE OVER TIME ====

# All U.S. aid
purpose_mx <- foreign_aid %>% 
  filter(country_name == "Mexico",
         constant_dollar_amount > 0,
         fiscal_year >= 1960) %>% 
  group_by(fiscal_year, international_purpose_name) %>% 
  summarize(constant_dollar_amount = sum(constant_dollar_amount, na.rm = T)) %>% 
  group_by(fiscal_year) %>% 
  mutate(perc = constant_dollar_amount / sum(constant_dollar_amount)) %>% 
  filter(!international_purpose_name %in% c("Operating Expenses", 
                                            "General budget support-related aid",
                                            "Administration and Oversight")) %>%
  group_by(international_purpose_name) %>%
  mutate(avg_perc = mean(perc)) %>% 
  ungroup() %>% 
  mutate(rank = dense_rank(desc(avg_perc))) %>% 
  filter(rank < 10)

# done.
