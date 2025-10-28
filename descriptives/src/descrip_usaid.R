#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, data.table, scales, janitor, ggtext)

# Files
paths <- list(output = here("descriptives/output/"),
              tema = here("descriptives/tema.R"))

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

# Population data
drive_download(drive_id_pop,
               path = paste0(tempdir(), "/population.csv"),
               overwrite = TRUE)

population <- fread(paste0(tempdir(), "/population.csv")) %>% 
  select(1, 3, 68) %>% 
  slice(-1) %>% 
  setNames(c("country_name", "indicator_type", "population_2023")) %>% 
  filter(indicator_type == "Population, total") %>% 
  select(-indicator_type) %>% 
  mutate(country_name = case_when(
    str_detect(country_name, "Venezuela") ~ "Venezuela",
    str_detect(country_name, "Bahamas") ~ "Bahamas",
    str_detect(country_name, "St. Vincent and") ~ "	St. Vincent and Grenadines",
    T ~ country_name))

file.remove(paste0(tempdir(), "/population.csv"))

# GRAPH PREP ====
aid_caption <- "Fuente: Elaboración por Data Cívica a partir de la base de datos U.S. Foreign Assistance (Department of State)"

source(paths$tema)

# USAID AMOUNT BY COUNTRY OVER TIME ====
usaid_amount_by_country <- foreign_aid %>% 
  filter(managing_agency_name == "U.S. Agency for International Development" &
           !str_detect(tolower(country_name), "region|world"),
         fiscal_year %in% 2000:2024) %>% 
  group_by(fiscal_year, country_name) %>%
  summarize(constant_dollar_amount = sum(constant_dollar_amount, na.rm = TRUE)) %>%
  group_by(country_name) %>% 
  mutate(
    max_amount = max(constant_dollar_amount, na.rm = TRUE),
    country_name = case_when(
      country_name == "Haiti" ~ "Haití",
      country_name == "Ethiopia" ~ "Etiopía",
      country_name == "Jordan" ~ "Jordania",
      country_name == "Congo (Kinshasa)" ~ "Congo",
      country_name == "Russia" ~ "Rusia",
      country_name == "Egypt" ~ "Egipto",
      country_name == "Pakistan" ~ "Pakistán",
      country_name == "Afghanistan" ~ "Afganistán",
      country_name == "Ukraine" ~ "Ucrania",
      country_name == "Iraq" ~ "Irak",
      country_name == "Mexico" ~ "México",
      T ~ country_name)) %>% 
  group_by(fiscal_year) %>%  
  mutate(
    rank_max = rank(-max_amount, ties.method = "first"),
    to_label = ifelse((rank_max < 5 | country_name == "México") & (country_name != "Egipto"),
                      TRUE, FALSE)) %>% 
  ungroup()

usaid_amount_by_country_graf <- ggplot(data = usaid_amount_by_country,
                                       aes(x = fiscal_year, y = constant_dollar_amount,
                                           color = country_name, group = country_name)) +
  geom_line(data = usaid_amount_by_country %>% filter(!to_label), color = ejes) +
  geom_line(data = usaid_amount_by_country %>% filter(to_label), linewidth = 1) +
  geom_label(
    data = usaid_amount_by_country %>% 
      filter(to_label & constant_dollar_amount == max_amount),
    aes(label = country_name),
    fill = fondo, fontface = "bold", vjust = -0.5, family = labels_font, size = 4) +
  labs(
    title = "Fondos de USAID por país de destino",
    subtitle = "2000-2024",
    x = "Año fiscal",
    y = "Millones de dólares constantes (2023)",
    caption = aid_caption) +
  tema +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M"),
                     limits = c(0, max(usaid_amount_by_country$constant_dollar_amount) * 1.05)) + 
  scale_color_manual(values = c(pal[2], pal[4], pal[5], pal[1], pal[6:7])) +
  scale_fill_manual(values = c(pal[2], pal[4], pal[5], pal[1], pal[6:7])) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))

for (device in devices){
  ggsave(usaid_amount_by_country_graf,
         filename = paste0(paths$output, "usaid_amount_by_country.", device),
         width = 12, height = 8)
  
  walk(paste0(paths$output, "usaid_amount_by_country.", device),
       ~add_both_logos(.x, escala = 7))
}

# PER CÁPITA AMOUNT BY COUNTRY (ONLY 2024) ====
usaid_per_capita_latam_2024 <- foreign_aid %>% 
  filter(managing_agency_name == "U.S. Agency for International Development" &
           !str_detect(tolower(country_name), "region|world") &
           region_name == "Western Hemisphere" &
           fiscal_year == 2024) %>%
  group_by(country_name) %>%
  summarize(constant_dollar_amount = sum(constant_dollar_amount, na.rm = TRUE)) %>% 
  left_join(population, by = "country_name") %>% 
  mutate(per_capita = constant_dollar_amount / population_2023,
         country_name = case_when(
           country_name == "Mexico" ~ "México",
           country_name == "Haiti" ~ "Haití",
           country_name == "Perú" ~ "Perú",
           country_name == "Brasil" ~ "Brasil",
           country_name == "Dominican Republic" ~ "República Dominicana",
           country_name == "St. Kitts and Nevis" ~ "San Cristóbal y Nieves",
           country_name == "Antigua and Barbuda" ~ "Antigua y Barbuda",
           country_name == "St. Lucia" ~ "Santa Lucía",
           country_name == "Trinidad and Tobago" ~ "Trinidad y Tobago",
           T ~ country_name)) %>% 
  filter(per_capita > 0)

usaid_per_capita_latam_2024_graf <- ggplot(data = usaid_per_capita_latam_2024,
                                           aes(x = reorder(country_name, per_capita),
                                               y = per_capita)) +
  geom_col(aes(fill = country_name == "México")) +
  geom_label(aes(label = scales::dollar(per_capita)),
             hjust = 0, size = 3.5, fontface = "bold", family = labels_font,
             fill = fondo, label.size = NA) +
  coord_flip() +
  labs(
    title = "Fondos de USAID per cápita (2024)",
    subtitle = "Por país",
    x = "",
    y = "Dólares por cada habitante",
    caption = aid_caption) +
  scale_fill_manual(values = rev(pal[1:2])) +
  scale_y_continuous(labels = scales::dollar_format()) +
  tema +
  theme(legend.position = "none")

for (device in devices){
  ggsave(usaid_per_capita_latam_2024_graf,
         filename = paste0(paths$output, "usaid_per_capita_latam_2024.", device),
         width = 12, height = 8)
  
  walk(paste0(paths$output, "usaid_per_capita_latam_2024.", device),
       ~add_both_logos(.x, escala = 7))
}

# AID PURPOSE (2024, MX VS. NON-MX. LATAM AVERAGE) ====
usaid_purpose_mx_vs_latam <- foreign_aid %>% 
  filter(fiscal_year == 2024, 
         managing_agency_name == "U.S. Agency for International Development",
         !str_detect(tolower(country_name), "region|world"),
         constant_dollar_amount > 0,
         region_name == "Western Hemisphere") %>%
  mutate(mx = ifelse(country_name == "Mexico", "México", "Resto de LATAM")) %>% 
  group_by(mx, international_purpose_name) %>%
  summarize(constant_dollar_amount = sum(constant_dollar_amount, na.rm = TRUE)) %>%
  group_by(mx) %>% 
  mutate(perc = constant_dollar_amount / sum(constant_dollar_amount),
         perc_mx = if("México" %in% mx) perc[mx == "México"] else NA_real_) %>% 
  filter(!international_purpose_name %in% c("Operating Expenses", 
                                            "General budget support-related aid",
                                            "Administration and Oversight")) %>%
  ungroup() %>% 
  mutate(rank = rank(-perc_mx, ties.method = "first")) %>% 
  group_by(international_purpose_name) %>%
  mutate(rank = min(rank, na.rm = T)) %>% 
  ungroup() %>% 
  filter(rank <= 16) %>% 
  mutate(international_purpose_name = case_when(
    international_purpose_name == "Agricultural policy and administrative management" ~ "Política agrícola",
    international_purpose_name == "Anti-corruption organisations and institutions" ~ "Anticorrupción",
    international_purpose_name == "Biosphere protection" ~ "Protección de la biosfera",
    international_purpose_name == "Business Policy and Administation" ~ "Política empresarial",
    international_purpose_name == "Civilian peace-building, conflict prevention and resolution" ~ "Prevención y resolución de conflictos",
    international_purpose_name == "Decentralisation and support to subnational government" ~ "Apoyo a gobiernos subnacionales",
    international_purpose_name == "Democratic participation and civil society" ~ "Participación democrática y sociedad civil",
    international_purpose_name == "Energy generation, renewable sources - multiple technologies" ~ "Energía renovable",
    international_purpose_name == "Energy policy and administrative management" ~ "Política energética",
    international_purpose_name == "Environmental policy and administrative management" ~ "Política ambiental",
    international_purpose_name == "Higher education" ~ "Educación superior",
    international_purpose_name == "Human rights" ~ "Derechos humanos",
    international_purpose_name == "Legal and judicial development" ~ "Desarrollo legal y judicial",
    international_purpose_name == "Multisector aid" ~ "Ayuda multisectorial",
    international_purpose_name == "Public sector policy and administrative management" ~ "Política del sector público",
    international_purpose_name == "Relief co-ordination; protection and support services" ~ "Coordinación de ayuda humanitaria",
    international_purpose_name == "Upper Secondary education" ~ "Educación media superior"))

usaid_purpose_mx_vs_latam_graf <- usaid_purpose_mx_vs_latam %>% 
  ggplot(aes(x = reorder(international_purpose_name, -rank), y = perc)) +
  geom_col(aes(fill = mx), position = "dodge") +
  geom_text(aes(label = percent(perc, accuracy = 0.1), group = mx),
            position = position_dodge(width = 0.9), family = labels_font,
            fontface = "bold", hjust = -0.05) +
  coord_flip() +
  labs(
    title = "Distribución de fondos de USAID (2024), por propósito",
    subtitle = paste0("<span style='color:", pal[1], ";'>**México**</span> vs. <span style='color:", pal[2], ";'>**resto de LATAM**</span>"),
    x = "",
    y = "Porcentaje del total para cada región",
    caption = aid_caption) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent_format()) +
  tema +
  theme(legend.position = "none",
        plot.subtitle = element_markdown())

for (device in devices){
  ggsave(usaid_purpose_mx_vs_latam_graf,
         filename = paste0(paths$output, "usaid_purpose_mx_vs_latam.", device),
         width = 12, height = 8)
  
  walk(paste0(paths$output, "usaid_purpose_mx_vs_latam.", device),
       ~add_both_logos(.x, escala = 7))
}

# done.