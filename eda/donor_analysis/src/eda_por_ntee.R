#
# Author: SW
# Maintainer(s): SW, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, readxl, janitor, ggthemes, scales,
               ggrepel)

# Files ====
paths <- list(output = here("donor_analysis/eda/output/"))

drive_auth(email = "sierra.wells@datacivica.org")

# Informe de transparencia 2024
informe_transparencia_id <- drive_ls(as_id("1C1nq47Q8fnMRaVa1wMeZb1ZIpIRl-Pt0")) %>% 
  filter(str_detect(name, "2024")) %>% 
  pull(id)

stopifnot(length(informe_transparencia_id) == 1)

# Códigos NTEE
ntee_id <- drive_ls(as_id("16CVo8XsSrjyMQyoS2g5R-yPIQhV5nGjm")) %>% 
  filter(str_detect(name, "ntee_classified_joined.csv")) %>% 
  pull(id)

stopifnot(length(ntee_id) == 1)

# NTEE descriptions
ntee_descrip_id <- drive_ls(as_id("1p25pJ15lMJUhtlGQCbb8ADo8vuUueM5J")) %>% 
  filter(name == "ntee_to_movements_class") %>% 
  pull(id)

stopifnot(length(ntee_descrip_id) == 1)

# NTEE groupings
ntee_groupings_id <- drive_ls(as_id("1xtTU_ZO8T2mFQoEx-uBHET-VUCbIdZtJ")) %>% 
  filter(str_detect(name, "2023")) %>% 
  pull(id)

stopifnot(length(ntee_groupings_id) == 1)

# READ DATA ====
# Informe de transparencia 2024 (Ingresos por donativos)
drive_download(informe_transparencia_id,
               path = paste0(tempdir(), "/informe_transparenica_2024.xlsx"),
               overwrite = TRUE)

ingresos_por_donativos_raw <- read_excel(paste0(tempdir(), "/informe_transparenica_2024.xlsx"),
                                         sheet = "Ingreso por donativos")

file.remove(paste0(tempdir(), "/informe_transparenica_2024.xlsx"))

# Códigos NTEE
drive_download(ntee_id,
               path = paste0(tempdir(), "/ntee_classified_joined.csv"),
               overwrite = TRUE)

ntee_codes <- read_csv(paste0(tempdir(), "/ntee_classified_joined.csv")) %>% 
  select(rfc, ntee, tipo_clasificacion_ntee) %>% 
  mutate(ntee = toupper(ntee))

file.remove(paste0(tempdir(), "/ntee_classified_joined.csv"))

# NTEE descriptions
drive_download(ntee_descrip_id,
               path = paste0(tempdir(), "/ntee_descrip.csv"),
               overwrite = TRUE)

ntee_descrip <- read_csv(paste0(tempdir(), "/ntee_descrip.csv"))

file.remove(paste0(tempdir(), "/ntee_descrip.csv"))

# NTEE groupings
drive_download(ntee_groupings_id,
               path = paste0(tempdir(), "/ntee_groupings.xlsx"),
               overwrite = TRUE)

ntee_groupings <- read_excel(paste0(tempdir(), "/ntee_groupings.xlsx"),
                             sheet = "Agrupación- Descriptiva") %>% 
  clean_names() %>% 
  slice(1:11) %>% 
  select(agrupacion, tematica) %>% 
  separate_rows(agrupacion, sep = ",")%>% 
  mutate(agrupacion = str_squish(agrupacion))

file.remove(paste0(tempdir(), "/ntee_groupings.xlsx"))

# RFC donantes
rfc_donantes <- readLines(paste0(paths$output, "rfc_donantes_2024.txt"))
rfc_top_donantes <- readLines(paste0(paths$output, "rfc_top_donantes_2024.txt"))

# CLEAN DATA ====
ingresos_por_donativos <- ingresos_por_donativos_raw %>% 
  clean_names() %>% 
  mutate(total = monto_efectivo + monto_especie) %>% 
  left_join(ntee_codes, by = "rfc", relationship = "many-to-many") %>% 
  filter(! rfc %in% rfc_donantes)

# GRAPH PREP ====
caption_informe_transparencia <- "Fuente: Elaboración por Data Cívica, a partir del Informe de Transparencia del SAT 2024"

# TOTAL FUNDING BY NTEE GROUP ====
funding_by_ntee_group <- ingresos_por_donativos %>% 
  filter(!is.na(ntee),
         !startsWith(ntee, "Z")) %>% 
  mutate(first_letter_ntee = substr(ntee, 1, 1)) %>% 
  left_join(ntee_groupings, by = c("first_letter_ntee" = "agrupacion")) %>% 
  group_by(tematica) %>% 
  reframe(total = sum(total, na.rm = TRUE),
          n_osc = n_distinct(rfc),
          total_mdp = total / 1e6,
          funding_per_osc_mdp = total_mdp / n_osc)

graf_funding_by_ntee_group <- funding_by_ntee_group %>% 
  ggplot(aes(y = reorder(str_wrap(tematica, 30), total_mdp), x = total_mdp)) +
  geom_col(fill = gdocs_pal()(1)) +
  geom_label(aes(label = dollar(total_mdp)),
            hjust = -0.1, size = 3.5, fontface = "bold",
            fill = "white", label.size = NA) +
  labs(title = "¿Cuáles sectores de la sociedad civil recibieron más donativos en 2024?",
       subtitle = "Total de donativos (MDP) desagregado por agrupación NTEE",
       x = "",
       y = "",
       caption = caption_informe_transparencia) +
  scale_x_continuous(labels = dollar_format(),
                     limits = c(0, max(funding_by_ntee_group$total_mdp) * 1.05)) +
  theme_gdocs()

ggsave(filename = paste0(paths$output, "total_fund_by_ntee_group.png"),
       plot = graf_funding_by_ntee_group,
       width = 12, height = 8)

# PER CÁPITA FUNDING BY NTEE GROUP ====
graf_funding_per_cap_by_ntee_group <- funding_by_ntee_group %>% 
  filter(tematica != "Organizaciones de Autobeneficio y por Membresía") %>% 
  ggplot(aes(y = reorder(str_wrap(tematica, 30), funding_per_osc_mdp), 
             x = funding_per_osc_mdp)) +
  geom_col(fill = gdocs_pal()(1)) +
  geom_label(aes(label = dollar(funding_per_osc_mdp)),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  labs(title = "¿Cuáles sectores de la sociedad civil recibieron más donativos por\norganización en 2024?",
       subtitle = "Ingresos por cada donataria autorizada (MDP), desagregado por agrupación NTEE",
       x = "",
       y = "",
       caption = caption_informe_transparencia) +
  scale_x_continuous(labels = dollar_format(),
                     limits = c(0, max(funding_by_ntee_group$funding_per_osc_mdp) * 1.05)) +
  theme_gdocs()

ggsave(filename = paste0(paths$output, "fund_per_cap_by_ntee_group.png"),
       plot = graf_funding_per_cap_by_ntee_group,
       width = 12, height = 8)

# NUMBER OF OSC BY NTEE GROUP ====
n_osc_by_ntee_group <- ntee_codes %>% 
  mutate(first_letter_ntee = substr(ntee, 1, 1)) %>%
  left_join(ntee_groupings, by = c("first_letter_ntee" = "agrupacion")) %>%
  group_by(tematica) %>%
  reframe(n_osc = n_distinct(rfc)) %>%
  filter(!is.na(tematica))

graf_n_osc_by_ntee_group <- n_osc_by_ntee_group %>% 
  ggplot(aes(y = reorder(str_wrap(tematica, 30), n_osc), x = n_osc)) +
  geom_col(fill = gdocs_pal()(1)) +
  geom_label(aes(label = n_osc),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  labs(title = "¿Cuáles sectores de la sociedad civil tienen más organizaciones?",
       subtitle = "Número de donatarias autorizadas de cada agrupación NTEE",
       x = "",
       y = "",
       caption = caption_informe_transparencia) +
  scale_x_continuous(limits = c(0, max(n_osc_by_ntee_group$n_osc) * 1.05)) +
  theme_gdocs()

ggsave(filename = paste0(paths$output, "n_osc_by_ntee_group.png"),
       plot = graf_n_osc_by_ntee_group,
       width = 12, height = 8)

# % OF FOREIGN FUNDING BY NTEE GROUP ====
foreign_funding_by_ntee_group <- ingresos_por_donativos %>% 
  filter(!is.na(ntee),
         !startsWith(ntee, "Z")) %>% 
  mutate(first_letter_ntee = substr(ntee, 1, 1),
         foreign = ifelse(str_detect(donante, "Nacional"), 0, 1)) %>% 
  left_join(ntee_groupings, by = c("first_letter_ntee" = "agrupacion")) %>% 
  group_by(tematica, foreign) %>%
  reframe(total = sum(total, na.rm = TRUE),
          n_osc = n_distinct(rfc)) %>% 
  group_by(tematica) %>% 
  mutate(perc = total / sum(total, na.rm = TRUE)) %>% 
  filter(foreign == 1,
         n_osc >= 25)

graf_foreign_funding_by_ntee_group <- foreign_funding_by_ntee_group %>%
  ggplot(aes(y = reorder(str_wrap(tematica, 30), perc), x = perc)) +
  geom_col(fill = gdocs_pal()(1)) +
  geom_label(aes(label = percent(perc, accuracy = 0.1)),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  labs(title = "¿Cuáles sectores de la sociedad civil reciben más financiamiento extranjero?",
       subtitle = "Porcentaje de donativos a donatarias autorizadas que son de donantes extranjeros, desagregado por agrupación NTEE",
       x = "",
       y = "",
       caption = caption_informe_transparencia) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1),
                     limits = c(0, max(foreign_funding_by_ntee_group$perc) * 1.05)) +
  theme_gdocs()

ggsave(filename = paste0(paths$output, "foreign_funding_by_ntee_group.png"),
       plot = graf_foreign_funding_by_ntee_group,
       width = 12, height = 8)

# % OF FUNDING FROM MEXICAN PUBLIC SECTOR ====
public_nac_funding_by_ntee_group <- ingresos_por_donativos %>% 
  filter(!is.na(ntee),
         !startsWith(ntee, "Z")) %>% 
  mutate(first_letter_ntee = substr(ntee, 1, 1),
         public_nac = ifelse(donante == "Donativo Sector Público Nacional", 1, 0)) %>% 
  left_join(ntee_groupings, by = c("first_letter_ntee" = "agrupacion")) %>% 
  group_by(tematica, public_nac) %>%
  reframe(total = sum(total, na.rm = TRUE),
          n_osc = n_distinct(rfc)) %>% 
  group_by(tematica) %>% 
  mutate(perc = total / sum(total, na.rm = TRUE)) %>% 
  filter(public_nac == 1,
         n_osc >= 25)

graf_public_nac_funding_by_ntee_group <- public_nac_funding_by_ntee_group %>%
  ggplot(aes(y = reorder(str_wrap(tematica, 30), perc), x = perc)) +
  geom_col(fill = gdocs_pal()(1)) +
  geom_label(aes(label = percent(perc, accuracy = 0.1)),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  labs(title = "¿Cuáles sectores de la sociedad civil reciben más financiamiento del\nsector público nacional?",
       subtitle = "Porcentaje de donativos a donatarias autorizadas que son del sector público nacional, desagregado por agrupación NTEE",
       x = "",
       y = "",
       caption = caption_informe_transparencia) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1),
                     limits = c(0, max(public_nac_funding_by_ntee_group$perc) * 1.05)) +
  theme_gdocs()

ggsave(filename = paste0(paths$output, "public_nac_funding_by_ntee_group.png"),
       plot = graf_public_nac_funding_by_ntee_group,
       width = 12, height = 8)

# done.              
