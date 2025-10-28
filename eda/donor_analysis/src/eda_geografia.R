#
# Author: SW
# Maintainer(s): SW, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, readxl, janitor, ggthemes, scales,
               ggrepel, ggtext, data.table, upstartr)

# Files ====
paths <- list(output = here("donor_analysis/eda/output/"))

drive_auth(email = "sierra.wells@datacivica.org")

# Informe de transparencia 2024
informe_transparencia_id <- drive_ls(as_id("1C1nq47Q8fnMRaVa1wMeZb1ZIpIRl-Pt0")) %>% 
  filter(str_detect(name, "2024")) %>% 
  pull(id)

stopifnot(length(informe_transparencia_id) == 1)

# OSC classifications
movements_id <- drive_ls(as_id("16CVo8XsSrjyMQyoS2g5R-yPIQhV5nGjm")) %>% 
  filter(str_detect(name, "movements_classified_joined.csv")) %>% 
  pull(id)

stopifnot(length(movements_id) == 1)

# Población
pob_id <- as_id("1XAf5VFOLxqACduLX08192GjfPhgvfHjt")

# READ DATA ====
drive_download(informe_transparencia_id,
               path = paste0(tempdir(), "/informe_transparencia_2024.xlsx"),
               overwrite = TRUE)

# Destino de donativos
destino_de_donativos <- read_excel(paste0(tempdir(), "/informe_transparencia_2024.xlsx"),
                                         sheet = "Destino de donativos") %>% 
  clean_names()

# Carátula
caratula <- read_excel(paste0(tempdir(), "/informe_transparencia_2024.xlsx"),
                                         sheet = "Carátula") %>% 
  clean_names()

file.remove(paste0(tempdir(), "/informe_transparencia_2024.xlsx"))

# OSC classifications
drive_download(movements_id,
               path = paste0(tempdir(), "/movements_classified_joined.csv"),
               overwrite = TRUE)

osc_classified <- read_csv(paste0(tempdir(), "/movements_classified_joined.csv")) %>% 
  select(rfc, ntee, main_movement)

file.remove(paste0(tempdir(), "/movements_classified_joined.csv"))

# Población
drive_download(pob_id,
               path = paste0(tempdir(), "/poblacion.csv"),
               overwrite = TRUE)

poblacion <- fread(paste0(tempdir(), "/poblacion.csv")) %>% 
  filter(nivel == "muni") %>% 
  select(nom_ent, nom_mun, pobtot) %>% 
  mutate(
    nom_ent = case_when(
      nom_ent == "México" ~ "Estado de México",
      nom_ent == "Coahuila de Zaragoza" ~ "Coahuila",
      nom_ent == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
      nom_ent == "Michoacán de Ocampo" ~ "Michoacán",
      T ~ nom_ent),
    nom_mun = tolower(unaccent(nom_mun)),
    nom_mun = case_when(
      str_detect(nom_mun, "villa tezoatlan de segura y luna") ~ "villa tezoatlan de segura y luna",
      T ~ nom_mun))

file.remove(paste0(tempdir(), "/poblacion.csv"))

# GRAPH PREP ====
caption_informe_transparencia <- paste0("Fuente: Elaboración por Data Cívica, a partir del Informe de Transparencia del SAT 2024.")

# SEDES POR ESTADO ====
n_osc_sedes_por_estado <- caratula %>% 
  group_by(entidad_federativa) %>% 
  summarise(n_osc = n_distinct(rfc)) %>% 
  mutate(perc = n_osc / sum(n_osc, na.rm = TRUE))

# 1 de cada 4 OSC (24.9%) tiene su sede en la CDMX

graf_n_osc_sedes_por_estado <- ggplot(n_osc_sedes_por_estado,
                                      aes(x = reorder(entidad_federativa, n_osc), y = n_osc)) +
  geom_col(fill = gdocs_pal()(1)) +
  geom_label(aes(label = paste0(n_osc, " (", percent(perc, accuracy = 0.1), ")")),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  coord_flip() +
  labs(title = "¿Dónde se concentran las sedes de las OSC?",
       subtitle = "Número de donatarias autorizadas con su sede principal en cada entidad federativa",
       x = "",
       y = "",
       caption = caption_informe_transparencia) +
  scale_y_continuous(limits = c(0, max(n_osc_sedes_por_estado$n_osc) * 1.1)) +
  theme_gdocs()

ggsave(filename = paste0(paths$output, "n_osc_sedes_por_estado.png"),
       plot = graf_n_osc_sedes_por_estado,
       width = 12, height = 8)

# DESTINO DE DONATIVOS POR ESTADO ====
perc_donativos_por_estado <- destino_de_donativos %>% 
  group_by(entidad_federativa) %>% 
  summarise(total = sum(monto, na.rm = TRUE)) %>% 
  mutate(perc = total / sum(total))

# Casi 1 de cada 3 pesos (30.6%) recibidos por las OSC se destinaron a la CDMX

graf_perc_donativos_por_estado <- ggplot(perc_donativos_por_estado,
                                       aes(x = reorder(entidad_federativa, perc), y = perc)) +
  geom_col(fill = gdocs_pal()(1)) +
  geom_label(aes(label = percent(perc, accuracy = 0.1)),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  coord_flip() +
  labs(title = "¿A dónde se destinan los donativos a las OSC?",
       subtitle = "Porcentaje de donativos destinados a cada entidad federativa",
       x = "",
       y = "",
       caption = caption_informe_transparencia) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                         limits = c(0, max(perc_donativos_por_estado$perc) * 1.05)) +
  theme_gdocs()

ggsave(filename = paste0(paths$output, "perc_donativos_por_estado.png"),
       plot = graf_perc_donativos_por_estado,
       width = 12, height = 8)

# % DONATIVOS A MUNICIPIOS RURALES, POR MOVIMIENTO ====
perc_donativos_municipios_rurales <- destino_de_donativos %>% 
  mutate(municipio = tolower(unaccent(municipio)),
         municipio = case_when(
           entidad_federativa == "Oaxaca" & municipio == "heroica ciudad de juchitan de zaragoza" ~ "juchitan de zaragoza",
           entidad_federativa == "Oaxaca" & municipio == "santiago chazumba" ~ "villa de santiago chazumba",
           entidad_federativa == "Chihuahua" & municipio == "batopilas" ~ "batopilas de manuel gomez morin",
           T ~ municipio)) %>%
  left_join(osc_classified, by = "rfc") %>% 
  left_join(poblacion %>% 
              group_by(nom_ent, nom_mun) %>% 
              reframe(menos_100_mil = pobtot < 100000) %>% 
              distinct(),
            by = c("entidad_federativa" = "nom_ent", "municipio" = "nom_mun")) %>% 
  filter(!is.na(main_movement)) %>% 
  group_by(main_movement) %>% 
  reframe(total = sum(monto, na.rm = TRUE),
          total_rural = sum(monto[menos_100_mil], na.rm = TRUE),
          n_osc = n_distinct(rfc)) %>%
  ungroup() %>% 
  mutate(mean_perc_rural = sum(total_rural, na.rm = TRUE) / sum(total, na.rm = TRUE)) %>% 
  filter(n_osc > 10) %>% 
  mutate(perc_rural = total_rural / total,
         rank = rank(-perc_rural),
         rank_categ = case_when(
           rank <= 5 ~ "Más altos",
           rank >= 35 ~ "Más bajos",
           T ~ NA_character_)) %>% 
  filter(!is.na(rank_categ))

# En general, solo 11.4% de los donativos se destinaron a municipios con menos de 100,000 habitantes.
# unique(perc_donativos_municipios_rurales$mean_perc_rural)

graf_perc_donativos_municipios_rurales <- ggplot(perc_donativos_municipios_rurales,
                                                  aes(x = reorder(str_wrap(main_movement, 40),
                                                                  perc_rural), y = perc_rural)) +
  geom_col(aes(fill = rank_categ), show.legend = FALSE) +
  geom_label(aes(label = percent(perc_rural, accuracy = 0.1)),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  facet_wrap(~rank_categ, ncol = 1, scales = "free_y") +
  coord_flip() +
  labs(title = "¿Cuáles movimientos destinan más donativos a zonas rurales?",
       subtitle = "Porcentaje de donativos destinados a municipios con menos de 100,000 habitantes",
       x = "",
       y = "",
       caption = caption_informe_transparencia) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                         limits = c(0, max(perc_donativos_municipios_rurales$perc_rural) * 1.05)) +
  scale_fill_manual(values = c("Más altos" = gdocs_pal()(1), "Más bajos" = gdocs_pal()(2)[2])) +
  theme_gdocs()

ggsave(filename = paste0(paths$output, "perc_donativos_municipios_rurales.png"),
       plot = graf_perc_donativos_municipios_rurales,
       width = 12, height = 8)
