# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2024 ©
# ---------------------------------------------

# TODO

# Packages

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, googledrive, ggplot2, scales, here)

# Files
paths <- list(output = here("descriptives/output/"),
              tema = here("descriptives/tema.R"))

# Define Drive folders
drive_auth(email = "sierra.wells@datacivica.org")
drive_in_id <- drive_ls(as_id("1-n5ZlkcH1L5naEthn9dGKFOQ7bRunp59")) %>% 
  pull(id)

stopifnot(length(drive_in_id) == 1)

# READ DATA ====
# Read ENCIG
drive_download(as_id(drive_in_id), 
               path = paste0(tempdir(), "/encig_clean.csv"), 
               overwrite = TRUE)

encig <- read_csv(paste0(tempdir(), "/encig_clean.csv"))

file.remove(paste0(tempdir(), "/encig_clean.csv"))

# GRAPH PREP ====
source(paths$tema)

encig_caption <- paste0("Fuente: Elaboración por Data Cívica a partir de la ENCIG 2023",
                        "\nIntervalos de confianza al 95%")

# PERCEPCIÓN DE CORRUPCIÓN POR INSTITUCIÓN ====
corr_inst_2023 <- encig %>% 
  filter(year == 2023) %>% 
  reframe(
    across(.cols = starts_with("corr_"),
           ~ weighted.mean(.x %in% c("Frecuente", "Muy frecuente"),
                           w = fac_p18, na.rm = TRUE)),
    n = n()) %>% 
  pivot_longer(cols = starts_with("corr_"),
              names_to = "inst", values_to = "perc") %>%
  mutate(inst = case_when(
    inst == "corr_univ_pub" ~ "Universidades públicas",
    inst == "corr_policias" ~ "Policías",
    inst == "corr_hosp_pub" ~ "Hospitales públicos",
    inst == "corr_apf" ~ "Administración pública federal",
    inst == "corr_empresarios" ~ "Empresarios",
    inst == "corr_gob_est" ~ "Gobierno estatal",
    inst == "corr_comp_trab" ~ "Sus compañeros de trabajo",
    inst == "corr_gob_mun" ~ "Gobierno municipal",
    inst == "corr_parientes" ~ "Sus parientes",
    inst == "corr_sindicatos" ~ "Sindicatos",
    inst == "corr_vecinos" ~ "Sus vecinos",
    inst == "corr_camara_dip_sen" ~ "Cámara de Diputados y Senado",
    inst == "corr_medios_com" ~ "Medios de comunicación",
    inst == "corr_inst_elect" ~ "Instituciones electorales",
    inst == "corr_comis_ddhh" ~ "Comisiones de derechos humanos",
    inst == "corr_esc_pub" ~ "Escuelas públicas",
    inst == "corr_jueces" ~ "Jueces",
    inst == "corr_iglesia" ~ "Iglesias",
    inst == "corr_partidos_pol" ~ "Partidos políticos",
    inst == "corr_gn" ~ "Guardia Nacional",
    inst == "corr_ffaa" ~ "Fuerzas armadas",
    inst == "corr_mp_fiscalia" ~ "Ministerio Público y Fiscalías",
    inst == "corr_ong" ~ "OSC",
    inst == "corr_ocas" ~ "Organismos autónomos públicos"),
    margin_error = 1.96 * sqrt((perc * (1 - perc)) / n))

graf_corr_inst_2023 <- ggplot(corr_inst_2023, aes(x = fct_reorder(inst, perc), y = perc)) +
  geom_col(aes(fill = inst == "OSC")) +
  geom_text(aes(label = percent(perc, accuracy = 0.1)), family = labels_font,
            hjust = 1.3, size = 4, fontface = "bold", color = fondo) +
  geom_errorbar(aes(ymin = perc - margin_error, ymax = perc + margin_error), 
                width = 0.2, color = texto) +
  coord_flip() +
  labs(title = "Percepción de corrupción por institución",
       subtitle = "Porcentaje de personas que consideran que la corrupción es frecuente o muy frecuente entre los siguientes actores",
       x = "",
       y = "",
       caption = encig_caption) +
  tema +
  scale_fill_manual(values = pal) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "plain"))

for (device in devices){
  ggsave(paste0(paths$output, "corr_inst_2023", device),
         plot = graf_corr_inst_2023,
         width = 14, height = 10)
  
  walk(paste0(paths$output, "corr_inst_2023", device),
       ~add_both_logos(.x, escala = 7))
}

# CONFIANZA EN INSTITUCIONES ====
conf_inst_2023 <- encig %>% 
  filter(year == 2023) %>% 
  reframe(
    across(.cols = starts_with("conf_"),
           ~ weighted.mean(.x %in% c("Mucha confianza", "Algo de confianza"),
                           w = fac_p18, na.rm = TRUE)),
    n = n()) %>% 
  pivot_longer(cols = starts_with("conf_"),
               names_to = "inst", values_to = "perc") %>%
  mutate(inst = case_when(
    inst == "conf_univ_pub" ~ "Universidades públicas",
    inst == "conf_policias" ~ "Policías",
    inst == "conf_hosp_pub" ~ "Hospitales públicos",
    inst == "conf_apf" ~ "Administración pública federal",
    inst == "conf_empresarios" ~ "Empresarios",
    inst == "conf_gob_est" ~ "Gobierno estatal",
    inst == "conf_comp_trab" ~ "Sus compañeros de trabajo",
    inst == "conf_gob_mun" ~ "Gobierno municipal",
    inst == "conf_parientes" ~ "Sus parientes",
    inst == "conf_sindicatos" ~ "Sindicatos",
    inst == "conf_vecinos" ~ "Sus vecinos",
    inst == "conf_camara_dip_sen" ~ "Cámara de Diputados y Senado",
    inst == "conf_medios_com" ~ "Medios de comunicación",
    inst == "conf_inst_elect" ~ "Instituciones electorales",
    inst == "conf_comis_ddhh" ~ "Comisiones de derechos humanos",
    inst == "conf_esc_pub" ~ "Escuelas públicas",
    inst == "conf_jueces" ~ "Jueces",
    inst == "conf_iglesia" ~ "Iglesias",
    inst == "conf_partidos_pol" ~ "Partidos políticos",
    inst == "conf_gn" ~ "Guardia Nacional",
    inst == "conf_ffaa" ~ "Fuerzas armadas",
    inst == "conf_mp_fiscalia" ~ "Ministerio Público y Fiscalías",
    inst == "conf_ong" ~ "OSC",
    inst == "conf_ocas" ~ "Organismos autónomos públicos",
    inst == "conf_serv_pub" ~ "Servidores públicos",
    T ~ inst),
    margin_error = 1.96 * sqrt((perc * (1 - perc)) / n))

graf_conf_inst_2023 <- ggplot(conf_inst_2023, aes(x = fct_reorder(inst, perc), y = perc)) +
  geom_col(aes(fill = inst == "OSC")) +
  geom_text(aes(label = percent(perc, accuracy = 0.1)), family = labels_font,
            hjust = 1.3, size = 4, fontface = "bold", color = fondo) +
  geom_errorbar(aes(ymin = perc - margin_error, ymax = perc + margin_error), 
                width = 0.2, color = texto) +
  
  coord_flip() +
  labs(title = "Confianza en instituciones",
       subtitle = "Porcentaje de personas que tienen mucha o algo de confianza en los siguientes actores",
       x = "",
       y = "",
       caption = encig_caption) +
  tema +
  scale_fill_manual(values = pal) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "plain"))

for (device in devices){
  ggsave(paste0(paths$output, "conf_inst_2023.", device),
         plot = graf_conf_inst_2023,
         width = 14, height = 10)
  
  walk(paste0(paths$output, "conf_inst_2023.", device),
       ~add_both_logos(.x, escala = 7))
}

# CONFIANZA EN LAS OSC POR SEXO ====
conf_osc_sexo <- encig %>% 
  filter(year == 2023) %>% 
  group_by(sexo) %>% 
  reframe(
    perc = weighted.mean(conf_ong %in% c("Mucha confianza", "Algo de confianza"),
                        w = fac_p18, na.rm = TRUE),
    n = n()) %>% 
  mutate(margin_error = 1.96 * sqrt((perc * (1 - perc)) / n),
         sexo = ifelse(sexo == "Mujer", "Mujeres", "Hombres"))

graf_conf_osc_sexo <- ggplot(conf_osc_sexo, aes(y = sexo, x = perc)) +
  geom_col(aes(fill = sexo)) +
  geom_text(aes(label = percent(perc, accuracy = 0.1)), family = labels_font,
            vjust = -6, hjust = 1.1, size = 6, fontface = "bold", color = fondo) +
  geom_errorbar(aes(xmin = perc - margin_error, xmax = perc + margin_error), 
                width = 0.2, color = texto) +
  labs(title = "Confianza en las organizaciones de la sociedad civil (OSC) por sexo",
       subtitle = "Porcentaje de personas que tienen mucha o algo de confianza en las OSC",
       x = "",
       y = "",
       caption = encig_caption) +
  tema +
  scale_fill_manual(values = pal) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold"))

for (device in devices){
  ggsave(paste0(paths$output, "conf_osc_sexo.", device),
         plot = graf_conf_osc_sexo,
         width = 14, height = 8)
  
  walk(paste0(paths$output, "conf_osc_sexo.", device),
       ~add_both_logos(.x, escala = 7))
}

# CONFIANZA EN LAS OSC POR EDAD ====
conf_osc_edad <- encig %>% 
  filter(year == 2023,
         !is.na(edad)) %>% 
  mutate(edad_cat = case_when(
    edad %in% 18:24 ~ "18-24",
    edad %in% 25:34 ~ "25-34",
    edad %in% 35:44 ~ "35-44",
    edad %in% 45:54 ~ "45-54",
    edad %in% 55:64 ~ "55-64",
    edad >= 65 ~ "65+",
    T ~ NA_character_)) %>% 
  group_by(edad_cat) %>% 
  reframe(
    perc = weighted.mean(conf_ong %in% c("Mucha confianza", "Algo de confianza"),
                        w = fac_p18, na.rm = TRUE),
    n = n()) %>% 
  mutate(margin_error = 1.96 * sqrt((perc * (1 - perc)) / n))

graf_conf_osc_edad <- ggplot(conf_osc_edad, aes(y = fct_reorder(edad_cat, perc), x = perc)) +
  geom_col(fill = pal[1]) +
  geom_text(aes(label = percent(perc, accuracy = 0.1)), family = labels_font,
            vjust = -1.9, hjust = 1.1, size = 5, fontface = "bold", color = fondo) +
  geom_errorbar(aes(xmin = perc - margin_error, xmax = perc + margin_error), 
                width = 0.2, color = texto) +
  labs(title = "Confianza en las organizaciones de la sociedad civil (OSC) por edad",
       subtitle = "Porcentaje de personas que tienen mucha o algo de confianza en las OSC",
       x = "",
       y = "Grupo de edad (años)",
       caption = encig_caption) +
  tema +
  scale_fill_manual(values = pal) +
  theme(legend.position = "none",
        axis.text.x = element_blank())

for (device in devices){
  ggsave(paste0(paths$output, "conf_osc_edad.", device),
         plot = graf_conf_osc_edad,
         width = 14, height = 8)
  
  walk(paste0(paths$output, "conf_osc_edad.", device),
       ~add_both_logos(.x, escala = 7))
}

# done.
