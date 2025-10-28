# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2024 ©
# ---------------------------------------------

# TODO

# Packages

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, googledrive, ggplot2, ggthemes, scales, here)

# Files
paths <- list(output = here("encig/eda/output/"))

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
encig_caption <- paste0("Fuente: Elaboración por Data Cívica a partir de la Encuesta Nacional de Calidad e Impacto Gubernamental (ENCIG) 2019-2023",
                        "\nIntervalos de confianza al 95%.")

# PERCEPCIÓN DE CORRUPCIÓN DE LAS OSC POR AÑO ====
corr_osc_2019_23 <- encig %>% 
  group_by(year) %>% 
  reframe(
    perc = weighted.mean(corr_ong %in% c("Frecuente", "Muy frecuente"),
                        w = fac_p18, na.rm = TRUE),
    n = n()) %>%
  mutate(margin_error = 1.96 * sqrt((perc * (1 - perc)) / n))

graf_corr_osc_2019_23 <- ggplot(corr_osc_2019_23, aes(x = year, y = perc)) +
  geom_line(group = 1, color = gdocs_pal()(1), linewidth = 1.5) +
  geom_point(size = 3, color = gdocs_pal()(1)) +
  geom_ribbon(aes(ymin =perc,
                  ymax = pmin(1, perc + margin_error)),
              alpha = 0.2, fill = gdocs_pal()(1)) +
  geom_errorbar(aes(ymin = pmax(0, perc - margin_error),
                  ymax = pmin(1, perc + margin_error)),
                width = 0.2) +
  geom_text(aes(label = percent(perc, accuracy = 0.1)),
            vjust = -1, size = 5, fontface = "bold", color = gdocs_pal()(1)) +
  scale_x_continuous(breaks = seq(2019, 2023, by = 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Percepción de corrupción en las organizaciones de la sociedad civil (OSC)",
       subtitle = "Porcentaje de personas que consideran que la corrupción es frecuente o muy frecuente entre las OSC",
       x = "Año",
       y = "",
       caption = encig_caption) +
  theme_gdocs() +
  theme(legend.position = "none")

ggsave(paste0(paths$output, "graf_corr_osc_2019_23.png"),
       plot = graf_corr_osc_2019_23,
       width = 12, height = 6)

# CONFIANZA EN LAS OSC POR AÑO ====
conf_osc_2019_23 <- encig %>% 
  group_by(year) %>% 
  reframe(
    perc = weighted.mean(conf_ong %in% c("Mucha confianza", "Algo de confianza"),
                        w = fac_p18, na.rm = TRUE),
    n = n()) %>%
  mutate(margin_error = 1.96 * sqrt((perc * (1 - perc)) / n))

graf_conf_osc_2019_23 <- ggplot(conf_osc_2019_23, aes(x = year, y = perc)) +
  geom_line(group = 1, color = gdocs_pal()(1), linewidth = 1.5) +
  geom_point(size = 3, color = gdocs_pal()(1)) +
  geom_errorbar(aes(ymin = pmax(0, perc - margin_error),
                    ymax = pmin(1, perc + margin_error)),
                width = 0.2) +
  geom_text(aes(label = percent(perc, accuracy = 0.1)),
            vjust = -1, size = 5, fontface = "bold", color = gdocs_pal()(1)) +
  scale_x_continuous(breaks = seq(2019, 2023, by = 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  # coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Confianza en las organizaciones de la sociedad civil (OSC)",
       subtitle = "Porcentaje de personas que tienen mucha o algo de confianza en las OSC",
       x = "Año",
       y = "",
       caption = encig_caption) +
  theme_gdocs() +
  theme(legend.position = "none")

ggsave(paste0(paths$output, "graf_conf_osc_2019_23.png"),
       plot = graf_conf_osc_2019_23,
       width = 12, height = 6)

# done.