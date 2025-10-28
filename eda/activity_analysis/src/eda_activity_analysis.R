#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# TODO
# Do distribución de año de inscripción para DAs

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, scales, ggthemes, VennDiagram,
               eulerr)

# Files
paths <- list(output = here("activity_analysis/eda/output"))

drive_auth(email = "sierra.wells@datacivica.org")

drive_in_id <- drive_ls(as_id("1f8-zhH4OxE-vipndMsIJgVJz4O7o76My")) %>% 
  filter(name == "da_indesol_historico.csv") %>% 
  pull(id)

stopifnot(length(drive_in_id) == 1)

# READ DATA ====
drive_download(drive_in_id,
               path = paste0(tempdir(), "/ayc_w_status.csv"),
               overwrite = TRUE)

ayc_w_status <- read_csv(paste0(tempdir(), "/ayc_w_status.csv"))

file.remove(paste0(tempdir(), "/ayc_w_status.csv"))

# sum(ayc_w_status$tiene_cluni, na.rm = T)
# 44,993 OSC c/ CLUNI según AyC

n_total_osc_indesol <- sum(!is.na(ayc_w_status$ultimo_anio_informe_indesol))
# 44,871 OSC c/ CLUNI según INDESOL

n_total_union_indesol_sat <- sum(!is.na(ayc_w_status$estatus_indesol) | 
                                 !is.na(ayc_w_status$estatus_da))
# 52,700 OSC inscritas en el registro del INDESOL y/o como DA

n_total_da <- sum(!is.na(ayc_w_status$estatus_da))

# INDESOL: ¿CUÁL FUE EL ÚLTIMO AÑO QUE PRESENTARON SU INFORME ANUAL? ====

ultimo_anio_informe <- ayc_w_status %>% 
  filter(!is.na(ultimo_anio_informe_indesol)) %>%
  mutate(
    ultimo_anio_informe_indesol = factor(ultimo_anio_informe_indesol, 
                                 levels = c(
                                   "Nunca",
                                   as.character(2005:2024)))) %>% 
  group_by(ultimo_anio_informe_indesol) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(perc = n / sum(n),
         activo = ifelse(ultimo_anio_informe_indesol %in% as.character(2023:2024), T, F))

ultimo_anio_informe_bar <- ultimo_anio_informe %>% 
  ggplot(aes(x = fct_rev(ultimo_anio_informe_indesol), y = perc, fill = activo)) +
  geom_col() + 
  geom_label(aes(label = paste0(comma(n), " (", percent(perc, accuracy = 0.1), ")")),
            fontface = "bold", nudge_y = 0.01, fill = "white", label.size = NA) + 
  coord_flip() +
  labs(title = "Último año que OSC en el registro del INDESOL presentaron su informe anual",
       subtitle = paste0("De un total de ", comma(n_total_osc_indesol), " OSC en el registro"),
       caption = paste0("Fuente: Elaboración por Data Cívica a partir del Registro Federal de las OSC 2025",
                        "\nSe excluyen organizaciones que se inscribieron a partir de 2024"),
       x = "", y = "") +
  theme_gdocs() +
  scale_fill_manual(values = c("FALSE" = "#f07b72", "TRUE" = "#71c287")) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

ggsave(ultimo_anio_informe_bar,
         filename = paste0(paths$output, "/ultimo_anio_informe_indesol.png"),
         width = 12, height = 8)

# INDESOL: DISTRIBUCIÓN DE AÑOS DE INSCRIPCIÓN DE OSC EN EL REGISTRO DEL INDESOL ====
anio_inscripcion_indesol <- ayc_w_status %>% 
  filter(anio_inscripcion_indesol %in% 2005:2024) %>% 
  group_by(anio_inscripcion_indesol) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(perc = n / sum(n),
         anio_inscripcion_indesol = factor(anio_inscripcion_indesol, 
                                           levels = 2005:2024))

anio_inscripcion_indesol_bar <- anio_inscripcion_indesol %>%
  ggplot(aes(x = fct_rev(anio_inscripcion_indesol), y = perc)) +
  geom_col(fill = gdocs_pal()(3)[3]) +
  geom_text(aes(label = paste0(comma(n), " (", percent(perc, accuracy = 0.1), ")")),
            fontface = "bold", nudge_y = -0.005, color = "white") +
  coord_flip() + 
  labs(title = "Año de inscripción de las OSC en el registro del INDESOL",
       subtitle = paste0("De un total de ", comma(n_total_osc_indesol), " OSC en el registro"),
       caption = paste0("Fuente: Elaboración por Data Cívica a partir del Registro Federal de las OSC 2025",
                        "\nSe excluye 2025 ya que no ha concluido el año"),
       x = "", y = "") +
  theme_gdocs() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())


ggsave(anio_inscripcion_indesol_bar, 
         filename = paste0(paths$output, "/anio_inscripcion_indesol.png"),
         width = 12, height = 8)

# INDESOL: DE OSC QUE PRESENTARON EN AÑO N-1, ¿QUÉ % PRESENTÓ EN AÑO N? ====

perc_presentaron_lag_ls <- list()

for (anio in 2006:2024){
  col_anio <- paste0("informe_", anio, "_presentado_indesol")
  col_anio_lag <- paste0("informe_", anio - 1, "_presentado_indesol")
  
  tempo <- ayc_w_status %>% 
    # Only OSC that presented the year prior
    filter(!!sym(col_anio_lag) == 1) %>%
    pull(col_anio) %>% 
    # What % presented that year?
    mean(na.rm = T)
  
  perc_presentaron_lag_ls[[as.character(anio)]] <- tempo
    
}

perc_presentaron_lag <- tibble(
  anio = names(perc_presentaron_lag_ls),
  perc = unlist(perc_presentaron_lag_ls)) %>% 
  mutate(anio = factor(anio, levels = as.character(2006:2024)))

perc_presentaron_lag_bar <- perc_presentaron_lag %>%
  ggplot(aes(x = fct_rev(anio), y = perc)) +
  geom_col(fill = gdocs_pal()(1)) +
  geom_text(aes(label = percent(perc, accuracy = 0.1)),
             fontface = "bold", nudge_y = -0.025, color = "white") +
  coord_flip() + 
  labs(title = "De las OSC que presentaron su informe anual ante el INDESOL el año anterior, % que\nvolvió a presentar",
       subtitle = "Por año",
       caption = "Fuente: Elaboración por Data Cívica a partir del Registro Federal de las OSC 2025",
       x = "", y = "") +
  theme_gdocs() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

ggsave(perc_presentaron_lag_bar, 
         filename = paste0(paths$output, "/perc_indesol_lag.png"),
         width = 12, height = 8)

# DA: ¿CUÁL FUE EL ÚLTIMO AÑO QUE CONTARON CON AUTORIZACIÓN COMO DA? ====
ultimo_anio_da <- ayc_w_status %>% 
  filter(!is.na(ultimo_anio_da)) %>%
  group_by(ultimo_anio_da) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(perc = n / sum(n),
         activo = ifelse(ultimo_anio_da == 2025, T, F),
         ultimo_anio_da = factor(ultimo_anio_da, 
                                 levels = 2002:2025))

ultimo_anio_da_bar <- ultimo_anio_da %>%
  ggplot(aes(x = fct_rev(ultimo_anio_da), y = perc, fill = activo)) +
  geom_col() +
  geom_label(aes(label = paste0(comma(n), " (", percent(perc, accuracy = 0.1), ")")),
             fontface = "bold", nudge_y = 0.03, fill = "white", label.size = NA) +
  coord_flip() +
  labs(title = "Último año que OSC contaron con autorización como donatarias autorizadas (DA)",
       subtitle = paste0("De un total de ", comma(n_total_da), " OSC autorizadas como DA en algún momento entre 2008 y 2025"),
       caption = "Fuente: Elaboración por Data Cívica a partir de los directorios de donatarias autorizadas",
       x = "", y = "") +
  theme_gdocs() +
  scale_fill_manual(values = c("FALSE" = "#f07b72", "TRUE" = "#71c287")) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

ggsave(ultimo_anio_da_bar, 
         filename = paste0(paths$output, "/ultimo_anio_da.png"),
         width = 12, height = 8)

# DA: DISTRIBUCIÓN DE AÑOS DE AUTORIZACIÓN DE DA ====
anio_autorizacion_da <- ayc_w_status %>% 
  filter(anio_autorizacion_da %in% 2002:2024) %>% 
  group_by(anio_autorizacion_da) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(perc = n / sum(n),
         anio_autorizacion_da = factor(anio_autorizacion_da, 
                                       levels = 2002:2024))

anio_autorizacion_da_bar <- anio_autorizacion_da %>%
  ggplot(aes(x = fct_rev(anio_autorizacion_da), y = perc)) +
  geom_col(fill = gdocs_pal()(3)[3]) +
  geom_text(aes(label = paste0(comma(n), " (", percent(perc, accuracy = 0.1), ")")),
            fontface = "bold", nudge_y = -0.005, color = "white") +
  coord_flip() + 
  labs(title = "Año de autorización de donatarias autorizadas",
       subtitle = paste0("De un total de ", comma(n_total_da), " OSC autorizadas como DA en algún momento"),
       caption = paste0("Fuente: Elaboración por Data Cívica a partir de los directorios de donatarias autorizadas",
                        "\nIncluye OSC que ya no cuentan con la autorización como DA,",
                        "\nSe excluye 2025 ya que no ha concluido el año"),
       x = "", y = "") +
  theme_gdocs() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

ggsave(anio_autorizacion_da_bar, 
         filename = paste0(paths$output, "/anio_autorizacion_da.png"),
         width = 12, height = 8)

# DA: DE OSC QUE CONTARON CON AUTORIZACIÓN COMO DA, ¿QUÉ % MANTUVIERON SU AUTORIZACIÓN? ====
perc_mantuvieron_da_ls <- list()

for (anio in 2009:2025){
  col_anio <- paste0("directorio_", anio, "_da")
  col_anio_lag <- paste0("directorio_", anio - 1, "_da")
  
  tempo <- ayc_w_status %>% 
    # Only OSC that were DA the year prior
    filter(!!sym(col_anio_lag) == 1) %>%
    pull(col_anio) %>% 
    # What % maintained their DA status that year?
    mean(na.rm = T)
  
  perc_mantuvieron_da_ls[[as.character(anio)]] <- tempo
    
}

perc_mantuvieron_da <- tibble(
  anio = names(perc_mantuvieron_da_ls),
  perc = unlist(perc_mantuvieron_da_ls)) %>% 
  mutate(anio = factor(anio, levels = as.character(2009:2025)))

perc_mantuvieron_da_bar <- perc_mantuvieron_da %>%
  ggplot(aes(x = fct_rev(anio), y = perc)) +
  geom_col(fill = gdocs_pal()(1)) +
  geom_text(aes(label = percent(perc, accuracy = 0.1)),
             fontface = "bold", nudge_y = -0.025, color = "white") +
  coord_flip() + 
  labs(title = "De las OSC que contaron con autorización como DA el año anterior, % que mantuvo\nsu autorización",
       subtitle = "Por año",
       caption = "Fuente: Elaboración por Data Cívica a partir de los directorios de donatarias autorizadas",
       x = "", y = "") +
  theme_gdocs() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

ggsave(perc_mantuvieron_da_bar, 
         filename = paste0(paths$output, "/perc_da_lag.png"),
         width = 12, height = 8)

# EULER DIAGRAM: ACTIVOS INDESOL VS. SAT ====
matrix_indesol_sat <- ayc_w_status %>% 
  mutate(
    "INDESOL: Activo" = estatus_indesol %in% c("Activa", "Activa condicionada"),
    "INDESOL: Inactivo" = estatus_indesol %in% c("Inactiva", "Disolución"),
    "DA: Activo" = estatus_da == "Activa",
    "DA: Inactivo" = estatus_da == "Inactiva") %>% 
  replace_na(list(
    "INDESOL: Activo" = FALSE,
    "INDESOL: Inactivo" = FALSE,
    "DA: Activo" = FALSE,
    "DA: Inactivo" = FALSE)) %>%
  select("INDESOL: Activo", "INDESOL: Inactivo", "DA: Activo", "DA: Inactivo") %>% 
  as.matrix()

png(filename = paste0(paths$output, "/euler_indesol_sat.png"),
    width = 1000, height = 700, res = 100)

fit_indesol_sat <- euler(matrix_indesol_sat)

gdocs_pal_4 <- gdocs_pal()(24)[c(13, 17:19)]

plot(fit_indesol_sat,
     quantities = list(labels = comma(fit_indesol_sat$original)),
     fills = gdocs_pal_4,
     main = paste0("Estatus de las ", comma(n_total_union_indesol_sat), 
                   " OSC inscritas en el registro del INDESOL y/o como DA"))

dev.off()

# done.
