#
# Author: SW
# Maintainer(s): SW, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, readxl, janitor, ggthemes, scales,
               ggrepel, ggtext)

# Files ====
paths <- list(output = here("donor_analysis/eda/output/"))

drive_auth(email = "sierra.wells@datacivica.org")

# Informe de transparencia 2024
informe_transparencia_id <- drive_ls(as_id("1C1nq47Q8fnMRaVa1wMeZb1ZIpIRl-Pt0")) %>% 
  filter(str_detect(name, "2024")) %>% 
  pull(id)

stopifnot(length(informe_transparencia_id) == 1)

# OSC classifications
ntee_id <- drive_ls(as_id("16CVo8XsSrjyMQyoS2g5R-yPIQhV5nGjm")) %>% 
  filter(str_detect(name, "movements_classified_joined.csv")) %>% 
  pull(id)

stopifnot(length(ntee_id) == 1)

# Movement taxonomy
movements_tax_id <- as_id("1-nvo9AOerfI1DEVdyMvTw_qPsv2IaWJllU87XgDEQSg")

# READ DATA ====
# Informe de transparencia 2024
drive_download(informe_transparencia_id,
               path = paste0(tempdir(), "/informe_transparencia_2024.xlsx"),
               overwrite = TRUE)

ingresos_por_donativos_raw <- read_excel(paste0(tempdir(), "/informe_transparencia_2024.xlsx"),
                                         sheet = "Ingreso por donativos")

transparencia_general_raw <- read_excel(paste0(tempdir(), "/informe_transparencia_2024.xlsx"),
                                         sheet = "Generales") %>% 
  clean_names()

file.remove(paste0(tempdir(), "/informe_transparencia_2024.xlsx"))

# OSC classifications
drive_download(ntee_id,
               path = paste0(tempdir(), "/movements_classified_joined.csv"),
               overwrite = TRUE)

osc_classified <- read_csv(paste0(tempdir(), "/movements_classified_joined.csv")) %>% 
  select(rfc, ntee, main_movement)

file.remove(paste0(tempdir(), "/movements_classified_joined.csv"))

# Movement taxonomy
drive_download(movements_tax_id,
               path = paste0(tempdir(), "/movements_tax.csv"),
               overwrite = TRUE)

movements_tax <- read_csv(paste0(tempdir(), "/movements_tax.csv")) %>% 
  clean_names() %>% 
  select(-descripcion)

file.remove(paste0(tempdir(), "/movements_tax.csv"))

# RFC donantes
rfc_donantes <- readLines(paste0(paths$output, "rfc_donantes_2024.txt"))
rfc_top_donantes <- readLines(paste0(paths$output, "rfc_top_donantes_2024.txt"))

# CLEAN DATA ====
ingresos_por_donativos <- ingresos_por_donativos_raw %>% 
  clean_names() %>% 
  left_join(osc_classified, by = "rfc") %>% 
  left_join(movements_tax, by = c("main_movement" = "sub_tema_intermedio")) %>% 
  mutate(total = monto_efectivo + monto_especie,
         macro_tematica = ifelse(is.na(macro_tematica), main_movement, macro_tematica)) %>% 
  filter(!rfc %in% rfc_donantes,
         !is.na(macro_tematica),
         !macro_tematica %in% c("Otro tipo de ONG", "Fundaciones"))

transparencia_general <- transparencia_general_raw %>% 
  clean_names() %>% 
  left_join(osc_classified, by = "rfc") %>% 
  left_join(movements_tax, by = c("main_movement" = "sub_tema_intermedio")) %>% 
  mutate(macro_tematica = ifelse(is.na(macro_tematica), main_movement, macro_tematica)) %>% 
  rename(total = total_del_estado_de_ingresos) %>% 
  filter(!rfc %in% rfc_donantes,
         !is.na(macro_tematica),
         !macro_tematica %in% c("Otro tipo de ONG", "Fundaciones"))

# GRAPH PREP ====
caption_informe_transparencia <- paste0("Fuente: Elaboración por Data Cívica, a partir del Informe de Transparencia del SAT 2024.",
                                        "\nSe excluyen OSC que donaron más de 1 MDP en 2024.")

# TOTAL FUNDING BY MACRO MOVEMENT ====
funding_by_macro_mov <- transparencia_general %>% 
  group_by(macro_tematica) %>% 
  reframe(total = sum(total, na.rm = TRUE),
          n_osc = n_distinct(rfc),
          total_mdp = total / 1e6,
          funding_per_osc_mdp = total_mdp / n_osc) %>% 
  filter(n_osc >= 30) %>% 
  mutate(macro_tematica = ifelse(macro_tematica %in% c("Hospitales",
                                                       "Escuelas/universidades",
                                                       "Iglesias"),
                                 paste0("**", str_wrap(macro_tematica, 40), "**"),
                                 str_wrap(macro_tematica, 40)),
         macro_tematica = str_replace_all(macro_tematica, "\n", "<br>"))

graf_funding_by_macro_mov <- funding_by_macro_mov %>% 
  ggplot(aes(y = reorder(macro_tematica, total_mdp), x = total_mdp)) +
  geom_col(fill = gdocs_pal()(1)) +
  geom_label(aes(label = dollar(total_mdp)),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  labs(title = "¿Cuáles sectores de la sociedad civil tienen mayores ingresos?",
       subtitle = paste0("Total del estado de ingresos (MDP) desagregado por macro-temática",
                         "<br>OSC no pertenicientes a un movimiento social/político en **negritas**"),
       x = "",
       y = "",
       caption = paste0(caption_informe_transparencia,
                        "\nSolo se incluyen movimientos con más de 30 OSC identificadas.")) +
  scale_x_continuous(labels = dollar_format(),
                     limits = c(0, max(funding_by_macro_mov$total_mdp) * 1.05)) +
  theme_gdocs() +
  theme(
    plot.subtitle = element_markdown(),
    axis.text.y = element_markdown())

ggsave(filename = paste0(paths$output, "total_fund_by_macro_mov.png"),
       plot = graf_funding_by_macro_mov,
       width = 14, height = 10)

# TOTAL FUNDING BY MICRO MOVEMENT ====
funding_by_micro_mov <- transparencia_general %>% 
  group_by(main_movement) %>% 
  reframe(total = sum(total, na.rm = TRUE),
          n_osc = n_distinct(rfc),
          total_mdp = total / 1e6,
          funding_per_osc_mdp = total_mdp / n_osc) %>% 
  filter(n_osc >= 30) %>% 
  mutate(main_movement = ifelse(main_movement %in% c("Hospitales",
                                                       "Escuelas/universidades",
                                                       "Iglesias"),
                                 paste0("**", str_wrap(main_movement, 40), "**"),
                                 str_wrap(main_movement, 40)),
         main_movement = str_replace_all(main_movement, "\n", "<br>"),
         rank_total = rank(-total_mdp),
         rank_total_categ = case_when(
           rank_total <= 5 ~ "Más altos",
           rank_total >= 23 ~ "Más bajos",
           T ~ NA_character_),
         rank_per_cap = rank(-funding_per_osc_mdp),
         rank_per_cap_categ = case_when(
           rank_per_cap <= 5 ~ "Más altos",
           rank_per_cap >= 23 ~ "Más bajos",
           T ~ NA_character_),
         rank_n_osc = rank(-n_osc),
         rank_n_osc_categ = case_when(
           rank_n_osc <= 5 ~ "Más altos",
           rank_n_osc >= 23 ~ "Más bajos",
           T ~ NA_character_),
         across(contains("rank"),
                ~ factor(.x, levels = c("Más altos", "Más bajos"),
                                       ordered = TRUE)))

graf_funding_by_micro_mov <- funding_by_micro_mov %>%
  filter(!is.na(rank_total_categ)) %>% 
  ggplot(aes(y = reorder(main_movement, total_mdp), x = total_mdp)) +
  facet_wrap(~ rank_total_categ, ncol = 1, scales = "free_y") +
  geom_col(aes(fill = rank_total_categ)) +
  geom_label(aes(label = dollar(total_mdp)),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  labs(title = "¿Cuáles sectores de la sociedad civil tienen mayores ingresos?",
       subtitle = paste0("Total del estado de ingresos (MDP) desagregado por movimiento",
                         "<br>OSC no pertenicientes a un movimiento social/político en **negritas**"),
       x = "",
       y = "",
       caption = paste0(caption_informe_transparencia,
                        "\nSolo se incluyen movimientos con más de 30 OSC identificadas.")) +
  scale_x_continuous(labels = dollar_format(),
                     limits = c(0, max(funding_by_micro_mov$total_mdp) * 1.05)) +
  scale_fill_manual(values = c("Más altos" = gdocs_pal()(2)[1], "Más bajos" = gdocs_pal()(2)[2])) +
  theme_gdocs() +
  theme(
    plot.subtitle = element_markdown(),
    axis.text.y = element_markdown(),
    legend.position = "none")

ggsave(filename = paste0(paths$output, "total_fund_by_micro_mov.png"),
       plot = graf_funding_by_micro_mov,
       width = 14, height = 10)
  
# PER CÁPITA FUNDING BY MACRO MOVEMENT ====
graf_funding_per_cap_by_macro_mov <- funding_by_macro_mov %>% 
  ggplot(aes(y = reorder(macro_tematica, funding_per_osc_mdp), 
             x = funding_per_osc_mdp)) +
  geom_col(fill = gdocs_pal()(1)) +
  geom_label(aes(label = dollar(funding_per_osc_mdp)),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  labs(title = "¿Cuáles sectores de la sociedad civil tienen mayores ingresos por organización?",
       subtitle = paste0("Ingresos por cada donataria autorizada (MDP), desagregado por macro-temática",
                         "<br>OSC no pertenicientes a un movimiento social/político en **negritas**"),
       x = "",
       y = "",
       caption = paste0(caption_informe_transparencia,
                        "\nSolo se incluyen movimientos con más de 30 OSC identificadas.")) +
  scale_x_continuous(labels = dollar_format(),
                     limits = c(0, max(funding_by_macro_mov$funding_per_osc_mdp) * 1.05)) +
  theme_gdocs() +
  theme(
    plot.subtitle = element_markdown(),
    axis.text.y = element_markdown())

ggsave(filename = paste0(paths$output, "fund_per_cap_by_macro_mov.png"),
       plot = graf_funding_per_cap_by_macro_mov,
       width = 14, height = 10)

# PER CÁPITA FUNDING BY MICRO MOVEMENT ====
graf_funding_per_cap_by_micro_mov <- funding_by_micro_mov %>%
  filter(!is.na(rank_per_cap_categ)) %>% 
  ggplot(aes(y = reorder(main_movement, funding_per_osc_mdp), 
             x = funding_per_osc_mdp)) +
  facet_wrap(~ rank_per_cap_categ, ncol = 1, scales = "free_y") +
  geom_col(aes(fill = rank_per_cap_categ)) +
  geom_label(aes(label = dollar(funding_per_osc_mdp)),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  labs(title = "¿Cuáles sectores de la sociedad civil tienen mayores ingresos por organización?",
       subtitle = paste0("Ingresos por cada donataria autorizada (MDP), desagregado por movimiento",
                         "<br>OSC no pertenicientes a un movimiento social/político en **negritas**"),
       x = "",
       y = "",
       caption = paste0(caption_informe_transparencia,
                        "\nSolo se incluyen movimientos con más de 30 OSC identificadas.")) +
  scale_x_continuous(labels = dollar_format(),
                     limits = c(0, max(funding_by_micro_mov$funding_per_osc_mdp) * 1.05)) +
  scale_fill_manual(values = c("Más altos" = gdocs_pal()(2)[1], "Más bajos" = gdocs_pal()(2)[2])) +
  theme_gdocs() +
  theme(
    plot.subtitle = element_markdown(),
    axis.text.y = element_markdown(),
    legend.position = "none")

ggsave(filename = paste0(paths$output, "fund_per_cap_by_micro_mov.png"),
       plot = graf_funding_per_cap_by_micro_mov,
       width = 14, height = 10)

# NUM. OSC POR MACRO MOVEMENT ====
n_osc_macro_mov <- osc_classified %>% 
  left_join(movements_tax, by = c("main_movement" = "sub_tema_intermedio")) %>%
  mutate(macro_tematica = ifelse(is.na(macro_tematica), main_movement, macro_tematica)) %>% 
  filter(!is.na(macro_tematica),
         macro_tematica != "Fundaciones",
         !rfc %in% rfc_donantes) %>%
  group_by(macro_tematica) %>% 
  reframe(n_osc = n_distinct(rfc)) %>% 
  mutate(macro_tematica = ifelse(macro_tematica %in% c("Hospitales",
                                                  "Escuelas/universidades",
                                                  "Iglesias",
                                                  "Otro tipo de ONG"),
                          paste0("**", str_wrap(macro_tematica, 40), "**"),
                          str_wrap(macro_tematica, 40)),
         macro_tematica = str_replace_all(macro_tematica, "\n", "<br>"))

graf_n_osc_macro_mov <- n_osc_macro_mov %>%
  ggplot(aes(y = reorder(macro_tematica, n_osc), x = n_osc)) +
  geom_col(fill = gdocs_pal()(1)) +
  geom_label(aes(label = n_osc),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  labs(title = "Donatarias autorizadas por movimiento/tipo de ONG",
       subtitle = "OSC no pertenicientes a un movimiento social/político en **negritas**",
       x = "",
       y = "",
       caption = paste0(caption_informe_transparencia,
                        "\n'Otro tipo de ONG' incluye ONGs que no pertenecen a un movimiento social/político pero que tampoco son hospitales, escuelas,\niglesias o fundaciones.")) +
  scale_x_continuous(limits = c(0, max(n_osc_macro_mov$n_osc) * 1.05)) +
  theme_gdocs() +
  theme(
    plot.subtitle = element_markdown(),
    axis.text.y = element_markdown())

ggsave(filename = paste0(paths$output, "n_osc_macro_mov.png"),
       plot = graf_n_osc_macro_mov,
       width = 14, height = 10)

# % OF FOREIGN FUNDING BY MACRO MOVEMENT ====
foreign_funding_by_macro_mov <- ingresos_por_donativos %>% 
  mutate(foreign = ifelse(str_detect(donante, "Nacional"), 0, 1)) %>% 
  group_by(macro_tematica, foreign) %>%
  reframe(total = sum(total, na.rm = TRUE),
          n_osc = n_distinct(rfc)) %>% 
  group_by(macro_tematica) %>% 
  mutate(perc = total / sum(total, na.rm = TRUE),
         macro_tematica = ifelse(macro_tematica %in% c("Hospitales",
                                                  "Escuelas/universidades",
                                                  "Iglesias"),
                          paste0("**", str_wrap(macro_tematica, 40), "**"),
                          str_wrap(macro_tematica, 40)),
         macro_tematica = str_replace_all(macro_tematica, "\n", "<br>")) %>% 
  filter(foreign == 1,
         n_osc >= 10)

graf_foreign_funding_by_macro_mov <- foreign_funding_by_macro_mov %>%
  ggplot(aes(y = reorder(str_wrap(macro_tematica, 30), perc), x = perc)) +
  geom_col(fill = gdocs_pal()(1)) +
  geom_label(aes(label = percent(perc, accuracy = 0.1)),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  labs(title = "¿Cuáles sectores de la sociedad civil reciben más financiamiento extranjero?",
       subtitle = paste0("% de donativos a donatarias autorizadas que son de donantes extranjeros, desagregado por macro-temática",
                         "<br>OSC no pertenicientes a un movimiento social/político en **negritas**"),
       x = "",
       y = "",
       caption = paste0(caption_informe_transparencia,
                        "\nSolo se incluyen movimientos con más de 10 OSC identificadas.")) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1),
                     limits = c(0, max(foreign_funding_by_macro_mov$perc) * 1.05)) +
  theme_gdocs() +
  theme(
    plot.subtitle = element_markdown(),
    axis.text.y = element_markdown())

ggsave(filename = paste0(paths$output, "foreign_funding_by_macro_mov.png"),
       plot = graf_foreign_funding_by_macro_mov,
       width = 14, height = 10)

# % OF FOREIGN FUNDING BY MICRO MOVEMENT ====
foreign_funding_by_micro_mov <- ingresos_por_donativos %>% 
  mutate(foreign = ifelse(str_detect(donante, "Nacional"), 0, 1)) %>% 
  group_by(main_movement, foreign) %>%
  reframe(total = sum(total, na.rm = TRUE),
          n_osc = n_distinct(rfc)) %>% 
  group_by(main_movement) %>% 
  mutate(perc = total / sum(total, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(foreign == 1,
         n_osc >= 10) %>% 
  mutate(rank = rank(-perc),
         rank_categ = case_when(
           rank <= 5 ~ "Más altos",
           rank >= 16 ~ "Más bajos",
           T ~ NA_character_),
         rank_categ = factor(rank_categ, levels = c("Más altos", "Más bajos"),
                                       ordered = TRUE),
         main_movement = ifelse(main_movement %in% c("Hospitales",
                                                  "Escuelas/universidades",
                                                  "Iglesias"),
                          paste0("**", str_wrap(main_movement, 40), "**"),
                          str_wrap(main_movement, 40)),
         main_movement = str_replace_all(main_movement, "\n", "<br>")) %>% 
  filter(!is.na(rank_categ))

graf_foreign_funding_by_micro_mov <- foreign_funding_by_micro_mov %>%
  ggplot(aes(y = reorder(main_movement, perc), x = perc)) +
  facet_wrap(~ rank_categ, ncol = 1, scales = "free_y") +
  geom_col(aes(fill = rank_categ)) +
  geom_label(aes(label = percent(perc, accuracy = 0.1)),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  labs(title = "¿Cuáles sectores de la sociedad civil reciben más financiamiento extranjero?",
       subtitle = paste0("% de donativos a donatarias autorizadas que son de donantes extranjeros, desagregado por movimiento",
                         "<br>OSC no pertenicientes a un movimiento social/político en **negritas**"),
       x = "",
       y = "",
       caption = paste0(caption_informe_transparencia,
                        "\nSolo se incluyen movimientos con más de 10 OSC identificadas.")) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1),
                     limits = c(0, max(foreign_funding_by_micro_mov$perc) * 1.05)) +
  scale_fill_manual(values = c("Más altos" = gdocs_pal()(2)[1], "Más bajos" = gdocs_pal()(2)[2])) +
  theme_gdocs() +
  theme(
    plot.subtitle = element_markdown(),
    axis.text.y = element_markdown(),
    legend.position = "none")

ggsave(filename = paste0(paths$output, "foreign_funding_by_micro_mov.png"),
       plot = graf_foreign_funding_by_micro_mov,
       width = 14, height = 8)

# % OF FUNDING FROM MEXICAN PUBLIC SECTOR BY MACRO MOVEMENT ====
public_nac_funding_by_macro_mov <- ingresos_por_donativos %>% 
  mutate(public_nac = ifelse(donante == "Donativo Sector Público Nacional", 1, 0)) %>% 
  group_by(macro_tematica, public_nac) %>%
  reframe(total = sum(total, na.rm = TRUE),
          n_osc = n_distinct(rfc)) %>% 
  group_by(macro_tematica) %>% 
  mutate(perc = total / sum(total, na.rm = TRUE),
         macro_tematica = ifelse(macro_tematica %in% c("Hospitales",
                                                  "Escuelas/universidades",
                                                  "Iglesias"),
                          paste0("**", str_wrap(macro_tematica, 40), "**"),
                          str_wrap(macro_tematica, 40))) %>% 
  filter(public_nac == 1,
         n_osc >= 10)

graf_public_nac_funding_by_macro_mov <- public_nac_funding_by_macro_mov %>%
  ggplot(aes(y = reorder(macro_tematica, perc), x = perc)) +
  geom_col(fill = gdocs_pal()(1)) +
  geom_label(aes(label = percent(perc, accuracy = 0.1)),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  labs(title = "¿Cuáles sectores de la sociedad civil reciben más financiamiento del\nsector público nacional?",
       subtitle = paste0("% de donativos a donatarias autorizadas que son del sector público nacional, desagregado por macro-temática",
                         "<br>OSC no pertenicientes a un movimiento social/político en **negritas**"),
       x = "",
       y = "",
       caption = paste0(caption_informe_transparencia,
                        "\nSolo se incluyen movimientos con más de 10 OSC identificadas.")) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1),
                     limits = c(0, max(public_nac_funding_by_macro_mov$perc) * 1.05)) +
  theme_gdocs() +
  theme(
    plot.subtitle = element_markdown(),
    axis.text.y = element_markdown())

ggsave(filename = paste0(paths$output, "public_nac_funding_by_macro_mov.png"),
       plot = graf_public_nac_funding_by_macro_mov,
       width = 14, height = 8)

# % OF FUNDING FROM MEXICAN PUBLIC SECTOR BY MICRO MOVEMENT ====
public_nac_funding_by_micro_mov <- ingresos_por_donativos %>% 
  mutate(public_nac = ifelse(donante == "Donativo Sector Público Nacional", 1, 0)) %>% 
  group_by(main_movement, public_nac) %>%
  reframe(total = sum(total, na.rm = TRUE),
          n_osc = n_distinct(rfc)) %>% 
  group_by(main_movement) %>% 
  mutate(perc = total / sum(total, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(public_nac == 1,
         n_osc >= 10) %>% 
  mutate(rank = rank(-perc),
         rank_categ = case_when(
           rank <= 5 ~ "Más altos",
           rank >= 15 ~ "Más bajos",
           T ~ NA_character_),
         rank_categ = factor(rank_categ, levels = c("Más altos", "Más bajos"),
                                       ordered = TRUE),
         main_movement = ifelse(main_movement %in% c("Hospitales",
                                                  "Escuelas/universidades",
                                                  "Iglesias"),
                          paste0("**", str_wrap(main_movement, 40), "**"),
                          str_wrap(main_movement, 40)),
         main_movement = str_replace_all(main_movement, "\n", "<br>")) %>% 
  filter(!is.na(rank_categ))

graf_public_nac_funding_by_micro_mov <- public_nac_funding_by_micro_mov %>%
  ggplot(aes(y = reorder(main_movement, perc), x = perc)) +
  facet_wrap(~ rank_categ, ncol = 1, scales = "free_y") +
  geom_col(aes(fill = rank_categ)) +
  geom_label(aes(label = percent(perc, accuracy = 0.1)),
             hjust = -0.1, size = 3.5, fontface = "bold",
             fill = "white", label.size = NA) +
  labs(title = "¿Cuáles sectores de la sociedad civil reciben más financiamiento del\nsector público nacional?",
       subtitle = paste0("Porcentaje de donativos a donatarias autorizadas que son del sector público nacional, desagregado por movimiento",
                         "<br>OSC no pertenicientes a un movimiento social/político en **negritas**"),
       x = "",
       y = "",
       caption = paste0(caption_informe_transparencia,
                        "\nSolo se incluyen movimientos con más de 10 OSC identificadas.")) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1),
                     limits = c(0, max(public_nac_funding_by_micro_mov$perc) * 1.05)) +
  scale_fill_manual(values = c("Más altos" = gdocs_pal()(2)[1], "Más bajos" = gdocs_pal()(2)[2])) +
  theme_gdocs() +
  theme(
    plot.subtitle = element_markdown(),
    axis.text.y = element_markdown(),
    legend.position = "none")

ggsave(filename = paste0(paths$output, "public_nac_funding_by_micro_mov.png"),
       plot = graf_public_nac_funding_by_micro_mov,
       width = 14, height = 8)

# done.              