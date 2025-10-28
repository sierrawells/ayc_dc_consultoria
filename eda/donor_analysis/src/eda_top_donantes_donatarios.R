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

# Movement taxonomy
movements_tax_id <- as_id("1-nvo9AOerfI1DEVdyMvTw_qPsv2IaWJllU87XgDEQSg")

# READ DATA ====
# Informe de transparencia 2024 (Donativos otorgados)
drive_download(informe_transparencia_id,
                path = paste0(tempdir(), "/informe_transparenica_2024.xlsx"),
                overwrite = TRUE)

donativos_otorgados_raw <- read_excel(paste0(tempdir(), "/informe_transparenica_2024.xlsx"),
                                             sheet = "Donativos otorgados")

transparencia_general_raw <- read_excel(paste0(tempdir(), "/informe_transparenica_2024.xlsx"),
                                                  sheet = "Generales")

file.remove(paste0(tempdir(), "/informe_transparenica_2024.xlsx"))

# OSC classifications
drive_download(ntee_id,
                path = paste0(tempdir(), "/movements_classified_joined.csv"),
                overwrite = TRUE)

osc_classified <- read_csv(paste0(tempdir(), "/movements_classified_joined.csv")) %>% 
  select(rfc, ntee, main_movement)

file.remove(paste0(tempdir(), "/movements_classified_joined.csv"))

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
  separate_rows(agrupacion, sep = ",")

file.remove(paste0(tempdir(), "/ntee_groupings.xlsx"))

# Movement taxonomy
drive_download(movements_tax_id,
                path = paste0(tempdir(), "/movements_tax.csv"),
                overwrite = TRUE)

movements_tax <- read_csv(paste0(tempdir(), "/movements_tax.csv")) %>% 
  clean_names() %>% 
  select(-descripcion)

file.remove(paste0(tempdir(), "/movements_tax.csv"))

# CLEAN DATA ====
# Informe de transparencia 2024
donativos_otorgados <- donativos_otorgados_raw %>% 
  clean_names() %>% 
  rename(
    rfc_donante = rfc,
    razon_social_donante = razon_social) %>% 
  select(starts_with("rfc"), starts_with("razon_social"), total) %>% 
  left_join(osc_classified %>% 
              rename(ntee_donante = ntee,
                     movement_donante = main_movement),
            by = c("rfc_donante" = "rfc"),
            relationship = "many-to-many") %>%
  left_join(osc_classified %>% 
              rename(ntee_destinatario = ntee,
                     movement_destinatario = main_movement),
            by = c("rfc_destinatario" = "rfc"),
            relationship = "many-to-many") %>% 
  left_join(movements_tax, by = c("movement_destinatario" = "sub_tema_intermedio"))

transparencia_general <- transparencia_general_raw %>% 
  clean_names() %>% 
  rename(total = total_del_estado_de_ingresos) %>% 
  left_join(osc_classified, by = "rfc", relationship = "many-to-many") %>% 
  left_join(movements_tax, by = c("main_movement" = "sub_tema_intermedio"))

# GRAPH PREP ====
caption_informe_transparencia <- "Fuente: Elaboración propia por Data Cívica, a partir del Informe de Transparencia 2024 del SAT"

# DISTRIBUCIÓN DE DONANTES ====
dist_donantes <- donativos_otorgados %>% 
  group_by(rfc_donante, razon_social_donante) %>% 
  summarize(total_donado = sum(total, na.rm = T)) %>% 
  filter(total_donado > 1e6) %>% 
  mutate(
    donante_group = case_when(
      total_donado < 5e6 ~ "1-5 MDP",
      total_donado < 1e7 ~ "5-10 MDP",
      total_donado < 5e7 ~ "10-50 MDP",
      total_donado < 1e8 ~ "50-100 MDP",
      total_donado < 5e8 ~ "100-500 MDP",
      total_donado < 1e9 ~ "500 MDP o más"),
    donante_group = factor(donante_group, 
                           levels = c("1-5 MDP","5-10 MDP",
                                      "10-50 MDP", "50-100 MDP",
                                      "100-500 MDP", "500 MDP o más"))) %>% 
  group_by(donante_group) %>%
  summarize(n_osc = n()) %>% 
  ungroup() %>% 
  mutate(perc = n_osc / sum(n_osc))

n_donantes <- sum(dist_donantes$n_osc)

rfc_donantes <- donativos_otorgados %>% 
  group_by(rfc_donante, razon_social_donante) %>% 
  summarize(total_donado = sum(total, na.rm = T)) %>% 
  filter(total_donado > 1e6) %>% 
  pull(rfc_donante)

writeLines(rfc_donantes, paste0(paths$output, "rfc_donantes_2024.txt"))

graf_dist_donantes <- dist_donantes %>%
  ggplot(aes(x = "fake", 
             y = n_osc,
             fill = fct_rev(donante_group),
             color = fct_rev(donante_group))) +
  geom_col(position = "stack") +
  geom_label(aes(label = paste0(donante_group, ": ", comma(n_osc), " OSC (", percent(perc, accuracy = 0.1), ")")),
             size = 4, position = position_stack(vjust = 0.5),
             fill = "white", fontface = "bold") +
  labs(
    title = "¿Cuánto donaron los donantes nacionales en 2024?",
    subtitle = paste0("De los ", n_donantes, " donantes nacionales que donaron más de 1 MDP"),
    x = "",
    y = "",
    fill = "Monto total donado",
    caption = caption_informe_transparencia) +
  theme_gdocs() +
  scale_fill_gdocs() +
  scale_color_gdocs(guide = "none") +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none")

ggsave(filename = paste0(paths$output, "dist_donantes_2024.png"),
       plot = graf_dist_donantes,
       width = 12, height = 10)

# ¿QUIÉNES ERAN LOS TOP DONANTES EN 2024?====
top_donantes <- donativos_otorgados %>% 
  group_by(rfc_donante, razon_social_donante) %>% 
  summarize(total_donado = sum(total, na.rm = T)) %>% 
  ungroup() %>% 
  slice_max(total_donado, n = 20) %>% 
  mutate(
    razon_social_donante = str_remove(razon_social_donante, "SIN TIPO DE SOCIEDAD") %>% 
      str_replace(., "I A P", "IAP"),
    razon_social_donante = str_squish(razon_social_donante),
    total_donado_label = dollar(total_donado, scale = 1e-6))

rfc_top_donantes <- top_donantes$rfc_donante

writeLines(rfc_top_donantes, paste0(paths$output, "rfc_top_donantes_2024.txt"))

graf_top_donantes <- top_donantes %>% 
  ggplot(aes(x = fct_reorder(str_wrap(razon_social_donante, 35), total_donado), 
                 y = total_donado)) +
  geom_col(fill = gdocs_pal()(1)) +
  geom_label(aes(label = total_donado_label),
             hjust = -0.1, size = 3.5,
             fill = "white", label.size = NA, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Top 20 donantes nacionales en 2024",
    subtitle = "Por monto total donado (en MDP)",
    x = "",
    y = "",
    caption = caption_informe_transparencia) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6),
                     limits = c(0, max(top_donantes$total_donado * 1.05))) +
  theme_gdocs()

ggsave(filename = paste0(paths$output, "top_donantes_2024.png"),
       plot = graf_top_donantes,
       width = 12, height = 8)

# ¿A QUÉ NTEE DONARON LOS DONANTES EN 2024? (TOP 20 vs. OTROS) ====
ntee_donantes_top_vs_otros <- donativos_otorgados %>% 
  mutate(top_donantes = ifelse(rfc_donante %in% rfc_top_donantes,
                               "Top 20 donantes",
                               "Resto de donantes"),
         ntee_destinatario_first_letter = substr(ntee_destinatario, 1, 1)) %>%
  left_join(ntee_groupings, by = c("ntee_destinatario_first_letter" = "agrupacion")) %>%
  filter(!is.na(tematica)) %>% 
  group_by(top_donantes, tematica) %>%
  summarize(total_donado = sum(total, na.rm = T)) %>% 
  group_by(top_donantes) %>% 
  mutate(perc = total_donado / sum(total_donado))

ntee_donantes_top_order <- ntee_donantes_top_vs_otros %>% 
  filter(top_donantes == "Top 20 donantes") %>% 
  arrange(perc) %>% 
  pull(tematica)

graf_ntee_donantes_top_vs_otros <- ntee_donantes_top_vs_otros %>%
  filter(tematica %in% ntee_donantes_top_order) %>% 
  ggplot(aes(x = perc, y = factor(str_wrap(tematica, 40),
                                  levels = str_wrap(ntee_donantes_top_order, 40)),
             fill = top_donantes)) +
  geom_col(position = "dodge") +
  geom_label(aes(label = percent(perc, accuracy = 0.1), color = top_donantes),
            position = position_dodge(width = 0.9), hjust = -0.1, size = 3.5,
            fontface = "bold", fill = "white", label.size = NA) +
  labs(
    title = "¿A qué tipos de OSC donaron los donantes nacionales en 2024?",
    subtitle = "Porcentaje del total donado por cada grupo de donantes",
    x = "",
    y = "",
    fill = "",
    caption = caption_informe_transparencia) +
  scale_x_continuous(labels = label_percent(accuracy = 0.1),
                     limits = c(0, max(ntee_donantes_top_vs_otros$perc * 1.05))) +
  scale_fill_gdocs() +
  scale_color_gdocs(guide = "none") +
  theme_gdocs() +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  theme(
    legend.position = "top",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

ggsave(filename = paste0(paths$output, "ntee_donantes_top_vs_otros_2024.png"),
       plot = graf_ntee_donantes_top_vs_otros,
       width = 12, height = 8)

# ¿A QUÉ MOVIMIENTOS DONARON LOS DONANTES EN 2024? (TOP 20 vs. OTROS) ====
movimientos_donantes_top_vs_otros <- donativos_otorgados %>% 
  mutate(top_donantes = ifelse(rfc_donante %in% rfc_top_donantes,
                               "Top 20 donantes",
                               "Resto de donantes")) %>%
  filter(!is.na(movement_destinatario)) %>%
  mutate(
    macro_tematica = ifelse(is.na(macro_tematica),
                            paste0("**", movement_destinatario, "**"),
                            macro_tematica),
    macro_tematica == ifelse(macro_tematica == "Otro tipo de ONG",
                            "Otro tipo de ONG*",
                            macro_tematica)) %>% 
  group_by(top_donantes, macro_tematica) %>%
  summarize(total_donado = sum(total, na.rm = T)) %>%
  group_by(top_donantes) %>%
  mutate(perc = total_donado / sum(total_donado),
         macro_tematica = str_wrap(macro_tematica, 40) %>% 
           str_replace_all("\n", "<br>")) %>% 
  filter(!str_detect(tolower(macro_tematica), "digitales"))

movimientos_donantes_top_order <- movimientos_donantes_top_vs_otros %>%
  filter(top_donantes == "Top 20 donantes") %>% 
  arrange(perc) %>% 
  pull(macro_tematica)

graf_movimientos_donantes_top_vs_otros <- movimientos_donantes_top_vs_otros %>%
  ggplot(aes(x = perc, 
             y = factor(macro_tematica,
                        levels = movimientos_donantes_top_order),
             fill = top_donantes)) +
  geom_col(position = "dodge") +
  geom_label(aes(label = percent(perc, accuracy = 0.1), color = top_donantes),
             position = position_dodge(width = 0.9), hjust = -0.1, size = 3.5,
             fontface = "bold", fill = "white", label.size = NA) +
  labs(
    title = "¿A qué tipo de OSC donaron los donantes nacionales en 2024?",
    subtitle = paste0("Porcentaje del total donado por cada grupo de donantes",
                      "<br>OSC no pertenicientes a un movimiento social/político en **negritas**"),
    x = "",
    y = "",
    fill = "",
    caption = paste0(caption_informe_transparencia,
                     "\n*'Otro tipo de ONG' incluye OSC que no pertenecen a ningún movimiento específico.")) +
  scale_x_continuous(labels = label_percent(accuracy = 0.1),
                     limits = c(0, max(movimientos_donantes_top_vs_otros$perc * 1.05))) +
  scale_fill_gdocs() +
  scale_color_gdocs(guide = "none") +
  theme_gdocs() +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  theme(
    legend.position = "top",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_markdown(),
    plot.subtitle = element_markdown())

ggsave(filename = paste0(paths$output, "movimientos_donantes_top_vs_otros_2024.png"),
       plot = graf_movimientos_donantes_top_vs_otros,
       width = 12, height = 10)

# DISTRIBUCIÓN DE DONATARIOS ====
dist_donatarios <- transparencia_general %>% 
  group_by(rfc, razon_social) %>%
  summarize(total_ingresos = sum(total, na.rm = T)) %>% 
  filter(total_ingresos > 0,
         ! rfc %in% rfc_donantes) %>% 
  mutate(
    donatario_group = case_when(
      total_ingresos < 1e6 ~ "Menos de 1 MDP",
      total_ingresos <= 5e6 ~ "1-5 MDP",
      total_ingresos <= 1e7 ~ "5-10 MDP",
      total_ingresos <= 5e7 ~ "10-50 MDP",
      total_ingresos <= 1e8 ~ "50-100 MDP",
      total_ingresos > 1e8 ~ "Más de 100 MDP"),
    donatario_group = factor(donatario_group, 
                            levels = c("Menos de 1 MDP", "1-5 MDP","5-10 MDP",
                                       "10-50 MDP", "50-100 MDP",
                                       "Más de 100 MDP"))) %>% 
  group_by(donatario_group) %>%
  summarize(n_osc = n()) %>%
  ungroup() %>%
  mutate(perc = n_osc / sum(n_osc))

n_donatarios <- sum(dist_donatarios$n_osc)

graf_dist_donatarios <- dist_donatarios %>%
  ggplot(aes(x = "fake", 
             y = n_osc,
             fill = fct_rev(donatario_group),
             color = fct_rev(donatario_group))) +
  geom_col(position = "stack") +
  geom_label(aes(label = paste0(donatario_group, ": ", comma(n_osc), " OSC (", percent(perc, accuracy = 0.1), ")")),
                   size = 4, position = position_stack(vjust = 0.5),
                   fill = "white", fontface = "bold") +
  labs(
    title = "¿Cuántos ingresos tienen las OSC mexicanas?",
    subtitle = paste0("Total del estado de ingresos en 2024"),
    x = "",
    y = "",
    caption = paste0(caption_informe_transparencia,
                     "\nSe excluyen donantes que donaron más de 1 MDP en 2024")) +
  theme_gdocs() +
  scale_fill_gdocs() +
  scale_color_gdocs(guide = "none") +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none")

ggsave(filename = paste0(paths$output, "dist_donatarios_2024.png"),
       plot = graf_dist_donatarios,
       width = 12, height = 10)

# ¿QUIÉNES ERAN LOS TOP DONATARIOS EN 2024?====
top_donatarios <- transparencia_general %>% 
  group_by(rfc, razon_social) %>%
  summarize(total = sum(total, na.rm = T)) %>% 
  filter(total > 0,
         ! rfc %in% rfc_donantes,
         razon_social != "FUNDACION TODO POR TI AC") %>% 
  ungroup() %>% 
  slice_max(total, n = 20) %>% 
  left_join(osc_classified, by = "rfc") %>% 
  mutate(
    razon_social = str_remove_all(razon_social, "SIN TIPO DE SOCIEDAD"),
    razon_social = case_when(
      str_detect(razon_social, "PATI&O") ~ str_replace(razon_social, "PATI&O", "PATIÑO"),
      T ~ razon_social),
    main_movement = case_when(
      str_detect(razon_social, "DOLORES OLMEDO") ~ "Derechos culturales y acceso a la cultura",
      str_detect(razon_social, "AMERICAN SCHOOL") ~ "Escuelas/universidades",
      str_detect(razon_social, "ESCUELA VETERANOS DE LA REVOLUCION") ~ "Escuelas/universidades",
      T ~ main_movement))

graf_top_donatarios <- top_donatarios %>%
  ggplot(aes(x = fct_reorder(str_wrap(razon_social, 40), total), 
                 y = total)) +
  geom_col(aes(fill = main_movement)) +
  geom_label(aes(label = dollar(total, scale = 1e-6)),
             hjust = -0.1, size = 3.5,
             fill = "white", label.size = NA, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Donatarias autorizadas con mayores ingresos",
    subtitle = "Total del estado de ingresos (MDP)",
    x = "",
    y = "",
    fill = "Tipo de organización",
    caption = caption_informe_transparencia) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6),
                     limits = c(0, max(top_donatarios$total * 1.05))) +
  theme_gdocs() +
  scale_fill_gdocs() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(nrow = 2))

ggsave(filename = paste0(paths$output, "top_donatarios_2024.png"),
       plot = graf_top_donatarios,
       width = 16, height = 12)

# TOP DONANTE NACIONAL DE LOS TOP DONATARIOS ====

# TRANSFORMACION COLECTIVA CON EQUIDAD AC (# 1)
# RFC: TCE1304091B1
# Solo 1 donación en ingresos_por_donativos
# De persona moral NACIONAL (debe estar en donativos_otorgados)
# No se encuentra número de folio (TRANS007821/2025) en donativos_otorgados

# FIDEICOMISO DE ADMINISTRACIÓN F/744942 (#4)
# RFC: CFI131217GB8
# Misma cosa: solo 1 donación en ingresos_por_donativos
# De persona moral NACIONAL (debe estar en donativos_otorgados)
# No se encuentra número de folio (TRANS002870/2025) en donativos_otorgados

# done.
           

